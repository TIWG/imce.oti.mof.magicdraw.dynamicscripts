import java.io.File
import sbt.Keys._
import sbt._

import gov.nasa.jpl.imce.sbt._
import gov.nasa.jpl.imce.sbt.ProjectHelper._

updateOptions := updateOptions.value.withCachedResolution(true)

shellPrompt in ThisBuild := { state => Project.extract(state).currentRef.project + "> " }

lazy val mdInstallDirectory = SettingKey[File]("md-install-directory", "MagicDraw Installation Directory")

mdInstallDirectory in Global :=
  baseDirectory.value / "target" / "md.package"

resolvers := {
  val previous = resolvers.value
  if (git.gitUncommittedChanges.value)
    Seq[Resolver](Resolver.mavenLocal) ++ previous
  else
    previous
}

lazy val core = Project("imce-oti-mof-magicdraw-dynamicscripts", file("."))
  .enablePlugins(IMCEGitPlugin)
  .enablePlugins(IMCEReleasePlugin)
  .settings(dynamicScriptsResourceSettings("imce.oti.mof.magicdraw.dynamicscripts"))
  .settings(IMCEPlugin.strictScalacFatalWarningsSettings)
  .settings(IMCEReleasePlugin.packageReleaseProcessSettings)
  .settings(
    IMCEKeys.licenseYearOrRange := "2016",
    IMCEKeys.organizationInfo := IMCEPlugin.Organizations.oti,
    IMCEKeys.targetJDK := IMCEKeys.jdk18.value,

    buildInfoPackage := "imce.oti.mof.magicdraw.dynamicscripts",
    buildInfoKeys ++= Seq[BuildInfoKey](BuildInfoKey.action("buildDateUTC") { buildUTCDate.value }),

    mappings in (Compile, packageSrc) ++= {
      import Path.{flat, relativeTo}
      val base = (sourceManaged in Compile).value
      val srcs = (managedSources in Compile).value
      srcs x (relativeTo(base) | flat)
    },

    projectID := {
      val previous = projectID.value
      previous.extra(
        "build.date.utc" -> buildUTCDate.value,
        "artifact.kind" -> "magicdaw.library")
    },

    git.baseVersion := Versions.version,

    resourceDirectory in Compile := baseDirectory.value / "resources",

    unmanagedClasspath in Compile <++= unmanagedJars in Compile,

    resolvers += Resolver.bintrayRepo("jpl-imce", "gov.nasa.jpl.imce"),
    resolvers += Resolver.bintrayRepo("tiwg", "org.omg.tiwg")
  )
  .dependsOnSourceProjectOrLibraryArtifacts(
    "imce-oti-uml-magicdraw-dynamicscripts",
    "imce.oti.uml.magicdraw.dynamicscripts",
    Seq(
      "org.omg.tiwg" %% "imce.oti.uml.magicdraw.dynamicscripts"
        % Versions_imce_oti_uml_magicdraw_dynamicscripts.version
        % "compile" withSources() withJavadoc() artifacts
        Artifact("imce.oti.uml.magicdraw.dynamicscripts", "zip", "zip", Some("resource"), Seq(), None, Map())
    )
  )
  .dependsOnSourceProjectOrLibraryArtifacts(
    "org-omg-oti-mof-schema",
    "org.omg.oti.mof.schema",
    Seq(
      "org.omg.tiwg" %% "org.omg.oti.mof.schema"
        % Versions_oti_mof_schema.version
        % "compile" withSources() withJavadoc() artifacts
        Artifact("org.omg.oti.mof.schema", "zip", "zip", Some("resource"), Seq(), None, Map())
    )
  )
  .settings(
    extractArchives := {
      val base = baseDirectory.value
      val up = update.value
      val s = streams.value
      val mdInstallDir = (mdInstallDirectory in ThisBuild).value

      if (!mdInstallDir.exists) {

        val parts = (for {
          cReport <- up.configurations
          if cReport.configuration == "compile"
          mReport <- cReport.modules
          if mReport.module.organization == "org.omg.tiwg.vendor.nomagic"
          (artifact, archive) <- mReport.artifacts
        } yield archive).sorted

        s.log.info(s"Extracting MagicDraw from ${parts.size} parts:")
        parts.foreach { p => s.log.info(p.getAbsolutePath) }

        val merged = File.createTempFile("md_merged", ".zip")
        println(s"merged: ${merged.getAbsolutePath}")

        val zip = File.createTempFile("md_no_install", ".zip")
        println(s"zip: ${zip.getAbsolutePath}")

        val script = File.createTempFile("unzip_md", ".sh")
        println(s"script: ${script.getAbsolutePath}")

        val out = new java.io.PrintWriter(new java.io.FileOutputStream(script))
        out.println("#!/bin/bash")
        out.println(parts.map(_.getAbsolutePath).mkString("cat ", " ", s" > ${merged.getAbsolutePath}"))
        out.println(s"zip -FF ${merged.getAbsolutePath} --out ${zip.getAbsolutePath}")
        out.println(s"unzip -q ${zip.getAbsolutePath} -d ${mdInstallDir.getAbsolutePath}")
        out.close()

        val result = sbt.Process(command = "/bin/bash", arguments = Seq[String](script.getAbsolutePath)).!

        require(0 <= result && result <= 2, s"Failed to execute script (exit=$result): ${script.getAbsolutePath}")

        val mdDynamicScriptsDir = mdInstallDir / "dynamicScripts"
        IO.createDirectory(mdDynamicScriptsDir)

        val zfilter: DependencyFilter = new DependencyFilter {
          def apply(c: String, m: ModuleID, a: Artifact): Boolean =
            (a.`type` == "zip" || a.`type` == "resource") &&
              a.extension == "zip" &&
              m.organization == "org.omg.tiwg"
        }
        val zs: Seq[File] = up.matching(zfilter)
        zs.foreach { zip =>
          val files = IO.unzip(zip, mdDynamicScriptsDir)
          s.log.info(
            s"=> extracted ${files.size} DynamicScripts files from zip: ${zip.getName}")
        }

      } else
        s.log.info(
          s"=> use existing md.install.dir=$mdInstallDir")
    },

    unmanagedJars in Compile := {
      val prev = (unmanagedJars in Compile).value
      val base = baseDirectory.value
      val s = streams.value
      val _ = extractArchives.value

      val mdInstallDir = base / "target" / "md.package"

      val allJars = (mdInstallDir ** "*.jar").get.map(Attributed.blank)
      s.log.info(s"=> Adding ${allJars.size} unmanaged jars")

      allJars
    },

    compile <<= (compile in Compile) dependsOn extractArchives
  )

def dynamicScriptsResourceSettings(projectName: String): Seq[Setting[_]] = {

  import com.typesafe.sbt.packager.universal.UniversalPlugin.autoImport._

  def addIfExists(f: File, name: String): Seq[(File, String)] =
    if (!f.exists) Seq()
    else Seq((f, name))

  Seq(
    // the '*-resource.zip' archive will start from: 'dynamicScripts'
    com.typesafe.sbt.packager.Keys.topLevelDirectory in Universal := None,

    // name the '*-resource.zip' in the same way as other artifacts
    com.typesafe.sbt.packager.Keys.packageName in Universal :=
      normalizedName.value + "_" + scalaBinaryVersion.value + "-" + version.value + "-resource",

    // contents of the '*-resource.zip' to be produced by 'universal:packageBin'
    mappings in Universal in packageBin ++= {
      val dir = baseDirectory.value
      val bin = (packageBin in Compile).value
      val src = (packageSrc in Compile).value
      val doc = (packageDoc in Compile).value
      val binT = (packageBin in Test).value
      val srcT = (packageSrc in Test).value
      val docT = (packageDoc in Test).value

      (dir * "*.md").pair(rebase(dir, projectName)) ++
        (dir / "resources" ***).pair(rebase(dir, projectName)) ++
        addIfExists(dir, ".classpath") ++
        addIfExists(bin, projectName + "/lib/" + bin.name) ++
        addIfExists(binT, projectName + "/lib/" + binT.name) ++
        addIfExists(src, projectName + "/lib.sources/" + src.name) ++
        addIfExists(srcT, projectName + "/lib.sources/" + srcT.name) ++
        addIfExists(doc, projectName + "/lib.javadoc/" + doc.name) ++
        addIfExists(docT, projectName + "/lib.javadoc/" + docT.name)
    },

    artifacts <+= (name in Universal) { n => Artifact(n, "zip", "zip", Some("resource"), Seq(), None, Map()) },
    packagedArtifacts <+= (packageBin in Universal, name in Universal) map { (p, n) =>
      Artifact(n, "zip", "zip", Some("resource"), Seq(), None, Map()) -> p
    }
  )
}
