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

// @see https://github.com/jrudolph/sbt-dependency-graph/issues/113
def zipFileSelector
( a: Artifact, f: File)
: Boolean
= a.`type` == "zip" || a.extension == "zip"

def pluginFileSelector
( a: Artifact, f: File)
: Boolean
= (a.`type` == "zip" || a.`type` == "resource") &&
  a.extension == "zip" &&
  a.name.endsWith("plugin_2.11")

def dsFileSelector
( a: Artifact, f: File)
: Boolean
= (a.`type` == "zip" || a.`type` == "resource") &&
  a.extension == "zip" &&
  ! a.name.startsWith("imce.third_party.") &&
  ! a.name.endsWith("plugin_2.11") &&
  ! a.classifier.getOrElse("").startsWith("part")

// @see https://github.com/jrudolph/sbt-dependency-graph/issues/113
def fromConfigurationReport
(report: ConfigurationReport,
 rootInfo: sbt.ModuleID,
 selector: (Artifact, File) => Boolean)
: net.virtualvoid.sbt.graph.ModuleGraph = {
  implicit def id(sbtId: sbt.ModuleID): net.virtualvoid.sbt.graph.ModuleId
  = net.virtualvoid.sbt.graph.ModuleId(sbtId.organization, sbtId.name, sbtId.revision)

  def moduleEdges(orgArt: OrganizationArtifactReport)
  : Seq[(net.virtualvoid.sbt.graph.Module, Seq[net.virtualvoid.sbt.graph.Edge])]
  = {
    val chosenVersion = orgArt.modules.find(!_.evicted).map(_.module.revision)
    orgArt.modules.map(moduleEdge(chosenVersion))
  }

  def moduleEdge(chosenVersion: Option[String])(report: ModuleReport)
  : (net.virtualvoid.sbt.graph.Module, Seq[net.virtualvoid.sbt.graph.Edge]) = {
    val evictedByVersion = if (report.evicted) chosenVersion else None

    val jarFile = report.artifacts.find(selector.tupled).map(_._2)
    (net.virtualvoid.sbt.graph.Module(
      id = report.module,
      license = report.licenses.headOption.map(_._1),
      evictedByVersion = evictedByVersion,
      jarFile = jarFile,
      error = report.problem),
      report.callers.map(caller ⇒ net.virtualvoid.sbt.graph.Edge(caller.caller, report.module)))
  }

  val (nodes, edges) = report.details.flatMap(moduleEdges).unzip
  val root = net.virtualvoid.sbt.graph.Module(rootInfo)

  net.virtualvoid.sbt.graph.ModuleGraph(root +: nodes, edges.flatten)
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
      srcs pair (relativeTo(base) | flat)
    },

    projectID := {
      val previous = projectID.value
      previous.extra(
        "build.date.utc" -> buildUTCDate.value,
        "artifact.kind" -> "magicdaw.library")
    },

    git.baseVersion := Versions.version,

    resourceDirectory in Compile := baseDirectory.value / "resources",

    unmanagedClasspath in Compile ++= (unmanagedJars in Compile).value,

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
      val s = streams.value
      val up = update.value
      val mdInstallDir = (mdInstallDirectory in ThisBuild).value
      if (!mdInstallDir.exists) {

        val crossV = CrossVersion(scalaVersion.value, scalaBinaryVersion.value)(projectID.value)
        val runtimeDepGraph =
          net.virtualvoid.sbt.graph.DependencyGraphKeys.ignoreMissingUpdate.value.configuration("runtime").get
        val compileDepGraph =
          net.virtualvoid.sbt.graph.DependencyGraphKeys.ignoreMissingUpdate.value.configuration("compile").get

        val mdParts = (for {
          cReport <- up.configurations
          if cReport.configuration == "compile"
          mReport <- cReport.modules
          if mReport.module.organization == "org.omg.tiwg.vendor.nomagic"
          (artifact, archive) <- mReport.artifacts
        } yield archive).sorted

        {
          s.log.warn(s"Extracting MagicDraw from ${mdParts.size} parts:")
          mdParts.foreach { p => s.log.warn(p.getAbsolutePath) }

          val merged = File.createTempFile("md_merged", ".zip")
          println(s"merged: ${merged.getAbsolutePath}")

          val zip = File.createTempFile("md_no_install", ".zip")
          println(s"zip: ${zip.getAbsolutePath}")

          val script = File.createTempFile("unzip_md", ".sh")
          println(s"script: ${script.getAbsolutePath}")

          val out = new java.io.PrintWriter(new java.io.FileOutputStream(script))
          out.println("#!/bin/bash")
          out.println(mdParts.map(_.getAbsolutePath).mkString("cat ", " ", s" > ${merged.getAbsolutePath}"))
          out.println(s"zip -FF ${merged.getAbsolutePath} --out ${zip.getAbsolutePath}")
          out.println(s"unzip -q ${zip.getAbsolutePath} -d ${mdInstallDir.getAbsolutePath}")
          out.close()

          val result = sbt.Process(command = "/bin/bash", arguments = Seq[String](script.getAbsolutePath)).!
          require(0 <= result && result <= 2, s"Failed to execute script (exit=$result): ${script.getAbsolutePath}")
          s.log.warn(s"Extracted.")
        }

        val pluginParts = (for {
          cReport <- up.configurations
          if cReport.configuration == "compile"
          mReport <- cReport.modules
          (artifact, archive) <- mReport.artifacts
          if artifact.name.startsWith("imce.dynamic_scripts.magicdraw.plugin")
          if artifact.classifier.getOrElse("").startsWith("part")
        } yield archive).sorted

        {
          s.log.warn(s"Extracting Plugin from ${pluginParts.size} parts:")
          pluginParts.foreach { p => s.log.warn(p.getAbsolutePath) }

          val merged = File.createTempFile("plugin_merged", ".zip")
          println(s"merged: ${merged.getAbsolutePath}")

          val zip = File.createTempFile("plugin_resource", ".zip")
          println(s"zip: ${zip.getAbsolutePath}")

          val script = File.createTempFile("unzip_plugin", ".sh")
          println(s"script: ${script.getAbsolutePath}")

          val out = new java.io.PrintWriter(new java.io.FileOutputStream(script))
          out.println("#!/bin/bash")
          out.println(pluginParts.map(_.getAbsolutePath).mkString("cat ", " ", s" > ${merged.getAbsolutePath}"))
          out.println(s"zip -FF ${merged.getAbsolutePath} --out ${zip.getAbsolutePath}")
          out.println(s"unzip -q ${zip.getAbsolutePath} -d ${mdInstallDir.getAbsolutePath}")
          out.close()

          val result = sbt.Process(command = "/bin/bash", arguments = Seq[String](script.getAbsolutePath)).!
          require(0 <= result && result <= 2, s"Failed to execute script (exit=$result): ${script.getAbsolutePath}")
          s.log.warn(s"Extracted.")

        }

        // @see https://github.com/jrudolph/sbt-dependency-graph/issues/113
        val g3 = fromConfigurationReport(compileDepGraph, crossV, dsFileSelector)

        for {
          module <- g3.nodes
          if module.id.organisation != "gov.nasa.jpl.cae.magicdraw.packages"
          archive <- module.jarFile
          extractFolder = mdInstallDir / "dynamicScripts"
          _ = IO.createDirectory(extractFolder)
          _ = s.log.warn(s"*** Extracting DynamicScripts: $archive")
          _ = s.log.warn(s"*** Extract to: $extractFolder")
          files = IO.unzip(archive, extractFolder)
          _ = require(files.nonEmpty)
          _ = s.log.warn(s"*** Extracted ${files.size} files")
        } yield ()

      } else
        s.log.warn(
          s"=> use existing md.install.dir=$mdInstallDir")
    },

    unmanagedJars in Compile := {
      val prev = (unmanagedJars in Compile).value
      val base = baseDirectory.value
      val s = streams.value
      val _ = extractArchives.value

      val mdInstallDir = base / "target" / "md.package"

      val allJars = (mdInstallDir ** "*.jar").get.map(Attributed.blank)
      s.log.warn(s"=> Adding ${allJars.size} unmanaged jars")

      allJars
    }
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


      (dir * ".classpath").pair(rebase(dir, projectName)) ++
      (dir * "*.md").pair(rebase(dir, projectName)) ++
      (dir / "resources" ***).pair(rebase(dir, projectName)) ++
      addIfExists(bin, projectName + "/lib/" + bin.name) ++
      addIfExists(binT, projectName + "/lib/" + binT.name) ++
      addIfExists(src, projectName + "/lib.sources/" + src.name) ++
      addIfExists(srcT, projectName + "/lib.sources/" + srcT.name) ++
      addIfExists(doc, projectName + "/lib.javadoc/" + doc.name) ++
      addIfExists(docT, projectName + "/lib.javadoc/" + docT.name)
    },

    artifacts += {
      val n = (name in Universal).value
      Artifact(n, "zip", "zip", Some("resource"), Seq(), None, Map())
    },
    packagedArtifacts += {
      val p = (packageBin in Universal).value
      val n = (name in Universal).value
      Artifact(n, "zip", "zip", Some("resource"), Seq(), None, Map()) -> p
    }
  )
}
