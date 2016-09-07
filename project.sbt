
sbtPlugin := false

name := "imce.oti.mof.magicdraw.dynamicscripts"

description := "JPL IMCE MagicDraw Dynamic Scripts for operating on MD models through the OMG Tool-Neutral Interoperability (OTI) MOF Information Schema & API"

moduleName := name.value

organization := "org.omg.tiwg"

homepage := Some(url(s"https://github.com/TIWG/${moduleName.value}"))

organizationName := "OMG Tool-Infrastructure Working Group"

organizationHomepage := Some(url(s"https://github.com/TIWG"))

git.remoteRepo := s"git@github.com:TIWG/${moduleName.value}"

startYear := Some(2016)

scmInfo := Some(ScmInfo(
  browseUrl = url(s"https://github.com/TIWG/${moduleName.value}"),
  connection = "scm:"+git.remoteRepo.value))

developers := List(
  Developer(
    id="NicolasRouquette",
    name="Nicolas F. Rouquette",
    email="nicolas.f.rouquette@jpl.nasa.gov",
    url=url("https://github.com/NicolasRouquette")))

