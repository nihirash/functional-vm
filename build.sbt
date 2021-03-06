import Dependencies._

lazy val virtualMachine = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "com.nihirash",
      scalaVersion := "2.12.5",
      version      := "0.1.0-SNAPSHOT"
    )),
    mainClass in assembly := Some("com.nihirash.virtualmachine.VirtualMachineApp"),
    assemblyJarName in assembly := "virtualmachine.jar",
    name := "VirtualMachine",
    libraryDependencies ++= Seq(
      scalaTest % Test
      )
  )
