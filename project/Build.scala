import sbt._
import Keys._
import play.Project._

object ApplicationBuild extends Build {

  val appName         = "othello"
  val appVersion      = "1.0-SNAPSHOT"

  val appDependencies = Seq(
    "org.scalatest" %% "scalatest" % "1.9.1" % "test"
  )


  val main = play.Project(appName, appVersion, appDependencies).settings(
    // Add your own project settings here      
  )

}
