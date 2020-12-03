val dottyVersion = "3.0.0-M2"

lazy val day01 = dayProject(1)


lazy val common = project
  .in(file("days/common"))
  .settings(
    name := f"Advent-of-Code 2020: Commons",
    version := "0.1.0",
    scalaVersion := dottyVersion,

    libraryDependencies ++= Seq(
      "com.github.pathikrit" % "better-files_2.13" % "3.9.1",
      "com.novocode" % "junit-interface" % "0.11" % "test"
    )
  )

def dayProject(day: Int) = Project.apply(f"day_$day%02d", file(f"days/$day%02d"))
  .settings(
    name := f"Advent-of-Code 2020: Day $day%2d",
    version := "0.1.0",
    scalaVersion := dottyVersion,

    libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % "test"
  )
  .dependsOn(common)