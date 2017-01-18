import sbt._

object Build {
  val akkaVersion = "2.4.16"
  val catsVersion = "0.8.1"
  val shapelessVersion = "2.3.2"

  val akkaActor = "com.typesafe.akka" %% "akka-actor" % akkaVersion
  val akkaTestkit = "com.typesafe.akka" %% "akka-testkit" % akkaVersion % "test"
  val akkaContrib = "com.typesafe.akka" %% "akka-contrib" % akkaVersion
  val akkaSharding = "com.typesafe.akka" %% "akka-cluster-sharding" % akkaVersion
  val akkaDData = "com.typesafe.akka" %% "akka-distributed-data-experimental" % akkaVersion
  val akkaStream = "com.typesafe.akka" %% "akka-stream-experimental" % "2.0.1"
  val akkaTyped = "com.typesafe.akka" %% "akka-typed-experimental" % akkaVersion
  val akkaPersistence = "com.typesafe.akka" %% "akka-persistence" % akkaVersion
  val akkaPersistenceQuery = "com.typesafe.akka" %% "akka-persistence-query-experimental" % akkaVersion

  val levelDb = "org.iq80.leveldb" % "leveldb" % "0.7"

  val cats = "org.typelevel" %% "cats" % catsVersion
  val shapeless = "com.chuusai" %% "shapeless" % shapelessVersion

  val sbtIO = "org.scala-sbt" %% "io" % "0.13"

  //  val scalaAsync = "org.scala-lang.modules" %% "scala-async" % "0.9.2"
  //  val scalaJava8 = "org.scala-lang.modules" %% "scala-java8-compat" % "0.7.0"
  //
  //  val playJson = "com.typesafe.play" %% "play-json" % "2.4.0"
  //
  val scalactic = "org.scalactic" %% "scalactic" % "3.0.1"

  val scalalogging = "com.typesafe.scala-logging" %% "scala-logging" % "3.5.0"

  val scalatest = "org.scalatest" %% "scalatest" % "3.0.1" % "test"
  val logback = "ch.qos.logback" % "logback-classic" % "1.1.8"
  val junit = "junit" % "junit" % "4.11" % "test"
}