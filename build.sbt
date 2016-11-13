
organization := "com.gu"
name := "facebook-news-bot"
scalacOptions ++= Seq("-feature", "-deprecation", "-unchecked", "-target:jvm-1.8", "-Xfatal-warnings")
scalacOptions in doc in Compile := Nil

scalaVersion := "2.11.8"

val CirceVersion = "0.5.0-M2"

val akkaVersion = "2.4.10"

resolvers += Resolver.bintrayRepo("hseeberger", "maven")

enablePlugins(RiffRaffArtifact, JavaAppPackaging)

val CapiVersion = "10.5"
val AwsVersion = "1.11.8"

libraryDependencies ++= Seq(
  "org.clulab" %% "processors-corenlp" % "6.0.1",
  "org.clulab" %% "processors-main" % "6.0.1",
  "org.clulab" %% "processors-models" % "6.0.1",
  "com.google.guava" % "guava" % "19.0",
  "joda-time" % "joda-time" % "2.9.4",
  "org.joda" % "joda-convert" % "1.8.1",
  "org.jsoup" % "jsoup" % "1.8.1",
  "com.google.code.findbugs" % "jsr305" % "2.0.3" % Compile,
  "com.typesafe.scala-logging" %% "scala-logging" % "3.5.0",
  "ch.qos.logback" %  "logback-classic" % "1.1.7",
  "com.gu" %% "configuration-magic-core" % "1.3.0",
  "com.gu" %% "content-api-client" % CapiVersion,
  "io.circe" %% "circe-optics" % CirceVersion,
  "io.circe" %% "circe-core" % CirceVersion,
  "io.circe" %% "circe-generic" % CirceVersion,
  "io.circe" %% "circe-parser" % CirceVersion,
  "com.typesafe.akka" %% "akka-actor" % akkaVersion,
  "com.typesafe.akka" %% "akka-contrib" % akkaVersion,
  "com.typesafe.akka" %% "akka-http-experimental" % akkaVersion,
  "com.typesafe.akka" %% "akka-http-spray-json-experimental" % akkaVersion,
  "com.github.ben-manes.caffeine" % "caffeine" % "2.3.3",
  "de.heikoseeberger" %% "akka-http-circe" % "1.10.0",
  "com.amazonaws" % "aws-java-sdk-sqs" % AwsVersion,
  "com.amazonaws" % "aws-java-sdk-dynamodb" % AwsVersion,
  "com.amazonaws" % "aws-java-sdk-ec2" % AwsVersion,
  "com.gu" %% "scanamo" % "0.7.0",
  "com.gu" % "kinesis-logback-appender" % "1.3.0",
  "net.logstash.logback" % "logstash-logback-encoder" % "4.6",
  "com.typesafe.akka" %% "akka-http-testkit" % akkaVersion,
  "org.scalatest" %% "scalatest" % "3.0.0" % "test",
  "com.gu" % "content-api-models-json" % CapiVersion % Test, //this version needs to match the one used by content-api-client
  "org.mockito" % "mockito-core" % "1.9.5"
)

startDynamoDBLocal <<= startDynamoDBLocal.dependsOn(compile in Test)
test in Test <<= (test in Test).dependsOn(startDynamoDBLocal)
testOptions in Test <+= dynamoDBLocalTestCleanup

topLevelDirectory in Universal := None
packageName in Universal := normalizedName.value

def env(key: String): Option[String] = Option(System.getenv(key))

riffRaffArtifactResources += baseDirectory.value / "logstash.conf" -> s"${riffRaffPackageName.value}/logstash.conf"

riffRaffPackageName := "facebook-news-bot"
riffRaffPackageType := (packageZipTarball in Universal).value
riffRaffUploadArtifactBucket := Option("riffraff-artifact")
riffRaffUploadManifestBucket := Option("riffraff-builds")
riffRaffManifestProjectName := s"Off-platform::${name.value}"
riffRaffManifestVcsUrl := "git@github.com:guardian/facebook-news-bot.git"
riffRaffManifestBranch := env("BRANCH_NAME").getOrElse("unknown_branch")
riffRaffBuildIdentifier := env("BUILD_NUMBER").getOrElse("DEV")
