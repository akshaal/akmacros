addSbtPlugin ("org.ensime" % "ensime-sbt-cmd" % "0.0.10")

resolvers += Resolver.url("sbt-plugin-releases", new URL("http://scalasbt.artifactoryonline.com/scalasbt/sbt-plugin-releases")) (Resolver.ivyStylePatterns)
  
addSbtPlugin("com.jsuereth" % "xsbt-gpg-plugin" % "0.6")
