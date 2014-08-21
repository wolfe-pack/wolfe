[![Build Status](https://api.travis-ci.org/wolfe-pack/wolfe.png)](https://travis-ci.org/wolfe-pack/wolfe)
[![Coverage Status](https://coveralls.io/repos/wolfe-pack/wolfe/badge.png)](https://coveralls.io/r/wolfe-pack/wolfe)
Installation
============
The easiest way to start a wolfe project is via [https://github.com/n8han/giter8](g8):
```
g8 wolfe-pack/wolfe
```

If you want to incorporate wolfe into an existing sbt project, add to your build file:
```
resolvers ++= Seq(
  "Wolfe Release" at "http://homeniscient.cs.ucl.ac.uk:8081/nexus/content/repositories/releases",
  "Wolfe Snapshots" at "http://homeniscient.cs.ucl.ac.uk:8081/nexus/content/repositories/snapshots")

libraryDependencies ++= Seq(
  "ml.wolfe" %% "wolfe-core" % "0.3.0-SNAPSHOT",
  "ml.wolfe" %% "wolfe-examples" % "0.3.0-SNAPSHOT"
)
```

