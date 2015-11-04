[![Build Status](https://api.travis-ci.org/wolfe-pack/wolfe.svg)](https://travis-ci.org/wolfe-pack/wolfe)
[![Stories in Ready](https://badge.waffle.io/wolfe-pack/wolfe.svg?label=ready&title=Ready)](http://waffle.io/wolfe-pack/wolfe)
<!-- [![Coverage Status](https://coveralls.io/repos/wolfe-pack/wolfe/badge.png)](https://coveralls.io/r/wolfe-pack/wolfe) -->
Please note that Wolfe is in very early alpha stage, so use it at your own risk.
Installation
=============
The easiest way to start a wolfe project is via [g8](https://github.com/n8han/giter8):
```
g8 wolfe-pack/wolfe
```

If you want to incorporate wolfe into an existing sbt project, add to your build file:
```
resolvers ++= Seq(
  "Wolfe Release" at "http://homeniscient.cs.ucl.ac.uk:8081/nexus/content/repositories/releases",
  "Wolfe Snapshots" at "http://homeniscient.cs.ucl.ac.uk:8081/nexus/content/repositories/snapshots")

libraryDependencies ++= Seq(
  "ml.wolfe" %% "wolfe-core" % "0.5.0",
  "ml.wolfe" %% "wolfe-examples" % "0.5.0"
)
```

Extending Wolfe
===============
To extend wolfe first clone this repository
```
git clone git@github.com:wolfe-pack/wolfe.git
```

Since sbt support has been integrated into [Intellij Idea](https://www.jetbrains.com/idea/) version 13, simply importing `wolfe` as a new project in Intellij Idea causes all dependencies to be resolved. Plus, Intellij Idea automatically refreshes the project when `Build.scala` is changed. 


Compilers
=========

## Torch
- Requires a [Torch](http://torch.ch/) installation
- Requires `dkjson`
```
luarocks install dkjson
```
- Run the Torch server
```
bin/wolfe-torch 
```

## ND4S
TODO
