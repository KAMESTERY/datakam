# Package

version       = "0.1.0"
author        = "outcastgeek"
description   = "Native Backend for Slapman"
license       = "proprietary"

binDir = "bin"

bin = @["trygui"]

srcDir = "src/nim"

# Dependencies

requires "nim >= 0.17.0"

requires "nimx >= 0.1"

task test, "Runs the test suite":
  exec "docker run --rm -v `pwd`:/usr/src/app -w /usr/src/app nimlang/nim:alpine nim c -r tests/nim/test.nim && rm -rf tests/nim/test"

