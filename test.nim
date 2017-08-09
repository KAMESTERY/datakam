
import unittest

proc printf(formatstr: cstring) {.importc: "printf", varargs, header: "<stdio.h>".}

proc is_leap_year(year: cint): bool {.importc}

suite "Check Lib Slapman":
  echo "suite setup: run once before the tests"

  setup:
    echo "Run before each Test"

  teardown:
    echo "Run after each Test"

  test "Check is_leap_year":

    let verify = proc(year: cint): auto =
      let
        ly: bool = is_leap_year(year)
        ly_str = if ly: "true" else: "false"
      printf("Is %d a leap year? %s\n", year, ly_str)
      return ly

    var ly = verify(2016)

    check ly == true

    ly = verify(2017)

    check ly == false

