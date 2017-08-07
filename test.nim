
proc printf(formatstr: cstring) {.importc: "printf", varargs, header: "<stdio.h>".}

proc is_leap_year(year: cint): bool {.importc}

let
    year: cint = 2017
    ly: bool = is_leap_year(year)
    ly_str = if ly: "true" else: "false"

printf("Is %d a leap year? %s\n", year, ly_str)
# discard printf("Is %d a leap year? %s\n", year, ly_str)

