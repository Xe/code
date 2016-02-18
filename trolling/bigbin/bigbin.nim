import macros

macro b(v:stmt): stmt =
  a v

macro a(v:stmt): stmt =
  b v

a:
  echo 1+1
