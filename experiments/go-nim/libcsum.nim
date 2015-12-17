proc add*(a, b: cint): cint {.importc, dynlib: "./libcsum.so", noSideEffect, asmNoStackFrame .}

when isMainModule:
  echo add(4, 5)
