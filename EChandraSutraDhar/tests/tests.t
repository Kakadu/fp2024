(** Copyright 2024-2025, Ram Prosad Chandra Sutra Dhar *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

  $ ../bin/main.exe -interpret -file manytests/typed/020simple.ml
  Fatal error: exception Sys_error("manytests/typed/020simple.ml: No such file or directory")
  Raised by primitive operation at Stdlib.open_in_gen in file "stdlib.ml", line 405, characters 28-54
  Called from Stdlib.open_in in file "stdlib.ml" (inlined), line 410, characters 2-45
  Called from Dune__exe__Main.read_file in file "bin/main.ml", line 38, characters 16-32
  Called from Dune__exe__Main.main in file "bin/main.ml", line 69, characters 23-41
  Called from Dune__exe__Main in file "bin/main.ml", line 82, characters 9-16
  [2]

  $ ../bin/main.exe -interpret -file manytests/typed/021recfun.ml
  Fatal error: exception Sys_error("manytests/typed/021recfun.ml: No such file or directory")
  Raised by primitive operation at Stdlib.open_in_gen in file "stdlib.ml", line 405, characters 28-54
  Called from Stdlib.open_in in file "stdlib.ml" (inlined), line 410, characters 2-45
  Called from Dune__exe__Main.read_file in file "bin/main.ml", line 38, characters 16-32
  Called from Dune__exe__Main.main in file "bin/main.ml", line 69, characters 23-41
  Called from Dune__exe__Main in file "bin/main.ml", line 82, characters 9-16
  [2]
