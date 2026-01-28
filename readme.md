# Ocaml include C


## What is this project about?
The main aim is to easily include C function definitions into ocaml project without any manual writing of bindings or writing C stubs.
Just making this process as seamless and easy as possible.

Example:
```ocaml

module Stdlib
    [%%include <cstdio.h>]
end

module CHeader
    [%%include "path/to/header/file.h"]
end

let () =
    Stdlib.printf ~format:"%s" "Hello world!";
    CHeader.foo ~input1:"Bar" ~input2:"Baz";
    let arr = [2;3;1] in
    CHeader.quickSort ~callback:(fun a b -> a < b) ~arr:arr
```

## Features
- Single drop in cross platform executable!
- Build in basic types with broad API to satisfy your development needs
- Generated functions keep their argument names
- Low level C api interaction library
- Memory arenas support

## Build 
Install clang from https://releases.llvm.org/
Have dune with ocaml

### Why not generate module to Ctypes?
I want to avoid as many dependencies as possible and I had some issues on windows platform. Also the verification step of bindings is unnecessary since the bindings are generated automatically from source.

### Why generator is written in C++?
1. Because I would have to write bindings for clang parser
2. You would get into bootstrapping issue where you would have hard time auditing generated code -> Is the generated code generated wrong because of clang, bindings or logic? Having low level native include avoids that.
3. Generated single drop in executable
4. Speed