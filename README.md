# Dylan Language Server

This is an implementation of the [Language Server
Protocol](https://microsoft.github.io/language-server-protocol/) for
Dylan.


## Current Status

As of 2024-04-19, the server implements

* Jump to declaration
* Jump to definition
* Diagnostics (i.e., compiler warnings)
* Hover (i.e., argument lists)

When applied to a symbol which is bound to a generic function, "jump to
definition" will show a list containing the generic function and its specific
methods, whereas "jump to declaration" will jump straight to the generic
function.

See documentation/source/index.rst for full documentation.
