# Dylan Language Server

An implementation of the [Language Server
Protocol](https://microsoft.github.io/language-server-protocol/) (LSP) for Dylan.


## Current Status

As of February 2026, the server implements the following features:

* Jump to declaration
* Jump to definition
* Diagnostics (i.e., compiler warnings)
* Hover (i.e., parameter lists)
* References

When applied to a symbol which is bound to a generic function, "jump to
definition" will show a list containing the generic function and its specific
methods, whereas "jump to declaration" will jump straight to the generic
function.

See https://package.opendylan.org/lsp-dylan for full documentation.
