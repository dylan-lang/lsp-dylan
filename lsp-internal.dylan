Module: lsp-dylan-impl
Synopis: General constants and globals for LSP
Author: Peter
Copyright: 2020

// Options etc.
// Server started with --debug command line option?
define variable *debug-mode* :: <boolean> = #f;
// LSP client asked to trace messages?
define variable *trace-messages* :: <boolean> = #f;
// LSP client asked to trace in more detail?
define variable *trace-verbose* :: <boolean> = #f;
