Module: lsp-dylan
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


// Error codes. (defined by LSP)
let $parse-error :: <integer> = -32700;
let $invalid-request :: <integer> = -32600;
let $method-not-found :: <integer> = -32601;
let $invalid-params :: <integer> = -32602;
let $internal-error :: <integer> = -32603;
let $server-error-start :: <integer> = -32099;
let $server-error-end :: <integer> = -32000;
let $server-not-initialized :: <integer> = -32002;
let $unknown-error-code :: <integer> = -32001;
let $request-cancelled :: <integer> = -32800;
let $content-modified :: <integer> = -32801;

define function default-error-message(code :: <integer>)
    => (message :: <string>)
  select (code)
    $parse-error => "Parse error";
    $invalid-request => "Invalid request";
    $method-not-found => "Method not found";
    $invalid-params => "Invalid parameters";
    $internal-error => "Internal error";
    $server-error-start => "Server Error Start";
    $server-error-end => "Server Error End";
    $server-not-initialized => "Server not initialized";
    $unknown-error-code => "Unknown error code";
    $request-cancelled => "Request cancelled";
    $content-modified => "Content modified";
    otherwise => "(code not defined)"
  end select;
end function;
