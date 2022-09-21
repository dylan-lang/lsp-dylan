Module: lsp-dylan-impl
Synopsis: Support for LSP objects (Range, Position, etc) and anything
          else specific to the LSP protocol.


// --- DiagnosticSeverity ---

define constant $diagnostic-severity-error = 1;
define constant $diagnostic-severity-warning = 2;
//define constant $diagnostic-severity-information = 3;
//define constant $diagnostic-severity-hint = 4;


// --- ErrorCodes ---

// Define by JSON
define constant $parse-error :: <integer> = -32700;
define constant $invalid-request :: <integer> = -32600;
define constant $method-not-found :: <integer> = -32601;
define constant $invalid-params :: <integer> = -32602;
define constant $internal-error :: <integer> = -32603;
define constant $server-error-start :: <integer> = -32099;
define constant $server-error-end :: <integer> = -32000;
define constant $server-not-initialized :: <integer> = -32002;
define constant $unknown-error-code :: <integer> = -32001;
// Defined by LSP
define constant $request-cancelled :: <integer> = -32800;
define constant $content-modified :: <integer> = -32801;

define function default-error-message
    (code :: <integer>) => (message :: <string>)
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
    otherwise => "(code not defined)";
  end select
end function;


// --- Message types ---

define constant $message-type-error = 1;
define constant $message-type-warning = 2;
define constant $message-type-info = 3;
define constant $message-type-log = 4;

define method window/show-message
    (msg-type :: <integer>, session :: <session>, fmt :: <string>, #rest args) => ()
  let msg = apply(format-to-string, fmt, args);
  let params = json("type", msg-type, "message", msg);
  send-notification(session, "window/showMessage", params);
end method;

define constant show-error   = curry(window/show-message, $message-type-error);
define constant show-warning = curry(window/show-message, $message-type-warning);
define constant show-info    = curry(window/show-message, $message-type-info);
define constant show-log     = curry(window/show-message, $message-type-log);


// --- Structures ---

// This roughly corresponds to the Structures section of the LSP spec.

// It may be worth defining Dylan classes for these basic LSP objects, for
// clarity and type checking. Also to/from-json methods.

// Make a json Range object. bpos and epos are Position objects.
// https://microsoft.github.io/language-server-protocol/specifications/specification-3-15/#range
define function make-range (bpos, epos)
  json("start", bpos, "end", epos)
end function;

// Make json for a Position object. Line and character are both zero-based.
// https://microsoft.github.io/language-server-protocol/specifications/specification-3-15/#position
define function make-position (line, character)
  json("line", line, "character", character)
end function;

// Make json for a Location that's range.
// https://microsoft.github.io/language-server-protocol/specifications/specification-3-15/#location
define function make-location (doc :: <string>, start-line, start-character, end-line, end-character)
  let start-pos = make-position(start-line, start-character);
  let end-pos = make-position(end-line, end-character);
  json("uri", doc, "range", make-range(start-pos, end-pos))
end function;

// Decode a Position json object.  Note line and character are zero-based.
// https://microsoft.github.io/language-server-protocol/specifications/specification-3-15/#position
define function decode-position
    (position) => (line :: <integer>, character :: <integer>)
  let line = position["line"];
  let character = position["character"];
  values(line, character)
end function;

// Create a MarkupContent json object.
// https://microsoft.github.io/language-server-protocol/specifications/specification-3-15/#markupContent
define function make-markup (txt, #key markdown?)
  let kind = if (markdown?)
               "markdown"
             else
               "plaintext"
             end;
  json("value", txt,
       "kind", kind)
end function;

