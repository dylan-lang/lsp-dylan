Module: lsp-dylan
Synopsis: Test stuff for language server protocol
Author: Peter
Copyright: 2019

// Options etc.
// Server started with --debug command line option?
define variable *debug-mode* :: <boolean> = #f;
// LSP client asked to trace messages?
define variable *trace-messages* :: <boolean> = #f;
// LSP client asked to trace in more detail?
define variable *trace-verbose* :: <boolean> = #f;
// Headers for the JSONRPC call
define variable $content-length = "Content-Length";

// Read the header part from a stream and return a
// table of the (key, value) pairs.
// Returns #f on error.
define function headers(stm)
  // First read lines up to the blank line
  let lines =
    block(return)
      let lines = #();
        while (#t)
          let (chars, found?) = read-to(stm, '\r');
          if (found? & read-element(stm) == '\n')
            if (empty?(chars))
              return(lines)
            else
              lines := pair(as(<string>, chars), lines)
            end;
          else
            // error case
            return(#f)
          end;
        end while;
    end block;
  // Then split each line to get the key and value
  if (lines)
    let headers = make(<string-table>);
    for (line in lines)
      let kv = split(line, ": ");
      if (size(kv) == 2)
        let key = first(kv);
        let value = second(kv);
        headers[key] := value;
      end;
    end;
    headers
  else
    #f
  end if
end function;
/**
 * Make a string-table from a sequence of key value pairs.
 * This is just for convenience.
 * TODO use a more lightweight structure than <string-table>
 * because these are just 'write-only'.
*/
define function json(#rest kvs) => (table :: <string-table>)
  let count :: <integer> = size(kvs);
  let ts = ash(count, -1);
  let table = make(<string-table>, size: ts);
  for (i from 0 below count by 2)
    let key = kvs[i];
    let value = kvs[i + 1];
    table[key] := value;
  end;
  table
end function;

define function dump(t :: <table>) => ()
  format(*standard-error*, "Table Dump\n==========\n");
  for (v keyed-by k in key-sequence(t))
    format(*standard-error*,
           "%s-->%s(%s)\n",
           k,
           v, object-class(v));
  end for;
end;

define method read-json-message(stream :: <stream>) => (json :: <object>)
  let hdrs = headers(stream);
  if (hdrs)
    let content-length = element(hdrs, $content-length, default: "0");
    let content-length = string-to-integer(content-length);
    let data = read(stream, content-length);
    parse-json(data);
  else
    #f
  end;
end method read-json-message;

define method write-json-message(stream :: <stream>, json :: <object>) => ()
  let str :: <string> = encode-json-to-string(json);
  let content-length = size(str);
  write(stream, $content-length);
  write(stream, ": ");
  write(stream, integer-to-string(content-length));
  write(stream, "\r\n\r\n");
  write(stream, str);
end method;

define constant $session-preinit = 1;
define constant $session-active = 2;
define constant $session-shutdown = 3;
define constant $session-killed = 4;

// This is just a table that uses \= to compare
// because IDs can be a number or string
define class <callback-table> (<table>)
end class;

define method table-protocol(table :: <callback-table>)
  => (compare :: <function>, hash :: <function>)
  values(\=, object-hash)
end method;

define class <session> (<object>)
  // Next ID to use in a request/notification
  slot id :: <integer> = 0;
  // Current state, see $session-preinit et al.
  slot state :: <integer> = $session-preinit;
  // Table of functions keyed by ID. Function signature is:
  // (session :: <session>, params :: object) => ()
  constant slot callbacks = make(<callback-table>);
  // Root path or URI
  slot root :: <object> = #f;
end class;

define generic send-raw-message(session :: <session>,
                                message :: <object>)
  => ();

define generic receive-raw-message(session :: <session>)
  => (message :: <object>);

define generic send-request(session :: <session>,
                            method-name :: <string>,
                            params :: <object>,
                            #key callback :: false-or(<function>))
  => ();

define generic send-response(session :: <session>,
                             id :: <object>,
                             result :: <object>)
  => ();

define generic send-error-response
  (session :: <session>,
   id :: <object>,
   error-code :: <integer>,
   #key error-message :: false-or(<string>) = #f,
        error-data :: <object> = #f)
  => ();

define generic send-notification(session :: <session>,
                                 method-name :: <string>,
                                 params :: <object>)
  => ();

define generic receive-message (session :: <session>)
  => (method-name :: <string>, id :: <object>, params :: <object>);

define generic flush(session :: <session>)
  => ();

// Make the 'skeleton' on a JSONRPC 2.0 message.
define function make-message(#key method-name = #f, id = #f)
  let msg = make(<string-table>);
  msg["jsonrpc"] := "2.0";
  if (method-name)
    msg["method"] := method-name;
  end;
  if (id)
    msg["id"] := id
  end;
  msg
end function;

define method send-notification(session :: <session>,
                                method-name :: <string>,
                                params :: <object>)
    => ()
  let message = make-message(method-name: method-name);
  if (params)
    message["params"] := params;
  end;
  send-raw-message(session, message);
  if (*trace-messages*)
    local-log("Server: send notification '%s'\n", method-name);
  end; 
end method;

/** 
* Get the next message.
* If the message is a notification or request, return it
* for processing. If it is a response to a request sent
* by the server, look up the reponse callback and call it.
*/ 
define method receive-message (session :: <session>)
  => (method-name :: <string>, id :: <object>, params :: <object>);
  block (return)
    let message = #f;
    while (message := receive-raw-message(session))
      let method-name = element(message, "method", default: #f);
      let id =  element(message, "id", default: #f);
      let params = element(message, "params", default: #f);
      if (method-name)
        if (*trace-messages*)
          local-log("Server: receive request '%s'\n", method-name);
        end; 
        // Received a request or notification
        return (method-name, id, params);
      else
        // Received a response
        if (*trace-messages*)
          local-log("Server: receive response\n");
        end; 
        let func = element(session.callbacks, id, default: #f);
        if (func)
          remove-key!(session.callbacks, id);
          func(session, params);
        end;
      end;
    end while;
  end block;
end method;


define method send-request(session :: <session>,
                           method-name :: <string>,
                           params :: <object>,
                           #key callback :: false-or(<function>) = #f)
    => ()
  let id = session.id;
  session.id := id + 1;
  let message = make-message(method-name: method-name, id: id);
  if (params)
    message["params"] := params;
  end if;
  if (callback)
    session.callbacks[id] := callback;
  end if;
  send-raw-message(session, message);
  if (*trace-messages*)
    local-log("Server: send request '%s'\n", method-name);
  end if; 
end method;

define method send-response(session :: <session>,
                            id :: <object>,
                            result :: <object>)
    => ()
  let message = make-message(id: id);
  message["result"] := result;
  send-raw-message(session, message);
  if (*trace-messages*)
    local-log("Server: send response\n");
  end; 
end method;

define method send-error-response(session :: <session>,
                                  id :: <object>,
                                  error-code :: <integer>,
                                  #key error-message :: false-or(<string>) = #f,
                                       error-data :: <object> = #f)
    => ()
  let message = make-message(id: id);
  let params = make(<string-table>);
  params["message"] := error-message | default-error-message(error-code);
  if (error-data)
    params["data"] := error-data;
  end if;
  message["error"] := params;
  send-raw-message(session, message);
  if (*trace-messages*)
    local-log("Server: send error response\n");
  end; 
end method;

define class <stdio-session> (<session>)
end class;

define method send-raw-message(session :: <stdio-session>,
                               message :: <object>)
    => ()
  write-json-message(*standard-output*, message);
end method;

define method receive-raw-message(session :: <stdio-session>)
  => (message :: <object>)
  read-json-message(*standard-input*)
end method;

define method flush(session :: <stdio-session>)
    => ()
  force-output(*standard-output*);
end method;

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

define constant $message-type-error = 1;
define constant $message-type-warning = 2;
define constant $message-type-info = 3;
define constant $message-type-log = 4;
define method show-message(session :: <session>,
                           msg-type :: <integer>,
                           m :: <string>)
    => ()
  let show-message-params = json("type", msg-type,
                                 "message", m);
  send-notification(session, "window/showMessage", show-message-params);
end method;

define inline method show-error(session :: <session>,
                                m :: <string>)
    => ()
  show-message(session, $message-type-error, m);
end method;

define inline method show-warning(session :: <session>,
                                  m :: <string>)
    => ()
  show-message(session, $message-type-warning, m);
end method;

define inline method show-info(session :: <session>,
                               m :: <string>)
    => ()
  show-message(session, $message-type-info, m);
end method;

define inline method show-log(session :: <session>,
                              m :: <string>)
    => ()
  show-message(session, $message-type-log, m);
end method;

define function local-log(m :: <string>, #rest params) => ()
  apply(format, *standard-error*, m, params);
  force-output(*standard-error*);
end function;

define function make-range(start, endp)
  json("start", start, "end", endp);
end function;

define function make-position(line, character)
  json("line", line, "character", character);
end function;

// make a location that's a single character
define function make-location(doc, line, character)
  let pos = make-position(line, character);
  json("uri", doc, "range", make-range(pos, pos))
end;

define function decode-position(position)
 => (line :: <integer>, character :: <integer>)
  let line = as(<integer>, position["line"]);
  let character = as(<integer>, position["character"]);
  values(line, character)
end function;
    

define function make-markup(txt, #key markdown = #f)
  let kind = if (markdown)
               "markdown"
             else
               "plaintext"
             end;
  json("value", txt,
       "kind", kind);
end function;
/*
define function list-all-package-names ()
  let res = #();
  local method collect-project
            (dir :: <pathname>, filename :: <string>, type :: <file-type>)
          if (type == #"file")
            local-log("P:%=\n", filename);
            if (last(filename) ~= '~')
              res := pair(filename, res);
            end;
          end;
        end;
  let regs = find-registries(as(<string>, target-platform-name()));
  let reg-paths = map(registry-location, regs);
  for (reg-path in reg-paths)
    if (file-exists?(reg-path))
      do-directory(collect-project, reg-path);
    end;
  end;
  res
end;
*/
define function handle-workspace/symbol (session :: <session>,
                                         id :: <object>,
                                         params :: <object>)
  => ()
  // TODO this is only a dummy
  let query = params["query"];
  local-log("Query: %s\n", query);
  let range = make-range(make-position(0, 0), make-position(0,5));
  let symbols = list(json("name", "a-name",
                          "kind", 13,
                          "location", json("range", range,
                                           "uri", "file:///home/peter/Projects/lsp-dylan/lsp-dylan.dylan")));
  send-response(session, id, symbols);
end function;

/* Show information about a symbol when we hover the cursor over it
See: https://microsoft.github.io/language-server-protocol/specifications/specification-3-15/#textDocument_hover
Parameters: textDocument, position, (optional) workDoneToken 
Returns: contents, (optional) range
*/
define function handle-textDocument/hover(session :: <session>,
                                          id :: <object>,
                                          params :: <object>) => ()
  // TODO this is only a dummy
  let text-document = params["textDocument"];
  let uri = text-document["uri"];
  let position = params["position"];
  let (line, column) = decode-position(position);
  let doc = $documents[uri];
  let symbol = symbol-at-position(doc, line, column);
  let txt = format-to-string("textDocument/hover %s (%d/%d)", symbol, line + 1, column + 1); 
  let hover = json("contents", make-markup(txt, markdown: #f));
  send-response(session, id, hover);
end function;

define function handle-textDocument/didOpen(session :: <session>,
                                            id :: <object>,
                                            params :: <object>) => ()
  // TODO this is only a dummy
  let textDocument = params["textDocument"];
  let uri = textDocument["uri"];
  let languageId = textDocument["languageId"];
  let version = textDocument["version"];
  let text = textDocument["text"];
  local-log("textDocument/didOpen: File %s of type %s, version %s, length %d\n",
            uri, languageId, version, size(text));
  // Only bother about dylan files for now.  
  if (languageId = "dylan") 
    register-file(uri, text);
  end if
end function;

// Go to definition:
// See https://microsoft.github.io/language-server-protocol/specifications/specification-3-15/#textDocument_definition

define function handle-textDocument/definition(session :: <session>,
                                               id :: <object>,
                                               params :: <object>) => ()
  let text-document = params["textDocument"];
  let uri = text-document["uri"];
  let position = params["position"];
  let (l, c) = decode-position(position);
  let doc = element($documents, uri, default: #f);
  let symbol = if (doc) symbol-at-position(doc, l, c) else "Cheese" end;
  let (doc, line, char) = lookup-symbol(session, symbol);
  doc := uri;
  let location = make-location(doc, line, char);
  local-log(">>%s<<\n", location); 
  send-response(session, id, location);
end function;

define function handle-workspace/didChangeConfiguration(session :: <session>,
                                            id :: <object>,
                                            params :: <object>) => ()
  local-log("Did change configuration\n");
  show-info(session, "The config was changed");
end function;

define function handle-initialized(session :: <session>,
                                            id :: <object>,
                                            params :: <object>) => ()
  /* Commented out because we don't need to do this (yet)
  let hregistration = json("id", "dylan-reg-hover",
                           "method", "textDocument/hover");
  let oregistration = json("id", "dylan-reg-open",
                           "method", "textDocument/didOpen");
  
  send-request(session, "client/registerCapability", json("registrations", list(hregistration, oregistration)),
               callback: method(session, params)
                           local-log("Callback called back..%s\n", session);
                           show-info(session, "Thanks la")
                         end);
*/
  show-info(session, "Dylan LSP server started.");
  local-log("debug: %s, messages: %s, verbose: %s\n", *debug-mode*, *trace-messages*, *trace-verbose*);
  let in-stream = make(<string-stream>);
  let out-stream = make(<string-stream>, direction: #"output");

  // Test code
  local-log("Env O-D-R=%s, PATH=%s\n",
            environment-variable("OPEN_DYLAN_RELEASE"),
            environment-variable("PATH"));
  send-request(session, "workspace/workspaceFolders", #f);
  *server* := start-compiler(in-stream, out-stream);
  // TODO don't hard-code the project name and module name.
  *project* := open-project(*server*, "testproject");
  *module* := "testproject";
  local-log("Compiler started:%=, project %=\n", *server*, *project*);
  local-log("Database: %=\n", project-compiler-database(*project*));
end function;

define function handle-initialize(session :: <session>,
                                  id :: <object>,
                                  params :: <object>) => ()
  let trace = element(params, "trace", default: "off");
  select (trace by \=)
    "off" => begin
               *trace-messages* := #f;
               *trace-verbose* := #f;
             end;
    "messages" => begin
                    *trace-messages* := #t;
                    *trace-verbose* := #f;
                  end;
    "verbose" => begin
                   *trace-messages* := #t;
                   *trace-verbose* := #t;
                 end;
  end select;
  // Save the workspace root (if provided) for later.
  session.root := element(params, "rootUri", default: #f) | element(params, "rootPath", default: #f);
  // Return the capabilities of this server
  let capabilities = json("hoverProvider", #t,
                          "textDocumentSync", 1,
                          "definitionProvider", #t,
                          "workspaceSymbolProvider", #t);
  let response-params = json("capabilities", capabilities);
  send-response(session, id, response-params);
  // All OK to proceed.
  session.state := $session-active;
end function;

/* Document Management */
define constant $documents = make(<string-table>);
// Represents one open file (given to us by textDocument/didOpen)
define class <open-document> (<object>)
    constant slot uri, required-init-keyword: uri:;
    slot lines, required-init-keyword: lines:;
end class;

define function register-file(uri, contents)
  let lines = split-lines(contents);
  local-log("register-file: %s, lines: %d\n", uri, size(lines)); 
  let doc = make(<open-document>, uri: uri, lines: lines);
  $documents[uri] := doc;
end function;

/* Given a document and a position, find the symbol that this position is within 
If the position is on a space, just return ""
*/
define function symbol-at-position(doc :: <open-document>, line, column) => (symbol :: <string>)
  local-log("symbol at (%d, %d) with %d\n", line, column, doc.lines.size);
  let line = doc.lines[line];
  local method any-character?(c) => (well? :: <boolean>)
          member?(c, "abcdefghijklmnopqrstuvwxyzABCDEFGHIHJLKMNOPQRSTUVWXYZ0123456789!&*<>|^$%@_-+~?/=")
        end;
  if (any-character?(line[column]))
    let symbol-start = column;
    let symbol-end = column;
    while (symbol-start >= 0 & any-character?(line[symbol-start]))
      symbol-start := symbol-start - 1;
    end while;
    while (symbol-end < size(line) & any-character?(line[symbol-end]))
      symbol-end := symbol-end + 1;
    end while;
    copy-sequence(line, start: symbol-start + 1, end: symbol-end)
  else
    // Hovered over some 'punctuation'
    ""
  end if
end function;

define function unregister-file(uri)
  // TODO
end function;

// Look up a symbol. Return the containing doc,
// the line and column
define function lookup-symbol(session, symbol) => (doc, line, column)
  let loc = symbol-location (symbol);
  let (name, line) = source-line-location(loc.source-location-source-record,
                                          loc.source-location-start-line);
  let column = loc.source-location-start-column;
  local-log("Looking up %s and got %=\nGoing to %= (%=, %=)\n", symbol, loc, name, line, column);
  values(name, line - 1, column)
end;

define function main
  (name :: <string>, arguments :: <vector>)
  // Command line processing
  if (member?("--debug", arguments, test: \=))
    *debug-mode* := #t;
  end if;
  // Set up.
  let msg = #f;
  let retcode = 1;
  let session = make(<stdio-session>);
  // Pre-init state
  while (session.state == $session-preinit)
    let (meth, id, params) = receive-message(session);
    select (meth by =)
      "initialize" => handle-initialize(session, id, params);
      "exit" => session.state := $session-killed;
      otherwise =>
        // Respond to any request with an error, and drop any notifications
        if (id)
          send-error-response(session, id, $server-not-initialized);
        end if;
    end select;
    flush(session);
  end while;
  // Active state
  while (session.state == $session-active)
    let (meth, id, params) = receive-message(session);
    select (meth by =)
      "initialize" =>
          send-error-response(session, id, $invalid-request);
      "initialized" => handle-initialized(session, id, params);
      "workspace/symbol" => handle-workspace/symbol(session, id, params);
      "textDocument/hover" => handle-textDocument/hover(session, id, params);
      "textDocument/didOpen" => handle-textDocument/didOpen(session, id, params);
      "textDocument/definition" => handle-textDocument/definition(session, id, params);
      "workspace/didChangeConfiguration" => handle-workspace/didChangeConfiguration(session, id, params);
      // TODO handle all other messages here
      "shutdown" =>
        begin
          // TODO shutdown everything
          send-response(session, id, $null);
          session.state := $session-shutdown;
        end;
      "exit" => session.state := $session-killed;
      otherwise =>
      // Respond to any other request with an not-implemented error.
      // Drop any other notifications
        begin
          local-log("%s '%s' is not implemented\n",
                    if (id)
                      "Request"
                    else
                      "Notification"
                    end,
                    meth);
          if (id)
            send-error-response(session, id, $method-not-found);
          end if;
        end;
    end select;
    flush(session);
  end while;
  // Shutdown state
  while (session.state == $session-shutdown)
    let (meth, id, params)  = receive-message(session);
    select (meth by =)
      "exit" =>
        begin
          retcode := 0;
          session.state := $session-killed;
        end;
      otherwise =>
        // Respond to any request with an invalid error,
        // Drop any notifications
        begin
          if (id)
            send-error-response(session, id, $invalid-request);
          end if;
        end;
    end select;
    flush(session);
  end while;

  exit-application(retcode);
end function main;

main(application-name(), application-arguments());


// Local Variables:
// indent-tabs-mode: nil
// compile-command: "dylan-compiler -build lsp-dylan"
// End:
