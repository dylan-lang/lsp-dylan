Module: lsp-dylan
Synopsis: Test stuff for language server protocol
Author: Peter
Copyright: 2019

// Options etc.
define variable *debug-mode* :: <boolean> = #f;
define variable *trace-messages* :: <boolean> = #f;
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
            end if;
          else
            // error case
            return(#f)
          end if;
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
      end if;
    end for;
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
  end for;
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
    parse-json(data)
  else
    #f
  end if;
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

define function make-message(#key method-name = #f, id = #f)
  let msg = make(<string-table>);
  msg["jsonrpc"] := "2.0";
  if (method-name)
    msg["method"] := method-name;
  end if;
  if (id)
    msg["id"] := id
  end if;
  msg
end function;

define method send-notification(session :: <session>,
                                method-name :: <string>,
                                params :: <object>)
    => ()
  let message = make-message(method-name: method-name);
  if (params)
    message["params"] := params;
  end if;
  send-raw-message(session, message);
  if (*trace-messages*)
    local-log("Server: send notification '%s'\n", method-name);
  end if; 
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
        end if; 
        // Received a request or notification
        return (method-name, id, params);
      else
        // Received a response
        if (*trace-messages*)
          local-log("Server: receive response\n");
        end if; 
        let func = element(session.callbacks, id, default: #f);
        if (func)
          remove-key!(session.callbacks, id);
          func(session, params);
        end if;
      end if;
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
  message["params"] := params;
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
  end if; 
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
  end if; 
end method;

define class <stdio-session> (<session>)
end class;

define method send-raw-message(session :: <stdio-session>,
                               message :: <object>)
    => ()
  write-json-message(*standard-output*, message);
end method send-raw-message;

define method receive-raw-message(session :: <stdio-session>)
  => (message :: <object>)
  read-json-message(*standard-input*)
end method receive-raw-message;

define method flush(session :: <stdio-session>)
    => ()
  force-output(*standard-output*);
end method;

// Error codes.
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
end function default-error-message;

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

define function handle-textDocument/hover(session :: <session>,
                                          id :: <object>,
                                          params :: <object>) => ()
  // TODO this is only a dummy
  let text-document = params["textDocument"];
  let uri = text-document["uri"];
  let position = params["position"];
  let (l, c) = decode-position(position);
  let txt = format-to-string("Hover %s (%d/%d)", uri, l + 1, c + 1); 
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
  local-log("File %s of type %s, version %s, length %d\n",
            uri, languageId, version, size(text));
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
  show-info(session, format-to-string("debug: %s, messages: %s, verbose: %s", *debug-mode*, *trace-messages*, *trace-verbose*));
  let in-stream = make(<string-stream>);
  let out-stream = make(<string-stream>, direction: #"output");
  *server* := start-compiler(in-stream, out-stream);
  *project* := open-project(*server*, "lsp-dylan");
  show-info(session, format-to-string("Compiler started:%=, project %=", *server*, *project*));
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
          let capabilities = json("hoverProvider", #t,
                                  "textDocumentSync", 1,
                                  "workspaceSymbolProvider", #t);
          let params = json("capabilities", capabilities);
          send-response(session, id, params);
          session.state := $session-active;
end function;

/* Document Management */
define constant $documents = make(<string-table>);
// Represents one open file (given to us by textDocument/didOpen)
define class <open-document> (<object>)
    slot uri;
    slot lines;
end class;
define function register-file(uri, contents)
  let lines = split-lines(contents);
  let doc = make(<open-document>, uri: uri, lines: lines);
  $documents[uri] := doc;
end function;
  
define function unregister-file(uri)
  
end function;
// get the symbol at the given position
define method text-at-position(doc :: <open-document>, line, col)
 => (text :: <string>)
  let text = doc.lines[line];
  text;
end method;

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
