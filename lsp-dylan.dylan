Module: lsp-dylan
Synopsis: Test stuff for language server protocol
Author: Peter
Copyright: 2019

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
/*
define function dump(t :: <table>) => ()
  format(*standard-error*, "Table Dump\n==========\n");
  for (v keyed-by k in key-sequence(t))
    format(*standard-error*,
           "%s-->%s(%s)\n",
           k,
           v, object-class(v));
  end for;
end;
*/
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
end method write-json-message;

define constant $session-preinit = 1;
define constant $session-active = 2;
define constant $session-shutdown = 3;
define constant $session-killed = 4;

define class <session> (<object>)
  slot id :: <integer> = 0;
  slot state :: <integer> = $session-preinit;
end class;

define generic send-raw-message(session :: <session>,
                                message :: <object>)
  => ();
define generic send-request(session :: <session>,
                            method-name :: <string>,
                            params :: <object>)
  => ();

define generic send-response(session :: <session>,
                             id :: <integer>,
                             result :: <object>)
  => ();

define generic send-error-response
  (session :: <session>,
   id :: <integer>,
   error-code :: <integer>,
   #key error-message :: false-or(<string>) = #f,
        error-data :: <object> = #f)
  => ();

define generic send-notification(session :: <session>,
                                 method-name :: <string>,
                                 params :: <object>)
  => ();

define generic receive-message (session :: <session>)
  => (message :: <object>);

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

define class <stdio-session> (<session>)
end class;

define method send-raw-message(session :: <stdio-session>,
                               message :: <object>)
    => ()
  write-json-message(*standard-output*, message);
end method;

define method send-request(session :: <session>,
                           method-name :: <string>,
                           params :: <object>)
    => ()
  let id = session.id;
  session.id := id + 1;
  let message = make-message(method-name: method-name, id: id);
  message["result"] := params;
  send-raw-message(session, message);
end method;

define method send-response(session :: <session>,
                            id :: <integer>,
                            result :: <object>)
    => ()
  let message = make-message(id: id);
  message["result"] := result;
  send-raw-message(session, message);
end method;

define method send-error-response(session :: <session>,
                                  id :: <integer>,
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
end method;

define method send-notification(session :: <session>,
                                method-name :: <string>,
                                params :: <object>)
    => ()
  let message = make-message(method-name: method-name);
  if (params)
    message["params"] := params;
  end if;
  send-raw-message(session, message);
end method;

define method receive-message (session :: <stdio-session>)
    => (message :: <object>)
  read-json-message(*standard-input*);
end method;

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
  let show-message-params = make(<string-table>);
  show-message-params["type"] := msg-type;
  show-message-params["message"] := m;
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

define function make-range(start, endp)
  let rnge = make(<string-table>);
  rnge["start"] := start;
  rnge["end"] := endp;
  rnge
end function;

define function make-position(line, character)
  let position = make(<string-table>);
  position["line"] := line;
  position["character"] := character;
  position
end function;

define function handle-workspace/symbol (session :: <session>,
                                         msg :: <string-table>)
  => ()
  let params = msg["params"];
  let query = params["query"];
  format(*standard-output*, "Query: %s\n", query);
  let symbol = make(<string-table>);
  let symbols = list(symbol);
  symbol["name"] := "fart";
  symbol["kind"] := 13;
  let location = make(<string-table>);
  location["uri"] := "lsp-dylan.dylan";
  let range = make-range(make-position(0, 0), make-position(0,5));
  location["range"] := range;
  symbol["location"] := location;
  send-response(session, msg["id"], symbols);
end function;

define function handle-textDocument/hover(session :: <session>,
                                          msg :: <string-table>) => ()
  let hover = make(<string-table>);
  hover["contents"] := "What!!";
  send-response(session, msg["id"], hover);
end function;

define function main
  (name :: <string>, arguments :: <vector>)
  let msg = #f;
  let retcode = 1;
  let session = make(<stdio-session>);
  // Pre-init state
  while (session.state == $session-preinit)
    let msg = receive-message(session);
    let meth = msg["method"];
    let id = element(msg, "id", default: #f);
    select (meth by =)
      "initialize" =>
        begin
          // TODO set up server based on client capabilities
          let capabilities = json("hoverProvider", #t,
                                  "workspaceSymbolProvider", #t);
          let params = json("capabilities", capabilities);
          send-response(session, id, params);
          session.state := $session-active;
          show-info(session, "Dylan LSP server started");
        end;
      "exit" => session.state := $session-killed;
      otherwise =>
        // Respond to any request with an error, and drop any notifications
        begin
          if (id)
            send-error-response(session, id, $server-not-initialized);
          end if;
        end;
    end select;
    flush(session);
  end while;
  // Active state
  while (session.state == $session-active)
    let msg = receive-message(session);
    let meth = msg["method"];
    let id = element(msg, "id", default: #f);
    select (meth by =)
      "initialize" =>
          send-error-response(session, id, $invalid-request);
      "workspace/symbol" => handle-workspace/symbol(session, msg); 
      "textDocument/hover" => handle-textDocument/hover(session, msg); 
      // TODO handle all messages here
      "shutdown" =>
        begin
          // TODO shutdown everything
          session.state := $session-shutdown;
          send-response(session, id, $null);
          session.state := $session-shutdown;
        end;
      "exit" => session.state := $session-killed;
      otherwise =>
      // Respond to any other request with an not-implemented error.
      // Drop any other notifications
        begin
          format(*standard-error*, "Method '%s' is not implemented\n", meth);
          force-output(*standard-error*);
          if (id)
            send-error-response(session, id, $method-not-found);
          end if;
        end;
    end select;
    flush(session);
  end while;
  // Shutdown state
  while (session.state == $session-shutdown)
    let msg = receive-message(session);
    let meth = msg["method"];
    let id = element(msg, "id", default: #f);
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
