Module: lsp-dylan
Synopsis: Support routines for json-rpc
Author: Peter
Copyright: 2020

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
*/
define function json (#rest kvs) => (table :: <string-table>)
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

/* Write a message with the base protocol headers
 * See: https://microsoft.github.io/language-server-protocol/specification#headerPart
 * We always assume the default encoding.
 */
define method write-json-message(stream :: <stream>, json :: <object>) => ()
  let str :: <string> = encode-json-to-string(json);
  let content-length = size(str);
  write(stream, $content-length);
  write(stream, ": ");
  write(stream, integer-to-string(content-length));
  write(stream, "\r\n\r\n");
  write(stream, str);
end method;

// This is just a table that uses \= to compare
// because IDs can be a number or string
define class <callback-table> (<table>)
end class;

define method table-protocol (table :: <callback-table>)
  => (compare :: <function>, hash :: <function>)
  values(\=, object-hash)
end method;

// Session lifecycle constants.
define constant $session-preinit = 1;
define constant $session-active = 2;
define constant $session-shutdown = 3;
define constant $session-killed = 4;

/* Manage one connection to a server,
* including life-cycle.
*/
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

/*
 * Send a request message.
 * Optionally, register a callback to be called with the response
 * to this message.
 * The callback is a method(session :: <session>, params :: <object>) => ()
 */
define generic send-request(session :: <session>,
                            method-name :: <string>,
                            params :: <object>,
                            #key callback :: false-or(<function>))
  => ();

/*
 * Send the response to a request with identifier id.
 * This applies to a successful request.
 */
define generic send-response(session :: <session>,
                             id :: <object>,
                             result :: <object>)
  => ();

/*
 * Send an error response to the request with identifier id.
 * Optionally include a human-readable error message and extra data
 */
define generic send-error-response
  (session :: <session>,
   id :: <object>,
   error-code :: <integer>,
   #key error-message :: false-or(<string>) = #f,
        error-data :: <object> = #f)
  => ();

/* 
 * Send an LSP notification-type message.
 * This has a method name but no ID because it isn't replied to
*/
define generic send-notification(session :: <session>,
                                 method-name :: <string>,
                                 params :: <object>)
  => ();

/* 
 * Get the next message.
 * If the message is a notification or request, return it
 * for processing. If it is a response to a request sent
 * by the server, look up the reponse callback and call it.
*/ 
define generic receive-message (session :: <session>)
  => (method-name :: <string>, id :: <object>, params :: <object>);

/*
 * Flush any pending messages through the connection.
 */
define generic flush(session :: <session>)
  => ();

/*
 * Make the 'skeleton' of a JSONRPC 2.0 message.
*/
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

/** receive a request or response.
 * If it is a request, return the request method, id and params.
 * If it is a response (to a request we sent to the client), look
 * up the callback, call it and loop round for another message.
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
          if (id)
            local-log("Server: receive request '%s - (%s)'\n", method-name, id);
          else
            local-log("Server: receive notification '%s'\n", method-name);
          end if;
        end; 
        // Received a request or notification
        return (method-name, id, params);
      else
        // Received a response
        if (*trace-messages*)
          local-log("Server: receive response (%s)\n", id);
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

define method send-request (session :: <session>,
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
    local-log("Server: send request: %s\n", encode-json-to-string(message));
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
    local-log("Server: send response %s\n", encode-json-to-string(message));
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
  params["code"] := error-code;
  params["message"] := error-message | default-error-message(error-code);
  if (error-data)
    params["data"] := error-data;
  end if;
  message["error"] := params;
  send-raw-message(session, message);
  if (*trace-messages*)
    local-log("Server: send error response: %s\n", encode-json-to-string(message));
  end; 
end method;

/* 
 * A session communicating over standard in/out.
 * This is the only one implemented for now.
 */
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
