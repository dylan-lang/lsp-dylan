Module: lsp-dylan-impl
Synopsis: Support routines for json-rpc
Author: Peter
Copyright: 2020

// Headers for the JSONRPC call
define variable $content-length = "Content-Length";

define function print-json-to-string
    (object, #key indent, sort-keys?) => (json :: <string>)
  with-output-to-string (s)
    print-json(object, s, indent: indent, sort-keys?: sort-keys?)
  end
end function;

// Read the header part from a stream and return a
// table of the (key, value) pairs.
// Returns #f on error.
define function read-headers(stm)
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

define method read-json-message (stream :: <stream>) => (json :: <object>)
  let hdrs = read-headers(stream);
  if (hdrs)
    let content-length = element(hdrs, $content-length, default: "0");
    let content-length = string-to-integer(content-length);
    let data = read(stream, content-length);
    parse-json(data)
  end
end method read-json-message;

/* Write a message with the base protocol headers
 * See: https://microsoft.github.io/language-server-protocol/specification#headerPart
 * We always assume the default encoding.
 */
define method write-json-message
    (stream :: <stream>, json :: <string>) => ()
  let content-length = size(json);
  write(stream, $content-length);
  write(stream, ": ");
  write(stream, integer-to-string(content-length));
  write(stream, "\r\n\r\n");
  write(stream, json);
end method;

define class <equal-table> (<table>)
end class;

define method table-protocol
    (table :: <equal-table>) => (compare :: <function>, hash :: <function>)
  values(\=, object-hash)
end method;

// Session lifecycle constants.
define constant $session-preinit = 1;
define constant $session-active = 2;
define constant $session-shutdown = 3;
define constant $session-killed = 4;

// Manage one connection to a server, including life-cycle.
define class <session> (<object>)
  // Next ID to use in a request/notification.
  slot session-id :: <integer> = 0;
  // Current state, see $session-preinit et al.
  slot session-state :: <integer> = $session-preinit;
  // Table of message-handler functions keyed by ID.
  constant slot session-callbacks = make(<equal-table>);
  // Root path or URI
  slot session-root = #f;
end class;

define generic send-raw-message
    (session :: <session>, message :: <object>) => ();
define generic receive-raw-message
    (session :: <session>) => (message :: <object>);

/*
 * Send a request message.
 * Optionally, register a callback to be called with the response
 * to this message.
 * The callback is a function as defined with 'define message-handler'.
 */
define generic send-request
    (session :: <session>, method-name :: <string>, params :: <object>,
     #key callback) => ();

/*
 * Send the response to a request with identifier id.
 * This applies to a successful request.
 */
define generic send-response
    (session :: <session>, id :: <object>, result :: <object>) => ();

/*
 * Send an error response to the request with identifier id.
 * Optionally include a human-readable error message and extra data
 */
define generic send-error-response
    (session :: <session>, id :: <object>, error-code :: <integer>,
     #key error-message, error-data)
 => ();

/*
 * Send an LSP notification-type message.
 * This has a method name but no ID because it isn't replied to
 */
define generic send-notification
    (session :: <session>, method-name :: <string>, params :: <object>) => ();

/*
 * Get the next message.
 * If the message is a notification or request, return it
 * for processing. If it is a response to a request sent
 * by the server, look up the reponse callback and call it.
 */
define generic receive-message
    (session :: <session>)
 => (method-name :: <string>, id :: <object>, params :: <object>);

/*
 * Flush any pending messages through the connection.
 */
define generic flush (session :: <session>) => ();

/*
 * Make the 'skeleton' of a JSONRPC 2.0 message.
 */
define function make-message (#key method-name, id)
  let msg = json("jsonrpc", "2.0");
  if (method-name)
    msg["method"] := method-name;
  end;
  if (id)
    msg["id"] := id
  end;
  msg
end function;

define method send-notification
    (session :: <session>, method-name :: <string>, params :: <object>)
 => ()
  let message = make-message(method-name: method-name);
  if (params)
    message["params"] := params;
  end;
  send-raw-message(session, message);
  if (*trace-messages*)
    log-debug("send-notification: %=", method-name);
  end;
end method;

/** receive a request or response.
 * If it is a request, return the request method, id and params.
 * If it is a response (to a request we sent to the client), look
 * up the callback, call it and loop round for another message.
 */
define method receive-message
    (session :: <session>)
 => (method-name :: <string>, id, params);
  block (return)
    let message = #f;
    while (message := receive-raw-message(session))
      let method-name = element(message, "method", default: #f);
      let id = element(message, "id", default: #f);
      let params = element(message, "params", default: #f);
      if (method-name)
        // Received a request or notification
        return(method-name, id, params);
      else
        // Received a response
        if (*trace-messages*)
          log-debug("receive-message: got id %=", id);
        end;
        let func = element(session.session-callbacks, id, default: #f);
        if (func)
          remove-key!(session.session-callbacks, id);
          func(session, id, params);
        end;
      end;
    end while;
  end block;
end method;

define method send-request
    (session :: <session>, method-name :: <string>, params :: <object>,
     #key callback :: false-or(<function>))
 => ()
  let id = session.session-id;
  session.session-id := id + 1;
  let message = make-message(method-name: method-name, id: id);
  if (params)
    message["params"] := params;
  end if;
  if (callback)
    session.session-callbacks[id] := callback;
  end if;
  send-raw-message(session, message);
end method;

define method send-response
    (session :: <session>, id :: <object>, result :: <object>) => ()
  let message = make-message(id: id);
  message["result"] := result;
  send-raw-message(session, message);
end method;

define method send-error-response
    (session :: <session>, id :: <object>, error-code :: <integer>,
     #key error-message :: false-or(<string>),
          error-data)
 => ()
  let message = make-message(id: id);
  let params = json("code", error-code,
                    "message", error-message | default-error-message(error-code));
  if (error-data)
    params["data"] := error-data;
  end if;
  message["error"] := params;
  send-raw-message(session, message);
end method;

/*
 * A session communicating over standard in/out.
 * This is the only one implemented for now.
 */
define class <stdio-session> (<session>)
  constant slot session-input-stream :: <stream>,
    required-init-keyword: input-stream:;
  constant slot session-output-stream :: <stream>,
    required-init-keyword: output-stream:;
end class;

define method send-raw-message
    (session :: <stdio-session>, message :: <object>) => ()
  let str :: <string> = print-json-to-string(message);
  if (*trace-messages*)
    log-debug("Sent JSON:\n%s",
              print-json-to-string(reduce-verbosity(message), indent: 2, sort-keys?: #t));
  end;
  write-json-message(session.session-output-stream, str);
end method;

define method receive-raw-message
    (session :: <stdio-session>) => (message :: <object>)
  let json = read-json-message(session.session-input-stream);
  if (*trace-messages*)
    log-debug("Received JSON:\n%s",
              print-json-to-string(reduce-verbosity(json), indent: 2, sort-keys?: #t));
  end;
  json
end method;

define method flush (session :: <stdio-session>) => ()
  force-output(session.session-output-stream);
end method;

// Replace the value of any attribute named "text" with a trimmed version of the string.
// We rely on the fact that the attribute is always named "text" but I don't know this to
// be true for all places where the full document is sent/received.
define function reduce-verbosity
    (data :: <string-table>) => (elided :: <string-table>)
  iterate deep-copy (thing = data)
    select (thing by instance?)
      <table> =>
        let t = make(thing.object-class);
        for (v keyed-by k in thing)
          t[k] := if (k = "text" & instance?(v, <string>) & v.size > 200)
                    concatenate(copy-sequence(v, end: 200), "[...]")
                  else
                    deep-copy(v)
                  end;
        end;
        t;
      <sequence> =>
        map(deep-copy, thing);
      otherwise =>
        thing;
    end
  end
end function;
