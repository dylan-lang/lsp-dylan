Module: lsp-dylan-impl
Synopsis: Support routines for json-rpc
Author: Peter
Copyright: 2020


define constant $content-length-header = "Content-Length";

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

// Make a <string-table> from a sequence of key value pairs.
// This is just for convenience.
define function json (#rest kvs) => (table :: <string-table>)
  let count :: <integer> = size(kvs);
  let table = make(<string-table>, size: floor/(count, 2));
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
    let content-length = element(hdrs, $content-length-header, default: "0");
    let content-length = string-to-integer(content-length);
    // TODO: Content-Type header
    let data = read(stream, content-length);
    parse-json(data)
  end
end method read-json-message;

// Write a message with the base protocol headers
// See: https://microsoft.github.io/language-server-protocol/specification#headerPart
// We always assume the default encoding.
define method write-json-message
    (stream :: <stream>, json :: <string>) => ()
  let content-length = size(json);
  write(stream, $content-length-header);
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

define function trace-messages? (session :: <session>) => (_ :: <boolean>)
  session.%trace ~== $trace-off
end function;

// Make the 'skeleton' of a JSONRPC 2.0 message.
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
  if (session.trace-messages?)
    log-debug("send-notification: %=", method-name);
  end;
end method;

// Receive a request or response.
// If it is a request, return the request method, id and params.
// If it is a response (to a request we sent to the client), look
// up the callback, call it and loop round for another message.
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
        if (session.trace-messages?)
          log-debug("receive-message: got id %=", id);
        end;
        let func = element(session.%callbacks, id, default: #f);
        remove-key!(session.%callbacks, id);
        if (func)
          func(session, id, params);
        else
          log-debug("No callback found for response with ID %d", id);
        end;
      end;
    end while;
  end block;
end method;

define method send-request
    (session :: <session>, method-name :: <string>, params :: <object>,
     #key callback :: false-or(<function>))
 => ()
  let id = session.%id;
  session.%id := id + 1;
  let message = make-message(method-name: method-name, id: id);
  if (params)
    message["params"] := params;
  end if;
  if (callback)
    session.%callbacks[id] := callback;
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


// Logging the hover methods is so verbose it makes the logs hard to use, so we have a
// simple mechanism to stifle them.  Not sure if it's worth implementing something like
// "define [silent] handler ...", but that would make it easier to turn logging on and
// off when working on hover or other silenced LSP methods.
define constant $do-not-log-methods = #["textDocument/hover"];

define variable *do-not-log-ids* :: <list> = #();

define function log-message-json? (json :: <object>) => (log? :: <boolean>)
  block (return)
    if (~instance?(json, <table>))
      return(#t);
    end;
    let id = element(json, "id", default: #f);
    let meth = element(json, "method", default: #f);
    if (meth & member?(meth, $do-not-log-methods, test: \=))
      id & (*do-not-log-ids* := pair(id, *do-not-log-ids*));
      // If a handler gets an error it might not send a reply so make sure our id list
      // doesn't grow without bound.
      if (*do-not-log-ids*.size > 100)
        *do-not-log-ids* := copy-sequence(*do-not-log-ids*, end: 10);
      end;
      return(#f);
    end;
    if (id & member?(id, *do-not-log-ids*))
      *do-not-log-ids* := remove!(*do-not-log-ids*, id);
      return(#f);
    end;
    #t
  end
end function;

define method send-raw-message
    (session :: <stdio-session>, message :: <object>) => ()
  let str :: <string> = print-json-to-string(message);
  if (session.trace-messages? & log-message-json?(message))
    log-debug("Sent JSON:\n%s",
              print-json-to-string(reduce-verbosity(message), indent: 2, sort-keys?: #t));
  end;
  write-json-message(session.%output-stream, str);
end method;

define method receive-raw-message
    (session :: <stdio-session>) => (message :: <object>)
  let json = read-json-message(session.%input-stream);
  if (session.trace-messages? & log-message-json?(json))
    log-debug("Received JSON:\n%s",
              print-json-to-string(reduce-verbosity(json), indent: 2, sort-keys?: #t));
  end;
  json
end method;

define method flush (session :: <stdio-session>) => ()
  force-output(session.%output-stream);
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
