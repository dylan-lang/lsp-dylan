Module: lsp-dylan-impl
Synopsis: The ubiquitous misc.


// Maps handler name strings like "textDocument/definition" to the
// corresponding handler function.
define constant $lsp-message-handlers = make(<string-table>);

define macro handler-definer
    { define handler ?:name (?args:*) ?:body end }
 => { define function "handle-" ## ?name (?args) ?body end;
      begin
        // This depends on the probably-unspecified behavior that case is
        // preserved for ?"name".
        $lsp-message-handlers[?"name"] := "handle-" ## ?name;
      end }
end macro;

define function invoke-message-handler
    (name :: <string>, session :: <session>, id, params) => ()
  let fn = element($lsp-message-handlers, name, default: #f);
  if (fn)
    if (*trace-messages*)
      log-debug("Invoking message handler %= with id %= and params %=",
                name, id, params);
    end;
    block ()
      fn(session, id, params);
    exception (err :: <error>, test: method (_) ~*debug-mode* end)
      log-error("Error handling message %= (id=%s): %s",
                name, id, err);
    end;
  else
    // Respond to any other request with an not-implemented error.
    // Drop notifications, which are distinguished by not having an id.
    log-debug("%s method %= is not yet implemented.",
              if (id) "Request" else "Notification" end,
              name);
    if (id)
      send-error-response(session, id, $method-not-found);
    end;
  end;
end function;
