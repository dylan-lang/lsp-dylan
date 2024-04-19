Module: lsp-dylan-impl
Synopsis: Language Server Protocol (LSP) server for Dylan
Author: Peter
Copyright: 2019

define function lsp-server-top-level
    (#key debug-server? = #t, debug-opendylan? = #t) => ()
  *debug-mode?* := debug-server?;
  if (debug-opendylan?)
    enable-od-environment-debug-logging();
  end;
  let session = make(<stdio-session>,
                     input-stream: *standard-input*,
                     output-stream: *standard-output*);
  block ()
    lsp-pre-init-state-loop(session);
    lsp-active-state-loop(session);
    lsp-shutdown-state-loop(session);
  cleanup
    log-debug("lsp-server-top-level exiting: bye!");
  end;
end function;

define function lsp-pre-init-state-loop
    (session :: <session>) => ()
  while (session.session-state == $session-preinit)
    log-debug("lsp-pre-init-state-loop: waiting for message");
    let (meth, id, params) = receive-message(session);
    if (meth = "initialize" | meth = "exit")
      invoke-message-handler(meth, session, id, params);
    elseif (id)
      // Respond to any Request with an error, and drop any Notifications.
      // (Notifications have no id.)
      send-error-response(session, id, $server-not-initialized);
    end;
    flush(session);
  end while;
end function;

define function lsp-active-state-loop
    (session :: <session>) => ()
  while (session.session-state == $session-active)
    log-debug("lsp-active-state-loop: waiting for message");
    let (meth, id, params) = receive-message(session);
    invoke-message-handler(meth, session, id, params);
    flush(session);
  end while;
end function;

define function lsp-shutdown-state-loop
    (session :: <session>) => ()
  block (return)
    while (session.session-state == $session-shutdown)
      log-debug("lsp-shutdown-state-loop: waiting for message");
      let (meth, id, params) = receive-message(session);
      if (meth = "exit")
        log-debug("Dylan LSP server exiting");
        return();
      else
        // Respond to any other request with an not-implemented error.
        // Drop notifications, which are distinguished by not having an id.
        log-debug("lsp-shutdown-state-loop: %s method %= is not yet implemented.",
                  if (id) "Request" else "Notification" end,
                  meth);
        if (id)
          send-error-response(session, id, $invalid-request);
        end;
      end;
      flush(session);
    end while;
  end block;
end function;
