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
      log-debug("Invoking %= with id %= and params %s",
                name, id, print-json-to-string(params));
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

// Find the workspace root. The "rootUri" LSP parameter takes precedence over
// the deprecated "rootPath" LSP parameter. We first look for a `dylan-tool`
// workspace root containing the file and then fall back to the nearest
// directory containing a `registry` directory. This should work for
// `dylan-tool` users and others equally well.
define function find-workspace-root
    (root-uri, root-path) => (root :: false-or(<directory-locator>))
  let directory
    = if (root-uri)
        file-uri-to-locator(ensure-trailing-slash(root-uri))
      elseif (root-path)
        as(<directory-locator>, root-path)
      end;
  let workspace = ws/find-workspace-file(directory)
                    & ws/load-workspace(directory: directory);
  if (workspace)
    ws/workspace-directory(workspace)
  else
    // Search up from `directory` to find the directory containing the
    // "registry" directory.
    iterate loop (dir = directory)
      if (dir)
        let registry-dir = subdirectory-locator(dir, "registry");
        if (file-exists?(registry-dir))
          dir
        else
          loop(dir.locator-directory)
        end
      end
    end
  end
end function;

define function ensure-trailing-slash
    (s :: <string>) => (s-slash :: <string>)
  if (ends-with?(s, "/"))
    s
  else
    concatenate(s, "/")
  end
end function;

// Turn a URL string received from the client into file or directory locator,
// depending on whether it ends in a '/' character.
define function file-uri-to-locator
    (url :: <string>) => (loc :: <locator>)
  assert(starts-with?(url, "file://"));
  let stripped = copy-sequence(url, start: size("file://"));
  if (ends-with?(stripped, "/"))
    as(<directory-locator>, stripped)
  else
    as(<file-locator>, stripped)
  end
end function;

// Turn a locator into a file URL string for sending back to the client.
define function locator-to-file-uri
    (loc :: <locator>) => (uri :: <string>)
  concatenate("file://", as(<string>, loc))
end function;


//// Documents

// For each file opened by the client we track a <document> object which keeps
// track of state for that document.

// Maps URI strings to <document> objects.
define constant $documents = make(<string-table>);

// Represents one open file (given to us by textDocument/didOpen)
define class <document> (<object>)
  constant slot document-uri :: <string>,
    required-init-keyword: uri:;
  slot document-project :: false-or(<project-object>) = #f,
    init-keyword: project:;
  slot document-module :: false-or(<module-object>) = #f,
    init-keyword: module:;
  slot document-contents :: <string> = "",
    init-keyword: contents:;
  slot document-lines :: <sequence>,
    required-init-keyword: lines:;
end class;

define function register-document
    (uri :: <string>, contents :: <string>) => (doc :: <document>)
  log-debug("register-document(%=)", uri);
  find-document(uri)
    | begin
        let lines = split-lines(contents);
        let doc = make(<document>, uri: uri, contents: contents, lines: lines);
        $documents[uri] := doc
      end
end function;

define generic find-document
    (id) => (doc :: false-or(<document>));

define method find-document
    (uri :: <string>) => (doc :: false-or(<document>))
  element($documents, uri, default: #f)
end method;

define method find-document
    (locator :: <file-locator>) => (doc :: false-or(<document>))
  find-document(concatenate("file://", as(<string>, locator)))
end method;

define function document-locator
    (doc :: <document>) => (locator :: <file-locator>)
  file-uri-to-locator(doc.document-uri)
end function;

// Search the open documents for a project matching `name`.
define function find-open-project
    (name :: <string>) => (p? :: false-or(<project-object>))
  block (return)
    for (doc in $documents)
      let project = doc.document-project;
      if (project & project.project-name = name)
        return(project)
      end
    end
  end
end function;

ignore(document-contents, document-contents-setter);
