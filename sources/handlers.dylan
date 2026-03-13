Module: lsp-dylan-impl
Synopsis: Language Server Protocol (LSP) server for Dylan
Author: Peter
Copyright: 2019

// Handlers are roughly grouped together by type. For example, initialization,
// textDocument/*, workspace/*, etc.  The handler is often a thin wrapper that extracts
// the necessary data from the protocol messages and then calls other functions.

// Initialize logging/tracing and store the workspace root for later.  Transmit
// the static capabilities of this server.  In the future we can register
// capabilities dynamically by sending messages back to the client; this seems
// to be the preferred 'new' way to do things.
// https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#initialize
define handler initialize
    (session :: <session>, id, params)
  let trace = element(params, "trace", default: "off");
  session.session-trace := name-to-trace-value(trace);
  // The initialize message may be received multiple times and we don't want
  // to change the working directory each time. Need to re-use the same _build
  // directory to keep build times short. (Should this be ~== $session-active ?)
  if (session.session-state == $session-preinit)
    let root-uri  = element(params, "rootUri", default: #f);
    let root-path = element(params, "rootPath", default: #f);
    // TODO(cgay): Both rootPath and rootUri are deprecated in favor of
    // workspaceFolders, but lsp-mode doesn't send workspaceFolders.
    // Does VS Code send it?
    session.session-root := find-workspace-root(root-uri, root-path);
    // lsp-dylan startup code stuffs the log file into the params.
    let log-file = params[$lsp-log-file-key];
    if (session.session-root)
      working-directory() := session.session-root;
      // If log-file is relative, put it in the project root by default.
      log-file := merge-locators(session.session-root,
                                 as(<file-locator>, log-file))
    end;
    // TBD whether our logging is totally redundant with sending `$/logTrace` messages.
    *log* := make(<log>,
                  name: "lsp-dylan",
                  level: select (session.session-trace)
                           $trace-messages, $trace-verbose => $debug-level;
                           otherwise => $info-level;
                         end,
                  targets: list($stderr-log-target,
                                make(<rolling-file-log-target>,
                                     pathname: log-file)));
    session.session-state := $session-active
  end;
  // Now that logging has been configured...
  if (session.trace-messages?)
    log-debug("Received LSP 'initialize' message with ID %d:\n%s",
              id, print-json-to-string(reduce-verbosity(params),
                                       indent: 2, sort-keys?: #t));
  end;

  // Return the capabilities of this server
  // TODO(cgay): diagnosticProvider
  let capabilities = json("hoverProvider", #t,
                          "textDocumentSync", 1,
                          "declarationProvider", #t,
                          "definitionProvider", #t,
                          "referencesProvider", #t,
                          "workspaceSymbolProvider", #t);
  let response-params
    = json("capabilities", capabilities,
           // TODO: send server version
           "serverInfo", json("name", "Dylan LSP Server"));
  send-response(session, id, response-params);
  log-info("Workspace root: %s", session.session-root);
  log-info("Debug server?: %=", *debug-server?*);
  log-info("Trace: %s", trace);
  log-info("Dylan LSP server initialized.");
end handler;

// Handler for 'initialized' message.
//
// Example: {"jsonrpc":"2.0","method":"initialized","params":{}}
//
// Here we will register the dynamic capabilities of the server with the client.
// Note we don't do this yet, any capabilities are registered statically in the
// 'initialize' message.
// Here also we will start the compiler session.
define handler initialized
    (session :: <session>, id, params)
  /* Commented out because we don't need to do this (yet)
  let hregistration = json("id", "dylan-reg-hover",
                           "method", "textDocument/hover");
  let oregistration = json("id", "dylan-reg-open",
                           "method", "textDocument/didOpen");

  send-request(session, "client/registerCapability", json("registrations", list(hregistration, oregistration)),
               callback: method(session, params)
                           log-debug("Callback called back..%s", session);
                           show-info(session, "Thanks la")
                         end);
*/
  show-info(session, "Dylan LSP server initialized");
  let in-stream = make(<string-stream>);
  let out-stream = make(<string-stream>, direction: #"output");
  for (var in list("OPEN_DYLAN_RELEASE",
                   "OPEN_DYLAN_RELEASE_BUILD",
                   "OPEN_DYLAN_RELEASE_INSTALL",
                   "OPEN_DYLAN_RELEASE_REGISTRIES",
                   "OPEN_DYLAN_USER_BUILD",
                   "OPEN_DYLAN_USER_INSTALL",
                   "OPEN_DYLAN_USER_PROJECTS",
                   "OPEN_DYLAN_USER_REGISTRIES",
                   "OPEN_DYLAN_USER_ROOT",
                   "PATH"))
    log-debug("initialized: %s=%s", var, environment-variable(var));
  end;
  send-request(session, "workspace/workspaceFolders", #f,
               callback: handle-workspace/workspaceFolders);
  *dylan-compiler* := start-compiler(in-stream, out-stream);
end handler;

define handler exit
    (session :: <session>, id, params)
  session.session-state := $session-killed;
end handler;

// --------------------------------------------------------------------------------
// Workspace handlers

define handler workspace/workspaceFolders
    (session :: <session>, id, params)
  // TODO: handle multi-folder workspaces.
  log-debug("Workspace folders were received: %=", params);
end handler;

define handler workspace/symbol
    (session :: <session>, id, params)
  // TODO this is only a dummy
  let query = params["query"];
  log-debug("Query: %s", query);
  let range = make-lsp-range(make-lsp-position(0, 0), make-lsp-position(0, 5));
  let symbols = list(json("name", "a-name",
                          "kind", 13,
                          "location", json("range", range,
                                           "uri", "file:///home/peter/Projects/lsp-dylan/lsp-dylan.dylan")));
  send-response(session, id, symbols);
end handler;

define handler workspace/didChangeConfiguration
    (session :: <session>, id, params)
  // NOTE: vscode always sends this just after initialized, whereas
  // emacs does not, so we need to ask for config items ourselves and
  // not wait to be told.

  // Nothing here yet...
end handler;

// --------------------------------------------------------------------------------
// textDocument handlers

// TODO: make this configurable.
define constant *module-name-replacements*
  = begin
      let t = make(<string-table>);
      t[":dylan:dylan"] := "";
      t
    end;

// Format symbol description into a hover message.
define function format-hover-message
    (text :: <string>) => (hover :: <string>)
  let lines = copy-sequence(split-lines(text), start: 1); // Remove source location info.
  let msg = join(lines, " ", key: strip);
  for (want keyed-by got in *module-name-replacements*)
    let pos = #f;
    while (pos := subsequence-position(msg, got))
      msg := replace-subsequence!(msg, want, start: pos, end: pos + got.size);
    end;
  end;
  msg
end function;

// Show information about a symbol when the pointer moves over it.
// https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_hover
define handler textDocument/hover
    (session :: <session>, id, params)
  with-lsp-params (params,
                   uri = "textDocument.uri",
                   line = "position.line",
                   column = "position.character")
    let doc = find-document(uri);
    if (~doc)
      log-debug("textDocument/hover: document %= not found", uri);
    else
      let module = doc.document-module;
      let name   = module & dylan-name-at-position(doc, line, column);
      let object = name   & find-environment-object(name, doc);
      let text   = object & od/environment-object-description(doc.%project, object, module);
      let msg    = text   & format-hover-message(text);
      let result
        = if (msg)
            json("contents", make-lsp-markup-content(msg, markdown?: #f))
          else
            log-debug("textDocument/hover: No data found for %s (line: %=, column: %=)",
                      doc, line, column);
            $null
          end;
      send-response(session, id, result);
    end;
  end;
end handler;

// https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_didOpen
define handler textDocument/didOpen
    (session :: <session>, id, params)
  let textDocument = params["textDocument"];
  let uri = textDocument["uri"];
  if (textDocument["languageId"] ~= "dylan")
    // Not sure how we would ever end up here, but...
    show-info("Ignoring non Dylan file: %s", uri)
  else
    let text :: <string> = textDocument["text"];
    let version :: <integer> = textDocument["version"];
    // TODO: have open-document signal an error once we've fixed
    // https://github.com/dylan-lang/lsp-dylan/issues/15
    let doc = open-document(session, uri, text, version);
    if (doc)
      $documents[uri] := doc;
    else
      show-error("Document not found: %s", uri);
    end;
  end if;
end handler;

// A document was saved. For Emacs, this is called when M-x lsp is executed on a new
// file. For now we don't care about the message at all, we just trigger a compilation of
// the associated project (if any) unconditionally.
// https://microsoft.github.io/language-server-protocol/specifications/specification-3-15/#textDocument_didSave
define handler textDocument/didSave
    (session :: <session>, id, params)
  let textDocument = params["textDocument"];
  let uri = textDocument["uri"];
  let doc = find-document(uri)
              | error("Document not found: %s", uri);
  let warnings = make(<stretchy-vector>);
  od/build-project(doc.%project,
                   // https://github.com/dylan-lang/lsp-dylan/issues/48#issuecomment-4040703053
                   link?: #f,
                   warning-callback: curry(add!, warnings),
                   error-handler: method (kind :: <symbol>, message :: <string>)
                                    log-debug("%s: %s", kind, message);
                                  end);
  log-debug("textDocument/didSave: done building %=", doc.%project);
  show-info(session, "Build complete, %s warning%s",
            if (empty?(warnings)) "no" else warnings.size end,
            if (warnings.size == 1) "" else "s" end);
  publish-diagnostics(session, uri, warnings);
end handler;

define variable *previous-warnings-by-uri* = #f;

// https://microsoft.github.io/language-server-protocol/specifications/specification-3-15/#textDocument_publishDiagnostics
// htt ps://microsoft.github.io/language-server-protocol/specifications/specification-3-15/#diagnostic
define function publish-diagnostics
     (session :: <session>, uri :: <string>, warnings :: <sequence>) => ()
  // Since textDocument/publishDiagnostics has a uri parameter it seems we have
  // to send warnings separately for each file that has them.
  let context = server-context(*dylan-compiler*);
  let project = od/context-project(context);
  local
    method source-uri (loc)
      let sr = loc & loc.source-location-source-record;
      if (sr)
        locator-to-file-uri(sr.source-record-location)
      end
    end method,
    method source-range (loc)
      let sr = loc & loc.source-location-source-record;
      if (~sr)
        make-lsp-range(make-lsp-position(0, 0), make-lsp-position(0, 0))
      else
        let soff = loc.source-location-start-offset;
        // sr.source-record-start-line is the number of Dylan Interchange Format
        // header lines.
        let start-line = soff.source-offset-line + sr.source-record-start-line - 1;
        let start-col = soff.source-offset-column;
        let eoff = loc.source-location-end-offset;
        let end-line = eoff.source-offset-line + sr.source-record-start-line - 1;
        let end-col = eoff.source-offset-column;
        make-lsp-range(make-lsp-position(start-line, start-col),
                       make-lsp-position(end-line, end-col));
      end
    end method;
  let warnings-by-uri = make(<string-table>);
  for (warning in warnings)
    let loc = od/environment-object-source-location(project, warning);
    // TODO: what's the right way to present diagnostics that have no source location
    // in LSP?  If none, perhaps just associate them with the current file? lsp-mode
    // explodes if no source is given.
    let uri = if (loc) source-uri(loc) else "/tmp/none" end;
    warnings-by-uri[uri] := add!(element(warnings-by-uri, uri, default: #[]), warning);
  end;
  for (warnings keyed-by uri in warnings-by-uri)
    let diagnostics = make(<stretchy-vector>);
    for (warning in warnings)
      // Unimplemented Diagnostic fields...
      //   "code" - probably not applicable for Open Dylan
      //   "codeDescription" - a URL with more info about the error
      //   "tags" - e.g., deprecated or unused code
      //   "relatedInformation" - e.g., location of colliding definition
      //   "data" - ??
      let severity
        = if (instance?(warning, od/<serious-compiler-warning-object>))
            $diagnostic-severity-error
          else
            $diagnostic-severity-warning
          end;
      let diagnostic
        = json("uri", uri,
               "range", source-range(od/environment-object-source-location(project, warning)),
               "severity", severity,
               "source", "Open Dylan",
               "message", od/compiler-warning-full-message(project, warning));
      add!(diagnostics, diagnostic);
    end for;
    send-notification(session, "textDocument/publishDiagnostics",
                      json("uri", uri,
                           "diagnostics", diagnostics));
  end;
  // Clear diagnostics for URIs that no longer have any.
  if (*previous-warnings-by-uri*)
    for (_ keyed-by old-uri in *previous-warnings-by-uri*)
      if (~element(warnings-by-uri, old-uri, default: #f))
        send-notification(session, "textDocument/publishDiagnostics",
                          json("uri", old-uri,
                               "diagnostics", #[]));
      end;
    end;
  end;
  *previous-warnings-by-uri* := warnings-by-uri;
end function;

// I (cgay) am not sure what we're meant to do with these messages. Theoretically we
// could use them to update OD's in-memory sources, do a build or just a parse, and
// report diagnostics, but currently we only build and report diagnostics on /didSave.
// Housel points out that a library-wide rebuild will often be too slow. But with the
// right OD interfaces maybe....
// https://microsoft.github.io/language-server-protocol/specifications/specification-3-15/#textDocument_didChange
define handler textDocument/didChange
    (session :: <session>, id, params)
  let text-document = params["textDocument"];
  let uri = text-document["uri"];
  let document = element($documents, uri, default: #f);
  if (document)
    let changes = params["contentChanges"];
    for (change in changes)
      apply-change(session, document, change);
    end;
  else
    // TODO: handlers should just signal an error of a certain type and
    // invoke-message-handler should DTRT.
    show-error(session, "Document not found on server: %s", uri);
  end;
end handler;

// Apply a sequence of changes to a document. Each change is a
// TextDocumentContentChangeEvent json object that has a "text" attribute and optional
// "range" attribute. If there is no range then text contains the entire new document.
define function apply-change
    (session :: <session>, document :: <document>, change :: <string-table>) => ()
  let text = change["text"];
  let range = element(change, "range", default: #f);
  if (range)
    show-error(session, "didChange doesn't support ranges yet");
  else
    log-debug("document replaced: %s", document.%uri);
    document.%lines := split-lines(text);
  end;
end function;

// Jump to Declaration
// https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_declaration
// In 'Dylan world' this means jump to the generic function if there is one
define handler textDocument/declaration
    (session :: <session>, id, params)
  with-lsp-params (params,
                   uri = "textDocument.uri",
                   line = "position.line",
                   column = "position.character")
    let doc = find-document(uri);
    let location = $null;
    if (~doc)
      log-debug("textDocument/declaration: document not found: %=", uri);
    else
      let module = doc.document-module;
      let name = module & dylan-name-at-position(doc, line, column);
      if (name)
        let lookups = lookup-symbol(name, doc);
        if (~empty?(lookups))
          location := first(lookups);
        else
          log-debug("textDocument/declaration: name %= not found", name);
        end;
      else
        log-debug("textDocument/declaration: name is #f, nothing to lookup", name);
        show-info(session, "No name found at current position.");
      end;
    end;
    send-response(session, id, location);
  end;
end handler;


// Jump to definition.
// https://microsoft.github.io/language-server-protocol/specifications/specification-3-15/#textDocument_definition
define handler textDocument/definition
    (session :: <session>, id, params)
  with-lsp-params (params,
                   uri = "textDocument.uri",
                   line = "position.line",
                   column = "position.character")
    let doc = find-document(uri);
    let locations = $null;
    if (~doc)
      log-debug("textDocument/definition: document not found: %=", uri);
      show-error(session, "Document not found: %s", uri);
    else
      let module = doc.document-module;
      let name = module & dylan-name-at-position(doc, line, column);
      log-debug("textDocument/definition: module: %=, name: %=", module, name);
      if (name)
        locations := lookup-symbol(name, doc);
        if (empty?(locations))
          log-debug("textDocument/definition: name %=, not found", name);
        end;
      else
        log-debug("textDocument/definition: name is #f, nothing to lookup", name);
        show-info(session, "No name found at current position.");
      end;
    end;
    send-response(session, id, locations);
  end;
end handler;

// Find references to a Dylan name.
// https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_references
define handler textDocument/references
    (session :: <session>, id, params)
  with-lsp-params (params,
                   uri = "textDocument.uri",
                   line = "position.line",
                   column = "position.character",
                   include-declaration? = "context.includeDeclaration")
    let doc = find-document(uri);
    let result = $null;
    if (~doc)
      log-debug("textDocument/references: document %= not found", uri);
      show-error(session, "Document not found: %s", uri);
    else
      let module = doc.document-module;
      let name = module & dylan-name-at-position(doc, line, column);
      if (name)
        let env-object = find-environment-object(name, doc);
        if (~env-object)
          show-error(session, "No definition found for %=", name);
        else
          let references = all-references(env-object, doc.%project,
                                          include-self?: include-declaration?);
          if (~empty?(references))
            result := map(method (reference)
                            let source-location
                              = od/environment-object-source-location(doc.%project, reference);
                            source-location-to-lsp-location(source-location);
                          end,
                          references);
          end;
        end;
      end;
    end;
    send-response(session, id, result);
  end;
end handler;

// Lookup a Dylan name and return a sequence of all the LSP Locations where it is
// defined.
define function lookup-symbol
    (name :: <string>, doc :: <document>) => (lsp-locations :: <sequence>)
  let object = find-environment-object(name, doc);
  if (object)
    let defs = all-definitions(doc.%project, object);
    let locs = map(curry(od/environment-object-source-location, doc.%project), defs);
    map(source-location-to-lsp-location, locs)
  else
    #()
  end if;
end function;

// Convert a <source-location> to LSP's Location object
define function source-location-to-lsp-location
    (source-location :: <source-location>) => (location :: <object>)
  let source-record = source-location.source-location-source-record;
  let absolute-path = source-record.source-record-location;
  let (name, start-line)
    = source-line-location(source-record, source-location.source-location-start-line);
  let (name, end-line)
    = source-line-location(source-record, source-location.source-location-end-line);
  let start-column = source-location.source-location-start-column;
  let end-column = source-location.source-location-end-column;
  let uri = locator-to-file-uri(absolute-path);
  make-lsp-location(uri, start-line - 1, start-column, end-line - 1, end-column);
end function;
