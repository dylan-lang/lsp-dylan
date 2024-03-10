Module: lsp-dylan-impl
Synopsis: Language Server Protocol (LSP) server for Dylan
Author: Peter
Copyright: 2019

// Handlers are roughly grouped together by type. For example, initialization,
// textDocument/*, workspace/*, etc.

// Initialize logging/tracing and store the workspace root for later.  Transmit
// the static capabilities of this server.  In the future we can register
// capabilities dynamically by sending messages back to the client; this seems
// to be the preferred 'new' way to do things.
// https://microsoft.github.io/language-server-protocol/specifications/specification-3-15/#initialize
define handler initialize
    (session :: <session>, id, params)
  // The very first received message is "initialize" (I think), and it seems
  // that for some reason it doesn't get logged, so log params here. The params
  // for this method are copious, so we log them with pretty printing.
  log-debug("initialize(%=, %=, %s)",
            session, id,
            with-output-to-string (s)
              print-json(params, s, indent: 2)
            end);
  let trace = element(params, "trace", default: "off");
  select (trace by \=)
    "off" =>
      *trace-messages* := #f;
      *trace-verbose* := #f;
    "messages" =>
      *trace-messages* := #t;
      *trace-verbose* := #f;
    "verbose" =>
      *trace-messages* := #t;
      *trace-verbose* := #t;
    otherwise =>
      log-error("initialize: trace must be 'off', 'messages' or 'verbose', not %=",
                trace);
  end select;
  log-debug("initialize: debug: %s, messages: %s, verbose: %s",
            *debug-mode*, *trace-messages*, *trace-verbose*);

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
    if (session.session-root)
      log-info("Found Dylan workspace root: %s", session.session-root);
      working-directory() := session.session-root;
    end;
    log-info("Dylan LSP server working directory: %s", working-directory());
    session.session-state := $session-active
  end;

  // Return the capabilities of this server
  // TODO(cgay): diagnosticProvider
  let capabilities = json("hoverProvider", #t,
                          "textDocumentSync", 1,
                          "declarationProvider", #t,
                          "definitionProvider", #t,
                          "referencesProvider", #t,
                          "workspaceSymbolProvider", #t);
  let response-params = json("capabilities", capabilities);
  send-response(session, id, response-params);
end handler;

/* Handler for 'initialized' message.
 *
 * Example: {"jsonrpc":"2.0","method":"initialized","params":{}}
 *
 * Here we will register the dynamic capabilities of the server with the client.
 * Note we don't do this yet, any capabilities are registered statically in the
 * 'initialize' message.
 * Here also we will start the compiler session.
 */
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
  show-info(session, "Dylan LSP server started.");
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
  show-info(session, "Opened project %s", lsp-open-project(session));
end handler;

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
  log-debug("Did change configuration");
  log-debug("Settings: %s", print-json-to-string(params));
  // TODO do something with this info.
  let settings = params["settings"];
  let dylan-settings = element(settings, "dylan", default: #f);
  let project-name = dylan-settings
                       & element(dylan-settings, "project", default: #f);
  if (project-name & ~empty?(project-name))
    *project-name* := project-name;
  end;
  //show-info(session, "The config was changed");
  lsp-open-project(session);
end handler;

// Format symbol description into a hover message.
// The description comes from the compiler database as a string with
// multiple lines - the first is a location which we don't need.
// Cut this and join the rest as one line.
define function format-hover-message
    (txt :: false-or(<string>)) => (hover :: false-or(<string>))
  if (txt)
    let lines = split-lines(txt);
    join(tail(lines), " ", key: strip);
  end if;
end function;

// Show information about a symbol when we hover the cursor over it
// See: https://microsoft.github.io/language-server-protocol/specifications/specification-3-15/#textDocument_hover
// Parameters: textDocument, position, (optional) workDoneToken
// Returns: contents, (optional) range
define handler textDocument/hover
    (session :: <session>, id, params)
  let (doc, line, column)  = textdocumentposition-to-position(params);
  if (~doc)
    let uri = params["textDocument"]["uri"];
    log-debug("textDocument/hover: document %= not found", uri);
    show-error(session, format-to-string("Document not found: %s", uri));
  else
    let module = doc.ensure-document-module;
    let symbol = symbol-at-position(doc, line, column);
    let hover = if (symbol)
                  let txt = describe-symbol(symbol, module: module);
                  let msg = format-hover-message(txt);
                  if (msg)
                    json("contents", make-lsp-markup-content(msg, markdown?: #f));
                  end;
                end;
    send-response(session, id, hover | $null);
  end if;
end handler;

define handler textDocument/didOpen
    (session :: <session>, id, params)
  // TODO this is only a dummy
  let textDocument = params["textDocument"];
  let uri = textDocument["uri"];
  let languageId = textDocument["languageId"];
  let version = textDocument["version"];
  let text = textDocument["text"];
  log-debug("textDocument/didOpen: File %s of type %s, version %s, length %d",
            uri, languageId, version, size(text));
  // Only bother about dylan files for now.
  if (languageId = "dylan")
    register-file(uri, text);
  end if;
  if (*project*)
    let file = file-uri-to-locator(uri);
    let (m, l) = od/file-module(*project*, file);
    log-debug("textDocument/didOpen: File: %= Module: %=, Library: %=",
              as(<string>, file),
              if (m) od/environment-object-primitive-name(*project*, m) end,
              if (l) od/environment-object-primitive-name(*project*, l) end);
  else
    log-debug("textDocument/didOpen: no project found");
  end if;
end handler;

// A document was saved. For Emacs, this is called when M-x lsp is executed on
// a new file. For now we don't care about the message at all, we just trigger
// a compilation of the associated project (if any) unconditionally.
// https://microsoft.github.io/language-server-protocol/specifications/specification-3-15/#textDocument_didSave
define handler textDocument/didSave
    (session :: <session>, id, params)
  let textDocument = params["textDocument"];
  let uri = textDocument["uri"];
  // TODO(cgay): obviously we should be passing uri to find-project-name here.
  let project = find-project-name();
  log-debug("textDocument/didSave: File %s, project %=", uri, project);
  if (project)
    let project-object = od/find-project(project);
    log-debug("textDocument/didSave: project = %=", project-object);
    if (project-object)
      let warnings = make(<stretchy-vector>);
      od/build-project(project-object,
                       link?: #f,
                       warning-callback: curry(add!, warnings),
                       error-handler: method (kind :: <symbol>, message :: <string>)
                                        log-debug("%s: %s", kind, message);
                                      end);
      log-debug("textDocument/didSave: done building %=", project);
      show-info(session, "Build complete, %s warning%s",
                if (empty?(warnings)) "no" else warnings.size end,
                if (warnings.size == 1) "" else "s" end);
      publish-diagnostics(session, uri, warnings);
    else
      show-error("Project %s not found.", project);
    end;
  else
    log-debug("handle-textDocument/didSave: project not found for %=", uri);
    show-error("Project %s not found.", project);
  end;
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
    show-error(session, format-to-string("Document not found on server: %s", uri));
  end;
end handler;

// Apply a sequence of changes to a document. Each change is a
// TextDocumentContentChangeEvent json object that has a "text" attribute and optional
// "range" attribute. If there is no range then text contains the entire new document.
define function apply-change
    (session :: <session>, document :: <open-document>, change :: <string-table>) => ()
  let text = change["text"];
  let range = element(change, "range", default: #f);
  if (range)
    show-error(session, "didChange doesn't support ranges yet");
  else
    log-debug("document replaced: %s", document.document-uri);
    document-lines(document) := split-lines(text);
  end;
end function;

// Jump to Declaration
// https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_declaration
// In 'Dylan world' this means jump to the generic function if there is one
define handler textDocument/declaration
    (session :: <session>, id, params)
  let (doc, line, column) = textdocumentposition-to-position(params);
  let location = $null;
  if (~doc)
    let uri = params["textDocument"]["uri"];
    log-debug("textDocument/declaration: document not found: %=", uri);
    show-error(session, format-to-string("Document not found: %s", uri));
  else
    let module = doc.ensure-document-module;
    let symbol = symbol-at-position(doc, line, column);
    if (symbol)
      let lookups = lookup-symbol(session, symbol, module: module);
      if (~empty?(lookups))
        location := first(lookups);
      else
        log-debug("textDocument/declaration: symbol %=, not found", symbol);
      end;
    else
      log-debug("textDocument/declaration: symbol is #f, nothing to lookup", symbol);
      show-info(session, "No symbol found at current position.");
    end;
  end;
  send-response(session, id, location);
end handler;


// Jump to definition.
// https://microsoft.github.io/language-server-protocol/specifications/specification-3-15/#textDocument_definition
define handler textDocument/definition
    (session :: <session>, id, params)
  let (doc, line, column) = textdocumentposition-to-position(params);
  let locations = $null;
  if (~doc)
    let uri = params["textDocument"]["uri"];
    log-debug("textDocument/definition: document not found: %=", uri);
    show-error(session, format-to-string("Document not found: %s", uri));
  else
    let module = doc.ensure-document-module;
    let symbol = symbol-at-position(doc, line, column);
    if (symbol)
      locations := lookup-symbol(session, symbol, module: module);
      if (empty?(locations))
        log-debug("textDocument/definition: symbol %=, not found", symbol);
      end;
    else
      log-debug("textDocument/definition: symbol is #f, nothing to lookup", symbol);
      show-info(session, "No symbol found at current position.");
    end;
  end;
  send-response(session, id, locations);
end handler;

// Find references to a symbol
// See https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_references
define handler textDocument/references
    (session :: <session>, id, params)
  let (doc, line, column) = textdocumentposition-to-position(params);
  let context = params["context"];
  let include-declaration = context["includeDeclaration"];
  let locations = $null;
  if (~doc)
    let uri = params["textDocument"]["uri"];
    log-debug("textDocument/references: document %= not found", uri);
    show-error(session, format-to-string("Document not found: %s", uri));
  else
    let module = doc.ensure-document-module;
    let symbol = symbol-at-position(doc, line, column);
    if (symbol)
      let env-object = get-environment-object(symbol, module: module);
      if (env-object)
        let references = all-references(env-object, include-self?: include-declaration);
        if (~empty?(references))
          locations := map(method(reference)
                             let source-location = get-location(reference);
                             source-location-to-lsp-location(source-location);
                           end, references);
        end;
      end;
    end;
  end;
  send-response(session, id, locations);
end handler;

// Maps URI strings to <open-document> objects.
define constant $documents = make(<string-table>);

// Represents one open file (given to us by textDocument/didOpen)
define class <open-document> (<object>)
  // The original URI string passed to us by the client to open this document.
  constant slot document-uri :: <string>,
    required-init-keyword: uri:;
  slot document-module :: false-or(od/<module-object>) = #f,
    init-keyword: module:;
  slot document-lines :: <sequence>,
    required-init-keyword: lines:;
end class;

define method ensure-document-module
    (document :: <open-document>) => (module :: od/<module-object>)
  document.document-module |
    begin
      let file = file-uri-to-locator(document.document-uri);
      let (mod, lib) = od/file-module(*project*, file);
      document.document-module := mod;
    end;
end;

define function register-file (uri, contents)
  log-debug("register-file(%=)", uri);
  let lines = split-lines(contents);
  let doc = make(<open-document>, uri: uri, lines: lines);
  $documents[uri] := doc;
end function;

// Characters that are part of the Dylan "name" BNF.
define constant $dylan-name-characters
  = "abcdefghijklmnopqrstuvwxyzABCDEFGHIHJLKMNOPQRSTUVWXYZ0123456789!&*<>|^$%@_-+~?/=";

// Given a document and a position, find the Dylan name (identifier) that is at
// (or immediately precedes) this position. If the position is, for example,
// the open paren following a function name, we should still find the name. If
// there is no name at position, return #f.
define function symbol-at-position
    (doc :: <open-document>, line, column) => (symbol :: false-or(<string>))
  if (line >= 0
        & line < size(doc.document-lines)
        & column >= 0
        & column <= size(doc.document-lines[line]))
    let line = doc.document-lines[line];
    local method name-character?(c) => (well? :: <boolean>)
            member?(c, $dylan-name-characters)
          end;
    let symbol-start = column;
    let symbol-end = column;
    while (symbol-start > 0 & name-character?(line[symbol-start - 1]))
      symbol-start := symbol-start - 1;
    end;
    while (symbol-end < size(line) & name-character?(line[symbol-end]))
      symbol-end := symbol-end + 1;
    end while;
    let name = copy-sequence(line, start: symbol-start, end: symbol-end);
    ~empty?(name) & name
  else
    log-debug("line %d column %d not in range for document %s",
              line, column, doc.document-uri);
    #f
  end
end function;

// Lookup a symbol, return a sequence of all the locations
// where it is defined
// Each one is in a format compatible with LSP's Location object
define function lookup-symbol
    (session, symbol :: <string>, #key module) => (symbols :: <sequence>)
  let object = get-environment-object(symbol, module: module);
  if (object)
    let defs = all-definitions(*project*, object);
    let locs = map(get-location, defs);
    map(source-location-to-lsp-location, locs);
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

// Convert TextDocumentPositionParams to (doc, line, column)
// See https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocumentPositionParams
// line and column are zero-based
define function textdocumentposition-to-position
    (params :: <object>) => (doc :: false-or(<open-document>), line :: <integer>, column :: <integer>)
  let text-document = params["textDocument"];
  let uri = text-document["uri"];
  let position = params["position"];
  let (line, column) = decode-lsp-position(position);
  let doc = element($documents, uri, default: #f);
  values(doc, line, column)
end function;

// Find the project library name to open.  Either it is set in the per-directory config
// (passed in from the client) or the workspace chooses a default library for us.
// Returns the name of a project or signals an error.
//
// TODO(cgay): Really we need to search the LID files to find the file in the
//   textDocument/didOpen message so we can figure out which library's project
//   to open.
// TODO(cgay): accept a locator argument so we know where to start, rather than
//   using working-directory(). Also better for testing.
// TODO(cgay): This always opens the project via the registry because when it's opened
//   via the .lid file directly the database doesn't get opened for reasons as yet
//   unknown.
define function find-project-name
    () => (name :: <string>)
  log-debug("find-project-name: working directory is %=", working-directory());
  if (*project-name*)
    // We've set it explicitly
    log-debug("Project name explicitly set: %s", *project-name*);
    *project-name*
  else
    let workspace = ws/load-workspace(); // May signal <workspace-error>
    let library-name = workspace & ws/workspace-default-library-name(workspace);
    if (library-name)
      log-debug("Found dylan-tool workspace default library name %=", library-name);
      library-name
    else
      error("Dylan workspace has no default library; no .lid files created yet?"
              " Check docs for how to configure a default project.");
    end
  end if
end function;

define handler exit
    (session :: <session>, id, params)
  session.session-state := $session-killed;
end handler;


ignore(*library*, run-compiler, list-all-package-names, document-lines-setter,
       dump, show-warning, show-log, show-error);

// Local Variables:
// indent-tabs-mode: nil
// End:
