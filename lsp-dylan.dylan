Module: lsp-dylan-impl
Synopsis: Language Server Protocol (LSP) server for Dylan
Author: Peter
Copyright: 2019

// Handlers are roughly grouped together by type. For example, initialization,
// textDocument/*, workspace/*, etc.

// Handle the 'initialize' message.
// Here we initialize logging/tracing and store the workspace root for later.
// Here we return the 'static capabilities' of this server.
// In the future we can register capabilities dynamically by sending messages
// back to the client; this seems to be the preferred 'new' way to do things.
// https://microsoft.github.io/language-server-protocol/specifications/specification-3-15/#initialize
define handler initialize
    (session :: <session>, id, params)
  // The very first received message is "initialize" (I think), and it seems
  // that for some reason it doesn't get logged, so log params here. The params
  // for this method are copious, so we log them with pretty printing.
  log-debug("initialize(%=, %=, %s)", session, id, json-text(params));
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

  // Save the workspace root (if provided) for later.
  // rootUri takes precedence over rootPath if both are provided.
  // TODO: can root-uri be something that's not a file:// URL?
  let root-uri  = element(params, "rootUri", default: #f);
  let root-path = element(params, "rootPath", default: #f);
  session.root := find-workspace-root(root-uri, root-path);
  if (session.root)
    log-info("Dylan workspace root: %s", session.root);
    working-directory() := session.root;
  end;
  log-info("Dylan LSP server working directory: %s", working-directory());

  // Return the capabilities of this server
  let capabilities = json("hoverProvider", #t,
                          "textDocumentSync", 1,
                          "definitionProvider", #t,
                          "workspaceSymbolProvider", #t);
  let response-params = json("capabilities", capabilities);
  send-response(session, id, response-params);
  // All OK to proceed.
  session.state := $session-active;
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
  send-request(session, "workspace/workspaceFolders", #f,
               callback: handle-workspace/workspaceFolders);
  *server* := start-compiler(in-stream, out-stream);
end handler;

define handler workspace/workspaceFolders
    (session :: <session>, id, params)
  // TODO: handle multi-folder workspaces.
end handler;

define handler workspace/symbol
    (session :: <session>, id, params)
  // TODO this is only a dummy
  let query = params["query"];
  let range = make-range(make-position(0, 0), make-position(0, 5));
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
  // TODO do something with this info.
  let settings = params["settings"];
  let dylan-settings = settings["dylan"];
  let project-name = element(dylan-settings, "project", default: #f);
  *project-name* := (project-name ~= "") & project-name;
end handler;

// Format symbol description into a hover message.
// The description comes from the compiler database as a string with
// four lines - location, name, params, return - this is too much
// to fit in, so just show the last three on one line.
// If the input is #f or not in four line format, return #f
define function format-hover-message
    (txt :: false-or(<string>)) => (hover :: false-or(<string>))
  if (txt)
    let (_, fname, params, returns) = apply(values, split-lines(txt));
    if (returns)
      format-to-string("%s %s %s",
                       strip(fname),
                       strip(params),
                       strip(returns))
    end if;
  end if;
end function;

// Show information about a symbol when we hover the cursor over it
// See: https://microsoft.github.io/language-server-protocol/specifications/specification-3-15/#textDocument_hover
// Parameters: textDocument, position, (optional) workDoneToken
// Returns: contents, (optional) range
define handler textDocument/hover
    (session :: <session>, id, params)
  let text-document = params["textDocument"];
  let uri = text-document["uri"];
  let position = params["position"];
  let (line, column) = decode-position(position);
  let doc = element($documents, uri, default: #f);
  if (~doc)
    log-debug("textDocument/hover: document %= not found", uri);
    show-error(session, format-to-string("Document not found: %s", uri));
  else
    let module = doc.ensure-document-module;
    let symbol = symbol-at-position(doc, line, column);
    let hover = if (symbol)
                  let txt = describe-symbol(symbol, module: module);
                  let msg = format-hover-message(txt);
                  if (msg)
                    json("contents", make-markup(msg, markdown?: #f));
                  end;
                end;
    send-response(session, id, hover);
  end if;
end handler;

define handler textDocument/didOpen
    (session :: <session>, id, params)
  block (return)
    let document = params["textDocument"];
    let uri = document["uri"];
    let language-id = document["languageId"];
    let text = document["text"];

    if (language-id = "dylan")
      register-file(uri, text);
    else
      log-info("textDocument/didOpen: ignoring non-Dylan file %s", uri);
      return();
    end if;

    if (*project*)
      // For now assume only one open project at a time.
      log-info("Reusing open project %s", *project*);
      return();
    end;

    let file = file-uri-to-locator(uri);
    let library-name = find-library-name(file);
    if (~library-name)
      show-error("No library found for %s", uri);
      return();
    end;

    log-info("textDocument/didOpen: found library name %=", library-name);
    *project* := open-project(*server*, library-name);
    // TODO(cgay): file-module is returning #f because (I believe)
    // project-compiler-database(*project*) returns #f and hence file-module
    // punts. Not sure who's responsible for opening the db and setting that
    // slot or why it has worked at all in the past.
    let (module, library) = file-module(*project*, file);
    log-debug("textDocument/didOpen: file: %s module: %=, library: %=",
              file,
              module & environment-object-primitive-name(*project*, module),
              library & environment-object-primitive-name(*project*, library));
    *module* := module;

    // Debugging ...
    let warn = curry(log-warning, "open-project-compiler-database: %=");
    let db = open-project-compiler-database(*project*, warning-callback: warn);
    log-debug("textDocument/didOpen: db = %=", db);
    for (source in project-sources(*project*))
      let rloc = source-record-location(source);
      log-debug("textDocument/didOpen: source = %s", rloc);
    end;
    do-project-file-libraries(method (lib, rec)
                                log-debug("textDocument/didOpen: lib = %s rec = %s",
                                          lib, rec);
                              end,
                              *project*,
                              as(<file-locator>, "library.dylan"));
  end block;
end handler;

// A document was saved. For Emacs, this is called when M-x lsp is executed on
// a new file. For now we don't care about the message at all, we just trigger
// a compilation of the associated project (if any) unconditionally.
// https://microsoft.github.io/language-server-protocol/specifications/specification-3-15/#textDocument_didSave
define handler textDocument/didSave
    (session :: <session>, id, params)
  let document = params["textDocument"];
  let uri = document["uri"];
  let project = find-library-name(file-uri-to-locator(uri));
  if (project)
    let project-object = find-project(project);
    if (project-object)
      let warnings = make(<stretchy-vector>);
      build-project(project-object,
                    link?: #f,
                    warning-callback: curry(add!, warnings));
      show-info(session, "Build complete, %s warning%s",
                if (empty?(warnings)) "no" else warnings.size end,
                if (warnings.size == 1) "" else "s" end);
      publish-diagnostics(session, uri, warnings);
    else
      show-error("Project %s not found.", project);
    end;
  else
    show-error("Project not found for %s.", uri);
  end;
end handler;

define variable *previous-warnings-by-uri* = #f;

// https://microsoft.github.io/language-server-protocol/specifications/specification-3-15/#textDocument_publishDiagnostics
// https://microsoft.github.io/language-server-protocol/specifications/specification-3-15/#diagnostic
define function publish-diagnostics
    (session :: <session>, uri :: <string>, warnings :: <sequence>) => ()
  // Since textDocument/publishDiagnostics has a uri parameter it seems we have
  // to send warnings separately for each file that has them.
  let context = server-context(*server*);
  let project = context-project(context);
  let warnings-by-uri = make(<string-table>);
  for (warning in warnings)
    let loc = environment-object-source-location(project, warning);
    let sr = loc.source-location-source-record;
    let uri = locator-to-file-uri(sr.source-record-location);
    warnings-by-uri[uri]
      := add!(element(warnings-by-uri, uri, default: #[]), warning);
  end;
  for (warnings keyed-by uri in warnings-by-uri)
    let diagnostics = make(<stretchy-vector>);
    for (warning in warnings)
      // Unimplemented Diagnostic fields...
      //   "code" - probably not applicable for Open Dylan
      //   "codeDescription" - a URL with more info about the error
      //   "tags" - e.g., deprecated or unused code
      //   "relatedInformation" - e.g., location of colliding definition
      let loc = environment-object-source-location(project, warning);
      let sr = loc.source-location-source-record;
      let soff = loc.source-location-start-offset;
      // sr.source-record-start-line is the number of Dylan Interchange Format
      // header lines.
      let start-line = soff.source-offset-line + sr.source-record-start-line - 1;
      let start-col = soff.source-offset-column;
      let eoff = loc.source-location-end-offset;
      let end-line = eoff.source-offset-line + sr.source-record-start-line - 1;
      let end-col = eoff.source-offset-column;
      let range = make-range(make-position(start-line, start-col),
                             make-position(end-line, end-col));
      let severity
        = if (instance?(warning, <serious-compiler-warning-object>))
            $diagnostic-severity-error
          else
            $diagnostic-severity-warning
          end;
      let diagnostic
        = json("uri", uri,
               "range", range,
               "severity", severity,
               "source", "Open Dylan",
               "message", compiler-warning-full-message(project, warning));
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
    show-info(session, "Document content replaced");
    document-lines(document) := split-lines(text);
  end;
end function;

// Jump to definition.
// https://microsoft.github.io/language-server-protocol/specifications/specification-3-15/#textDocument_definition
define handler textDocument/definition
    (session :: <session>, id, params)
  let text-document = params["textDocument"];
  let uri = text-document["uri"];
  let position = params["position"];
  let (line, character) = decode-position(position);
  let doc = element($documents, uri, default: #f);
  let location = $null;
  if (~doc)
    log-debug("textDocument/definition: document not found: %=", uri);
    show-error(session, format-to-string("Document not found: %s", uri));
  else
    let module = doc.ensure-document-module;
    let symbol = symbol-at-position(doc, line, character);
    if (symbol)
      let (target, line, char)
        = lookup-symbol(session, symbol, module: module);
      if (target)
        log-debug("textDocument/definition: Lookup %s and got target=%s, line=%d, char=%d",
                  symbol, target, line, char);
        let uri :: <string> = locator-to-file-uri(target);
        location := make-empty-location(uri, line, char);
      else
        log-debug("textDocument/definition: symbol %=, not found", symbol);
      end;
    else
      log-debug("textDocument/definition: symbol is #f, nothing to lookup", symbol);
      show-info(session, "No symbol found at current position.");
    end;
  end;
  send-response(session, id, location);
end handler;

// Maps URI strings to <open-document> objects.
define constant $documents = make(<string-table>);

// Represents one open file (given to us by textDocument/didOpen)
define class <open-document> (<object>)
  // The original URI string passed to us by the client to open this document.
  constant slot document-uri :: <string>,
    required-init-keyword: uri:;
  slot document-module :: false-or(<module-object>) = #f,
    init-keyword: module:;
  slot document-lines :: <sequence>,
    required-init-keyword: lines:;
end class;

define method ensure-document-module
    (document :: <open-document>) => (module :: <module-object>)
  document.document-module |
    begin
      let file = file-uri-to-locator(document.document-uri);
      let (mod, lib) = file-module(*project*, file);
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

// Look up a symbol. Return the containing doc,
// the line and column
define function lookup-symbol
    (session, symbol :: <string>, #key module) => (doc, line, column)
  let sloc = symbol-location(symbol, module: module);
  if (sloc)
    let sr = sloc.source-location-source-record;
    let locator = sr.source-record-location;
    let (name, line) = source-line-location(sr, sloc.source-location-start-line);
    let column = sloc.source-location-start-column;
    log-debug("sloc = %s\nsr = %s\nlocator = %s\nname = %s", sloc, sr, locator, name);

    /* M-. on source-line-location, above, yields this...what's up with locator
       being in _build/databases? [later] It seems to have been due to not having
       OPEN_DYLAN_USER_REGISTRIES set to contain $OD/sources/registry. My guess
       is that it therefore opened a precompiled database-only project and that
       gets the source-record-location wrong. Might be worth diving into.

       log: Invoking message handler "textDocument/definition" with id 3 and params {<string-table> 9}
       log: source-line-location -> module is {class <module-object>}lsp-dylan-impl
       log: sloc = {<compiler-range-source-location> {flat file "source-records"} (66, 0) - (67, 61) 10}

       sr = {flat file "source-records"}
       locator = /home/cgay/dylan/workspaces/lsp/_build/databases/source-records.dylan
       name = source-records.dylan
      */
    values(locator, line - 1, column)
  else
    log-debug("Looking up %s, not found", symbol);
    #f
  end
end function;

// Find the name of a project (i.e., library) to open. For now, this requires a
// workspace.json file with a "default-library" setting.
define function find-library-name
    (file :: <file-locator>) => (name :: false-or(<string>))
  block (return)
    if (*project-name*)
      // It was explicitly set.
      log-debug("Project name set explicitly: %s", *project-name*);
      return(*project-name*);
    end;
    let dir = locator-directory(file);
    let workspace = ws/load-workspace(dir);
    let name = workspace & ws/workspace-default-library-name(workspace);
    if (name)
      log-debug("found dylan-tool workspace default library name %=", name);
      return(name);
    end;
    log-debug("dylan-tool workspace has no default library configured.");

    // When/if we decide to implement find-root-libraries in the "workspaces"
    // library (not that hard), uncomment this. For now we require (above) that
    // a "default-library" be configured in workspace.json.
/*
    let root = if (workspace)
                 ws/workspace-directory(workspace)
               else
                 locator-directory(find-registry-directory(dir) | file)
               end;
    let names = #[]; // TODO(maybe): ws/find-root-libraries(file)
    select (size(names))
      0 =>
        error("No project found for %s. Make sure you're in a Dylan workspace"
                " or there is a 'registry' directory next to or above that file.",
              file);
      1 =>
        names[0];
      otherwise =>
        local method test-library? (name)
                ends-with?(name, "-test-suite")
                  | ends-with?(name, "-tests")
                  | ends-with?(name, "-test")
              end;
        let test-libraries = choose(test-library?, names);
        select (test-libraries.size)
          0 =>
            log-warning("Multiple libraries found for %s, using the first one: %s",
                        file, join(names, ", "));
            names[0];
          1 =>
            test-libraries[0];
          otherwise =>
            log-warning("Multiple test libraries found for %s, using the first one: %s",
                        file, join(test-libraries, ", "));
            test-libraries[0];
        end select
    end select
*/
  end block
end function;

define handler exit
    (session :: <session>, id, params)
  session.state := $session-killed;
end handler;

// Feels like the top-level loop should be in the dylan-lsp-server library but
// I tried moving it and the amount of exporting needed seemed pointless at
// this early stage of development. Maybe reconsider once the code settles.

define function lsp-pre-init-state-loop
    (session :: <session>) => ()
  while (session.state == $session-preinit)
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
  while (session.state == $session-active)
    log-debug("lsp-active-state-loop: waiting for message");
    let (meth, id, params) = receive-message(session);
    invoke-message-handler(meth, session, id, params);
    flush(session);
  end while;
end function;

define function lsp-shutdown-state-loop
    (session :: <session>) => ()
  block (return)
    while (session.state == $session-shutdown)
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

define function lsp-server-top-level
    (#key debug-server? = #t, debug-opendylan? = #t) => ()
  *debug-mode* := debug-server?;
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

// This makes it possible to modify the OD environment sources with debug-out
// messages and see them in our local logs. debug-out et al are from the
// simple-debugging:dylan module.
define function enable-od-environment-debug-logging ()
  debugging?() := #t;
  // Added most of the sources/environment/ debug-out categories here. --cgay
  debug-parts() := #(#"dfmc-environment-application",
                     #"dfmc-environment-database",
                     #"dfmc-environment-projects",
                     #"environment-debugger",
                     #"environment-profiler",
                     #"environment-protocols",
                     #"lsp");   // our own temp category. debug-out(#"lsp", ...)
  local method lsp-debug-out (fn :: <function>)
          let (fmt, #rest args) = apply(values, fn());
          // I wish we could log the "part" here, but debug-out drops it.
          apply(log-debug, concatenate("debug-out: ", fmt), args)
        end;
  debug-out-function() := lsp-debug-out;
  // Not yet...
  //*dfmc-debug-out* := #(#"whatever");  // For dfmc-common's debug-out.
end function;

ignore(*library*, run-compiler, list-all-package-names, document-lines-setter,
       one-off-debug, dump, show-warning, show-log, show-error);


// Local Variables:
// indent-tabs-mode: nil
// End:
