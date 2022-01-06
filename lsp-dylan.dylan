Module: lsp-dylan
Synopsis: Language Server Protocol (LSP) server for Dylan
Author: Peter
Copyright: 2019


// Log to stderr so it shows up in the *dylan-lsp::stderr* buffer.  Log to a
// rolling temp file so we have a history and because I've seen the Emacs LSP
// client's *dylan-lsp::stderr* buffer not be kept up to date when the process
// restarts.
define function initialize-logging ()
  let file-target
    = make(<rolling-file-log-target>,
           pathname: merge-locators(as(<file-locator>,"lsp.log"),
                                    temp-directory()));
  *log* := make(<log>,
                name: "lsp",
                level: $debug-level,
                targets: list($stderr-log-target, file-target));
end function;


define constant $message-type-error = 1;
define constant $message-type-warning = 2;
define constant $message-type-info = 3;
define constant $message-type-log = 4;

define method window/show-message
    (msg-type :: <integer>, session :: <session>, fmt :: <string>, #rest args) => ()
  let msg = apply(format-to-string, fmt, args);
  let params = json("type", msg-type, "message", msg);
  send-notification(session, "window/showMessage", params);
end method;

define constant show-error   = curry(window/show-message, $message-type-error);
define constant show-warning = curry(window/show-message, $message-type-warning);
define constant show-info    = curry(window/show-message, $message-type-info);
define constant show-log     = curry(window/show-message, $message-type-log);

// It may be worth defining Dylan classes for these basic LSP objects, for
// clarity and type checking. Also to/from-json methods.

// Make a json Range object. bpos and epos are Position objects.
// https://microsoft.github.io/language-server-protocol/specifications/specification-3-15/#range
define function make-range (bpos, epos)
  json("start", bpos, "end", epos)
end function;

// Make json for a Position object. Line and character are both zero-based.
// https://microsoft.github.io/language-server-protocol/specifications/specification-3-15/#position
define function make-position (line, character)
  json("line", line, "character", character)
end function;

// Make json for a Location that's a 'zero size' range.
// https://microsoft.github.io/language-server-protocol/specifications/specification-3-15/#location
define function make-empty-location (doc :: <string>, line, character)
  let pos = make-position(line, character);
  json("uri", doc, "range", make-range(pos, pos))
end function;

// Decode a Position json object.  Note line and character are zero-based.
// https://microsoft.github.io/language-server-protocol/specifications/specification-3-15/#position
define function decode-position
    (position) => (line :: <integer>, character :: <integer>)
  let line = position["line"];
  let character = position["character"];
  values(line, character)
end function;

// Create a MarkupContent json object.
// https://microsoft.github.io/language-server-protocol/specifications/specification-3-15/#markupContent
define function make-markup (txt, #key markdown?)
  let kind = if (markdown?)
               "markdown"
             else
               "plaintext"
             end;
  json("value", txt,
       "kind", kind)
end function;

define function handle-workspace/symbol
    (session :: <session>, id :: <object>, params :: <object>) => ()
  // TODO this is only a dummy
  let query = params["query"];
  log-debug("Query: %s", query);
  let range = make-range(make-position(0, 0), make-position(0, 5));
  let symbols = list(json("name", "a-name",
                          "kind", 13,
                          "location", json("range", range,
                                           "uri", "file:///home/peter/Projects/lsp-dylan/lsp-dylan.dylan")));
  send-response(session, id, symbols);
end function;

// Show information about a symbol when we hover the cursor over it
// See: https://microsoft.github.io/language-server-protocol/specifications/specification-3-15/#textDocument_hover
// Parameters: textDocument, position, (optional) workDoneToken
// Returns: contents, (optional) range
define function handle-textDocument/hover
    (session :: <session>, id :: <object>, params :: <object>) => ()
  // TODO this is only a dummy
  let text-document = params["textDocument"];
  let uri = text-document["uri"];
  let position = params["position"];
  let (line, column) = decode-position(position);
  let doc = $documents[uri];
  let symbol = symbol-at-position(doc, line, column);
  if (symbol)
    let txt = format-to-string("textDocument/hover %s (%d/%d)", symbol, line + 1, column + 1);
    let hover = json("contents", make-markup(txt, markdown?: #f));
    send-response(session, id, hover);
  else
    // No symbol found (probably out of range)
    send-response(session, id, #f);
  end;
end function;

define function handle-textDocument/didOpen
    (session :: <session>, id :: <object>, params :: <object>) => ()
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
    // This is just test code.
    // Let's see if we can find a module
    let u = as(<url>, uri);
    let f = make-file-locator(u);
    let (m, l) = file-module(*project*, f);
    log-debug("textDocument/didOpen: File: %= Module: %=, Library: %=",
              as(<string>, f),
              if (m) environment-object-primitive-name(*project*, m) end,
              if (l) environment-object-primitive-name(*project*, l) end);
  else
    log-debug("textDocument/didOpen: no project found");
  end if;
end function;

// A document was saved. For Emacs, this is called when M-x lsp is executed on
// a new file. For now we don't care about the message at all, we just trigger
// a compilation of the associated project (if any) unconditionally.
// https://microsoft.github.io/language-server-protocol/specifications/specification-3-15/#textDocument_didSave
define function handle-textDocument/didSave
    (session :: <session>, id :: <object>, params :: <object>) => ()
  let textDocument = params["textDocument"];
  let uri = textDocument["uri"];
  let project = find-project-name();
  log-debug("textDocument/didSave: File %s, project %=", uri, project);
  if (project)
    let project-object = find-project(project);
    log-debug("textDocument/didSave: project = %=", project-object);
    if (project-object)
      let warnings = make(<stretchy-vector>);
      local method note-warning (#rest args)
              add!(warnings, args);
            end;
      // TODO(cgay): do we want `save-databases?: #t` here?
      // TODO(cgay): how to display warnings on client side. I assume there's a message
      //   we should be sending.
      build-project(project-object,
                    link?: #f,
                    warning-callback: note-warning);
      log-debug("textDocument/didSave: done building %=", project);
      show-info(session, "Build complete, %s warnings",
                if (empty?(warnings)) "no" else warnings.size end);
    else
      show-error("Project %s not found.", project);
    end;
  else
    log-debug("handle-textDocument/didSave: project not found for %=", uri);
    show-error("Project %s not found.", project);
  end;
end function;

// https://microsoft.github.io/language-server-protocol/specifications/specification-3-15/#textDocument_didChange
define function handle-textDocument/didChange
    (session :: <session>, id :: <object>, params :: <object>) => ()
  let text-document = params["textDocument"];
  let uri = text-document["uri"];
  let document = element($documents, uri, default: #f);
  if (document)
    let changes = params["contentChanges"];
    for (change in changes)
      apply-change(session, document, change);
    end;
  else
    show-error(session, format-to-string("Document not found on server: %s", uri));
  end;
end function;

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
    show-info(session, "Document content replaced");
    document-lines(document) := split-lines(text);
  end;
end function;

// Jump to definition.
// https://microsoft.github.io/language-server-protocol/specifications/specification-3-15/#textDocument_definition
define function handle-textDocument/definition
    (session :: <session>, id :: <object>, params :: <object>) => ()
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
    unless (doc.document-module)
      let local-dir = make(<directory-locator>, path: locator-path(doc.document-uri));
      let local-file = make(<file-locator>,
                            directory: local-dir,
                            name: locator-name(doc.document-uri));
      let (mod, lib) = file-module(*project*, local-file);
      log-debug("textDocument/definition: module=%s, library=%s", mod, lib);
      doc.document-module := mod;
    end;
    let symbol = symbol-at-position(doc, line, character);
    if (symbol)
      let (target, line, char)
        = lookup-symbol(session, symbol, module: doc.document-module);
      if (target)
        log-debug("textDocument/definition: Lookup %s and got target=%s, line=%d, char=%d",
                  symbol, target, line, char);
        let uri = make-file-uri(target); // TODO
        location := make-empty-location(as(<string>, uri), line, char);
      else
        log-debug("textDocument/definition: symbol %=, not found", symbol);
      end;
    else
      log-debug("textDocument/definition: symbol is #f, nothing to lookup", symbol);
      show-info(session, "No symbol found at current position.");
    end;
  end;
  send-response(session, id, location);
end function;

define function handle-workspace/didChangeConfiguration
    (session :: <session>, id :: <object>, params :: <object>) => ()
  // NOTE: vscode always sends this just after initialized, whereas
  // emacs does not, so we need to ask for config items ourselves and
  // not wait to be told.
  log-debug("Did change configuration");
  log-debug("Settings: %s", print-json-to-string(params));
  // TODO do something with this info.
  let settings = params["settings"];
  let dylan-settings = settings["dylan"];
  let project-name = element(dylan-settings, "project", default: #f);
  *project-name* := (project-name ~= "") & project-name;
  //show-info(session, "The config was changed");
  test-open-project(session);
end function;

/* Handler for 'initialized' message.
 *
 * Example: {"jsonrpc":"2.0","method":"initialized","params":{}}
 *
 * Here we will register the dynamic capabilities of the server with the client.
 * Note we don't do this yet, any capabilities are registered statically in the
 * 'initialize' message.
 * Here also we will start the compiler session.
 */
define function handle-initialized
    (session :: <session>, id :: <object>, params :: <object>) => ()
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

  // Test code
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
    log-debug("handle-initialized: %s=%s", var, environment-variable(var));
  end;
  send-request(session, "workspace/workspaceFolders", #f,
               callback: handle-workspace/workspaceFolders);
  *server* := start-compiler(in-stream, out-stream);
  test-open-project(session);
end function handle-initialized;

define function test-open-project(session) => ()
  let project-name = find-project-name();
  log-debug("test-open-project: Found project name %=", project-name);
  *project* := open-project(*server*, project-name);
  log-debug("test-open-project: Project opened");

  // Let's see if we can find a module.

  // TODO(cgay): file-module is returning #f because (I believe)
  // project-compiler-database(*project*) returns #f and hence file-module
  // punts. Not sure who's responsible for opening the db and setting that slot
  // or why it has worked at all in the past.
  let (m, l) = file-module(*project*, "library.dylan");
  log-debug("test-open-project: m = %=, l = %=", m, l);
  log-debug("test-open-project: Try Module: %=, Library: %=",
            m & environment-object-primitive-name(*project*, m),
            l & environment-object-primitive-name(*project*, l));

  log-debug("test-open-project: project-library = %=", project-library(*project*));
  log-debug("test-open-project: project db = %=", project-compiler-database(*project*));

  *module* := m;
  if (*project*)
    let warn = curry(log-warning, "open-project-compiler-database: %=");
    let db = open-project-compiler-database(*project*, warning-callback: warn);
    log-debug("test-open-project: db = %=", db);
    for (s in project-sources(*project*))
      let rl = source-record-location(s);
      log-debug("test-open-project: Source: %=, a %= in %=",
                s,
                object-class(s),
                as(<string>, rl));
    end;
    log-debug("test-open-project: listing project file libraries:");
    do-project-file-libraries(method (l, r)
                                log-debug("test-open-project: Lib: %= Rec: %=", l, r);
                              end,
                              *project*,
                              as(<file-locator>, "library.dylan"));
  else
    log-debug("test-open-project: project did't open");
  end if;
  log-debug("test-open-project: Compiler started: %=, Project %=", *server*, *project*);
  log-debug("test-open-project: Database: %=", project-compiler-database(*project*));
end function;

define function ensure-trailing-slash
    (s :: <string>) => (s-slash :: <string>)
  if (ends-with?(s, "/"))
    s
  else
    concatenate(s, "/")
  end
end function;

// Handle the 'initialize' message.
// Here we initialize logging/tracing and store the workspace root for later.
// Here we return the 'static capabilities' of this server.
// In the future we can register capabilities dynamically by sending messages
// back to the client; this seems to be the preferred 'new' way to do things.
// https://microsoft.github.io/language-server-protocol/specifications/specification-3-15/#initialize
define function handle-initialize
    (session :: <session>, id :: <object>, params :: <object>) => ()
  // The very first received message is "initialize" (I think), and it seems
  // that for some reason it doesn't get logged, so log params here. The params
  // for this method are copious, so we log them with pretty printing.
  log-debug("handle-initialize(%=, %=, %s)",
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
      log-error("handle-initialize: trace must be"
                  " \"off\", \"messages\" or \"verbose\", not %=", trace);
  end select;
  log-debug("handle-initialize: debug: %s, messages: %s, verbose: %s",
            *debug-mode*, *trace-messages*, *trace-verbose*);

  // Save the workspace root (if provided) for later.
  // rootUri takes precedence over rootPath if both are provided.
  // TODO: can root-uri be something that's not a file:// URL?
  let root-uri  = element(params, "rootUri", default: #f);
  let root-path = element(params, "rootPath", default: #f);
  session.root := find-workspace-root(root-uri, root-path);
  if (session.root)
    working-directory() := session.root;
  end;
  log-debug("handle-initialize: Working directory is now %s", working-directory());

  // Return the capabilities of this server
  let capabilities = json("hoverProvider", #f,
                          "textDocumentSync", 1,
                          "definitionProvider", #t,
                          "workspaceSymbolProvider", #t);
  let response-params = json("capabilities", capabilities);
  send-response(session, id, response-params);
  // All OK to proceed.
  session.state := $session-active;
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
        let url = as(<url>, ensure-trailing-slash(root-uri));
        make(<directory-locator>, path: locator-path(url))
      elseif (root-path)
        as(<directory-locator>, root-path)
      end;
  let workspace = ws/find-workspace-file(directory) & ws/load-workspace(directory);
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

define function handle-workspace/workspaceFolders
    (session :: <session>, params :: <object>) => ()
  // TODO: handle multi-folder workspaces.
  log-debug("Workspace folders were received: %=", params);
end;

// Maps URI strings to <open-document> objects.
define constant $documents = make(<string-table>);

// Represents one open file (given to us by textDocument/didOpen)
define class <open-document> (<object>)
  constant slot document-uri :: <url>,
    required-init-keyword: uri:;
  slot document-module :: false-or(<module-object>) = #f,
    init-keyword: module:;
  slot document-lines :: <sequence>,
    required-init-keyword: lines:;
end class;

define function register-file (uri, contents)
  log-debug("register-file(%=)", uri);
  let lines = split-lines(contents);
  let doc = make(<open-document>, uri: as(<url>, uri), lines: lines);
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

define function unregister-file (uri)
  // TODO
  remove-key!($documents, uri)
end function;

// Make a file:// URI from a local file path.
// This is supposed to follow RFC 8089
// (locators library not v. helpful here)
define function make-file-uri
    (f :: <file-locator>) => (uri :: <url>)
  if (f.locator-relative?)
    f := merge-locators(f, working-directory());
  end;
  let server = make(<file-server>, host: "");
  let directory = make(<directory-url>,
                       server: server,
                       path: locator-path(f));
  make(<file-url>,
       directory: directory,
       name: locator-name(f))
end function;

define function make-file-locator
    (f :: <url>) => (loc :: <file-locator>)
  // TODO - what if it isnt a file:/, etc etc
  let d = make(<directory-locator>, path: locator-path(f));
  make(<file-locator>, directory: d, name: locator-name(f))
end function;

// Look up a symbol. Return the containing doc,
// the line and column
define function lookup-symbol
    (session, symbol :: <string>, #key module) => (doc, line, column)
  let loc = symbol-location(symbol, module: module);
  if (loc)
    let source-record = loc.source-location-source-record;
    let absolute-path = source-record.source-record-location;
    let (name, line) = source-line-location(source-record,
                                          loc.source-location-start-line);
    let column = loc.source-location-start-column;
    values(absolute-path, line - 1, column)
  else
    log-debug("Looking up %s, not found", symbol);
    #f
  end
end function;

// Find the project name to open.
// Either it is set in the per-directory config (passed in from the client)
// or we'll guess it is the only lid file in the workspace root.
// If there is more than one lid file, that's an error, don't return
// any project.
// Returns: the name of a project
//
// TODO(cgay): Really we need to search the LID files to find the file in the
//   textDocument/didOpen message so we can figure out which library's project
//   to open.
// TODO(cgay): accept a locator argument so we know where to start, rather than
//   using working-directory(). Also better for testing.
define function find-project-name
    () => (name :: false-or(<string>))
  if (*project-name*)
    // We've set it explicitly
    log-debug("Project name explicitly:%s", *project-name*);
    *project-name*
  elseif (ws/find-workspace-file(working-directory()))
    // There's a dylan-tool workspace.
    let workspace = ws/load-workspace(working-directory());
    let library-name = workspace & ws/workspace-default-library-name(workspace);
    if (library-name)
      log-debug("found dylan-tool workspace default library name %=", library-name);
      library-name
    else
      log-debug("dylan-tool workspace has no default library configured.");
      #f
    end;
  else
    log-debug("no workspace file found starting in %s", working-directory());
    // Guess based on there being one .lid file in the workspace root
    block(return)
      local method return-lid(dir, name, type)
              if (type = #"file")
                let file = as(<file-locator>, name);
                if (locator-extension(file) = "lid")
                  // TODO(cgay): This strips the extension so that the project will be
                  // opened via the registry because when it's opened via the .lid file
                  // directly the database doesn't get opened. Note that when opened by
                  // .lid file it opens a <dfmc-hdp-project-object> whereas when opened
                  // via the registry it opens a <dfmc-lid-project-object>. Go figure.
                  return(locator-base(file));
                end if;
              end if;
            end method;
      do-directory(return-lid, working-directory());
      log-debug("find-project-name found no LID files in %s", working-directory());
      #f
    end block
  end if
end function;

define function lsp-pre-init-state-loop
    (session :: <session>) => ()
  while (session.state == $session-preinit)
    log-debug("lsp-pre-init-state-loop: waiting for message");
    let (meth, id, params) = receive-message(session);
    select (meth by =)
      "initialize" =>
        handle-initialize(session, id, params);
      "exit" =>
        session.state := $session-killed;
      otherwise =>
        // Respond to any request with an error, and drop any notifications
        if (id)
          send-error-response(session, id, $server-not-initialized);
        end if;
    end select;
    flush(session);
  end while;
end function;

define function lsp-active-state-loop
    (session :: <session>) => ()
  while (session.state == $session-active)
    log-debug("lsp-active-state-loop: waiting for message");
    let (meth, id, params) = receive-message(session);
    select (meth by =)
      "exit" =>
        session.state := $session-killed;
      "initialize" =>
        send-error-response(session, id, $invalid-request);
      "initialized" =>
        handle-initialized(session, id, params);
      "shutdown" =>
        send-response(session, id, $null);
        session.state := $session-shutdown;
      "textDocument/definition" =>
        handle-textDocument/definition(session, id, params);
      "textDocument/didChange" =>
        handle-textDocument/didChange(session, id, params);
      "textDocument/didOpen" =>
        handle-textDocument/didOpen(session, id, params);
      "textDocument/didSave" =>
        handle-textDocument/didSave(session, id, params);
      "textDocument/hover" =>
        handle-textDocument/hover(session, id, params);
      "workspace/didChangeConfiguration" =>
        handle-workspace/didChangeConfiguration(session, id, params);
      "workspace/symbol" =>
        handle-workspace/symbol(session, id, params);
      otherwise =>
        // Respond to any other request with an not-implemented error.
        // Drop any other notifications
        log-debug("lsp-active-state-loop: %s method '%s' is not yet implemented.",
                  if (id) "Request" else "Notification" end, meth);
        if (id)
          send-error-response(session, id, $method-not-found);
        end;
    end select;
    flush(session);
  end while;
end function;

define function lsp-shutdown-state-loop
    (session :: <session>) => ()
  while (session.state == $session-shutdown)
    log-debug("lsp-shutdown-state-loop: waiting for message");
    let (meth, id, params) = receive-message(session);
    select (meth by =)
      "exit" =>
        log-debug("Dylan LSP server exiting");
        clp/abort-command(0);
      otherwise =>
        // Respond to any request with an invalid error,
        // Drop any notifications
        if (id)
          send-error-response(session, id, $invalid-request);
        end;
    end select;
    flush(session);
  end while;
end function;

define function lsp-server-top-level
    (command :: <lsp-server-command-line>) => ()
  *debug-mode* := command.debug-server?;
  if (command.debug-opendylan?)
    enable-od-environment-debug-logging();
  end;

  let session = make(<stdio-session>);
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

define clp/command-line <lsp-server-command-line> ()
  option debug-server? :: <boolean> = #t, // default to #f eventually
    names: #("debug-server"),
    kind: clp/<flag-option>,
    help: "Turn on debugging for the LSP server.";
  option debug-opendylan? :: <boolean> = #t, // default to #f eventually
    names: #("debug-opendylan"),
    kind: clp/<flag-option>,
    help: "Turn on debugging for Open Dylan.";
end clp/command-line;

define function main
    (name :: <string>, arguments :: <vector>)
  initialize-logging();
  let command = make(<lsp-server-command-line>,
                     help: "Dylan LSP server");
  block ()
    clp/parse-command-line(command, application-arguments());
    lsp-server-top-level(command);
  exception (err :: clp/<abort-command-error>)
    exit-application(clp/exit-status(err));
  end;
end function;

ignore(*library*, run-compiler, describe-symbol, list-all-package-names,
       document-lines-setter, unregister-file,
       one-off-debug, dump, show-warning, show-log, show-error);

main(application-name(), application-arguments());



// Local Variables:
// indent-tabs-mode: nil
// End:
