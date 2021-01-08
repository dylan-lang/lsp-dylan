Module: lsp-dylan
Synopsis: Test stuff for language server protocol
Author: Peter
Copyright: 2019


define constant $message-type-error = 1;
define constant $message-type-warning = 2;
define constant $message-type-info = 3;
define constant $message-type-log = 4;
define method show-message (session :: <session>,
                            msg-type :: <integer>,
                            m :: <string>)
    => ()
  let show-message-params = json("type", msg-type,
                                 "message", m);
  send-notification(session, "window/showMessage", show-message-params);
end method;

define method log-message (session :: <session>,
                            msg-type :: <integer>,
                            m :: <string>)
    => ()
  let show-message-params = json("type", msg-type,
                                 "message", m);
  send-notification(session, "window/logMessage", show-message-params);
end method;

define inline method show-error (session :: <session>,
                                 m :: <string>)
    => ()
  show-message(session, $message-type-error, m);
end method;

define inline method show-warning (session :: <session>,
                                   m :: <string>)
    => ()
  show-message(session, $message-type-warning, m);
end method;

define inline method show-info (session :: <session>,
                                m :: <string>)
    => ()
  show-message(session, $message-type-info, m);
end method;

define inline method show-log(session :: <session>,
                              m :: <string>)
    => ()
  show-message(session, $message-type-log, m);
end method;

define function local-log(m :: <string>, #rest params) => ()
  apply(format, *standard-error*, m, params);
  force-output(*standard-error*);
end function;

define function make-range(start, endp)
  json("start", start, "end", endp);
end function;

/*
 * Make a Position object
 * See https://microsoft.github.io/language-server-protocol/specifications/specification-3-15/#position
 */
define function make-position(line, character)
  json("line", line, "character", character);
end function;

/*
 * Make a Location that's 'zero size' range
 * See https://microsoft.github.io/language-server-protocol/specifications/specification-3-15/#location
 */
define function make-location(doc, line, character)
  let pos = make-position(line, character);
  json("uri", doc, "range", make-range(pos, pos))
end;

/*
 * Decode a Position object.
 * Note line and character are zero-based.
 * See https://microsoft.github.io/language-server-protocol/specifications/specification-3-15/#position
 */
define function decode-position(position)
 => (line :: <integer>, character :: <integer>)
  let line = as(<integer>, position["line"]);
  let character = as(<integer>, position["character"]);
  values(line, character)
end function;

/*
 * Create a MarkupContent object.
 * See https://microsoft.github.io/language-server-protocol/specifications/specification-3-15/#markupContent
 */
define function make-markup(txt, #key markdown = #f)
  let kind = if (markdown)
               "markdown"
             else
               "plaintext"
             end;
  json("value", txt,
       "kind", kind);
end function;

define function handle-workspace/symbol (session :: <session>,
                                         id :: <object>,
                                         params :: <object>)
  => ()
  // TODO this is only a dummy
  let query = params["query"];
  local-log("Query: %s\n", query);
  let range = make-range(make-position(0, 0), make-position(0,5));
  let symbols = list(json("name", "a-name",
                          "kind", 13,
                          "location", json("range", range,
                                           "uri", "file:///home/peter/Projects/lsp-dylan/lsp-dylan.dylan")));
  send-response(session, id, symbols);
end function;

/* Show information about a symbol when we hover the cursor over it
 * See: https://microsoft.github.io/language-server-protocol/specifications/specification-3-15/#textDocument_hover
 * Parameters: textDocument, position, (optional) workDoneToken
 * Returns: contents, (optional) range
 */
define function handle-textDocument/hover(session :: <session>,
                                          id :: <object>,
                                          params :: <object>) => ()
  // TODO this is only a dummy
  let text-document = params["textDocument"];
  let uri = text-document["uri"];
  let position = params["position"];
  let (line, column) = decode-position(position);
  let doc = $documents[uri];
  let symbol = symbol-at-position(doc, line, column);
  if (symbol)
    let txt = format-to-string("textDocument/hover %s (%d/%d)", symbol, line + 1, column + 1);
    let hover = json("contents", make-markup(txt, markdown: #f));
    send-response(session, id, hover);
  else
    // No symbol found (probably out of range)
    send-response(session, id, #f);
  end;
end function;

define function handle-textDocument/didOpen(session :: <session>,
                                            id :: <object>,
                                            params :: <object>) => ()
  // TODO this is only a dummy
  let textDocument = params["textDocument"];
  let uri = textDocument["uri"];
  let languageId = textDocument["languageId"];
  let version = textDocument["version"];
  let text = textDocument["text"];
  local-log("textDocument/didOpen: File %s of type %s, version %s, length %d\n",
            uri, languageId, version, size(text));
  // Only bother about dylan files for now.
  if (languageId = "dylan")
    register-file(uri, text);
  end if;
  show-info(session, "handle-textDocument/didOpen");
  if (*project*)
    // This is just test code.
    // Let's see if we can find a module
    let u = as(<url>, uri);
    let f = make-file-locator(u);
    let (m, l) = file-module(*project*, f);
    local-log("File: %=\nModule: %=, Library: %=\n",
              as(<string>, f),
              if (m) environment-object-primitive-name(*project*, m) else "?" end,
              if (l) environment-object-primitive-name(*project*, l) else "?" end);
  end if;
end function;

// Go to definition:
// See https://microsoft.github.io/language-server-protocol/specifications/specification-3-15/#textDocument_definition

define function handle-textDocument/definition(session :: <session>,
                                               id :: <object>,
                                               params :: <object>) => ()
  let text-document = params["textDocument"];
  let uri = text-document["uri"];
  let position = params["position"];
  let (l, c) = decode-position(position);
  let doc = element($documents, uri, default: #f);
  let location = $null;
  if (doc)
    unless (doc.document-module)
      let local-dir = make(<directory-locator>, path: locator-path(doc.document-uri));
      let local-file = make(<file-locator>, directory: local-dir,
                            name: locator-name(doc.document-uri));
      local-log("local-dir=%s\nlocal-file=%s\n", as(<string>, local-dir),
                as(<string>, local-file));
      doc.document-module := file-module(*project*, local-file);
    end;
    let symbol = symbol-at-position(doc, l, c);
    let (target, line, char) = lookup-symbol(session, symbol, module: doc.document-module);
    if (target) 
      local-log("Lookup %s and got target=%s, line=%d, char=%d\n", symbol, target, line, char);
      let uri = make-file-uri(target); // TODO
      location := make-location(as(<string>, uri), line, char);
    else
      local-log("Lookup %s, not found\n", symbol);
    end;
  end;
  send-response(session, id, location);
end function;

define function handle-workspace/didChangeConfiguration(session :: <session>,
                                            id :: <object>,
                                                        params :: <object>) => ()
  // NOTE: vscode always sends this just after initialized, whereas
  // emacs does not, so we need to ask for config items ourselves and
  // not wait to be told.
  local-log("Did change configuration\n");
  local-log("Settings: %s\n", encode-json-to-string(params));
  // TODO do something with this info.
  let settings = params["settings"];
  let dylan-settings = settings["dylan"];
  *project-name* := element(dylan-settings, "project", default: #f);
  //show-info(session, "The config was changed");
  test-open-project(session);
end function;

// Debug print out a file locator.
define function deo(l :: <file-locator>) => ()
  local-log("%= %= %= %=\n",
            as(<string>, l),
            locator-path(l),
            locator-name(l),
            locator-relative?(l));
end;

define function trailing-slash(s :: <string>) => (s-with-slash :: <string>)
  if (s[s.size - 1] = '/')
    s
  else
    concatenate(s, "/")
  end
end;

/* Handler for 'initialized' message.
 * Here we will register the dynamic capabilities of the server with the client.
 * Note we don't do this yet, any capabilities are registered statically in the 
 * 'initialize' message.
 * Here also we will start the compiler session.
 */
define function handle-initialized(session :: <session>,
                                            id :: <object>,
                                            params :: <object>) => ()
  /* Commented out because we don't need to do this (yet)
  let hregistration = json("id", "dylan-reg-hover",
                           "method", "textDocument/hover");
  let oregistration = json("id", "dylan-reg-open",
                           "method", "textDocument/didOpen");

  send-request(session, "client/registerCapability", json("registrations", list(hregistration, oregistration)),
               callback: method(session, params)
                           local-log("Callback called back..%s\n", session);
                           show-info(session, "Thanks la")
                         end);
*/
  show-info(session, "Dylan LSP server started.");
  local-log("debug: %s, messages: %s, verbose: %s\n", *debug-mode*, *trace-messages*, *trace-verbose*);
  let in-stream = make(<string-stream>);
  let out-stream = make(<string-stream>, direction: #"output");

  // Test code
  local-log("Env O-D-R=%s, PATH=%s\n",
            environment-variable("OPEN_DYLAN_RELEASE"),
            environment-variable("PATH"));
  send-request(session, "workspace/workspaceFolders", #f, callback: handle-workspace/workspaceFolders);
  *server* := start-compiler(in-stream, out-stream);
  test-open-project(session);
end function handle-initialized;

define function test-open-project(session) => ()  
    show-info(session, "test-open-project 0");
  // TODO don't hard-code the project name and module name.
  local-log("Select project %=\n", find-project-name());
    show-info(session, "test-open-project 1");

  *project* := open-project(*server*, find-project-name());
  show-info(session, "test-open-project 2");
    // Let's see if we can find a module
  let (m, l) = file-module(*project*, "/home/peter/Projects/lsp-dylan/testproject/testproject.dylan");
  local-log("Try\nModule: %=, Library: %=\n",
            if (m) environment-object-primitive-name(*project*, m) else "?" end,
            if (l) environment-object-primitive-name(*project*, l) else "?" end);

  *module* := "testproject";
  local-log("Test, listing sources:\n");
  for (s in project-sources(*project*))
    let rl = source-record-location(s);
    local-log("Source: %=, a %= in %= \n",
              s,
              object-class(s),
              as(<string>, rl));
              
    deo(rl);
  end;
  if (*project*)
    local-log("Test, listing project file libraries\n");
    do-project-file-libraries(method(l, r)
                                  local-log("Lib:%= Rec:%=\n", l, r);
                              end,
                              *project*,
                              as(<file-locator>, "/home/peter/Projects/lsp-dylan/testproject/testproject.dylan"));
  end;
//  local-log("dylan-sources:%=\n", project-dylan-sources(*project*));
  local-log("Compiler started:%=, project %=\n", *server*, *project*);
  local-log("Database: %=\n", project-compiler-database(*project*));
end function;

define function add-trailing-slash(s :: <string>) => (s-slash :: <string>)
  if (last(s) = '/')
    s
  else
    concatenate(s, "/")
  end;
end function;

/* Handle the 'initialize' message.
 * Here we initialize logging/tracing and store the workspace root for later.
 * Here we return the 'static capabilities' of this server.
 * In the future we can register capabilities dynamically by sending messages
 * back to the client; this seems to be the preferred 'new' way to do things.
*/
define function handle-initialize (session :: <session>,
                                   id :: <object>,
                                   params :: <object>) => ()
  let trace = element(params, "trace", default: "off");
  select (trace by \=)
    "off" => begin
               *trace-messages* := #f;
               *trace-verbose* := #f;
             end;
    "messages" => begin
                    *trace-messages* := #t;
                    *trace-verbose* := #f;
                  end;
    "verbose" => begin
                   *trace-messages* := #t;
                   *trace-verbose* := #t;
                 end;
    otherwise => local-log("trace must be \"off\", \"messages\" or \"verbose\", not %s\n", trace);
  end select;
  // Save the workspace root (if provided) for later.
  // TODO: can root-uri be something that's not a file:// URL?
  let root-uri  = element(params, "rootUri", default: #f);
  if (root-uri)
    let url = as(<url>, trailing-slash(root-uri));
    let dir = make(<directory-locator>, path: locator-path(url));
    session.root := dir;
  else
    let root-path = element(params, "rootPath", default: #f);
    if (root-path)
      session.root := as(<directory-locator>, root-path);
    end;
  end;
  // Set CWD
  if (session.root)
    working-directory() := session.root;
  end;
  local-log("Working directory is now:%s\n", as(<string>, working-directory()));
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

define function handle-workspace/workspaceFolders (session :: <session>,
                                                   params :: <object>)
   => ()
  local-log("Workspace folders were received\n");
end;
/* Document Management */
define constant $documents = make(<string-table>);
// Represents one open file (given to us by textDocument/didOpen)
define class <open-document> (<object>)
  constant slot document-uri, required-init-keyword: uri:;
  slot document-module, init-value: #f; // Module if we know it.
  slot document-lines, required-init-keyword: lines:;
end class;

define function register-file (uri, contents)
  let lines = split-lines(contents);
  local-log("register-file: %s(%s), lines: %d\n", uri, object-class(uri), size(lines));
  let doc = make(<open-document>, uri: as(<url>, uri), lines: lines);
  $documents[uri] := doc;
end function;

/* Given a document and a position, find the symbol that this position is within
If the position not on a symbol, return #f
*/
define function symbol-at-position (doc :: <open-document>, line, column) => (symbol :: false-or(<string>))
  if (line >=0 & line < size(doc.document-lines) & column >=0 & column < size(doc.document-lines[line]))
    let line = doc.document-lines[line];
    local method any-character?(c) => (well? :: <boolean>)
            member?(c, "abcdefghijklmnopqrstuvwxyzABCDEFGHIHJLKMNOPQRSTUVWXYZ0123456789!&*<>|^$%@_-+~?/=")
          end;
    if (any-character?(line[column]))
      let symbol-start = column;
      let symbol-end = column;
      while (symbol-start >= 0 & any-character?(line[symbol-start]))
      symbol-start := symbol-start - 1;
      end while;
      while (symbol-end < size(line) & any-character?(line[symbol-end]))
        symbol-end := symbol-end + 1;
      end while;
      copy-sequence(line, start: symbol-start + 1, end: symbol-end)
    else
      // Hovered over some 'punctuation'
      #f
    end if
  else
    // Not in range
    #f
  end;
end function;

define function unregister-file(uri)
  // TODO
  remove-key!($documents, uri)
end function;

/*
 * Make a file:// URI from a local file path.
 * This is supposed to follow RFC 8089
 * (locators library not v. helpful here)
 */
define function make-file-uri (f :: <file-locator>)
 => (uri :: <url>)
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
end;

define function make-file-locator (f :: <url>)
 => (loc :: <file-locator>)
  /* TODO - what if it isnt a file:/, etc etc */
  let d = make(<directory-locator>, path: locator-path(f));
  local-log("dir:%=", d);
  make(<file-locator>, directory: d, name: locator-name(f))
end;

// Look up a symbol. Return the containing doc,
// the line and column
define function lookup-symbol (session, symbol, #key module = #f) => (doc, line, column)
  let loc = symbol-location (symbol, module: module);
  if (loc)
    let source-record = loc.source-location-source-record;
    let absolute-path = source-record.source-record-location;
    let (name, line) = source-line-location(source-record,
                                          loc.source-location-start-line);
    let column = loc.source-location-start-column;
    values(absolute-path, line - 1, column)
  else
    local-log("Looking up %s, not found\n", symbol);
    #f
  end
end;

/* Find the project name to open.
 * Either it is set in the per-directory config (passed in from the client)
 * or we'll guess it is the only lid file in the workspace root.
 * If there is more than one lid file, that's an error, don't return
 * any project.
 * Returns: the name of a project
 */
define function find-project-name()
 => (name :: false-or(<string>))
  if (*project-name*)
    // We've set it explicitly
    local-log("Project name explicitly:%s\n", *project-name*);
    *project-name*;
  else
    // Guess based on there being one .lid file in the workspace root
    block(return) 
      local method return-lid(dir, name, type)
              local-log("Project scan %s\n", name);
              if (type = #"file")
                let file = as(<file-locator>, name);
                if (locator-extension(file) = "lid")
                  return (name);
                end if;
              end if;
            end method;
      do-directory(return-lid, working-directory());
      local-log("Project name, got nothing\n");
      #f
    end block;
  end if;
end function;

define function main
    (name :: <string>, arguments :: <vector>)
  //one-off-debug();

  // Command line processing
  if (member?("--debug", arguments, test: \=))
    *debug-mode* := #t;
  end if;
  // Set up.
  let msg = #f;
  let retcode = 1;
  let session = make(<stdio-session>);
  // Pre-init state
  while (session.state == $session-preinit)
    let (meth, id, params) = receive-message(session);
    select (meth by =)
      "initialize" => handle-initialize(session, id, params);
      "exit" => session.state := $session-killed;
      otherwise =>
        // Respond to any request with an error, and drop any notifications
        if (id)
          send-error-response(session, id, $server-not-initialized);
        end if;
    end select;
    flush(session);
  end while;
  // Active state
  while (session.state == $session-active)
    let (meth, id, params) = receive-message(session);
    select (meth by =)
      "initialize" =>
          send-error-response(session, id, $invalid-request);
      "initialized" => handle-initialized(session, id, params);
      "workspace/symbol" => handle-workspace/symbol(session, id, params);
      "textDocument/hover" => handle-textDocument/hover(session, id, params);
      "textDocument/didOpen" => handle-textDocument/didOpen(session, id, params);
      "textDocument/definition" => handle-textDocument/definition(session, id, params);
      "workspace/didChangeConfiguration" => handle-workspace/didChangeConfiguration(session, id, params);
      // TODO handle all other messages here
      "shutdown" =>
        begin
          // TODO shutdown everything
          send-response(session, id, $null);
          session.state := $session-shutdown;
        end;
      "exit" => session.state := $session-killed;
      otherwise =>
      // Respond to any other request with an not-implemented error.
      // Drop any other notifications
        begin
          local-log("%s '%s' is not implemented\n",
                    if (id)
                      "Request"
                    else
                      "Notification"
                    end,
                    meth);
          if (id)
            send-error-response(session, id, $method-not-found);
          end if;
        end;
    end select;
    flush(session);
  end while;
  // Shutdown state
  while (session.state == $session-shutdown)
    let (meth, id, params)  = receive-message(session);
    select (meth by =)
      "exit" =>
        begin
          retcode := 0;
          session.state := $session-killed;
        end;
      otherwise =>
        // Respond to any request with an invalid error,
        // Drop any notifications
        begin
          if (id)
            send-error-response(session, id, $invalid-request);
          end if;
        end;
    end select;
    flush(session);
  end while;

  exit-application(retcode);
end function main;

main(application-name(), application-arguments());



// Local Variables:
// indent-tabs-mode: nil
// compile-command: "dylan-compiler -build lsp-dylan"
// End:
