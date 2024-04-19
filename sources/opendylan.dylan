Module: lsp-dylan-impl
Synopsis: Communicaton with the Open Dylan command-line compiler
Author: Peter
Copyright: 2019

// The basis of this code is taken from the dswank module.
// Author:    Andreas Bogk and Hannes Mehnert
// Copyright: Original Code is Copyright (c) 2008-2012 Dylan Hackers; All rights reversed.


define constant $open-dylan-user-registries = "OPEN_DYLAN_USER_REGISTRIES";

define variable *dylan-compiler* :: false-or(<command-line-server>) = #f;

// Currently we only support a single open project. This is #f until
// textDocument/didOpen is first called.
define variable *project* :: false-or(od/<project-object>) = #f;

define variable *library* = #f;
define variable *project-name* = #f;

define function start-compiler
    (input-stream, output-stream) => (server :: <command-line-server>)
  od/make-environment-command-line-server(input-stream: input-stream,
                                          output-stream: output-stream)
end function;

// Execute a single 'command-line' style command on the server
define function run-compiler(server, string :: <string>) => ()
  execute-command-line(server, string);
end function;

// Open the project containing `file`.
define function lsp-open-project
    (session, file :: <file-locator>)
 => (project :: false-or(od/<project-object>), project-name :: false-or(<string>))
  let project-name = find-project-name(file);
  log-debug("lsp-open-project: Found project name %=", project-name);
  let command = make-command(od/<open-project-command>,
                             server: server-context(*dylan-compiler*),
                             file: as(<file-locator>, project-name));

  // Set OPEN_DYLAN_USER_REGISTRIES so we aren't depending on the working
  // directory being correct.  Reset it to the original value when done so we
  // don't keep appending the same value to it.  This obviously isn't thread
  // safe; <open-project-command> needs a way to pass this through.
  let original-user-registries = environment-variable($open-dylan-user-registries);
  let project = #f;
  block ()
    let space = ws/load-workspace(directory: file.locator-directory);
    // Make sure the file's workspace registry is first.
    let regs = concatenate(list(as(<string>, ws/workspace-registry-directory(space))),
                           if (original-user-registries)
                             list(original-user-registries)
                           else
                             #()
                           end);
    let user-registries = join(regs, if ($os-name == #"win32") ";" else ":" end);
    log-debug("lsp-open-project: Setting ODUR to %=", user-registries);
    environment-variable($open-dylan-user-registries) := user-registries;
    project := execute-command(command);
    log-debug("lsp-open-project: Result of opening %= is %=", project-name, project);
  cleanup
    environment-variable($open-dylan-user-registries)
      := original-user-registries;
  exception (err :: <abort>)
    // This is usually because no registry file was found. Why isn't a better
    // error signaled?
    log-debug("lsp-open-project: condition of class %= signaled", err);
  end;

  // Debugging only...

  if (~project)
    log-debug("lsp-open-project: project did't open");
  else
    let (mod, lib) = project & od/file-module(project, file);
    log-debug("lsp-open-project: mod = %=, lib = %=", mod, lib);
    log-debug("lsp-open-project: Try Module: %=, Library: %=",
              mod & od/environment-object-primitive-name(project, mod),
              lib & od/environment-object-primitive-name(project, lib));

    log-debug("lsp-open-project: project-library = %=", od/project-library(project));
    log-debug("lsp-open-project: project db = %=", od/project-compiler-database(project));

    let warn = curry(log-warning, "open-project-compiler-database: %=");
    let db = od/open-project-compiler-database(project, warning-callback: warn);
    log-debug("lsp-open-project: db = %=", db);
    for (s in od/project-sources(project))
      let rl = source-record-location(s);
      log-debug("lsp-open-project: Source: %=, a %= in %=",
                s,
                object-class(s),
                as(<string>, rl));
    end;
    log-debug("lsp-open-project: listing project file libraries:");
    od/do-project-file-libraries(method (l, r)
                                   log-debug("lsp-open-project: Lib: %= Rec: %=", l, r);
                                 end,
                                 project,
                                 as(<file-locator>, "library.dylan"));
    log-debug("lsp-open-project: Database: %=", od/project-compiler-database(project));
  end if;

  values(project, project-name)
end function;

// Get a symbol's description from the compiler database.
// This is used to implement the 'hover' function.
//
// Parameters:
//  symbol-name - a <string>
//  module - a <module-object> or #f
// Returns:
//  description - a <string> or #f
define function describe-symbol
    (symbol-name :: <string>, #key module) => (description :: false-or(<string>))
  let env = get-environment-object(symbol-name, module: module);
  if (env)
    od/environment-object-description(*project*, env, module)
  end
end function;

// Given a definition, make a list of all the places it is used.
//
// Parameters:
//  object - the <definition-object> to look up.
//  include-self? If true, the list also includes the source record of the passed-in object.
// Returns:
//  A sequence of source records.
define function all-references
    (object :: od/<definition-object>, #key include-self?) => (references :: <sequence>)
  let clients = od/source-form-clients(*project*, object);
  if (include-self?)
    add(clients, object)
  else
    clients
  end if;
end function;

// Given an environment-object, get its source-location
define function get-location
    (object :: od/<environment-object>) => (location :: <source-location>)
  od/environment-object-source-location(*project*, object);
end function;

define function list-all-package-names ()
  local method collect-project
            (dir :: <pathname>, filename :: <string>, type :: <file-type>)
          if (type == #"file")
            if (last(filename) ~= '~')
              log-debug("%s", filename);
            end;
          end;
        end;
  let regs = od/find-registries(as(<string>, target-platform-name()));
  let reg-paths = map(od/registry-location, regs);
  for (reg-path in reg-paths)
    if (file-exists?(reg-path))
      do-directory(collect-project, reg-path);
    end;
  end;
end function;

define method n (x :: od/<environment-object>)
  // for debugging!
  let s = od/print-environment-object-to-string(*project*, x);
  format-to-string("%s%s", object-class(x), s);
end;

define method n (x :: <string>)
  format-to-string("\"%s\"", x)
end;

define method n (x :: <locator>)
  format-to-string("locator:\"%s\"", as(<string>, x))
end;

define method n (x == #f)
  "#f"
end;

define function get-environment-object
    (symbol-name :: <string>, #key module) => (object :: false-or(od/<environment-object>))
  let library = od/project-library(*project*);
  log-debug("%s -> module is %s", symbol-name, n(module));
  od/find-environment-object(*project*, symbol-name,
                             library: library, module: module);
end function;

// Given a definition, find all associated definitions.
// Returns a sequence of <definition-object>s.
define generic all-definitions
  (server :: od/<server>, object :: od/<definition-object>) => (definitions :: <sequence>);

// For most definition objects it's just a list with the thing itself
define method all-definitions
    (server :: od/<server>, object :: od/<definition-object>) => (definitions :: <sequence>)
  list(object)
end method;

// For generic functions it's the GF at the front followed by the GF methods.
define method all-definitions
    (server :: od/<server>, gf :: od/<generic-function-object>) => (definitions :: <sequence>)
  local method source-locations-equal? (def1, def2)
          // Note that there's a source-location-equal? method but it doesn't
          // work for <compiler-range-source-location>s. We should fix that.
          let loc1 = od/environment-object-source-location(server, def1);
          let loc2 = od/environment-object-source-location(server, def2);
          loc1.source-location-source-record = loc2.source-location-source-record
            & loc1.source-location-start-line = loc2.source-location-start-line
            & loc1.source-location-end-line = loc2.source-location-end-line
        end;
  let methods = od/generic-function-object-methods(server, gf);
  // Add gf to the result, but only if it's not an implicitly defined generic
  // function, since that would cause unnecessary prompting for which method
  // when there's only one. Since <generic-function-object>s have no
  // implicit/explicit marker, look for equal source locations.
  if (any?(curry(source-locations-equal?, gf), methods))
    methods
  else
    concatenate(vector(gf), methods) // Put gf first.
  end
end method;

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
                     #"lsp",   // our own temp category. debug-out(#"lsp", ...)
                     #"project-manager");
  local method lsp-debug-out (fn :: <function>)
          let (fmt, #rest args) = apply(values, fn());
          // I wish we could log the "part" here, but debug-out drops it.
          apply(log-debug, concatenate("debug-out: ", fmt), args)
        end;
  debug-out-function() := lsp-debug-out;
  // Not yet...
  //*dfmc-debug-out* := #(#"whatever");  // For dfmc-common's debug-out.
end function;
