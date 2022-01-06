Module: lsp-dylan
Synopsis: Communicaton with the Dylan command-line compiler
Author: Peter
Copyright: 2019

// The basis of this code is taken from the dswank module.
// Author:    Andreas Bogk and Hannes Mehnert
// Copyright: Original Code is Copyright (c) 2008-2012 Dylan Hackers; All rights reversed.


define variable *server* = #f;
define variable *project* = #f;
define variable *module* = #f;
define variable *library* = #f;
define variable *project-name* = #f;

define function start-compiler
    (input-stream, output-stream) => (server :: <command-line-server>)
  make-environment-command-line-server(input-stream: input-stream,
                                       output-stream: output-stream)
end function;

// Execute a single 'command-line' style command on the server
define function run-compiler(server, string :: <string>) => ()
  execute-command-line(server, string);
end function run-compiler;

// Ask the command line compiler to open a project.
// Param: server - the command line server.
// Param: name - either a library name or a lid file.
// Returns: an instance of <project-object>
define function open-project
    (server, name :: <string>) => (project :: <object>)
  let command = make-command(<open-project-command>,
                             server: server.server-context,
                             file: as(<file-locator>, name));
  let project = execute-command(command);
  log-debug("Result of opening %s is %=", name, project);
  log-debug("Result of find %s is %=",
            project-name(project),
            find-project(project-name(project)));
  project
end function;


define function describe-symbol (symbol-name)
  let env = get-environment-object(symbol-name, module: *module*);
  environment-object-description(*project*, env, *module*)
end;

define function symbol-location
    (symbol-name :: <string>, #key module)
  let env = get-environment-object(symbol-name, module: module);
  if (env)
    environment-object-source-location(*project*, env)
  else
    log-debug("No environment object for %s in module %s", symbol-name, module);
  end
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
  let regs = find-registries(as(<string>, target-platform-name()));
  let reg-paths = map(registry-location, regs);
  for (reg-path in reg-paths)
    if (file-exists?(reg-path))
      do-directory(collect-project, reg-path);
    end;
  end;
end function;

define function one-off-debug ()
  //list-all-package-names();
  let in-stream = make(<string-stream>);
  let out-stream = make(<string-stream>, direction: #"output");
  let srv = start-compiler(in-stream, out-stream);
  let project = open-project(srv, "testproject");
  log-debug("Database: %=", project-compiler-database(project));

  *project* := project;
  *server* := srv;
  let symbol-name = "zeor";
  let library = project-library(project);
  let module = find-module(project, "testproject", library: library);
  let loc = environment-object-source-location(project, module).source-location-source-record;
  let env = find-environment-object(project,
                                    symbol-name,
                                    library: library,
                                    module: module);
  log-debug("one-off-debug:\n  find-environment-object(%s, %s, library:%s, module:%s) => %=",
            n(project),
            n(symbol-name),
            n(library),
            n(module),
            n(env));
  let fp = as(<file-locator>, "/Users/peterhull/Projects/lsp-dylan/testproject/library.dylan");
  let pfl = source-record-location(loc);
//  fp := "testproject.dylan";
//  fp := pfl;
  let (m, l) = file-module(project, fp);
  log-debug("one-off-debug:\n  1. %=\n  2. %=\n3. %=", n(fp), n(m), n(l));
  let same? = pfl = fp;
  log-debug("one-off-debug:\n  1. %=\n  2. %=\n",
            locator-relative?(fp),
            locator-relative?(pfl));
  log-debug("one-off-debug:\n  1. %=\n  2. %=\n",
            locator-base(fp),
            locator-base(pfl));
  log-debug("one-off-debug:\n  1. %=\n  2. %=\n",
            locator-extension(fp),
            locator-extension(pfl));
  log-debug("one-off-debug:\n  1. %=\n  2. %=\n  3.%=\n",
            locator-path(fp),
            locator-path(pfl),
            same?);
end function;

define method n (x :: <environment-object>)
  // for debugging!
  let s = print-environment-object-to-string(*project*, x);
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
    (symbol-name :: <string>, #key module) => (o :: false-or(<environment-object>))
  let library = project-library(*project*);
  log-debug("%s -> module is %s", symbol-name, n(module));
  find-environment-object(*project*, symbol-name,
                          library: library,
                          module: module);
end function;
