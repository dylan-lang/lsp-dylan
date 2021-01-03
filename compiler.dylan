Module: lsp-dylan
Synopsis: Communicaton with the Dylan command-line compiler
Author: Peter
Copyright: 2019

/* The basis of this code is taken from the dswank module
* author:    Andreas Bogk and Hannes Mehnert
* copyright: Original Code is Copyright (c) 2008-2012 Dylan Hackers; All rights reversed.
*/

define variable *server* = #f;
define variable *project* = #f;
define variable *module* = #f;
define variable *library* = #f;
define variable *project-name* = #f;

define function start-compiler(input-stream, output-stream)
  make-environment-command-line-server(input-stream: input-stream,
                                       output-stream: output-stream)
end function;

// Execute a single 'command-line' style command on the server
define function run-compiler(server, string :: <string>) => ()
  execute-command-line(server, string);
end function run-compiler;

/* Ask the command line compiler to open a project.
 * Param: server - the command line server
 * Param: name - the project name (either a registry name or a lid file)
 * Returns: the project object. (instance of <project-object>)
*/
define function open-project(server, name :: <string>)
 => (project :: <object>)
  let command = make-command(<open-project-command>,
                             server: server.server-context,
                             file: as(<file-locator>, name));
  let project = execute-command(command);
  local-log("Result of opening %s is %=\n", name, project);
  local-log("Result of find %s is %=\n", project-name(project),
            find-project(project-name(project)));
  project
end function;


define function describe-symbol (symbol-name)
  let env = get-environment-object(symbol-name);
  environment-object-description(*project*, env, *module*)
end;

define function symbol-location (symbol-name, #key module = #f)
  let env = symbol-name & get-environment-object(symbol-name, module: module);
  if (env)
    environment-object-source-location(*project*, env)
  else
    local-log("No environment object for %s\n", symbol-name);
  end
end function;

define function list-all-package-names ()
  local method collect-project
            (dir :: <pathname>, filename :: <string>, type :: <file-type>)
          if (type == #"file")
            if (last(filename) ~= '~')
              local-log("%s\n", filename);
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
end;

define function one-off-debug()
  //list-all-package-names();
  let in-stream = make(<string-stream>);
  let out-stream = make(<string-stream>, direction: #"output");
  let srv = start-compiler(in-stream, out-stream);
  let project = open-project(srv, "testproject");
  local-log("Database: %=\n", project-compiler-database(project));

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
  local-log("one-off-debug:\nfind-environment-object(%s, %s, library:%s, module:%s) => %=\n",
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
  local-log("one-off-debug:\n1. %=\n2. %=\n3. %=\n", n(fp), n(m), n(l));
  let same? = pfl = fp;
  local-log("one-off-debug:\n1. %=\n2. %=\n",
            locator-relative?(fp),
            locator-relative?(pfl));
  local-log("one-off-debug:\n1. %=\n2. %=\n",
            locator-base(fp),
            locator-base(pfl));
  local-log("one-off-debug:\n1. %=\n2. %=\n",
            locator-extension(fp),
            locator-extension(pfl));
  local-log("one-off-debug:\n1. %=\n2. %=\n3.%=\n",
            locator-path(fp),
            locator-path(pfl),
            same?);
end;

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
define method n ( x == #f)
  "#f"
end;
define function get-environment-object (symbol-name, #key module = #f)
  let library = project-library(*project*);
  unless (module)
    // TODO not hard code
    module := find-module(*project*, "testproject", library: library);
  end;
  local-log("%s -> module is %s\n", symbol-name, n(module));

  find-environment-object(*project*, symbol-name,
                                    library: library,
                                    module: module);
end;

