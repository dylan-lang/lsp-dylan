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

define function start-compiler(input-stream, output-stream)
  make-environment-command-line-server(input-stream: input-stream,
                                       output-stream: output-stream)
end function;

// Execute a single 'command-line' style command on the server
define function run-compiler(server, string :: <string>) => ()
  execute-command-line(server, string);
end function run-compiler;

define function open-project(server, project-name :: <string>)
 => (project :: <object>)
  let command = make-command(<open-project-command>,
                             server: server.server-context,
                             file: as(<file-locator>, project-name));
  execute-command(command);
  find-project(project-name);
end function;


define function describe-symbol (symbol-name)
  let env = get-environment-object(symbol-name);
  environment-object-description(*project*, env, *module*)
end;

define function symbol-location (symbol-name)
  let env = symbol-name & get-environment-object(symbol-name);
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
  let project = *project*;
  let symbol-name = "testproject:zeor";
  let library = project-library(project);
  let module = find-module(project, "testproject", library: library);

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

end;

define method n (x :: <environment-object>)
  // for debugging!
  print-environment-object-to-string(*project*, x);
end;
define method n (x :: <string>)
  format-to-string("\"%s\"", x)
end;
define method n ( x == #f)
  "#f"
end;
define function get-environment-object (symbols)
  let env = split(symbols, ":");
  let symbol-name = env[0];
  let library = #f;
  let module = #f;
  let project = #f;

  if (env.size == 3)
    project := *project*;
    library := find-library(project, env[2]);
    module := find-module(project, env[1], library: library);
  end;
  *module* := "testproject";
  local method check-and-set-module (p, lib)
          unless(module)
            module := find-module(p, *module*, library: lib);
            if (module)
              library := lib;
            end;
          end;
        end;
  for(p in open-projects())
    unless (project)
      check-and-set-module(p, project-library(p));
      do-project-used-libraries(curry(check-and-set-module, p), p, p);
      if (library)
        project := p;
      end;
    end;
  end;
  if (env.size == 1)
    *project* := project;
    *library* := library;
    *module* := module;
  end;

  find-environment-object(project, symbol-name,
                                    library: library,
                                    module: module);
end;

