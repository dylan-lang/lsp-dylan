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
  execute-command(command)
end function;


define function describe-symbol (symbol-name)
  let env = get-environment-object(symbol-name);
  environment-object-description(*project*, env, *module*)
end;

define function symbol-location (symbol-name)
  let env = get-environment-object(symbol-name);
  environment-object-source-location(*project*, env)
end function;

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
  local method check-and-set-module (p, lib)
          unless(module)
            if (instance?(*module*, <string>)) 
              module := find-module(p, *module*, library: lib);
              local-log("find-module(%=, %=, library:%=)->%=\n",
                        p, *module*, lib, module);
            else
              module := *module*;
            end;
            if (module)
              library := lib;
            end;
          end;
        end;
  for(p in open-projects())
    local-log("%= is an open project with %=\n", p, project-compiler-database(p));
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

