Module: lsp-dylan-impl
Synopsis: Communicaton with the Dylan command-line compiler
Author: Peter
Copyright: 2019

// The basis of this code is taken from the dswank module.
// Author:    Andreas Bogk and Hannes Mehnert
// Copyright: Original Code is Copyright (c) 2008-2012 Dylan Hackers; All rights reversed.


define variable *dylan-compiler* :: false-or(<command-line-server>) = #f;

define function start-compiler
    (input-stream, output-stream) => (server :: <command-line-server>)
  make-environment-command-line-server(input-stream: input-stream,
                                       output-stream: output-stream)
end function;

// Execute a single 'command-line' style command on the server
define function run-compiler(server, string :: <string>) => ()
  execute-command-line(server, string);
end function run-compiler;

// Ask the command line compiler to open a project. `name` may be a pathname to
// a .lid, .hdp, or .exe file pathname or a library name. If a library name,
// the registry is searched.
define function open-project
    (server :: <command-line-server>, name :: <string>) => (project :: <project-object>)
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

// Get a symbol's description from the compiler database.
define function symbol-description
    (name :: <string>, doc :: <document>) => (description :: false-or(<string>))
  let module = doc.ensure-document-module;
  let project = doc.document-project;
  let object = get-environment-object(project, name, module: module);
  if (object)
    environment-object-description(project, object, module)
  end
end function;

// Given a definition, make a list of all the places it is used.
//
// Parameters:
//  object - the <definition-object> to look up.
//  include-self? If true, the list also includes the source record of the passed-in object.
// Returns:
//  A sequence of source records.
define function object-references
    (object :: <definition-object>, doc :: <document>, #key include-self?)
 => (references :: <sequence>)
  let clients = source-form-clients(doc.document-project, object);
  if (include-self?)
    add(clients, object)
  else
    clients
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

// Let's get rid of this and just inline the calls to find-environment-object.
define function get-environment-object
    (project :: <project-object>, symbol-name :: <string>, #key module)
 => (object :: false-or(<environment-object>))
  find-environment-object(project, symbol-name,
                          library: project-library(project),
                          module: module)
end function;

// Given a definition, find all associated definitions.
// Returns a sequence of <definition-object>s.
define generic all-definitions
  (server :: <server>, object :: <definition-object>) => (definitions :: <sequence>);

// For most definition objects it's just a list with the thing itself
define method all-definitions
    (server :: <server>, object :: <definition-object>) => (definitions :: <sequence>)
  list(object)
end method;

// For generic functions it's the GF at the front followed by the GF methods.
define method all-definitions
    (server :: <server>, gf :: <generic-function-object>) => (definitions :: <sequence>)
  local method source-locations-equal? (def1, def2)
          // Note that there's a source-location-equal? method but it doesn't
          // work for <compiler-range-source-location>s. We should fix that.
          let loc1 = environment-object-source-location(server, def1);
          let loc2 = environment-object-source-location(server, def2);
          loc1.source-location-source-record = loc2.source-location-source-record
            & loc1.source-location-start-line = loc2.source-location-start-line
            & loc1.source-location-end-line = loc2.source-location-end-line
        end;
  let methods = generic-function-object-methods(server, gf);
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
