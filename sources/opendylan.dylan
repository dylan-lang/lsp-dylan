Module: lsp-dylan-impl
Synopsis: Communicaton with the Open Dylan command-line compiler
Author: Peter
Copyright: 2019

// The basis of this code is taken from the dswank module.
// Author:    Andreas Bogk and Hannes Mehnert
// Copyright: Original Code is Copyright (c) 2008-2012 Dylan Hackers; All rights reversed.


define variable *dylan-compiler* :: false-or(<command-line-server>) = #f;

define function start-compiler
    (input-stream, output-stream) => (server :: <command-line-server>)
  od/make-environment-command-line-server(input-stream: input-stream,
                                          output-stream: output-stream)
end function;

// Given a definition, make a list of all the places it is used.
//
// Parameters:
//  object - the <definition-object> to look up.
//  include-self? If true, the list also includes the source record of the passed-in object.
// Returns:
//  A sequence of source records.
define function all-references
    (object :: od/<definition-object>, project :: od/<project-object>, #key include-self?)
 => (references :: <sequence>)
  let clients = od/source-form-clients(project, object);
  if (include-self?)
    add(clients, object)
  else
    clients
  end if;
end function;

define function find-environment-object
    (name :: <string>, doc :: <document>)
 => (object :: false-or(od/<environment-object>))
  let library = od/project-library(doc.%project);
  od/find-environment-object(doc.%project, name,
                             library: library,
                             module: doc.document-module);
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
