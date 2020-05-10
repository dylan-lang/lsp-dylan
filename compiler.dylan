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



