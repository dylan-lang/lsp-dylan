Module: dylan-lsp-server

define command-line <lsp-server-command-line> ()
  option debug-server? :: <boolean> = #t, // default to #f eventually
    names: #("debug-server"),
    kind: <flag-option>,
    help: "Turn on debugging for the LSP server.";
  option debug-opendylan? :: <boolean> = #t, // default to #f eventually
    names: #("debug-opendylan"),
    kind: <flag-option>,
    help: "Turn on debugging for Open Dylan.";
end command-line;

define function main
    (name :: <string>, arguments :: <vector>)
  let command = make(<lsp-server-command-line>, help: "Dylan LSP server");
  block ()
    parse-command-line(command, application-arguments());
    lsp-server-top-level(debug-server?: command.debug-server?,
                         debug-opendylan?: command.debug-opendylan?);
  exception (err :: <abort-command-error>)
    exit-application(exit-status(err));
  end;
end function;

main(application-name(), application-arguments());
