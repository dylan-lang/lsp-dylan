Module: dylan-lsp-server

define command-line <lsp-server-command-line> ()
  option debug-server? :: <boolean> = #f,
    names: #("debug-server"),
    kind: <flag-option>,
    help: "Turn on debugging for the LSP server. [%default%]";
  option debug-opendylan? :: <boolean> = #f,
    names: #("debug-opendylan"),
    kind: <flag-option>,
    help: "Turn on debugging for Open Dylan. [%default%]";
  option log-file,
    names: #("log"),
    variable: "FILE",
    kind: <parameter-option>,
    default: "dylan-lsp-server.log",
    help: "Server log file. [%default%]";
end command-line;

define function main
    (name :: <string>, arguments :: <vector>)
  let command = make(<lsp-server-command-line>,
                     help: "Dylan LSP server");
  block ()
    parse-command-line(command, application-arguments());
    lsp-server-top-level(debug-server?: command.debug-server?,
                         debug-opendylan?: command.debug-opendylan?,
                         log-file: command.log-file);
  exception (err :: <abort-command-error>)
    format-err("Error: %s\n", err);
    force-err();
    exit-application(exit-status(err));
  exception (err :: <error>)
    // If the 'initialize' LSP handler was successful the log will be in the workspace
    // root directory, otherwise in the working directory where lsp-dylan was started.
    log-error("Error: %s", err);
    format-err("Error: %s", err);
    force-err();
    exit-application(1);
  end;
end function;

main(application-name(), application-arguments());
