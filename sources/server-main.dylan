Module: dylan-lsp-server

define command-line <lsp-server-command-line> ()
  option debug-server? :: <boolean> = #f,
    names: #("debug-server"),
    kind: <flag-option>,
    help: "Enter the debugger (or crash with a backtrace) on error. [%default%]";
  option debug-opendylan? :: <boolean> = #f,
    names: #("debug-opendylan"),
    kind: <flag-option>,
    help: "Include Open Dylan debug messages in the log. [%default%]";
  option log-file,
    names: #("log"),
    variable: "FILE",
    kind: <parameter-option>,
    default: "dylan-lsp-server.log",
    help: "Server log file, relative to the initial workspace root directory if not an absolute path. [%default%]"
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
  end;
end function;

main(application-name(), application-arguments());
