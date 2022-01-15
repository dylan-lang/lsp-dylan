Module: dylan-lsp-server

define command-line <lsp-server-command-line> ()
  option debug-server? :: <boolean> = #t, // default to #f eventually
    names: #("debug-server"),
    kind: <flag-option>,
    help: "Turn on debugging for the LSP server. [%default%]";
  option debug-opendylan? :: <boolean> = #t, // default to #f eventually
    names: #("debug-opendylan"),
    kind: <flag-option>,
    help: "Turn on debugging for Open Dylan. [%default%]";
  option log-file :: <file-locator>,
    names: #("log"),
    variable: "FILE",
    kind: <parameter-option>,
    default: as(<file-locator>, "dylan-lsp-server.log"),
    help: "Server log file. [%default%]";
end command-line;

define function main
    (name :: <string>, arguments :: <vector>)
  let command = make(<lsp-server-command-line>, help: "Dylan LSP server");
  block ()
    parse-command-line(command, application-arguments());

    // Log to stderr so it shows up in the *dylan-lsp::stderr* buffer.  Log to
    // a rolling temp file so we have a history and because I've seen the Emacs
    // LSP client's *dylan-lsp::stderr* buffer not be kept up to date when the
    // process restarts.
    let file-target = make(<rolling-file-log-target>,
                           pathname: command.log-file);
    *log* := make(<log>,
                  name: "lsp",
                  level: $debug-level,
                  targets: list($stderr-log-target, file-target));

    lsp-server-top-level(debug-server?: command.debug-server?,
                         debug-opendylan?: command.debug-opendylan?);
  exception (err :: <abort-command-error>)
    exit-application(exit-status(err));
  end;
end function;

main(application-name(), application-arguments());
