Module: dylan-user

define library lsp-dylan
  use common-dylan;
  use io;
  use json;
  use system;
  use dylan;
  use strings;
  use commands;
  use environment-commands;
  export lsp-dylan;
end library lsp-dylan;

define module lsp-dylan
  use common-dylan;
  use format-out;
  use format;
  use standard-io;
  use streams;
  use file-system;
  use locators;
  use json;
  use threads;
  use operating-system;
  use strings;
  use command-lines;
  use environment-commands;
  use commands;
  export
    <session>,
    <stdio-session>,
    send-request,
    send-response,
    show-warning,
    show-info,
    show-error,
    show-log;
end module lsp-dylan;
