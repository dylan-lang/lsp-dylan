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
  use environment-protocols;
  use registry-projects;
  use build-system;
  use dfmc-reader;
  use source-records;
  use file-source-records;
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
  use environment-protocols, exclude: {
                                       open-project,
                                       application-filename,
                                       application-arguments,
                                       run-application};
  use commands;
  use registry-projects;
  use build-system;
  use dfmc-reader;
  use source-records;
  use file-source-records;
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
