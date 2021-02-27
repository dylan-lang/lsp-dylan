Module: dylan-user

define library lsp-dylan
  use build-system;
  use commands;
  use common-dylan;
  use dfmc-back-end-implementations;
  use dfmc-reader;
  use dylan;
  use environment-commands;
  use environment-internal-commands;
  use environment-protocols;
  use file-source-records;
  use io;
  use json;
  use lisp-reader;
  use network;
  use registry-projects;
  use release-info;
  use source-records;
  //use stack-walker;
  use strings;
  use system;
  use system;
end library;

define module lsp-dylan
  use build-system;
  use command-lines;
  use commands;
  use common-dylan;
  use dfmc-reader;
  use environment-commands;
  use environment-protocols,
    exclude: { open-project,
               application-filename,
               application-arguments,
               run-application};
  use file-source-records;
  use file-system;
  use format-out;
  use format;
  use json;
  use locators;
  use operating-system;
  use registry-projects;
  use source-records;
  use standard-io;
  use streams;
  use strings;
  use threads;
end module lsp-dylan;
