Module: dylan-user

define library lsp-dylan
  use common-dylan;
  use io;
  use network;
  use lisp-reader;
  use json;
  use system;
  use dylan;
  use strings;
  use environment-commands;
  use environment-protocols;
  use build-system;
  use commands;
  use environment-internal-commands;
  use dfmc-reader;
  use source-records;
  use file-source-records;
  use system;
  use registry-projects;
  //use stack-walker;
  use release-info;
  use dfmc-back-end-implementations;
end library;

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
end module lsp-dylan;
