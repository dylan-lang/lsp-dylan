Module: dylan-user

define library lsp-dylan
  use build-system;
  use command-line-parser;
  use commands;
  use common-dylan;
  use dfmc-back-end-implementations; // For side effect?
  use dfmc-reader;
  use dylan;
  use deft;
  use environment-commands;
  use environment-internal-commands; // For side effect
  use environment-protocols;
  use file-source-records;
  use io;
  use json;
  use lisp-reader;
  use logging;
  use network;
  use registry-projects;
  use release-info;
  use source-records;
  use strings;
  use system;
  use uncommon-dylan;

  export
    lsp-dylan,
    lsp-dylan-impl;
end library;

define module lsp-dylan
  create
    lsp-server-top-level;
end module;

define module lsp-dylan-impl
  use lsp-dylan;

  use build-system;
  use command-lines;
  use commands;
  use common-dylan;
  use file-source-records;
  use file-system;
  use format;
  use json;
  use locators;
  use logging;
  use operating-system;
  use print;
  use simple-debugging;
  use standard-io;
  use streams;
  use strings;
  use threads;
  use workspaces, prefix: "ws/";
  use uncommon-utils;

  // Modules in Open Dylan proper (not those in opendylan/sources/lib) use the od/ prefix.
  use dfmc-reader, prefix: "od/";
  use environment-commands, prefix: "od/";
  use environment-protocols, prefix: "od/";
  use registry-projects, prefix: "od/";

  // For test suite.
  export
    session-id,
    json,
    send-raw-message,
    send-request,
    <stdio-session>,
      session-output-stream;
end module;
