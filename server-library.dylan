Module: dylan-user

define library dylan-lsp-server
  use common-dylan;
  use command-line-parser;
  use logging;
  use lsp-dylan;
  use system;
end library;

define module dylan-lsp-server
  use common-dylan;
  use command-line-parser;
  use file-system;
  use locators;
  use logging;
  use lsp-dylan;
end module;
