Module: dylan-user

define library lsp-dylan-test-suite
  use common-dylan;
  use io;
  use lsp-dylan;
  use strings;
  use testworks;
end library;

define module lsp-dylan-test-suite
  use common-dylan;
  use lsp-dylan-impl;
  use streams;
  use strings;
  use testworks;
end module;
