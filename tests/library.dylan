Module: dylan-user

define library lsp-dylan-test-suite
  use common-dylan;
  use testworks;
  use lsp-dylan;
end library;

define module lsp-dylan-test-suite
  use common-dylan;
  use testworks;
  use lsp-dylan-impl;
end module;
