Module: lsp-dylan-test-suite

define test test-send-request ()
  assert-equal(send-request, send-request); // TODO
end test;
