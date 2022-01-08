Module: lsp-dylan-test-suite

define test test-send-request ()
  let session = make(<stdio-session>,
                     input-stream: make(<string-stream>),
                     output-stream: make(<string-stream>, direction: #"output"));
  send-request(session, "MakeWhirledPeas", json("param1", 1));
  assert-equal(1, session.id);
  let output = session.output-stream.stream-contents;
  assert-true(starts-with?(output, "Content-Length: "));
  assert-true(find-substring(output, "param1"));
end test;
