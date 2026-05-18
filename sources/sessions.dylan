Module: lsp-dylan-impl


define constant $session-preinit  = 1;
define constant $session-active   = 2;
define constant $session-shutdown = 3;
define constant $session-killed   = 4;

// Manage the connection to the client, including life cycle.
define class <session> (<object>)
  // Next ID to use in a request/notification.  The spec doesn't say whether these IDs
  // are in a separate namespace from the requests sent to us from the client, but we
  // assume they are.  That is, we may receive a request with ID 0 that is distinct from
  // a request with ID 0 that we send to the client.
  slot %id :: <integer> = 0;
  slot %state :: <integer> = $session-preinit;
  slot %trace :: <integer> = $trace-verbose;
   // ID => function to handle response, called with session, id, params.
  constant slot %callbacks = make(<equal-table>);
end class;

define generic send-raw-message (session :: <session>, message :: <object>) => ();
define generic receive-raw-message (session :: <session>) => (message :: <object>);

// Flush pending messages
define generic flush (session :: <session>) => ();

// Send a request message.
// Optionally, register a callback to be called with the response
// to this message.
// The callback is a function as defined with 'define message-handler'.
define generic send-request
    (session :: <session>, method-name :: <string>, params :: <object>,
     #key callback) => ();

// Send the response to a request with identifier id.
// This applies to a successful request.
define generic send-response
    (session :: <session>, id :: <object>, result :: <object>) => ();

// Send an error response to the request with identifier id.
// Optionally include a human-readable error message and extra data
define generic send-error-response
    (session :: <session>, id :: <object>, error-code :: <integer>,
     #key error-message, error-data)
 => ();

// Send an LSP notification-type message.
// This has a method name but no ID because it isn't replied to
define generic send-notification
    (session :: <session>, method-name :: <string>, params :: <object>) => ();

// Get the next message.
// If the message is a notification or request, return it
// for processing. If it is a response to a request sent
// by the server, look up the reponse callback and call it.
define generic receive-message
    (session :: <session>)
 => (method-name :: <string>, id :: <object>, params :: <object>);

// --------------------------------------------------------------------------------
// <stdio-session>

define class <stdio-session> (<session>)
  constant slot %input-stream :: <stream>, required-init-keyword: input-stream:;
  constant slot %output-stream :: <stream>, required-init-keyword: output-stream:;
end class;

// --------------------------------------------------------------------------------
// Projects

// A simple wrapper for projects that we explicitly open, so we can track metadata for them.
define class <project> (<object>)
  constant slot %project-object :: od/<project-object>, required-init-keyword: project-object:;

  // Sequence of URI strings that had warnings in the most recent build of this project,
  // kept so that if those same files have no warnings in the next build their
  // diagnostics can be cleared.
  sealed slot %diagnostics-uris :: <sequence> = #[];
end class;
