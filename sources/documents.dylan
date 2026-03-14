Module: lsp-dylan-impl
Synopsis: A document abstraction and management of same


// Maps URI strings to <document> objects.
define constant $documents = make(<string-table>);

// Represents one open file (given to us by textDocument/didOpen)
define class <document> (<object>)
  // The original URI string passed to us by the client to open this document.
  constant slot %uri :: <string>, required-init-keyword: uri:;
  constant slot %locator :: <file-locator>, required-init-keyword: locator:;
  constant slot %project :: od/<project-object>, required-init-keyword: project:;
  constant slot %version :: <integer>, required-init-keyword: version:;
  slot %lines :: <sequence>, required-init-keyword: lines:;
end class;

define method print-object
    (doc :: <document>, stream :: <stream>) => ()
  printing-object (doc, stream)
    format(stream, "%s@%d", doc.%uri, doc.%version);
  end;
end method;

define method document-module
    (doc :: <document>) => (module :: false-or(od/<module-object>))
  log-debug("document-module: project: %=, locator: %=", doc.%project.od/project-name, doc.%locator);
  od/file-module(doc.%project, doc.%locator) // 2nd value is library, ignored
end method;

define function find-document (uri :: <string>) => (doc :: false-or(<document>))
  element($documents, uri, default: #f)
end function;

// Open a document and the Dylan project it is associated with.
define function open-document
    (session :: <session>, uri :: <string>, text :: <string>, version :: <integer>)
 => (doc :: false-or(<document>))
  let file = file-uri-to-locator(uri);
  let ws = with-logged-stdio ()
             ws/load-workspace(directory: file.locator-directory)
           end;
  local method matches-current-platform?
            (lid :: ws/<lid>) => (matches? :: <boolean>)
          let platforms = ws/lid-values(lid, #"platforms") | #[];
          empty?(platforms)
            | member?($platform-name, map(curry(as, <symbol>), platforms))
        end method;
  let library
    // TODO: export something like "locator-library" from workspaces.
    = block (exit-block)
        for (lid keyed-by lid-path in ws/lids-by-pathname(ws))
          if (matches-current-platform?(lid))
            for (filename in ws/lid-values(lid, #"files"))
              let loc = file-locator(as(<file-locator>, lid-path).locator-directory,
                                     if (ends-with?(filename, ".dylan"))
                                       filename
                                     else
                                       concatenate(filename, ".dylan")
                                     end);
              if (loc = file)
                exit-block(ws/library-name(lid, error?: #t));
              end;
            end;
          end;
        end for;
      end block;
  if (~library)
    error("project not found for %s", uri);
  else
    // TODO: I think this command stuff is due to code being inherited from DIME. We
    // should be able to call OD's open-project directly (whatever it's called).
    let command = make-command(od/<open-project-command>,
                               server: server-context(*dylan-compiler*),
                               file: as(<file-locator>, library));
    let project :: false-or(od/<project-object>)
      = execute-command(command)
          | error("project not found for library %=", library);
    let doc = make(<document>,
                   uri: uri,
                   locator: file,
                   lines: split-lines(text),
                   version: version,
                   project: project);
    $documents[uri] := doc;
    build-project(session, doc, link?: #f);
  end if;
end function;

// Characters that are part of the Dylan "name" BNF.
define constant $dylan-name-characters
  = "abcdefghijklmnopqrstuvwxyzABCDEFGHIHJLKMNOPQRSTUVWXYZ0123456789!&*<>|^$%@_-+~?/=";

// Given a document and a position, find the Dylan name (identifier) that is at
// (or immediately precedes) this position. If the position is, for example,
// the open paren following a function name, we should still find the name. If
// there is no name at position, return #f.
//
// TODO: Fancy stuff like if the line begins with "define" look up the "-definer".  Maybe
// return a second value to indicate that this might be a definer.
define function dylan-name-at-position
    (doc :: <document>, line, column) => (symbol :: false-or(<string>))
  if (line >= 0
        & line < size(doc.%lines)
        & column >= 0
        & column <= size(doc.%lines[line]))
    let line = doc.%lines[line];
    local method name-character?(c) => (well? :: <boolean>)
            member?(c, $dylan-name-characters)
          end;
    let name-start = column;
    let name-end = column;
    while (name-start > 0 & name-character?(line[name-start - 1]))
      name-start := name-start - 1;
    end;
    while (name-end < size(line) & name-character?(line[name-end]))
      name-end := name-end + 1;
    end while;
    let name = copy-sequence(line, start: name-start, end: name-end);
    ~empty?(name) & name
  else
    log-debug("line %d column %d not in range for document %s",
              line, column, doc.%uri);
    #f
  end
end function;
