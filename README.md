# Dylan Language Server

This is an implementation of the [Language Server
Protocol](https://microsoft.github.io/language-server-protocol/) for
Dylan.  At the moment it will respond to the initialize/ shutdown
sequence but does not actually implement any of the methods defined in
the LSP.  Note that this project includes a git submodule for
json. This adds `null` handling and a method to output json as a
string to the open-dylan/json project.

We are currently using version [3.15 of the LSP protocol](https://microsoft.github.io/language-server-protocol/specifications/specification-3-15/)

## Usage

Testing with Emacs [lsp-mode](https://github.com/emacs-lsp/lsp-mode).

1. Install lsp-mode (see github project page for details)
2. Start emacs with `emacs --load=setup.el testproject/testproject.dylan` in
   this directory. (For now "testproject" is the single, hard-coded project
   name, soon to be fixed.)
3. Type `M-x lsp` to start the client, which will connect to the server

The file `setup.el` is used just to avoid making any changes to the
user's `.emacs`.

If you are **not** using [dylan-tool](https://github.com/cgay/dylan-tool) then
you must set `OPEN_DYLAN_RELEASE` to wherever your "opendylan" directory is and
`OPEN_DYLAN_USER_REGISTRIES` to the appropriate "registry" directory.

Currently the only function is `lsp-find-definition` which will jump to the
definition of the symbol under the cursor. Unfortunately it is still not
reliable and depends on some hard-coded defaults.

Testing with VS Code (1.45.0 on macos)

1. Open the `vscode` folder in VS Code
1. First time only, `npm install` to get the dependencies
2. Start the extension client with `CTRL+SHIFT+B`
3. In the debug viewlet, click the green play arrow (Launch Extension)
4. A new VS Code window will open with the extension running.


## References

* [Intro to LSP from
  Microsoft](https://docs.microsoft.com/en-us/visualstudio/extensibility/language-server-protocol)
  Besides being a quick introduction, this has links to some other tools that
  would help in developing VS Code integration for Dylan.

* [LSP v3.15
  Specification](https://microsoft.github.io/language-server-protocol/specifications/specification-3-15/)
  This is the version we are currently coding to.

* [langserver.org](https://langserver.org/) lists LSP implementations that
  support at least one of the six major LSP features.
