# Dylan Language Server

This is an implementation of the [Language Server
Protocol](https://microsoft.github.io/language-server-protocol/) for
Dylan.  At the moment it will respond to the initialize/ shutdown
sequence but does not actually implement any of the methods defined in
the LSP.  Note that this project includes a git submodule for
json. This adds `null` handling and a method to output json as a
string to the open-dylan/json project.

## Usage

Testing with Emacs [lsp-mode](https://github.com/emacs-lsp/lsp-mode).

1. Install lsp-mode (see github project page for details)
2. Start emacs with `emacs --load=setup.el` in this directory
3. Open a Dylan file
4. Type `M-x lsp` to start the client, which will connect to the server

The file `setup.el` is used just to avoid making any changes to the
user's `.emacs`.

Testing with VS Code (1.33.1 on x64 linux)

1. Open the `vscode` folder in VS Code
1. First time only, `npm install` to get the dependencies
2. Start the extension client with `CTRL+SHIFT+B`
3. In the debug viewlet, click the green play arrow (Launch Extension)
4. A new VS Code window will open with the extension running.




