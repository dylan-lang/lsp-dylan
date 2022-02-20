# Dylan Language Server

This is an implementation of the [Language Server
Protocol](https://microsoft.github.io/language-server-protocol/) for
Dylan.


## Current Status as of Feb 2022

We are currently using version [3.15 of the LSP protocol](https://microsoft.github.io/language-server-protocol/specifications/specification-3-15/).

These features are implemented:

*  Jump to definition (`textDocument/definition`)
*  Hover text (`textDocument/hover`)
*  Compiler diagnostics (i.e., warnings) on saving a file.

In Emacs, when you open a new `.dylan` file (whether manually or by `M-.`) that
file does not in automatically have LSP enabled so you must use `M-x lsp`
again.

When using Emacs in a terminal, compiler warnings are highlighted in red or
yellow and may be navigated with `flymake` commands like
`flymake-goto-next-error`. With Emacs GUI the fringe and the mode-line are
clickable.


## How Projects are Opened

The LSP server needs to be able to open a project (a Dylan library) associated
with the file you're editing when you turn on LSP in your editor. It does this
by searching up in the directory structure until it finds a `workspace.json`
file, which indicates a [dylan-tool](https://github.com/dylan-lang/dylan-tool)
workspace.  It looks for the "default-library" setting in that file and opens
that library.  Example `workspace.json` file:

```json
{
    "default-library": "http"
}
```

See the [dylan-tool documentation](https://github.com/dylan-lang/dylan-tool)
for how to create workspaces.


## Emacs Usage

1. Install [lsp-mode](https://github.com/emacs-lsp/lsp-mode).

2. Set environment variables.

   a. The LSP server opens projects via the Dylan registry. The registry
      directory is located in the Dylan workspace root, and `lsp-dylan`
      automatically does compilation in that directory so that Open Dylan will
      find the registry.

      If you make changes to project dependencies make sure the registry is
      up-to-date by running `dylan update` inside the workspace.

      If you are developing `lsp-dylan` itself you will probably want to add
      the Open Dylan registry to `OPEN_DYLAN_USER_REGISTRIES`.  Otherwise, when
      you use `M-.` etc to jump to definitions for used libraries, files in the
      Open Dylan install directory (which is not under source control) will be
      opened. Example:

        export OPEN_DYLAN_USER_REGISTRIES=/path/to/opendylan/sources/registry

   b. Point `OPEN_DYLAN_RELEASE_INSTALL` at the Open Dylan installation
      directory. This is necessary so that the compiler can find the Jam build
      scripts. For example:

        export OPEN_DYLAN_RELEASE_INSTALL=/path/to/opendylan-2020.1

3. Start emacs and make sure that `setup.el` is loaded. For example:

     `emacs --load=/path/to/lsp-dylan/setup.el`

   Obviously you may modify your Emacs init file instead, if you prefer.

4. Open a Dylan source file and type `M-x lsp` to start the client. The client
   starts the LSP server (the `dylan-lsp-server` executable) and connects to
   it. You must either `(setq dylan-lsp-exe-pathname
   "/path/to/dylan-lsp-server")` in your Emacs init file or make sure that the
   `dylan-lsp-server` binary is on your `PATH`.


## Visual Studio Code Usage

These instructions were tested on Linux and macOS.

1.  Install Visual Studio Code and `npm`.
2.  The vscode extension is in the folder `vscode-dylan`. It is necessary to run 
    `npm install` in this folder before starting the extension for the first time, and any 
    time a git pull updates the dependencies.
3.  Open the `vscode-dylan` folder in VS Code.
4.  In the debug viewlet, click the green play arrow (Launch Extension) or press `F5`
5.  A build process will begin in 'watch mode'; whenever the source is changed, the
    extension will be rebuilt. It is possible to debug the vscode extension in this
    window, set breakpoints, watch variables and so on.
6.  A new VS Code window will open with the extension running. 
7.  Open a folder with a Dylan project in it.
8.  If `dylan-lsp-server` is on the system path, it will be found. Otherwise, open the 
    Settings *in the new extension window*, find the Dylan section under Extensions, and
    edit the path to the LSP server. The full, absolute pathname to the executable needs
    to be specified. It is usually better to set this in the 'User' scope, otherwise it will
    only apply to that particular project.
9.  It should now be possible to use the extension window to edit Dylan code using LSP.
10. If the vscode extension is changed, it is necessary to restart the extension host, or
    just close and re-open the extension window.


## References

* [Intro to LSP from
  Microsoft](https://docs.microsoft.com/en-us/visualstudio/extensibility/language-server-protocol)
  Besides being a quick introduction, this has links to some other tools that
  may help in developing VS Code integration for Dylan.

* [LSP v3.15
  Specification](https://microsoft.github.io/language-server-protocol/specifications/specification-3-15/)
  This is the version we are currently coding to.

* [langserver.org](https://langserver.org/) lists LSP implementations that
  support at least one of the six major LSP features.
