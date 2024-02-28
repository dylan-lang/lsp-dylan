# Dylan Language Server

This is an implementation of the [Language Server
Protocol](https://microsoft.github.io/language-server-protocol/) for
Dylan.


## Current Status

As of 2022-11-07, the server implements

* Jump to declaration
* Jump to definition
* Diagnostics (i.e., compiler warnings)
* Hover (i.e., argument lists)

When applied to a symbol which is bound to a generic function, "jump to
definition" will show a clickable list containing the generic function and
its specific methods, whereas "jump to declaration" will jump straight to
the generic function.

We are currently using version [3.15 of the LSP protocol](https://microsoft.github.io/language-server-protocol/specifications/specification-3-15/).


## Opening Projects

The LSP server needs to be able to open a project (that is, a Dylan library)
associated with the file you're editing when you turn on LSP in your editor. It
makes two attempts, in the following order:

1. Search up in the directory structure until it finds a `workspace.json` file,
   which indicates a [dylan-tool](https://github.com/dylan-lang/dylan-tool)
   workspace.  In this case it looks for the "default-library" setting in the
   workspace file and opens that library.  If there is no default library set
   and there is only one "active" library, it uses that. Otherwise it fails.

2. Search up in the directory structure for a `registry` directory and open the
   library associated with the first `*.lid` file it finds there.

Normally you shouldn't need to set any environment variables; everything is
derived from the full pathname to the `dylan-compiler` executable, which must
be on your `PATH`.

However, sometimes you may be working on several libraries at once and they're
not all listed in the registry found as described above. In that case, add all
the relevant registry directories to `OPEN_DYLAN_USER_REGISTRIES`. For example:

    export OPEN_DYLAN_USER_REGISTRIES=/project1/registry:/project2/registry

See [Using Source
Registries](https://opendylan.org/getting-started-cli/source-registries.html)
for more.

## Emacs Usage

1. Make sure the `dylan-lsp-server` executable is on your `PATH`, or customize
   the `lsp-dylan-exe-pathname` elisp variable. See below for more on
   customization.

1. Install [lsp-mode](https://github.com/emacs-lsp/lsp-mode) and Dylan mode.
   Both of these are available from MELPA.

2. When you jump to another `.dylan` file, that file does not automatically
   have LSP enabled so you must use `M-x lsp` again. To make this automatic,
   add this to your emacs init file:

       `(add-hook 'dylan-mode-hook 'lsp)`

3. Start emacs and make sure that `lsp-dylan.el` is loaded. For example:

     `emacs --load=/path/to/lsp-dylan/lsp-dylan.el`

   You will probably want to modify your Emacs init file to load the file.

4. Open a Dylan source file and type `M-x lsp` to start the client (unless you
   added the hook above, in which case it started automatically).

The client starts the LSP server (the `dylan-lsp-server` executable) and
connects to it.

The emacs client has a customization group "Lsp Dylan" which is a member of the
"Programming / Tools" group, and has the following variables:

* `lsp-dylan-exe-pathname`
* `lsp-dylan-extra-command-line-options`
* `lsp-dylan-log-pathname`
* `lsp-dylan-open-dylan-release`

These are documented in the customization interface within emacs. Use `M-x
customize-group` `lsp-dylan` to customize these variables.

## Visual Studio Code Usage

These instructions were tested on Linux and macOS.

1.  Install Visual Studio Code and `npm`.
2.  The VS Code extension is in the folder `vscode-dylan`. It is necessary to
    run `npm install` in this folder before starting the extension for the
    first time, and any time a git pull updates the dependencies.
3.  Open the `vscode-dylan` folder in VS Code.
4.  In the debug viewlet, click the green play arrow (Launch Extension) or
    press `F5`.
5.  A build process will begin in 'watch mode'; whenever the source is changed,
    the extension will be rebuilt. It is possible to debug the VS Code
    extension in this window, set breakpoints, watch variables and so on.
6.  A new VS Code window will open with the extension running.
7.  Open a folder with a Dylan project in it.
8.  If `dylan-lsp-server` is on the system path, it will be found. Otherwise,
    open the Settings *in the new extension window*, find the Dylan section
    under Extensions, and edit the path to the LSP server. The full, absolute
    pathname to the executable needs to be specified. It is usually better to
    set this in the 'User' scope, otherwise it will only apply to that
    particular project.
9.  It should now be possible to use the extension window to edit Dylan code
    using LSP.
10. If the VS Code extension is changed, it is necessary to restart the
    extension host, or just close and re-open the extension window.


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
