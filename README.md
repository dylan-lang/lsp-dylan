# Dylan Language Server

This is an implementation of the [Language Server
Protocol](https://microsoft.github.io/language-server-protocol/) for
Dylan.


## Current Status

As of 2022-09-07, the server implements

* Jump to declaration
* Jump to definition
* Hover

When applied to a symbol which is bound to a generic function, "jump to 
definition" will show a clickable list containing the generic function and 
its specific methods, whereas "jump to declaration" will jump straight to 
the generic function.

In Emacs, when you jump to another `.dylan` file, that file does
not in automatically have LSP enabled so you must use `M-x lsp` again.

We are currently using version [3.15 of the LSP protocol](https://microsoft.github.io/language-server-protocol/specifications/specification-3-15/).


## Opening Projects

The LSP server needs to be able to open a project (essentially a Dylan library)
associated with the file you're editing when you turn on LSP in your editor. It
makes two attempts, in the following order:

1. Search up in the directory structure until it finds a `workspace.json` file,
   which indicates a [dylan-tool](https://github.com/dylan-lang/dylan-tool)
   workspace.  In this case it looks for the "default-library" setting in the
   workspace file and opens that library.  If there is no default library set
   and there is only one "active" library, it uses that. Otherwise it fails.

2. Search up in the directory structure for a `registry` directory and open the
   library associated with the first `*.lid` file it finds there. (Note that it
   currently removes the `.lid` suffix and assumes that a library by the same
   name as the basename of the file can be opened via the registry. This should
   eventually be fixed.)


## Emacs Usage

Testing with Emacs [lsp-mode](https://github.com/emacs-lsp/lsp-mode).

1. Install [lsp-mode](https://github.com/emacs-lsp/lsp-mode) and Dylan mode. 
   Both these are available from MELPA.

2. Set environment variables.

   a. Currently the LSP server only opens projects via the Dylan registry so
      it's important to either start emacs in the directory containing your
      "registry" directory or set `OPEN_DYLAN_USER_REGISTRIES` to contain that
      registry directory.

      If you are developing the lsp-dylan code itself and also modifying Open
      Dylan at the same time, you may want to include
      `.../opendylan/sources/registry` in the list as well. Otherwise, when you
      use `M-.` etc to jump to definitions for used libraries, files in the
      Open Dylan install directory (which is not under source control) will be
      opened. For example:

        export OPEN_DYLAN_USER_REGISTRIES=/path/to/lsp-dylan/registry:/path/to/opendylan/sources/registry

   b. The server needs the Open Dylan installation directory, so it
      can find the Jam build scripts and core libraries. If
      `dylan-compiler` is on the path, the emacs client will find the
      installation directory relative to that. To override it, either
      set the emacs variable `dylan-lsp-open-dylan-release` (using the
      customization interface) or set the environment variable
      `OPEN_DYLAN_RELEASE_INSTALL`. For example:

          export OPEN_DYLAN_RELEASE_INSTALL=/path/to/opendylan-2021.1

      The emacs variable takes priority if both are set.

3. Start emacs and make sure that `setup.el` is loaded. For example:

     `emacs --load=/path/to/lsp-dylan/setup.el`

   Obviously you may modify your Emacs init file instead, if you prefer.

4. Open a Dylan source file and type `M-x lsp` to start the client. The client
   starts the LSP server (the `dylan-lsp-server` executable) and connects to
   it. You must either `(setq dylan-lsp-exe-pathname
   "/absolute/path/to/dylan-lsp-server")` in your Emacs init file or make sure
   that the `dylan-lsp-server` binary is on your `PATH`.
   
The emacs client has a customization group "Dylan Lsp" which is a member of the "Programming 
/ Tools" group, and has the following variables:

* `dylan-lsp-exe-pathname`
* `dylan-lsp-debug-server`
* `dylan-lsp-debug-opendylan`
* `dylan-lsp-log-pathname`

These are documented in the customization interface within emacs.

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
  would help in developing VS Code integration for Dylan.

* [LSP v3.15
  Specification](https://microsoft.github.io/language-server-protocol/specifications/specification-3-15/)
  This is the version we are currently coding to.

* [langserver.org](https://langserver.org/) lists LSP implementations that
  support at least one of the six major LSP features.
