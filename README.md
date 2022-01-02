# Dylan Language Server

This is an implementation of the [Language Server
Protocol](https://microsoft.github.io/language-server-protocol/) for
Dylan.


## Current Status

As of 2022-01-04, the only function fully implemented is "jump to definition"
and (at least in Emacs) when you jump to another `.dylan` file, that file does
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

1. Install [lsp-mode](https://github.com/emacs-lsp/lsp-mode).

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

   b. Point `OPEN_DYLAN_RELEASE_INSTALL` at the Open Dylan installation
      directory. This is necessary so that it can find the Jam build scripts,
      and core libraries. For example:

        export OPEN_DYLAN_RELEASE_INSTALL=/path/to/opendylan-2021.1

3. Start emacs and make sure that `setup.el` is loaded. For example:

     `emacs --load=/path/to/lsp-dylan/setup.el`

   Obviously you may modify your Emacs init file instead, if you prefer.

4. Open a Dylan source file and type `M-x lsp` to start the client. The client
   starts the LSP server (the `lsp-dylan` executable) and connects to it.

   Currently `lsp-dylan` must be in `./_build/bin/lsp-dylan` or
   `${DYLAN}/workspaces/lsp/_build/bin/lsp-dylan`. (TODO: search for it on
   `PATH`.)

## VS Code Usage (1.45.0 on macos)

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
