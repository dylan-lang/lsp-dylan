/* --------------------------------------------------------------------------------------------
 * Based on LSP example code which is
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 * ------------------------------------------------------------------------------------------ */

import * as path from 'path';
import { ExtensionContext } from 'vscode';

import {
	LanguageClient,
	LanguageClientOptions,
	ServerOptions,
	ExecutableOptions
} from 'vscode-languageclient';

let client: LanguageClient;
// TODO - get from env?
function user_registries() {
	return ["/Users/peterhull/registry"].join(';');
}

export function activate(context: ExtensionContext): void {
	// The server is implemented in dylan native code
	const serverExe = context.asAbsolutePath(
		path.join('..', '_build', 'bin', 'lsp-dylan')
	);

	const openDylanRelease =
		// HACK - should find the real dylan-compiler or allow
		// to be overridden
		path.join(path.dirname('/opt/local/2020.1pre/bin/dylan-compiler'), '..');
	const runOptions: ExecutableOptions = {
		env: {
			...process.env, OPEN_DYLAN_RELEASE: openDylanRelease,
			OPEN_DYLAN_USER_REGISTRIES: user_registries()
		}
	}
	const serverOptions: ServerOptions = {
		run: {
			command: serverExe,
			options: runOptions
		},
		debug: {
			command: serverExe, args: ['--debug'],
			options: runOptions
		}
	};

	// Options to control the language client
	const clientOptions: LanguageClientOptions = {
		// Register the server for dylan source documents
		documentSelector: [{ scheme: 'file', language: 'dylan' }],
		synchronize: { configurationSection: 'dylan' }
	};

	// Create the language client and start the client.
	client = new LanguageClient(
		'dylan',
		'Dylan Language Server',
		serverOptions,
		clientOptions
	);

	// Start the client. This will also launch the server
	const disposable = client.start();
	context.subscriptions.push(disposable);
}

export function deactivate(): Thenable<void> | undefined {
	if (!client) {
		return undefined;
	}
	return client.stop();
}
