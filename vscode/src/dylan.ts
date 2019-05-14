/* --------------------------------------------------------------------------------------------
 * Based on LSP example code which is
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 * ------------------------------------------------------------------------------------------ */

import * as path from 'path';
import { workspace, ExtensionContext } from 'vscode';

import {
	LanguageClient,
	LanguageClientOptions,
	ServerOptions
} from 'vscode-languageclient';

let client: LanguageClient;

export function activate(context: ExtensionContext) {
	// The server is implemented in node
	let serverExe = context.asAbsolutePath(
		path.join('..', '_build', 'bin', 'lsp-dylan')
	);
	let serverOptions: ServerOptions = {
		run: {
			command: serverExe
		},
		debug: { command: serverExe, args: ['--debug'] }
	};

	// Options to control the language client
	let clientOptions: LanguageClientOptions = {
		// Register the server for dylan source documents
		documentSelector: [{ scheme: 'file', language: 'dylan' }],
		synchronize: {configurationSection: 'dylan'}
	};

	// Create the language client and start the client.
	client = new LanguageClient(
		'dylan',
		'Dylan Language Server',
		serverOptions,
		clientOptions
	);
	
	// Start the client. This will also launch the server
	let disposable = client.start();
	context.subscriptions.push(disposable);
}

export function deactivate(): Thenable<void> | undefined {
	if (!client) {
		return undefined;
	}
	return client.stop();
}
