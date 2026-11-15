// SPDX-License-Identifier: GPL-3.0-or-later
import assert from 'node:assert/strict';
import { once } from 'node:events';
import { createServer } from 'node:http';
import test from 'node:test';
import { createApp } from '../dist/server.js';
import { fixtureBackend, id } from '../evals/fixtures.mjs';

async function listen(t, server) {
    t.after(() => new Promise(resolve => {
        server.closeAllConnections();
        server.close(resolve);
    }));
    server.listen(0, '127.0.0.1');
    await once(server, 'listening');
    return 'http://127.0.0.1:' + server.address().port;
}

test('chat delivers reasoning before completion and executes a streamed tool call', async t => {
    let finish;
    const canFinish = new Promise(resolve => { finish = resolve; });
    t.after(finish);
    const inference = await listen(t, createServer(async (req, res) => {
        const chunks = [];
        for await (const chunk of req) chunks.push(chunk);
        const input = JSON.parse(Buffer.concat(chunks));
        assert.equal(input.stream, true);
        res.writeHead(200, { 'content-type': 'text/event-stream' });
        const emit = (delta, finish_reason = null) =>
            res.write('data: ' + JSON.stringify({ choices: [{ delta, finish_reason }] }) + '\n\n');
        if (input.messages.at(-1).role === 'tool') {
            const result = JSON.parse(input.messages.at(-1).content);
            assert.equal(result.tool, 'list_currencies');
            emit({ reasoning_content: 'Using the retrieved currencies.' });
            emit({ content: 'GBP is configured, according to OreStudio.' }, 'stop');
            return res.end('data: [DONE]\n\n');
        }
        emit({ reasoning_content: 'Checking the configured catalogue.' });
        await canFinish;
        emit({ tool_calls: [{ index: 0, id: 'call-1', function: { name: 'list_currencies', arguments: '{"limit":' } }] });
        emit({ tool_calls: [{ index: 0, function: { arguments: '1}' } }] });
        emit({}, 'tool_calls');
        res.end('data: [DONE]\n\n');
    }));
    const fixture = fixtureBackend();
    const backend = { ...fixture, request: async (op, ...args) => {
        if (op === 'login') return {
            success: true, token: 'backend-fixture-token', session_id: 'fixture-session',
            username: 'fixture', tenant_id: id(1), selected_party_id: id(2),
        };
        if (op === 'logout') return { success: true };
        return fixture.request(op, ...args);
    } };
    const url = await listen(t, createApp({ allowedOrigins: [], inference: {
        baseUrl: inference, model: 'fixture', timeoutMs: 5000,
    } }, backend));
    const login = await fetch(url + '/auth/login', {
        method: 'POST', body: JSON.stringify({ principal: 'fixture', password: 'fixture' }),
    });
    assert.ok(login.ok);
    const { accessToken } = await login.json();
    const response = await fetch(url + '/chat', {
        method: 'POST',
        headers: { Accept: 'application/x-ndjson', Authorization: 'Bearer ' + accessToken },
        body: JSON.stringify({ message: 'List configured currencies.' }),
        signal: AbortSignal.timeout(5000),
    });
    assert.equal(response.headers.get('content-type'), 'application/x-ndjson');
    const reader = response.body.getReader();
    const decoder = new TextDecoder();
    let pending = '';
    const readEvent = async () => {
        while (!pending.includes('\n')) {
            const chunk = await reader.read();
            assert.equal(chunk.done, false, 'Expected another chat event');
            pending += decoder.decode(chunk.value, { stream: true });
        }
        const newline = pending.indexOf('\n');
        const event = JSON.parse(pending.slice(0, newline));
        pending = pending.slice(newline + 1);
        return event;
    };
    try {
        assert.deepEqual(await readEvent(), {
            type: 'reasoning', text: 'Checking the configured catalogue.',
        });
        finish();
        assert.deepEqual(await readEvent(), { type: 'reasoning', text: 'Using the retrieved currencies.' });
        const { type, result } = await readEvent();
        assert.equal(type, 'result');
        assert.match(result.answer, /GBP/);
        assert.doesNotMatch(result.answer, /Checking the configured catalogue/);
        assert.equal(result.evidence.length, 1);
    } finally {
        finish();
        await reader.cancel();
    }
});
