import test from 'node:test';
import assert from 'node:assert/strict';
import { once } from 'node:events';
import { createContext, runInContext } from 'node:vm';
import { createApp } from '../dist/server.js';
import { validateConfig } from '../dist/config.js';
import { serveUi } from '../dist/ui.js';
const config = { host: '127.0.0.1', port: 0, allowedOrigins: ['http://127.0.0.1'], nats: null, inference: { baseUrl: 'http://127.0.0.1:20010/v1', model: 'lfm3-1.7b', timeoutMs: 60000 } };
test('live health, dependency readiness, origin rejection and clean shutdown', async () => {
    const server = createApp(validateConfig(config));
    server.listen(0, '127.0.0.1'); await once(server, 'listening');
    const url = `http://127.0.0.1:${server.address().port}`;
    try {
        assert.equal((await fetch(url + '/healthz')).status, 200);
        assert.equal((await fetch(url + '/readyz')).status, 503);
        assert.equal((await fetch(url + '/healthz', { headers: { origin: 'http://untrusted.invalid' } })).status, 403);
    } finally { server.closeAllConnections(); await new Promise(r => server.close(r)); }
});
test('reject malformed and remotely bound configurations', () => {
    assert.throws(() => validateConfig({ ...config, host: '0.0.0.0' }));
    assert.throws(() => validateConfig({ ...config, port: -1 }));
    assert.throws(() => validateConfig({ ...config, nats: {} }));
    assert.throws(() => validateConfig({ ...config, inference: { ...config.inference, baseUrl: 'http://remote.invalid/v1' } }));
    assert.throws(() => validateConfig({ ...config, inference: { ...config.inference, baseUrl: 'https://user:secret@remote.invalid/v1' } }));
});

test('container config opts in to the container bind and private inference service', () => {
    const inference = { ...config.inference, baseUrl: 'http://llama:8080/v1' };
    assert.throws(() => validateConfig({ ...config, inference }));
    const container = { ...config, deployment: 'container', host: '0.0.0.0', inference };
    assert.deepEqual(validateConfig(container), container);
    for (const baseUrl of ['http://llama.example/v1', 'http://user@llama/v1', 'http://llama/v1?key=x']) {
        assert.throws(() => validateConfig({ ...container, inference: { ...inference, baseUrl } }));
    }
});

test('the composer submits browser-filled text without an input event', () => {
    let html;
    serveUi({ writeHead() {}, end(body) { html = body; } });
    assert.match(html, /<button id="send" type="submit">Send<\/button>/);
    const elements = new Map();
    const submissions = [];
    const context = createContext({
        document: { getElementById(id) {
            if (!elements.has(id)) elements.set(id, {});
            return elements.get(id);
        } },
        submissions,
    });
    runInContext(html.match(/<script\b[^>]*>([\s\S]*?)<\/script>/)[1], context);
    runInContext('sendChat = payload => submissions.push(payload.message)', context);
    const composer = elements.get('message');
    const chat = elements.get('chat');
    const submit = () => chat.onsubmit({ preventDefault() {} });
    chat.requestSubmit = submit;
    composer.value = '  show my runs  ';
    composer.onkeydown({ key: 'Enter', shiftKey: true, preventDefault: () => assert.fail('Shift+Enter should insert a newline') });
    composer.onkeydown({ key: 'Enter', preventDefault() {} });
    submit();
    composer.value = '   ';
    submit();
    assert.deepEqual(submissions, ['show my runs', 'show my runs']);
});
