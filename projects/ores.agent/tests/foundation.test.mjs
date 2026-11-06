import test from 'node:test';
import assert from 'node:assert/strict';
import { once } from 'node:events';
import { createApp } from '../dist/server.js';
import { validateConfig } from '../dist/config.js';
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
});
