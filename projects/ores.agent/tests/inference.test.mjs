import test from 'node:test';
import assert from 'node:assert/strict';
import { createServer } from 'node:http';
import { once } from 'node:events';
import { HttpInference } from '../dist/inference.js';
import { validateConfig } from '../dist/config.js';
test('HTTP inference uses bounded responses and deadlines; malformed/oversized replies fail closed', async () => {
    let mode = 'ok', outputBudget = 2048;
    const server = createServer(async (req, res) => {
        let data = ''; for await (const chunk of req) data += chunk;
        const input = JSON.parse(data); assert.equal(input.max_tokens, outputBudget); assert.equal(input.parallel_tool_calls, false);
        res.setHeader('content-type', 'application/json');
        if (mode === 'huge') return res.end('x'.repeat(66000));
        if (mode === 'invalid') return res.end('{broken');
        if (mode === 'timeout') return;
        res.end(JSON.stringify({ choices: [{ finish_reason: mode === 'length' ? 'length' : 'stop', message: { role: 'assistant', content: 'test fixture' } }] }));
    });
    server.listen(0, '127.0.0.1'); await once(server, 'listening');
    const inference = new HttpInference({ baseUrl: `http://127.0.0.1:${server.address().port}/v1`, model: 'fixture', timeoutMs: 100 });
    const call = () => inference.complete([{ role: 'user', content: 'fixture' }], [], new AbortController().signal);
    try {
        assert.equal((await call()).content, 'test fixture');
        outputBudget = 1024;
        assert.equal((await new HttpInference({ ...inference.config, maxOutputTokens: 1024 }).complete([{ role: 'user', content: 'fixture' }], [], new AbortController().signal)).content, 'test fixture');
        outputBudget = 2048;
        mode = 'invalid'; await assert.rejects(call, /invalid response/);
        mode = 'huge'; await assert.rejects(call, /exceeds limit/);
        mode = 'length'; await assert.rejects(call, /response budget/);
        mode = 'timeout'; await assert.rejects(call, /timed out/);
    } finally { server.closeAllConnections(); await new Promise(r => server.close(r)); }
});

test('configured inference output allowances are bounded integers', () => {
    const config = { host: '127.0.0.1', port: 0, allowedOrigins: [], nats: null,
        inference: { baseUrl: 'http://127.0.0.1:20010/v1', model: 'fixture', timeoutMs: 1000 } };
    assert.equal(validateConfig(config).inference.maxOutputTokens, undefined);
    for (const value of [128, 2048, 8192]) assert.equal(validateConfig({ ...config, inference: { ...config.inference, maxOutputTokens: value } }).inference.maxOutputTokens, value);
    for (const value of [0, 127, 8193, 1.5, '2048', null]) assert.throws(() => validateConfig({ ...config, inference: { ...config.inference, maxOutputTokens: value } }), /maxOutputTokens/);
});
