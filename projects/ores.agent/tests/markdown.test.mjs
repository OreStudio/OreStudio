import test from 'node:test';
import assert from 'node:assert/strict';
import { renderMarkdown, markdownTable } from '../dist/markdown.js';
import { formatDiscovery } from '../dist/discovery.js';
import { createServer } from 'node:http';
import { once } from 'node:events';
import { createApp } from '../dist/server.js';

test('replies render headings, tables, lists, emphasis and fenced code', () => {
    const html = renderMarkdown('# Results\n\n## FX\n\n| Pair | Rate |\n| :--- | ---: |\n| EUR/USD | 1.0552 |\n\n- **Stored** quote\n\n```sql\nSELECT 1 < 2;\n```');
    for (const part of ['<h1>Results</h1>', '<h2>FX</h2>', '<table>', '<th>Rate</th>', '<td>1.0552</td>', '<ul>', '<strong>Stored</strong>', '<pre><code class="language-sql">SELECT 1 &lt; 2;']) assert.ok(html.includes(part), part);
    assert.doesNotMatch(html, /style=/);
});

test('raw HTML, unsafe links and images cannot execute or load remote content', () => {
    const html = renderMarkdown('<script>alert(1)</script>\n<img src=x onerror=alert(1)>\n\n[x](javascript:alert%281%29) [x](jav&#x61;script:alert%281%29) [x](data:text/html,test)\n\n![track](https://example.com/pixel)\n\n[Safe](https://example.com)');
    assert.doesNotMatch(html, /<script|<img|href="(?:javascript|data|vbscript|file):/i);
    assert.match(html, /&lt;script&gt;/);
    assert.match(html, /href="https:\/\/example.com"/);
});

test('table cells keep literal names and exact numbers without adding markup', () => {
    const html = renderMarkdown(markdownTable(['Name', 'Value'], [['A|B\n# <img> & **raw**', '-6502.44000001']]));
    assert.equal((html.match(/<td>/g) ?? []).length, 2);
    assert.match(html, /A\|B # &lt;img&gt; &amp; \*\*raw\*\*/);
    assert.match(html, /-6502.44000001/);
    assert.doesNotMatch(html, /<img|<strong|<h1/);
    assert.equal(markdownTable(['Name'], []), '');
});

test('record discovery produces a table without losing scope or pagination', () => {
    const answer = formatDiscovery({ tool: 'find_books', status: 'partial', totalMatches: 2, catalogueScope: 'selected_party', offset: 0, nextOffset: 1, rows: [{ name: 'FX | Hedges', id: 'book-id' }] });
    const html = renderMarkdown(answer);
    assert.match(html, /<table>/);
    assert.match(html, /<td>FX \| Hedges<\/td>/);
    assert.match(html, /book-id/);
    assert.match(html, /Partial catalogue search/);
    assert.match(html, /Next catalogue offset: 1/);
});

test('authenticated chat returns Markdown source and safe HTML with unchanged evidence', async () => {
    const answer = '## Fixture\n\n| Value |\n| --- |\n| 1.0552 |\n\n<script>bad()</script>';
    const inference = createServer((req, res) => {
        req.resume();
        res.setHeader('content-type', 'application/json');
        res.end(JSON.stringify({ choices: [{ message: { role: 'assistant', content: answer } }] }));
    });
    inference.listen(0, '127.0.0.1'); await once(inference, 'listening');
    const backend = { ready: () => true, close: async () => {}, request: async op => op === 'login' ? {
        success: true, token: 'private-fixture-token', session_id: 'fixture-session', username: 'fixture',
        tenant_id: '11111111-1111-4111-8111-111111111111', selected_party_id: '22222222-2222-4222-8222-222222222222'
    } : {} };
    const app = createApp({ host: '127.0.0.1', port: 0, allowedOrigins: [], nats: null, inference: {
        baseUrl: `http://127.0.0.1:${inference.address().port}/v1`, model: 'fixture', timeoutMs: 1000
    } }, backend);
    app.listen(0, '127.0.0.1'); await once(app, 'listening');
    const url = `http://127.0.0.1:${app.address().port}`;
    try {
        const post = (path, data, token) => fetch(url + path, { method: 'POST', headers: { 'content-type': 'application/json', ...(token ? { authorization: `Bearer ${token}` } : {}) }, body: JSON.stringify(data) });
        assert.equal((await post('/chat', { message: 'Hello' })).status, 401);
        const { accessToken } = await (await post('/auth/login', { principal: 'fixture', password: 'fixture' })).json();
        const response = await post('/chat', { message: 'Hello' }, accessToken);
        assert.equal(response.status, 200);
        const result = await response.json();
        assert.equal(result.answer, answer);
        assert.equal(result.answerHtml, renderMarkdown(answer));
        assert.deepEqual(result.evidence, []);
        assert.doesNotMatch(result.answerHtml, /<script>/);
        assert.match(result.answerHtml, /<table>/);
        assert.equal((await post('/auth/logout', {}, accessToken)).status, 200);
    } finally {
        for (const server of [app, inference]) { server.closeAllConnections(); await new Promise(resolve => server.close(resolve)); }
    }
});
