import { createServer, type IncomingMessage, type ServerResponse } from 'node:http';
import { randomUUID } from 'node:crypto';
import type { Config } from './config.js';

export function json(res: ServerResponse, status: number, body: unknown): void {
    res.writeHead(status, { 'content-type': 'application/json', 'cache-control': 'no-store', 'x-content-type-options': 'nosniff' });
    res.end(JSON.stringify(body));
}

export async function body(req: IncomingMessage): Promise<unknown> {
    let data = '';
    for await (const chunk of req) {
        data += chunk;
        if (Buffer.byteLength(data) > 32768) throw new Error('Request too large.');
    }
    return JSON.parse(data);
}

export function createApp(config: Config) {
    const server = createServer((req, res) => {
        const requestId = randomUUID();
        res.setHeader('x-request-id', requestId);
        if (req.headers.origin && !config.allowedOrigins.includes(req.headers.origin)) return json(res, 403, { error: 'Origin rejected.' });
        const host = req.headers.host ?? '';
        if (!/^(127\.0\.0\.1|localhost|\[::1\])(:\d+)?$/.test(host)) return json(res, 403, { error: 'Host rejected.' });
        if (req.method === 'GET' && req.url === '/healthz') return json(res, 200, { status: 'ok', component: 'ores.agent', version: '0.1.0' });
        if (req.method === 'GET' && req.url === '/readyz') return json(res, 503, { status: 'not_ready', reason: 'Backend not connected.' });
        return json(res, 404, { error: 'Not found.', requestId });
    });
    server.requestTimeout = 65000;
    server.headersTimeout = 10000;
    return server;
}
