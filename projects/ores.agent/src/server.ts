import { createServer, type IncomingMessage, type ServerResponse } from 'node:http';
import { randomUUID } from 'node:crypto';
import type { Config } from './config.js';
import { AppError, type Backend } from './backend.js';
import { Sessions } from './auth.js';
import { ToolExecutor, tools } from './tools.js';
import { z } from 'zod';
import { createMcp } from './mcp.js';

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

export function createApp(config: Config, backend?: Backend) {
    const sessions = backend ? new Sessions(backend) : undefined;
    const executor = backend ? new ToolExecutor(backend) : undefined;
    const mcp = sessions && executor ? createMcp(sessions, executor) : undefined;
    const server = createServer((req, res) => { void (async () => {
        const requestId = randomUUID();
        res.setHeader('x-request-id', requestId);
        if (req.headers.origin && !config.allowedOrigins.includes(req.headers.origin)) return json(res, 403, { error: 'Origin rejected.' });
        const host = req.headers.host ?? '';
        if (!/^(127\.0\.0\.1|localhost|\[::1\])(:\d+)?$/.test(host)) return json(res, 403, { error: 'Host rejected.' });
        if (req.method === 'GET' && req.url === '/healthz') return json(res, 200, { status: 'ok', component: 'ores.agent', version: '0.1.0' });
        if (req.method === 'GET' && req.url === '/readyz') return json(res, backend?.ready() ? 200 : 503, { status: backend?.ready() ? 'ready' : 'not_ready', backend: backend?.ready() ?? false });
        if (!sessions || !executor) return json(res, 503, { error: 'Backend not configured.' });
        if (req.method === 'POST' && req.url === '/auth/login') return json(res, 200, await sessions.login(await body(req)));
        if (req.method === 'POST' && req.url === '/auth/logout') { await sessions.logout(req.headers.authorization); return json(res, 200, { signedOut: true }); }
        const session = sessions.require(req.headers.authorization);
        if (req.url === '/mcp' && mcp) return await mcp.handle(req, res);
        if (req.method === 'GET' && req.url === '/tools') return json(res, 200, Object.entries(tools).map(([name, t]) => ({ name, description: t.description, inputSchema: z.toJSONSchema(t.schema) })));
        if (req.method === 'POST' && req.url?.startsWith('/tools/')) {
            const abort = new AbortController();
            res.on('close', () => { if (!res.writableFinished) abort.abort(); });
            return json(res, 200, await executor.execute(req.url.slice(7), await body(req), session, abort.signal));
        }
        return json(res, 404, { error: 'Not found.', requestId });
    })().catch(error => {
        if (res.destroyed || res.writableEnded) return;
        const safe = error instanceof AppError ? error : error instanceof z.ZodError || error instanceof SyntaxError ? new AppError('invalid_request', 'Request or response did not match the contract.', 400) : new AppError('internal_error', 'Request failed.', 500);
        json(res, safe.status, { error: safe.message, code: safe.code });
    });
    });
    server.requestTimeout = 65000;
    server.headersTimeout = 10000;
    server.on('close', () => { void sessions?.close(); void mcp?.close(); });
    return server;
}
