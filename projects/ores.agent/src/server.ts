// SPDX-License-Identifier: GPL-3.0-or-later
import { createServer, type IncomingMessage, type ServerResponse } from 'node:http';
import { randomUUID } from 'node:crypto';
import type { Config } from './config.js';
import { AppError, type Backend } from './backend.js';
import { Sessions } from './auth.js';
import { ToolExecutor, tools } from './tools.js';
import { z } from 'zod';
import { createMcp } from './mcp.js';
import { Agent } from './agent.js';
import { HttpInference, withInferenceReasoning } from './inference.js';
import { serveUi } from './ui.js';
import { renderMarkdown } from './markdown.js';
import { version } from './version.js';
import { tracedBackend } from './telemetry.js';
import { NpvRuns } from './npv-runs.js';
import { BookingDrafts } from './booking-drafts.js';
import { TradeBookings } from './trade-bookings.js';
import { marketId } from './market.js';
import { PythonSandbox } from './python-sandbox.js';

export function json(res: ServerResponse, status: number, body: unknown): void {
    res.writeHead(status, {
        'content-type': 'application/json',
        'cache-control': 'no-store',
        'x-content-type-options': 'nosniff',
    });
    res.end(JSON.stringify(body));
}

const chatStreamType = 'application/x-ndjson';
function chatEvent(res: ServerResponse, event: object, final = false) {
    if (res.destroyed || res.writableEnded) return;
    if (!res.headersSent)
        res.writeHead(200, {
            'content-type': chatStreamType,
            'cache-control': 'no-store',
            'x-content-type-options': 'nosniff',
        });
    const line = JSON.stringify(event) + '\n';
    if (final) res.end(line);
    else res.write(line);
}

export async function body(req: IncomingMessage): Promise<unknown> {
    const chunks: Buffer[] = [];
    let size = 0;
    for await (const chunk of req) {
        size += chunk.length;
        if (size > 32768) throw new AppError('request_too_large', 'Request exceeds 32 KiB.', 413);
        chunks.push(Buffer.from(chunk));
    }
    return JSON.parse(Buffer.concat(chunks).toString('utf8'));
}

export function createApp(config: Config, backend?: Backend, submissionDirectory?: string) {
    if (backend) backend = tracedBackend(backend);
    const sessions = backend ? new Sessions(backend) : undefined;
    const python = config.pythonSandbox ? new PythonSandbox(config.pythonSandbox) : undefined;
    const executor = backend ? new ToolExecutor(backend, undefined, python) : undefined;
    const mcp = sessions && executor ? createMcp(sessions, executor) : undefined;
    const inference = new HttpInference(config.inference);
    const reportRuns =
        backend && submissionDirectory
            ? new NpvRuns(backend, submissionDirectory, executor)
            : undefined;
    const agent = executor
        ? new Agent(
              inference,
              executor,
              (event) => console.log(JSON.stringify(event)),
              reportRuns,
              new BookingDrafts(
                  inference,
                  executor,
                  backend ? new TradeBookings(backend) : undefined,
                  backend,
              ),
              Boolean(python),
          )
        : undefined;
    const server = createServer((req, res) => {
        void (async () => {
            const requestId = randomUUID();
            res.setHeader('x-request-id', requestId);
            if (req.headers.origin && !config.allowedOrigins.includes(req.headers.origin))
                return json(res, 403, { error: 'Origin rejected.' });
            const host = req.headers.host ?? '';
            if (!/^(127\.0\.0\.1|localhost|\[::1\])(:\d+)?$/.test(host))
                return json(res, 403, { error: 'Host rejected.' });
            if (req.method === 'GET' && req.url === '/') return serveUi(res);
            if (req.method === 'GET' && req.url === '/healthz')
                return json(res, 200, { status: 'ok', component: 'ores.agent', version });
            if (req.method === 'GET' && req.url === '/readyz')
                return json(res, backend?.ready() ? 200 : 503, {
                    status: backend?.ready() ? 'ready' : 'not_ready',
                    backend: backend?.ready() ?? false,
                });
            if (!sessions || !executor) return json(res, 503, { error: 'Backend not configured.' });
            if (req.method === 'POST' && req.url === '/auth/login')
                return json(res, 200, await sessions.login(await body(req)));
            if (req.method === 'POST' && req.url === '/auth/logout') {
                await sessions.logout(req.headers.authorization);
                return json(res, 200, { signedOut: true });
            }
            const session = sessions.require(req.headers.authorization);
            if (req.url === '/mcp' && mcp) return await mcp.handle(req, res);
            if (req.method === 'POST' && req.url === '/chat/reset' && agent) {
                agent.resetChat(session);
                return json(res, 200, { reset: true });
            }
            if (req.method === 'POST' && req.url === '/chat/report-progress' && reportRuns) {
                const { runId } = z
                    .object({ runId: marketId })
                    .strict()
                    .parse(await body(req));
                const abort = new AbortController();
                res.on('close', () => {
                    if (!res.writableFinished) abort.abort();
                });
                const progress = await reportRuns.progress(
                    runId.toLowerCase(),
                    session,
                    abort.signal,
                );
                return json(res, 200, { ...progress, answerHtml: renderMarkdown(progress.answer) });
            }
            if (req.method === 'POST' && req.url === '/chat' && agent) {
                const abort = new AbortController();
                res.on('close', () => {
                    if (!res.writableFinished) abort.abort();
                });
                const input = await body(req);
                const run = () => agent.chat(input, session, abort.signal, requestId);
                const stream = req.headers.accept
                    ?.split(',')
                    .some((type) => type.trim() === chatStreamType);
                let remaining = 16000;
                const result = stream
                    ? await withInferenceReasoning((text) => {
                          const chunk = text.slice(0, remaining);
                          remaining -= chunk.length;
                          if (chunk) chatEvent(res, { type: 'reasoning', text: chunk });
                      }, run)
                    : await run();
                const response = { ...result, answerHtml: renderMarkdown(result.answer) };
                if (stream) return chatEvent(res, { type: 'result', result: response }, true);
                return json(res, 200, response);
            }
            if (req.method === 'GET' && req.url === '/tools')
                return json(
                    res,
                    200,
                    Object.entries(tools).map(([name, t]) => ({
                        name,
                        description: t.description,
                        inputSchema: z.toJSONSchema(t.schema, { io: 'input' }),
                    })),
                );
            if (req.method === 'POST' && req.url?.startsWith('/tools/')) {
                const name = req.url.slice(7);
                if (!Object.hasOwn(tools, name))
                    throw new AppError('unknown_tool', 'Tool is not available.', 400);
                const abort = new AbortController();
                res.on('close', () => {
                    if (!res.writableFinished) abort.abort();
                });
                return json(
                    res,
                    200,
                    await executor.execute(name, await body(req), session, abort.signal),
                );
            }
            return json(res, 404, { error: 'Not found.', requestId });
        })().catch((error) => {
            if (res.destroyed || res.writableEnded) return;
            const safe =
                error instanceof AppError
                    ? error
                    : error instanceof z.ZodError || error instanceof SyntaxError
                      ? new AppError(
                            'invalid_request',
                            'Request or response did not match the contract.',
                            400,
                        )
                      : new AppError('internal_error', 'Request failed.', 500);
            if (res.headersSent) {
                if (res.getHeader('content-type') === chatStreamType)
                    chatEvent(
                        res,
                        { type: 'error', error: safe.message, status: safe.status },
                        true,
                    );
                else res.end();
            } else json(res, safe.status, { error: safe.message, code: safe.code });
        });
    });
    server.requestTimeout = 65000;
    server.headersTimeout = 10000;
    let cleanup: Promise<void> | undefined;
    const closeResources = () =>
        (cleanup ??= (async () => {
            const results = await Promise.allSettled([sessions?.close(), mcp?.close()]);
            for (const result of results) if (result.status === 'rejected') throw result.reason;
        })());
    // Ordinary close callers still release resources. Shutdown callers await completion.
    server.on('close', () => {
        void closeResources().catch(() => {});
    });
    let shutdown: Promise<void> | undefined;
    return Object.assign(server, {
        shutdown: () =>
            (shutdown ??= (async () => {
                try {
                    await new Promise<void>((resolve, reject) =>
                        server.close((error) => (error ? reject(error) : resolve())),
                    );
                } finally {
                    await closeResources();
                }
            })()),
    });
}
