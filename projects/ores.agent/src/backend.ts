// SPDX-License-Identifier: GPL-3.0-or-later
// Deliberately independent of the moving web/BFF source tree.
import { connect, headers, type NatsConnection } from 'nats';
import { encode, decode } from '@msgpack/msgpack';
import { readFile } from 'node:fs/promises';
import { gunzipSync } from 'node:zlib';
import { randomUUID } from 'node:crypto';
import type { Config } from './config.js';

export const backendRevision = 'fc4f559dfb187a368bf2a8cd4053d8e833348835';
export const subjects = {
    login: 'iam.v1.ops.login', logout: 'iam.v1.ops.logout', selectParty: 'iam.v1.ops.select_party',
    currencies: 'refdata.v1.currencies.list', series: 'marketdata.v1.market_series.list',
    observations: 'marketdata.v1.market_observations.list_by_series_id',
    trades: 'trading.v1.trades.list', books: 'refdata.v1.books.list', portfolios: 'refdata.v1.portfolios.list',
    reportDefinitions: 'reporting.v1.report_definitions.list', reportInstances: 'reporting.v1.report_instances.list',
    runResults: 'reporting.v1.run_results.list', runHealth: 'reporting.v1.ops.get_run_health',
    reportMetrics: 'reporting.v1.report_metrics.list',
    counterparties: 'refdata.v1.counterparties.list', trade: 'trading.v1.trades.get',
    tradeBooking: 'trading.v1.trade_bookings.get', tradePortfolios: 'trading.v1.trade_portfolios.list_by_trade_id',
    tradeBookings: 'trading.v1.trade_bookings.list',
    exportPortfolio: 'trading.v1.ops.export_portfolio', fxForward: 'trading.v1.fx_forward_instruments.get', bond: 'trading.v1.bond_instruments.get',
    counterparty: 'refdata.v1.counterparties.get',
} as const;
export type Operation = keyof typeof subjects;
export interface Identity { token: string; sessionId: string }
export interface Backend {
    ready(): boolean;
    request(operation: Operation, payload: unknown, identity?: Identity, signal?: AbortSignal): Promise<unknown>;
    close(): Promise<void>;
}
export class AppError extends Error {
    constructor(public readonly code: string, message: string, public readonly status = 502) { super(message); }
}
export function decodeReply(data: Uint8Array, encoding?: string): unknown {
    if (data.length > 2_000_000) throw new AppError('oversized_reply', 'Backend reply exceeds limit.');
    const bytes = encoding === 'gzip' ? gunzipSync(data, { maxOutputLength: 2_000_000 }) : data;
    try { return decode(bytes, { maxArrayLength: 10000, maxMapLength: 1000, maxStrLength: 1_000_000 }); }
    catch { throw new AppError('invalid_reply', 'Backend returned an invalid reply.'); }
}
export class NatsBackend implements Backend {
    private connected = true;
    private constructor(private nc: NatsConnection, private prefix: string) {
        void (async () => { for await (const status of nc.status()) {
            if (status.type === 'disconnect') this.connected = false;
            if (status.type === 'reconnect') this.connected = true;
        } })().catch(() => { this.connected = false; });
    }
    static async open(config: NonNullable<Config['nats']>): Promise<NatsBackend> {
        const [ca, cert, key] = await Promise.all([config.ca, config.cert, config.key].map(p => readFile(p, 'utf8')));
        const nc = await connect({ servers: config.server, name: 'ores.agent', tls: { ca, cert, key },
            timeout: 5000, reconnect: true, maxReconnectAttempts: -1, reconnectTimeWait: 1000 });
        return new NatsBackend(nc, config.prefix);
    }
    ready() { return this.connected && !this.nc.isClosed() && !this.nc.isDraining(); }
    async request(operation: Operation, payload: unknown, identity?: Identity, signal?: AbortSignal): Promise<unknown> {
        signal?.throwIfAborted();
        if (!Object.hasOwn(subjects, operation)) throw new AppError('invalid_operation', 'Operation is not allowed.', 400);
        const h = headers();
        h.set('Nats-Correlation-Id', randomUUID());
        if (identity) { h.set('Authorization', `Bearer ${identity.token}`); h.set('Nats-Session-Id', identity.sessionId); }
        try {
            // Core NATS read request: finite deadline, no mutation retries, no subscriptions.
            const reply = await this.nc.request(`${this.prefix}.${subjects[operation]}`, encode(payload, { ignoreUndefined: true }), { headers: h, timeout: 8000 });
            signal?.throwIfAborted();
            const error = reply.headers?.get('X-Error');
            if (error) throw new AppError(error === 'token_expired' ? 'session_expired' : 'backend_denied', 'Backend rejected the request.', 403);
            return decodeReply(reply.data, reply.headers?.get('X-Content-Encoding'));
        } catch (error) {
            if (error instanceof AppError || signal?.aborted) throw error;
            throw new AppError('backend_unavailable', 'Backend did not answer within its deadline.', 503);
        }
    }
    async close() { await this.nc.drain().catch(() => this.nc.close()); }
}
