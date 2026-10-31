import { dirname, resolve } from 'node:path';
import { fileURLToPath } from 'node:url';
import { describe, expect, it } from 'vitest';
import type { OresClient } from '@ores/wire-protocol';
import { INBOX_SUBJECTS } from '@ores/wire-protocol';
import { subjects as authorisationSubjects } from '@ores/wire-protocol/generated/trading/protocol/trade_authorisation_request_protocol';
import { subjects as bookSubjects } from '@ores/wire-protocol/generated/refdata/protocol/book_protocol';
import { subjects as counterpartySubjects } from '@ores/wire-protocol/generated/refdata/protocol/counterparty_protocol';
import { subjects as identifierSubjects } from '@ores/wire-protocol/generated/trading/protocol/trade_identifier_protocol';
import { subjects as summarySubjects } from '@ores/wire-protocol/generated/trading/protocol/trade_summary_protocol';
import { subjects as tradeSubjects } from '@ores/wire-protocol/generated/trading/protocol/trade_protocol';
import type { Config } from './config.js';
import { buildServer, sessionCookieName } from './server.js';
import { createSessionStore } from './sessions.js';
import { loadSiteConfiguration, SITE_CONFIG_VARIABLE } from './site-config.js';

/**
 * The Operations queue's read.
 *
 * The cases prove the three things the screen relies on and the inbox cannot
 * state on its own: the deals are named from the trading component, the queue is
 * divided by the complexity of the deal rather than by its asset class, and a
 * deal that moved after its request was raised says so instead of waiting
 * silently.
 */

const config: Config = {
    port: 0,
    host: '127.0.0.1',
    logLevel: 'silent',
    session: { ttlSeconds: 3600, cookieSecure: false },
    allowedOrigins: [],
    loginAttemptsPerMinute: 100,
};

const ENVIRONMENT_ID = 'eager_maxwell';
const ok = { outcome: 'ok', code: '', message: '' };

const RATES_BOOK = '11111111-1111-4111-8111-111111111111';
const EQUITY_BOOK = '22222222-2222-4222-8222-222222222222';
const BANK = '33333333-3333-4333-8333-333333333333';
const FUND = '44444444-4444-4444-8444-444444444444';
const SWAP = 'aaaaaaaa-aaaa-4aaa-8aaa-aaaaaaaaaaaa';
const PACKAGE = 'bbbbbbbb-bbbb-4bbb-8bbb-bbbbbbbbbbbb';
const REQUEST_ONE = 'cccccccc-cccc-4ccc-8ccc-cccccccccccc';
const REQUEST_TWO = 'dddddddd-dddd-4ddd-8ddd-dddddddddddd';
const REQUEST_ROLES = 'eeeeeeee-eeee-4eee-8eee-eeeeeeeeeeee';

function siteConfiguration(): ReturnType<typeof loadSiteConfiguration> {
    const path = resolve(
        dirname(fileURLToPath(import.meta.url)),
        '../../../config/environments.json',
    );
    return loadSiteConfiguration({
        environment: { [SITE_CONFIG_VARIABLE]: path },
        environmentId: ENVIRONMENT_ID,
    });
}

/**
 * Two authorisation requests and one of another kind, which the queue must not
 * draw: the queue is the trade queue, not every queue in the installation.
 */
function requests(): readonly Record<string, unknown>[] {
    return [
        {
            id: REQUEST_ONE,
            version: 1,
            kind_code: 'trading.operational_authorisation',
            state_code: 'waiting',
            requested_by: 'account-one',
            requested_at: '2026-10-31 09:00:00Z',
            reason: 'A live trade awaits operational authorisation.',
            expires_at: '2026-11-30 09:00:00Z',
        },
        {
            id: REQUEST_TWO,
            version: 1,
            kind_code: 'trading.operational_authorisation',
            state_code: 'waiting',
            requested_by: 'account-one',
            requested_at: '2026-10-31 10:00:00Z',
            reason: 'A live trade awaits operational authorisation.',
            expires_at: '2026-11-30 10:00:00Z',
        },
        {
            id: REQUEST_ROLES,
            version: 1,
            kind_code: 'iam.role_grant',
            state_code: 'waiting',
            requested_by: 'account-one',
            requested_at: '2026-10-31 11:00:00Z',
            reason: 'Please grant the trading role.',
            expires_at: '2026-11-30 11:00:00Z',
        },
    ];
}

function replies(): Readonly<Record<string, unknown>> {
    return {
        [INBOX_SUBJECTS.queue]: {
            result: ok,
            requests: requests(),
            total: 3,
            answered: [],
        },
        [authorisationSubjects.list_trade_authorisation_requests_request]: {
            result: ok,
            trade_authorisation_requests: [
                { request_id: REQUEST_ONE, trade_id: SWAP, trade_version: 2 },
                { request_id: REQUEST_TWO, trade_id: PACKAGE, trade_version: 1 },
            ],
            total: 2,
        },
        [identifierSubjects.list_trade_identifiers_request]: {
            result: ok,
            identifiers: [
                { trade_id: SWAP, id_value: 'POP-1A2B-000001' },
                { trade_id: PACKAGE, id_value: 'PKG-1A2B-000002' },
            ],
            total: 2,
        },
        [tradeSubjects.list_trades_request]: {
            result: ok,
            trades: [
                // The swap has moved on since its request: version 3 against 2.
                { id: SWAP, trade_type: 'Swap', version: 3 },
                { id: PACKAGE, trade_type: 'Package', version: 1 },
            ],
            total: 2,
        },
        [summarySubjects.list_trade_summaries_request]: {
            result: ok,
            trade_summaries: [
                {
                    id: SWAP,
                    book_id: RATES_BOOK,
                    counterparty_id: BANK,
                    trade_date: '2026-10-31',
                    status: 'live',
                },
                {
                    id: PACKAGE,
                    book_id: EQUITY_BOOK,
                    counterparty_id: FUND,
                    trade_date: '2026-10-30',
                    status: 'live',
                },
            ],
            total: 2,
        },
        [bookSubjects.list_books_request]: {
            result: ok,
            books: [
                { id: RATES_BOOK, name: 'GBP Vanilla Swaps' },
                { id: EQUITY_BOOK, name: 'GBP Equity Derivatives' },
            ],
            total: 2,
        },
        [counterpartySubjects.list_counterparties_request]: {
            result: ok,
            counterparties: [
                { id: BANK, full_name: 'Deutsche Bank AG' },
                { id: FUND, full_name: 'A Pension Fund' },
            ],
            total: 2,
        },
    };
}

function buildTestServer(): {
    readonly server: ReturnType<typeof buildServer>;
    readonly sessionId: string;
} {
    const canned = replies();
    const sessions = createSessionStore({ ttlSeconds: 60 });
    /*
     * A subject the case does not state answers with the envelope alone: the
     * schemas default every list to empty, so a read this route makes only for
     * names costs nothing here and does not have to be restated each case.
     */
    const client = {
        async callAuthenticated(
            subject: string,
            _body: unknown,
            schema: { parse: (value: unknown) => unknown },
        ): Promise<unknown> {
            return schema.parse(subject in canned ? canned[subject] : { result: ok });
        },
        async close(): Promise<void> {
            return undefined;
        },
    } as unknown as OresClient;
    const session = sessions.create({
        client,
        session: null,
        username: 'tenant_admin',
        email: 'tenant_admin@acme.example',
        accountId: '11111111-1111-1111-1111-111111111111',
        tenantId: '44444444-4444-4444-4444-444444444444',
        tenantName: 'Acme',
        mode: 'tenant-administration',
        version: 'v0.0.29 (test)',
        availableParties: [],
        accessLifetimeSeconds: 1800,
        passwordResetRequired: false,
        sessionId: '22222222-2222-2222-2222-222222222222',
    });
    const server = buildServer({
        config,
        site: siteConfiguration(),
        sessions,
        createClient: () => ({ client, connect: async () => undefined }),
    });
    return { server, sessionId: session.id };
}

async function get(
    server: ReturnType<typeof buildServer>,
    sessionId: string,
    url: string,
): Promise<{ statusCode: number; json: () => any }> {
    const response = await server.inject({
        method: 'GET',
        url,
        headers: { cookie: `${sessionCookieName(siteConfiguration().environment.id)}=${sessionId}` },
    });
    return { statusCode: response.statusCode, json: () => response.json() };
}

interface Row {
    readonly requestId: string;
    readonly reference: string;
    readonly classCode: string;
    readonly book: string;
    readonly counterparty: string;
    readonly tradeType: string;
    readonly tradeVersion: number;
    readonly requestTradeVersion: number;
    readonly changed: boolean;
    readonly stateCode: string;
}

describe('the Operations queue', () => {
    it('draws one row per authorisation request and drops another kind', async () => {
        const { server, sessionId } = buildTestServer();
        const body = (await get(server, sessionId, '/api/queue/authorisations')).json();
        const rows = body.items as Row[];
        expect(rows.map((row) => row.requestId)).toEqual([REQUEST_ONE, REQUEST_TWO]);
        expect(rows.map((row) => row.reference)).toEqual(['POP-1A2B-000001', 'PKG-1A2B-000002']);
    });

    it('names the book and the counterparty from the catalogues', async () => {
        const { server, sessionId } = buildTestServer();
        const rows = (
            (await get(server, sessionId, '/api/queue/authorisations')).json() as { items: Row[] }
        ).items;
        expect(rows.map((row) => [row.book, row.counterparty])).toEqual([
            ['GBP Vanilla Swaps', 'Deutsche Bank AG'],
            ['GBP Equity Derivatives', 'A Pension Fund'],
        ]);
    });

    it('divides the queue by how much work the deal is, not by its asset class', async () => {
        const { server, sessionId } = buildTestServer();
        const rows = (
            (await get(server, sessionId, '/api/queue/authorisations')).json() as { items: Row[] }
        ).items;
        // A plain swap is flow; a package is not, whichever class its legs are.
        expect(rows.map((row) => [row.tradeType, row.classCode])).toEqual([
            ['Swap', 'flow'],
            ['Package', 'non-flow'],
        ]);
    });

    it('says a deal moved after its request was raised, and still draws it', async () => {
        const { server, sessionId } = buildTestServer();
        const rows = (
            (await get(server, sessionId, '/api/queue/authorisations')).json() as { items: Row[] }
        ).items;
        expect(rows[0]).toMatchObject({
            requestTradeVersion: 2,
            tradeVersion: 3,
            changed: true,
            stateCode: 'waiting',
            requestVersion: 1,
        });
        // Unchanged deals are drawn the same way, changed is a fact and not a filter.
        expect(rows[1]).toMatchObject({ requestTradeVersion: 1, tradeVersion: 1, changed: false });
    });
});
