import { dirname, resolve } from 'node:path';
import { fileURLToPath } from 'node:url';
import { describe, expect, it } from 'vitest';
import type { OresClient } from '@ores/wire-protocol';
import type { Config } from './config.js';
import { buildServer, sessionCookieName } from './server.js';
import { createSessionStore } from './sessions.js';
import { loadSiteConfiguration, SITE_CONFIG_VARIABLE } from './site-config.js';

/**
 * The blotter's read.
 *
 * The cases prove the two things the screen relies on and the wire cannot state
 * on its own: a structure stands in place of its legs, and it is placed where its
 * legs are — on the book they share, or on the lowest portfolio that holds them
 * all. The rail counts what sits where, so a structure counts once, on the node a
 * person opens it from.
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
const SESSION_COOKIE = sessionCookieName(ENVIRONMENT_ID);
const ok = { outcome: 'ok', code: '', message: '' };

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

const PARTY = '99999999-9999-4999-8999-999999999999';
const RATES = '11111111-1111-4111-8111-111111111111';
const CREDIT = '22222222-2222-4222-8222-222222222222';
const EQUITIES = '33333333-3333-4333-8333-333333333333';
const EMEA = '44444444-4444-4444-8444-444444444444';
const CREDIT_FLOW = '55555555-5555-4555-8555-555555555555';
const EQUITY_BOOK = '66666666-6666-4666-8666-666666666666';
const SPANNING = '77777777-7777-4777-8777-777777777777';
const SINGLE = '88888888-8888-4888-8888-888888888888';
const LEG_RATES = 'aaaaaaaa-aaaa-4aaa-8aaa-aaaaaaaaaaaa';
const LEG_CREDIT = 'bbbbbbbb-bbbb-4bbb-8bbb-bbbbbbbbbbbb';
const LEG_EQUITY = 'cccccccc-cccc-4ccc-8ccc-cccccccccccc';
const FREE_EMEA = 'dddddddd-dddd-4ddd-8ddd-dddddddddddd';
const FREE_EQUITY = 'eeeeeeee-eeee-4eee-8eee-eeeeeeeeeeee';
const ACME_BANK = 'f1f1f1f1-f1f1-4f1f-8f1f-f1f1f1f1f1f1';
const GLOBEX = 'f2f2f2f2-f2f2-4f2f-8f2f-f2f2f2f2f2f2';

const subjects = {
    books: 'refdata.v1.books.list',
    portfolios: 'refdata.v1.portfolios.list',
    counterparties: 'refdata.v1.counterparties.list',
    structures: 'trading.v1.structures.list',
    members: 'trading.v1.structure_members.list',
    identifiers: 'trading.v1.structure_identifiers.list',
    tradeIdentifiers: 'trading.v1.trade_identifiers.list',
    tradeTypes: 'trading.v1.trade_types.list',
    instrumentCodes: 'refdata.v1.instrument_codes.list',
    summaries: 'trading.v1.trade_summaries.list',
};

interface SummaryRow {
    readonly id: string;
    readonly row_kind: string;
    readonly book_id: string | null;
    readonly is_leg: boolean;
    readonly structure_id: string | null;
    readonly counterparty_id: string | null;
    readonly instrument_type: string;
    readonly status: string;
    readonly trade_date: string;
}

/**
 * The party's rows: a structure spanning two books under one portfolio, a
 * structure of one book, two free trades, and the legs behind the structures.
 */
function rows(): readonly SummaryRow[] {
    return [
        {
            id: SPANNING,
            row_kind: 'structure',
            book_id: null,
            is_leg: false,
            structure_id: null,
            counterparty_id: ACME_BANK,
            instrument_type: 'Package',
            status: 'Booked',
            trade_date: '2026-03-02',
        },
        {
            id: FREE_EMEA,
            row_kind: 'trade',
            book_id: EMEA,
            is_leg: false,
            structure_id: null,
            counterparty_id: ACME_BANK,
            instrument_type: 'Swap',
            status: 'Booked',
            trade_date: '2026-03-03',
        },
        {
            id: SINGLE,
            row_kind: 'structure',
            book_id: null,
            is_leg: false,
            structure_id: null,
            counterparty_id: GLOBEX,
            instrument_type: 'Package',
            status: 'Booked',
            trade_date: '2026-03-04',
        },
        {
            id: FREE_EQUITY,
            row_kind: 'trade',
            book_id: EQUITY_BOOK,
            is_leg: false,
            structure_id: null,
            counterparty_id: GLOBEX,
            instrument_type: 'Swap',
            status: 'Booked',
            trade_date: '2026-03-05',
        },
        {
            id: LEG_RATES,
            row_kind: 'trade',
            book_id: EMEA,
            is_leg: true,
            structure_id: SPANNING,
            counterparty_id: ACME_BANK,
            instrument_type: 'Swap',
            status: 'Booked',
            trade_date: '2026-03-06',
        },
        {
            id: LEG_CREDIT,
            row_kind: 'trade',
            book_id: CREDIT_FLOW,
            is_leg: true,
            structure_id: SPANNING,
            counterparty_id: ACME_BANK,
            instrument_type: 'Swap',
            status: 'Booked',
            trade_date: '2026-03-07',
        },
        {
            id: LEG_EQUITY,
            row_kind: 'trade',
            book_id: EQUITY_BOOK,
            is_leg: true,
            structure_id: SINGLE,
            counterparty_id: GLOBEX,
            instrument_type: 'Swap',
            status: 'Booked',
            trade_date: '2026-03-08',
        },
    ];
}

function replies(): Readonly<Record<string, unknown>> {
    return {
        [subjects.books]: {
            result: ok,
            books: [
                { id: EMEA, name: 'EMEA Rates', parent_portfolio_id: RATES },
                { id: CREDIT_FLOW, name: 'Credit Flow', parent_portfolio_id: CREDIT },
                { id: EQUITY_BOOK, name: 'Equity Derivatives', parent_portfolio_id: EQUITIES },
            ],
            total: 3,
        },
        [subjects.portfolios]: {
            result: ok,
            portfolios: [
                { id: RATES, name: 'Rates', parent_portfolio_id: null },
                { id: CREDIT, name: 'Credit', parent_portfolio_id: RATES },
                { id: EQUITIES, name: 'Equities', parent_portfolio_id: null },
            ],
            total: 3,
        },
        [subjects.counterparties]: {
            result: ok,
            counterparties: [
                { id: ACME_BANK, full_name: 'Acme Bank', short_code: 'ACME' },
                { id: GLOBEX, full_name: 'Globex', short_code: 'GLBX' },
            ],
            total: 2,
        },
        [subjects.structures]: {
            result: ok,
            structures: [
                { id: SPANNING, kind: 'Package' },
                { id: SINGLE, kind: 'Package' },
            ],
            total: 2,
        },
        [subjects.members]: {
            result: ok,
            members: [
                { trade_id: LEG_RATES, structure_id: SPANNING },
                { trade_id: LEG_CREDIT, structure_id: SPANNING },
                { trade_id: LEG_EQUITY, structure_id: SINGLE },
            ],
            total: 3,
        },
        [subjects.identifiers]: {
            result: ok,
            identifiers: [
                { structure_id: SPANNING, id_value: 'PKG-0001' },
                { structure_id: SINGLE, id_value: 'PKG-0002' },
            ],
            total: 2,
        },
        [subjects.tradeIdentifiers]: {
            result: ok,
            identifiers: [
                { trade_id: FREE_EMEA, id_type: 'Internal', id_value: 'IRS-0001' },
                { trade_id: LEG_RATES, id_type: 'Internal', id_value: 'IRS-0002' },
            ],
            total: 2,
        },
        [subjects.tradeTypes]: {
            result: ok,
            types: [{ code: 'Swap', description: 'Interest Rate Swap', product_type: 'swap' }],
            total: 1,
        },
        [subjects.instrumentCodes]: {
            result: ok,
            instruments: [
                {
                    code: 'IRS',
                    name: 'Interest Rate Swap',
                    ore_trade_type: 'Swap',
                    asset_class: 'interest_rates',
                },
            ],
            total: 1,
        },
        [subjects.summaries]: { result: ok, trade_summaries: rows(), total: rows().length },
    };
}

interface Asked {
    readonly filter?: {
        readonly is_leg?: boolean | null;
        readonly row_kind?: string | null;
        readonly id_one_of?: readonly string[] | null;
        readonly search?: string | null;
    };
}

/** The canned summaries, narrowed the way the service narrows them. */
function summariesFor(asked: Asked): readonly SummaryRow[] {
    const filter = asked.filter;
    return rows()
        .filter((row) => filter?.is_leg == null || row.is_leg === filter.is_leg)
        .filter((row) => filter?.row_kind == null || row.row_kind === filter.row_kind)
        .filter(
            (row) =>
                filter?.id_one_of == null ||
                filter.id_one_of.length === 0 ||
                filter.id_one_of.includes(row.id),
        );
}

function buildTestServer(): {
    readonly server: ReturnType<typeof buildServer>;
    readonly sessionId: string;
} {
    const canned = replies();
    const sessions = createSessionStore({ ttlSeconds: 60 });
    const client = {
        async callAuthenticated(
            subject: string,
            body: unknown,
            schema: { parse: (value: unknown) => unknown },
        ): Promise<unknown> {
            if (!(subject in canned)) {
                throw new Error(`No canned reply for ${subject}`);
            }
            if (subject === subjects.summaries) {
                const found = summariesFor(body as Asked);
                return schema.parse({ result: ok, trade_summaries: found, total: found.length });
            }
            return schema.parse(canned[subject]);
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
        sessionId: '33333333-3333-3333-3333-333333333333',
    });
    const party = {
        id: PARTY,
        name: 'Acme UK',
        partyCategory: 'Operational',
        businessCenterCode: 'GBLO',
    };
    sessions.switchParty(session.id, party, [party], 1800);
    return {
        server: buildServer({
            config,
            site: siteConfiguration(),
            sessions,
            createClient: () => ({ client, connect: async () => undefined }),
        }),
        sessionId: session.id,
    };
}

function get(
    server: ReturnType<typeof buildServer>,
    sessionId: string,
    url: string,
): ReturnType<ReturnType<typeof buildServer>['inject']> {
    return server.inject({ method: 'GET', url, cookies: { [SESSION_COOKIE]: sessionId } });
}

describe('the blotter read', () => {
    it('shows each structure in place of its legs', async () => {
        const { server, sessionId } = buildTestServer();
        const answer = await get(server, sessionId, '/api/blotter');
        expect(answer.statusCode).toBe(200);
        const body = answer.json();
        expect(body.rows.map((row: { id: string }) => row.id)).toEqual([
            SPANNING,
            FREE_EMEA,
            SINGLE,
            FREE_EQUITY,
        ]);
        expect(body.rows[0]).toMatchObject({
            row_kind: 'structure',
            reference: 'PKG-0001',
            spans_books: true,
        });
        expect(body.rows[2]).toMatchObject({ reference: 'PKG-0002', spans_books: false });
        // A structure has no book, date or state of its own: they are its legs'.
        expect(body.rows[0]).toMatchObject({
            book_name: 'EMEA Rates + Credit Flow',
            trade_date: '2026-03-06',
            status: 'Booked',
        });
        expect(body.rows[2]).toMatchObject({
            book_name: 'Equity Derivatives',
            trade_date: '2026-03-08',
        });
        // The instrument is the name the catalogue holds, not the code stored.
        expect(body.rows[1]).toMatchObject({
            instrument_type: 'Swap',
            instrument_type_name: 'Interest Rate Swap',
            product_code: 'IRS',
            product_name: 'Interest Rate Swap',
        });
    });

    it('counts each row once, where it is placed', async () => {
        const { server, sessionId } = buildTestServer();
        const body = (await get(server, sessionId, '/api/blotter')).json();
        const counts = Object.fromEntries(
            (body.tree as { id: string; count: number }[]).map((node) => [node.id, node.count]),
        );
        // The party holds every top row: two structures and two free trades.
        expect(counts['']).toBe(4);
        // Rates holds the spanning structure and the EMEA trade. Its Credit child
        // holds nothing, so the rail does not draw it: a node nobody can open
        // answers no question.
        expect(counts[`portfolio:${RATES}`]).toBe(2);
        expect(counts[`portfolio:${CREDIT}`]).toBeUndefined();
        // A book holds the rows placed on it, and a leg is not one of them.
        expect(counts[`book:${EMEA}`]).toBe(1);
        expect(counts[`book:${EQUITY_BOOK}`]).toBe(2);
    });

    it('does not show a spanning structure under either book, but under the portfolio holding them', async () => {
        const { server, sessionId } = buildTestServer();
        const onBook = (await get(server, sessionId, `/api/blotter?node=book:${EMEA}`)).json();
        expect(onBook.rows.map((row: { id: string }) => row.id)).toEqual([FREE_EMEA]);
        const onRates = (
            await get(server, sessionId, `/api/blotter?node=portfolio:${RATES}`)
        ).json();
        expect(onRates.rows.map((row: { id: string }) => row.id)).toEqual([SPANNING, FREE_EMEA]);
    });

    it('reads the legs in the flat list, each naming its structure', async () => {
        const { server, sessionId } = buildTestServer();
        const body = (await get(server, sessionId, '/api/blotter?flat=1')).json();
        expect(body.rows.map((row: { id: string }) => row.id)).toEqual([
            FREE_EMEA,
            FREE_EQUITY,
            LEG_RATES,
            LEG_CREDIT,
            LEG_EQUITY,
        ]);
        expect(body.rows[2]).toMatchObject({
            structure: SPANNING,
            structure_reference: 'PKG-0001',
        });
        expect(body.rows[0]).toMatchObject({ structure: '' });
    });

    it('draws the instrument rail from the classes and narrows to one', async () => {
        const { server, sessionId } = buildTestServer();
        const body = (await get(server, sessionId, '/api/blotter?view=instrument')).json();
        const rail = (body.tree as { id: string; label: string; count: number }[]).filter(
            (node) => node.id !== '',
        );
        // The fixture's structures are packages, which are no class at all, and its
        // trades are swaps: one class, and the party states the rows inside one.
        expect(rail.map((node) => [node.label, node.count])).toEqual([['Rates', 2]]);
        const narrowed = (
            await get(
                server,
                sessionId,
                '/api/blotter?view=instrument&node=instrument:interest_rates',
            )
        ).json();
        expect(narrowed.rows.map((row: { id: string }) => row.id)).toEqual([
            FREE_EMEA,
            FREE_EQUITY,
        ]);
    });

    it('draws the counterparty rail from the rows and narrows to one', async () => {
        const { server, sessionId } = buildTestServer();
        const body = (await get(server, sessionId, '/api/blotter?view=counterparty')).json();
        const rail = (body.tree as { id: string; label: string; count: number }[]).filter(
            (node) => node.id !== '',
        );
        expect(rail.map((node) => [node.label, node.count])).toEqual([
            ['Acme Bank', 2],
            ['Globex', 2],
        ]);
        const narrowed = (
            await get(
                server,
                sessionId,
                `/api/blotter?view=counterparty&node=counterparty:${ACME_BANK}`,
            )
        ).json();
        expect(narrowed.rows.map((row: { id: string }) => row.id)).toEqual([SPANNING, FREE_EMEA]);
    });
});
