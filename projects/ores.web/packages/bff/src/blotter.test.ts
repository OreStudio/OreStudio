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
 * The rows are the trade summary's and the tree is the party's books. The two
 * things worth proving are the two the wire does not state: a structure's legs
 * are not rows of the top list, and a structure stands in their place. The flat
 * list inverts that and names each leg's structure. The tree counts a book by the
 * rows it holds.
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

const STRUCTURE = '77777777-7777-4777-8777-777777777777';
const LEG = '55555555-5555-4555-8555-555555555555';
const FREE_TRADE = '56565656-5656-4565-8656-565656565656';
const BOOK = '12121212-1212-4212-8212-121212121212';
const PORTFOLIO = '13131313-1313-4313-8313-131313131313';
const PARTY = '99999999-9999-4999-8999-999999999999';

const subjects = {
    books: 'refdata.v1.books.list',
    portfolios: 'refdata.v1.portfolios.list',
    structures: 'trading.v1.structures.list',
    members: 'trading.v1.structure_members.list',
    identifiers: 'trading.v1.structure_identifiers.list',
    summaries: 'trading.v1.trade_summaries.list',
};

function replies(): Readonly<Record<string, unknown>> {
    return {
        [subjects.books]: {
            result: ok,
            books: [{ id: BOOK, name: 'EMEA Rates', parent_portfolio_id: PORTFOLIO }],
            total: 1,
        },
        [subjects.portfolios]: {
            result: ok,
            portfolios: [{ id: PORTFOLIO, name: 'Rates and credit', parent_portfolio_id: null }],
            total: 1,
        },
        [subjects.structures]: {
            result: ok,
            structures: [{ id: STRUCTURE, kind: 'Package', counterparty_id: null }],
            total: 1,
        },
        [subjects.members]: {
            result: ok,
            members: [{ trade_id: LEG, structure_id: STRUCTURE, role: 'leg' }],
            total: 1,
        },
        [subjects.identifiers]: {
            result: ok,
            identifiers: [{ structure_id: STRUCTURE, id_value: 'PKG-0004' }],
            total: 1,
        },
        [subjects.summaries]: {
            result: ok,
            trade_summaries: [
                {
                    id: STRUCTURE,
                    row_kind: 'structure',
                    book_id: null,
                    is_leg: false,
                    structure_id: null,
                },
                {
                    id: LEG,
                    row_kind: 'trade',
                    book_id: BOOK,
                    is_leg: true,
                    structure_id: STRUCTURE,
                },
                {
                    id: FREE_TRADE,
                    row_kind: 'trade',
                    book_id: BOOK,
                    is_leg: false,
                    structure_id: null,
                },
            ],
            total: 3,
        },
    };
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
            _body: unknown,
            schema: { parse: (value: unknown) => unknown },
        ): Promise<unknown> {
            if (!(subject in canned)) {
                throw new Error(`No canned reply for ${subject}`);
            }
            const reply = canned[subject] as {
                trade_summaries?: { is_leg: boolean; row_kind: string }[];
            };
            const asked = (_body as { filter?: { is_leg?: boolean | null } }).filter?.is_leg;
            const kind = (_body as { filter?: { row_kind?: string | null } }).filter?.row_kind;
            if (
                subject === subjects.summaries &&
                (typeof asked === 'boolean' || typeof kind === 'string')
            ) {
                const rows = (reply.trade_summaries ?? []).filter(
                    (row) =>
                        (typeof asked !== 'boolean' || row.is_leg === asked) &&
                        (typeof kind !== 'string' || row.row_kind === kind),
                );
                return schema.parse({ ...reply, trade_summaries: rows, total: rows.length });
            }
            return schema.parse(reply);
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
    it('shows the structure in place of its leg, and counts the book it sits in', async () => {
        const { server, sessionId } = buildTestServer();
        const answer = await get(server, sessionId, '/api/blotter');
        expect(answer.statusCode).toBe(200);
        const body = answer.json();
        const kinds = body.rows.map((row: { row_kind: string }) => row.row_kind);
        const ids = body.rows.map((row: { id: string }) => row.id);
        // The structure stands for its leg: the leg is not a row of the top list.
        expect(kinds).toEqual(['structure', 'trade']);
        expect(ids).toContain(STRUCTURE);
        expect(ids).toContain(FREE_TRADE);
        expect(ids).not.toContain(LEG);
        // A structure row states its own reference and how many legs it holds.
        expect(body.rows[0]).toMatchObject({ reference: 'PKG-0004', legs: 1, spans_books: false });
        // The tree counts the book by the rows it holds, and the party by all of them.
        const tree = body.tree as { id: string; kind: string; count: number }[];
        expect(tree.find((node) => node.kind === 'book')?.count).toBe(3);
        expect(tree[0]).toMatchObject({ kind: 'party', count: 3 });
    });

    it('reads the legs in the flat list, each naming its structure', async () => {
        const { server, sessionId } = buildTestServer();
        const answer = await get(server, sessionId, '/api/blotter?flat=1');
        const body = answer.json();
        expect(body.rows.map((row: { id: string }) => row.id)).toEqual([LEG, FREE_TRADE]);
        expect(body.rows[0]).toMatchObject({
            structure: STRUCTURE,
            structure_reference: 'PKG-0004',
        });
        expect(body.rows[1]).toMatchObject({ structure: '' });
    });
});
