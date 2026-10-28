import { dirname, resolve } from 'node:path';
import { fileURLToPath } from 'node:url';
import { describe, expect, it } from 'vitest';
import type { OresClient } from '@ores/wire-protocol';
import type { Config } from './config.js';
import { buildServer, sessionCookieName } from './server.js';
import { createSessionStore } from './sessions.js';
import { loadSiteConfiguration, SITE_CONFIG_VARIABLE } from './site-config.js';

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

function buildTestServer(replies: Readonly<Record<string, unknown>>): {
    readonly server: ReturnType<typeof buildServer>;
    readonly sessionId: string;
    readonly calls: { subject: string; body: unknown }[];
} {
    const sessions = createSessionStore({ ttlSeconds: 60 });
    const calls: { subject: string; body: unknown }[] = [];
    const client = {
        async callAuthenticated(
            subject: string,
            body: unknown,
            schema: { parse: (value: unknown) => unknown },
        ): Promise<unknown> {
            calls.push({ subject, body });
            if (!(subject in replies)) {
                throw new Error(`No canned reply for ${subject}`);
            }
            const canned = replies[subject] as {
                members?: readonly { trade_id: string }[];
            };
            if (subject === 'trading.v1.structure_members.list' && canned.members !== undefined) {
                const asked = (
                    body as { filter?: { trade_id_one_of?: readonly string[] | null } }
                ).filter?.trade_id_one_of;
                const members =
                    asked === null || asked === undefined
                        ? canned.members
                        : canned.members.filter((entry) => asked.includes(entry.trade_id));
                return schema.parse({ ...canned, members });
            }
            return schema.parse(replies[subject]);
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
    return {
        server: buildServer({
            config,
            site: siteConfiguration(),
            sessions,
            createClient: () => ({ client, connect: async () => undefined }),
        }),
        sessionId: session.id,
        calls,
    };
}

const STRUCTURE = '66666666-6666-4666-8666-666666666666';
const OTHER_STRUCTURE = '12121212-1212-4212-8212-121212121212';
const LEG = '55555555-5555-4555-8555-555555555555';
const FREE_TRADE = '56565656-5656-4565-8656-565656565656';
const COUNTERPARTY = '88888888-8888-4888-8888-888888888888';
const PARTY = '99999999-9999-4999-8999-999999999999';

const lists = {
    structures: 'trading.v1.structures.list',
    identifiers: 'trading.v1.structure_identifiers.list',
    members: 'trading.v1.structure_members.list',
    putMember: 'trading.v1.structure_members.put',
    tradeIdentifiers: 'trading.v1.trade_identifiers.list',
    counterparties: 'refdata.v1.counterparties.list',
    summaries: 'trading.v1.trade_summaries.list',
    trades: 'trading.v1.trades.list',
    versions: 'trading.v1.structures_versions.list',
};

const structureRow = {
    id: STRUCTURE,
    label: 'PKG-83ffae10',
    kind: 'Package',
    template_code: '',
    counterparty_id: COUNTERPARTY,
};

function repliesWith(extra: Readonly<Record<string, unknown>> = {}) {
    return {
        [lists.structures]: { result: ok, structures: [structureRow] },
        [lists.identifiers]: {
            result: ok,
            identifiers: [{ structure_id: STRUCTURE, id_type: 'Internal', id_value: 'PKG-83ffae10' }],
        },
        [lists.members]: {
            result: ok,
            members: [
                { trade_id: LEG, structure_id: STRUCTURE, role: 'leg', sequence_number: 1 },
            ],
        },
        [lists.tradeIdentifiers]: {
            result: ok,
            identifiers: [{ trade_id: LEG, id_type: 'Internal', id_value: 'POP-1' }],
        },
        [lists.counterparties]: {
            result: ok,
            counterparties: [{ id: COUNTERPARTY, full_name: 'Banque Banorient France' }],
        },
        [lists.trades]: {
            result: ok,
            trades: [
                {
                    id: LEG,
                    party_id: PARTY,
                    trade_type: 'Swap',
                    external_version: 3,
                },
            ],
        },
        [lists.versions]: {
            result: ok,
            versions: [
                {
                    version: 1,
                    modified_by: 'diane.kay',
                    recorded_at: '2026-10-27 10:00:00+00',
                    change_reason_code: 'system.new_record',
                    change_commentary: 'Grouped by the population.',
                },
            ],
        },
        ...extra,
    };
}

function send(
    server: ReturnType<typeof buildServer>,
    sessionId: string,
    url: string,
    body?: unknown,
): ReturnType<ReturnType<typeof buildServer>['inject']> {
    return body === undefined
        ? server.inject({ method: 'GET', url, cookies: { [SESSION_COOKIE]: sessionId } })
        : server.inject({
              method: 'POST',
              url,
              cookies: { [SESSION_COOKIE]: sessionId },
              payload: body as Record<string, unknown>,
          });
}

describe('the structure page route', () => {
    it('reads a structure by the reference that names it and the trades that are its legs', async () => {
        const { server, sessionId } = buildTestServer(repliesWith());
        const answer = await send(server, sessionId, `/api/structures/${STRUCTURE}`);

        expect(answer.statusCode).toBe(200);
        const body = answer.json();
        expect(body.reference).toBe('PKG-83ffae10');
        expect(body.legs).toHaveLength(1);
        // The leg is named by the reference the desk reads, never by its uuid.
        expect(body.legs[0]).toMatchObject({
            reference: 'POP-1',
            role: 'leg',
            sequence_number: 1,
            // The product is the type's name, and the state and the notional are
            // the facts the page states beside it.
            kind: 'Swap',
            external_version: 3,
        });
        expect(body.history).toHaveLength(1);
        expect(body.structure.counterparty_name).toBe('Banque Banorient France');
        expect(body.roles).toEqual(['leg']);
    });

    it('answers 404 for a structure that is not there', async () => {
        const { server, sessionId } = buildTestServer(repliesWith({ [lists.structures]: { result: ok, structures: [] } }));
        const answer = await send(server, sessionId, `/api/structures/${STRUCTURE}`);
        expect(answer.statusCode).toBe(404);
    });
});

describe('adding a leg', () => {
    it('takes the next place in the structure and states the role it was given', async () => {
        const { server, sessionId, calls } = buildTestServer(
            repliesWith({
                [lists.putMember]: { result: ok, structure_member: { trade_id: FREE_TRADE } },
            }),
        );
        const answer = await send(server, sessionId, `/api/structures/${STRUCTURE}/legs`, {
            tradeId: FREE_TRADE,
            role: 'leg',
        });

        expect(answer.statusCode).toBe(201);
        expect(answer.json()).toMatchObject({ tradeId: FREE_TRADE, role: 'leg', sequenceNumber: 2 });
        const written = calls.find((call) => call.subject === lists.putMember);
        expect(written?.body).toMatchObject({
            change: {
                write: { trade_id: FREE_TRADE, structure_id: STRUCTURE, sequence_number: 2 },
            },
        });
    });

    it('refuses a trade another structure already holds, and says which', async () => {
        const { server, sessionId, calls } = buildTestServer(
            repliesWith({
                [lists.members]: {
                    result: ok,
                    members: [
                        {
                            trade_id: FREE_TRADE,
                            structure_id: OTHER_STRUCTURE,
                            role: 'leg',
                            sequence_number: 1,
                        },
                    ],
                },
            }),
        );
        const answer = await send(server, sessionId, `/api/structures/${STRUCTURE}/legs`, {
            tradeId: FREE_TRADE,
            role: 'leg',
        });

        expect(answer.statusCode).toBe(409);
        expect(answer.json().message).toBe('The trade is a leg of another structure.');
        // Nothing was written: the refusal is the answer, not a failed write.
        expect(calls.some((call) => call.subject === lists.putMember)).toBe(false);
    });
});

describe('the add-a-leg picker', () => {
    it('offers the trades no structure has grouped, and not the ones that are legs', async () => {
        const { server, sessionId } = buildTestServer(
            repliesWith({
                [lists.summaries]: {
                    result: ok,
                    trade_summaries: [
                        {
                            id: LEG,
                            row_kind: 'trade',
                            instrument_type: 'Swap',
                            trade_date: '2026-09-14',
                        },
                        {
                            id: FREE_TRADE,
                            row_kind: 'trade',
                            instrument_type: 'Bond',
                            trade_date: '2026-09-13',
                        },
                    ],
                },
            }),
        );
        const answer = await send(
            server,
            sessionId,
            `/api/structures/${STRUCTURE}/leg-candidates`,
        );

        expect(answer.statusCode).toBe(200);
        const rows = answer.json().rows as readonly { id: string }[];
        expect(rows.map((row) => row.id)).toEqual([FREE_TRADE]);
    });
});
