/**
 * Booking a trade: what the route states, and what it makes of the answer.
 *
 * The service owns the rules, so the case is about the two things this route
 * decides: the request it builds — every member stated, the party from the
 * session, the trade's identity made here — and the status a refusal becomes.
 */
import { dirname, resolve } from 'node:path';
import { fileURLToPath } from 'node:url';
import { describe, expect, it } from 'vitest';
import type { OresClient } from '@ores/wire-protocol';
import type { Config } from './config.js';
import { buildServer, sessionCookieName } from './server.js';
import { createSessionStore } from './sessions.js';
import { loadSiteConfiguration, SITE_CONFIG_VARIABLE } from './site-config.js';

const here = dirname(fileURLToPath(import.meta.url));
const config: Config = {
    host: '127.0.0.1',
    port: 0,
    natsUrl: 'nats://127.0.0.1:4222',
    subjectPrefix: '',
    sessionTtlSeconds: 3600,
    cookieSecure: false,
} as Config;

const ENVIRONMENT_ID = 'eager_maxwell';

function siteConfiguration(): ReturnType<typeof loadSiteConfiguration> {
    const path = resolve(here, '../../../config/environments.json');
    return loadSiteConfiguration({
        environment: { [SITE_CONFIG_VARIABLE]: path },
        environmentId: ENVIRONMENT_ID,
    });
}

const ok = { outcome: 'ok', code: '', message: 'ok' };
const PARTY = '99999999-9999-4999-8999-999999999999';
const BOOK = '77777777-7777-4777-8777-777777777777';
const COUNTERPARTY = '88888888-8888-4888-8888-888888888888';
const ISSUE = '66666666-6666-4666-8666-666666666666';
const ACTIVITY = '55555555-5555-4555-8555-555555555555';
const SESSION_COOKIE = sessionCookieName(ENVIRONMENT_ID);

function buildTestServer(
    replies: Readonly<Record<string, unknown>>,
    party: { id: string } | null = { id: PARTY },
): {
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
            return schema.parse(replies[subject]);
        },
        async close(): Promise<void> {
            return undefined;
        },
    } as unknown as OresClient;
    const partySummary =
        party === null
            ? undefined
            : {
                  id: party.id,
                  name: 'Acme UK',
                  partyCategory: 'Operational',
                  businessCenterCode: 'GBLO',
              };
    const session = sessions.create({
        client,
        session: null,
        username: 'diane.kay',
        email: 'diane.kay@acme.example',
        accountId: '11111111-1111-1111-1111-111111111111',
        tenantId: '44444444-4444-4444-4444-444444444444',
        tenantName: 'Acme',
        mode: 'tenant-administration',
        version: 'v0.0.29 (test)',
        availableParties: [],
        accessLifetimeSeconds: 1800,
        passwordResetRequired: false,
        sessionId: '33333333-3333-3333-3333-333333333333',
        availableParties: partySummary === undefined ? [] : [partySummary],
    });
    if (partySummary !== undefined) {
        sessions.switchParty(session.id, partySummary, [partySummary], 1800);
    }
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

const booking = {
    bookId: BOOK,
    counterpartyId: COUNTERPARTY,
    tradeDate: '2026-10-25',
    issueId: ISSUE,
    notional: '2500000.50',
    identifierType: 'Internal',
    identifierValue: 'DESK-1',
};

function book(
    server: ReturnType<typeof buildServer>,
    sessionId: string,
    body: unknown = booking,
): ReturnType<ReturnType<typeof buildServer>['inject']> {
    return server.inject({
        method: 'POST',
        url: '/api/trades',
        cookies: { [SESSION_COOKIE]: sessionId },
        payload: body as never,
    });
}

describe('POST /api/trades', () => {
    it('books the trade the form states and answers the trade it made', async () => {
        const { server, sessionId, calls } = buildTestServer({
            'trading.v1.ops.book_trade': { result: ok, activity_id: ACTIVITY },
            'trading.v1.trade_identifiers.put': { result: ok },
            'trading.v1.bond_instruments.put': { result: ok },
        });

        const reply = await book(server, sessionId);
        expect(reply.statusCode).toBe(201);
        expect(reply.json()).toEqual({ id: expect.stringMatching(/^[0-9a-f-]{36}$/) });
        const booked = calls[0]?.body as {
            anchor: Record<string, unknown>;
            booking: Record<string, unknown>;
            activity_type_code: string;
        };
        expect(booked.anchor['party_id']).toBe(PARTY);
        expect(booked.anchor['counterparty_id']).toBe(COUNTERPARTY);
        expect(booked.anchor['trade_type']).toBe('Bond');
        expect(booked.anchor['time']).toBeUndefined();
        expect(booked.booking['book_id']).toBe(BOOK);
        expect(booked.booking['trade_date']).toBe('2026-10-25');
        expect(booked.activity_type_code).toBe('new_booking');
        // The identifier and the instrument carry the activity the service made.
        const identified = calls[1]?.body as { change: { write: Record<string, unknown> } };
        expect(identified.change.write['trade_activity_id']).toBe(ACTIVITY);
        expect(identified.change.write['id_value']).toBe('DESK-1');
        const instrumented = calls[2]?.body as { change: { write: Record<string, unknown> } };
        expect(instrumented.change.write['notional']).toBe('2500000.50');
        expect(instrumented.change.write['issue_id']).toBe(ISSUE);
    });

    it('answers a frozen book with the service reason, as a bad request', async () => {
        const { server, sessionId } = buildTestServer({
            'trading.v1.ops.book_trade': {
                result: { outcome: 'failed', code: 'book_not_open', message: 'Book is not open.' },
                activity_id: null,
            },
        });

        const reply = await book(server, sessionId);

        expect(reply.statusCode).toBe(400);
        expect(reply.json()).toEqual({ code: 'invalid-request', message: 'Book is not open.' });
    });

    it('refuses a booking from a session that works for no party', async () => {
        const { server, sessionId, calls } = buildTestServer({}, null);

        const reply = await book(server, sessionId);
        expect(reply.statusCode).toBe(400);
        expect(calls).toEqual([]);
    });

    it('refuses a body that is not a booking', async () => {
        const { server, sessionId } = buildTestServer({});

        const reply = await book(server, sessionId, { bookId: 'not-a-uuid' });

        expect(reply.statusCode).toBe(400);
    });
});
