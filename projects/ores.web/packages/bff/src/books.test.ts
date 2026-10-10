/** -*- mode: typescript-ts-mode; tab-width: 4; indent-tabs-mode: nil -*-
 *
 * Copyright (C) 2026 Marco Craveiro <marco.craveiro@gmail.com>
 *
 * This program is free software; you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation; either version 3 of the License, or (at your option) any later
 * version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
 * details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 *
 */

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

/** The environment this file's site configuration serves. */
const ENVIRONMENT_ID = 'eager_maxwell';
const SESSION_COOKIE = sessionCookieName(ENVIRONMENT_ID);

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

const ok = { outcome: 'ok', code: '', message: '' };

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
        version: 'v0.0.25 (test)',
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

function send(
    server: ReturnType<typeof buildServer>,
    sessionId: string,
    method: 'GET' | 'POST' | 'PUT' | 'DELETE',
    url: string,
    payload?: unknown,
): ReturnType<ReturnType<typeof buildServer>['inject']> {
    return server.inject({
        method,
        url,
        cookies: { [SESSION_COOKIE]: sessionId },
        ...(payload === undefined ? {} : { payload: payload as Record<string, unknown> }),
    });
}

const ID = '55555555-5555-4555-8555-555555555555';
const PARTY = '77777777-7777-4777-8777-777777777777';
const INTENT = { reason_code: 'common.correction', commentary: '' };

describe('book structure routes', () => {
    it('reads the tree: every portfolio and every book', async () => {
        const { server, sessionId } = buildTestServer({
            'refdata.v1.portfolios.list': { result: ok, portfolios: [{ id: ID, name: 'Rates' }] },
            'refdata.v1.books.list': {
                result: ok,
                books: [{ id: PARTY, name: 'RATES-1', parent_portfolio_id: ID }],
            },
        });
        const response = await send(server, sessionId, 'GET', '/api/books/tree');
        expect(response.json().portfolios).toHaveLength(1);
        expect(response.json().books[0].name).toBe('RATES-1');
    });

    it('reads the pick lists the screen draws', async () => {
        const rows = (field: string): unknown => ({
            result: ok,
            [field]: [{ code: 'A', version: 1 }],
        });
        const { server, sessionId } = buildTestServer({
            'refdata.v1.book_statuses.list': rows('statuses'),
            'refdata.v1.regulatory_book_types.list': rows('types'),
            'refdata.v1.book_purpose_types.list': rows('types'),
            'refdata.v1.ledger_feed_types.list': rows('types'),
            'refdata.v1.purpose_types.list': rows('types'),
            'refdata.v1.currencies.list': rows('currencies'),
            'refdata.v1.business_centres.list': rows('centres'),
            'refdata.v1.business_units.list': rows('business_units'),
        });
        const response = await send(server, sessionId, 'GET', '/api/books/pick-lists');
        expect(Object.keys(response.json()).sort()).toEqual([
            'bookPurposeTypes',
            'bookStatuses',
            'businessCentres',
            'businessUnits',
            'currencies',
            'ledgerFeedTypes',
            'purposeTypes',
            'regulatoryBookTypes',
        ]);
    });

    it('reads the rights at a node with the accounts that hold them', async () => {
        const { server, sessionId, calls } = buildTestServer({
            'refdata.v1.portfolio_rights.list_by_portfolio_id': {
                result: ok,
                portfolio_rights: [{ account_id: PARTY, portfolio_id: ID, right_code: 'read' }],
            },
            'iam.v1.accounts.list': { result: ok, accounts: [{ id: PARTY, username: 'ana' }] },
        });
        const response = await send(server, sessionId, 'GET', `/api/books/portfolios/${ID}/rights`);
        expect(response.json().rights).toHaveLength(1);
        expect(response.json().accounts[0].username).toBe('ana');
        expect(calls[0]?.body).toMatchObject({ portfolio_id: ID });
    });

    it('writes a new book as new and a changed one against its version', async () => {
        const { server, sessionId, calls } = buildTestServer({
            'refdata.v1.books.put': { result: ok, book: { id: ID, version: 1 } },
        });
        const write = { id: ID, party_id: PARTY, name: 'RATES-1' };
        const created = await send(server, sessionId, 'PUT', '/api/books/books', {
            intent: INTENT,
            version: null,
            write,
        });
        expect(created.json().book.version).toBe(1);
        await send(server, sessionId, 'PUT', '/api/books/books', {
            intent: INTENT,
            version: 3,
            write,
        });
        expect(calls[0]?.body).toMatchObject({
            change: { precondition: { kind: 'must_not_exist' } },
        });
        expect(calls[1]?.body).toMatchObject({
            change: { precondition: { kind: 'must_match_version', version: 3 } },
        });
    });

    it('returns the typed refusal a status transition gives', async () => {
        const { server, sessionId } = buildTestServer({
            'refdata.v1.books.put': {
                result: {
                    outcome: 'invalid',
                    code: 'status_transition_not_allowed',
                    message: 'No.',
                    fields: [{ field: 'book_status', code: 'transition', message: 'No.' }],
                },
                book: null,
            },
        });
        const response = await send(server, sessionId, 'PUT', '/api/books/books', {
            intent: INTENT,
            version: 1,
            write: { id: ID, party_id: PARTY, name: 'RATES-1' },
        });
        expect(response.json().result.code).toBe('status_transition_not_allowed');
        expect(response.json().result.fields[0].field).toBe('book_status');
    });

    it('writes a portfolio and refuses a body with no name', async () => {
        const { server, sessionId } = buildTestServer({
            'refdata.v1.portfolios.put': { result: ok, portfolio: { id: ID, version: 1 } },
        });
        const made = await send(server, sessionId, 'PUT', '/api/books/portfolios', {
            intent: INTENT,
            version: null,
            write: { id: ID, party_id: PARTY, name: 'Rates' },
        });
        expect(made.json().portfolio.version).toBe(1);
        const bad = await send(server, sessionId, 'PUT', '/api/books/portfolios', {
            intent: INTENT,
            version: null,
            write: { id: ID, party_id: PARTY, name: '' },
        });
        expect(bad.statusCode).toBe(400);
    });
});
