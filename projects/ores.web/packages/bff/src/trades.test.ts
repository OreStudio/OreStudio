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

function send(
    server: ReturnType<typeof buildServer>,
    sessionId: string,
    url: string,
): ReturnType<ReturnType<typeof buildServer>['inject']> {
    return server.inject({ method: 'GET', url, cookies: { [SESSION_COOKIE]: sessionId } });
}

const TRADE = '55555555-5555-4555-8555-555555555555';
const STRUCTURE = '66666666-6666-4666-8666-666666666666';
const BOOK = '77777777-7777-4777-8777-777777777777';
const COUNTERPARTY = '88888888-8888-4888-8888-888888888888';
const PARTY = '99999999-9999-4999-8999-999999999999';

const listSummaries = 'trading.v1.trade_summaries.list';

/** The two rows the read model returns: one trade and one structure beside it. */
const SUMMARIES = {
    result: ok,
    total: 2,
    trade_summaries: [
        {
            id: TRADE,
            row_kind: 'trade',
            party_id: PARTY,
            counterparty_id: COUNTERPARTY,
            book_id: BOOK,
            instrument_type: 'Swap',
            status: 'live',
            authorisation_status: 'not_requested',
            trade_date: '2026-09-14',
            trade_count: 1,
            search_text: TRADE,
        },
        {
            id: STRUCTURE,
            row_kind: 'structure',
            party_id: PARTY,
            counterparty_id: COUNTERPARTY,
            book_id: null,
            instrument_type: 'Strategy',
            status: null,
            authorisation_status: 'not_applicable',
            trade_date: null,
            trade_count: 2,
            search_text: STRUCTURE,
        },
    ],
};

describe('trade list route', () => {
    it('pages the read model and enriches the page with names, references and versions', async () => {
        const { server, sessionId, calls } = buildTestServer({
            [listSummaries]: SUMMARIES,
            'trading.v1.trade_identifiers.list': {
                result: ok,
                identifiers: [
                    { id: 'a', trade_id: TRADE, id_type: 'UTI', id_value: 'UTI-1' },
                    { id: 'b', trade_id: TRADE, id_type: 'Internal', id_value: 'ACME-100201' },
                ],
            },
            'refdata.v1.books.list': {
                result: ok,
                books: [{ id: BOOK, name: 'EMEA Rates' }],
            },
            'refdata.v1.counterparties.list': {
                result: ok,
                counterparties: [{ id: COUNTERPARTY, full_name: 'Northwind Capital Ltd' }],
            },
            'trading.v1.trades.list': {
                result: ok,
                trades: [{ id: TRADE, version: 5, external_version: 3 }],
            },
        });

        const reply = await send(server, sessionId, '/api/trades');
        const body = reply.json();

        expect(body.total).toBe(2);
        expect(body.rows).toHaveLength(2);
        expect(body.rows[0]).toMatchObject({
            id: TRADE,
            row_kind: 'trade',
            identifier: 'ACME-100201',
            counterparty_name: 'Northwind Capital Ltd',
            book_name: 'EMEA Rates',
            version: 5,
            external_version: 3,
        });
        expect(calls[0]?.subject).toBe(listSummaries);
        expect(calls[0]?.body).toMatchObject({
            offset: 0,
            limit: 25,
            order: { field: 'trade_date', descending: true },
        });
    });

    it('reads one trade with its parts, its links at both ends and its history', async () => {
        const { server, sessionId, calls } = buildTestServer({
            [listSummaries]: SUMMARIES,
            'trading.v1.trade_identifiers.list': {
                result: ok,
                identifiers: [
                    { id: 'a', trade_id: TRADE, id_type: 'Internal', id_value: 'ACME-1' },
                ],
            },
            'trading.v1.trade_party_roles.list': {
                result: ok,
                roles: [{ trade_id: TRADE, party_id: COUNTERPARTY, role_type: 'buyer' }],
            },
            'trading.v1.trade_bookings.list': {
                result: ok,
                trade_bookings: [
                    { trade_id: TRADE, trade_date: '2026-01-02', execution_timestamp: null },
                ],
            },
            'trading.v1.trade_links.list': {
                result: ok,
                links: [],
            },
            'trading.v1.bond_instruments.list': {
                result: ok,
                bond_instruments: [
                    {
                        trade_id: TRADE,
                        notional: '1000000.00',
                        issue_id: 'a',
                        identity: { trade_type_code: 'Bond' },
                    },
                ],
            },
            'trading.v1.bond_issues.list': {
                result: ok,
                bond_issues: [{ issue_id: 'a', security_id: 'POP09MP3R9XP5' }],
            },
            'trading.v1.trades_versions.list': {
                result: ok,
                versions: [{ id: TRADE, version: 2 }],
                total: 1,
            },
            'refdata.v1.books.list': { result: ok, books: [{ id: BOOK, name: 'EMEA Rates' }] },
            'refdata.v1.counterparties.list': {
                result: ok,
                counterparties: [{ id: COUNTERPARTY, full_name: 'Northwind Capital Ltd' }],
            },
            'trading.v1.trades.list': {
                result: ok,
                trades: [{ id: TRADE, version: 2, external_version: 0 }],
            },
        });

        const body = (await send(server, sessionId, `/api/trades/${TRADE}`)).json();

        expect(body.trade).toMatchObject({ id: TRADE, identifier: 'ACME-1' });
        expect(body.identifiers).toHaveLength(1);
        expect(body.roles).toHaveLength(1);
        expect(body.booking).toMatchObject({ trade_date: '2026-01-02' });
        expect(body.links).toEqual({ from: [], to: [] });
        expect(body.history).toHaveLength(1);
        // The fixture's trade is a Swap, which has no instrument table yet, so
        // the page is told that rather than shown empty terms.
        expect(body.terms).toBeNull();
        expect(body.routed).toBe(false);
        // Both ends of the links are read, which is how one trade's links are found.
        const linkCalls = calls.filter((call) => call.subject === 'trading.v1.trade_links.list');
        expect(linkCalls).toHaveLength(2);
    });

    it("reads a bond trade's terms and names the issue they refer to", async () => {
        const bond = { ...SUMMARIES.trade_summaries[0], instrument_type: 'Bond' };
        const { server, sessionId } = buildTestServer({
            [listSummaries]: { result: ok, trade_summaries: [bond], total: 1 },
            'trading.v1.bond_instruments.list': {
                result: ok,
                bond_instruments: [
                    {
                        trade_id: TRADE,
                        notional: '1000000.00',
                        issue_id: 'i',
                        identity: { trade_type_code: 'Bond' },
                    },
                ],
            },
            'trading.v1.bond_issues.list': {
                result: ok,
                issues: [{ issue_id: 'i', security_id: 'POP09MP3R9XP5' }],
            },
            'trading.v1.trade_identifiers.list': { result: ok, identifiers: [] },
            'trading.v1.trade_party_roles.list': { result: ok, roles: [] },
            'trading.v1.trade_bookings.list': { result: ok, trade_bookings: [] },
            'trading.v1.trade_links.list': { result: ok, links: [] },
            'trading.v1.trades_versions.list': { result: ok, versions: [] },
            'refdata.v1.books.list': { result: ok, books: [] },
            'refdata.v1.counterparties.list': { result: ok, counterparties: [] },
            'trading.v1.trades.list': { result: ok, trades: [] },
        });

        const body = (await send(server, sessionId, `/api/trades/${TRADE}`)).json();

        expect(body.routed).toBe(true);
        expect(body.terms).toMatchObject({
            notional: '1000000.00',
            issue: { security_id: 'POP09MP3R9XP5' },
        });
    });

    it('answers not found for a trade that does not exist', async () => {
        const { server, sessionId } = buildTestServer({
            [listSummaries]: { result: ok, trade_summaries: [], total: 0 },
        });

        const reply = await send(server, sessionId, `/api/trades/${TRADE}`);

        expect(reply.statusCode).toBe(404);
    });

    it('reads a structure row without a trade behind it', async () => {
        const { server, sessionId } = buildTestServer({
            [listSummaries]: SUMMARIES,
            'trading.v1.trade_identifiers.list': { result: ok, identifiers: [] },
            'refdata.v1.books.list': { result: ok, books: [] },
            'refdata.v1.counterparties.list': { result: ok, counterparties: [] },
            'trading.v1.trades.list': { result: ok, trades: [] },
        });

        const body = (await send(server, sessionId, '/api/trades')).json();
        expect(body.rows[1]).toMatchObject({
            row_kind: 'structure',
            identifier: '',
            version: null,
            external_version: null,
        });
    });

    it('sends the screen filters as the filter record the read model declares', async () => {
        const { server, sessionId, calls } = buildTestServer({
            [listSummaries]: { result: ok, total: 0, trade_summaries: [] },
        });

        await send(
            server,
            sessionId,
            `/api/trades?search=acme&status=live&type=Swap&party=${PARTY}&book=${BOOK}&from=2026-01-01&to=2026-12-31&sort=status&descending=false`,
        );

        expect(calls[0]?.body).toMatchObject({
            order: { field: 'status', descending: false },
            filter: {
                search: 'acme',
                status: 'live',
                instrument_type: 'Swap',
                party_id: PARTY,
                book_id: BOOK,
                trade_date_from: '2026-01-01',
                trade_date_to: '2026-12-31',
                counterparty_id: null,
            },
        });
    });

    it('refuses a sort the read model cannot order by', async () => {
        const { server, sessionId } = buildTestServer({});
        const reply = await send(server, sessionId, '/api/trades?sort=notional');
        expect(reply.statusCode).toBe(400);
    });
});
