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
import { uuid, type OresClient, type SessionMode } from '@ores/wire-protocol';
import type { Config } from './config.js';
import { buildServer, sessionCookieName } from './server.js';
import { createSessionStore } from './sessions.js';
import { loadSiteConfiguration, SITE_CONFIG_VARIABLE } from './site-config.js';

/**
 * Booking through the trading service's operations: a script validated against
 * a trade's variables, a scripted trade and a product type booked, and the form
 * a trade is reopened in.
 */

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

function buildTestServer(replies: Record<string, unknown>) {
    const sessions = createSessionStore({ ttlSeconds: 60 });
    const calls: { subject: string; body: unknown }[] = [];
    const client = {
        enteredTenant: undefined,
        async callAuthenticated(
            subject: string,
            body: unknown,
            schema: { parse: (value: unknown) => unknown },
        ): Promise<unknown> {
            calls.push({ subject, body });
            return schema.parse(replies[subject]);
        },
        async close(): Promise<void> {
            return undefined;
        },
    } as unknown as OresClient;
    const party = {
        id: uuid('22222222-2222-2222-2222-222222222222'),
        name: 'System Party',
        partyCategory: 'System',
        businessCenterCode: '',
    };
    const own = {
        accountId: '11111111-1111-1111-1111-111111111111',
        tenantId: '44444444-4444-4444-4444-444444444444',
        tenantName: 'Acme Corporation',
        version: 'v0.0.25 (test)',
        username: 'admin',
        email: 'admin@example.com',
        availableParties: [party],
        accessLifetimeSeconds: 1800,
        passwordResetRequired: false,
        sessionId: '33333333-3333-3333-3333-333333333333',
    };
    const session = sessions.create({
        ...own,
        client,
        session: { ...own, kind: 'active', token: 't', party },
        mode: 'application' as SessionMode,
    });
    const server = buildServer({
        config,
        site: siteConfiguration(),
        sessions,
        createClient: () => ({ client, connect: async () => undefined }),
    });
    return { server, cookies: { [SESSION_COOKIE]: session.id }, calls };
}

const VALIDATE = 'trading.v1.ops.validate_script';
const SCRIPTED = 'trading.v1.ops.book_scripted';
const PRODUCT = 'trading.v1.ops.book_product';
const TRADE_PRODUCT = 'trading.v1.trade_products.get';
const SCRIPTED_ROW = 'trading.v1.scripted_instruments.get';

const header = {
    bookId: '55555555-5555-4555-8555-555555555555',
    counterpartyId: '66666666-6666-4666-8666-666666666666',
    tradeDate: '2026-11-23',
};

const variables = [
    { name: 'Notional', type: 'Number', isArray: false, values: ['1000'] },
    { name: 'Expiry', type: 'Event', isArray: false, values: ['2027-06-30'] },
];

describe('POST /api/trading/booking/validate-script', () => {
    it('sends the script and the variables as the wire states them, and answers the findings', async () => {
        const { server, cookies, calls } = buildTestServer({
            [VALIDATE]: {
                success: true,
                message: '1 error(s) and 0 warning(s).',
                is_valid: false,
                diagnostics: [
                    {
                        purpose: '',
                        severity: 'error',
                        code: 'SCR2001',
                        line: 0,
                        column: null,
                        message: 'missing',
                        subject: 'PayCcy',
                    },
                ],
            },
        });

        const response = await server.inject({
            method: 'POST',
            url: '/api/trading/booking/validate-script',
            cookies,
            payload: { scriptName: 'TenantPayoff', variables },
        });
        await server.close();

        expect(response.statusCode).toBe(200);
        expect(calls).toEqual([
            {
                subject: VALIDATE,
                body: {
                    script_name: 'TenantPayoff',
                    script_version: 0,
                    code: '',
                    variables: [
                        { name: 'Notional', type: 'Number', is_array: false, values: ['1000'] },
                        { name: 'Expiry', type: 'Event', is_array: false, values: ['2027-06-30'] },
                    ],
                },
            },
        ]);
        expect(response.json()).toMatchObject({
            isValid: false,
            diagnostics: [{ subject: 'PayCcy', code: 'SCR2001' }],
        });
    });
});

describe('POST /api/trading/booking/scripted', () => {
    it('books a scripted trade with the session party and answers its id and activity', async () => {
        const { server, cookies, calls } = buildTestServer({
            [SCRIPTED]: {
                result: { outcome: 'ok' },
                activity_id: '77777777-7777-4777-8777-777777777777',
                diagnostics: [],
            },
        });

        const response = await server.inject({
            method: 'POST',
            url: '/api/trading/booking/scripted',
            cookies,
            payload: { ...header, scriptName: 'TenantPayoff', variables },
        });
        await server.close();

        expect(response.statusCode).toBe(201);
        const sent = calls[0]?.body as Record<string, Record<string, unknown>>;
        expect(calls[0]?.subject).toBe(SCRIPTED);
        expect(sent['anchor']).toMatchObject({
            party_id: '22222222-2222-2222-2222-222222222222',
            counterparty_id: header.counterpartyId,
            trade_type: 'ScriptedTrade',
        });
        expect(sent['booking']).toMatchObject({
            book_id: header.bookId,
            trade_date: header.tradeDate,
        });
        expect(sent).toMatchObject({ script_name: 'TenantPayoff', product_code: '' });
        expect(response.json()).toEqual({
            id: sent['anchor']['id'],
            activityId: '77777777-7777-4777-8777-777777777777',
        });
    });

    it('answers a script that fails validation with a 422 that carries its findings', async () => {
        const { server, cookies } = buildTestServer({
            [SCRIPTED]: {
                result: { outcome: 'failed', code: 'script_invalid', message: 'refused' },
                activity_id: null,
                diagnostics: [
                    {
                        purpose: '',
                        severity: 'error',
                        code: 'SCR2001',
                        line: 0,
                        column: null,
                        message: 'missing',
                        subject: 'Notional',
                    },
                ],
            },
        });

        const response = await server.inject({
            method: 'POST',
            url: '/api/trading/booking/scripted',
            cookies,
            payload: { ...header, scriptName: 'TenantPayoff', variables: [] },
        });
        await server.close();

        expect(response.statusCode).toBe(422);
        expect(JSON.stringify(response.json())).toContain('Notional');
    });
});

describe('POST /api/trading/booking/product', () => {
    it('books a product type in a form, passing its rows through', async () => {
        const { server, cookies, calls } = buildTestServer({
            [PRODUCT]: {
                result: { outcome: 'ok' },
                activity_id: '77777777-7777-4777-8777-777777777777',
            },
        });
        const instruments = { equity_basket_option_instruments: [{ currency: 'USD' }] };

        const response = await server.inject({
            method: 'POST',
            url: '/api/trading/booking/product',
            cookies,
            payload: {
                ...header,
                productCode: 'EquityBasketOption',
                form: 'scripted',
                instruments,
            },
        });
        await server.close();

        expect(response.statusCode).toBe(201);
        expect(calls[0]?.subject).toBe(PRODUCT);
        expect(calls[0]?.body).toMatchObject({
            product_code: 'EquityBasketOption',
            booking_form: 'scripted',
            script_name: '',
            instruments,
        });
    });

    it('answers a form the product does not carry with a 404', async () => {
        const { server, cookies } = buildTestServer({
            [PRODUCT]: {
                result: { outcome: 'failed', code: 'form_not_offered', message: 'No such form.' },
            },
        });

        const response = await server.inject({
            method: 'POST',
            url: '/api/trading/booking/product',
            cookies,
            payload: {
                ...header,
                productCode: 'EquityBasketOption',
                form: 'scripted',
                instruments: {},
            },
        });
        await server.close();

        expect(response.statusCode).toBe(404);
    });
});

describe('GET /api/trading/booking/:tradeId/form', () => {
    it('answers the form a scripted trade was booked in, with the script version the store pinned', async () => {
        const tradeId = '88888888-8888-4888-8888-888888888888';
        const { server, cookies } = buildTestServer({
            [TRADE_PRODUCT]: {
                result: { outcome: 'ok' },
                trade_product: { product_code: 'EquityBasketOption', booking_form: 'scripted' },
            },
            [SCRIPTED_ROW]: {
                result: { outcome: 'ok' },
                scripted_instrument: { script_name: 'VanillaBasketOption', script_version: 3 },
            },
        });

        const response = await server.inject({
            method: 'GET',
            url: `/api/trading/booking/${tradeId}/form`,
            cookies,
        });
        await server.close();

        expect(response.statusCode).toBe(200);
        expect(response.json()).toEqual({
            tradeId,
            productCode: 'EquityBasketOption',
            form: 'scripted',
            scriptName: 'VanillaBasketOption',
            scriptVersion: 3,
        });
    });

    it('answers a trade that records no product with a 404', async () => {
        const { server, cookies } = buildTestServer({
            [TRADE_PRODUCT]: { result: { outcome: 'ok' }, trade_product: null },
        });

        const response = await server.inject({
            method: 'GET',
            url: '/api/trading/booking/88888888-8888-4888-8888-888888888888/form',
            cookies,
        });
        await server.close();

        expect(response.statusCode).toBe(404);
    });
});
