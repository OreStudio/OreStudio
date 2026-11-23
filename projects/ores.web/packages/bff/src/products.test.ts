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
 * The products a screen can book, and the description of one.
 *
 * The routes name each field they answer, so a screen reads one shape, and a
 * product the service cannot describe is a 404 that states the service's reason.
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

const DESCRIBE = 'trading.v1.ops.describe_product';
const LIST = 'trading.v1.ops.list_products';

describe('GET /api/trading/products', () => {
    it('lists the products the service names, by code, name and construction', async () => {
        const { server, cookies, calls } = buildTestServer({
            [LIST]: {
                success: true,
                message: '',
                products: [
                    { code: 'Swap', name: 'Swap', construction: 'native', extra: 1 },
                    { code: 'Corridor', name: 'Corridor', construction: 'script' },
                ],
            },
        });

        const response = await server.inject({
            method: 'GET',
            url: '/api/trading/products',
            cookies,
        });
        await server.close();

        expect(response.statusCode).toBe(200);
        expect(calls).toEqual([{ subject: LIST, body: {} }]);
        expect(response.json()).toEqual({
            rows: [
                { code: 'Swap', name: 'Swap', construction: 'native' },
                { code: 'Corridor', name: 'Corridor', construction: 'script' },
            ],
        });
    });
});

describe('GET /api/trading/products/:code', () => {
    it('answers a code no product has with a 404, and a service fault with a 5xx', async () => {
        const { server, cookies } = buildTestServer({
            [DESCRIBE]: { success: false, refusal_code: 'not_found', message: 'No product.' },
        });
        const missing = await server.inject({
            method: 'GET',
            url: '/api/trading/products/Nothing',
            cookies,
        });
        await server.close();
        expect(missing.statusCode).toBe(404);

        const faulty = buildTestServer({
            [DESCRIBE]: { success: false, refusal_code: '', message: 'It logged the reason.' },
        });
        const fault = await faulty.server.inject({
            method: 'GET',
            url: '/api/trading/products/Swap',
            cookies: faulty.cookies,
        });
        await faulty.server.close();
        expect(fault.statusCode).toBeGreaterThanOrEqual(500);
    });

    it('describes a product: its fields, the inputs and codes of its scripts, and its forms', async () => {
        const { server, cookies, calls } = buildTestServer({
            [DESCRIBE]: {
                success: true,
                message: '',
                product: { code: 'Swap', name: 'Swap', construction: 'native' },
                element: 'SwapData',
                fields: [
                    {
                        type_name: 'trade',
                        name: 'SwapData',
                        position: 3,
                        label: 'Swap data',
                        help: 'The data of a Swap trade.',
                        description: 'The data of a Swap trade.',
                        value_domain: 'record:swapData',
                        is_required: true,
                        default_text: 'None.',
                        format: 'A swap data record.',
                        xpath: "/Trade[TradeType='Swap']/SwapData",
                    },
                ],
                inputs: [
                    {
                        script_name: 'SwapScript',
                        input_name: 'PutCall',
                        data_type: 'Number',
                        is_array: false,
                        value_rule: null,
                        description: '1 for call, -1 for put',
                    },
                ],
                codes: [
                    { script_name: 'SwapScript', input_name: 'PutCall', code: -1, label: 'put' },
                ],
                forms: [
                    { kind: 'native', script_name: '', product_tag: '' },
                    {
                        kind: 'scripted',
                        script_name: 'SwapScript',
                        product_tag: 'SingleAssetOption',
                    },
                ],
            },
        });

        const response = await server.inject({
            method: 'GET',
            url: '/api/trading/products/Swap',
            cookies,
        });
        await server.close();

        expect(response.statusCode).toBe(200);
        expect(calls).toEqual([{ subject: DESCRIBE, body: { code: 'Swap' } }]);
        const body = response.json();
        expect(body.element).toBe('SwapData');
        expect(body.fields[0]).toMatchObject({
            value_domain: 'record:swapData',
            is_required: true,
        });
        expect(body.inputs).toEqual([
            {
                script_name: 'SwapScript',
                name: 'PutCall',
                data_type: 'Number',
                is_array: false,
                value_rule: '',
                description: '1 for call, -1 for put',
            },
        ]);
        expect(body.codes).toEqual([
            { script_name: 'SwapScript', input_name: 'PutCall', code: -1, label: 'put' },
        ]);
        expect(body.forms.map((f: { kind: string }) => f.kind)).toEqual(['native', 'scripted']);
    });

    it('answers a product with nothing to describe with a 422 that states its reason', async () => {
        const reason =
            'ORE has no trade builder for the product type CommodityPairwiseVarianceSwap.';
        const { server, cookies } = buildTestServer({
            [DESCRIBE]: { success: false, refusal_code: 'not_describable', message: reason },
        });

        const response = await server.inject({
            method: 'GET',
            url: '/api/trading/products/CommodityPairwiseVarianceSwap',
            cookies,
        });
        await server.close();

        expect(response.statusCode).toBe(422);
        expect(response.json().message).toBe(reason);
    });
});
