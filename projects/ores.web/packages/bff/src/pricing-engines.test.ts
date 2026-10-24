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

const CONFIG_ID = '55555555-5555-4555-8555-555555555555';
const PRODUCT_ID = '66666666-6666-4666-8666-666666666666';
const PARAMETER_ID = '88888888-8888-4888-8888-888888888888';
const HEADER_ID = '99999999-9999-4999-8999-999999999999';

const PARTY_ID = '0a12c57f-1111-4222-8333-444444444444';

const header = {
    id: CONFIG_ID,
    party_id: PARTY_ID,
    version: 3,
    name: 'EOD pricing',
    description: 'The end of day set',
    config_variant: 'eod',
    configuration_id: HEADER_ID,
    modified_by: 'quant',
    recorded_at: '2026-10-19 10:00:00.000000Z',
    change_reason_code: 'common.correction',
    change_commentary: 'Fix the grid',
};
const product = {
    id: PRODUCT_ID,
    version: 1,
    pricing_model_config_id: CONFIG_ID,
    pricing_engine_type_code: 'BermudanSwaption',
    model: 'LGM',
    engine: 'Grid',
    position: 0,
};
const parameter = {
    id: PARAMETER_ID,
    version: 1,
    pricing_model_config_id: CONFIG_ID,
    pricing_model_product_id: PRODUCT_ID,
    parameter_scope: 'model',
    parameter_name: 'Calibration',
    parameter_value: 'Bootstrap',
    position: 0,
};
const stranger = { ...product, id: HEADER_ID, pricing_model_config_id: HEADER_ID };

const reads = {
    'iam.v1.ops.get_my_parties': {
        result: ok,
        default_party_id: PARTY_ID,
        parties: [
            {
                party_id: PARTY_ID,
                name: 'ACME Corporation UK',
                short_code: 'ACMEUK',
                party_category: 'Operational',
                business_center_code: 'GBLO',
            },
        ],
    },
    'analytics.v1.pricing_model_configs.list': { result: ok, configs: [header] },
    'analytics.v1.pricing_model_products.list': { result: ok, products: [product, stranger] },
    'analytics.v1.pricing_model_product_parameters.list': { result: ok, parameters: [parameter] },
    'reporting.v1.report_configurations.list': { result: ok, report_configurations: [] },
};

const INTENT = { reason_code: 'common.correction', commentary: 'Fix the grid' };
const body = {
    name: 'EOD pricing',
    description: 'The end of day set',
    variant: 'eod',
    products: [
        {
            id: PRODUCT_ID,
            version: 1,
            typeCode: 'BermudanSwaption',
            model: 'LGM',
            engine: 'Grid',
            position: 0,
        },
    ],
    parameters: [
        {
            id: PARAMETER_ID,
            version: 1,
            productId: PRODUCT_ID,
            scope: 'model',
            name: 'Calibration',
            value: 'BestFit',
            position: 0,
        },
    ],
    intent: INTENT,
};

/** The replies the vocabulary route reads from the analytics and refdata services. */
function vocabularyReads(): Record<string, unknown> {
    return {
        'analytics.v1.pricing_engine_types.list': {
            result: ok,
            types: [
                {
                    code: 'BermudanSwaption',
                    description: 'Bermudan',
                    instrument_type_code: 'SWPTN',
                },
            ],
        },
        'refdata.v1.instrument_codes.list': {
            result: ok,
            instruments: [{ code: 'SWPTN', asset_class: 'interest_rates' }],
        },
        'reporting.v1.ore_parameters.list': {
            result: ok,
            parameters: [
                {
                    schema_name: 'ore.xsd',
                    scope: 'Models',
                    subtype: '',
                    name: 'LGM',
                    label: 'LGM',
                    help: 'Linear Gauss Markov model',
                    description: 'A one-factor Gaussian rates model.',
                    default_text: 'The name of a pricing model.',
                    format: 'The name of a pricing model.',
                    introduced_in: '1.8.7.0',
                    removed_in: '',
                    xpath: "/PricingEngines/Product/Model[@name='LGM']",
                },
                {
                    schema_name: 'ore.xsd',
                    scope: 'Engines',
                    subtype: '',
                    name: 'Grid',
                    label: 'Grid',
                    help: 'Convolution grid engine',
                    description: 'Rolls the value back on a grid.',
                    default_text: 'The name of a pricing engine.',
                    format: 'The name of a pricing engine.',
                    introduced_in: '1.8.7.0',
                    removed_in: '',
                    xpath: "/PricingEngines/Product/Engine[@name='Grid']",
                },
                {
                    schema_name: 'ore.xsd',
                    scope: 'EngineParameters',
                    subtype: 'BermudanSwaption',
                    name: 'SensitivityTemplate',
                    label: 'Sensitivity template',
                    help: 'The sensitivity template of the product.',
                    description: 'The sensitivity template of the product.',
                    default_text: 'IR_FD',
                    format: 'The code of a sensitivity template.',
                    introduced_in: '1.8.0.7 or earlier',
                    removed_in: '',
                    xpath: "/PricingEngines/Product[@type='BermudanSwaption']/EngineParameters/Parameter[@name='SensitivityTemplate']",
                },
            ],
        },
        'analytics.v1.pricing_model_engine_pairs.list': {
            result: ok,
            pairs: [
                {
                    pricing_engine_type_code: 'BermudanSwaption',
                    model: 'LGM',
                    engine: 'Grid',
                    is_usual: true,
                    note: '',
                },
            ],
        },
        'analytics.v1.pricing_pair_settings.list': {
            result: ok,
            settings: [
                {
                    pricing_engine_type_code: 'BermudanSwaption',
                    model: 'LGM',
                    engine: 'Grid',
                    scope: 'model',
                    name: 'Calibration',
                    kind: 'select',
                    usual_value: 'Bootstrap',
                    options: 'Bootstrap,BestFit,None',
                    is_required: true,
                    note: 'How the model is fitted.',
                },
                {
                    pricing_engine_type_code: 'BermudanSwaption',
                    model: 'LGM',
                    engine: 'Grid',
                    scope: 'engine',
                    name: 'SensitivityTemplate',
                    kind: 'reference',
                    usual_value: 'IR_FD',
                    options: '',
                    is_required: false,
                    note: 'The sensitivity template of the product.',
                },
            ],
        },
        'analytics.v1.pricing_models.list': {
            result: ok,
            models: [
                {
                    code: 'LGM',
                    description: 'Linear Gauss Markov model',
                    detail: 'A one-factor Gaussian rates model.',
                    source: 'ore_guide',
                },
            ],
        },
        'analytics.v1.pricing_methods.list': {
            result: ok,
            methods: [
                {
                    code: 'Grid',
                    description: 'Convolution grid engine',
                    detail: 'Rolls the value back on a grid.',
                    source: 'ore_guide',
                },
            ],
        },
        'analytics.v1.sensitivity_templates.list': {
            result: ok,
            templates: [
                {
                    code: 'IR_FD',
                    asset_class: 'IR',
                    method: 'Finite difference',
                    description: 'Interest rate products priced on a grid.',
                },
            ],
        },
        'refdata.v1.tenors.list': {
            result: ok,
            tenors: [
                { code: 'SPOT', display_name: 'Spot', unit: 'DAY', sort_order: 1 },
                { code: '6M', display_name: '6 months', unit: 'MONTH', sort_order: 30 },
                { code: '3M', display_name: '3 months', unit: 'MONTH', sort_order: 20 },
                { code: 'ON', display_name: 'Overnight', unit: 'NONE', sort_order: 0 },
            ],
        },
    };
}

describe('pricing engine routes', () => {
    it('lists the configurations with how many products and parameters each holds', async () => {
        const { server, sessionId } = buildTestServer(reads);
        const response = await send(server, sessionId, 'GET', '/api/reporting/pricing-engines');
        expect(response.statusCode).toBe(200);
        expect(response.json().rows).toEqual([
            expect.objectContaining({
                id: CONFIG_ID,
                name: 'EOD pricing',
                products: 1,
                parameters: 1,
            }),
        ]);
    });

    it('names the party that holds each configuration', async () => {
        const { server, sessionId } = buildTestServer(reads);
        const list = await send(server, sessionId, 'GET', '/api/reporting/pricing-engines');
        expect(list.json().rows[0]).toMatchObject({
            partyId: PARTY_ID,
            partyName: 'ACME Corporation UK',
            partyCode: 'ACMEUK',
        });
        const one = await send(
            server,
            sessionId,
            'GET',
            `/api/reporting/pricing-engines/${CONFIG_ID}`,
        );
        expect(one.json()).toMatchObject({ partyId: PARTY_ID, partyName: 'ACME Corporation UK' });
    });

    it('still lists the configurations when the parties cannot be read', async () => {
        const { 'iam.v1.ops.get_my_parties': _parties, ...withoutParties } = reads;
        const { server, sessionId } = buildTestServer(withoutParties);
        const response = await send(server, sessionId, 'GET', '/api/reporting/pricing-engines');
        expect(response.statusCode).toBe(200);
        expect(response.json().rows[0]).toMatchObject({ partyId: PARTY_ID, partyName: '' });
    });

    it('reads one configuration with only its own products and parameters', async () => {
        const { server, sessionId } = buildTestServer(reads);
        const response = await send(
            server,
            sessionId,
            'GET',
            `/api/reporting/pricing-engines/${CONFIG_ID}`,
        );
        expect(response.statusCode).toBe(200);
        const document = response.json();
        expect(document.version).toBe(3);
        expect(document.products).toEqual([
            {
                id: PRODUCT_ID,
                version: 1,
                typeCode: 'BermudanSwaption',
                model: 'LGM',
                engine: 'Grid',
                position: 0,
            },
        ]);
        expect(document.parameters[0]).toMatchObject({
            productId: PRODUCT_ID,
            name: 'Calibration',
            value: 'Bootstrap',
        });
    });

    it('answers 404 for a configuration the party does not hold', async () => {
        const { server, sessionId } = buildTestServer({
            ...reads,
            'analytics.v1.pricing_model_configs.list': { result: ok, configs: [] },
        });
        const response = await send(
            server,
            sessionId,
            'GET',
            `/api/reporting/pricing-engines/${CONFIG_ID}`,
        );
        expect(response.statusCode).toBe(404);
    });

    it('answers 400 for an address that is not an id', async () => {
        const { server, sessionId } = buildTestServer(reads);
        const response = await send(
            server,
            sessionId,
            'GET',
            '/api/reporting/pricing-engines/not-an-id',
        );
        expect(response.statusCode).toBe(400);
    });

    it('reads the vocabulary the combo boxes draw from', async () => {
        const { server, sessionId } = buildTestServer(vocabularyReads());
        const response = await send(
            server,
            sessionId,
            'GET',
            '/api/reporting/pricing-engines/vocabulary',
        );
        expect(response.statusCode).toBe(200);
        expect(response.json().types[0]).toEqual({
            code: 'BermudanSwaption',
            description: 'Bermudan',
            instrumentTypeCode: 'SWPTN',
            assetClass: 'interest_rates',
        });
        expect(response.json().pairs[0]).toMatchObject({
            model: 'LGM',
            engine: 'Grid',
            usual: true,
        });
        expect(response.json().settings[0].options).toEqual(['Bootstrap', 'BestFit', 'None']);
        expect(response.json().settings[0].required).toBe(true);
        const documented = response
            .json()
            .settings.find((setting: { name: string }) => setting.name === 'SensitivityTemplate');
        expect(documented).toMatchObject({
            documented: true,
            label: 'Sensitivity template',
            help: 'The sensitivity template of the product.',
            defaultText: 'IR_FD',
            format: 'The code of a sensitivity template.',
            xpath: "/PricingEngines/Product[@type='BermudanSwaption']/EngineParameters/Parameter[@name='SensitivityTemplate']",
        });
        expect(response.json().globals).toEqual([]);
    });

    it('describes each model, engine and sensitivity template', async () => {
        const { server, sessionId } = buildTestServer(vocabularyReads());
        const response = await send(
            server,
            sessionId,
            'GET',
            '/api/reporting/pricing-engines/vocabulary',
        );
        expect(response.json().models[0]).toEqual({
            code: 'LGM',
            description: 'Linear Gauss Markov model',
            detail: 'A one-factor Gaussian rates model.',
            source: 'ore_guide',
        });
        expect(response.json().methods[0]).toMatchObject({ code: 'Grid' });
        expect(response.json().templates[0]).toEqual({
            code: 'IR_FD',
            assetClass: 'IR',
            method: 'Finite difference',
            description: 'Interest rate products priced on a grid.',
        });
    });

    it('offers the tenors that are a count of days, weeks, months or years, in order', async () => {
        const { server, sessionId } = buildTestServer(vocabularyReads());
        const response = await send(
            server,
            sessionId,
            'GET',
            '/api/reporting/pricing-engines/vocabulary',
        );
        expect(response.json().tenors.map((tenor: { code: string }) => tenor.code)).toEqual([
            '3M',
            '6M',
        ]);
    });

    it('writes the header and then the document when it creates a configuration', async () => {
        const { server, sessionId, calls } = buildTestServer({
            ...reads,
            'reporting.v1.configurations.put': { result: ok },
            'analytics.v1.pricing_engines_documents.put': {
                success: true,
                message: '',
                id: CONFIG_ID,
            },
        });
        const response = await send(
            server,
            sessionId,
            'POST',
            '/api/reporting/pricing-engines',
            body,
        );
        expect(response.statusCode).toBe(201);
        const subjects = calls.map((call) => call.subject);
        expect(subjects.indexOf('reporting.v1.configurations.put')).toBeLessThan(
            subjects.indexOf('analytics.v1.pricing_engines_documents.put'),
        );
        const saved = calls.find(
            (call) => call.subject === 'analytics.v1.pricing_engines_documents.put',
        )?.body as {
            document: { config: Record<string, unknown>; products: Record<string, unknown>[] };
        };
        expect(saved.document.config).toMatchObject({
            version: 0,
            name: 'EOD pricing',
            config_variant: 'eod',
            change_reason_code: 'common.correction',
        });
        expect(saved.document.products[0]).toMatchObject({
            pricing_model_config_id: saved.document.config['id'],
        });
    });

    it('removes the header again when the document does not save', async () => {
        const { server, sessionId, calls } = buildTestServer({
            'reporting.v1.configurations.put': { result: ok },
            'reporting.v1.configurations.delete': { result: ok },
            'analytics.v1.pricing_engines_documents.put': {
                success: false,
                message: 'Not allowed here.',
                id: '',
            },
        });
        const response = await send(
            server,
            sessionId,
            'POST',
            '/api/reporting/pricing-engines',
            body,
        );
        expect(response.statusCode).toBe(400);
        expect(calls.map((call) => call.subject)).toContain('reporting.v1.configurations.delete');
    });

    it('saves the version it was read at and answers 409 for a stale one', async () => {
        const { server, sessionId, calls } = buildTestServer({
            ...reads,
            'analytics.v1.pricing_engines_documents.put': {
                success: false,
                message: 'version_conflict: expected 3, found 4',
                id: '',
            },
        });
        const response = await send(
            server,
            sessionId,
            'PUT',
            `/api/reporting/pricing-engines/${CONFIG_ID}`,
            {
                ...body,
                version: 3,
            },
        );
        expect(response.statusCode).toBe(409);
        const saved = calls.find(
            (call) => call.subject === 'analytics.v1.pricing_engines_documents.put',
        )?.body as { document: { config: Record<string, unknown> } };
        expect(saved.document.config).toMatchObject({
            id: CONFIG_ID,
            version: 3,
            configuration_id: HEADER_ID,
        });
    });

    it('saves a document whose ids carry no version or variant bits', async () => {
        const stored = 'e137272c-f3b7-7647-67d9-2103be4b4596';
        const storedParameter = '3a0fd1e0-c83d-2185-2e05-7c883ca22139';
        const { server, sessionId } = buildTestServer({
            ...reads,
            'analytics.v1.pricing_engines_documents.put': {
                success: true,
                message: '',
                id: CONFIG_ID,
            },
        });
        const response = await send(
            server,
            sessionId,
            'PUT',
            `/api/reporting/pricing-engines/${stored}`,
            {
                ...body,
                version: 1,
                products: [{ ...body.products[0], id: stored }],
                parameters: [{ ...body.parameters[0], id: storedParameter, productId: stored }],
            },
        );
        expect(response.statusCode).toBe(200);
    });

    it('refuses a write without a reason', async () => {
        const { server, sessionId } = buildTestServer(reads);
        const response = await send(
            server,
            sessionId,
            'PUT',
            `/api/reporting/pricing-engines/${CONFIG_ID}`,
            {
                ...body,
                version: 3,
                intent: { reason_code: '', commentary: '' },
            },
        );
        expect(response.statusCode).toBe(400);
    });

    it('deletes the document through the header it fills', async () => {
        const { server, sessionId, calls } = buildTestServer({
            ...reads,
            'analytics.v1.pricing_engines_documents.delete': { success: true, message: '' },
        });
        const response = await send(
            server,
            sessionId,
            'DELETE',
            `/api/reporting/pricing-engines/${CONFIG_ID}`,
        );
        expect(response.statusCode).toBe(204);
        expect(
            calls.find((call) => call.subject === 'analytics.v1.pricing_engines_documents.delete')
                ?.body,
        ).toEqual({ configuration_id: HEADER_ID });
    });
});
