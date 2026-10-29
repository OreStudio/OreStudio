/** -*- mode: typescript-ts-mode; tab-width: 4; indent-tabs-mode: nil -*-
 *
 * Copyright (C) 2026 Marco Craveiro <marco.craveiro@gmail.com>
 *
 * This program is free software; you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation; either version 3 of the License, or (at your option) any later
 * version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT ANY
 * WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
 * PARTICULAR PURPOSE. See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51 Franklin
 * Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 *
 */

import { dirname, resolve } from 'node:path';
import { fileURLToPath } from 'node:url';
import { describe, expect, it } from 'vitest';
import type { OresClient } from '@ores/wire-protocol';
import type { Config } from './config.js';
import type { CurveConfigurationView } from './curve-configuration.js';
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

const ENVIRONMENT_ID = 'merry_newton';
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

const CONFIGURATION_ID = '11111111-1111-4111-8111-111111111111';

/**
 * A configuration id as the ORE bundle import writes one.
 *
 * The import mints ids whose version and variant bits are not the ones RFC 4122
 * defines, so a route that reads the address as an RFC uuid refuses the very rows
 * the import created. The page must open them.
 */
const IMPORTED_CONFIGURATION_ID = '90e6d408-aa8e-3e14-206b-d510eefb7c29';
const CONFIGURATION_ID_NONE = '22222222-2222-4222-8222-222222222222';
const PARTY_ID = '33333333-3333-4333-8333-333333333333';
const DEFINITION_ID = '44444444-4444-4444-8444-444444444444';
const EQUITY_DEFINITION_ID = '55555555-5555-4555-8555-555555555555';
const YIELD_ROW_ID = '66666666-6666-4666-8666-666666666666';
const CURVE_ID = '99999999-9999-4999-8999-999999999999';

/** One curve configuration document: a yield curve entry and an equity curve entry. */
function document(): Record<string, unknown> {
    return {
        config: {
            id: '77777777-7777-4777-8777-777777777777',
            party_id: PARTY_ID,
            version: 3,
            name: 'CurveConfiguration',
            description: 'Imported from ORE XML',
            modified_by: 'analyst',
            recorded_at: '2026-10-27 09:00:00.000000Z',
        },
        sections: [
            { section_code: 'YieldCurves' },
            { section_code: 'EquityCurves' },
            { section_code: 'CurveSettings' },
        ],
        definitions: [
            {
                id: DEFINITION_ID,
                section_code: 'YieldCurves',
                curve_id: 'USD-6M',
                description: 'USD discount curve',
                position: 1,
                version: 2,
            },
            {
                id: EQUITY_DEFINITION_ID,
                section_code: 'EquityCurves',
                curve_id: 'ACME',
                description: 'ACME forward curve',
                position: 0,
                version: 1,
            },
        ],
        yield_curves: [
            {
                id: YIELD_ROW_ID,
                curve_definition_id: DEFINITION_ID,
                currency: 'USD',
                interpolation_method: 'Linear',
                tolerance: 0.0001,
                extrapolation: null,
                has_report: true,
            },
        ],
        equity_curves: [
            {
                id: '88888888-8888-4888-8888-888888888888',
                curve_definition_id: EQUITY_DEFINITION_ID,
                currency: 'USD',
                equity_type: 'Forward',
            },
        ],
    };
}

/** The contract's yield curve leaves, two documented and one not. */
const PARAMETERS = [
    {
        schema_name: 'curveconfig.xsd',
        xpath: '/CurveConfiguration/YieldCurves/YieldCurve/Currency',
        scope: 'YieldCurves',
        name: 'Currency',
        label: 'Currency',
        help: 'The currency the curve is built in.',
        description: 'The currency the curve is built in.',
        default_text: 'None: the curve must give it.',
        format: 'A currency code.',
        introduced_in: '1.8.0.7 or earlier',
        removed_in: null,
        is_required: true,
    },
    {
        schema_name: 'curveconfig.xsd',
        xpath: '/CurveConfiguration/YieldCurves/YieldCurve/InterpolationMethod',
        scope: 'YieldCurves',
        name: 'InterpolationMethod',
        label: 'Interpolation method',
        help: 'How the curve interpolates between pillars.',
        description: 'How the curve interpolates between the pillars it bootstraps.',
        default_text: 'Linear.',
        format: 'A method ORE knows.',
        introduced_in: '1.8.0.7 or earlier',
        removed_in: null,
        is_required: false,
    },
    {
        schema_name: 'ore.xsd',
        xpath: '/ORE/Setup/Parameter[@name=',
        scope: 'Setup',
        name: 'other',
        label: 'Other',
    },
];

const LINKS = [
    {
        scope: 'YieldCurves',
        name: 'InterpolationMethod',
        value_set_code: 'curve_interpolation_method',
    },
];

type Replies = Readonly<Record<string, unknown>>;

function buildTestServer(replies: Replies): {
    readonly server: ReturnType<typeof buildServer>;
    readonly sessionId: string;
    /** Every subject the request reached, with the payload it sent. */
    readonly calls: readonly { readonly subject: string; readonly payload: unknown }[];
} {
    const sessions = createSessionStore({ ttlSeconds: 60 });
    const calls: { subject: string; payload: unknown }[] = [];
    const client = {
        async callAuthenticated(
            subject: string,
            body: unknown,
            schema: { parse: (value: unknown) => unknown },
        ): Promise<unknown> {
            calls.push({ subject, payload: body });
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
        username: 'analyst',
        email: 'analyst@acme.example',
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

function repliesFor(overrides: Replies = {}): Replies {
    return {
        'refdata.v1.curve_configuration_documents.get': {
            success: true,
            message: '',
            document: document(),
        },
        'refdata.v1.curve_configurations.list': {
            result: ok,
            configurations: [
                {
                    id: CURVE_ID,
                    configuration_id: CONFIGURATION_ID,
                    name: 'UK curve configuration',
                    description: 'The UK curves',
                    party_id: PARTY_ID,
                    version: 1,
                },
            ],
        },
        'refdata.v1.curve_definitions.list': {
            result: ok,
            curves: [
                { id: '1', curve_configuration_id: CURVE_ID, section_code: 'YieldCurves' },
                { id: '2', curve_configuration_id: CURVE_ID, section_code: 'YieldCurves' },
                { id: '3', curve_configuration_id: CURVE_ID, section_code: 'DefaultCurves' },
            ],
        },
        'refdata.v1.curve_sections.list': {
            result: ok,
            sections: [
                { code: 'EquityCurves', entry_element: 'EquityCurve', description: 'Equity.' },
                { code: 'YieldCurves', entry_element: 'YieldCurve', description: 'Rates.' },
                { code: 'CurveSettings', entry_element: 'CurveSetting', description: 'New.' },
            ],
        },
        'reporting.v1.ore_parameters.list': { result: ok, parameters: PARAMETERS },
        'reporting.v1.ore_parameter_value_sets.list': {
            result: ok,
            sets: [
                {
                    code: 'curve_interpolation_method',
                    source_kind: 'enumerated',
                    minimum: null,
                    maximum: null,
                },
            ],
        },
        'reporting.v1.ore_parameter_value_set_values.list': {
            result: ok,
            values: [
                {
                    value_set_code: 'curve_interpolation_method',
                    value: 'Linear',
                    label: 'Linear',
                    display_order: 0,
                },
                {
                    value_set_code: 'curve_interpolation_method',
                    value: 'Cubic',
                    label: 'Cubic',
                    display_order: 1,
                },
            ],
        },
        'reporting.v1.ore_parameter_value_set_catalogues.list': { result: ok, catalogues: [] },
        'reporting.v1.ore_parameter_setting_value_sets.list': {
            result: ok,
            setting_value_sets: LINKS,
        },
        ...overrides,
    };
}

async function read(
    replies: Replies,
    url = `/api/refdata/curve-configurations/${CONFIGURATION_ID}`,
): Promise<ReturnType<ReturnType<typeof buildServer>['inject']>> {
    const { server, sessionId } = buildTestServer(replies);
    return server.inject({
        method: 'GET',
        url,
        cookies: { [SESSION_COOKIE]: sessionId },
    });
}

/** A save of one leaf, as the page sends it. */
function amendment(version: number, column: string, value: string | null): Record<string, unknown> {
    return {
        version,
        reasonCode: 'common.correction',
        commentary: 'The interpolation is wrong.',
        changes: [{ entity: 'yield_curve_config', entryId: DEFINITION_ID, column, value }],
    };
}

async function amend(
    replies: Replies,
    body: Record<string, unknown>,
): Promise<{
    readonly answer: ReturnType<ReturnType<typeof buildServer>['inject']>;
    readonly calls: readonly { readonly subject: string; readonly payload: unknown }[];
}> {
    const { server, sessionId, calls } = buildTestServer(replies);
    const answer = await server.inject({
        method: 'PUT',
        url: `/api/refdata/curve-configurations/${CONFIGURATION_ID}`,
        cookies: { [SESSION_COOKIE]: sessionId },
        payload: body,
    });
    return { answer, calls };
}

describe('changing a curve configuration', () => {
    it('writes the one row it changed, with the reason, and answers no content', async () => {
        const { answer, calls } = await amend(
            repliesFor({
                'refdata.v1.curve_configuration_documents.put': {
                    success: true,
                    message: '',
                    id: '',
                },
            }),
            amendment(3, 'interpolation_method', 'Cubic'),
        );

        expect(answer.statusCode).toBe(204);
        const saved = calls.find(
            (call) => call.subject === 'refdata.v1.curve_configuration_documents.put',
        );
        expect(saved).toBeDefined();
        const payload = saved?.payload as {
            document: Record<string, unknown>;
            reason_code: string;
            commentary: string;
        };
        expect(payload.reason_code).toBe('common.correction');
        expect(payload.commentary).toBe('The interpolation is wrong.');
        // Every family is named, empty where nothing changed: the service reads
        // the document whole, and writes only the rows it is sent.
        expect(Object.keys(payload.document)).toContain('config');
        expect(Object.keys(payload.document)).toHaveLength(34);
        const empty = Object.entries(payload.document).filter(
            ([family, rows]) => family !== 'config' && family !== 'yield_curves',
        );
        expect(empty.every(([, rows]) => Array.isArray(rows) && rows.length === 0)).toBe(true);
        const rows = payload.document['yield_curves'] as Record<string, unknown>[];
        expect(rows).toHaveLength(1);
        expect(rows[0]?.['interpolation_method']).toBe('Cubic');
        expect(rows[0]?.['currency']).toBe('USD');
    });

    it("changes the curve's own description, which lives in the definitions", async () => {
        const { answer, calls } = await amend(
            repliesFor({
                'refdata.v1.curve_configuration_documents.put': {
                    success: true,
                    message: '',
                    id: '',
                },
            }),
            {
                version: 3,
                reasonCode: 'common.rectification',
                commentary: '',
                changes: [
                    {
                        entity: 'curve_definition',
                        entryId: DEFINITION_ID,
                        column: 'description',
                        value: 'USD discount curve, six months',
                    },
                ],
            },
        );

        expect(answer.statusCode).toBe(204);
        const saved = calls.find(
            (call) => call.subject === 'refdata.v1.curve_configuration_documents.put',
        );
        const document = (saved?.payload as { document: Record<string, unknown> }).document;
        const definitions = document['definitions'] as Record<string, unknown>[];
        expect(definitions).toHaveLength(2);
        expect(definitions[0]?.['description']).toBe('USD discount curve, six months');
        expect(definitions[1]?.['description']).toBe('ACME forward curve');
        expect(document['yield_curves']).toEqual([]);
    });

    it('refuses a value the leaf does not accept', async () => {
        const { answer } = await amend(
            repliesFor(),
            amendment(3, 'interpolation_method', 'Cosine'),
        );
        expect(answer.statusCode).toBe(400);
        expect(answer.json()).toMatchObject({ code: 'invalid-request' });
        expect(answer.body).toContain('must be one of Linear, Cubic');
    });

    it('refuses a change to a version that moved on, and writes nothing', async () => {
        const { answer, calls } = await amend(
            repliesFor(),
            amendment(2, 'interpolation_method', 'Cubic'),
        );
        expect(answer.statusCode).toBe(409);
        expect(answer.json()).toMatchObject({ code: 'conflict' });
        expect(calls.map((call) => call.subject)).not.toContain(
            'refdata.v1.curve_configuration_documents.put',
        );
    });
});

describe('the curve configuration a report binds', () => {
    it('lists the sections in the order the schema declares them, and a section it does not know last', async () => {
        const reply = await read(repliesFor());
        expect(reply.statusCode).toBe(200);
        const body = reply.json() as CurveConfigurationView;
        expect(body.sections.map((section) => section.code)).toEqual([
            'YieldCurves',
            'EquityCurves',
            'CurveSettings',
        ]);
        expect(body.sections.map((section) => section.mapped)).toEqual([true, true, false]);
    });

    it('draws an entry with the value of each column, its documentation and the values it accepts', async () => {
        const reply = await read(repliesFor());
        const body = reply.json() as CurveConfigurationView;
        const yieldCurves = body.sections.find((section) => section.code === 'YieldCurves');
        expect(yieldCurves?.entries).toHaveLength(1);
        const entry = yieldCurves?.entries[0];
        expect(entry?.curveId).toBe('USD-6M');
        expect(entry?.description).toBe('USD discount curve');

        const currency = entry?.parameters.find((leaf) => leaf.ore === 'Currency');
        expect(currency?.value).toBe('USD');
        expect(currency?.label).toBe('Currency');
        expect(currency?.required).toBe(true);

        const method = entry?.parameters.find((leaf) => leaf.ore === 'InterpolationMethod');
        expect(method?.value).toBe('Linear');
        expect(method?.choices.map((choice) => choice.value)).toEqual(['Linear', 'Cubic']);

        const tolerance = entry?.parameters.find((leaf) => leaf.ore === 'Tolerance');
        expect(tolerance?.value).toBe('0.0001');

        // A leaf the contract does not hold is still a parameter: the gap shows.
        expect(tolerance?.documented).toBe(false);

        // A leaf the document leaves empty has no value, and the screen shows its default.
        const extrapolation = entry?.parameters.find((leaf) => leaf.ore === 'Extrapolation');
        expect(extrapolation?.value).toBeNull();
    });

    it('names the block an entry holds and whether the entry states it', async () => {
        const reply = await read(repliesFor());
        const body = reply.json() as CurveConfigurationView;
        const entry = body.sections.find((section) => section.code === 'YieldCurves')?.entries[0];
        expect(entry?.blocks.map((block) => block.element)).toContain('Report');
        expect(entry?.blocks.find((block) => block.element === 'Report')?.present).toBe(true);
        expect(entry?.blocks.find((block) => block.element === 'Segments')?.present).toBeNull();
    });

    it('counts the parameters the contract documents against the ones the entries carry', async () => {
        const reply = await read(repliesFor());
        const body = reply.json() as CurveConfigurationView;
        const entry = body.sections.find((section) => section.code === 'YieldCurves')?.entries[0];
        // The entry's own columns, in the order the projection reads them, and no
        // column for a block the entry holds: Report is a child, not a parameter.
        expect(entry?.parameters.map((leaf) => leaf.ore)).toEqual([
            'CurveId',
            'CurveDescription',
            'Currency',
            'DiscountCurve',
            'InterpolationVariable',
            'InterpolationMethod',
            'MixedInterpolationCutoff',
            'YieldCurveDayCounter',
            'Tolerance',
            'Extrapolation',
            'ExtrapolationMethod',
            'ExcludeT0FromInterpolation',
        ]);
        expect(body.documented).toBe(2);
        expect(body.total).toBe(12 + 11);
    });

    it('opens a configuration whose id the ORE import minted, which is not an RFC uuid', async () => {
        const reply = await read(
            repliesFor(),
            `/api/refdata/curve-configurations/${IMPORTED_CONFIGURATION_ID}`,
        );
        expect(reply.statusCode).toBe(200);
        const body = reply.json() as CurveConfigurationView;
        expect(body.configurationId).toBe(IMPORTED_CONFIGURATION_ID);
    });

    it('lists a configuration without reading its document, counting its sections and curves', async () => {
        const reply = await read(repliesFor(), '/api/refdata/curve-configurations');
        expect(reply.statusCode).toBe(200);
        const body = reply.json() as {
            rows: readonly {
                configurationId: string;
                name: string;
                partyName: string;
                sections: number;
                curves: number;
            }[];
        };
        expect(body.rows).toHaveLength(1);
        expect(body.rows[0]).toMatchObject({
            configurationId: CONFIGURATION_ID,
            name: 'UK curve configuration',
            sections: 2,
            curves: 3,
        });
    });

    it('answers 404 when no document fills the configuration', async () => {
        const reply = await read(
            repliesFor({
                'refdata.v1.curve_configuration_documents.get': {
                    success: false,
                    message: `No curve configuration document fills configuration ${CONFIGURATION_ID_NONE}.`,
                    document: {},
                },
            }),
            `/api/refdata/curve-configurations/${CONFIGURATION_ID_NONE}`,
        );
        expect(reply.statusCode).toBe(404);
    });
});
