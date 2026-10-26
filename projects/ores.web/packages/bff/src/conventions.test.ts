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
import { CONVENTION_FAMILY_ENTRIES } from '@ores/wire-protocol/generated/refdata/domain/convention_family_registry';
import type { Config } from './config.js';
import { readFileSync } from 'node:fs';
import { ABSORBED_REGISTRY_KEYS, CONVENTION_FAMILIES, kindCodeOf } from './conventions.js';
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
const PR_ID = '66666666-6666-4666-8666-666666666666';
const LS_ID = '77777777-7777-4777-8777-777777777777';
const PR_PART_ID = '88888888-8888-4888-8888-888888888888';
const LS_PART_ID = '99999999-9999-4999-8999-999999999999';
const INTENT = { reason_code: 'common.correction', commentary: '' };

describe('the family list', () => {
    it('lists every registry family the screen does not absorb, so a new family needs no edit', () => {
        const listed = new Set(CONVENTION_FAMILIES.map((family) => family.entity));
        for (const entry of CONVENTION_FAMILY_ENTRIES) {
            if (!ABSORBED_REGISTRY_KEYS.has(entry.key)) {
                expect(listed.has(entry.entity), entry.key).toBe(true);
            }
        }
        expect(CONVENTION_FAMILIES.map((family) => family.key)).toContain('zero-inflation-index');
    });

    it('lists the composite and the screen-only kinds, and nothing else beyond the registry', () => {
        const registryEntities = new Set(CONVENTION_FAMILY_ENTRIES.map((entry) => entry.entity));
        const extra = CONVENTION_FAMILIES.filter(
            (family) => !registryEntities.has(family.entity),
        ).map((family) => family.key);
        expect(extra.sort()).toEqual(['currency-pair', 'tenor', 'tenor-basis-swap'].sort());
    });

    it('addresses each registry family at the subject stem the registry states', () => {
        for (const entry of CONVENTION_FAMILY_ENTRIES) {
            const family = CONVENTION_FAMILIES.find(
                (candidate) => candidate.entity === entry.entity,
            );
            if (family !== undefined) {
                expect(`refdata.v1.${family.prefix}`, entry.key).toBe(entry.subjectStem);
            }
        }
    });

    it('absorbs only keys the registry states, so a mistyped key cannot hide nothing', () => {
        const registryKeys = new Set(CONVENTION_FAMILY_ENTRIES.map((entry) => entry.key));
        for (const key of ABSORBED_REGISTRY_KEYS) {
            expect(registryKeys.has(key), key).toBe(true);
        }
    });

    it('names each family once', () => {
        const keys = CONVENTION_FAMILIES.map((family) => family.key);
        expect(new Set(keys).size).toBe(keys.length);
    });
});

describe('the families and their kinds', () => {
    // The kinds the database seeds, read from the populate script the families route reads them from.
    const seeded = new Set(
        [
            ...readFileSync(
                resolve(
                    dirname(fileURLToPath(import.meta.url)),
                    '../../../../ores.sql/populate/refdata/refdata_convention_kinds_populate.sql',
                ),
                'utf-8',
            ).matchAll(/\(ores_utility_system_tenant_id_fn\(\), '([A-Z_]+)', 0,/g),
        ].map((match) => match[1]),
    );

    it('names a seeded kind for every family, so every card can take its class', () => {
        expect(seeded.size).toBeGreaterThan(0);
        expect(
            CONVENTION_FAMILIES.map((family) => kindCodeOf(family.key)).filter(
                (code) => !seeded.has(code),
            ),
        ).toEqual([]);
    });

    it('states a family as shared only where the registry does not, so the two cannot disagree', () => {
        const registry = new Set(
            CONVENTION_FAMILY_ENTRIES.map((entry) => entry.key.replaceAll('_', '-')),
        );
        expect(
            ['tenor', 'currency-pair', 'tenor-basis-swap'].filter((key) => registry.has(key)),
        ).toEqual([]);
    });
});

describe('convention routes', () => {
    it('answers one convention with its kind and its terms, each documented from the contract', async () => {
        const replies: Record<string, unknown> = {
            'refdata.v1.swap_conventions.list': {
                result: ok,
                swap_conventions: [
                    {
                        id: 'EUR-6M-SWAP',
                        version: 3,
                        fixed_calendar: 'TARGET',
                        fixed_frequency: 'A',
                        index: 'EUR-EURIBOR-6M',
                    },
                ],
            },
            'refdata.v1.convention_kinds.list': {
                result: ok,
                kinds: [
                    {
                        code: 'SWAP',
                        name: 'Swap',
                        description: 'Vanilla swap',
                        asset_class: 'interest-rates',
                        ore_element: 'Swap',
                    },
                ],
            },
            'refdata.v1.convention_kind_terms.list': {
                result: ok,
                terms: [
                    {
                        kind_code: 'SWAP',
                        entity_name: 'swap_convention',
                        column_name: 'fixed_frequency',
                        schema_name: 'conventions.xsd',
                        xpath: '/Conventions/Swap/FixedFrequency',
                        position: 1,
                    },
                    {
                        kind_code: 'SWAP',
                        entity_name: 'swap_convention',
                        column_name: 'fixed_calendar',
                        schema_name: 'conventions.xsd',
                        xpath: '/Conventions/Swap/FixedCalendar',
                        position: 0,
                    },
                    {
                        kind_code: 'OIS',
                        entity_name: 'ois_convention',
                        column_name: 'index',
                        schema_name: 'conventions.xsd',
                        xpath: '/Conventions/OIS/Index',
                        position: 0,
                    },
                ],
            },
            'reporting.v1.ore_parameters.list': {
                result: ok,
                parameters: [
                    {
                        schema_name: 'conventions.xsd',
                        xpath: '/Conventions/Swap/FixedFrequency',
                        scope: 'Swap',
                        name: 'FixedFrequency',
                        label: 'Fixed frequency',
                        help: 'The frequency of payments on the fixed leg.',
                        description: 'The frequency of payments on the fixed leg.',
                        default_text: 'None: the convention must give it.',
                        format: 'A frequency, such as Annual or Quarterly.',
                        introduced_in: '1.8.0.7 or earlier',
                        removed_in: '',
                        is_required: true,
                    },
                ],
            },
            'reporting.v1.ore_parameter_value_sets.list': {
                result: ok,
                sets: [{ code: 'convention.frequency', source_kind: 'enumerated' }],
            },
            'reporting.v1.ore_parameter_value_set_values.list': {
                result: ok,
                values: [
                    {
                        value_set_code: 'convention.frequency',
                        value: 'A',
                        label: 'Annual',
                        display_order: 0,
                    },
                    {
                        value_set_code: 'convention.frequency',
                        value: 'Q',
                        label: 'Quarterly',
                        display_order: 1,
                    },
                ],
            },
            'reporting.v1.ore_parameter_value_set_catalogues.list': { result: ok, catalogues: [] },
            'reporting.v1.ore_parameter_setting_value_sets.list': {
                result: ok,
                setting_value_sets: [
                    {
                        scope: 'Swap',
                        name: 'FixedFrequency',
                        value_set_code: 'convention.frequency',
                    },
                ],
            },
        };
        const { server, sessionId } = buildTestServer(replies);
        const response = await send(
            server,
            sessionId,
            'GET',
            '/api/conventions/swap/documents/EUR-6M-SWAP',
        );
        expect(response.statusCode).toBe(200);
        const body = response.json();
        expect(body.kind).toMatchObject({
            code: 'SWAP',
            assetClass: 'interest-rates',
            oreElement: 'Swap',
        });
        expect(body.version).toBe(3);
        // The kind's own terms only, in the position order the data states.
        expect(body.terms.map((term: { column: string }) => term.column)).toEqual([
            'fixed_calendar',
            'fixed_frequency',
        ]);
        const [calendar, frequency] = body.terms;
        // A term the contract holds no entry for is still drawn, marked undocumented.
        expect(calendar).toMatchObject({
            value: 'TARGET',
            documented: false,
            ore: 'FixedCalendar',
            choices: [],
        });
        expect(frequency).toMatchObject({
            value: 'A',
            documented: true,
            required: true,
            label: 'Fixed frequency',
            sourceKind: 'enumerated',
            choices: [
                { value: 'A', label: 'Annual' },
                { value: 'Q', label: 'Quarterly' },
            ],
        });
    });

    it('answers 404 for a convention the family does not hold', async () => {
        const { server, sessionId } = buildTestServer({
            'refdata.v1.swap_conventions.list': { result: ok, swap_conventions: [] },
        });
        const response = await send(
            server,
            sessionId,
            'GET',
            '/api/conventions/swap/documents/NOPE',
        );
        expect(response.statusCode).toBe(404);
    });

    it("takes each family's asset class from its kind and its holder from the registry", async () => {
        const replies: Record<string, unknown> = {};
        for (const family of CONVENTION_FAMILIES) {
            replies[`refdata.v1.${family.prefix}.list`] = { result: ok, total: 1 };
        }
        replies['refdata.v1.conventions.list'] = { result: ok, total: 1 };
        replies['refdata.v1.convention_kinds.list'] = {
            result: ok,
            kinds: [
                { code: 'SWAP', asset_class: 'interest-rates' },
                { code: 'TENOR_BASIS_SWAP', asset_class: 'interest-rates' },
                { code: 'CDS', asset_class: 'credit' },
            ],
        };
        const { server, sessionId } = buildTestServer(replies);
        const response = await send(server, sessionId, 'GET', '/api/conventions/families');
        const families: { key: string; assetClass: string | null; shared: boolean }[] =
            response.json().families;
        const card = (key: string) => families.find((f) => f.key === key);
        expect(card('swap')?.assetClass).toBe('interest-rates');
        expect(card('tenor-basis-swap')?.assetClass).toBe('interest-rates');
        expect(card('cds')?.assetClass).toBe('credit');
        // A kind the data gives no class to has none, and the screen groups it under Other.
        expect(card('fra')?.assetClass).toBeNull();
        expect(card('ibor-index')?.shared).toBe(true);
        expect(card('tenor')?.shared).toBe(true);
        expect(card('swap')?.shared).toBe(false);
        expect(card('tenor-basis-swap')?.shared).toBe(false);
    });

    it('counts every family, and reads a count it cannot as null', async () => {
        const replies: Record<string, unknown> = {};
        for (const family of CONVENTION_FAMILIES) {
            replies[`refdata.v1.${family.prefix}.list`] = { result: ok, total: 2 };
        }
        // A composite family has no list of its own: its count is the central
        // convention list, filtered by the kind the family carries.
        replies['refdata.v1.conventions.list'] = { result: ok, total: 4 };
        delete replies['refdata.v1.future_conventions.list'];
        replies['refdata.v1.fra_conventions.list'] = {
            result: { outcome: 'denied', code: '', message: 'No.' },
            total: 0,
        };
        const { server, sessionId, calls } = buildTestServer(replies);
        const response = await send(server, sessionId, 'GET', '/api/conventions/families');
        const families = response.json().families;
        expect(families).toHaveLength(CONVENTION_FAMILIES.length);
        expect(families.find((f: { key: string }) => f.key === 'swap').count).toBe(2);
        expect(families.find((f: { key: string }) => f.key === 'tenor-basis-swap').count).toBe(4);
        expect(families.find((f: { key: string }) => f.key === 'fra').count).toBeNull();
        // A call that throws leaves its own count null and the others intact.
        expect(families.find((f: { key: string }) => f.key === 'future').count).toBeNull();
        expect(
            families
                .filter((f: { writable: boolean }) => f.writable)
                .map((f: { key: string }) => f.key),
        ).toEqual(['deposit', 'swap']);
        // The composite count is scoped by kind, so a second atomised kind does
        // not inflate the first family's count.
        expect(
            calls.find((call) => call.subject === 'refdata.v1.conventions.list')?.body,
        ).toMatchObject({ filter: { kind_code: 'TENOR_BASIS_SWAP' } });
    });

    it('lists the rows of one family under its own subject', async () => {
        const { server, sessionId, calls } = buildTestServer({
            'refdata.v1.swap_conventions.list': {
                result: ok,
                total: 1,
                swap_conventions: [{ id: ID, version: 3, index: 'USD-LIBOR-3M' }],
            },
        });
        const response = await send(server, sessionId, 'GET', '/api/conventions/swap');
        expect(response.json().rows[0].index).toBe('USD-LIBOR-3M');
        expect(response.json().total).toBe(1);
        expect(calls[0]?.subject).toBe('refdata.v1.swap_conventions.list');
    });

    it('refuses a family it does not know', async () => {
        const { server, sessionId } = buildTestServer({});
        const response = await send(server, sessionId, 'GET', '/api/conventions/nonsense');
        expect(response.statusCode).toBe(404);
    });

    it('reads a composite family as one row per convention, drawn from whichever part is populated', async () => {
        const { server, sessionId, calls } = buildTestServer({
            'refdata.v1.conventions.list': {
                result: ok,
                total: 2,
                conventions: [
                    {
                        id: PR_ID,
                        version: 4,
                        kind_code: 'TENOR_BASIS_SWAP',
                        name: 'EUR-3M-6M-BASIS',
                        oresmd_uri: null,
                    },
                    {
                        id: LS_ID,
                        version: 7,
                        kind_code: 'TENOR_BASIS_SWAP',
                        name: 'EUR-6M-12M-BASIS',
                        oresmd_uri: null,
                    },
                ],
            },
            'refdata.v1.tenor_basis_swap_conventions.get_many': {
                result: ok,
                items: [
                    {
                        key: { id: PR_ID },
                        convention: { id: PR_ID, name: 'EUR-3M-6M-BASIS' },
                        pay_receive: [
                            {
                                id: PR_PART_ID,
                                version: 2,
                                convention_id: PR_ID,
                                pay_index: 'EUR-EURIBOR-6M',
                                receive_index: 'EUR-EURIBOR-3M',
                                include_spread: true,
                                sub_periods_coupon_type: null,
                            },
                        ],
                        long_short: [],
                    },
                    {
                        key: { id: LS_ID },
                        convention: { id: LS_ID, name: 'EUR-6M-12M-BASIS' },
                        pay_receive: [],
                        long_short: [
                            {
                                id: LS_PART_ID,
                                version: 9,
                                convention_id: LS_ID,
                                long_index: 'EUR-EURIBOR-12M',
                                short_index: 'EUR-EURIBOR-6M',
                                spread_on_short: false,
                                sub_periods_coupon_type: 'Compounding',
                            },
                        ],
                    },
                ],
            },
        });
        const response = await send(server, sessionId, 'GET', '/api/conventions/tenor-basis-swap');
        const body = response.json();
        expect(body.total).toBe(2);
        expect(body.rows).toHaveLength(2);
        // The central row keeps its own identity, version and name.
        expect(body.rows[0]).toMatchObject({
            id: PR_ID,
            version: 4,
            name: 'EUR-3M-6M-BASIS',
            pay_index: 'EUR-EURIBOR-6M',
            receive_index: 'EUR-EURIBOR-3M',
            include_spread: true,
        });
        // The other convention is drawn from the other part.
        expect(body.rows[1]).toMatchObject({
            id: LS_ID,
            version: 7,
            long_index: 'EUR-EURIBOR-12M',
            short_index: 'EUR-EURIBOR-6M',
            spread_on_short: false,
            sub_periods_coupon_type: 'Compounding',
        });
        // The part's own version never replaces the central row's version.
        expect(body.rows[0].version).toBe(4);
        expect(body.rows[1].version).toBe(7);
        // The page is the server's: the central list is filtered by kind, and
        // the parts are read for exactly that page's keys, in its order.
        expect(calls[0]?.subject).toBe('refdata.v1.conventions.list');
        expect(calls[0]?.body).toMatchObject({ filter: { kind_code: 'TENOR_BASIS_SWAP' } });
        expect(calls[1]?.subject).toBe('refdata.v1.tenor_basis_swap_conventions.get_many');
        expect(calls[1]?.body).toMatchObject({ keys: [{ id: PR_ID }, { id: LS_ID }] });
        // The flat subject no longer exists, so a screen that reads it is a
        // runtime break rather than a compile error.
        expect(calls.map((call) => call.subject)).not.toContain(
            'refdata.v1.tenor_basis_swap_conventions.list',
        );
    });

    it('reads the pick lists the terms draw from', async () => {
        const rows = (field: string): unknown => ({
            result: ok,
            [field]: [{ code: 'A', version: 1 }],
        });
        const { server, sessionId } = buildTestServer({
            'refdata.v1.calendar_names.list': rows('calendar_names'),
            'refdata.v1.business_day_convention_types.list': rows('types'),
            'refdata.v1.day_count_fraction_types.list': rows('types'),
            'refdata.v1.floating_index_types.list': rows('types'),
            'refdata.v1.payment_frequencies.list': rows('payment_frequencies'),
            'refdata.v1.sub_periods_coupon_types.list': rows('types'),
            'refdata.v1.currencies.list': rows('currencies'),
        });
        const response = await send(server, sessionId, 'GET', '/api/conventions/pick-lists');
        expect(Object.keys(response.json()).sort()).toEqual([
            'businessDayConventions',
            'calendars',
            'currencies',
            'dayCountFractions',
            'floatingIndices',
            'paymentFrequencies',
            'subPeriodsCouponTypes',
        ]);
    });

    it('writes a drawn family as new or against its version, and returns the refusal', async () => {
        const { server, sessionId, calls } = buildTestServer({
            'refdata.v1.swap_conventions.put': {
                result: { outcome: 'conflict', code: 'stale_version', message: 'Stale.' },
                swap_convention: null,
            },
        });
        const response = await send(server, sessionId, 'PUT', '/api/conventions/swap', {
            intent: INTENT,
            version: 3,
            write: { id: ID, index: 'X' },
        });
        expect(response.json().result.code).toBe('stale_version');
        expect(calls[0]?.body).toMatchObject({
            change: { precondition: { kind: 'must_match_version', version: 3 } },
        });
    });

    it('refuses a write to a family whose terms are not drawn', async () => {
        const { server, sessionId, calls } = buildTestServer({});
        const response = await send(server, sessionId, 'PUT', '/api/conventions/fra', {
            intent: INTENT,
            version: null,
            write: { id: ID },
        });
        expect(response.statusCode).toBe(400);
        expect(calls).toHaveLength(0);
    });
});
