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

import type { FastifyInstance, FastifyRequest } from 'fastify';
import { z } from 'zod';
import {
    REFDATA_RECORDS,
    listRecordPage,
    listRecords,
    readRecord,
    readCalendarYear,
    rebuildCalendar,
    recordResource,
    resourceName,
    removeRecord,
    saveRecord,
    type RecordResource,
    type WriteOutcome,
} from '@ores/wire-protocol';
import { subjects as oreCurrencySubjects } from '@ores/wire-protocol/generated/ore/protocol/currency_config_protocol';
import type { CalendarWrite } from '@ores/wire-protocol/generated/refdata/protocol/calendar_protocol';
import type { CalendarEventWrite } from '@ores/wire-protocol/generated/refdata/protocol/calendar_event_protocol';
import type { CalendarExceptionWrite } from '@ores/wire-protocol/generated/refdata/protocol/calendar_exception_protocol';
import type { CalendarRuleWrite } from '@ores/wire-protocol/generated/refdata/protocol/calendar_rule_protocol';
import type { CurrencyWrite } from '@ores/wire-protocol/generated/refdata/protocol/currency_protocol';
import type { CurrencyCalendarWrite } from '@ores/wire-protocol/generated/refdata/protocol/currency_calendar_protocol';
import type { CurrencyCountryWrite } from '@ores/wire-protocol/generated/refdata/protocol/currency_country_protocol';
import type { CurrencyCurrencyGroupWrite } from '@ores/wire-protocol/generated/refdata/protocol/currency_currency_group_protocol';
import type { CurrencyGroupWrite } from '@ores/wire-protocol/generated/refdata/protocol/currency_group_protocol';
import type { CurrencyPairWrite } from '@ores/wire-protocol/generated/refdata/protocol/currency_pair_protocol';
import type { CurrencyPairConventionWrite } from '@ores/wire-protocol/generated/refdata/protocol/currency_pair_convention_protocol';
import type { CurrencyPairConventionCalendarWrite } from '@ores/wire-protocol/generated/refdata/protocol/currency_pair_convention_calendar_protocol';
import type { PartyWrite } from '@ores/wire-protocol/generated/refdata/protocol/party_protocol';
import type { PartyIdentifierWrite } from '@ores/wire-protocol/generated/refdata/protocol/party_identifier_protocol';
import type { PartyContactInformationWrite } from '@ores/wire-protocol/generated/refdata/protocol/party_contact_information_protocol';
import type { PartyCountryWrite } from '@ores/wire-protocol/generated/refdata/protocol/party_country_protocol';
import type { PartyCurrencyWrite } from '@ores/wire-protocol/generated/refdata/protocol/party_currency_protocol';
import type { PartyCounterpartyWrite } from '@ores/wire-protocol/generated/refdata/protocol/party_counterparty_protocol';
import type { BusinessUnitWrite } from '@ores/wire-protocol/generated/refdata/protocol/business_unit_protocol';
import type { IrCurveBootstrapConfigWrite } from '@ores/wire-protocol/generated/refdata/protocol/ir_curve_bootstrap_config_protocol';
import type { IrCurveBootstrapPillarWrite } from '@ores/wire-protocol/generated/refdata/protocol/ir_curve_bootstrap_pillar_protocol';
import { HttpFailure, invalidRequest, notFound, notPermitted } from './errors.js';
import { pageQuerySchema } from './refdata-calls.js';
import type { LiveSession } from './sessions.js';

const code = z.string().trim().min(1).max(100);
const isoCode = z.string().trim().length(3);
const pairCode = z.string().trim().min(7).max(15);
const text = z.string().max(2000);
const count = z.int().min(0).max(1_000_000);
const isoDay = z.iso.date();
const calendarYear = z.int().min(1900).max(2200);

const RULE_KINDS = [
    'fixed_date',
    'nth_weekday_of_month',
    'last_weekday_of_month',
    'easter_offset',
] as const;

const RULE_FIELDS = ['month', 'day', 'weekday', 'occurrence', 'day_offset'] as const;

/** The fields each rule kind is made of; the rule engine ignores the others. */
const RULE_NEEDS: Readonly<
    Record<(typeof RULE_KINDS)[number], readonly (typeof RULE_FIELDS)[number][]>
> = {
    fixed_date: ['month', 'day'],
    nth_weekday_of_month: ['month', 'weekday', 'occurrence'],
    last_weekday_of_month: ['month', 'weekday'],
    easter_offset: ['day_offset'],
};

/**
 * Rules and exceptions make a calendar's business days, so the BFF writes them
 * only to a calendar that is editable. The server does not check this yet, and
 * a calendar that stops being editable between this read and the write is not
 * caught; the server's own check is the real fix.
 */
const EDITABLE_PARENT = new Set(['calendar-rules', 'calendar-exceptions']);

async function refuseReadOnlyCalendar(session: LiveSession, calendarCode: string): Promise<void> {
    const calendars = recordResource('calendars');
    const calendar =
        calendars === undefined
            ? undefined
            : await readRecord(session.client, calendars, calendarCode);
    if (calendar === undefined) {
        throw invalidRequest(`There is no calendar ${calendarCode}.`);
    }
    if (calendar['is_editable'] !== true) {
        throw notPermitted(
            `${calendarCode} takes its holidays from QuantLib and is read only. Derive a calendar to change them.`,
        );
    }
}

/**
 * What each writable resource accepts, in the server's own field names.
 *
 * Each schema satisfies the resource's generated write type, so a model change
 * the schema misses fails the typecheck rather than the request.
 */
const WRITES: Readonly<Record<string, z.ZodType<Record<string, unknown>>>> = {
    currencies: z.object({
        iso_code: isoCode,
        name: text.min(1),
        numeric_code: z.string().regex(/^([0-9]{3})?$/),
        symbol: z.string().max(20),
        fraction_symbol: z.string().max(20),
        fractions_per_unit: count,
        rounding_type: code,
        rounding_precision: z.int().min(0).max(20),
        format: z.string().max(100),
        monetary_nature: code,
        market_tier: code,
        ore_currency_type: z.string().max(100).nullable(),
        image_id: z.uuid().nullable(),
        spot_days: z.int().min(0).max(10),
        day_basis: z.string().max(20),
        base_precedence: count,
    }) satisfies z.ZodType<CurrencyWrite>,
    'currency-groups': z.object({
        code,
        name: text.min(1),
        description: text,
        display_order: count,
    }) satisfies z.ZodType<CurrencyGroupWrite>,
    'currency-countries': z.object({
        currency_iso_code: isoCode,
        country_alpha2_code: code,
    }) satisfies z.ZodType<CurrencyCountryWrite>,
    'currency-calendars': z.object({
        currency_iso_code: isoCode,
        calendar_code: code,
    }) satisfies z.ZodType<CurrencyCalendarWrite>,
    'currency-memberships': z.object({
        currency_iso_code: isoCode,
        currency_group_code: code,
    }) satisfies z.ZodType<CurrencyCurrencyGroupWrite>,
    'currency-pairs': z
        .object({
            pair_code: pairCode,
            base_currency: isoCode,
            quote_currency: isoCode,
            classification: code,
        })
        .refine((pair) => pair.base_currency !== pair.quote_currency, {
            message: 'The two legs of a pair differ.',
        })
        .refine((pair) => pair.pair_code === `${pair.base_currency}/${pair.quote_currency}`, {
            message: 'A pair code is its base and quote currencies, as BASE/QUOTE.',
        }) satisfies z.ZodType<CurrencyPairWrite>,
    'currency-pair-conventions': z.object({
        pair_code: pairCode,
        pip_factor: z.number().positive(),
        tick_size: z.number().positive(),
        decimal_places: z.int().min(0).max(12),
        business_day_convention: code.nullable(),
        spot_relative: z.boolean().nullable(),
        end_of_month: z.boolean().nullable(),
        oresmd_uri: text.nullable(),
    }) satisfies z.ZodType<CurrencyPairConventionWrite>,
    'pair-calendars': z.object({
        pair_code: pairCode,
        calendar_code: code,
    }) satisfies z.ZodType<CurrencyPairConventionCalendarWrite>,
    calendars: z.object({
        code,
        name: text.min(1),
        calendar_type: code,
        country_code: code,
        image_id: z.uuid().nullable(),
        source: z.literal('user'),
        is_editable: z.literal(true),
        base_calendar_code: code.nullable(),
    }) satisfies z.ZodType<CalendarWrite>,
    'calendar-rules': z
        .object({
            id: z.uuid(),
            calendar_code: code,
            kind: z.enum(RULE_KINDS),
            month: z.int().min(1).max(12).nullable(),
            day: z.int().min(1).max(31).nullable(),
            weekday: z.int().min(0).max(6).nullable(),
            occurrence: z.int().min(1).max(4).nullable(),
            day_offset: z.int().min(-366).max(366).nullable(),
            shift: z.enum(['none', 'nearest_weekday', 'roll_forward_to_monday']),
            effective_from: calendarYear.nullable(),
            effective_to: calendarYear.nullable(),
        })
        .superRefine((rule, context) => {
            const needs = RULE_NEEDS[rule.kind];
            const missing = needs.filter((field) => rule[field] === null);
            const extra = RULE_FIELDS.filter(
                (field) => !needs.includes(field) && rule[field] !== null,
            );
            if (missing.length > 0 || extra.length > 0) {
                context.addIssue({
                    code: 'custom',
                    message: `A ${rule.kind} rule needs ${needs.join(', ')}, and no other of ${RULE_FIELDS.join(', ')}.`,
                });
            }
            if (
                rule.effective_from !== null &&
                rule.effective_to !== null &&
                rule.effective_from > rule.effective_to
            ) {
                context.addIssue({
                    code: 'custom',
                    message: "A rule's first year is not after its last year.",
                });
            }
        }) satisfies z.ZodType<CalendarRuleWrite>,
    'calendar-exceptions': z.object({
        id: z.uuid(),
        calendar_code: code,
        exception_date: isoDay,
        is_business_day: z.boolean(),
        description: text.nullable(),
    }) satisfies z.ZodType<CalendarExceptionWrite>,
    'calendar-events': z.object({
        id: z.uuid(),
        calendar_code: code,
        event_date: isoDay,
        diary_entry_type: code,
        name: text.min(1),
        description: text.nullable(),
        source: text.nullable(),
    }) satisfies z.ZodType<CalendarEventWrite>,
    parties: z.object({
        id: z.uuid(),
        short_code: code,
        full_name: text.min(1),
        codename: code,
        transliterated_name: text.nullable(),
        party_category: code,
        party_type: code,
        parent_party_id: z.uuid().nullable(),
        business_center_code: code,
        status: code,
        image_id: z.uuid().nullable(),
        is_registration_default: z.boolean(),
    }) satisfies z.ZodType<PartyWrite>,
    'party-identifiers': z.object({
        id: z.uuid(),
        party_id: z.uuid(),
        id_scheme: code,
        id_value: text.min(1),
        description: text,
    }) satisfies z.ZodType<PartyIdentifierWrite>,
    'party-contacts': z.object({
        id: z.uuid(),
        party_id: z.uuid(),
        contact_type: code,
        street_line_1: text,
        street_line_2: text,
        city: text,
        state: text,
        country_code: z.string().max(3),
        postal_code: z.string().max(20),
        phone: z.string().max(50),
        email: z.string().max(254),
        web_page: z.string().max(500),
        is_primary: z.boolean(),
    }) satisfies z.ZodType<PartyContactInformationWrite>,
    'party-countries': z.object({
        party_id: z.uuid(),
        country_alpha2_code: code,
    }) satisfies z.ZodType<PartyCountryWrite>,
    'party-currencies': z.object({
        party_id: z.uuid(),
        currency_iso_code: isoCode,
    }) satisfies z.ZodType<PartyCurrencyWrite>,
    'party-counterparties': z.object({
        party_id: z.uuid(),
        counterparty_id: z.uuid(),
    }) satisfies z.ZodType<PartyCounterpartyWrite>,
    'business-units': z.object({
        id: z.uuid(),
        party_id: z.uuid(),
        unit_name: text.min(1),
        parent_business_unit_id: z.uuid().nullable(),
        unit_code: code,
        business_centre_code: z.string().max(100),
        unit_type_id: z.uuid().nullable(),
        status: code,
    }) satisfies z.ZodType<BusinessUnitWrite>,
    'ir-curve-bootstrap-configs': z.object({
        id: z.uuid(),
        output_series_id: z.uuid(),
        currency_code: isoCode,
        index_tenor: code,
        curve_family_role: z.enum(['FUNDING', 'PROJECTION']),
        discount_curve_config_id: z.uuid(),
        interpolation_method: z.enum([
            'LOG_LINEAR_DISCOUNT',
            'FLAT_FORWARD_THEN_LOG_LINEAR',
            'CUBIC_SPLINE',
        ]),
        day_count_convention: code,
        tenor_convention_code: code,
        split_tenor_code: code,
    }) satisfies z.ZodType<IrCurveBootstrapConfigWrite>,
    'ir-curve-bootstrap-pillars': z.object({
        id: z.uuid(),
        party_id: z.uuid(),
        bootstrap_config_id: z.uuid(),
        sequence_index: z.int().min(0).max(10_000),
        start_tenor_code: code,
        end_tenor_code: code,
        curve_role_code: z.enum(['DEPOSIT', 'FRA', 'SWAP']),
    }) satisfies z.ZodType<IrCurveBootstrapPillarWrite>,
};

const intentSchema = z.object({
    reasonCode: z.string().trim().min(1).max(200),
    commentary: z.string().max(2000).default(''),
});

const saveBodySchema = intentSchema.extend({
    write: z.record(z.string(), z.unknown()),
    version: z.int().nonnegative().nullable(),
});

const removeBodySchema = intentSchema.extend({
    key: z.record(z.string(), code),
    version: z.int().nonnegative().nullable().default(null),
});

/**
 * The resource the address names, or a 404.
 *
 * The browser names a resource by its key, never by a subject: the registry
 * maps the key to the subjects, and a key the registry lacks reaches nothing.
 */
function resourceFor(request: FastifyRequest): RecordResource {
    const { resource } = request.params as { resource: string };
    const found = recordResource(resource);
    if (found === undefined) {
        throw notFound(`There is no reference data resource ${resource}.`);
    }
    return found;
}

/** The resource the address names, refused when it is read only. */
function writableFor(request: FastifyRequest): RecordResource {
    const resource = resourceFor(request);
    if (!resource.writable) {
        throw notPermitted(`${resource.key} is read only here.`);
    }
    return resource;
}

/**
 * The filters a page states, each named in the address as `f.<member>`. A
 * member the resource does not declare filterable is refused rather than
 * ignored, because a filter that narrowed nothing would read as an answer.
 */
function filtersOf(
    resource: RecordResource,
    query: Readonly<Record<string, unknown>>,
): Readonly<Record<string, string>> {
    const named = resource.filters ?? [];
    const set: Record<string, string> = {};
    for (const [key, value] of Object.entries(query)) {
        if (!key.startsWith('f.')) {
            continue;
        }
        const member = key.slice('f.'.length);
        if (!named.includes(member)) {
            throw invalidRequest(`${resource.key} cannot be filtered by ${member}.`);
        }
        if (typeof value !== 'string') {
            throw invalidRequest(`The filter ${member} takes one value.`);
        }
        set[member] = value;
    }
    return set;
}

/**
 * Turns a refused write into the status that says why. The server sends no
 * words with some refusals, so a conflict says what it means.
 */
function answer(outcome: WriteOutcome): void {
    if (outcome.done) {
        return;
    }
    switch (outcome.outcome) {
        case 'conflict':
            throw new HttpFailure(409, {
                code: 'conflict',
                message:
                    outcome.message === ''
                        ? 'This record changed since it was read, or it already exists. Reload and try again.'
                        : outcome.message,
            });
        case 'invalid':
            throw invalidRequest(outcome.message);
        case 'denied':
            throw notPermitted(outcome.message);
        case 'missing':
            throw notFound(outcome.message);
        default:
            throw new HttpFailure(502, { code: 'upstream-unavailable', message: outcome.message });
    }
}

/**
 * The reference data record routes: a resource's rows, one parent's rows of a
 * junction, and the writes. The registry decides which resources exist.
 */
const oreDocumentSchema = z.object({
    success: z.boolean(),
    message: z.string(),
    count: z.number().int(),
    files: z.array(z.object({ name: z.string(), content: z.string() })),
});

const orePreviewSchema = z.object({
    success: z.boolean(),
    message: z.string(),
    root_element: z.string(),
    in_file: z.number().int(),
    held: z.number().int(),
    changed: z.number().int(),
    added: z.number().int(),
    removed: z.number().int(),
    rows: z.array(
        z.object({ iso_code: z.string(), name: z.string(), change: z.string(), what: z.string() }),
    ),
    issues: z.array(z.object({ kind: z.string(), subject: z.string(), detail: z.string() })),
});

const oreImportResultSchema = z.object({
    success: z.boolean(),
    message: z.string(),
    added: z.number().int(),
    changed: z.number().int(),
    deleted: z.number().int(),
    kept: z.number().int(),
    errors: z.array(z.object({ iso_code: z.string(), message: z.string() })),
});

const oreImportBodySchema = z.object({
    content: z.string().min(1),
    choices: z
        .array(
            z.object({
                iso_code: z.string().trim().min(1).max(10),
                action: z.enum(['take', 'keep', 'delete', 'skip']),
            }),
        )
        .max(10_000),
    reasonCode: z.string().trim().min(1).max(200),
    commentary: z.string().max(2000),
});

export function registerRecordRoutes(
    server: FastifyInstance,
    requireSession: (request: FastifyRequest) => LiveSession,
): void {
    /**
     * The resources a screen may read, each with its key fields and the
     * permissions the server checks to write and to remove a row.
     */
    server.get('/api/refdata', async (request) => {
        requireSession(request);
        return {
            resources: REFDATA_RECORDS.map((resource) => ({
                key: resource.key,
                entityType: resource.entityType,
                keyFields: resource.keyFields,
                versioned: resource.versioned,
                writable: resource.writable,
                search: resource.search,
                sortable: resource.sortable,
                events: {
                    component: resource.subjects.put.split('.')[0] ?? '',
                    entity: resourceName(resource),
                },
                writePermission: `refdata::${resourceName(resource)}:write`,
                deletePermission: `refdata::${resourceName(resource)}:delete`,
            })),
        };
    });

    /**
     * A resource's rows. A read that names a limit gets one page and the total,
     * searched, filtered and ordered on the server; a read without one gets
     * every row, for the pickers that offer a whole kind.
     */
    server.get('/api/refdata/:resource', async (request) => {
        const session = requireSession(request);
        const resource = resourceFor(request);
        const query = request.query as Record<string, unknown>;
        if (query['limit'] === undefined) {
            return { rows: await listRecords(session.client, resource) };
        }
        const page = pageQuerySchema.safeParse(query);
        if (!page.success) {
            throw invalidRequest('A page names an offset and a limit of 1 to 1000.');
        }
        if (page.data.sort !== '' && !resource.sortable.includes(page.data.sort)) {
            throw invalidRequest(`${resource.key} cannot be ordered by ${page.data.sort}.`);
        }
        if (page.data.search !== '' && !resource.search) {
            throw invalidRequest(`${resource.key} cannot be searched.`);
        }
        const filters = filtersOf(resource, query);
        return await listRecordPage(session.client, resource, { ...page.data, filters });
    });

    /** One record, named by its key, for a record page. */
    server.get('/api/refdata/:resource/key/:key', async (request) => {
        const session = requireSession(request);
        const resource = resourceFor(request);
        if (resource.keyFields.length !== 1) {
            throw notFound(`${resource.key} is not read by one key.`);
        }
        const key = code.safeParse((request.params as { key: string }).key);
        if (!key.success) {
            throw invalidRequest('A record is named by a code of 1 to 100 characters.');
        }
        const row = await readRecord(session.client, resource, key.data);
        if (row === undefined) {
            throw notFound(`There is no ${resource.key} record ${key.data}.`);
        }
        return { row };
    });

    /** The rows of a junction that belong to one parent, such as one currency's countries. */
    server.get('/api/refdata/:resource/by/:parent', async (request) => {
        const session = requireSession(request);
        const resource = resourceFor(request);
        if (resource.listBy === undefined) {
            throw notFound(`${resource.key} is not read by a parent.`);
        }
        const parent = code.safeParse((request.params as { parent: string }).parent);
        if (!parent.success) {
            throw invalidRequest('A parent is named by a code of 1 to 100 characters.');
        }
        return { rows: await listRecords(session.client, resource, parent.data) };
    });

    /** The materialised days of one calendar in one year, for the business days panel. */
    server.get('/api/refdata/calendars/:code/days', async (request) => {
        const session = requireSession(request);
        const { code: calendar } = request.params as { code: string };
        const query = z
            .object({ year: z.coerce.number().pipe(calendarYear) })
            .safeParse(request.query);
        if (!query.success) {
            throw invalidRequest('The days are read one year at a time, between 1900 and 2200.');
        }
        return { days: await readCalendarYear(session.client, calendar, query.data.year) };
    });

    /**
     * The standard Import and Export pair of the currencies (RS-ORE-06): the
     * document the ORE service writes, what a file would change, and the write.
     * Each answers with the service's own words; a refusal is a 200 with
     * success false, because the person reads the reason on the step.
     */
    server.get('/api/refdata/currencies/ore-document', async (request) => {
        const session = requireSession(request);
        return session.client.callAuthenticated(
            oreCurrencySubjects.export_currency_config_request,
            { iso_codes: [] },
            oreDocumentSchema,
        );
    });

    server.post('/api/refdata/currencies/ore-import/preview', async (request) => {
        const session = requireSession(request);
        const body = z.object({ content: z.string().min(1) }).safeParse(request.body);
        if (!body.success) {
            throw invalidRequest('A preview needs the text of the file.');
        }
        return session.client.callAuthenticated(
            oreCurrencySubjects.preview_currency_config_import_request,
            { content: body.data.content },
            orePreviewSchema,
        );
    });

    server.post('/api/refdata/currencies/ore-import', async (request) => {
        const session = requireSession(request);
        const body = oreImportBodySchema.safeParse(request.body);
        if (!body.success) {
            throw invalidRequest('An import needs the file, the choices and a reason.');
        }
        return session.client.callAuthenticated(
            oreCurrencySubjects.import_currency_config_request,
            {
                content: body.data.content,
                choices: body.data.choices,
                reason_code: body.data.reasonCode,
                commentary: body.data.commentary,
            },
            oreImportResultSchema,
        );
    });

    /** Builds the business days of one calendar up to the end of a year. */
    server.post('/api/refdata/calendars/:code/rebuild', async (request) => {
        const session = requireSession(request);
        const { code: calendar } = request.params as { code: string };
        const body = z.object({ endYear: calendarYear }).safeParse(request.body);
        if (!body.success) {
            throw invalidRequest('A rebuild names the last year to build, between 1900 and 2200.');
        }
        return { written: await rebuildCalendar(session.client, calendar, body.data.endYear) };
    });

    /** Writes one row: a new row with no version, else a correction of the version read. */
    server.put('/api/refdata/:resource', async (request, reply) => {
        const session = requireSession(request);
        const resource = writableFor(request);
        const body = saveBodySchema.safeParse(request.body);
        if (!body.success) {
            throw invalidRequest(
                'A write needs the row, the version it was read at, and a reason.',
            );
        }
        const write = WRITES[resource.key]?.safeParse(body.data.write);
        if (write === undefined || !write.success) {
            const rule = write?.error.issues.find((issue) => issue.code === 'custom');
            throw invalidRequest(rule?.message ?? `The row is not a valid ${resource.key} row.`);
        }
        if (EDITABLE_PARENT.has(resource.key)) {
            await refuseReadOnlyCalendar(session, String(write.data['calendar_code']));
        }
        answer(
            await saveRecord(session.client, resource, write.data, body.data.version, {
                reasonCode: body.data.reasonCode,
                commentary: body.data.commentary,
            }),
        );
        return reply.code(204).send();
    });

    /**
     * Removes one row, named by exactly its key fields. A removal that names
     * the version it read is refused with 409 when the row moved on since.
     */
    server.delete('/api/refdata/:resource', async (request, reply) => {
        const session = requireSession(request);
        const resource = writableFor(request);
        const body = removeBodySchema.safeParse(request.body);
        const named = body.success ? Object.keys(body.data.key).sort().join(',') : '';
        if (!body.success || named !== [...resource.keyFields].sort().join(',')) {
            throw invalidRequest(
                `A removal names the row by ${resource.keyFields.join(' and ')}, and a reason.`,
            );
        }
        answer(
            await removeRecord(session.client, resource, body.data.key, body.data.version, {
                reasonCode: body.data.reasonCode,
                commentary: body.data.commentary,
            }),
        );
        return reply.code(204).send();
    });
}
