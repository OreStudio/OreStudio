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

import { z } from 'zod';
import type { AuthenticatedCaller } from './account-operations.js';
import { OperationFailedError } from './errors.js';
import { subjects as currencySubjects } from './generated/refdata/protocol/currency_protocol.js';
import { subjects as countrySubjects } from './generated/refdata/protocol/country_protocol.js';
import { subjects as businessCentreSubjects } from './generated/refdata/protocol/business_centre_protocol.js';
import { subjects as calendarSubjects } from './generated/refdata/protocol/calendar_protocol.js';
import { subjects as calendarRuleSubjects } from './generated/refdata/protocol/calendar_rule_protocol.js';
import { subjects as calendarExceptionSubjects } from './generated/refdata/protocol/calendar_exception_protocol.js';
import { subjects as calendarEventSubjects } from './generated/refdata/protocol/calendar_event_protocol.js';
import { subjects as calendarDateSubjects } from './generated/refdata/protocol/calendar_date_protocol.js';
import { subjects as calendarMaterialisationSubjects } from './generated/refdata/protocol/calendar_materialisation_protocol.js';
import { subjects as currencyGroupSubjects } from './generated/refdata/protocol/currency_group_protocol.js';
import { subjects as currencyCountrySubjects } from './generated/refdata/protocol/currency_country_protocol.js';
import { subjects as currencyCalendarSubjects } from './generated/refdata/protocol/currency_calendar_protocol.js';
import { subjects as currencyCurrencyGroupSubjects } from './generated/refdata/protocol/currency_currency_group_protocol.js';
import { subjects as currencyPairSubjects } from './generated/refdata/protocol/currency_pair_protocol.js';
import { subjects as currencyPairConventionSubjects } from './generated/refdata/protocol/currency_pair_convention_protocol.js';
import { subjects as currencyPairConventionCalendarSubjects } from './generated/refdata/protocol/currency_pair_convention_calendar_protocol.js';
import { resultEnvelopeSchema } from './operations.js';

/**
 * One refdata record resource a screen reads and writes: its key fields, the
 * field of the list reply that carries its rows, and its subjects.
 *
 * A junction has no versions and is read by its parent, through `listBy`.
 * A resource that is not writable is read only, for the pickers that need it.
 * `search` and `sortable` say what the model made searchable and sortable on
 * the server, so a list pages, searches and sorts there and never in the browser.
 */
export interface RecordResource {
    readonly key: string;
    readonly entityType: string;
    readonly keyFields: readonly string[];
    readonly rows: string;
    readonly versioned: boolean;
    readonly writable: boolean;
    readonly asOf: boolean;
    readonly search: boolean;
    readonly sortable: readonly string[];
    readonly listBy?: string;
    readonly subjects: {
        readonly list: string;
        readonly put: string;
        readonly remove: string;
        readonly listBy?: string;
    };
}

/**
 * The refdata resources the reference data screens read and write.
 *
 * This table is the BFF's authority: a resource it does not name cannot be
 * reached through the record routes. The subjects come from each entity's
 * generated protocol, so a rename reaches this table through the generator.
 */
export const REFDATA_RECORDS: readonly RecordResource[] = [
    {
        key: 'currencies',
        entityType: 'ores.refdata.currency',
        keyFields: ['iso_code'],
        rows: 'currencies',
        versioned: true,
        writable: true,
        search: true,
        sortable: ['iso_code', 'name', 'monetary_nature', 'market_tier'],
        asOf: true,
        subjects: {
            list: currencySubjects.list_currencies_request,
            put: currencySubjects.put_currency_request,
            remove: currencySubjects.delete_currency_request,
        },
    },
    {
        key: 'countries',
        entityType: 'ores.refdata.country',
        keyFields: ['alpha2_code'],
        rows: 'countries',
        versioned: true,
        writable: false,
        search: false,
        sortable: [],
        asOf: true,
        subjects: {
            list: countrySubjects.list_countries_request,
            put: countrySubjects.put_country_request,
            remove: countrySubjects.delete_country_request,
        },
    },
    {
        key: 'business-centres',
        entityType: 'ores.refdata.business_centre',
        keyFields: ['code'],
        rows: 'centres',
        versioned: true,
        writable: false,
        search: false,
        sortable: [],
        asOf: true,
        subjects: {
            list: businessCentreSubjects.list_business_centres_request,
            put: businessCentreSubjects.put_business_centre_request,
            remove: businessCentreSubjects.delete_business_centre_request,
        },
    },
    {
        key: 'calendars',
        entityType: 'ores.refdata.calendar',
        keyFields: ['code'],
        rows: 'calendars',
        versioned: true,
        writable: true,
        search: true,
        sortable: ['code', 'name', 'calendar_type', 'country_code'],
        asOf: true,
        subjects: {
            list: calendarSubjects.list_calendars_request,
            put: calendarSubjects.put_calendar_request,
            remove: calendarSubjects.delete_calendar_request,
        },
    },
    {
        key: 'calendar-rules',
        entityType: 'ores.refdata.calendar_rule',
        keyFields: ['id'],
        rows: 'calendar_rules',
        versioned: true,
        writable: true,
        search: false,
        sortable: [],
        asOf: true,
        listBy: 'calendar_code',
        subjects: {
            list: calendarRuleSubjects.list_calendar_rules_request,
            put: calendarRuleSubjects.put_calendar_rule_request,
            remove: calendarRuleSubjects.delete_calendar_rule_request,
            listBy: calendarRuleSubjects.list_by_calendar_code_calendar_rules_request,
        },
    },
    {
        key: 'calendar-exceptions',
        entityType: 'ores.refdata.calendar_exception',
        keyFields: ['id'],
        rows: 'calendar_exceptions',
        versioned: true,
        writable: true,
        search: false,
        sortable: [],
        asOf: true,
        listBy: 'calendar_code',
        subjects: {
            list: calendarExceptionSubjects.list_calendar_exceptions_request,
            put: calendarExceptionSubjects.put_calendar_exception_request,
            remove: calendarExceptionSubjects.delete_calendar_exception_request,
            listBy: calendarExceptionSubjects.list_by_calendar_code_calendar_exceptions_request,
        },
    },
    {
        key: 'calendar-events',
        entityType: 'ores.refdata.calendar_event',
        keyFields: ['id'],
        rows: 'calendar_events',
        versioned: true,
        writable: true,
        search: false,
        sortable: [],
        asOf: true,
        listBy: 'calendar_code',
        subjects: {
            list: calendarEventSubjects.list_calendar_events_request,
            put: calendarEventSubjects.put_calendar_event_request,
            remove: calendarEventSubjects.delete_calendar_event_request,
            listBy: calendarEventSubjects.list_by_calendar_code_calendar_events_request,
        },
    },
    {
        key: 'currency-groups',
        entityType: 'ores.refdata.currency_group',
        keyFields: ['code'],
        rows: 'groups',
        versioned: true,
        writable: true,
        search: true,
        sortable: ['code', 'name', 'display_order'],
        asOf: true,
        subjects: {
            list: currencyGroupSubjects.list_currency_groups_request,
            put: currencyGroupSubjects.put_currency_group_request,
            remove: currencyGroupSubjects.delete_currency_group_request,
        },
    },
    {
        key: 'currency-countries',
        entityType: 'ores.refdata.currency_country',
        keyFields: ['currency_iso_code', 'country_alpha2_code'],
        rows: 'currency_countries',
        versioned: false,
        writable: true,
        search: false,
        sortable: [],
        asOf: false,
        listBy: 'currency_iso_code',
        subjects: {
            list: currencyCountrySubjects.list_currency_countries_request,
            put: currencyCountrySubjects.put_currency_country_request,
            remove: currencyCountrySubjects.delete_currency_country_request,
            listBy: currencyCountrySubjects.list_by_currency_iso_code_currency_countries_request,
        },
    },
    {
        key: 'currency-calendars',
        entityType: 'ores.refdata.currency_calendar',
        keyFields: ['currency_iso_code', 'calendar_code'],
        rows: 'currency_calendars',
        versioned: false,
        writable: true,
        search: false,
        sortable: [],
        asOf: false,
        listBy: 'currency_iso_code',
        subjects: {
            list: currencyCalendarSubjects.list_currency_calendars_request,
            put: currencyCalendarSubjects.put_currency_calendar_request,
            remove: currencyCalendarSubjects.delete_currency_calendar_request,
            listBy: currencyCalendarSubjects.list_by_currency_iso_code_currency_calendars_request,
        },
    },
    {
        key: 'currency-memberships',
        entityType: 'ores.refdata.currency_currency_group',
        keyFields: ['currency_iso_code', 'currency_group_code'],
        rows: 'currency_currency_groups',
        versioned: false,
        writable: true,
        search: false,
        sortable: [],
        asOf: false,
        listBy: 'currency_iso_code',
        subjects: {
            list: currencyCurrencyGroupSubjects.list_currency_currency_groups_request,
            put: currencyCurrencyGroupSubjects.put_currency_currency_group_request,
            remove: currencyCurrencyGroupSubjects.delete_currency_currency_group_request,
            listBy: currencyCurrencyGroupSubjects.list_by_currency_iso_code_currency_currency_groups_request,
        },
    },
    {
        key: 'currency-pairs',
        entityType: 'ores.refdata.currency_pair',
        keyFields: ['pair_code'],
        rows: 'pairs',
        versioned: true,
        writable: true,
        search: true,
        sortable: ['pair_code', 'base_currency', 'quote_currency', 'classification'],
        asOf: true,
        subjects: {
            list: currencyPairSubjects.list_currency_pairs_request,
            put: currencyPairSubjects.put_currency_pair_request,
            remove: currencyPairSubjects.delete_currency_pair_request,
        },
    },
    {
        key: 'currency-pair-conventions',
        entityType: 'ores.refdata.currency_pair_convention',
        keyFields: ['pair_code'],
        rows: 'conventions',
        versioned: true,
        writable: true,
        search: false,
        sortable: [],
        asOf: true,
        subjects: {
            list: currencyPairConventionSubjects.list_currency_pair_conventions_request,
            put: currencyPairConventionSubjects.put_currency_pair_convention_request,
            remove: currencyPairConventionSubjects.delete_currency_pair_convention_request,
        },
    },
    {
        key: 'pair-calendars',
        entityType: 'ores.refdata.currency_pair_convention_calendar',
        keyFields: ['pair_code', 'calendar_code'],
        rows: 'currency_pair_convention_calendars',
        versioned: false,
        writable: true,
        search: false,
        sortable: [],
        asOf: false,
        listBy: 'pair_code',
        subjects: {
            list: currencyPairConventionCalendarSubjects.list_currency_pair_convention_calendars_request,
            put: currencyPairConventionCalendarSubjects.put_currency_pair_convention_calendar_request,
            remove: currencyPairConventionCalendarSubjects.delete_currency_pair_convention_calendar_request,
            listBy: currencyPairConventionCalendarSubjects.list_by_pair_code_currency_pair_convention_calendars_request,
        },
    },
];

export function recordResource(key: string): RecordResource | undefined {
    return REFDATA_RECORDS.find((resource) => resource.key === key);
}

/** The name a resource's subjects and permissions share, such as `currencies`. */
export function resourceName(resource: RecordResource): string {
    return resource.subjects.put.split('.')[2] ?? '';
}

/** Why a write is made: the reason code and the person's commentary. */
export interface WriteIntent {
    readonly reasonCode: string;
    readonly commentary: string;
}

/**
 * The outcome of a write: done, or refused with the server's outcome and
 * words, so a route can tell a stale version from a bad input.
 */
export type WriteOutcome =
    | { readonly done: true }
    | { readonly done: false; readonly outcome: string; readonly message: string };

export function intentFor(intent: WriteIntent): unknown {
    return { reason_code: intent.reasonCode, commentary: intent.commentary };
}

/** A new row claims nothing exists; a correction claims the version it read. */
export function preconditionFor(version: number | null): unknown {
    return version === null
        ? { kind: 'must_not_exist', version: null }
        : { kind: 'must_match_version', version };
}

export function outcomeOf(result: z.infer<typeof resultEnvelopeSchema>): WriteOutcome {
    return result.outcome === 'ok'
        ? { done: true }
        : { done: false, outcome: result.outcome, message: result.message };
}

/** A record as the server sends it: its own field names, with its version. */
export type RecordRow = Readonly<Record<string, unknown>> & { readonly version: number };

/** A junction keeps no versions; the default lets one schema read every resource. */
const rowSchema = z.looseObject({ version: z.int().nonnegative().default(0) });

const RECORD_PAGE = 1000;

const resultReplySchema = z.object({ result: resultEnvelopeSchema });

/**
 * A list's filter record, with every member the resource's filter has. The
 * server decodes the record whole, so a member the call does not use is sent
 * as null rather than left out.
 */
function filterFor(
    resource: RecordResource,
    members: { readonly oneOf?: readonly string[]; readonly search?: string },
): Record<string, unknown> {
    const [key] = resource.keyFields;
    return {
        ...(key === undefined || resource.keyFields.length !== 1
            ? {}
            : { [`${key}_one_of`]: members.oneOf ?? null }),
        ...(resource.search ? { search: members.search ?? null } : {}),
    };
}

/** How a list asks for one page: where it starts, how long it is, the search and the order. */
export interface PageRequest {
    readonly offset: number;
    readonly limit: number;
    readonly search: string;
    readonly sort: string;
    readonly descending: boolean;
}

/** One page of a list, and how many rows match in all. */
export interface RecordPage {
    readonly rows: readonly RecordRow[];
    readonly total: number;
}

const pageReplySchema = z.looseObject({
    result: resultEnvelopeSchema,
    total: z.int().nonnegative().default(0),
});

/**
 * One page of a resource, with the server's total, searched and ordered on the
 * server. The search and the order are refused by the caller unless the
 * resource declares them, so they reach the server only where the model has
 * them.
 */
export async function listRecordPage(
    caller: AuthenticatedCaller,
    resource: RecordResource,
    page: PageRequest,
): Promise<RecordPage> {
    const reply = await caller.callAuthenticated(
        resource.subjects.list,
        {
            offset: page.offset,
            limit: page.limit,
            order: { field: page.sort, descending: page.descending },
            filter: page.search === '' ? null : filterFor(resource, { search: page.search }),
            ...(resource.asOf ? { as_of: null } : {}),
        },
        pageReplySchema,
    );
    if (reply.result.outcome !== 'ok') {
        throw new OperationFailedError(resource.subjects.list, reply.result.message);
    }
    return {
        rows: z.array(rowSchema).default([]).parse(reply[resource.rows]),
        total: reply.total,
    };
}

/**
 * One record, named by its key, or undefined when there is none. The list's
 * one-of filter on the key reads it, so a record page reads one row rather
 * than the whole list. Only a resource with a one-field key is read this way.
 */
export async function readRecord(
    caller: AuthenticatedCaller,
    resource: RecordResource,
    key: string,
): Promise<RecordRow | undefined> {
    const [field] = resource.keyFields;
    if (field === undefined || resource.keyFields.length !== 1) {
        throw new OperationFailedError(
            resource.subjects.list,
            `${resource.key} has no one-field key.`,
        );
    }
    const reply = await caller.callAuthenticated(
        resource.subjects.list,
        {
            offset: 0,
            limit: 1,
            order: { field: '', descending: false },
            filter: filterFor(resource, { oneOf: [key] }),
            ...(resource.asOf ? { as_of: null } : {}),
        },
        z.looseObject({ result: resultEnvelopeSchema }),
    );
    if (reply.result.outcome !== 'ok') {
        throw new OperationFailedError(resource.subjects.list, reply.result.message);
    }
    return z.array(rowSchema).default([]).parse(reply[resource.rows])[0];
}

/**
 * Every row of a resource, or every row of one parent when `parent` names it,
 * read a page at a time until a page comes back short.
 *
 * Rows keep the server's field names: a screen reads them through the
 * resource it asked for, and a mapping per resource would only rename them.
 */
export async function listRecords(
    caller: AuthenticatedCaller,
    resource: RecordResource,
    parent?: string,
): Promise<readonly RecordRow[]> {
    const byParent = parent !== undefined && resource.listBy !== undefined;
    const subject = byParent
        ? (resource.subjects.listBy ?? resource.subjects.list)
        : resource.subjects.list;
    const rows: RecordRow[] = [];
    for (let offset = 0; ; offset += RECORD_PAGE) {
        const reply = await caller.callAuthenticated(
            subject,
            {
                ...(byParent && resource.listBy !== undefined
                    ? { [resource.listBy]: parent, scope: 'direct' }
                    : {}),
                offset,
                limit: RECORD_PAGE,
                order: { field: '', descending: false },
                filter: null,
                ...(resource.asOf && !byParent ? { as_of: null } : {}),
            },
            z.looseObject({ result: resultEnvelopeSchema }),
        );
        if (reply.result.outcome !== 'ok') {
            throw new OperationFailedError(subject, reply.result.message);
        }
        const page = z.array(rowSchema).default([]).parse(reply[resource.rows]);
        rows.push(...page);
        if (page.length < RECORD_PAGE) {
            return rows;
        }
    }
}

/** Writes one row: a new row when no version is given, else a new version of it. */
export async function saveRecord(
    caller: AuthenticatedCaller,
    resource: RecordResource,
    write: Readonly<Record<string, unknown>>,
    version: number | null,
    intent: WriteIntent,
): Promise<WriteOutcome> {
    const reply = await caller.callAuthenticated(
        resource.subjects.put,
        { change: { write, precondition: preconditionFor(version) }, intent: intentFor(intent) },
        resultReplySchema,
    );
    return outcomeOf(reply.result);
}

/**
 * Closes one row, named by its key fields. A versioned row stays in its
 * history. A removal that names the version it read is refused when the row
 * moved on since; a junction row has no version to name.
 */
export async function removeRecord(
    caller: AuthenticatedCaller,
    resource: RecordResource,
    key: Readonly<Record<string, unknown>>,
    version: number | null,
    intent: WriteIntent,
): Promise<WriteOutcome> {
    const reply = await caller.callAuthenticated(
        resource.subjects.remove,
        {
            removal: {
                key,
                precondition:
                    version === null
                        ? { kind: 'any', version: null }
                        : { kind: 'must_match_version', version },
            },
            intent: intentFor(intent),
        },
        resultReplySchema,
    );
    return outcomeOf(reply.result);
}

/** One materialised day of a calendar: whether it is a business day, and what produced it. */
export interface CalendarDay {
    readonly date: string;
    readonly businessDay: boolean;
    readonly source: string;
}

const calendarDaysReplySchema = z.object({
    result: resultEnvelopeSchema,
    calendar_dates: z
        .array(z.object({ date: z.string(), is_business_day: z.boolean(), source: z.string() }))
        .default([]),
});

/**
 * The materialised days of one calendar in one year.
 *
 * The dates filter takes no range, so this reads the calendar's days in date
 * order, a page at a time, until it passes the end of the year. The store
 * pages in key order, calendar then date, and refuses a named order field, so
 * the empty order is what gives date order here.
 */
export async function readCalendarYear(
    caller: AuthenticatedCaller,
    calendarCode: string,
    year: number,
): Promise<readonly CalendarDay[]> {
    const first = `${String(year)}-01-01`;
    const last = `${String(year)}-12-31`;
    const days: CalendarDay[] = [];
    for (let offset = 0; ; offset += RECORD_PAGE) {
        const reply = await caller.callAuthenticated(
            calendarDateSubjects.list_by_calendar_code_calendar_dates_request,
            {
                calendar_code: calendarCode,
                scope: 'direct',
                offset,
                limit: RECORD_PAGE,
                order: { field: '', descending: false },
                filter: null,
            },
            calendarDaysReplySchema,
        );
        if (reply.result.outcome !== 'ok') {
            throw new OperationFailedError(
                calendarDateSubjects.list_by_calendar_code_calendar_dates_request,
                reply.result.message,
            );
        }
        for (const row of reply.calendar_dates) {
            if (row.date >= first && row.date <= last) {
                days.push({ date: row.date, businessDay: row.is_business_day, source: row.source });
            }
        }
        const end = reply.calendar_dates.at(-1)?.date;
        if (reply.calendar_dates.length < RECORD_PAGE || end === undefined || end > last) {
            return days;
        }
    }
}

const rebuildReplySchema = z.object({
    success: z.boolean(),
    message: z.string().default(''),
    rows_written: z.int().nonnegative().default(0),
});

/**
 * Builds the business days of one calendar up to the end of a year.
 *
 * The server only adds days it has not built; a day already built keeps the
 * value it was built with.
 */
export async function rebuildCalendar(
    caller: AuthenticatedCaller,
    calendarCode: string,
    endYear: number,
): Promise<number> {
    const subject = calendarMaterialisationSubjects.regenerate_calendar_dates_request;
    const reply = await caller.callAuthenticated(
        subject,
        { calendar_code: calendarCode, end_year: endYear },
        rebuildReplySchema,
    );
    if (!reply.success) {
        throw new OperationFailedError(subject, reply.message);
    }
    return reply.rows_written;
}
