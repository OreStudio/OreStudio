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
import type {
    BadgePresentation,
    ClassificationList,
    ClassificationRow,
    ClassificationShape,
    HistoryVersion,
} from './domain.js';
import { readBadgeCatalogue } from './badges.js';
import { OperationFailedError } from './errors.js';
import { subjects as badgeMappingSubjects } from './generated/dq/protocol/badge_mapping_protocol.js';
import {
    historySubjectFor,
    type GetEntityHistoryRequest,
} from './generated/history/protocol/history_protocol.js';
import { subjects as monetaryNatureSubjects } from './generated/refdata/protocol/monetary_nature_protocol.js';
import { subjects as roundingTypeSubjects } from './generated/refdata/protocol/rounding_type_protocol.js';
import { subjects as currencyMarketTierSubjects } from './generated/refdata/protocol/currency_market_tier_protocol.js';
import { subjects as currencyPairClassificationSubjects } from './generated/refdata/protocol/currency_pair_classification_protocol.js';
import { subjects as calendarTypeSubjects } from './generated/refdata/protocol/calendar_type_protocol.js';
import { subjects as diaryEntryTypeSubjects } from './generated/refdata/protocol/diary_entry_type_protocol.js';
import { subjects as calendarNameSubjects } from './generated/refdata/protocol/calendar_name_protocol.js';
import { subjects as businessDayConventionTypeSubjects } from './generated/refdata/protocol/business_day_convention_type_protocol.js';
import { subjects as partyTypeSubjects } from './generated/refdata/protocol/party_type_protocol.js';
import { subjects as partyStatusSubjects } from './generated/refdata/protocol/party_status_protocol.js';
import { subjects as contactTypeSubjects } from './generated/refdata/protocol/contact_type_protocol.js';
import { subjects as bookStatusSubjects } from './generated/refdata/protocol/book_status_protocol.js';
import { subjects as bookPurposeTypeSubjects } from './generated/refdata/protocol/book_purpose_type_protocol.js';
import { subjects as ledgerFeedTypeSubjects } from './generated/refdata/protocol/ledger_feed_type_protocol.js';
import { subjects as regulatoryBookTypeSubjects } from './generated/refdata/protocol/regulatory_book_type_protocol.js';
import { subjects as purposeTypeSubjects } from './generated/refdata/protocol/purpose_type_protocol.js';
import { subjects as assetClassCodeSubjects } from './generated/refdata/protocol/asset_class_code_protocol.js';
import { subjects as legTypeSubjects } from './generated/refdata/protocol/leg_type_protocol.js';
import { subjects as floatingIndexTypeSubjects } from './generated/refdata/protocol/floating_index_type_protocol.js';
import { subjects as dayCounterSubjects } from './generated/refdata/protocol/day_counter_protocol.js';
import { subjects as dayCountFractionTypeSubjects } from './generated/refdata/protocol/day_count_fraction_type_protocol.js';
import { subjects as curveRoleSubjects } from './generated/refdata/protocol/curve_role_protocol.js';
import { subjects as tenorKindSubjects } from './generated/refdata/protocol/tenor_kind_protocol.js';
import { subjects as tenorUnitSubjects } from './generated/refdata/protocol/tenor_unit_protocol.js';
import { subjects as tenorAnchorSubjects } from './generated/refdata/protocol/tenor_anchor_protocol.js';
import { subjects as tenorResolutionAlgorithmSubjects } from './generated/refdata/protocol/tenor_resolution_algorithm_protocol.js';
import { subjects as derivationKindSubjects } from './generated/refdata/protocol/derivation_kind_protocol.js';
import { subjects as seriesSubclassCodeSubjects } from './generated/refdata/protocol/series_subclass_code_protocol.js';
import { resultEnvelopeSchema } from './operations.js';
import {
    intentFor,
    outcomeOf,
    preconditionFor,
    type WriteIntent as ClassificationIntent,
    type WriteOutcome as ClassificationWrite,
} from './records.js';

export type { ClassificationIntent, ClassificationWrite };

/**
 * One classification list: what the screen shows, and the subjects that read
 * and write it.
 *
 * The subjects are taken from each list's generated protocol, so a rename
 * reaches this table through the generator. `rows` names the field of the
 * list reply that carries the rows, which differs between lists.
 */
interface ClassificationDescriptor extends Omit<
    ClassificationList,
    'writePermission' | 'deletePermission'
> {
    readonly rows: string;
    /** Whether the list request carries a point-in-time field, which its decoder requires. */
    readonly asOf?: true;
    readonly subjects: {
        readonly list: string;
        readonly put: string;
        readonly putMany: string;
        readonly remove: string;
    };
}

/**
 * The 28 lists the classification screen maintains, in screen order.
 *
 * This table is the BFF's authority: a list it does not name cannot be read
 * or written through the classification routes. The five lists of ORE
 * spellings are not editable, because an ORE document must write them
 * exactly.
 */
export const CLASSIFICATION_LISTS: readonly ClassificationDescriptor[] = [
    {
        key: 'monetary-nature',
        entityType: 'ores.refdata.monetary_nature',
        topic: 'currencies',
        shape: 'named',
        editable: true,
        rows: 'types',
        subjects: {
            list: monetaryNatureSubjects.list_monetary_natures_request,
            put: monetaryNatureSubjects.put_monetary_nature_request,
            putMany: monetaryNatureSubjects.put_many_monetary_natures_request,
            remove: monetaryNatureSubjects.delete_monetary_nature_request,
        },
    },
    {
        key: 'rounding-type',
        entityType: 'ores.refdata.rounding_type',
        topic: 'currencies',
        shape: 'named',
        editable: true,
        rows: 'types',
        subjects: {
            list: roundingTypeSubjects.list_rounding_types_request,
            put: roundingTypeSubjects.put_rounding_type_request,
            putMany: roundingTypeSubjects.put_many_rounding_types_request,
            remove: roundingTypeSubjects.delete_rounding_type_request,
        },
    },
    {
        key: 'currency-market-tier',
        entityType: 'ores.refdata.currency_market_tier',
        topic: 'currencies',
        shape: 'named',
        editable: true,
        rows: 'types',
        subjects: {
            list: currencyMarketTierSubjects.list_currency_market_tiers_request,
            put: currencyMarketTierSubjects.put_currency_market_tier_request,
            putMany: currencyMarketTierSubjects.put_many_currency_market_tiers_request,
            remove: currencyMarketTierSubjects.delete_currency_market_tier_request,
        },
    },
    {
        key: 'currency-pair-classification',
        entityType: 'ores.refdata.currency_pair_classification',
        topic: 'currencies',
        shape: 'named',
        editable: true,
        rows: 'classifications',
        subjects: {
            list: currencyPairClassificationSubjects.list_currency_pair_classifications_request,
            put: currencyPairClassificationSubjects.put_currency_pair_classification_request,
            putMany:
                currencyPairClassificationSubjects.put_many_currency_pair_classifications_request,
            remove: currencyPairClassificationSubjects.delete_currency_pair_classification_request,
        },
    },
    {
        key: 'calendar-type',
        entityType: 'ores.refdata.calendar_type',
        topic: 'calendars',
        shape: 'named',
        editable: true,
        rows: 'types',
        subjects: {
            list: calendarTypeSubjects.list_calendar_types_request,
            put: calendarTypeSubjects.put_calendar_type_request,
            putMany: calendarTypeSubjects.put_many_calendar_types_request,
            remove: calendarTypeSubjects.delete_calendar_type_request,
        },
    },
    {
        key: 'diary-entry-type',
        entityType: 'ores.refdata.diary_entry_type',
        topic: 'calendars',
        shape: 'named',
        editable: true,
        rows: 'entry_types',
        subjects: {
            list: diaryEntryTypeSubjects.list_diary_entry_types_request,
            put: diaryEntryTypeSubjects.put_diary_entry_type_request,
            putMany: diaryEntryTypeSubjects.put_many_diary_entry_types_request,
            remove: diaryEntryTypeSubjects.delete_diary_entry_type_request,
        },
    },
    {
        key: 'calendar-name',
        entityType: 'ores.refdata.calendar_name',
        topic: 'calendars',
        shape: 'plain',
        editable: false,
        rows: 'calendar_names',
        subjects: {
            list: calendarNameSubjects.list_calendar_names_request,
            put: calendarNameSubjects.put_calendar_name_request,
            putMany: calendarNameSubjects.put_many_calendar_names_request,
            remove: calendarNameSubjects.delete_calendar_name_request,
        },
    },
    {
        key: 'business-day-convention-type',
        entityType: 'ores.refdata.business_day_convention_type',
        topic: 'calendars',
        shape: 'named',
        editable: false,
        rows: 'types',
        subjects: {
            list: businessDayConventionTypeSubjects.list_business_day_convention_types_request,
            put: businessDayConventionTypeSubjects.put_business_day_convention_type_request,
            putMany:
                businessDayConventionTypeSubjects.put_many_business_day_convention_types_request,
            remove: businessDayConventionTypeSubjects.delete_business_day_convention_type_request,
        },
    },
    {
        key: 'party-type',
        entityType: 'ores.refdata.party_type',
        topic: 'parties',
        shape: 'named',
        editable: true,
        rows: 'types',
        subjects: {
            list: partyTypeSubjects.list_party_types_request,
            put: partyTypeSubjects.put_party_type_request,
            putMany: partyTypeSubjects.put_many_party_types_request,
            remove: partyTypeSubjects.delete_party_type_request,
        },
    },
    {
        key: 'party-status',
        entityType: 'ores.refdata.party_status',
        topic: 'parties',
        shape: 'named',
        editable: true,
        rows: 'statuses',
        subjects: {
            list: partyStatusSubjects.list_party_statuses_request,
            put: partyStatusSubjects.put_party_status_request,
            putMany: partyStatusSubjects.put_many_party_statuses_request,
            remove: partyStatusSubjects.delete_party_status_request,
        },
    },
    {
        key: 'contact-type',
        entityType: 'ores.refdata.contact_type',
        topic: 'parties',
        shape: 'named',
        editable: true,
        rows: 'types',
        subjects: {
            list: contactTypeSubjects.list_contact_types_request,
            put: contactTypeSubjects.put_contact_type_request,
            putMany: contactTypeSubjects.put_many_contact_types_request,
            remove: contactTypeSubjects.delete_contact_type_request,
        },
    },
    {
        key: 'book-status',
        entityType: 'ores.refdata.book_status',
        topic: 'books',
        shape: 'named',
        editable: true,
        rows: 'statuses',
        asOf: true,
        subjects: {
            list: bookStatusSubjects.list_book_statuses_request,
            put: bookStatusSubjects.put_book_status_request,
            putMany: bookStatusSubjects.put_many_book_statuses_request,
            remove: bookStatusSubjects.delete_book_status_request,
        },
    },
    {
        key: 'book-purpose-type',
        entityType: 'ores.refdata.book_purpose_type',
        topic: 'books',
        shape: 'named',
        editable: true,
        rows: 'types',
        subjects: {
            list: bookPurposeTypeSubjects.list_book_purpose_types_request,
            put: bookPurposeTypeSubjects.put_book_purpose_type_request,
            putMany: bookPurposeTypeSubjects.put_many_book_purpose_types_request,
            remove: bookPurposeTypeSubjects.delete_book_purpose_type_request,
        },
    },
    {
        key: 'ledger-feed-type',
        entityType: 'ores.refdata.ledger_feed_type',
        topic: 'books',
        shape: 'named',
        editable: true,
        rows: 'types',
        subjects: {
            list: ledgerFeedTypeSubjects.list_ledger_feed_types_request,
            put: ledgerFeedTypeSubjects.put_ledger_feed_type_request,
            putMany: ledgerFeedTypeSubjects.put_many_ledger_feed_types_request,
            remove: ledgerFeedTypeSubjects.delete_ledger_feed_type_request,
        },
    },
    {
        key: 'regulatory-book-type',
        entityType: 'ores.refdata.regulatory_book_type',
        topic: 'books',
        shape: 'named',
        editable: true,
        rows: 'types',
        asOf: true,
        subjects: {
            list: regulatoryBookTypeSubjects.list_regulatory_book_types_request,
            put: regulatoryBookTypeSubjects.put_regulatory_book_type_request,
            putMany: regulatoryBookTypeSubjects.put_many_regulatory_book_types_request,
            remove: regulatoryBookTypeSubjects.delete_regulatory_book_type_request,
        },
    },
    {
        key: 'purpose-type',
        entityType: 'ores.refdata.purpose_type',
        topic: 'books',
        shape: 'named',
        editable: true,
        rows: 'types',
        subjects: {
            list: purposeTypeSubjects.list_purpose_types_request,
            put: purposeTypeSubjects.put_purpose_type_request,
            putMany: purposeTypeSubjects.put_many_purpose_types_request,
            remove: purposeTypeSubjects.delete_purpose_type_request,
        },
    },
    {
        key: 'asset-class-code',
        entityType: 'ores.refdata.asset_class_code',
        topic: 'products',
        shape: 'named',
        editable: true,
        rows: 'asset_classes',
        subjects: {
            list: assetClassCodeSubjects.list_asset_class_codes_request,
            put: assetClassCodeSubjects.put_asset_class_code_request,
            putMany: assetClassCodeSubjects.put_many_asset_class_codes_request,
            remove: assetClassCodeSubjects.delete_asset_class_code_request,
        },
    },
    {
        key: 'leg-type',
        entityType: 'ores.refdata.leg_type',
        topic: 'products',
        shape: 'plain',
        editable: false,
        rows: 'types',
        subjects: {
            list: legTypeSubjects.list_leg_types_request,
            put: legTypeSubjects.put_leg_type_request,
            putMany: legTypeSubjects.put_many_leg_types_request,
            remove: legTypeSubjects.delete_leg_type_request,
        },
    },
    {
        key: 'floating-index-type',
        entityType: 'ores.refdata.floating_index_type',
        topic: 'products',
        shape: 'plain',
        editable: false,
        rows: 'types',
        subjects: {
            list: floatingIndexTypeSubjects.list_floating_index_types_request,
            put: floatingIndexTypeSubjects.put_floating_index_type_request,
            putMany: floatingIndexTypeSubjects.put_many_floating_index_types_request,
            remove: floatingIndexTypeSubjects.delete_floating_index_type_request,
        },
    },
    {
        key: 'day-counter',
        entityType: 'ores.refdata.day_counter',
        topic: 'products',
        shape: 'plain',
        editable: false,
        rows: 'day_counters',
        subjects: {
            list: dayCounterSubjects.list_day_counters_request,
            put: dayCounterSubjects.put_day_counter_request,
            putMany: dayCounterSubjects.put_many_day_counters_request,
            remove: dayCounterSubjects.delete_day_counter_request,
        },
    },
    {
        key: 'day-count-fraction-type',
        entityType: 'ores.refdata.day_count_fraction_type',
        topic: 'products',
        shape: 'named',
        editable: true,
        rows: 'types',
        subjects: {
            list: dayCountFractionTypeSubjects.list_day_count_fraction_types_request,
            put: dayCountFractionTypeSubjects.put_day_count_fraction_type_request,
            putMany: dayCountFractionTypeSubjects.put_many_day_count_fraction_types_request,
            remove: dayCountFractionTypeSubjects.delete_day_count_fraction_type_request,
        },
    },
    {
        key: 'curve-role',
        entityType: 'ores.refdata.curve_role',
        topic: 'products',
        shape: 'named',
        editable: true,
        rows: 'roles',
        subjects: {
            list: curveRoleSubjects.list_curve_roles_request,
            put: curveRoleSubjects.put_curve_role_request,
            putMany: curveRoleSubjects.put_many_curve_roles_request,
            remove: curveRoleSubjects.delete_curve_role_request,
        },
    },
    {
        key: 'tenor-kind',
        entityType: 'ores.refdata.tenor_kind',
        topic: 'tenors',
        shape: 'named',
        editable: true,
        rows: 'kinds',
        subjects: {
            list: tenorKindSubjects.list_tenor_kinds_request,
            put: tenorKindSubjects.put_tenor_kind_request,
            putMany: tenorKindSubjects.put_many_tenor_kinds_request,
            remove: tenorKindSubjects.delete_tenor_kind_request,
        },
    },
    {
        key: 'tenor-unit',
        entityType: 'ores.refdata.tenor_unit',
        topic: 'tenors',
        shape: 'named',
        editable: true,
        rows: 'units',
        subjects: {
            list: tenorUnitSubjects.list_tenor_units_request,
            put: tenorUnitSubjects.put_tenor_unit_request,
            putMany: tenorUnitSubjects.put_many_tenor_units_request,
            remove: tenorUnitSubjects.delete_tenor_unit_request,
        },
    },
    {
        key: 'tenor-anchor',
        entityType: 'ores.refdata.tenor_anchor',
        topic: 'tenors',
        shape: 'ordered',
        editable: true,
        rows: 'anchors',
        subjects: {
            list: tenorAnchorSubjects.list_tenor_anchors_request,
            put: tenorAnchorSubjects.put_tenor_anchor_request,
            putMany: tenorAnchorSubjects.put_many_tenor_anchors_request,
            remove: tenorAnchorSubjects.delete_tenor_anchor_request,
        },
    },
    {
        key: 'tenor-resolution-algorithm',
        entityType: 'ores.refdata.tenor_resolution_algorithm',
        topic: 'tenors',
        shape: 'named',
        editable: true,
        rows: 'algorithms',
        subjects: {
            list: tenorResolutionAlgorithmSubjects.list_tenor_resolution_algorithms_request,
            put: tenorResolutionAlgorithmSubjects.put_tenor_resolution_algorithm_request,
            putMany: tenorResolutionAlgorithmSubjects.put_many_tenor_resolution_algorithms_request,
            remove: tenorResolutionAlgorithmSubjects.delete_tenor_resolution_algorithm_request,
        },
    },
    {
        key: 'derivation-kind',
        entityType: 'ores.refdata.derivation_kind',
        topic: 'market-data',
        shape: 'named',
        editable: true,
        rows: 'kinds',
        subjects: {
            list: derivationKindSubjects.list_derivation_kinds_request,
            put: derivationKindSubjects.put_derivation_kind_request,
            putMany: derivationKindSubjects.put_many_derivation_kinds_request,
            remove: derivationKindSubjects.delete_derivation_kind_request,
        },
    },
    {
        key: 'series-subclass-code',
        entityType: 'ores.refdata.series_subclass_code',
        topic: 'market-data',
        shape: 'named',
        editable: true,
        rows: 'series_subclasses',
        subjects: {
            list: seriesSubclassCodeSubjects.list_series_subclass_codes_request,
            put: seriesSubclassCodeSubjects.put_series_subclass_code_request,
            putMany: seriesSubclassCodeSubjects.put_many_series_subclass_codes_request,
            remove: seriesSubclassCodeSubjects.delete_series_subclass_code_request,
        },
    },
];

/** The list with this key, or nothing when the catalogue has none. */
export function classificationList(key: string): ClassificationDescriptor | undefined {
    return CLASSIFICATION_LISTS.find((list) => list.key === key);
}

/**
 * The name the list's subjects and permissions share, such as `rounding_types`.
 *
 * Read from the put subject, so the permission a screen checks is the one the
 * server enforces for that subject.
 */
function resourceOf(list: ClassificationDescriptor): string {
    return list.subjects.put.split('.')[2] ?? '';
}

/** The code domain of a list's labels: its entity name, as the label catalogue keys it. */
export function codeDomainOf(list: ClassificationDescriptor): string {
    return list.entityType.split('.')[2] ?? '';
}

/** Every list's public description, without its subjects. */
export function classificationCatalogue(): readonly ClassificationList[] {
    return CLASSIFICATION_LISTS.map((list) => ({
        key: list.key,
        entityType: list.entityType,
        topic: list.topic,
        shape: list.shape,
        editable: list.editable,
        writePermission: `refdata::${resourceOf(list)}:write`,
        deletePermission: `refdata::${resourceOf(list)}:delete`,
    }));
}

/** How many rows a list holds, read without reading the rows. */
export async function countClassificationRows(
    caller: AuthenticatedCaller,
    list: ClassificationDescriptor,
): Promise<number> {
    const reply = await caller.callAuthenticated(
        list.subjects.list,
        {
            offset: 0,
            limit: 1,
            order: { field: '', descending: false },
            filter: null,
            ...(list.asOf === true ? { as_of: null } : {}),
        },
        z.looseObject({ result: resultEnvelopeSchema, total: z.int().nonnegative().default(0) }),
    );
    if (reply.result.outcome !== 'ok') {
        throw new OperationFailedError(list.subjects.list, reply.result.message);
    }
    return reply.total;
}

/** The lists are short; one page reads any of them whole. */
const ROW_PAGE = 1000;

const wireRowSchema = z.object({
    version: z.int().nonnegative().default(0),
    code: z.string(),
    name: z.string().default(''),
    description: z.string().default(''),
    display_order: z.int().optional(),
    modified_by: z.string().default(''),
    recorded_at: z.string().default(''),
    change_reason_code: z.string().default(''),
    change_commentary: z.string().default(''),
});

const resultReplySchema = z.object({ result: resultEnvelopeSchema });

const wireRowsSchema = z.array(wireRowSchema).default([]);

/**
 * The list reply, with its rows read from the field the list names. The
 * field differs between lists, so it is read by name rather than declared.
 */
function rowsReplySchema(field: string) {
    return z.looseObject({ result: resultEnvelopeSchema }).transform((reply, context) => {
        const rows = wireRowsSchema.safeParse(reply[field]);
        if (!rows.success) {
            context.addIssue({ code: 'custom', message: `Malformed ${field}`, path: [field] });
            return z.NEVER;
        }
        return { result: reply.result, rows: rows.data };
    });
}

/**
 * Reads every row of one list, in display order where the list has one.
 *
 * The generated list contract orders by key only, so the rows are read in key
 * order and sorted here.
 */
export async function listClassificationRows(
    caller: AuthenticatedCaller,
    list: ClassificationDescriptor,
): Promise<readonly ClassificationRow[]> {
    const reply = await caller.callAuthenticated(
        list.subjects.list,
        {
            offset: 0,
            limit: ROW_PAGE,
            order: { field: '', descending: false },
            filter: null,
            ...(list.asOf === true ? { as_of: null } : {}),
        },
        rowsReplySchema(list.rows),
    );
    if (reply.result.outcome !== 'ok') {
        throw new OperationFailedError(list.subjects.list, reply.result.message);
    }
    return reply.rows
        .map((row) => ({
            code: row.code,
            name: row.name,
            description: row.description,
            displayOrder: row.display_order ?? null,
            version: row.version,
            modifiedBy: row.modified_by,
            recordedAt: row.recorded_at,
            reasonCode: row.change_reason_code,
            commentary: row.change_commentary,
            labelCode: null,
        }))
        .sort(
            (a, b) => (a.displayOrder ?? 0) - (b.displayOrder ?? 0) || a.code.localeCompare(b.code),
        );
}

/** What a person writes for one row: the version they read, or none for a new row. */
export interface ClassificationRowInput {
    readonly code: string;
    readonly name: string;
    readonly description: string;
    readonly displayOrder: number | null;
    readonly version: number | null;
}

function writeFor(
    shape: ClassificationShape,
    row: ClassificationRowInput,
): Record<string, unknown> {
    switch (shape) {
        case 'named':
            return {
                code: row.code,
                name: row.name,
                description: row.description,
                display_order: row.displayOrder ?? 0,
            };
        case 'ordered':
            return {
                code: row.code,
                description: row.description,
                display_order: row.displayOrder ?? 0,
            };
        case 'plain':
            return { code: row.code, description: row.description };
    }
}

function changeFor(shape: ClassificationShape, row: ClassificationRowInput): unknown {
    return { write: writeFor(shape, row), precondition: preconditionFor(row.version) };
}

/** Writes one row: a new row when no version is given, else a new version of it. */
export async function saveClassificationRow(
    caller: AuthenticatedCaller,
    list: ClassificationDescriptor,
    row: ClassificationRowInput,
    intent: ClassificationIntent,
): Promise<ClassificationWrite> {
    const reply = await caller.callAuthenticated(
        list.subjects.put,
        { change: changeFor(list.shape, row), intent: intentFor(intent) },
        resultReplySchema,
    );
    return outcomeOf(reply.result);
}

/** Writes several rows in one call, each against the version it was read at. */
export async function saveClassificationRows(
    caller: AuthenticatedCaller,
    list: ClassificationDescriptor,
    rows: readonly ClassificationRowInput[],
    intent: ClassificationIntent,
): Promise<ClassificationWrite> {
    const reply = await caller.callAuthenticated(
        list.subjects.putMany,
        { changes: rows.map((row) => changeFor(list.shape, row)), intent: intentFor(intent) },
        resultReplySchema,
    );
    return outcomeOf(reply.result);
}

/** Closes one row. Its versions stay in the history. */
export async function removeClassificationRow(
    caller: AuthenticatedCaller,
    list: ClassificationDescriptor,
    code: string,
    intent: ClassificationIntent,
): Promise<ClassificationWrite> {
    const reply = await caller.callAuthenticated(
        list.subjects.remove,
        {
            removal: { key: { code }, precondition: { kind: 'any', version: null } },
            intent: intentFor(intent),
        },
        resultReplySchema,
    );
    return outcomeOf(reply.result);
}

const historyReplySchema = z.object({
    success: z.boolean().default(false),
    message: z.string().default(''),
    versions: z
        .array(
            z.object({
                version: z.int().nonnegative().default(0),
                modified_by: z.string().default(''),
                recorded_at: z.string().default(''),
                fields: z
                    .array(z.object({ name: z.string(), value: z.string().default('') }))
                    .default([]),
                changes: z
                    .object({
                        entries: z
                            .array(
                                z.object({
                                    field_name: z.string(),
                                    old_value: z.string().default(''),
                                    new_value: z.string().default(''),
                                }),
                            )
                            .default([]),
                    })
                    .default({ entries: [] }),
            }),
        )
        .default([]),
});

/**
 * Every version of one record, newest first, with what changed in each.
 *
 * The subject is derived from the entity type, so one request serves every
 * component that registers a history provider.
 */
export async function readEntityHistory(
    caller: AuthenticatedCaller,
    entityType: string,
    entityId: string,
): Promise<readonly HistoryVersion[]> {
    const subject = historySubjectFor(entityType);
    const request: GetEntityHistoryRequest = { entity_type: entityType, entity_id: entityId };
    const reply = await caller.callAuthenticated(subject, request, historyReplySchema);
    if (!reply.success) {
        throw new OperationFailedError(subject, reply.message);
    }
    return reply.versions
        .map((version) => ({
            version: version.version,
            modifiedBy: version.modified_by,
            recordedAt: version.recorded_at,
            fields: version.fields,
            changes: version.changes.entries.map((entry) => ({
                field: entry.field_name,
                before: entry.old_value,
                after: entry.new_value,
            })),
        }))
        .sort((a, b) => b.version - a.version);
}

/** The label catalogue holds fewer mappings than this, so one page reads them all. */
const MAPPING_PAGE = 2000;

const mappingsReplySchema = z.object({
    result: resultEnvelopeSchema,
    badge_mappings: z
        .array(
            z.object({
                code_domain_code: z.string(),
                entity_code: z.string(),
                badge_code: z.string(),
            }),
        )
        .default([]),
});

/**
 * The label each code of one list carries: its badge code, keyed by the row's
 * code. A code with no mapping has no entry.
 */
export async function readClassificationLabels(
    caller: AuthenticatedCaller,
    list: ClassificationDescriptor,
): Promise<Readonly<Record<string, string>>> {
    const subject = badgeMappingSubjects.list_by_code_domain_code_badge_mappings_request;
    const reply = await caller.callAuthenticated(
        subject,
        {
            code_domain_code: codeDomainOf(list),
            scope: 'direct',
            offset: 0,
            limit: MAPPING_PAGE,
            order: { field: '', descending: false },
            filter: null,
        },
        mappingsReplySchema,
    );
    if (reply.result.outcome !== 'ok') {
        throw new OperationFailedError(subject, reply.result.message);
    }
    return Object.fromEntries(reply.badge_mappings.map((row) => [row.entity_code, row.badge_code]));
}

/**
 * The label catalogue: every label, and the labels each code domain uses.
 *
 * A picker offers a list its own domain's labels first, and the rest grouped by
 * the domain that uses them.
 */
export async function readLabelCatalogue(caller: AuthenticatedCaller): Promise<{
    readonly labels: readonly BadgePresentation[];
    readonly domains: Readonly<Record<string, readonly string[]>>;
}> {
    const subject = badgeMappingSubjects.list_badge_mappings_request;
    const [catalogue, mappings] = await Promise.all([
        readBadgeCatalogue(caller),
        caller.callAuthenticated(
            subject,
            {
                offset: 0,
                limit: MAPPING_PAGE,
                order: { field: '', descending: false },
                filter: null,
            },
            mappingsReplySchema,
        ),
    ]);
    if (mappings.result.outcome !== 'ok') {
        throw new OperationFailedError(subject, mappings.result.message);
    }
    const domains: Record<string, string[]> = {};
    for (const row of mappings.badge_mappings) {
        const codes = (domains[row.code_domain_code] ??= []);
        if (!codes.includes(row.badge_code)) {
            codes.push(row.badge_code);
        }
    }
    return { labels: Object.values(catalogue), domains };
}

/**
 * Gives one row a label, or takes its label away when the badge is null.
 *
 * The label is a mapping in the catalogue, not a column of the row, so this
 * writes no new version of the row.
 */
export async function setClassificationLabel(
    caller: AuthenticatedCaller,
    list: ClassificationDescriptor,
    code: string,
    badgeCode: string | null,
    intent: ClassificationIntent,
): Promise<ClassificationWrite> {
    const key = { code_domain_code: codeDomainOf(list), entity_code: code };
    const reply =
        badgeCode === null
            ? await caller.callAuthenticated(
                  badgeMappingSubjects.delete_badge_mapping_request,
                  {
                      removal: { key, precondition: { kind: 'any', version: null } },
                      intent: intentFor(intent),
                  },
                  resultReplySchema,
              )
            : await caller.callAuthenticated(
                  badgeMappingSubjects.put_badge_mapping_request,
                  {
                      change: {
                          write: { ...key, badge_code: badgeCode },
                          precondition: { kind: 'any', version: null },
                      },
                      intent: intentFor(intent),
                  },
                  resultReplySchema,
              );
    return outcomeOf(reply.result);
}
