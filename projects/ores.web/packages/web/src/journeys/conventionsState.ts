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

/**
 * The convention journey's draft, and what the confirm makes of it.
 *
 * Twenty-five families have twenty-five shapes, and the screen draws the terms
 * of two in full, the swap and the deposit. Each is a table of term specs: the
 * column, the group it is drawn in, how it is edited and which list it picks
 * from. The draft, the review and the write all read that table, so a family is
 * a row of data and not a branch of code. The draft holds every term as the
 * text a form holds, and the write turns each back into the column's type.
 */

import { useReducer } from 'react';
import type { ConventionIntent, ConventionRow } from './conventionsServer.js';

/** The reason a correction is filed under until the person states another. */
export const CONVENTION_REASON = 'common.non_material_update';

/** The reason a new convention is filed under, which the corrections list does not offer. */
export const NEW_CONVENTION_REASON = 'system.new_record';

/** How a term is edited, and so how its text turns back into a column value. */
export type TermKind = 'pick' | 'text' | 'flag' | 'tristate' | 'integer';

/** The picker lists a term can draw from. */
export type PickList =
    | 'calendars'
    | 'businessDayConventions'
    | 'dayCountFractions'
    | 'floatingIndices'
    | 'paymentFrequencies'
    | 'subPeriodsCouponTypes';

export interface TermSpec {
    readonly column: string;
    /** The part of the convention the term is drawn under. The grouping is the screen's. */
    readonly group: 'general' | 'fixed' | 'floating' | 'settlement';
    readonly kind: TermKind;
    readonly pick?: PickList;
    /** Whether the column is NOT NULL, so the form asks for it. */
    readonly required: boolean;
}

/** The families whose terms are drawn in full, with the terms each carries. */
export const TERM_SPECS: Readonly<Record<string, readonly TermSpec[]>> = {
    swap: [
        {
            column: 'fixed_calendar',
            group: 'fixed',
            kind: 'pick',
            pick: 'calendars',
            required: false,
        },
        {
            column: 'fixed_frequency',
            group: 'fixed',
            kind: 'pick',
            pick: 'paymentFrequencies',
            required: true,
        },
        {
            column: 'fixed_convention',
            group: 'fixed',
            kind: 'pick',
            pick: 'businessDayConventions',
            required: false,
        },
        {
            column: 'fixed_day_count_fraction',
            group: 'fixed',
            kind: 'pick',
            pick: 'dayCountFractions',
            required: true,
        },
        {
            column: 'index',
            group: 'floating',
            kind: 'pick',
            pick: 'floatingIndices',
            required: true,
        },
        {
            column: 'float_frequency',
            group: 'floating',
            kind: 'pick',
            pick: 'paymentFrequencies',
            required: false,
        },
        {
            column: 'sub_periods_coupon_type',
            group: 'floating',
            kind: 'pick',
            pick: 'subPeriodsCouponTypes',
            required: false,
        },
        { column: 'oresmd_uri', group: 'general', kind: 'text', required: false },
    ],
    deposit: [
        { column: 'index_based', group: 'general', kind: 'flag', required: true },
        {
            column: 'index',
            group: 'general',
            kind: 'pick',
            pick: 'floatingIndices',
            required: false,
        },
        {
            column: 'calendar',
            group: 'settlement',
            kind: 'pick',
            pick: 'calendars',
            required: false,
        },
        {
            column: 'convention',
            group: 'settlement',
            kind: 'pick',
            pick: 'businessDayConventions',
            required: false,
        },
        { column: 'end_of_month', group: 'settlement', kind: 'tristate', required: false },
        {
            column: 'day_count_fraction',
            group: 'settlement',
            kind: 'pick',
            pick: 'dayCountFractions',
            required: false,
        },
        { column: 'settlement_days', group: 'settlement', kind: 'integer', required: false },
        { column: 'oresmd_uri', group: 'general', kind: 'text', required: false },
    ],
};

export const CONVENTION_ENTITY_PREFIX = 'ores.refdata.';

/** The audit entity type of a family's convention entity, for its history. */
export function entityTypeOf(entity: string): string {
    return `${CONVENTION_ENTITY_PREFIX}${entity}`;
}

/** The terms of a family, or none when its terms are not drawn. */
export function specsOf(family: string): readonly TermSpec[] {
    return TERM_SPECS[family] ?? [];
}

/** A term as the form holds it: the text of the value, empty for none. */
export type Terms = Readonly<Record<string, string>>;

/** The text a form holds for one column value. */
export function termText(kind: TermKind, value: unknown): string {
    if (value === null || value === undefined) {
        return kind === 'flag' ? 'false' : '';
    }
    return String(value);
}

/** The column value a term's text stands for. */
export function termValue(kind: TermKind, text: string): string | number | boolean | null {
    switch (kind) {
        case 'flag':
            return text === 'true';
        case 'tristate':
            return text === '' ? null : text === 'true';
        case 'integer': {
            const parsed = Number(text);
            return text.trim() === '' || Number.isNaN(parsed) ? null : parsed;
        }
        default:
            return text.trim() === '' ? null : text;
    }
}

/** What the server refused, kept so the review can name it and the person can walk back. */
export interface ConventionRefusal {
    readonly step: string;
    readonly subject: string;
    readonly code: string;
    readonly message: string;
    readonly fields: readonly {
        readonly field: string;
        readonly code: string;
        readonly message: string;
    }[];
}

export interface ConventionDraft {
    /** The family key, or empty before one is chosen. */
    readonly family: string;
    /** The convention as read, or undefined for a new one. */
    readonly opened: ConventionRow | undefined;
    /** Whether a convention is being authored at all. */
    readonly authoring: boolean;
    readonly conventionId: string;
    readonly terms: Terms;
    readonly reasonCode: string;
    readonly commentary: string;
    readonly refusal: ConventionRefusal | undefined;
    readonly written: ConventionRow | undefined;
}

export function blankDraft(): ConventionDraft {
    return {
        family: '',
        opened: undefined,
        authoring: false,
        conventionId: '',
        terms: {},
        reasonCode: CONVENTION_REASON,
        commentary: '',
        refusal: undefined,
        written: undefined,
    };
}

/** The terms a row holds, in the form's text. */
export function termsOf(family: string, row: ConventionRow | undefined): Terms {
    return Object.fromEntries(
        specsOf(family).map((spec) => [
            spec.column,
            termText(spec.kind, row === undefined ? null : row[spec.column]),
        ]),
    );
}

export type ConventionAction =
    | { readonly kind: 'choose-family'; readonly family: string }
    | { readonly kind: 'start-convention'; readonly id: string }
    | { readonly kind: 'open-convention'; readonly row: ConventionRow }
    | { readonly kind: 'set-term'; readonly column: string; readonly value: string }
    | { readonly kind: 'set-reason'; readonly reasonCode: string }
    | { readonly kind: 'set-commentary'; readonly commentary: string }
    | { readonly kind: 'record-written'; readonly row: ConventionRow | undefined }
    | { readonly kind: 'record-refusal'; readonly refusal: ConventionRefusal }
    | { readonly kind: 'clear-refusal' };

export function reduceConvention(
    draft: ConventionDraft,
    action: ConventionAction,
): ConventionDraft {
    switch (action.kind) {
        case 'choose-family':
            return { ...blankDraft(), family: action.family };
        case 'start-convention':
            return {
                ...draft,
                authoring: true,
                opened: undefined,
                conventionId: action.id,
                terms: termsOf(draft.family, undefined),
                reasonCode: NEW_CONVENTION_REASON,
                written: undefined,
                refusal: undefined,
            };
        case 'open-convention':
            return {
                ...draft,
                authoring: true,
                opened: action.row,
                conventionId: action.row.id,
                terms: termsOf(draft.family, action.row),
                reasonCode: CONVENTION_REASON,
                written: undefined,
                refusal: undefined,
            };
        case 'set-term':
            return { ...draft, terms: { ...draft.terms, [action.column]: action.value } };
        case 'set-reason':
            return { ...draft, reasonCode: action.reasonCode };
        case 'set-commentary':
            return { ...draft, commentary: action.commentary };
        case 'record-written':
            return { ...draft, written: action.row, refusal: undefined };
        case 'record-refusal':
            return { ...draft, refusal: action.refusal };
        case 'clear-refusal':
            return { ...draft, refusal: undefined };
    }
}

/** One line of the review: a term, its value before, its value after. */
export interface Change {
    readonly id: string;
    readonly column: string;
    readonly before: string;
    readonly after: string;
}

/** Every term that differs from the convention as read, in the model's own order. */
export function changesOf(draft: ConventionDraft): readonly Change[] {
    if (!draft.authoring) {
        return [];
    }
    const before = termsOf(draft.family, draft.opened);
    return specsOf(draft.family)
        .filter((spec) => (draft.terms[spec.column] ?? '') !== (before[spec.column] ?? ''))
        .map((spec) => ({
            id: spec.column,
            column: spec.column,
            before: before[spec.column] === '' ? '-' : (before[spec.column] ?? '-'),
            after: draft.terms[spec.column] === '' ? '-' : (draft.terms[spec.column] ?? '-'),
        }));
}

/** What the terms still lack, in words the form can show. */
export function termProblems(draft: ConventionDraft): readonly string[] {
    if (!draft.authoring) {
        return [];
    }
    const problems: string[] = [];
    for (const spec of specsOf(draft.family)) {
        if (
            spec.required &&
            spec.kind !== 'flag' &&
            (draft.terms[spec.column] ?? '').trim() === ''
        ) {
            problems.push(spec.column);
        }
    }
    if (draft.family === 'deposit') {
        const indexBased = draft.terms['index_based'] === 'true';
        if (indexBased && (draft.terms['index'] ?? '') === '') {
            problems.push('index');
        }
        if (!indexBased) {
            for (const column of ['day_count_fraction', 'settlement_days']) {
                if ((draft.terms[column] ?? '') === '') {
                    problems.push(column);
                }
            }
        }
    }
    const settlement = draft.terms['settlement_days'];
    if (settlement !== undefined && settlement !== '' && !/^\d+$/.test(settlement)) {
        problems.push('settlement_days');
    }
    return [...new Set(problems)];
}

/** What the confirm sends. */
export interface WritePlan {
    readonly family: string;
    readonly intent: ConventionIntent;
    readonly write: Readonly<Record<string, unknown>>;
    /** The version the convention was read at, or null for a new one. */
    readonly version: number | null;
}

/** The plan the confirm runs, or undefined before a convention is being authored. */
export function writePlan(draft: ConventionDraft): WritePlan | undefined {
    if (!draft.authoring || draft.family === '') {
        return undefined;
    }
    const columns = Object.fromEntries(
        specsOf(draft.family).map((spec) => [
            spec.column,
            termValue(spec.kind, draft.terms[spec.column] ?? ''),
        ]),
    );
    return {
        family: draft.family,
        intent: { reason_code: draft.reasonCode, commentary: draft.commentary },
        // Only the id and the terms: the service owns the audit columns and the version.
        write: { id: draft.conventionId, ...columns },
        version: draft.opened === undefined ? null : draft.opened.version,
    };
}

export interface ConventionTerms extends ConventionDraft {
    readonly changes: readonly Change[];
    readonly chooseFamily: (family: string) => void;
    readonly startConvention: () => void;
    readonly openConvention: (row: ConventionRow) => void;
    readonly setTerm: (column: string, value: string) => void;
    readonly setReason: (reasonCode: string) => void;
    readonly setCommentary: (commentary: string) => void;
    readonly recordWritten: (row: ConventionRow | undefined) => void;
    readonly recordRefusal: (refusal: ConventionRefusal) => void;
    readonly clearRefusal: () => void;
}

export function useConvention(): ConventionTerms {
    const [draft, dispatch] = useReducer(reduceConvention, undefined, blankDraft);
    return {
        ...draft,
        changes: changesOf(draft),
        chooseFamily: (family) => dispatch({ kind: 'choose-family', family }),
        // The id is made here, not in the reducer, so the reducer stays pure.
        startConvention: () => dispatch({ kind: 'start-convention', id: crypto.randomUUID() }),
        openConvention: (row) => dispatch({ kind: 'open-convention', row }),
        setTerm: (column, value) => dispatch({ kind: 'set-term', column, value }),
        setReason: (reasonCode) => dispatch({ kind: 'set-reason', reasonCode }),
        setCommentary: (commentary) => dispatch({ kind: 'set-commentary', commentary }),
        recordWritten: (row) => dispatch({ kind: 'record-written', row }),
        recordRefusal: (refusal) => dispatch({ kind: 'record-refusal', refusal }),
        clearRefusal: () => dispatch({ kind: 'clear-refusal' }),
    };
}
