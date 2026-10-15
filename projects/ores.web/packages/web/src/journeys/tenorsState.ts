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
 * The tenor journey's draft, and what the confirm makes of it.
 *
 * A tenor is two halves: the code a person reads, and the resolution the
 * system computes under a convention. The draft holds the tenor row, the
 * convention row and the convention's set of tenors, each as the text a form
 * holds, and the review is a function of the differences from what was read.
 * The server serves no operation that resolves a tenor to a date, so the draft
 * holds no resolved date.
 */

import { useReducer } from 'react';
import type { Tenor } from '@ores/wire-protocol/generated/refdata/domain/tenor';
import type { TenorConvention } from '@ores/wire-protocol/generated/refdata/domain/tenor_convention';
import type { TenorConventionResolution } from '@ores/wire-protocol/generated/refdata/domain/tenor_convention_resolution';
import type { TenorIntent } from './tenorsServer.js';

/** The reason a correction is filed under until the person states another. */
export const TENOR_REASON = 'common.non_material_update';

/** The reason a new record is filed under, which the corrections list does not offer. */
export const NEW_TENOR_REASON = 'system.new_record';

export const TENOR_ENTITY_TYPE = 'ores.refdata.tenor';
export const CONVENTION_ENTITY_TYPE = 'ores.refdata.tenor_convention';

/** The tenor fields the person edits, as form text. The code is the key and does not change. */
export interface TenorFields {
    readonly code: string;
    readonly displayName: string;
    readonly description: string;
    readonly sortOrder: string;
    readonly kind: string;
    readonly unit: string;
    readonly multiplier: string;
}

export type TenorField = keyof TenorFields;

/** The convention fields the person edits, as form text. The code is the key. */
export interface ConventionFields {
    readonly code: string;
    readonly description: string;
    readonly measuredFrom: string;
    readonly resolutionAlgorithm: string;
    readonly rollRule: string;
    readonly oresmdUri: string;
}

export type ConventionField = keyof ConventionFields;

/** One tenor's place in the convention's set: whether it was and whether it is to be. */
export interface Member {
    readonly tenorCode: string;
    readonly original: TenorConventionResolution | undefined;
    readonly wanted: boolean;
    readonly anchorOverride: string;
}

/** What the server refused, kept so the review can name it and the person can walk back. */
export interface TenorRefusal {
    readonly step: string;
    readonly subject: string;
    readonly code: string;
    readonly message: string;
    readonly fields: readonly {
        readonly field: string;
        readonly code: string;
        readonly message: string;
    }[];
    /** The calls that were already written when this one was refused. */
    readonly written: readonly string[];
}

export interface TenorDraft {
    /** Whether a tenor is being shaped, existing or new. */
    readonly tenorActive: boolean;
    readonly tenorOpened: Tenor | undefined;
    readonly tenor: TenorFields;
    /** Whether a convention is being shaped, existing or new. */
    readonly conventionActive: boolean;
    readonly conventionOpened: TenorConvention | undefined;
    readonly convention: ConventionFields;
    readonly members: readonly Member[];
    readonly reasonCode: string;
    readonly commentary: string;
    readonly refusal: TenorRefusal | undefined;
    readonly written: boolean;
}

function blankTenor(): TenorFields {
    return {
        code: '',
        displayName: '',
        description: '',
        sortOrder: '0',
        kind: '',
        unit: '',
        multiplier: '',
    };
}

function blankConvention(): ConventionFields {
    return {
        code: '',
        description: '',
        measuredFrom: '',
        resolutionAlgorithm: '',
        rollRule: 'ModifiedFollowing',
        oresmdUri: '',
    };
}

export function blankDraft(): TenorDraft {
    return {
        tenorActive: false,
        tenorOpened: undefined,
        tenor: blankTenor(),
        conventionActive: false,
        conventionOpened: undefined,
        convention: blankConvention(),
        members: [],
        reasonCode: TENOR_REASON,
        commentary: '',
        refusal: undefined,
        written: false,
    };
}

export function tenorFieldsOf(tenor: Tenor): TenorFields {
    return {
        code: tenor.code,
        displayName: tenor.display_name,
        description: tenor.description,
        sortOrder: String(tenor.sort_order),
        kind: tenor.kind,
        unit: tenor.unit,
        multiplier: tenor.multiplier === null ? '' : String(tenor.multiplier),
    };
}

export function conventionFieldsOf(convention: TenorConvention): ConventionFields {
    return {
        code: convention.code,
        description: convention.description,
        measuredFrom: convention.measured_from,
        resolutionAlgorithm: convention.resolution_algorithm,
        rollRule: convention.business_day_convention_type,
        oresmdUri: convention.oresmd_uri ?? '',
    };
}

export type TenorAction =
    | { readonly kind: 'open-tenor'; readonly tenor: Tenor }
    | { readonly kind: 'start-tenor' }
    | { readonly kind: 'set-tenor-field'; readonly field: TenorField; readonly value: string }
    | {
          readonly kind: 'open-convention';
          readonly convention: TenorConvention;
          readonly resolutions: readonly TenorConventionResolution[];
          readonly tenorCodes: readonly string[];
      }
    | { readonly kind: 'start-convention'; readonly tenorCodes: readonly string[] }
    | {
          readonly kind: 'set-convention-field';
          readonly field: ConventionField;
          readonly value: string;
      }
    | { readonly kind: 'toggle-member'; readonly tenorCode: string }
    | { readonly kind: 'set-anchor'; readonly tenorCode: string; readonly value: string }
    | { readonly kind: 'set-reason'; readonly reasonCode: string }
    | { readonly kind: 'set-commentary'; readonly commentary: string }
    | { readonly kind: 'record-written' }
    | { readonly kind: 'record-refusal'; readonly refusal: TenorRefusal }
    | { readonly kind: 'clear-refusal' };

function membersFor(
    tenorCodes: readonly string[],
    resolutions: readonly TenorConventionResolution[],
): readonly Member[] {
    return tenorCodes.map((tenorCode) => {
        const original = resolutions.find((row) => row.tenor_code === tenorCode);
        return {
            tenorCode,
            original,
            wanted: original !== undefined,
            anchorOverride: original?.anchor_override ?? '',
        };
    });
}

export function reduceTenor(draft: TenorDraft, action: TenorAction): TenorDraft {
    switch (action.kind) {
        case 'open-tenor':
            return {
                ...draft,
                tenorActive: true,
                tenorOpened: action.tenor,
                tenor: tenorFieldsOf(action.tenor),
                reasonCode: TENOR_REASON,
                written: false,
                refusal: undefined,
            };
        case 'start-tenor':
            return {
                ...draft,
                tenorActive: true,
                tenorOpened: undefined,
                tenor: blankTenor(),
                reasonCode: NEW_TENOR_REASON,
                written: false,
                refusal: undefined,
            };
        case 'set-tenor-field':
            return { ...draft, tenor: { ...draft.tenor, [action.field]: action.value } };
        case 'open-convention':
            return {
                ...draft,
                conventionActive: true,
                conventionOpened: action.convention,
                convention: conventionFieldsOf(action.convention),
                members: membersFor(action.tenorCodes, action.resolutions),
                written: false,
                refusal: undefined,
            };
        case 'start-convention':
            return {
                ...draft,
                conventionActive: true,
                conventionOpened: undefined,
                convention: blankConvention(),
                members: membersFor(action.tenorCodes, []),
                written: false,
                refusal: undefined,
            };
        case 'set-convention-field':
            return { ...draft, convention: { ...draft.convention, [action.field]: action.value } };
        case 'toggle-member':
            return {
                ...draft,
                members: draft.members.map((member) =>
                    member.tenorCode === action.tenorCode
                        ? { ...member, wanted: !member.wanted }
                        : member,
                ),
            };
        case 'set-anchor':
            return {
                ...draft,
                members: draft.members.map((member) =>
                    member.tenorCode === action.tenorCode
                        ? { ...member, anchorOverride: action.value }
                        : member,
                ),
            };
        case 'set-reason':
            return { ...draft, reasonCode: action.reasonCode };
        case 'set-commentary':
            return { ...draft, commentary: action.commentary };
        case 'record-written':
            return { ...draft, written: true, refusal: undefined };
        case 'record-refusal':
            return { ...draft, refusal: action.refusal };
        case 'clear-refusal':
            return { ...draft, refusal: undefined };
    }
}

/** One line of the review: what changes, before, after, and the operation that writes it.
 *
 * The words are not here. `what` and `part` are catalogue keys and `before`/`after`
 * name the state they show, so the screen reaches every word through the catalogue
 * and this module holds no English. See TS-C04.
 */
export interface Change {
    readonly id: string;
    /** The catalogue key for the thing that changes, such as the field or the member. */
    readonly what: string;
    readonly part: string | undefined;
    readonly before: ChangeValue;
    readonly after: ChangeValue;
    readonly operation: string;
}

/** What one side of a change shows: a value read, nothing, or a named state. */
export type ChangeValue =
    | { readonly kind: 'value'; readonly value: string }
    | { readonly kind: 'absent' }
    | { readonly kind: 'state'; readonly state: 'inSet' | 'notInSet'; readonly tenorCode: string };

const present = (tenorCode: string): ChangeValue => ({
    kind: 'state',
    state: 'inSet',
    tenorCode,
});
const absentFromSet = (tenorCode: string): ChangeValue => ({
    kind: 'state',
    state: 'notInSet',
    tenorCode,
});

/** The tenor fields a change is drawn for, in the order the review lists them. */
const TENOR_FIELDS: readonly TenorField[] = [
    'displayName',
    'description',
    'sortOrder',
    'kind',
    'unit',
    'multiplier',
];

/** The convention fields a change is drawn for, in the order the review lists them. */
const CONVENTION_FIELDS: readonly ConventionField[] = [
    'description',
    'measuredFrom',
    'resolutionAlgorithm',
    'rollRule',
    'oresmdUri',
];

function asValue(text: string): ChangeValue {
    return text === '' ? { kind: 'absent' } : { kind: 'value', value: text };
}

function tenorChanged(draft: TenorDraft): boolean {
    if (!draft.tenorActive) {
        return false;
    }
    if (draft.tenorOpened === undefined) {
        return true;
    }
    const before = tenorFieldsOf(draft.tenorOpened);
    return TENOR_FIELDS.some((field) => draft.tenor[field] !== before[field]);
}

function conventionChanged(draft: TenorDraft): boolean {
    if (!draft.conventionActive) {
        return false;
    }
    if (draft.conventionOpened === undefined) {
        return true;
    }
    const before = conventionFieldsOf(draft.conventionOpened);
    return CONVENTION_FIELDS.some((field) => draft.convention[field] !== before[field]);
}

/** The members whose place in the set or whose anchor override differs from what was read. */
function memberChanges(draft: TenorDraft): readonly Member[] {
    return draft.members.filter(
        (member) =>
            member.wanted !== (member.original !== undefined) ||
            (member.wanted &&
                member.original !== undefined &&
                member.anchorOverride !== (member.original.anchor_override ?? '')),
    );
}

/** Every difference from what was read, as the review draws it. */
export function changesOf(draft: TenorDraft): readonly Change[] {
    const changes: Change[] = [];
    if (draft.tenorActive) {
        const before =
            draft.tenorOpened === undefined ? blankTenor() : tenorFieldsOf(draft.tenorOpened);
        if (draft.tenorOpened === undefined) {
            changes.push({
                id: 'tenor:new',
                what: 'journey.tenors.review.what.newTenor',
                part: undefined,
                before: { kind: 'absent' },
                after: asValue(draft.tenor.code),
                operation: 'refdata.v1.tenors.put',
            });
        }
        for (const field of TENOR_FIELDS) {
            if (draft.tenor[field] !== before[field]) {
                changes.push({
                    id: `tenor:${field}`,
                    what: 'journey.tenors.review.what.tenor',
                    part: `journey.tenors.tenor.fields.${field}`,
                    before: asValue(before[field]),
                    after: asValue(draft.tenor[field]),
                    operation: 'refdata.v1.tenors.put',
                });
            }
        }
    }
    if (draft.conventionActive) {
        const before =
            draft.conventionOpened === undefined
                ? blankConvention()
                : conventionFieldsOf(draft.conventionOpened);
        if (draft.conventionOpened === undefined) {
            changes.push({
                id: 'convention:new',
                what: 'journey.tenors.review.what.newConvention',
                part: undefined,
                before: { kind: 'absent' },
                after: asValue(draft.convention.code),
                operation: 'refdata.v1.tenor_conventions.put',
            });
        }
        for (const field of CONVENTION_FIELDS) {
            if (draft.convention[field] !== before[field]) {
                changes.push({
                    id: `convention:${field}`,
                    what: 'journey.tenors.review.what.convention',
                    part: `journey.tenors.convention.fields.${field}`,
                    before: asValue(before[field]),
                    after: asValue(draft.convention[field]),
                    operation: 'refdata.v1.tenor_conventions.put',
                });
            }
        }
        for (const member of memberChanges(draft)) {
            const was = member.original !== undefined;
            changes.push({
                id: `member:${member.tenorCode}`,
                what: 'journey.tenors.review.what.member',
                part: undefined,
                before: was ? present(member.tenorCode) : absentFromSet(member.tenorCode),
                after: member.wanted ? present(member.tenorCode) : absentFromSet(member.tenorCode),
                operation: member.wanted
                    ? 'refdata.v1.tenor_convention_resolutions.put'
                    : 'refdata.v1.tenor_convention_resolutions.delete',
            });
        }
    }
    return changes;
}

/** What one side of a change shows, as the catalogue draws it. */
export interface ChangeSide {
    /** A value read, drawn as it stands. */
    readonly value?: string;
    /** A catalogue key, drawn with `params`. */
    readonly key?: string;
    readonly params?: Readonly<Record<string, string>>;
}

/** A change as the review row draws it, every word reached through the catalogue. */
export interface ChangeText {
    readonly whatKey: string;
    readonly whatParams: Readonly<Record<string, string>>;
    readonly before: ChangeSide;
    readonly after: ChangeSide;
}

function sideOf(value: ChangeValue): ChangeSide {
    switch (value.kind) {
        case 'absent':
            return { key: 'journey.tenors.review.absent' };
        case 'value':
            return { value: value.value };
        case 'state':
            return {
                key:
                    value.state === 'inSet'
                        ? 'journey.tenors.convention.inSet'
                        : 'journey.tenors.convention.notInSet',
                params: { tenor: value.tenorCode },
            };
    }
}

/** A change as the catalogue keys and values that draw its row. */
export function changeText(change: Change): ChangeText {
    return {
        whatKey: change.what,
        whatParams: change.part === undefined ? {} : { part: change.part },
        before: sideOf(change.before),
        after: sideOf(change.after),
    };
}

const whole = /^-?\d+$/;

/** A reason the draft cannot be written yet, named by the catalogue key that says so. */
export interface Problem {
    readonly what: string;
}

/** What the draft still lacks, as catalogue keys the form renders. */
export function tenorProblems(draft: TenorDraft): readonly Problem[] {
    const problems: Problem[] = [];
    if (draft.tenorActive) {
        const t = draft.tenor;
        if (t.code.trim() === '' || t.displayName.trim() === '') {
            problems.push({ what: 'journey.tenors.problems.nameTenor' });
        }
        if (t.kind === '' || t.unit === '') {
            problems.push({ what: 'journey.tenors.problems.chooseKindAndUnit' });
        }
        if (!whole.test(t.sortOrder)) {
            problems.push({ what: 'journey.tenors.problems.sortOrderWhole' });
        }
        if (t.multiplier !== '' && !whole.test(t.multiplier)) {
            problems.push({ what: 'journey.tenors.problems.multiplierWhole' });
        }
    }
    if (draft.conventionActive) {
        const c = draft.convention;
        if (c.code.trim() === '') {
            problems.push({ what: 'journey.tenors.problems.nameConvention' });
        }
        if (c.measuredFrom === '' || c.resolutionAlgorithm === '' || c.rollRule === '') {
            problems.push({ what: 'journey.tenors.problems.chooseAnchorAlgorithmRoll' });
        }
    }
    return problems;
}

/** What the confirm sends, in the order it sends it. */
export interface WritePlan {
    readonly intent: TenorIntent;
    /** The tenor row and the version it was read at, or null for a new tenor. */
    readonly tenor: { readonly write: Tenor; readonly version: number | null } | undefined;
    readonly convention:
        { readonly write: TenorConvention; readonly version: number | null } | undefined;
    readonly links: readonly {
        readonly tenorCode: string;
        readonly anchorOverride: string | null;
        readonly version: number | null;
    }[];
    readonly unlinks: readonly { readonly tenorCode: string; readonly version: number }[];
}

function numberOrNull(text: string): number | null {
    return text.trim() === '' ? null : Number(text);
}

/** The audit fields a write carries empty: the service sets tenancy, provenance and the validity window. */
const STAMP = {
    version: 0,
    tenant_id: '',
    modified_by: '',
    performed_by: '',
    change_reason_code: '',
    change_commentary: '',
    recorded_at: '',
} as const;

/** The plan the confirm runs, or undefined before anything is being shaped. */
export function writePlan(draft: TenorDraft): WritePlan | undefined {
    if (!draft.tenorActive && !draft.conventionActive) {
        return undefined;
    }
    const t = draft.tenor;
    const c = draft.convention;
    const changed = memberChanges(draft);
    return {
        intent: { reason_code: draft.reasonCode, commentary: draft.commentary },
        tenor: tenorChanged(draft)
            ? {
                  write: {
                      ...STAMP,
                      ...(draft.tenorOpened ?? {}),
                      code: draft.tenorOpened?.code ?? t.code.trim(),
                      display_name: t.displayName.trim(),
                      description: t.description,
                      sort_order: Number(t.sortOrder),
                      kind: t.kind,
                      unit: t.unit,
                      multiplier: numberOrNull(t.multiplier),
                  },
                  version: draft.tenorOpened === undefined ? null : draft.tenorOpened.version,
              }
            : undefined,
        convention: conventionChanged(draft)
            ? {
                  write: {
                      ...STAMP,
                      ...(draft.conventionOpened ?? {}),
                      code: draft.conventionOpened?.code ?? c.code.trim(),
                      description: c.description,
                      measured_from: c.measuredFrom,
                      resolution_algorithm: c.resolutionAlgorithm,
                      business_day_convention_type: c.rollRule,
                      oresmd_uri: c.oresmdUri.trim() === '' ? null : c.oresmdUri,
                  },
                  version:
                      draft.conventionOpened === undefined ? null : draft.conventionOpened.version,
              }
            : undefined,
        links: changed
            .filter((member) => member.wanted)
            .map((member) => ({
                tenorCode: member.tenorCode,
                anchorOverride: member.anchorOverride === '' ? null : member.anchorOverride,
                version: member.original === undefined ? null : member.original.version,
            })),
        unlinks: changed.flatMap((member) =>
            !member.wanted && member.original !== undefined
                ? [{ tenorCode: member.tenorCode, version: member.original.version }]
                : [],
        ),
    };
}

export interface TenorDetails extends TenorDraft {
    readonly changes: readonly Change[];
    readonly openTenor: (tenor: Tenor) => void;
    readonly startTenor: () => void;
    readonly setTenorField: (field: TenorField, value: string) => void;
    readonly openConvention: (
        convention: TenorConvention,
        resolutions: readonly TenorConventionResolution[],
        tenorCodes: readonly string[],
    ) => void;
    readonly startConvention: (tenorCodes: readonly string[]) => void;
    readonly setConventionField: (field: ConventionField, value: string) => void;
    readonly toggleMember: (tenorCode: string) => void;
    readonly setAnchor: (tenorCode: string, value: string) => void;
    readonly setReason: (reasonCode: string) => void;
    readonly setCommentary: (commentary: string) => void;
    readonly recordWritten: () => void;
    readonly recordRefusal: (refusal: TenorRefusal) => void;
    readonly clearRefusal: () => void;
}

export function useTenor(): TenorDetails {
    const [draft, dispatch] = useReducer(reduceTenor, undefined, blankDraft);
    return {
        ...draft,
        changes: changesOf(draft),
        openTenor: (tenor) => dispatch({ kind: 'open-tenor', tenor }),
        startTenor: () => dispatch({ kind: 'start-tenor' }),
        setTenorField: (field, value) => dispatch({ kind: 'set-tenor-field', field, value }),
        openConvention: (convention, resolutions, tenorCodes) =>
            dispatch({ kind: 'open-convention', convention, resolutions, tenorCodes }),
        startConvention: (tenorCodes) => dispatch({ kind: 'start-convention', tenorCodes }),
        setConventionField: (field, value) =>
            dispatch({ kind: 'set-convention-field', field, value }),
        toggleMember: (tenorCode) => dispatch({ kind: 'toggle-member', tenorCode }),
        setAnchor: (tenorCode, value) => dispatch({ kind: 'set-anchor', tenorCode, value }),
        setReason: (reasonCode) => dispatch({ kind: 'set-reason', reasonCode }),
        setCommentary: (commentary) => dispatch({ kind: 'set-commentary', commentary }),
        recordWritten: () => dispatch({ kind: 'record-written' }),
        recordRefusal: (refusal) => dispatch({ kind: 'record-refusal', refusal }),
        clearRefusal: () => dispatch({ kind: 'clear-refusal' }),
    };
}
