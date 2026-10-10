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
 * The party details journey's draft, and what the confirm makes of it.
 *
 * A party is corrected, never replaced, so the draft starts from the party as
 * read and records only the person's differences from it. The review is a
 * function of those differences, and so is the write: nothing is sent for a
 * field the person did not change. The reductions are pure, as the other
 * journeys' are, so a test walks the same ones the screens do.
 *
 * An identifier's value is part of its key, so a changed value is two acts, a
 * retire of the old row and a write of the new one, and the plan keeps them
 * apart. The composite carries the party and the rows to write; the retires,
 * the membership links and the unit reclassifications are their own calls.
 */

import { useReducer } from 'react';
import type { BusinessUnit } from '@ores/wire-protocol/generated/refdata/domain/business_unit';
import type { Party } from '@ores/wire-protocol/generated/refdata/domain/party';
import type { PartyContactInformation } from '@ores/wire-protocol/generated/refdata/domain/party_contact_information';
import type { PartyIdentifier } from '@ores/wire-protocol/generated/refdata/domain/party_identifier';
import type { BusinessUnitType } from '@ores/wire-protocol/generated/refdata/domain/business_unit_type';
import type { PartyCompositeRequest, PartyIntent, PartySets } from './partyDetailsServer.js';

/** The reason a correction is filed under until the person states another. */
export const CORRECTION_REASON = 'common.non_material_update';

export const PARTY_ENTITY_TYPE = 'ores.refdata.party';
export const PARTY_IDENTIFIER_ENTITY_TYPE = 'ores.refdata.party_identifier';
export const PARTY_CONTACT_ENTITY_TYPE = 'ores.refdata.party_contact_information';

/** The party fields the person edits. The codename and the category are read only. */
export interface PartyFields {
    readonly shortCode: string;
    readonly fullName: string;
    readonly transliteratedName: string;
    readonly partyType: string;
    readonly status: string;
    readonly parentPartyId: string;
    readonly businessCenterCode: string;
    readonly isRegistrationDefault: boolean;
}

export type PartyField = Exclude<keyof PartyFields, 'isRegistrationDefault'>;

/** One identifier the draft holds: the row as read, if any, and what it is now. */
export interface IdentifierRow {
    readonly key: string;
    readonly original: PartyIdentifier | undefined;
    readonly scheme: string;
    readonly value: string;
    readonly description: string;
    readonly retired: boolean;
}

/** One contact the draft holds: one row per contact type. */
export interface ContactRow {
    readonly contactType: string;
    readonly original: PartyContactInformation | undefined;
    readonly streetLine1: string;
    readonly streetLine2: string;
    readonly city: string;
    readonly state: string;
    readonly countryCode: string;
    readonly postalCode: string;
    readonly phone: string;
    readonly email: string;
    readonly webPage: string;
    readonly isPrimary: boolean;
}

export type ContactField = Exclude<keyof ContactRow, 'contactType' | 'original' | 'isPrimary'>;

/** A membership or a link: whether it was open when read and whether the person wants it open. */
export interface Membership {
    readonly code: string;
    readonly original: boolean;
    readonly wanted: boolean;
}

/** A unit and the type the person chose for it. */
export interface UnitRow {
    readonly unit: BusinessUnit;
    readonly unitTypeId: string | null;
}

/** What the server refused, kept so the review can name it and the person can walk back. */
export interface PartyRefusal {
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

export interface PartyDraft {
    readonly opened: Party | undefined;
    readonly fields: PartyFields;
    readonly identifiers: readonly IdentifierRow[];
    readonly contacts: readonly ContactRow[];
    readonly countries: readonly Membership[];
    readonly currencies: readonly Membership[];
    readonly counterparties: readonly Membership[];
    readonly units: readonly UnitRow[];
    readonly reasonCode: string;
    readonly commentary: string;
    readonly refusal: PartyRefusal | undefined;
    readonly written: Party | undefined;
}

export function blankFields(): PartyFields {
    return {
        shortCode: '',
        fullName: '',
        transliteratedName: '',
        partyType: '',
        status: '',
        parentPartyId: '',
        businessCenterCode: '',
        isRegistrationDefault: false,
    };
}

export function blankDraft(): PartyDraft {
    return {
        opened: undefined,
        fields: blankFields(),
        identifiers: [],
        contacts: [],
        countries: [],
        currencies: [],
        counterparties: [],
        units: [],
        reasonCode: CORRECTION_REASON,
        commentary: '',
        refusal: undefined,
        written: undefined,
    };
}

function fieldsOf(party: Party): PartyFields {
    return {
        shortCode: party.short_code,
        fullName: party.full_name,
        transliteratedName: party.transliterated_name ?? '',
        partyType: party.party_type,
        status: party.status,
        parentPartyId: party.parent_party_id ?? '',
        businessCenterCode: party.business_center_code,
        isRegistrationDefault: party.is_registration_default,
    };
}

function contactRowOf(contact: PartyContactInformation): ContactRow {
    return {
        contactType: contact.contact_type,
        original: contact,
        streetLine1: contact.street_line_1,
        streetLine2: contact.street_line_2,
        city: contact.city,
        state: contact.state,
        countryCode: contact.country_code,
        postalCode: contact.postal_code,
        phone: contact.phone,
        email: contact.email,
        webPage: contact.web_page,
        isPrimary: contact.is_primary,
    };
}

/** The draft of a party opened from the list, with the sets read for it. */
export function draftFrom(party: Party, sets: PartySets): PartyDraft {
    return {
        ...blankDraft(),
        opened: party,
        fields: fieldsOf(party),
        identifiers: sets.identifiers.map((identifier) => ({
            key: identifier.id,
            original: identifier,
            scheme: identifier.id_scheme,
            value: identifier.id_value,
            description: identifier.description,
            retired: false,
        })),
        contacts: sets.contacts.map(contactRowOf),
        countries: sets.countries.map((row) => ({
            code: row.country_alpha2_code,
            original: true,
            wanted: true,
        })),
        currencies: sets.currencies.map((row) => ({
            code: row.currency_iso_code,
            original: true,
            wanted: true,
        })),
        counterparties: sets.counterparties.map((row) => ({
            code: row.counterparty_id,
            original: true,
            wanted: true,
        })),
        units: sets.units.map((unit) => ({ unit, unitTypeId: unit.unit_type_id })),
    };
}

export type PartyAction =
    | { readonly kind: 'open'; readonly party: Party; readonly sets: PartySets }
    | { readonly kind: 'set-field'; readonly field: PartyField; readonly value: string }
    | { readonly kind: 'set-default'; readonly value: boolean }
    | {
          readonly kind: 'add-identifier';
          readonly scheme: string;
          readonly value: string;
          readonly description: string;
      }
    | { readonly kind: 'set-identifier'; readonly key: string; readonly value: string }
    | { readonly kind: 'set-identifier-description'; readonly key: string; readonly value: string }
    | { readonly kind: 'retire-identifier'; readonly key: string; readonly retired: boolean }
    | { readonly kind: 'add-contact'; readonly contactType: string }
    | {
          readonly kind: 'set-contact';
          readonly contactType: string;
          readonly field: ContactField;
          readonly value: string;
      }
    | { readonly kind: 'mark-primary'; readonly contactType: string }
    | {
          readonly kind: 'toggle-membership';
          readonly set: 'countries' | 'currencies' | 'counterparties';
          readonly code: string;
      }
    | {
          readonly kind: 'set-unit-type';
          readonly unitId: string;
          readonly unitTypeId: string | null;
      }
    | { readonly kind: 'revert-fields'; readonly party: Party }
    | { readonly kind: 'set-reason'; readonly reasonCode: string }
    | { readonly kind: 'set-commentary'; readonly commentary: string }
    | { readonly kind: 'record-written'; readonly party: Party | undefined }
    | { readonly kind: 'record-refusal'; readonly refusal: PartyRefusal }
    | { readonly kind: 'clear-refusal' };

function blankContact(contactType: string): ContactRow {
    return {
        contactType,
        original: undefined,
        streetLine1: '',
        streetLine2: '',
        city: '',
        state: '',
        countryCode: '',
        postalCode: '',
        phone: '',
        email: '',
        webPage: '',
        isPrimary: false,
    };
}

function toggled(rows: readonly Membership[], code: string): readonly Membership[] {
    return rows.some((row) => row.code === code)
        ? rows.map((row) => (row.code === code ? { ...row, wanted: !row.wanted } : row))
        : [...rows, { code, original: false, wanted: true }];
}

export function reduceParty(draft: PartyDraft, action: PartyAction): PartyDraft {
    switch (action.kind) {
        case 'open':
            return draftFrom(action.party, action.sets);
        case 'set-field':
            return { ...draft, fields: { ...draft.fields, [action.field]: action.value } };
        case 'set-default':
            return { ...draft, fields: { ...draft.fields, isRegistrationDefault: action.value } };
        case 'add-identifier':
            return {
                ...draft,
                identifiers: [
                    ...draft.identifiers,
                    {
                        key: crypto.randomUUID(),
                        original: undefined,
                        scheme: action.scheme,
                        value: action.value,
                        description: action.description,
                        retired: false,
                    },
                ],
            };
        case 'set-identifier':
            return {
                ...draft,
                identifiers: draft.identifiers.map((row) =>
                    row.key === action.key ? { ...row, value: action.value } : row,
                ),
            };
        case 'set-identifier-description':
            return {
                ...draft,
                identifiers: draft.identifiers.map((row) =>
                    row.key === action.key ? { ...row, description: action.value } : row,
                ),
            };
        case 'retire-identifier':
            return {
                ...draft,
                identifiers: draft.identifiers.flatMap((row) => {
                    if (row.key !== action.key) {
                        return [row];
                    }
                    // A row the server never held is dropped; one it holds is retired.
                    return row.original === undefined ? [] : [{ ...row, retired: action.retired }];
                }),
            };
        case 'add-contact':
            return draft.contacts.some((row) => row.contactType === action.contactType)
                ? draft
                : { ...draft, contacts: [...draft.contacts, blankContact(action.contactType)] };
        case 'set-contact':
            return {
                ...draft,
                contacts: draft.contacts.map((row) =>
                    row.contactType === action.contactType
                        ? { ...row, [action.field]: action.value }
                        : row,
                ),
            };
        case 'mark-primary':
            return {
                ...draft,
                contacts: draft.contacts.map((row) => ({
                    ...row,
                    isPrimary: row.contactType === action.contactType,
                })),
            };
        case 'toggle-membership':
            return { ...draft, [action.set]: toggled(draft[action.set], action.code) };
        case 'set-unit-type':
            return {
                ...draft,
                units: draft.units.map((row) =>
                    row.unit.id === action.unitId ? { ...row, unitTypeId: action.unitTypeId } : row,
                ),
            };
        case 'revert-fields':
            return { ...draft, fields: fieldsOf(action.party) };
        case 'set-reason':
            return { ...draft, reasonCode: action.reasonCode };
        case 'set-commentary':
            return { ...draft, commentary: action.commentary };
        case 'record-written':
            return { ...draft, written: action.party, refusal: undefined };
        case 'record-refusal':
            return { ...draft, refusal: action.refusal };
        case 'clear-refusal':
            return { ...draft, refusal: undefined };
    }
}

/** The ceiling on how many values one scheme may hold, or none when the scheme is silent. */
export function cardinalityProblems(
    draft: Pick<PartyDraft, 'identifiers'>,
    maxCardinality: (scheme: string) => number | null,
): readonly string[] {
    const problems: string[] = [];
    const held = draft.identifiers.filter((row) => !row.retired);
    if (held.some((row) => row.value.trim() === '')) {
        problems.push('Every identifier needs a value.');
    }
    for (const scheme of new Set(held.map((row) => row.scheme))) {
        const limit = maxCardinality(scheme);
        const count = held.filter((row) => row.scheme === scheme).length;
        if (limit !== null && count > limit) {
            problems.push(
                `${scheme} holds ${String(limit)} value at most, and this has ${String(count)}.`,
            );
        }
    }
    return problems;
}

/** One line of the review: what changes, before, after, and the operation that writes it. */
export interface Change {
    readonly id: string;
    readonly what: string;
    readonly before: string;
    readonly after: string;
    readonly operation: string;
}

const PARTY_FIELD_LABELS: readonly (readonly [PartyField | 'isRegistrationDefault', string])[] = [
    ['shortCode', 'Short code'],
    ['fullName', 'Full name'],
    ['transliteratedName', 'Transliterated name'],
    ['partyType', 'Party type'],
    ['status', 'Status'],
    ['parentPartyId', 'Parent party'],
    ['businessCenterCode', 'Business center'],
    ['isRegistrationDefault', 'Registration default'],
];

function show(value: string | boolean): string {
    if (typeof value === 'boolean') {
        return value ? 'yes' : 'no';
    }
    return value === '' ? '-' : value;
}

const CONTACT_FIELDS: readonly (readonly [ContactField, string])[] = [
    ['streetLine1', 'street'],
    ['streetLine2', 'street 2'],
    ['city', 'city'],
    ['state', 'state'],
    ['countryCode', 'country'],
    ['postalCode', 'postal code'],
    ['phone', 'phone'],
    ['email', 'email'],
    ['webPage', 'web page'],
];

function contactChanged(row: ContactRow): boolean {
    if (row.original === undefined) {
        return true;
    }
    const before = contactRowOf(row.original);
    return (
        CONTACT_FIELDS.some(([field]) => row[field] !== before[field]) ||
        row.isPrimary !== before.isPrimary
    );
}

function identifierWrites(row: IdentifierRow): boolean {
    return (
        !row.retired &&
        (row.original === undefined ||
            row.value.trim() !== row.original.id_value ||
            row.description !== row.original.description)
    );
}

function identifierRetires(row: IdentifierRow): boolean {
    return (
        row.original !== undefined &&
        (row.retired || (!row.retired && row.value.trim() !== row.original.id_value))
    );
}

/** Every difference from the party as read, as the review draws it. */
export function changesOf(draft: PartyDraft): readonly Change[] {
    const opened = draft.opened;
    if (opened === undefined) {
        return [];
    }
    const before = fieldsOf(opened);
    const changes: Change[] = [];
    for (const [field, label] of PARTY_FIELD_LABELS) {
        if (draft.fields[field] !== before[field]) {
            changes.push({
                id: `party:${field}`,
                what: label,
                before: show(before[field]),
                after: show(draft.fields[field]),
                operation: 'refdata.v1.ops.put_party_composite',
            });
        }
    }
    for (const row of draft.identifiers) {
        if (identifierRetires(row) && row.original !== undefined) {
            changes.push({
                id: `identifier-retire:${row.key}`,
                what: `${row.scheme} identifier`,
                before: row.original.id_value,
                after: row.retired ? 'retired' : 'replaced',
                operation: 'refdata.v1.party_identifiers.delete',
            });
        }
        if (identifierWrites(row)) {
            changes.push({
                id: `identifier-write:${row.key}`,
                what: `${row.scheme} identifier`,
                before: row.original === undefined ? '-' : row.original.id_value,
                after: row.value.trim(),
                operation: 'refdata.v1.ops.put_party_composite',
            });
        }
    }
    for (const row of draft.contacts) {
        if (!contactChanged(row)) {
            continue;
        }
        const original = row.original === undefined ? undefined : contactRowOf(row.original);
        for (const [field, label] of CONTACT_FIELDS) {
            const was = original === undefined ? '' : original[field];
            if (row[field] !== was) {
                changes.push({
                    id: `contact:${row.contactType}:${field}`,
                    what: `${row.contactType} ${label}`,
                    before: show(was),
                    after: show(row[field]),
                    operation: 'refdata.v1.ops.put_party_composite',
                });
            }
        }
        if (row.isPrimary !== (original?.isPrimary ?? false)) {
            changes.push({
                id: `contact:${row.contactType}:primary`,
                what: `${row.contactType} primary`,
                before: show(original?.isPrimary ?? false),
                after: show(row.isPrimary),
                operation: 'refdata.v1.ops.put_party_composite',
            });
        }
    }
    const memberships = [
        ['countries', 'Country', 'refdata.v1.party_countries'],
        ['currencies', 'Currency', 'refdata.v1.party_currencies'],
        ['counterparties', 'Counterparty link', 'refdata.v1.party_counterparties'],
    ] as const;
    for (const [set, label, subject] of memberships) {
        for (const row of draft[set]) {
            if (row.original === row.wanted) {
                continue;
            }
            changes.push({
                id: `${set}:${row.code}`,
                what: `${label} ${row.code}`,
                before: row.original ? 'open' : 'none',
                after: row.wanted ? 'open' : 'closed',
                operation: row.wanted ? `${subject}.put` : `${subject}.delete`,
            });
        }
    }
    for (const row of draft.units) {
        if (row.unitTypeId !== row.unit.unit_type_id) {
            changes.push({
                id: `unit:${row.unit.id}`,
                what: `Unit ${row.unit.unit_code} type`,
                before: show(row.unit.unit_type_id ?? ''),
                after: show(row.unitTypeId ?? ''),
                operation: 'refdata.v1.business_units.put',
            });
        }
    }
    return changes;
}

/** The party the composite writes: the party as read, with the person's edits. */
function writtenParty(draft: PartyDraft, opened: Party): Party {
    const fields = draft.fields;
    return {
        ...opened,
        short_code: fields.shortCode.trim(),
        full_name: fields.fullName.trim(),
        transliterated_name:
            fields.transliteratedName.trim() === '' ? null : fields.transliteratedName,
        party_type: fields.partyType,
        status: fields.status,
        parent_party_id: fields.parentPartyId === '' ? null : fields.parentPartyId,
        business_center_code: fields.businessCenterCode,
        is_registration_default: fields.isRegistrationDefault,
    };
}

function stamp(): Pick<
    PartyIdentifier,
    | 'version'
    | 'tenant_id'
    | 'modified_by'
    | 'performed_by'
    | 'change_reason_code'
    | 'change_commentary'
    | 'recorded_at'
> {
    return {
        version: 0,
        tenant_id: '',
        modified_by: '',
        performed_by: '',
        change_reason_code: '',
        change_commentary: '',
        recorded_at: '',
    };
}

/** What the confirm sends, in the order it sends it. */
export interface WritePlan {
    readonly composite: PartyCompositeRequest;
    /** Identifiers whose old value is retired, with the version read. */
    readonly retires: readonly { readonly value: string; readonly version: number }[];
    readonly links: readonly {
        readonly set: 'countries' | 'currencies' | 'counterparties';
        readonly code: string;
        readonly open: boolean;
    }[];
    readonly units: readonly UnitRow[];
}

/**
 * The plan the confirm runs, or undefined before a party is open.
 *
 * The composite carries the party always, because a child write bumps the
 * party's version on its own and the party must state the version it read.
 */
export function writePlan(draft: PartyDraft): WritePlan | undefined {
    const opened = draft.opened;
    if (opened === undefined) {
        return undefined;
    }
    const intent: PartyIntent = { reason_code: draft.reasonCode, commentary: draft.commentary };
    const identifiers: PartyIdentifier[] = draft.identifiers
        .filter(identifierWrites)
        .map((row) => ({
            ...stamp(),
            ...(row.original ?? {}),
            id:
                row.original !== undefined && row.value.trim() === row.original.id_value
                    ? row.original.id
                    : crypto.randomUUID(),
            party_id: opened.id,
            id_scheme: row.scheme,
            id_value: row.value.trim(),
            description: row.description,
        }));
    const contacts: PartyContactInformation[] = draft.contacts
        .filter(contactChanged)
        .map((row) => ({
            ...stamp(),
            ...(row.original ?? {}),
            id: row.original?.id ?? crypto.randomUUID(),
            party_id: opened.id,
            contact_type: row.contactType,
            street_line_1: row.streetLine1,
            street_line_2: row.streetLine2,
            city: row.city,
            state: row.state,
            country_code: row.countryCode,
            postal_code: row.postalCode,
            phone: row.phone,
            email: row.email,
            web_page: row.webPage,
            is_primary: row.isPrimary,
        }));
    const links = (['countries', 'currencies', 'counterparties'] as const).flatMap((set) =>
        draft[set]
            .filter((row) => row.original !== row.wanted)
            .map((row) => ({ set, code: row.code, open: row.wanted })),
    );
    return {
        composite: { intent, party: writtenParty(draft, opened), identifiers, contacts },
        retires: draft.identifiers
            .filter(identifierRetires)
            .flatMap((row) =>
                row.original === undefined
                    ? []
                    : [{ value: row.original.id_value, version: row.original.version }],
            ),
        links,
        units: draft.units.filter((row) => row.unitTypeId !== row.unit.unit_type_id),
    };
}

/** The business unit types ordered by level, so the picker reads shallow to deep. */
export function typesByLevel(types: readonly BusinessUnitType[]): readonly BusinessUnitType[] {
    return [...types].sort((left, right) => left.level - right.level);
}

/**
 * A unit whose chosen type breaks the level rule against its parent, or
 * undefined. The server refuses it; the screen says so before the write.
 */
export function levelBreach(
    row: UnitRow,
    units: readonly UnitRow[],
    types: readonly BusinessUnitType[],
): { readonly parent: UnitRow; readonly level: number; readonly parentLevel: number } | undefined {
    const levelOf = (id: string | null): number | undefined =>
        types.find((type) => type.id === id)?.level;
    const parent = units.find((other) => other.unit.id === row.unit.parent_business_unit_id);
    const level = levelOf(row.unitTypeId);
    const parentLevel = parent === undefined ? undefined : levelOf(parent.unitTypeId);
    if (parent === undefined || level === undefined || parentLevel === undefined) {
        return undefined;
    }
    return level > parentLevel ? undefined : { parent, level, parentLevel };
}

export interface PartyDetails extends PartyDraft {
    readonly changes: readonly Change[];
    readonly open: (party: Party, sets: PartySets) => void;
    readonly setField: (field: PartyField, value: string) => void;
    readonly setDefault: (value: boolean) => void;
    readonly addIdentifier: (scheme: string, value: string, description: string) => void;
    readonly setIdentifier: (key: string, value: string) => void;
    readonly setIdentifierDescription: (key: string, value: string) => void;
    readonly retireIdentifier: (key: string, retired: boolean) => void;
    readonly addContact: (contactType: string) => void;
    readonly setContact: (contactType: string, field: ContactField, value: string) => void;
    readonly markPrimary: (contactType: string) => void;
    readonly toggleMembership: (
        set: 'countries' | 'currencies' | 'counterparties',
        code: string,
    ) => void;
    readonly setUnitType: (unitId: string, unitTypeId: string | null) => void;
    readonly revertFields: (party: Party) => void;
    readonly setReason: (reasonCode: string) => void;
    readonly setCommentary: (commentary: string) => void;
    readonly recordWritten: (party: Party | undefined) => void;
    readonly recordRefusal: (refusal: PartyRefusal) => void;
    readonly clearRefusal: () => void;
}

export function useParty(): PartyDetails {
    const [draft, dispatch] = useReducer(reduceParty, undefined, blankDraft);
    return {
        ...draft,
        changes: changesOf(draft),
        open: (party, sets) => dispatch({ kind: 'open', party, sets }),
        setField: (field, value) => dispatch({ kind: 'set-field', field, value }),
        setDefault: (value) => dispatch({ kind: 'set-default', value }),
        addIdentifier: (scheme, value, description) =>
            dispatch({ kind: 'add-identifier', scheme, value, description }),
        setIdentifier: (key, value) => dispatch({ kind: 'set-identifier', key, value }),
        setIdentifierDescription: (key, value) =>
            dispatch({ kind: 'set-identifier-description', key, value }),
        retireIdentifier: (key, retired) => dispatch({ kind: 'retire-identifier', key, retired }),
        addContact: (contactType) => dispatch({ kind: 'add-contact', contactType }),
        setContact: (contactType, field, value) =>
            dispatch({ kind: 'set-contact', contactType, field, value }),
        markPrimary: (contactType) => dispatch({ kind: 'mark-primary', contactType }),
        toggleMembership: (set, code) => dispatch({ kind: 'toggle-membership', set, code }),
        setUnitType: (unitId, unitTypeId) =>
            dispatch({ kind: 'set-unit-type', unitId, unitTypeId }),
        revertFields: (party) => dispatch({ kind: 'revert-fields', party }),
        setReason: (reasonCode) => dispatch({ kind: 'set-reason', reasonCode }),
        setCommentary: (commentary) => dispatch({ kind: 'set-commentary', commentary }),
        recordWritten: (party) => dispatch({ kind: 'record-written', party }),
        recordRefusal: (refusal) => dispatch({ kind: 'record-refusal', refusal }),
        clearRefusal: () => dispatch({ kind: 'clear-refusal' }),
    };
}
