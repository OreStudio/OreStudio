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

import { describe, expect, it } from 'vitest';
import type { BusinessUnit } from '@ores/wire-protocol/generated/refdata/domain/business_unit';
import type { BusinessUnitType } from '@ores/wire-protocol/generated/refdata/domain/business_unit_type';
import type { Party } from '@ores/wire-protocol/generated/refdata/domain/party';
import type { PartyContactInformation } from '@ores/wire-protocol/generated/refdata/domain/party_contact_information';
import type { PartyIdentifier } from '@ores/wire-protocol/generated/refdata/domain/party_identifier';
import type { PartySets } from './partyDetailsServer.js';
import {
    blankDraft,
    cardinalityProblems,
    changesOf,
    levelBreach,
    reduceParty,
    writePlan,
    type PartyAction,
    type PartyDraft,
} from './partyDetailsState.js';

const audit = {
    tenant_id: 't',
    modified_by: 'm',
    performed_by: 'p',
    change_reason_code: 'r',
    change_commentary: '',
    recorded_at: '2026-01-01T00:00:00Z',
};

const party: Party = {
    ...audit,
    version: 3,
    id: 'party-1',
    short_code: 'ACME',
    full_name: 'Acme Ltd',
    codename: 'swift-otter',
    transliterated_name: null,
    party_category: 'Operational',
    party_type: 'Corporate',
    parent_party_id: null,
    business_center_code: 'GBLO',
    status: 'Active',
    image_id: null,
    is_registration_default: false,
};

const identifier: PartyIdentifier = {
    ...audit,
    version: 2,
    id: 'id-1',
    party_id: 'party-1',
    id_scheme: 'LEI',
    id_value: 'LEI-OLD',
    description: '',
};

const contact: PartyContactInformation = {
    ...audit,
    version: 1,
    id: 'c-1',
    party_id: 'party-1',
    contact_type: 'Legal',
    street_line_1: '1 High St',
    street_line_2: '',
    city: 'London',
    state: '',
    country_code: 'GB',
    postal_code: 'E1',
    phone: '1',
    email: 'a@acme.example',
    web_page: '',
    is_primary: true,
};

const unit = (id: string, parent: string | null, type: string | null): BusinessUnit => ({
    ...audit,
    version: 5,
    id,
    party_id: 'party-1',
    unit_name: id,
    parent_business_unit_id: parent,
    unit_code: id.toUpperCase(),
    business_centre_code: 'GBLO',
    unit_type_id: type,
    status: 'Active',
});

const sets: PartySets = {
    identifiers: [identifier],
    contacts: [contact],
    countries: [{ ...audit, version: 1, party_id: 'party-1', country_alpha2_code: 'GB' }],
    currencies: [{ ...audit, version: 1, party_id: 'party-1', currency_iso_code: 'GBP' }],
    counterparties: [],
    units: [unit('desk', 'division', 'type-desk'), unit('division', null, 'type-div')],
};

function walk(...actions: readonly PartyAction[]): PartyDraft {
    return actions.reduce(reduceParty, reduceParty(blankDraft(), { kind: 'open', party, sets }));
}

describe('party details draft', () => {
    it('has no change until the person changes something', () => {
        expect(changesOf(walk())).toEqual([]);
    });

    it('records a field change with its before and after', () => {
        const changes = changesOf(
            walk({ kind: 'set-field', field: 'fullName', value: 'Acme plc' }),
        );
        expect(changes).toEqual([
            expect.objectContaining({
                what: 'Full name',
                before: 'Acme Ltd',
                after: 'Acme plc',
                operation: 'refdata.v1.ops.put_party_composite',
            }),
        ]);
    });

    it('retires the old identifier and writes a new row when the value changes', () => {
        const draft = walk({ kind: 'set-identifier', key: 'id-1', value: 'LEI-NEW' });
        const plan = writePlan(draft);
        expect(plan?.retires).toEqual([{ value: 'LEI-OLD', version: 2 }]);
        expect(plan?.composite.identifiers).toHaveLength(1);
        expect(plan?.composite.identifiers[0]?.id_value).toBe('LEI-NEW');
        expect(plan?.composite.identifiers[0]?.id).not.toBe('id-1');
        expect(changesOf(draft).map((c) => c.operation)).toEqual([
            'refdata.v1.party_identifiers.delete',
            'refdata.v1.ops.put_party_composite',
        ]);
    });

    it('keeps the identifier row when only its description changes', () => {
        const plan = writePlan(
            walk({ kind: 'set-identifier-description', key: 'id-1', value: 'Registry' }),
        );
        expect(plan?.retires).toEqual([]);
        expect(plan?.composite.identifiers[0]?.id).toBe('id-1');
        expect(plan?.composite.identifiers[0]?.version).toBe(2);
    });

    it('retires an identifier without writing a replacement', () => {
        const plan = writePlan(walk({ kind: 'retire-identifier', key: 'id-1', retired: true }));
        expect(plan?.retires).toHaveLength(1);
        expect(plan?.composite.identifiers).toEqual([]);
    });

    it('drops a new identifier the server never held when it is retired', () => {
        const draft = walk({
            kind: 'add-identifier',
            scheme: 'BIC',
            value: 'ACMEGB2L',
            description: '',
        });
        const key = draft.identifiers.find((row) => row.original === undefined)?.key ?? '';
        expect(
            reduceParty(draft, { kind: 'retire-identifier', key, retired: true }).identifiers,
        ).toHaveLength(1);
    });

    it('writes a contact row only when it differs from the one read', () => {
        expect(writePlan(walk())?.composite.contacts).toEqual([]);
        const plan = writePlan(
            walk({ kind: 'set-contact', contactType: 'Legal', field: 'phone', value: '2' }),
        );
        expect(plan?.composite.contacts).toHaveLength(1);
        expect(plan?.composite.contacts[0]).toMatchObject({ id: 'c-1', version: 1, phone: '2' });
    });

    it('moves the primary mark to one contact', () => {
        const draft = walk(
            { kind: 'add-contact', contactType: 'Billing' },
            { kind: 'mark-primary', contactType: 'Billing' },
        );
        expect(draft.contacts.map((row) => [row.contactType, row.isPrimary])).toEqual([
            ['Legal', false],
            ['Billing', true],
        ]);
    });

    it('closes an open membership and links a new one', () => {
        const draft = walk(
            { kind: 'toggle-membership', set: 'countries', code: 'GB' },
            { kind: 'toggle-membership', set: 'currencies', code: 'USD' },
        );
        expect(writePlan(draft)?.links).toEqual([
            { set: 'countries', code: 'GB', open: false },
            { set: 'currencies', code: 'USD', open: true },
        ]);
        const undone = reduceParty(draft, {
            kind: 'toggle-membership',
            set: 'countries',
            code: 'GB',
        });
        expect(writePlan(undone)?.links).toHaveLength(1);
    });

    it('plans a unit write only for a unit whose type moved', () => {
        const draft = walk({ kind: 'set-unit-type', unitId: 'desk', unitTypeId: 'type-div' });
        expect(writePlan(draft)?.units.map((row) => row.unit.id)).toEqual(['desk']);
    });

    it('states the version the party was read at in the composite', () => {
        expect(writePlan(walk())?.composite.party.version).toBe(3);
    });

    it('has no plan before a party is open', () => {
        expect(writePlan(blankDraft())).toBeUndefined();
    });

    it('carries the reason and the commentary on the composite intent', () => {
        const plan = writePlan(
            walk(
                { kind: 'set-reason', reasonCode: 'common.correction' },
                { kind: 'set-commentary', commentary: 'registry changed' },
            ),
        );
        expect(plan?.composite.intent).toEqual({
            reason_code: 'common.correction',
            commentary: 'registry changed',
        });
    });
});

describe('party details checks', () => {
    it('refuses a scheme over its cardinality', () => {
        const draft = walk({
            kind: 'add-identifier',
            scheme: 'LEI',
            value: 'LEI-2',
            description: '',
        });
        expect(cardinalityProblems(draft, () => 1)).toHaveLength(1);
        expect(cardinalityProblems(draft, () => null)).toEqual([]);
    });

    it('does not count a retired identifier', () => {
        const draft = walk(
            { kind: 'add-identifier', scheme: 'LEI', value: 'LEI-2', description: '' },
            { kind: 'retire-identifier', key: 'id-1', retired: true },
        );
        expect(cardinalityProblems(draft, () => 1)).toEqual([]);
    });

    it('finds a unit whose level is not deeper than its parent', () => {
        const types: BusinessUnitType[] = [
            {
                ...audit,
                version: 1,
                id: 'type-div',
                coding_scheme_code: '',
                code: 'DIV',
                name: 'Division',
                level: 1,
                description: '',
            },
            {
                ...audit,
                version: 1,
                id: 'type-desk',
                coding_scheme_code: '',
                code: 'DESK',
                name: 'Desk',
                level: 2,
                description: '',
            },
        ];
        const draft = walk({ kind: 'set-unit-type', unitId: 'desk', unitTypeId: 'type-div' });
        const row = draft.units.find((candidate) => candidate.unit.id === 'desk');
        expect(row && levelBreach(row, draft.units, types)?.parent.unit.id).toBe('division');
        const fine = walk();
        const ok = fine.units.find((candidate) => candidate.unit.id === 'desk');
        expect(ok && levelBreach(ok, fine.units, types)).toBeUndefined();
    });
});
