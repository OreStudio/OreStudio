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
import {
    NEW_COUNTERPARTY_REASON,
    authoritativeIdentifier,
    blankAgreement,
    blankDraft,
    blankSet,
    compositeRequest,
    contactProblems,
    identifierProblems,
    reduceCounterparty,
    writeCounts,
    type CounterpartyAction,
    type CounterpartyDraft,
    type CounterpartyIntent,
} from './counterpartyState.js';

const INTENT: CounterpartyIntent = {
    reason_code: NEW_COUNTERPARTY_REASON,
    commentary: '',
};

function state(actions: readonly CounterpartyAction[] = []): CounterpartyDraft {
    return actions.reduce(reduceCounterparty, blankDraft());
}

describe('the counterparty draft', () => {
    it('starts empty, so nothing is suggested that the person did not choose', () => {
        const draft = blankDraft();

        expect(draft.identifiers).toEqual([]);
        expect(draft.contacts).toEqual([]);
        expect(draft.agreements).toEqual([]);
        expect(draft.written).toBeUndefined();
    });

    it('keeps one identifier authoritative when another is marked', () => {
        const draft = state([
            { kind: 'add-identifier', scheme: 'BIC', value: 'NWCAGB2L' },
            { kind: 'add-identifier', scheme: 'LEI', value: '549300NWCAPITAL00001' },
        ]);
        const first = draft.identifiers[0]!;
        const second = draft.identifiers[1]!;

        const marked = reduceCounterparty(draft, {
            kind: 'mark-authoritative',
            id: second.id,
        });

        expect(authoritativeIdentifier(marked.identifiers)?.value).toBe('549300NWCAPITAL00001');
        expect(marked.identifiers.find((row) => row.id === first.id)?.authoritative).toBe(false);
    });

    it('proposes the LEI, then the BIC, when nobody marked one', () => {
        const draft = state([
            { kind: 'add-identifier', scheme: 'BIC', value: 'NWCAGB2L' },
            { kind: 'add-identifier', scheme: 'LEI', value: '549300NWCAPITAL00001' },
        ]);

        expect(authoritativeIdentifier(draft.identifiers)?.scheme).toBe('LEI');
    });

    it('keeps both contacts of the same type until one is removed', () => {
        const draft = state([
            { kind: 'add-contact', contactType: 'Legal' },
            { kind: 'add-contact', contactType: 'Legal' },
        ]);

        expect(draft.contacts).toHaveLength(2);
        expect(contactProblems(draft)).toEqual(['Two contacts of type Legal.']);
    });

    it('drops a contact and its editor with it', () => {
        const draft = state([{ kind: 'add-contact', contactType: 'Legal' }]);
        const contact = draft.contacts[0]!;

        const removed = reduceCounterparty(draft, { kind: 'remove-contact', id: contact.id });

        expect(removed.contacts).toEqual([]);
    });

    it('edits an agreement, a set and its collateral terms', () => {
        const agreement = blankAgreement('ISDA-2026-014');
        const set = blankSet('NS-NWCAP-IRS');
        const opened = state([{ kind: 'add-agreement', agreement }]);
        const withSet = reduceCounterparty(opened, {
            kind: 'add-set',
            agreementId: agreement.id,
            set,
        });
        const edited = reduceCounterparty(withSet, {
            kind: 'set-set',
            agreementId: agreement.id,
            setId: set.id,
            field: 'code',
            value: 'NS-NWCAP-CDS',
        });
        const collateral = reduceCounterparty(edited, {
            kind: 'set-csa',
            agreementId: agreement.id,
            setId: set.id,
            field: 'currency',
            value: 'EUR',
        });

        expect(collateral.agreements[0]?.sets[0]?.code).toBe('NS-NWCAP-CDS');
        expect(collateral.agreements[0]?.sets[0]?.csa.currency).toBe('EUR');
    });
});

describe('what the identifiers step refuses', () => {
    it('asks for the first identifier', () => {
        expect(identifierProblems(state())).toEqual([
            'A counterparty needs at least one identifier.',
        ]);
    });

    it('asks for a value on every identifier', () => {
        const draft = state([{ kind: 'add-identifier', scheme: 'LEI', value: '  ' }]);

        expect(identifierProblems(draft)).toContain('Every identifier needs a value.');
    });

    it('refuses the same scheme and value twice', () => {
        const draft = state([
            { kind: 'add-identifier', scheme: 'LEI', value: '549300NWCAPITAL00001' },
            { kind: 'add-identifier', scheme: 'LEI', value: '549300NWCAPITAL00001' },
        ]);

        expect(identifierProblems(draft)).toContain(
            'LEI 549300NWCAPITAL00001 is on this counterparty twice.',
        );
    });

    it('accepts one identifier with a value', () => {
        const draft = state([
            { kind: 'add-identifier', scheme: 'LEI', value: '549300NWCAPITAL00001' },
        ]);

        expect(identifierProblems(draft)).toEqual([]);
    });
});

describe('the one request the confirm sends', () => {
    it('names every child by the ids the draft minted and links them to the counterparty', () => {
        const agreement = blankAgreement('ISDA-2026-014');
        const set = blankSet('NS-NWCAP-IRS');
        const withSet = { ...set, identifiers: [], eligible: [] };
        const draft = state([
            { kind: 'set-identity', field: 'shortCode', value: 'NWCAP' },
            { kind: 'set-identity', field: 'fullName', value: 'Northwind Capital Ltd' },
            { kind: 'add-identifier', scheme: 'LEI', value: '549300NWCAPITAL00001' },
            { kind: 'add-contact', contactType: 'Legal' },
            { kind: 'add-agreement', agreement: { ...agreement, sets: [withSet] } },
        ]);

        const request = compositeRequest(draft, 'p-ores', INTENT);

        expect(request.intent).toEqual(INTENT);
        expect(request.counterparty.id).toBe(draft.id);
        expect(request.counterparty.short_code).toBe('NWCAP');
        expect(request.counterparty.transliterated_name).toBeNull();
        expect(request.counterparty.parent_counterparty_id).toBeNull();
        expect(request.identifiers[0]?.counterparty_id).toBe(draft.id);
        expect(request.contacts[0]?.counterparty_id).toBe(draft.id);
        expect(request.agreements[0]?.counterparty_id).toBe(draft.id);
        expect(request.agreements[0]?.party_id).toBe('p-ores');
        expect(request.netting_sets[0]?.netting_agreement_id).toBe(agreement.id);
        expect(request.csas[0]?.netting_set_id).toBe(set.id);
        expect(request.netting_sets[0]?.party_id).toBe('p-ores');
        expect(request.csas[0]?.party_id).toBe('p-ores');
    });

    it('parses the collateral numbers and leaves an empty one null', () => {
        const agreement = blankAgreement('ISDA-2026-014');
        const set = blankSet('NS-NWCAP-IRS');
        const draft = state([{ kind: 'add-agreement', agreement: { ...agreement, sets: [set] } }]);
        const withTerms = reduceCounterparty(draft, {
            kind: 'set-csa',
            agreementId: agreement.id,
            setId: set.id,
            field: 'thresholdPay',
            value: '250000',
        });

        const request = compositeRequest(withTerms, 'p-ores', INTENT);

        expect(request.csas[0]?.threshold_pay).toBe(250000);
        expect(request.csas[0]?.threshold_receive).toBeNull();
    });

    it('keeps the order of the eligible currencies the person gave them', () => {
        const agreement = blankAgreement('ISDA-2026-014');
        const set = blankSet('NS-NWCAP-IRS');
        const added = state([{ kind: 'add-agreement', agreement: { ...agreement, sets: [set] } }]);
        const withEuro = reduceCounterparty(added, {
            kind: 'add-eligible-currency',
            agreementId: agreement.id,
            setId: set.id,
            currencyCode: 'EUR',
        });
        const withPound = reduceCounterparty(withEuro, {
            kind: 'add-eligible-currency',
            agreementId: agreement.id,
            setId: set.id,
            currencyCode: 'GBP',
        });

        const request = compositeRequest(withPound, 'p-ores', INTENT);

        expect(request.eligible_currencies.map((currency) => currency.currency_code)).toEqual([
            'EUR',
            'GBP',
        ]);
        expect(request.eligible_currencies.map((currency) => currency.position)).toEqual([0, 1]);
    });
});

describe('what the review states the confirm writes', () => {
    it('counts one counterparty, its children and its one act', () => {
        const agreement = blankAgreement('ISDA-2026-014');
        const set = blankSet('NS-NWCAP-IRS');
        const draft = state([
            { kind: 'add-identifier', scheme: 'LEI', value: '549300NWCAPITAL00001' },
            { kind: 'add-contact', contactType: 'Legal' },
            { kind: 'add-agreement', agreement: { ...agreement, sets: [set] } },
        ]);

        const counts = new Map(writeCounts(draft));

        expect(counts.get('refdata.v1.ops.put_counterparty_composite')).toBe(1);
        expect(counts.get('refdata.v1.counterparty_identifiers.put_many')).toBe(1);
        expect(counts.get('refdata.v1.counterparty_contact_informations.put_many')).toBe(1);
        expect(counts.get('refdata.v1.netting_agreements.put')).toBe(1);
        expect(counts.get('refdata.v1.netting_sets.put')).toBe(1);
        expect(counts.get('refdata.v1.csas.put')).toBe(1);
    });
});
