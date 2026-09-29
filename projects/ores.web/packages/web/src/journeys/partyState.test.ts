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
    EMPTY_PARTY,
    partyFullName,
    partyLei,
    partyRequest,
    reduceParty,
    type PartyAction,
    type PartyDraft,
} from './partyState.js';
import type { LeiEntityChoice } from '@ores/wire-protocol/browser';

const barclays: LeiEntityChoice = {
    lei: '213800LBQA1Y9L22JB70',
    legalName: 'BARCLAYS PLC',
    country: 'GB',
    partyCount: 82,
};

/** Runs a list of the screens' own actions over the state. */
function walk(actions: readonly PartyAction[], from: PartyDraft = EMPTY_PARTY): PartyDraft {
    return actions.reduce(reduceParty, from);
}

describe('the party a person describes', () => {
    it('takes its name and its LEI from the entity it was built from', () => {
        const draft = walk([{ kind: 'choose-entity', entity: barclays }]);

        expect(partyFullName(draft)).toBe('BARCLAYS PLC');
        expect(partyLei(draft)).toBe('213800LBQA1Y9L22JB70');
        expect(draft.shortCode).toBe('BRCLYS');
    });

    it('takes the name a person types for a party that is not one of ours', () => {
        const draft = walk([
            { kind: 'describe-by-hand' },
            { kind: 'type-name', name: 'Northwind Capital' },
        ]);

        expect(partyFullName(draft)).toBe('Northwind Capital');
        expect(partyLei(draft)).toBe('');
        expect(draft.byHand).toBe(true);
    });

    it('keeps its name when the person goes back to searching', () => {
        const draft = walk([
            { kind: 'describe-by-hand' },
            { kind: 'type-name', name: 'Northwind Capital' },
            { kind: 'search-again' },
        ]);

        expect(draft.byHand).toBe(false);
        expect(partyFullName(draft)).toBe('');
    });

    it('follows the name until somebody types a code of their own', () => {
        const following = walk([
            { kind: 'describe-by-hand' },
            { kind: 'type-name', name: 'Northwind Capital' },
            { kind: 'type-name', name: 'Northwind Capital Markets' },
        ]);
        expect(following.shortCode).toBe('NOCAMA');

        const typed = walk([
            { kind: 'describe-by-hand' },
            { kind: 'type-name', name: 'Northwind Capital' },
            { kind: 'set-short-code', code: 'NWCAP' },
            { kind: 'type-name', name: 'Northwind Capital Markets' },
        ]);
        expect(typed.shortCode).toBe('NWCAP');
    });

    it('forgets the party when the person adds another one', () => {
        const draft = walk([
            { kind: 'choose-entity', entity: barclays },
            { kind: 'record-run', partyId: 'p', instanceId: 'i' },
            { kind: 'record-run-complete' },
            { kind: 'restart' },
        ]);

        expect(draft).toEqual(EMPTY_PARTY);
    });

    it('records the party and the run the stage started', () => {
        const started = walk([{ kind: 'record-run', partyId: 'party-1', instanceId: 'run-1' }]);
        expect(started.partyId).toBe('party-1');
        expect(started.instanceId).toBe('run-1');
        expect(started.runComplete).toBe(false);

        const finished = reduceParty(started, { kind: 'record-run-complete' });
        expect(finished.runComplete).toBe(true);
    });
});

describe('the request a described party becomes', () => {
    it('carries the name, the short code, the LEI and the starting point', () => {
        const draft = walk([
            { kind: 'choose-entity', entity: barclays },
            { kind: 'set-short-code', code: 'BRCLYS' },
        ]);

        expect(
            partyRequest({
                fullName: partyFullName(draft),
                lei: partyLei(draft),
                shortCode: draft.shortCode,
            }),
        ).toEqual({
            fullName: 'BARCLAYS PLC',
            shortCode: 'BRCLYS',
            lei: '213800LBQA1Y9L22JB70',
        });
    });

    it('carries no LEI for a party that is not one of ours', () => {
        const draft = walk([
            { kind: 'describe-by-hand' },
            { kind: 'type-name', name: 'Northwind Capital' },
        ]);

        expect(
            partyRequest({
                fullName: partyFullName(draft),
                lei: partyLei(draft),
                shortCode: draft.shortCode,
            }).lei,
        ).toBe('');
    });
});
