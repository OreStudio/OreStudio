/** -*- typescript-ts-mode; tab-width: 4; indent-tabs-mode: nil -*-
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
 * The new party steps' state, and what a party request is built from.
 *
 * A party is found, named and confirmed, and the rail moves between the screens
 * while the answer stays put: the entity found on one step is the name on the
 * next and the summary after it. The functions below are the only place that
 * state becomes a request, so the form and the request cannot disagree.
 *
 * There are two ways to name a party and they end in the same place. A legal
 * entity the deployment holds brings its own legal name and its LEI; a party
 * that is not one of them has neither, and the person types the name. The LEI
 * is what tells the two apart, so it is also what the request states: an empty
 * one is a party described by hand.
 *
 * The state is a plain value with reductions rather than a set of setters,
 * because the rail's screens are not the only thing that moves it: a test walks
 * the same reductions the screens do, with no renderer in the way.
 */

import { useReducer } from 'react';
import { partyCodeFromName } from './derive.js';
import type { LeiEntityChoice, ProvisionPartyRequest } from '@ores/wire-protocol/browser';

/** A party as the person has described it so far, before anything exists. */
export interface PartyDraft {
    /** The entity the party is built from, absent when it is described by hand. */
    readonly entity: LeiEntityChoice | undefined;
    /** Whether the person chose to describe a party that is not one of ours. */
    readonly byHand: boolean;
    /** The name typed for such a party, which is empty for an entity-backed one. */
    readonly typedName: string;
    readonly shortCode: string;
    /**
     * Whether the person typed the short code themselves.
     *
     * The proposal stops the moment somebody edits the field it proposes: the
     * derivation helps while a name is being settled, and it never overwrites
     * what somebody wrote.
     */
    readonly codeTouched: boolean;
    readonly instanceId: string | undefined;
    readonly partyId: string | undefined;
    /** Whether the run has reached its end, so the person may move on. */
    readonly runComplete: boolean;
}

export const EMPTY_PARTY: PartyDraft = {
    entity: undefined,
    byHand: false,
    typedName: '',
    shortCode: '',
    codeTouched: false,
    instanceId: undefined,
    partyId: undefined,
    runComplete: false,
};

export type PartyAction =
    | { readonly kind: 'choose-entity'; readonly entity: LeiEntityChoice }
    | { readonly kind: 'describe-by-hand' }
    | { readonly kind: 'search-again' }
    | { readonly kind: 'type-name'; readonly name: string }
    | { readonly kind: 'set-short-code'; readonly code: string }
    | { readonly kind: 'record-run'; readonly partyId: string; readonly instanceId: string }
    | { readonly kind: 'record-run-complete' }
    | { readonly kind: 'restart' };

/** The name the party will carry, from whichever source names it. */
export function partyFullName(draft: PartyDraft): string {
    return draft.entity?.legalName ?? (draft.byHand ? draft.typedName.trim() : '');
}

/** The entity's LEI, and empty for a party that has none. */
export function partyLei(draft: PartyDraft): string {
    return draft.entity?.lei ?? '';
}

function withProposedCode(draft: PartyDraft, name: string): PartyDraft {
    return draft.codeTouched ? draft : { ...draft, shortCode: partyCodeFromName(name) };
}

export function reduceParty(draft: PartyDraft, action: PartyAction): PartyDraft {
    switch (action.kind) {
        case 'choose-entity': {
            const chosen: PartyDraft = {
                ...draft,
                entity: action.entity,
                byHand: false,
            };
            return withProposedCode(chosen, action.entity.legalName);
        }
        case 'describe-by-hand': {
            const typed: PartyDraft = { ...draft, entity: undefined, byHand: true };
            return withProposedCode(typed, draft.typedName);
        }
        case 'search-again':
            return { ...draft, byHand: false };
        case 'type-name': {
            const named: PartyDraft = { ...draft, typedName: action.name };
            return withProposedCode(named, action.name);
        }
        case 'set-short-code':
            return { ...draft, codeTouched: true, shortCode: action.code };
        case 'record-run':
            return { ...draft, partyId: action.partyId, instanceId: action.instanceId };
        case 'record-run-complete':
            return { ...draft, runComplete: true };
        case 'restart':
            return EMPTY_PARTY;
    }
}

export interface NewParty extends PartyDraft {
    /** The name the party will carry, from either source. */
    readonly fullName: string;
    /** The entity's LEI, and empty for a party that has none. */
    readonly lei: string;
    chooseEntity(entity: LeiEntityChoice): void;
    describeByName(): void;
    searchAgain(): void;
    typeName(name: string): void;
    setShortCode(code: string): void;
    recordRun(partyId: string, instanceId: string): void;
    recordRunComplete(): void;
    /** Forgets this party, for a person who is adding another one. */
    restart(): void;
}

export function useNewParty(): NewParty {
    const [draft, dispatch] = useReducer(reduceParty, EMPTY_PARTY);
    return {
        ...draft,
        fullName: partyFullName(draft),
        lei: partyLei(draft),
        chooseEntity: (entity) => dispatch({ kind: 'choose-entity', entity }),
        describeByName: () => dispatch({ kind: 'describe-by-hand' }),
        searchAgain: () => dispatch({ kind: 'search-again' }),
        typeName: (name) => dispatch({ kind: 'type-name', name }),
        setShortCode: (code) => dispatch({ kind: 'set-short-code', code }),
        recordRun: (partyId, instanceId) => dispatch({ kind: 'record-run', partyId, instanceId }),
        recordRunComplete: () => dispatch({ kind: 'record-run-complete' }),
        restart: () => dispatch({ kind: 'restart' }),
    };
}

/**
 * The request one described party becomes.
 *
 * The starting point is absent because nothing here can name one: the profiles
 * are the system tenant's rows and a tenant administrator reads only its own,
 * so the service that holds both decides which stage publishes the party.
 */
export function partyRequest(
    state: Pick<PartyDraft, 'shortCode'> & { readonly fullName: string; readonly lei: string },
): ProvisionPartyRequest {
    return {
        fullName: state.fullName,
        shortCode: state.shortCode,
        lei: state.lei,
    };
}
