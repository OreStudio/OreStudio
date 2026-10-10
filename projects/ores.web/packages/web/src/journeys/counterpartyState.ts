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
 * The counterparty journey's draft, and what the confirm makes of it.
 *
 * A counterparty is onboarded in one screen and written in one act, so the
 * draft holds the whole of it: the identity, the identifiers, the contacts, the
 * agreements with their sets, collateral terms and eligible currencies. The
 * rail moves between those screens while the answer stays put, and the confirm
 * turns the answer into the one composite request the server takes.
 *
 * The draft is plain and the reductions are pure, as the party journey's are,
 * because the rail's screens are not the only thing that moves it: a test walks
 * the same reductions the screens do, with no renderer in the way. Identifiers
 * are minted here rather than by the server because a child names its parent by
 * id before the parent exists, and the composite write is one act over a graph
 * the client has already assembled.
 */

import { useReducer } from 'react';
import type { Counterparty } from '@ores/wire-protocol/generated/refdata/domain/counterparty';
import type { CounterpartyIdentifier } from '@ores/wire-protocol/generated/refdata/domain/counterparty_identifier';
import type { CounterpartyContactInformation } from '@ores/wire-protocol/generated/refdata/domain/counterparty_contact_information';
import type { CounterpartyBusinessCentre } from '@ores/wire-protocol/generated/refdata/domain/counterparty_business_centre';
import type { NettingAgreement } from '@ores/wire-protocol/generated/refdata/domain/netting_agreement';
import type { NettingSet } from '@ores/wire-protocol/generated/refdata/domain/netting_set';
import type { NettingSetIdentifier } from '@ores/wire-protocol/generated/refdata/domain/netting_set_identifier';
import type { Csa } from '@ores/wire-protocol/generated/refdata/domain/csa';
import type { CsaEligibleCurrency } from '@ores/wire-protocol/generated/refdata/domain/csa_eligible_currency';
import type { PutCounterpartyCompositeRequest } from '@ores/wire-protocol/generated/refdata/protocol/counterparty_protocol';

/**
 * Why a write is made.
 *
 * The shape is the generated utility protocol's, restated because that module
 * is not on the wire-protocol package's browser-safe surface.
 */
export interface CounterpartyIntent {
    readonly reason_code: string;
    readonly commentary: string;
}

/** The reason a counterparty is written with. No other reason applies to a new record. */
export const NEW_COUNTERPARTY_REASON = 'system.new_record';

/** The reason the composite is the entity type the history route reads by. */
export const COUNTERPARTY_ENTITY_TYPE = 'ores.refdata.counterparty';

/** One identifier the draft holds, before the server sees it. */
export interface IdentifierDraft {
    readonly id: string;
    readonly scheme: string;
    readonly value: string;
    readonly description: string;
    readonly authoritative: boolean;
}

/** One contact the draft holds. The person keeps one row per contact type. */
export interface ContactDraft {
    readonly id: string;
    readonly contactType: string;
    readonly streetLine1: string;
    readonly streetLine2: string;
    readonly city: string;
    readonly state: string;
    readonly countryCode: string;
    readonly postalCode: string;
    readonly phone: string;
    readonly email: string;
    readonly webPage: string;
}

/** A field of a contact the person can change. */
export type ContactField = Exclude<keyof ContactDraft, 'id' | 'contactType'>;

/** One name a netting set answers to. */
export interface SetIdentifierDraft {
    readonly id: string;
    readonly scheme: string;
    readonly value: string;
    readonly description: string;
}

/** One currency the CSA accepts as collateral, in the order ORE lists it. */
export interface EligibleCurrencyDraft {
    readonly id: string;
    readonly currencyCode: string;
    readonly position: number;
}

/** The collateral terms of a set. */
export interface CsaDraft {
    readonly id: string;
    readonly isActive: boolean;
    readonly bilateral: string;
    readonly currency: string;
    readonly indexName: string;
    readonly marginPeriodOfRisk: string;
    readonly thresholdPay: string;
    readonly thresholdReceive: string;
    readonly minimumTransferAmountPay: string;
    readonly minimumTransferAmountReceive: string;
    readonly independentAmount: string;
    readonly independentAmountType: string;
    readonly callFrequency: string;
    readonly postFrequency: string;
    readonly spreadReceive: string;
    readonly spreadPay: string;
    readonly applyInitialMargin: boolean;
    readonly calculateImAmount: boolean;
    readonly calculateVmAmount: boolean;
    readonly nonExemptImRegulations: string;
}

/** A field of a CSA the person can change. */
export type CsaField = Exclude<keyof CsaDraft, 'id' | 'isActive'>;

/** One netting set under an agreement. */
export interface NettingSetDraft {
    readonly id: string;
    readonly code: string;
    readonly callType: string;
    readonly initialMarginType: string;
    readonly riskWeight: string;
    readonly description: string;
    readonly identifiers: readonly SetIdentifierDraft[];
    readonly csa: CsaDraft;
    readonly eligible: readonly EligibleCurrencyDraft[];
}

/** A field of a netting set the person can change. */
export type NettingSetField = Exclude<
    keyof NettingSetDraft,
    'id' | 'identifiers' | 'csa' | 'eligible'
>;

/** One netting agreement under the counterparty, and the sets it holds. */
export interface AgreementDraft {
    readonly id: string;
    readonly agreementNumber: string;
    readonly agreementType: string;
    readonly governingLaw: string;
    readonly description: string;
    readonly sets: readonly NettingSetDraft[];
}

/** A field of an agreement the person can change. */
export type AgreementField = Exclude<keyof AgreementDraft, 'id' | 'sets'>;

/** What the server refused, and what the step keeps. */
export interface CounterpartyRefusal {
    /** The step whose write the server refused. */
    readonly step: string;
    /** The subject that was sent, named as the journey's table names it. */
    readonly subject: string;
    readonly code: string;
    readonly message: string;
    readonly fields: readonly {
        readonly field: string;
        readonly code: string;
        readonly message: string;
    }[];
}

/** The whole of a counterparty as the person has described it so far. */
export interface CounterpartyDraft {
    readonly id: string;
    /** The version read for a counterparty already on board, else zero. */
    readonly version: number;
    readonly opened: boolean;
    readonly shortCode: string;
    readonly fullName: string;
    readonly transliteratedName: string;
    readonly partyType: string;
    readonly status: string;
    readonly parentCounterpartyId: string;
    readonly businessCentreCodes: readonly string[];
    readonly identifiers: readonly IdentifierDraft[];
    readonly contacts: readonly ContactDraft[];
    readonly agreements: readonly AgreementDraft[];
    /** The row the confirm wrote, once it has. */
    readonly written: Counterparty | undefined;
    readonly refusal: CounterpartyRefusal | undefined;
}

export function blankCsa(): CsaDraft {
    return {
        id: crypto.randomUUID(),
        isActive: true,
        bilateral: 'Bilateral',
        currency: '',
        indexName: '',
        marginPeriodOfRisk: '',
        thresholdPay: '',
        thresholdReceive: '',
        minimumTransferAmountPay: '',
        minimumTransferAmountReceive: '',
        independentAmount: '',
        independentAmountType: '',
        callFrequency: '',
        postFrequency: '',
        spreadReceive: '',
        spreadPay: '',
        applyInitialMargin: false,
        calculateImAmount: false,
        calculateVmAmount: false,
        nonExemptImRegulations: '',
    };
}

export function blankSet(code: string): NettingSetDraft {
    return {
        id: crypto.randomUUID(),
        code,
        callType: 'Bilateral',
        initialMarginType: 'Bilateral',
        riskWeight: '',
        description: '',
        identifiers: [],
        csa: blankCsa(),
        eligible: [],
    };
}

export function blankAgreement(number: string): AgreementDraft {
    return {
        id: crypto.randomUUID(),
        agreementNumber: number,
        agreementType: 'ISDA Master Agreement',
        governingLaw: 'English law',
        description: '',
        sets: [],
    };
}

/** A counterparty nobody has described yet. */
export function blankDraft(): CounterpartyDraft {
    return {
        id: crypto.randomUUID(),
        version: 0,
        opened: false,
        shortCode: '',
        fullName: '',
        transliteratedName: '',
        partyType: '',
        status: '',
        parentCounterpartyId: '',
        businessCentreCodes: [],
        identifiers: [],
        contacts: [],
        agreements: [],
        written: undefined,
        refusal: undefined,
    };
}

/**
 * A draft holding what a counterparty already on board reads as, and nothing
 * more: the identity, the identifiers and the contacts. The agreements are read
 * one subject at a time and are not loaded here, which is the gap the journey
 * document records.
 */
export function draftFrom(row: Counterparty, children: CounterpartyChildren): CounterpartyDraft {
    return {
        ...blankDraft(),
        id: row.id,
        version: row.version,
        opened: true,
        shortCode: row.short_code,
        fullName: row.full_name,
        transliteratedName: row.transliterated_name ?? '',
        partyType: row.party_type,
        status: row.status,
        parentCounterpartyId: row.parent_counterparty_id ?? '',
        businessCentreCodes: children.centres.map((centre) => centre.business_centre_code),
        identifiers: children.identifiers.map((identifier) => ({
            id: identifier.id,
            scheme: identifier.id_scheme,
            value: identifier.id_value,
            description: identifier.description,
            authoritative: identifier.is_authoritative,
        })),
        contacts: children.contacts.map((contact) => ({
            id: contact.id,
            contactType: contact.contact_type,
            streetLine1: contact.street_line_1,
            streetLine2: contact.street_line_2,
            city: contact.city,
            state: contact.state,
            countryCode: contact.country_code,
            postalCode: contact.postal_code,
            phone: contact.phone,
            email: contact.email,
            webPage: contact.web_page,
        })),
    };
}

/** The identifiers and contacts the landing list read for one counterparty. */
export interface CounterpartyChildren {
    readonly counterpartyId: string;
    readonly identifiers: readonly CounterpartyIdentifier[];
    readonly contacts: readonly CounterpartyContactInformation[];
    readonly centres: readonly CounterpartyBusinessCentre[];
}

export type CounterpartyAction =
    | { readonly kind: 'start' }
    | {
          readonly kind: 'open';
          readonly row: Counterparty;
          readonly children: CounterpartyChildren;
      }
    | {
          readonly kind: 'set-identity';
          readonly field:
              | 'shortCode'
              | 'fullName'
              | 'transliteratedName'
              | 'partyType'
              | 'status'
              | 'parentCounterpartyId';
          readonly value: string;
      }
    | { readonly kind: 'set-centres'; readonly codes: readonly string[] }
    | { readonly kind: 'add-identifier'; readonly scheme: string; readonly value: string }
    | { readonly kind: 'remove-identifier'; readonly id: string }
    | { readonly kind: 'mark-authoritative'; readonly id: string }
    | { readonly kind: 'add-contact'; readonly contactType: string }
    | { readonly kind: 'remove-contact'; readonly id: string }
    | {
          readonly kind: 'set-contact';
          readonly id: string;
          readonly field: ContactField;
          readonly value: string;
      }
    | { readonly kind: 'add-agreement'; readonly agreement: AgreementDraft }
    | { readonly kind: 'remove-agreement'; readonly id: string }
    | {
          readonly kind: 'set-agreement';
          readonly id: string;
          readonly field: AgreementField;
          readonly value: string;
      }
    | { readonly kind: 'add-set'; readonly agreementId: string; readonly set: NettingSetDraft }
    | { readonly kind: 'remove-set'; readonly agreementId: string; readonly setId: string }
    | {
          readonly kind: 'set-set';
          readonly agreementId: string;
          readonly setId: string;
          readonly field: NettingSetField;
          readonly value: string;
      }
    | {
          readonly kind: 'add-set-identifier';
          readonly agreementId: string;
          readonly setId: string;
          readonly scheme: string;
          readonly value: string;
      }
    | {
          readonly kind: 'remove-set-identifier';
          readonly agreementId: string;
          readonly setId: string;
          readonly identifierId: string;
      }
    | {
          readonly kind: 'set-csa';
          readonly agreementId: string;
          readonly setId: string;
          readonly field: CsaField;
          readonly value: string;
      }
    | {
          readonly kind: 'toggle-csa';
          readonly agreementId: string;
          readonly setId: string;
          readonly field:
              'isActive' | 'applyInitialMargin' | 'calculateImAmount' | 'calculateVmAmount';
          readonly value: boolean;
      }
    | {
          readonly kind: 'add-eligible-currency';
          readonly agreementId: string;
          readonly setId: string;
          readonly currencyCode: string;
      }
    | {
          readonly kind: 'remove-eligible-currency';
          readonly agreementId: string;
          readonly setId: string;
          readonly currencyId: string;
      }
    | { readonly kind: 'record-written'; readonly counterparty: Counterparty }
    | { readonly kind: 'record-refusal'; readonly refusal: CounterpartyRefusal }
    | { readonly kind: 'clear-refusal' };

function mapIdentifiers(
    draft: CounterpartyDraft,
    change: (identifiers: readonly IdentifierDraft[]) => readonly IdentifierDraft[],
): CounterpartyDraft {
    return { ...draft, identifiers: change(draft.identifiers) };
}

function mapContacts(
    draft: CounterpartyDraft,
    change: (contacts: readonly ContactDraft[]) => readonly ContactDraft[],
): CounterpartyDraft {
    return { ...draft, contacts: change(draft.contacts) };
}

function mapAgreements(
    draft: CounterpartyDraft,
    change: (agreements: readonly AgreementDraft[]) => readonly AgreementDraft[],
): CounterpartyDraft {
    return { ...draft, agreements: change(draft.agreements) };
}

function mapSets(
    agreement: AgreementDraft,
    change: (sets: readonly NettingSetDraft[]) => readonly NettingSetDraft[],
): AgreementDraft {
    return { ...agreement, sets: change(agreement.sets) };
}

function mapSet(
    agreement: AgreementDraft,
    setId: string,
    change: (set: NettingSetDraft) => NettingSetDraft,
): AgreementDraft {
    return mapSets(agreement, (sets) => sets.map((set) => (set.id === setId ? change(set) : set)));
}

export function reduceCounterparty(
    draft: CounterpartyDraft,
    action: CounterpartyAction,
): CounterpartyDraft {
    switch (action.kind) {
        case 'start':
            return blankDraft();
        case 'open':
            return draftFrom(action.row, action.children);
        case 'set-identity':
            return { ...draft, [action.field]: action.value };
        case 'set-centres':
            return { ...draft, businessCentreCodes: action.codes };
        case 'add-identifier':
            return mapIdentifiers(draft, (identifiers) => [
                ...identifiers,
                {
                    id: crypto.randomUUID(),
                    scheme: action.scheme,
                    value: action.value,
                    description: '',
                    authoritative: false,
                },
            ]);
        case 'remove-identifier':
            return mapIdentifiers(draft, (identifiers) =>
                identifiers.filter((identifier) => identifier.id !== action.id),
            );
        case 'mark-authoritative':
            return mapIdentifiers(draft, (identifiers) =>
                identifiers.map((identifier) => ({
                    ...identifier,
                    authoritative: identifier.id === action.id,
                })),
            );
        case 'add-contact':
            return mapContacts(draft, (contacts) => [
                ...contacts,
                {
                    id: crypto.randomUUID(),
                    contactType: action.contactType,
                    streetLine1: '',
                    streetLine2: '',
                    city: '',
                    state: '',
                    countryCode: '',
                    postalCode: '',
                    phone: '',
                    email: '',
                    webPage: '',
                },
            ]);
        case 'remove-contact':
            return mapContacts(draft, (contacts) =>
                contacts.filter((contact) => contact.id !== action.id),
            );
        case 'set-contact':
            return mapContacts(draft, (contacts) =>
                contacts.map((contact) =>
                    contact.id === action.id
                        ? { ...contact, [action.field]: action.value }
                        : contact,
                ),
            );
        case 'add-agreement':
            return mapAgreements(draft, (agreements) => [...agreements, action.agreement]);
        case 'remove-agreement':
            return mapAgreements(draft, (agreements) =>
                agreements.filter((agreement) => agreement.id !== action.id),
            );
        case 'set-agreement':
            return mapAgreements(draft, (agreements) =>
                agreements.map((agreement) =>
                    agreement.id === action.id
                        ? { ...agreement, [action.field]: action.value }
                        : agreement,
                ),
            );
        case 'add-set':
            return mapAgreements(draft, (agreements) =>
                agreements.map((agreement) =>
                    agreement.id === action.agreementId
                        ? mapSets(agreement, (sets) => [...sets, action.set])
                        : agreement,
                ),
            );
        case 'remove-set':
            return mapAgreements(draft, (agreements) =>
                agreements.map((agreement) =>
                    agreement.id === action.agreementId
                        ? mapSets(agreement, (sets) =>
                              sets.filter((set) => set.id !== action.setId),
                          )
                        : agreement,
                ),
            );
        case 'set-set':
            return mapAgreements(draft, (agreements) =>
                agreements.map((agreement) =>
                    agreement.id === action.agreementId
                        ? mapSet(agreement, action.setId, (set) => ({
                              ...set,
                              [action.field]: action.value,
                          }))
                        : agreement,
                ),
            );
        case 'add-set-identifier':
            return mapAgreements(draft, (agreements) =>
                agreements.map((agreement) =>
                    agreement.id === action.agreementId
                        ? mapSet(agreement, action.setId, (set) => ({
                              ...set,
                              identifiers: [
                                  ...set.identifiers,
                                  {
                                      id: crypto.randomUUID(),
                                      scheme: action.scheme,
                                      value: action.value,
                                      description: '',
                                  },
                              ],
                          }))
                        : agreement,
                ),
            );
        case 'remove-set-identifier':
            return mapAgreements(draft, (agreements) =>
                agreements.map((agreement) =>
                    agreement.id === action.agreementId
                        ? mapSet(agreement, action.setId, (set) => ({
                              ...set,
                              identifiers: set.identifiers.filter(
                                  (identifier) => identifier.id !== action.identifierId,
                              ),
                          }))
                        : agreement,
                ),
            );
        case 'set-csa':
            return mapAgreements(draft, (agreements) =>
                agreements.map((agreement) =>
                    agreement.id === action.agreementId
                        ? mapSet(agreement, action.setId, (set) => ({
                              ...set,
                              csa: { ...set.csa, [action.field]: action.value },
                          }))
                        : agreement,
                ),
            );
        case 'toggle-csa':
            return mapAgreements(draft, (agreements) =>
                agreements.map((agreement) =>
                    agreement.id === action.agreementId
                        ? mapSet(agreement, action.setId, (set) => ({
                              ...set,
                              csa: { ...set.csa, [action.field]: action.value },
                          }))
                        : agreement,
                ),
            );
        case 'add-eligible-currency':
            return mapAgreements(draft, (agreements) =>
                agreements.map((agreement) =>
                    agreement.id === action.agreementId
                        ? mapSet(agreement, action.setId, (set) => ({
                              ...set,
                              eligible: [
                                  ...set.eligible,
                                  {
                                      id: crypto.randomUUID(),
                                      currencyCode: action.currencyCode,
                                      position: set.eligible.length,
                                  },
                              ],
                          }))
                        : agreement,
                ),
            );
        case 'remove-eligible-currency':
            return mapAgreements(draft, (agreements) =>
                agreements.map((agreement) =>
                    agreement.id === action.agreementId
                        ? mapSet(agreement, action.setId, (set) => ({
                              ...set,
                              eligible: set.eligible
                                  .filter((currency) => currency.id !== action.currencyId)
                                  .map((currency, position) => ({ ...currency, position })),
                          }))
                        : agreement,
                ),
            );
        case 'record-written':
            return { ...draft, written: action.counterparty, refusal: undefined };
        case 'record-refusal':
            return { ...draft, refusal: action.refusal };
        case 'clear-refusal':
            return { ...draft, refusal: undefined };
    }
}

/** The counterparty draft, and every reduction the screens apply to it. */
export interface NewCounterparty extends CounterpartyDraft {
    readonly hasIdentity: boolean;
    /** The identifier the tenant resolves this counterparty by. */
    readonly authoritative: IdentifierDraft | undefined;
    startNew(): void;
    open(row: Counterparty, children: CounterpartyChildren): void;
    setIdentity(
        field:
            | 'shortCode'
            | 'fullName'
            | 'transliteratedName'
            | 'partyType'
            | 'status'
            | 'parentCounterpartyId',
        value: string,
    ): void;
    setCentres(codes: readonly string[]): void;
    addIdentifier(scheme: string, value: string): void;
    removeIdentifier(id: string): void;
    markAuthoritative(id: string): void;
    addContact(contactType: string): void;
    removeContact(id: string): void;
    setContact(id: string, field: ContactField, value: string): void;
    addAgreement(agreement: AgreementDraft): void;
    removeAgreement(id: string): void;
    setAgreement(id: string, field: AgreementField, value: string): void;
    addSet(agreementId: string, set: NettingSetDraft): void;
    removeSet(agreementId: string, setId: string): void;
    setSet(agreementId: string, setId: string, field: NettingSetField, value: string): void;
    addSetIdentifier(agreementId: string, setId: string, scheme: string, value: string): void;
    removeSetIdentifier(agreementId: string, setId: string, identifierId: string): void;
    setCsa(agreementId: string, setId: string, field: CsaField, value: string): void;
    toggleCsa(
        agreementId: string,
        setId: string,
        field: 'isActive' | 'applyInitialMargin' | 'calculateImAmount' | 'calculateVmAmount',
        value: boolean,
    ): void;
    addEligibleCurrency(agreementId: string, setId: string, currencyCode: string): void;
    removeEligibleCurrency(agreementId: string, setId: string, currencyId: string): void;
    recordWritten(counterparty: Counterparty): void;
    recordRefusal(refusal: CounterpartyRefusal): void;
    clearRefusal(): void;
}

/**
 * The identifier the tenant resolves a counterparty by.
 *
 * The flag is the person's when they set one. A draft none of whose identifiers
 * carries the flag proposes the LEI, then the BIC, which is the precedence the
 * journey document states.
 */
export function authoritativeIdentifier(
    identifiers: readonly IdentifierDraft[],
): IdentifierDraft | undefined {
    const marked = identifiers.find((identifier) => identifier.authoritative);
    if (marked !== undefined) {
        return marked;
    }
    return (
        identifiers.find((identifier) => identifier.scheme === 'LEI') ??
        identifiers.find((identifier) => identifier.scheme === 'BIC')
    );
}

/** Whether the identity step is answered: the short code and the full name. */
export function hasIdentity(draft: Pick<CounterpartyDraft, 'shortCode' | 'fullName'>): boolean {
    return draft.shortCode.trim() !== '' && draft.fullName.trim() !== '';
}

/** What the identifiers step still refuses, in the order the server would state it. */
export function identifierProblems(
    draft: Pick<CounterpartyDraft, 'identifiers'>,
): readonly string[] {
    const problems: string[] = [];
    if (draft.identifiers.length === 0) {
        problems.push('A counterparty needs at least one identifier.');
        return problems;
    }
    if (authoritativeIdentifier(draft.identifiers) === undefined) {
        problems.push('Mark one identifier authoritative, or add an LEI or a BIC.');
    }
    if (draft.identifiers.some((identifier) => identifier.value.trim() === '')) {
        problems.push('Every identifier needs a value.');
    }
    const seen = new Set<string>();
    for (const identifier of draft.identifiers) {
        const key = `${identifier.scheme}:${identifier.value.trim().toLowerCase()}`;
        if (identifier.value.trim() !== '' && seen.has(key)) {
            problems.push(
                `${identifier.scheme} ${identifier.value} is on this counterparty twice.`,
            );
        }
        seen.add(key);
    }
    return problems;
}

/** Whether the contacts step is answered: one row per type, and no two of a type. */
export function contactProblems(draft: Pick<CounterpartyDraft, 'contacts'>): readonly string[] {
    const seen = new Set<string>();
    const problems: string[] = [];
    for (const contact of draft.contacts) {
        if (seen.has(contact.contactType)) {
            problems.push(`Two contacts of type ${contact.contactType}.`);
        }
        seen.add(contact.contactType);
    }
    return problems;
}

function textOrNull(value: string): string | null {
    return value.trim() === '' ? null : value;
}

function numberOrNull(value: string): number | null {
    const trimmed = value.trim();
    if (trimmed === '') {
        return null;
    }
    const parsed = Number(trimmed);
    return Number.isNaN(parsed) ? null : parsed;
}

function stamp(): Pick<
    Counterparty,
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

/**
 * The one request the confirm sends.
 *
 * Every child is named by an id the draft minted, and every child that names
 * the counterparty carries the same id the counterparty write carries, so the
 * graph is whole before it reaches the server and the single transaction has
 * nothing to resolve. The tenant's party is passed in because an agreement's
 * two parties are fixed and the tenant side is the session's, not the form's.
 */
export function compositeRequest(
    draft: CounterpartyDraft,
    partyId: string,
    intent: CounterpartyIntent,
): PutCounterpartyCompositeRequest {
    const counterparty: Counterparty = {
        ...stamp(),
        version: draft.version,
        id: draft.id,
        short_code: draft.shortCode.trim(),
        full_name: draft.fullName.trim(),
        transliterated_name: textOrNull(draft.transliteratedName),
        party_type: draft.partyType,
        parent_counterparty_id: textOrNull(draft.parentCounterpartyId),
        status: draft.status,
        image_id: null,
    };
    const identifiers: CounterpartyIdentifier[] = draft.identifiers.map((identifier) => ({
        ...stamp(),
        id: identifier.id,
        counterparty_id: draft.id,
        id_scheme: identifier.scheme,
        id_value: identifier.value.trim(),
        description: identifier.description,
        is_authoritative: identifier.authoritative,
    }));
    const contacts: CounterpartyContactInformation[] = draft.contacts.map((contact) => ({
        ...stamp(),
        id: contact.id,
        counterparty_id: draft.id,
        contact_type: contact.contactType,
        street_line_1: contact.streetLine1,
        street_line_2: contact.streetLine2,
        city: contact.city,
        state: contact.state,
        country_code: contact.countryCode,
        postal_code: contact.postalCode,
        phone: contact.phone,
        email: contact.email,
        web_page: contact.webPage,
    }));
    const agreements: NettingAgreement[] = draft.agreements.map((agreement) => ({
        ...stamp(),
        id: agreement.id,
        agreement_number: agreement.agreementNumber.trim(),
        party_id: partyId,
        counterparty_id: draft.id,
        agreement_type: agreement.agreementType,
        governing_law: textOrNull(agreement.governingLaw),
        description: textOrNull(agreement.description),
    }));
    const sets: NettingSet[] = draft.agreements.flatMap((agreement) =>
        agreement.sets.map((set) => ({
            ...stamp(),
            id: set.id,
            code: set.code.trim(),
            netting_agreement_id: agreement.id,
            counterparty_id: draft.id,
            party_id: partyId,
            call_type: textOrNull(set.callType),
            initial_margin_type: textOrNull(set.initialMarginType),
            risk_weight: numberOrNull(set.riskWeight),
            description: textOrNull(set.description),
        })),
    );
    const setIdentifiers: NettingSetIdentifier[] = draft.agreements.flatMap((agreement) =>
        agreement.sets.flatMap((set) =>
            set.identifiers.map((identifier) => ({
                ...stamp(),
                id: identifier.id,
                netting_set_id: set.id,
                party_id: partyId,
                id_scheme: identifier.scheme,
                id_value: identifier.value.trim(),
                description: identifier.description,
            })),
        ),
    );
    const csas: Csa[] = draft.agreements.flatMap((agreement) =>
        agreement.sets.map((set) => ({
            ...stamp(),
            id: set.csa.id,
            netting_set_id: set.id,
            party_id: partyId,
            is_active: set.csa.isActive,
            bilateral: textOrNull(set.csa.bilateral),
            csa_currency: textOrNull(set.csa.currency),
            index_name: textOrNull(set.csa.indexName),
            threshold_pay: numberOrNull(set.csa.thresholdPay),
            threshold_receive: numberOrNull(set.csa.thresholdReceive),
            minimum_transfer_amount_pay: numberOrNull(set.csa.minimumTransferAmountPay),
            minimum_transfer_amount_receive: numberOrNull(set.csa.minimumTransferAmountReceive),
            independent_amount_held: numberOrNull(set.csa.independentAmount),
            independent_amount_type: textOrNull(set.csa.independentAmountType),
            call_frequency: textOrNull(set.csa.callFrequency),
            post_frequency: textOrNull(set.csa.postFrequency),
            margin_period_of_risk: textOrNull(set.csa.marginPeriodOfRisk),
            collateral_compounding_spread_receive: numberOrNull(set.csa.spreadReceive),
            collateral_compounding_spread_pay: numberOrNull(set.csa.spreadPay),
            apply_initial_margin: set.csa.applyInitialMargin,
            initial_margin_type: textOrNull(set.csa.applyInitialMargin ? 'Bilateral' : ''),
            calculate_im_amount: set.csa.calculateImAmount,
            calculate_vm_amount: set.csa.calculateVmAmount,
            non_exempt_im_regulations: textOrNull(set.csa.nonExemptImRegulations),
        })),
    );
    const eligibleCurrencies: CsaEligibleCurrency[] = draft.agreements.flatMap((agreement) =>
        agreement.sets.flatMap((set) =>
            set.eligible.map((currency) => ({
                ...stamp(),
                id: currency.id,
                csa_id: set.csa.id,
                currency_code: currency.currencyCode,
                position: currency.position,
            })),
        ),
    );

    return {
        intent,
        counterparty,
        identifiers,
        contacts,
        agreements,
        netting_sets: sets,
        netting_set_identifiers: setIdentifiers,
        csas,
        eligible_currencies: eligibleCurrencies,
    };
}

/** How many rows each subject the confirm sends carries. */
export function writeCounts(draft: CounterpartyDraft): readonly (readonly [string, number])[] {
    const setCount = draft.agreements.reduce(
        (total, agreement) => total + agreement.sets.length,
        0,
    );
    const setIdentifierCount = draft.agreements.reduce(
        (total, agreement) =>
            total + agreement.sets.reduce((count, set) => count + set.identifiers.length, 0),
        0,
    );
    const eligibleCount = draft.agreements.reduce(
        (total, agreement) =>
            total + agreement.sets.reduce((count, set) => count + set.eligible.length, 0),
        0,
    );
    return [
        ['refdata.v1.ops.put_counterparty_composite', 1],
        ['refdata.v1.counterparties.put', 1],
        ['refdata.v1.counterparty_identifiers.put_many', draft.identifiers.length],
        ['refdata.v1.counterparty_contact_informations.put_many', draft.contacts.length],
        ['refdata.v1.netting_agreements.put', draft.agreements.length],
        ['refdata.v1.netting_sets.put', setCount],
        ['refdata.v1.netting_set_identifiers.put_many', setIdentifierCount],
        ['refdata.v1.csas.put', setCount],
        ['refdata.v1.csa_eligible_currencies.put_many', eligibleCount],
        ['refdata.v1.counterparty_business_centres.put_many', draft.businessCentreCodes.length],
    ];
}

export function useCounterparty(): NewCounterparty {
    const [draft, dispatch] = useReducer(reduceCounterparty, undefined, blankDraft);
    return {
        ...draft,
        hasIdentity: hasIdentity(draft),
        authoritative: authoritativeIdentifier(draft.identifiers),
        startNew: () => dispatch({ kind: 'start' }),
        open: (row, children) => dispatch({ kind: 'open', row, children }),
        setIdentity: (field, value) => dispatch({ kind: 'set-identity', field, value }),
        setCentres: (codes) => dispatch({ kind: 'set-centres', codes }),
        addIdentifier: (scheme, value) => dispatch({ kind: 'add-identifier', scheme, value }),
        removeIdentifier: (id) => dispatch({ kind: 'remove-identifier', id }),
        markAuthoritative: (id) => dispatch({ kind: 'mark-authoritative', id }),
        addContact: (contactType) => dispatch({ kind: 'add-contact', contactType }),
        removeContact: (id) => dispatch({ kind: 'remove-contact', id }),
        setContact: (id, field, value) => dispatch({ kind: 'set-contact', id, field, value }),
        addAgreement: (agreement) => dispatch({ kind: 'add-agreement', agreement }),
        removeAgreement: (id) => dispatch({ kind: 'remove-agreement', id }),
        setAgreement: (id, field, value) => dispatch({ kind: 'set-agreement', id, field, value }),
        addSet: (agreementId, set) => dispatch({ kind: 'add-set', agreementId, set }),
        removeSet: (agreementId, setId) => dispatch({ kind: 'remove-set', agreementId, setId }),
        setSet: (agreementId, setId, field, value) =>
            dispatch({ kind: 'set-set', agreementId, setId, field, value }),
        addSetIdentifier: (agreementId, setId, scheme, value) =>
            dispatch({ kind: 'add-set-identifier', agreementId, setId, scheme, value }),
        removeSetIdentifier: (agreementId, setId, identifierId) =>
            dispatch({ kind: 'remove-set-identifier', agreementId, setId, identifierId }),
        setCsa: (agreementId, setId, field, value) =>
            dispatch({ kind: 'set-csa', agreementId, setId, field, value }),
        toggleCsa: (agreementId, setId, field, value) =>
            dispatch({ kind: 'toggle-csa', agreementId, setId, field, value }),
        addEligibleCurrency: (agreementId, setId, currencyCode) =>
            dispatch({ kind: 'add-eligible-currency', agreementId, setId, currencyCode }),
        removeEligibleCurrency: (agreementId, setId, currencyId) =>
            dispatch({ kind: 'remove-eligible-currency', agreementId, setId, currencyId }),
        recordWritten: (counterparty) => dispatch({ kind: 'record-written', counterparty }),
        recordRefusal: (refusal) => dispatch({ kind: 'record-refusal', refusal }),
        clearRefusal: () => dispatch({ kind: 'clear-refusal' }),
    };
}
