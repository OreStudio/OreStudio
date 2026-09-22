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
 */
/**
 * AUTO-GENERATED FILE - DO NOT EDIT MANUALLY
 * Template: ts_protocol.ts.mustache
 * To modify, update the template and regenerate.
 */
import type { PartyCounterparty } from '../domain/party_counterparty.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';
import type { Scope } from '../../../utility/protocol.js';

export interface PartyCounterpartyKey {
    party_id: string;
    counterparty_id: string;
}

export interface PartyCounterpartyWrite {
    party_id: string;
    counterparty_id: string;
}

export interface PartyCounterpartyChange {
    write: PartyCounterpartyWrite;
    precondition: Precondition;
}

export interface PartyCounterpartyRemoval {
    key: PartyCounterpartyKey;
    precondition: Precondition;
}

export interface PartyCounterpartyLookup {
    key: PartyCounterpartyKey;
    party_counterparty: PartyCounterparty | null;
}

export interface PartyCounterpartiesFilter {
    party_id: string | null;
}

export interface PartyCounterpartyEvent {
    event_id: string;
    key: PartyCounterpartyKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface ListPartyCounterpartiesRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: PartyCounterpartiesFilter | null;
}

export interface ListPartyCounterpartiesResponse {
    result: Result;
    party_counterparties: PartyCounterparty[];
    total: number;
}

export interface GetPartyCounterpartyRequest {
    key: PartyCounterpartyKey;
}

export interface GetPartyCounterpartyResponse {
    result: Result;
    party_counterparty: PartyCounterparty | null;
}

export interface GetManyPartyCounterpartiesRequest {
    keys: PartyCounterpartyKey[];
}

export interface GetManyPartyCounterpartiesResponse {
    result: Result;
    entries: PartyCounterpartyLookup[];
}

export interface PutPartyCounterpartyRequest {
    change: PartyCounterpartyChange;
    intent: ChangeIntent;
}

export interface PutPartyCounterpartyResponse {
    result: Result;
    party_counterparty: PartyCounterparty;
}

export interface PutManyPartyCounterpartiesRequest {
    changes: PartyCounterpartyChange[];
    intent: ChangeIntent;
}

export interface PutManyPartyCounterpartiesResponse {
    result: Result;
    party_counterparties: PartyCounterparty[];
}

export interface DeletePartyCounterpartyRequest {
    removal: PartyCounterpartyRemoval;
    intent: ChangeIntent;
}

export interface DeletePartyCounterpartyResponse {
    result: Result;
}

export interface DeleteManyPartyCounterpartiesRequest {
    removals: PartyCounterpartyRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyPartyCounterpartiesResponse {
    result: Result;
}

export interface ListByPartyIdPartyCounterpartiesRequest {
    party_id: string;
    scope: Scope;
    offset: number;
    limit: number;
    order: Order;
    filter: PartyCounterpartiesFilter | null;
}

export interface ListByPartyIdPartyCounterpartiesResponse {
    result: Result;
    party_counterparties: PartyCounterparty[];
    total: number;
}

export const subjects = {
    list_party_counterparties_request: "refdata.v1.party_counterparties.list",
    get_party_counterparty_request: "refdata.v1.party_counterparties.get",
    get_many_party_counterparties_request: "refdata.v1.party_counterparties.get_many",
    put_party_counterparty_request: "refdata.v1.party_counterparties.put",
    put_many_party_counterparties_request: "refdata.v1.party_counterparties.put_many",
    delete_party_counterparty_request: "refdata.v1.party_counterparties.delete",
    delete_many_party_counterparties_request: "refdata.v1.party_counterparties.delete_many",
    list_by_party_id_party_counterparties_request: "refdata.v1.party_counterparties.list_by_party_id",
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_party_counterparties_request: true,
    get_party_counterparty_request: true,
    get_many_party_counterparties_request: true,
    put_party_counterparty_request: true,
    put_many_party_counterparties_request: true,
    delete_party_counterparty_request: true,
    delete_many_party_counterparties_request: true,
    list_by_party_id_party_counterparties_request: true,
} as const;
