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
import type { PartyIdScheme } from '../domain/party_id_scheme.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface PartyIdSchemeKey {
    code: string;
}

export interface PartyIdSchemeWrite {
    code: string;
    name: string;
    description: string;
    coding_scheme_code: string;
    display_order: number;
    max_cardinality: number | null;
}

export interface PartyIdSchemeChange {
    write: PartyIdSchemeWrite;
    precondition: Precondition;
}

export interface PartyIdSchemeRemoval {
    key: PartyIdSchemeKey;
    precondition: Precondition;
}

export interface PartyIdSchemeLookup {
    key: PartyIdSchemeKey;
    party_id_scheme: PartyIdScheme | null;
}

export interface PartyIdSchemeEvent {
    event_id: string;
    key: PartyIdSchemeKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface PartyIdSchemeVersionKey {
    party_id_scheme: PartyIdSchemeKey;
    version: number;
}

export interface PartyIdSchemeVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListPartyIdSchemesRequest {
    offset: number;
    limit: number;
    order: Order;
}

export interface ListPartyIdSchemesResponse {
    result: Result;
    schemes: PartyIdScheme[];
    total: number;
}

export interface GetPartyIdSchemeRequest {
    key: PartyIdSchemeKey;
}

export interface GetPartyIdSchemeResponse {
    result: Result;
    party_id_scheme: PartyIdScheme | null;
}

export interface GetManyPartyIdSchemesRequest {
    keys: PartyIdSchemeKey[];
}

export interface GetManyPartyIdSchemesResponse {
    result: Result;
    entries: PartyIdSchemeLookup[];
}

export interface PutPartyIdSchemeRequest {
    change: PartyIdSchemeChange;
    intent: ChangeIntent;
}

export interface PutPartyIdSchemeResponse {
    result: Result;
    party_id_scheme: PartyIdScheme;
}

export interface PutManyPartyIdSchemesRequest {
    changes: PartyIdSchemeChange[];
    intent: ChangeIntent;
}

export interface PutManyPartyIdSchemesResponse {
    result: Result;
    schemes: PartyIdScheme[];
}

export interface DeletePartyIdSchemeRequest {
    removal: PartyIdSchemeRemoval;
    intent: ChangeIntent;
}

export interface DeletePartyIdSchemeResponse {
    result: Result;
}

export interface DeleteManyPartyIdSchemesRequest {
    removals: PartyIdSchemeRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyPartyIdSchemesResponse {
    result: Result;
}

export interface ListPartyIdSchemeVersionsRequest {
    key: PartyIdSchemeKey;
    offset: number;
    limit: number;
    order: Order;
    filter: PartyIdSchemeVersionsFilter | null;
}

export interface ListPartyIdSchemeVersionsResponse {
    result: Result;
    versions: PartyIdScheme[];
    total: number;
}

export interface GetPartyIdSchemeVersionRequest {
    key: PartyIdSchemeVersionKey;
}

export interface GetPartyIdSchemeVersionResponse {
    result: Result;
    version: PartyIdScheme;
}

export const subjects = {
    list_party_id_schemes_request: "refdata.v1.party_id_schemes.list",
    get_party_id_scheme_request: "refdata.v1.party_id_schemes.get",
    get_many_party_id_schemes_request: "refdata.v1.party_id_schemes.get_many",
    put_party_id_scheme_request: "refdata.v1.party_id_schemes.put",
    put_many_party_id_schemes_request: "refdata.v1.party_id_schemes.put_many",
    delete_party_id_scheme_request: "refdata.v1.party_id_schemes.delete",
    delete_many_party_id_schemes_request: "refdata.v1.party_id_schemes.delete_many",
    list_party_id_scheme_versions_request: "refdata.v1.party_id_schemes_versions.list",
    get_party_id_scheme_version_request: "refdata.v1.party_id_schemes_versions.get",
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_party_id_schemes_request: true,
    get_party_id_scheme_request: true,
    get_many_party_id_schemes_request: true,
    put_party_id_scheme_request: true,
    put_many_party_id_schemes_request: true,
    delete_party_id_scheme_request: true,
    delete_many_party_id_schemes_request: true,
    list_party_id_scheme_versions_request: true,
    get_party_id_scheme_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: "refdata.v1.party_id_schemes_events.created",
    updated: "refdata.v1.party_id_schemes_events.updated",
    deleted: "refdata.v1.party_id_schemes_events.deleted",
} as const;
