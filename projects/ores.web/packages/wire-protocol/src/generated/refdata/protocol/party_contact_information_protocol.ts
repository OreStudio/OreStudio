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
import type { PartyContactInformation } from '../domain/party_contact_information.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';
import type { Scope } from '../../../utility/protocol.js';

export interface PartyContactInformationKey {
    id: string;
}

export interface PartyContactInformationWrite {
    id: string;
    contact_type: string;
    street_line_1: string;
    street_line_2: string;
    city: string;
    state: string;
    country_code: string;
    postal_code: string;
    phone: string;
    email: string;
    web_page: string;
}

export interface PartyContactInformationChange {
    write: PartyContactInformationWrite;
    precondition: Precondition;
}

export interface PartyContactInformationRemoval {
    key: PartyContactInformationKey;
    precondition: Precondition;
}

export interface PartyContactInformationLookup {
    key: PartyContactInformationKey;
    party_contact_information: PartyContactInformation | null;
}

export interface PartyContactInformationsFilter {
    party_id: string | null;
}

export interface PartyContactInformationEvent {
    event_id: string;
    key: PartyContactInformationKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface PartyContactInformationVersionKey {
    party_contact_information: PartyContactInformationKey;
    version: number;
}

export interface PartyContactInformationVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListPartyContactInformationsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: PartyContactInformationsFilter | null;
}

export interface ListPartyContactInformationsResponse {
    result: Result;
    party_contact_informations: PartyContactInformation[];
    total: number;
}

export interface GetPartyContactInformationRequest {
    key: PartyContactInformationKey;
}

export interface GetPartyContactInformationResponse {
    result: Result;
    party_contact_information: PartyContactInformation | null;
}

export interface GetManyPartyContactInformationsRequest {
    keys: PartyContactInformationKey[];
}

export interface GetManyPartyContactInformationsResponse {
    result: Result;
    entries: PartyContactInformationLookup[];
}

export interface PutPartyContactInformationRequest {
    change: PartyContactInformationChange;
    intent: ChangeIntent;
}

export interface PutPartyContactInformationResponse {
    result: Result;
    party_contact_information: PartyContactInformation;
}

export interface PutManyPartyContactInformationsRequest {
    changes: PartyContactInformationChange[];
    intent: ChangeIntent;
}

export interface PutManyPartyContactInformationsResponse {
    result: Result;
    party_contact_informations: PartyContactInformation[];
}

export interface DeletePartyContactInformationRequest {
    removal: PartyContactInformationRemoval;
    intent: ChangeIntent;
}

export interface DeletePartyContactInformationResponse {
    result: Result;
}

export interface DeleteManyPartyContactInformationsRequest {
    removals: PartyContactInformationRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyPartyContactInformationsResponse {
    result: Result;
}

export interface ListByPartyIdPartyContactInformationsRequest {
    party_id: string;
    scope: Scope;
    offset: number;
    limit: number;
    order: Order;
    filter: PartyContactInformationsFilter | null;
}

export interface ListByPartyIdPartyContactInformationsResponse {
    result: Result;
    party_contact_informations: PartyContactInformation[];
    total: number;
}

export interface ListPartyContactInformationVersionsRequest {
    key: PartyContactInformationKey;
    offset: number;
    limit: number;
    order: Order;
    filter: PartyContactInformationVersionsFilter | null;
}

export interface ListPartyContactInformationVersionsResponse {
    result: Result;
    versions: PartyContactInformation[];
    total: number;
}

export interface GetPartyContactInformationVersionRequest {
    key: PartyContactInformationVersionKey;
}

export interface GetPartyContactInformationVersionResponse {
    result: Result;
    version: PartyContactInformation;
}

export const subjects = {
    list_party_contact_informations_request: "refdata.v1.party_contact_informations.list",
    get_party_contact_information_request: "refdata.v1.party_contact_informations.get",
    get_many_party_contact_informations_request: "refdata.v1.party_contact_informations.get_many",
    put_party_contact_information_request: "refdata.v1.party_contact_informations.put",
    put_many_party_contact_informations_request: "refdata.v1.party_contact_informations.put_many",
    delete_party_contact_information_request: "refdata.v1.party_contact_informations.delete",
    delete_many_party_contact_informations_request: "refdata.v1.party_contact_informations.delete_many",
    list_by_party_id_party_contact_informations_request: "refdata.v1.party_contact_informations.list_by_party_id",
    list_party_contact_information_versions_request: "refdata.v1.party_contact_informations_versions.list",
    get_party_contact_information_version_request: "refdata.v1.party_contact_informations_versions.get",
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_party_contact_informations_request: true,
    get_party_contact_information_request: true,
    get_many_party_contact_informations_request: true,
    put_party_contact_information_request: true,
    put_many_party_contact_informations_request: true,
    delete_party_contact_information_request: true,
    delete_many_party_contact_informations_request: true,
    list_by_party_id_party_contact_informations_request: true,
    list_party_contact_information_versions_request: true,
    get_party_contact_information_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: "refdata.v1.party_contact_informations_events.created",
    updated: "refdata.v1.party_contact_informations_events.updated",
    deleted: "refdata.v1.party_contact_informations_events.deleted",
} as const;
