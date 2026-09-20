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
import type { Party } from '../domain/party.js';
import type { PartyContactInformation } from '../domain/party_contact_information.js';
import type { PartyIdentifier } from '../domain/party_identifier.js';
import type { HierarchyNode } from '../../../utility/hierarchy.js';

export interface GetPartiesRequest {
    offset: number;
    limit: number;
}

export interface GetPartiesResponse {
    parties: Party[];
    total_available_count: number;
    success: boolean;
    message: string;
}

export interface SavePartyRequest {
    data: Party;
}

export interface SavePartyResponse {
    success: boolean;
    message: string;
}

export interface DeletePartyRequest {
    ids: string[];
}

export interface DeletePartyResponse {
    success: boolean;
    message: string;
}

export interface GetPartyHistoryRequest {
    id: string;
}

export interface GetPartyHistoryResponse {
    history: Party[];
    success: boolean;
    message: string;
}

export interface GetPartyHierarchyRequest {
    root_id: string;
    from_root: boolean;
}

export interface GetPartyHierarchyResponse {
    success: boolean;
    message: string;
    roots: HierarchyNode[];
}

export interface ReadPartiesForCacheRequest {
    tenant_id: string;
}

export interface ReadPartiesForCacheResponse {
    success: boolean;
    message: string;
    parties: Party[];
}

/**
 * @brief Reads a party as it stood at a specific version, together with its
 * identifiers and contact information as they stood during that same
 * version's [valid_from, valid_to) window. See the "Temporal composite
 * entity versioning" architecture doc.
 */
export interface GetPartyCompositeAsOfRequest {
    id: string;
    version: number;
}

export interface GetPartyCompositeAsOfResponse {
    success: boolean;
    message: string;
    party: Party;
    identifiers: PartyIdentifier[];
    contacts: PartyContactInformation[];
}

export const subjects = {
    get_parties_request: "refdata.v1.parties.list",
    save_party_request: "refdata.v1.parties.save",
    delete_party_request: "refdata.v1.parties.delete",
    get_party_history_request: "refdata.v1.parties.history",
    get_party_hierarchy_request: "refdata.v1.parties.hierarchy",
    read_parties_for_cache_request: "refdata.v1.parties.read",
    get_party_composite_as_of_request: "refdata.v1.parties.composite_as_of",
} as const;
