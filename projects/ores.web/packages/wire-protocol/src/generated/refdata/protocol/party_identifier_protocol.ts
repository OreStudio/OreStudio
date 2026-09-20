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
import type { PartyIdentifier } from '../domain/party_identifier.js';

export interface GetPartyIdentifiersRequest {
    offset: number;
    limit: number;
}

export interface GetPartyIdentifiersResponse {
    party_identifiers: PartyIdentifier[];
    total_available_count: number;
    success: boolean;
    message: string;
}

export interface SavePartyIdentifierRequest {
    data: PartyIdentifier;
}

export interface SavePartyIdentifierResponse {
    success: boolean;
    message: string;
}

export interface DeletePartyIdentifierRequest {
    ids: string[];
}

export interface DeletePartyIdentifierResponse {
    success: boolean;
    message: string;
}

export interface GetPartyIdentifierHistoryRequest {
    id: string;
}

export interface GetPartyIdentifierHistoryResponse {
    history: PartyIdentifier[];
    success: boolean;
    message: string;
}

export interface GetPartyIdentifiersByPartyIdRequest {
    party_id: string;
    offset: number;
    limit: number;
}

export interface GetPartyIdentifiersByPartyIdResponse {
    party_identifiers: PartyIdentifier[];
    total_available_count: number;
    success: boolean;
    message: string;
}

export const subjects = {
    get_party_identifiers_request: "refdata.v1.party_identifiers.list",
    save_party_identifier_request: "refdata.v1.party_identifiers.save",
    delete_party_identifier_request: "refdata.v1.party_identifiers.delete",
    get_party_identifier_history_request: "refdata.v1.party_identifiers.history",
    get_party_identifiers_by_party_id_request: "refdata.v1.party_identifiers.list_by_party_id",
} as const;
