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

export interface GetPartyContactInformationsRequest {
    offset: number;
    limit: number;
}

export interface GetPartyContactInformationsResponse {
    party_contact_informations: PartyContactInformation[];
    total_available_count: number;
    success: boolean;
    message: string;
}

export interface SavePartyContactInformationRequest {
    data: PartyContactInformation;
}

export interface SavePartyContactInformationResponse {
    success: boolean;
    message: string;
}

export interface DeletePartyContactInformationRequest {
    ids: string[];
}

export interface DeletePartyContactInformationResponse {
    success: boolean;
    message: string;
}

export interface GetPartyContactInformationHistoryRequest {
    id: string;
}

export interface GetPartyContactInformationHistoryResponse {
    history: PartyContactInformation[];
    success: boolean;
    message: string;
}

export interface GetPartyContactInformationsByPartyIdRequest {
    party_id: string;
    offset: number;
    limit: number;
}

export interface GetPartyContactInformationsByPartyIdResponse {
    party_contact_informations: PartyContactInformation[];
    total_available_count: number;
    success: boolean;
    message: string;
}

export const subjects = {
    get_party_contact_informations_request: "refdata.v1.party_contact_informations.list",
    save_party_contact_information_request: "refdata.v1.party_contact_informations.save",
    delete_party_contact_information_request: "refdata.v1.party_contact_informations.delete",
    get_party_contact_information_history_request: "refdata.v1.party_contact_informations.history",
    get_party_contact_informations_by_party_id_request: "refdata.v1.party_contact_informations.list_by_party_id",
} as const;
