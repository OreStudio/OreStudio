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

export interface GetPartyIdSchemesRequest {
    offset: number;
    limit: number;
}

export interface GetPartyIdSchemesResponse {
    schemes: PartyIdScheme[];
    total_available_count: number;
    success: boolean;
    message: string;
}

export interface SavePartyIdSchemeRequest {
    data: PartyIdScheme;
}

export interface SavePartyIdSchemeResponse {
    success: boolean;
    message: string;
}

export interface DeletePartyIdSchemeRequest {
    codes: string[];
}

export interface DeletePartyIdSchemeResponse {
    success: boolean;
    message: string;
}

export interface GetPartyIdSchemeHistoryRequest {
    code: string;
}

export interface GetPartyIdSchemeHistoryResponse {
    history: PartyIdScheme[];
    success: boolean;
    message: string;
}

export const subjects = {
    get_party_id_schemes_request: "refdata.v1.party_id_schemes.list",
    save_party_id_scheme_request: "refdata.v1.party_id_schemes.save",
    delete_party_id_scheme_request: "refdata.v1.party_id_schemes.delete",
    get_party_id_scheme_history_request: "refdata.v1.party_id_schemes.history",
} as const;
