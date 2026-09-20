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
import type { PartyStatus } from '../domain/party_status.js';

export interface GetPartyStatusesRequest {
    offset: number;
    limit: number;
}

export interface GetPartyStatusesResponse {
    statuses: PartyStatus[];
    total_available_count: number;
    success: boolean;
    message: string;
}

export interface SavePartyStatusRequest {
    data: PartyStatus;
}

export interface SavePartyStatusResponse {
    success: boolean;
    message: string;
}

export interface DeletePartyStatusRequest {
    codes: string[];
}

export interface DeletePartyStatusResponse {
    success: boolean;
    message: string;
}

export interface GetPartyStatusHistoryRequest {
    code: string;
}

export interface GetPartyStatusHistoryResponse {
    history: PartyStatus[];
    success: boolean;
    message: string;
}

export const subjects = {
    get_party_statuses_request: "refdata.v1.party_statuses.list",
    save_party_status_request: "refdata.v1.party_statuses.save",
    delete_party_status_request: "refdata.v1.party_statuses.delete",
    get_party_status_history_request: "refdata.v1.party_statuses.history",
} as const;
