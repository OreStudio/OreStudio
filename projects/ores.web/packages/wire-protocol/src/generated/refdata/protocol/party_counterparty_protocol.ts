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

export interface GetPartyCounterpartiesRequest {
    offset: number;
    limit: number;
}

export interface GetPartyCounterpartiesResponse {
    party_counterparties: PartyCounterparty[];
    total_available_count: number;
    success: boolean;
    message: string;
}

export interface GetPartyCounterpartiesByPartyRequest {
    party_id: string;
    offset: number;
    limit: number;
}

export interface GetPartyCounterpartiesByPartyResponse {
    party_counterparties: PartyCounterpartyView[];
    total_available_count: number;
    success: boolean;
    message: string;
}

export interface SavePartyCounterpartyRequest {
    party_counterparties: PartyCounterparty[];
}

export interface SavePartyCounterpartyResponse {
    success: boolean;
    message: string;
}

export interface DeletePartyCounterpartyRequest {
    party_ids: string[];
    counterparty_ids: string[];
}

export interface DeletePartyCounterpartyResponse {
    success: boolean;
    message: string;
}

export interface CountPartyCounterpartiesByPartyRequest {
    party_id: string;
}

export interface CountPartyCounterpartiesByPartyResponse {
    total_available_count: number;
}

export interface CountPartyCounterpartiesByCounterpartyRequest {
    counterparty_id: string;
}

export interface CountPartyCounterpartiesByCounterpartyResponse {
    total_available_count: number;
}

export interface PartyCounterpartyView {
    party_counterparty: PartyCounterparty;
}

export const subjects = {
    get_party_counterparties_request: "refdata.v1.party_counterparties.list",
    get_party_counterparties_by_party_request: "refdata.v1.party_counterparties.list_by_party_id",
    save_party_counterparty_request: "refdata.v1.party_counterparties.save",
    delete_party_counterparty_request: "refdata.v1.party_counterparties.delete",
    count_party_counterparties_by_party_request: "refdata.v1.party_counterparties.count_by_party_id",
    count_party_counterparties_by_counterparty_request: "refdata.v1.party_counterparties.count_by_counterparty_id",
} as const;
