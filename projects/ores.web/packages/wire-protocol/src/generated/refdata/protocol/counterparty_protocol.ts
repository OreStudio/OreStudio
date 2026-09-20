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
import type { Counterparty } from '../domain/counterparty.js';
import type { HierarchyNode } from '../../../utility/hierarchy.js';

export interface GetCounterpartiesRequest {
    offset: number;
    limit: number;
}

export interface GetCounterpartiesResponse {
    counterparties: Counterparty[];
    total_available_count: number;
    success: boolean;
    message: string;
}

export interface SaveCounterpartyRequest {
    data: Counterparty;
}

export interface SaveCounterpartyResponse {
    success: boolean;
    message: string;
}

export interface DeleteCounterpartyRequest {
    ids: string[];
}

export interface DeleteCounterpartyResponse {
    success: boolean;
    message: string;
}

export interface GetCounterpartyHistoryRequest {
    id: string;
}

export interface GetCounterpartyHistoryResponse {
    history: Counterparty[];
    success: boolean;
    message: string;
}

export interface GetCounterpartyHierarchyRequest {
    root_id: string;
    from_root: boolean;
}

export interface GetCounterpartyHierarchyResponse {
    success: boolean;
    message: string;
    roots: HierarchyNode[];
}

/**
 * @brief Reads a counterparty as it stood at a specific version, together
 * with its identifiers and contact information as they stood during that
 * same version's [valid_from, valid_to) window. See the "Temporal composite
 * entity versioning" architecture doc.
 */
export interface GetCounterpartyCompositeAsOfRequest {
    id: string;
    version: number;
}

export interface GetCounterpartyCompositeAsOfResponse {
    success: boolean;
    message: string;
    counterparty: Counterparty;
    identifiers: CounterpartyIdentifier[];
    contacts: CounterpartyContactInformation[];
}

export const subjects = {
    get_counterparties_request: "refdata.v1.counterparties.list",
    save_counterparty_request: "refdata.v1.counterparties.save",
    delete_counterparty_request: "refdata.v1.counterparties.delete",
    get_counterparty_history_request: "refdata.v1.counterparties.history",
    get_counterparty_hierarchy_request: "refdata.v1.counterparties.hierarchy",
    get_counterparty_composite_as_of_request: "refdata.v1.counterparties.composite_as_of",
} as const;
