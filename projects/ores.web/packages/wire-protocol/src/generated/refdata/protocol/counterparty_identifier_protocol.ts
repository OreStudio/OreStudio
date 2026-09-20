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
import type { CounterpartyIdentifier } from '../domain/counterparty_identifier.js';

export interface GetCounterpartyIdentifiersRequest {
    offset: number;
    limit: number;
}

export interface GetCounterpartyIdentifiersResponse {
    counterparty_identifiers: CounterpartyIdentifier[];
    total_available_count: number;
    success: boolean;
    message: string;
}

export interface SaveCounterpartyIdentifierRequest {
    data: CounterpartyIdentifier;
}

export interface SaveCounterpartyIdentifierResponse {
    success: boolean;
    message: string;
}

export interface DeleteCounterpartyIdentifierRequest {
    ids: string[];
}

export interface DeleteCounterpartyIdentifierResponse {
    success: boolean;
    message: string;
}

export interface GetCounterpartyIdentifierHistoryRequest {
    id: string;
}

export interface GetCounterpartyIdentifierHistoryResponse {
    history: CounterpartyIdentifier[];
    success: boolean;
    message: string;
}

export interface GetCounterpartyIdentifiersByCounterpartyIdRequest {
    counterparty_id: string;
    offset: number;
    limit: number;
}

export interface GetCounterpartyIdentifiersByCounterpartyIdResponse {
    counterparty_identifiers: CounterpartyIdentifier[];
    total_available_count: number;
    success: boolean;
    message: string;
}

export const subjects = {
    get_counterparty_identifiers_request: "refdata.v1.counterparty_identifiers.list",
    save_counterparty_identifier_request: "refdata.v1.counterparty_identifiers.save",
    delete_counterparty_identifier_request: "refdata.v1.counterparty_identifiers.delete",
    get_counterparty_identifier_history_request: "refdata.v1.counterparty_identifiers.history",
    get_counterparty_identifiers_by_counterparty_id_request: "refdata.v1.counterparty_identifiers.list_by_counterparty_id",
} as const;
