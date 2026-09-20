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
import type { CounterpartyContactInformation } from '../domain/counterparty_contact_information.js';

export interface GetCounterpartyContactInformationsRequest {
    offset: number;
    limit: number;
}

export interface GetCounterpartyContactInformationsResponse {
    counterparty_contact_informations: CounterpartyContactInformation[];
    total_available_count: number;
    success: boolean;
    message: string;
}

export interface SaveCounterpartyContactInformationRequest {
    data: CounterpartyContactInformation;
}

export interface SaveCounterpartyContactInformationResponse {
    success: boolean;
    message: string;
}

export interface DeleteCounterpartyContactInformationRequest {
    ids: string[];
}

export interface DeleteCounterpartyContactInformationResponse {
    success: boolean;
    message: string;
}

export interface GetCounterpartyContactInformationHistoryRequest {
    id: string;
}

export interface GetCounterpartyContactInformationHistoryResponse {
    history: CounterpartyContactInformation[];
    success: boolean;
    message: string;
}

export interface GetCounterpartyContactInformationsByCounterpartyIdRequest {
    counterparty_id: string;
    offset: number;
    limit: number;
}

export interface GetCounterpartyContactInformationsByCounterpartyIdResponse {
    counterparty_contact_informations: CounterpartyContactInformation[];
    total_available_count: number;
    success: boolean;
    message: string;
}

export const subjects = {
    get_counterparty_contact_informations_request: "refdata.v1.counterparty_contact_informations.list",
    save_counterparty_contact_information_request: "refdata.v1.counterparty_contact_informations.save",
    delete_counterparty_contact_information_request: "refdata.v1.counterparty_contact_informations.delete",
    get_counterparty_contact_information_history_request: "refdata.v1.counterparty_contact_informations.history",
    get_counterparty_contact_informations_by_counterparty_id_request: "refdata.v1.counterparty_contact_informations.list_by_counterparty_id",
} as const;
