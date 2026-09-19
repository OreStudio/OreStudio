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
import type { AccountContactInformation } from '../domain/account_contact_information.js';

export interface GetAccountContactInformationsRequest {
    offset: number;
    limit: number;
}

export interface GetAccountContactInformationsResponse {
    account_contact_informations: AccountContactInformation[];
    total_available_count: number;
    success: boolean;
    message: string;
}

export interface SaveAccountContactInformationRequest {
    data: AccountContactInformation;
}

export interface SaveAccountContactInformationResponse {
    success: boolean;
    message: string;
}

export interface DeleteAccountContactInformationRequest {
    ids: string[];
}

export interface DeleteAccountContactInformationResponse {
    success: boolean;
    message: string;
}

export interface GetAccountContactInformationHistoryRequest {
    id: string;
}

export interface GetAccountContactInformationHistoryResponse {
    history: AccountContactInformation[];
    success: boolean;
    message: string;
}

export interface GetAccountContactInformationsByAccountIdRequest {
    account_id: string;
    offset: number;
    limit: number;
}

export interface GetAccountContactInformationsByAccountIdResponse {
    account_contact_informations: AccountContactInformation[];
    total_available_count: number;
    success: boolean;
    message: string;
}

export const subjects = {
    get_account_contact_informations_request: "iam.v1.account_contact_informations.list",
    save_account_contact_information_request: "iam.v1.account_contact_informations.save",
    delete_account_contact_information_request: "iam.v1.account_contact_informations.delete",
    get_account_contact_information_history_request: "iam.v1.account_contact_informations.history",
    get_account_contact_informations_by_account_id_request: "iam.v1.account_contact_informations.list_by_account_id",
} as const;
