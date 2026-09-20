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
import type { DepositConvention } from '../domain/deposit_convention.js';

export interface GetDepositConventionsRequest {
    offset: number;
    limit: number;
}

export interface GetDepositConventionsResponse {
    deposit_conventions: DepositConvention[];
    total_available_count: number;
    success: boolean;
    message: string;
}

export interface SaveDepositConventionRequest {
    data: DepositConvention;
}

export interface SaveDepositConventionResponse {
    success: boolean;
    message: string;
}

export interface DeleteDepositConventionRequest {
    ids: string[];
}

export interface DeleteDepositConventionResponse {
    success: boolean;
    message: string;
}

export interface GetDepositConventionHistoryRequest {
    id: string;
}

export interface GetDepositConventionHistoryResponse {
    history: DepositConvention[];
    success: boolean;
    message: string;
}

export const subjects = {
    get_deposit_conventions_request: "refdata.v1.deposit_conventions.list",
    save_deposit_convention_request: "refdata.v1.deposit_conventions.save",
    delete_deposit_convention_request: "refdata.v1.deposit_conventions.delete",
    get_deposit_convention_history_request: "refdata.v1.deposit_conventions.history",
} as const;
