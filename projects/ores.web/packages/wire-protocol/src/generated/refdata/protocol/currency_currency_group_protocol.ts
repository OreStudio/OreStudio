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
import type { CurrencyCurrencyGroup } from '../domain/currency_currency_group.js';

export interface GetCurrencyCurrencyGroupsRequest {
    offset: number;
    limit: number;
}

export interface GetCurrencyCurrencyGroupsResponse {
    currency_currency_groups: CurrencyCurrencyGroup[];
    total_available_count: number;
    success: boolean;
    message: string;
}

export interface GetCurrencyCurrencyGroupsByCurrencyRequest {
    currency_iso_code: string;
    offset: number;
    limit: number;
}

export interface GetCurrencyCurrencyGroupsByCurrencyResponse {
    currency_currency_groups: CurrencyCurrencyGroupView[];
    total_available_count: number;
    success: boolean;
    message: string;
}

export interface SaveCurrencyCurrencyGroupRequest {
    currency_currency_groups: CurrencyCurrencyGroup[];
}

export interface SaveCurrencyCurrencyGroupResponse {
    success: boolean;
    message: string;
}

export interface DeleteCurrencyCurrencyGroupRequest {
    currency_iso_codes: string[];
    currency_group_codes: string[];
}

export interface DeleteCurrencyCurrencyGroupResponse {
    success: boolean;
    message: string;
}

export interface CountCurrencyCurrencyGroupsByCurrencyRequest {
    currency_iso_code: string;
}

export interface CountCurrencyCurrencyGroupsByCurrencyResponse {
    total_available_count: number;
}

export interface CountCurrencyCurrencyGroupsByGroupRequest {
    currency_group_code: string;
}

export interface CountCurrencyCurrencyGroupsByGroupResponse {
    total_available_count: number;
}

export interface CurrencyCurrencyGroupView {
    currency_currency_group: CurrencyCurrencyGroup;
}

export const subjects = {
    get_currency_currency_groups_request: "refdata.v1.currency_currency_groups.list",
    get_currency_currency_groups_by_currency_request: "refdata.v1.currency_currency_groups.list_by_currency_iso_code",
    save_currency_currency_group_request: "refdata.v1.currency_currency_groups.save",
    delete_currency_currency_group_request: "refdata.v1.currency_currency_groups.delete",
    count_currency_currency_groups_by_currency_request: "refdata.v1.currency_currency_groups.count_by_currency_iso_code",
    count_currency_currency_groups_by_group_request: "refdata.v1.currency_currency_groups.count_by_currency_group_code",
} as const;
