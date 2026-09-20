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
import type { MonetaryNature } from '../domain/monetary_nature.js';

export interface GetMonetaryNaturesRequest {
    offset: number;
    limit: number;
}

export interface GetMonetaryNaturesResponse {
    types: MonetaryNature[];
    total_available_count: number;
    success: boolean;
    message: string;
}

export interface SaveMonetaryNatureRequest {
    data: MonetaryNature;
}

export interface SaveMonetaryNatureResponse {
    success: boolean;
    message: string;
}

export interface DeleteMonetaryNatureRequest {
    codes: string[];
}

export interface DeleteMonetaryNatureResponse {
    success: boolean;
    message: string;
}

export interface GetMonetaryNatureHistoryRequest {
    code: string;
}

export interface GetMonetaryNatureHistoryResponse {
    history: MonetaryNature[];
    success: boolean;
    message: string;
}

export const subjects = {
    get_monetary_natures_request: "refdata.v1.monetary_natures.list",
    save_monetary_nature_request: "refdata.v1.monetary_natures.save",
    delete_monetary_nature_request: "refdata.v1.monetary_natures.delete",
    get_monetary_nature_history_request: "refdata.v1.monetary_natures.history",
} as const;
