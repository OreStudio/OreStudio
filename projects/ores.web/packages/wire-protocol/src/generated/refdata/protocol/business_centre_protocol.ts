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
import type { BusinessCentre } from '../domain/business_centre.js';

export interface GetBusinessCentresRequest {
    offset: number;
    limit: number;
}

export interface GetBusinessCentresResponse {
    centres: BusinessCentre[];
    total_available_count: number;
    success: boolean;
    message: string;
}

export interface SaveBusinessCentreRequest {
    data: BusinessCentre;
}

export interface SaveBusinessCentreResponse {
    success: boolean;
    message: string;
}

export interface DeleteBusinessCentreRequest {
    codes: string[];
}

export interface DeleteBusinessCentreResponse {
    success: boolean;
    message: string;
}

export interface GetBusinessCentreHistoryRequest {
    code: string;
}

export interface GetBusinessCentreHistoryResponse {
    history: BusinessCentre[];
    success: boolean;
    message: string;
}

export const subjects = {
    get_business_centres_request: "refdata.v1.business_centres.list",
    save_business_centre_request: "refdata.v1.business_centres.save",
    delete_business_centre_request: "refdata.v1.business_centres.delete",
    get_business_centre_history_request: "refdata.v1.business_centres.history",
} as const;
