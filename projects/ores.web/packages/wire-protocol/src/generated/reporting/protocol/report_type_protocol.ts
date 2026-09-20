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
import type { ReportType } from '../domain/report_type.js';

export interface GetReportTypesRequest {
    offset: number;
    limit: number;
}

export interface GetReportTypesResponse {
    types: ReportType[];
    total_available_count: number;
    success: boolean;
    message: string;
}

export interface SaveReportTypeRequest {
    data: ReportType;
}

export interface SaveReportTypeResponse {
    success: boolean;
    message: string;
}

export interface DeleteReportTypeRequest {
    codes: string[];
}

export interface DeleteReportTypeResponse {
    success: boolean;
    message: string;
}

export interface GetReportTypeHistoryRequest {
    code: string;
}

export interface GetReportTypeHistoryResponse {
    history: ReportType[];
    success: boolean;
    message: string;
}

export const subjects = {
    get_report_types_request: "reporting.v1.report_types.list",
    save_report_type_request: "reporting.v1.report_types.save",
    delete_report_type_request: "reporting.v1.report_types.delete",
    get_report_type_history_request: "reporting.v1.report_types.history",
} as const;
