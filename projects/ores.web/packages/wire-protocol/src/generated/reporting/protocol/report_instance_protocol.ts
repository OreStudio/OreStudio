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
import type { ReportInstance } from '../domain/report_instance.js';

export interface GetReportInstancesRequest {
    offset: number;
    limit: number;
}

export interface GetReportInstancesResponse {
    instances: ReportInstance[];
    total_available_count: number;
    success: boolean;
    message: string;
}

export interface SaveReportInstanceRequest {
    data: ReportInstance;
}

export interface SaveReportInstanceResponse {
    success: boolean;
    message: string;
}

export interface DeleteReportInstanceRequest {
    ids: string[];
}

export interface DeleteReportInstanceResponse {
    success: boolean;
    message: string;
}

export interface GetReportInstanceHistoryRequest {
    id: string;
}

export interface GetReportInstanceHistoryResponse {
    history: ReportInstance[];
    success: boolean;
    message: string;
}

export const subjects = {
    get_report_instances_request: "reporting.v1.report_instances.list",
    save_report_instance_request: "reporting.v1.report_instances.save",
    delete_report_instance_request: "reporting.v1.report_instances.delete",
    get_report_instance_history_request: "reporting.v1.report_instances.history",
} as const;
