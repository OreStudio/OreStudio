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
import type { ReportDefinition } from '../domain/report_definition.js';

export interface GetReportDefinitionsRequest {
    offset: number;
    limit: number;
}

export interface GetReportDefinitionsResponse {
    definitions: ReportDefinition[];
    total_available_count: number;
    success: boolean;
    message: string;
}

export interface SaveReportDefinitionRequest {
    data: ReportDefinition;
}

export interface SaveReportDefinitionResponse {
    success: boolean;
    message: string;
}

export interface DeleteReportDefinitionRequest {
    ids: string[];
}

export interface DeleteReportDefinitionResponse {
    success: boolean;
    message: string;
}

export interface GetReportDefinitionHistoryRequest {
    id: string;
}

export interface GetReportDefinitionHistoryResponse {
    history: ReportDefinition[];
    success: boolean;
    message: string;
}

export const subjects = {
    get_report_definitions_request: "dq.v1.report_definitions.list",
    save_report_definition_request: "dq.v1.report_definitions.save",
    delete_report_definition_request: "dq.v1.report_definitions.delete",
    get_report_definition_history_request: "dq.v1.report_definitions.history",
} as const;
