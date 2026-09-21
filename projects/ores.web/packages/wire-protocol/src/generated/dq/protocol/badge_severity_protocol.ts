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
import type { BadgeSeverity } from '../domain/badge_severity.js';

export interface GetBadgeSeveritiesRequest {
    offset: number;
    limit: number;
}

export interface GetBadgeSeveritiesResponse {
    severities: BadgeSeverity[];
    total_available_count: number;
    success: boolean;
    message: string;
}

export interface SaveBadgeSeverityRequest {
    data: BadgeSeverity;
}

export interface SaveBadgeSeverityResponse {
    success: boolean;
    message: string;
}

export interface DeleteBadgeSeverityRequest {
    codes: string[];
}

export interface DeleteBadgeSeverityResponse {
    success: boolean;
    message: string;
}

export interface GetBadgeSeverityHistoryRequest {
    code: string;
}

export interface GetBadgeSeverityHistoryResponse {
    history: BadgeSeverity[];
    success: boolean;
    message: string;
}

export const subjects = {
    get_badge_severities_request: "dq.v1.badge_severities.list",
    save_badge_severity_request: "dq.v1.badge_severities.save",
    delete_badge_severity_request: "dq.v1.badge_severities.delete",
    get_badge_severity_history_request: "dq.v1.badge_severities.history",
} as const;
