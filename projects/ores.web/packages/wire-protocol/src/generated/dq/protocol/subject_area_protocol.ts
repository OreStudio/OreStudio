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
import type { SubjectArea } from '../domain/subject_area.js';

export interface GetSubjectAreasRequest {
    offset: number;
    limit: number;
}

export interface GetSubjectAreasResponse {
    areas: SubjectArea[];
    total_available_count: number;
    success: boolean;
    message: string;
}

export interface SaveSubjectAreaRequest {
    data: SubjectArea;
}

export interface SaveSubjectAreaResponse {
    success: boolean;
    message: string;
}

export interface DeleteSubjectAreaRequest {
    names: string[];
    domain_names: string[];
}

export interface DeleteSubjectAreaResponse {
    success: boolean;
    message: string;
}

export interface GetSubjectAreaHistoryRequest {
    name: string;
}

export interface GetSubjectAreaHistoryResponse {
    history: SubjectArea[];
    success: boolean;
    message: string;
}

export const subjects = {
    get_subject_areas_request: "dq.v1.subject_areas.list",
    save_subject_area_request: "dq.v1.subject_areas.save",
    delete_subject_area_request: "dq.v1.subject_areas.delete",
    get_subject_area_history_request: "dq.v1.subject_areas.history",
} as const;
