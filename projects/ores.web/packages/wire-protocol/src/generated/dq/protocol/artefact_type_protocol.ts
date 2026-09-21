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
import type { ArtefactType } from '../domain/artefact_type.js';

export interface GetArtefactTypesRequest {
    offset: number;
    limit: number;
}

export interface GetArtefactTypesResponse {
    types: ArtefactType[];
    total_available_count: number;
    success: boolean;
    message: string;
}

export interface SaveArtefactTypeRequest {
    data: ArtefactType;
}

export interface SaveArtefactTypeResponse {
    success: boolean;
    message: string;
}

export interface DeleteArtefactTypeRequest {
    codes: string[];
}

export interface DeleteArtefactTypeResponse {
    success: boolean;
    message: string;
}

export interface GetArtefactTypeHistoryRequest {
    code: string;
}

export interface GetArtefactTypeHistoryResponse {
    history: ArtefactType[];
    success: boolean;
    message: string;
}

export const subjects = {
    get_artefact_types_request: "dq.v1.artefact_types.list",
    save_artefact_type_request: "dq.v1.artefact_types.save",
    delete_artefact_type_request: "dq.v1.artefact_types.delete",
    get_artefact_type_history_request: "dq.v1.artefact_types.history",
} as const;
