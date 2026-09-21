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
import type { LeiRelationship } from '../domain/lei_relationship.js';

export interface GetLeiRelationshipsRequest {
    offset: number;
    limit: number;
}

export interface GetLeiRelationshipsResponse {
    relationships: LeiRelationship[];
    total_available_count: number;
    success: boolean;
    message: string;
}

export interface SaveLeiRelationshipRequest {
    data: LeiRelationship;
}

export interface SaveLeiRelationshipResponse {
    success: boolean;
    message: string;
}

export interface DeleteLeiRelationshipRequest {
    relationship_start_node_node_ids: string[];
}

export interface DeleteLeiRelationshipResponse {
    success: boolean;
    message: string;
}

export interface GetLeiRelationshipHistoryRequest {
    relationship_start_node_node_id: string;
}

export interface GetLeiRelationshipHistoryResponse {
    history: LeiRelationship[];
    success: boolean;
    message: string;
}

export const subjects = {
    get_lei_relationships_request: "dq.v1.lei_relationships.list",
    save_lei_relationship_request: "dq.v1.lei_relationships.save",
    delete_lei_relationship_request: "dq.v1.lei_relationships.delete",
    get_lei_relationship_history_request: "dq.v1.lei_relationships.history",
} as const;
