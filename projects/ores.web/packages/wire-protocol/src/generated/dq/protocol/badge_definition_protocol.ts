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
import type { BadgeDefinition } from '../domain/badge_definition.js';

export interface GetBadgeDefinitionsRequest {
    offset: number;
    limit: number;
}

export interface GetBadgeDefinitionsResponse {
    definitions: BadgeDefinition[];
    total_available_count: number;
    success: boolean;
    message: string;
}

export interface SaveBadgeDefinitionRequest {
    data: BadgeDefinition;
}

export interface SaveBadgeDefinitionResponse {
    success: boolean;
    message: string;
}

export interface DeleteBadgeDefinitionRequest {
    codes: string[];
}

export interface DeleteBadgeDefinitionResponse {
    success: boolean;
    message: string;
}

export interface GetBadgeDefinitionHistoryRequest {
    code: string;
}

export interface GetBadgeDefinitionHistoryResponse {
    history: BadgeDefinition[];
    success: boolean;
    message: string;
}

export const subjects = {
    get_badge_definitions_request: "dq.v1.badge_definitions.list",
    save_badge_definition_request: "dq.v1.badge_definitions.save",
    delete_badge_definition_request: "dq.v1.badge_definitions.delete",
    get_badge_definition_history_request: "dq.v1.badge_definitions.history",
} as const;
