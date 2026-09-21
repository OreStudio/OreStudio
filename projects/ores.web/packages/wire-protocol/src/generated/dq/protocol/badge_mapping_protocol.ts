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
import type { BadgeMapping } from '../domain/badge_mapping.js';

export interface GetBadgeMappingsRequest {
    offset: number;
    limit: number;
}

export interface GetBadgeMappingsResponse {
    badge_mappings: BadgeMapping[];
    total_available_count: number;
    success: boolean;
    message: string;
}

export interface SaveBadgeMappingRequest {
    badge_mappings: BadgeMapping[];
}

export interface SaveBadgeMappingResponse {
    success: boolean;
    message: string;
}

export interface DeleteBadgeMappingRequest {
    code_domain_codes: string[];
    entity_codes: string[];
}

export interface DeleteBadgeMappingResponse {
    success: boolean;
    message: string;
}

export interface CountBadgeMappingsByCodeDomainRequest {
    code_domain_code: string;
}

export interface CountBadgeMappingsByCodeDomainResponse {
    total_available_count: number;
}

export interface CountBadgeMappingsByEntityRequest {
    entity_code: string;
}

export interface CountBadgeMappingsByEntityResponse {
    total_available_count: number;
}

export interface BadgeMappingView {
    badge_mapping: BadgeMapping;
}

export const subjects = {
    get_badge_mappings_request: "dq.v1.badge_mappings.list",
    save_badge_mapping_request: "dq.v1.badge_mappings.save",
    delete_badge_mapping_request: "dq.v1.badge_mappings.delete",
    count_badge_mappings_by_code_domain_request: "dq.v1.badge_mappings.count_by_code_domain_code",
    count_badge_mappings_by_entity_request: "dq.v1.badge_mappings.count_by_entity_code",
} as const;
