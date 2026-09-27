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
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';
import type { Scope } from '../../../utility/protocol.js';

export interface BadgeMappingKey {
    code_domain_code: string;
    entity_code: string;
}

export interface BadgeMappingWrite {
    code_domain_code: string;
    entity_code: string;
    badge_code: string;
}

export interface BadgeMappingChange {
    write: BadgeMappingWrite;
    precondition: Precondition;
}

export interface BadgeMappingRemoval {
    key: BadgeMappingKey;
    precondition: Precondition;
}

export interface BadgeMappingLookup {
    key: BadgeMappingKey;
    badge_mapping: BadgeMapping | null;
}

export interface BadgeMappingsFilter {
    code_domain_code: string | null;
}

export interface ListBadgeMappingsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: BadgeMappingsFilter | null;
}

export interface ListBadgeMappingsResponse {
    result: Result;
    badge_mappings: BadgeMapping[];
    total: number;
}

export interface GetBadgeMappingRequest {
    key: BadgeMappingKey;
}

export interface GetBadgeMappingResponse {
    result: Result;
    badge_mapping: BadgeMapping | null;
}

export interface GetManyBadgeMappingsRequest {
    keys: BadgeMappingKey[];
}

export interface GetManyBadgeMappingsResponse {
    result: Result;
    entries: BadgeMappingLookup[];
}

export interface PutBadgeMappingRequest {
    change: BadgeMappingChange;
    intent: ChangeIntent;
}

export interface PutBadgeMappingResponse {
    result: Result;
    badge_mapping: BadgeMapping | null;
}

export interface PutManyBadgeMappingsRequest {
    changes: BadgeMappingChange[];
    intent: ChangeIntent;
}

export interface PutManyBadgeMappingsResponse {
    result: Result;
    badge_mappings: BadgeMapping[];
}

export interface DeleteBadgeMappingRequest {
    removal: BadgeMappingRemoval;
    intent: ChangeIntent;
}

export interface DeleteBadgeMappingResponse {
    result: Result;
}

export interface DeleteManyBadgeMappingsRequest {
    removals: BadgeMappingRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyBadgeMappingsResponse {
    result: Result;
}

export interface ListByCodeDomainCodeBadgeMappingsRequest {
    code_domain_code: string;
    scope: Scope;
    offset: number;
    limit: number;
    order: Order;
    filter: BadgeMappingsFilter | null;
}

export interface ListByCodeDomainCodeBadgeMappingsResponse {
    result: Result;
    badge_mappings: BadgeMapping[];
    total: number;
}

export const subjects = {
    list_badge_mappings_request: 'dq.v1.badge_mappings.list',
    get_badge_mapping_request: 'dq.v1.badge_mappings.get',
    get_many_badge_mappings_request: 'dq.v1.badge_mappings.get_many',
    put_badge_mapping_request: 'dq.v1.badge_mappings.put',
    put_many_badge_mappings_request: 'dq.v1.badge_mappings.put_many',
    delete_badge_mapping_request: 'dq.v1.badge_mappings.delete',
    delete_many_badge_mappings_request: 'dq.v1.badge_mappings.delete_many',
    list_by_code_domain_code_badge_mappings_request:
        'dq.v1.badge_mappings.list_by_code_domain_code',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_badge_mappings_request: true,
    get_badge_mapping_request: true,
    get_many_badge_mappings_request: true,
    put_badge_mapping_request: true,
    put_many_badge_mappings_request: true,
    delete_badge_mapping_request: true,
    delete_many_badge_mappings_request: true,
    list_by_code_domain_code_badge_mappings_request: true,
} as const;
