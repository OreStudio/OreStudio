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
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface BadgeDefinitionKey {
    code: string;
}

export interface BadgeDefinitionWrite {
    code: string;
    name: string;
    description: string;
    background_colour: string;
    text_colour: string;
    severity_code: string;
    css_class: string;
    display_order: number;
}

export interface BadgeDefinitionChange {
    write: BadgeDefinitionWrite;
    precondition: Precondition;
}

export interface BadgeDefinitionRemoval {
    key: BadgeDefinitionKey;
    precondition: Precondition;
}

export interface BadgeDefinitionLookup {
    key: BadgeDefinitionKey;
    badge_definition: BadgeDefinition | null;
}

export interface BadgeDefinitionEvent {
    event_id: string;
    key: BadgeDefinitionKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface BadgeDefinitionVersionKey {
    badge_definition: BadgeDefinitionKey;
    version: number;
}

export interface BadgeDefinitionVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListBadgeDefinitionsRequest {
    offset: number;
    limit: number;
    order: Order;
}

export interface ListBadgeDefinitionsResponse {
    result: Result;
    definitions: BadgeDefinition[];
    total: number;
}

export interface GetBadgeDefinitionRequest {
    key: BadgeDefinitionKey;
}

export interface GetBadgeDefinitionResponse {
    result: Result;
    badge_definition: BadgeDefinition | null;
}

export interface GetManyBadgeDefinitionsRequest {
    keys: BadgeDefinitionKey[];
}

export interface GetManyBadgeDefinitionsResponse {
    result: Result;
    entries: BadgeDefinitionLookup[];
}

export interface PutBadgeDefinitionRequest {
    change: BadgeDefinitionChange;
    intent: ChangeIntent;
}

export interface PutBadgeDefinitionResponse {
    result: Result;
    badge_definition: BadgeDefinition;
}

export interface PutManyBadgeDefinitionsRequest {
    changes: BadgeDefinitionChange[];
    intent: ChangeIntent;
}

export interface PutManyBadgeDefinitionsResponse {
    result: Result;
    definitions: BadgeDefinition[];
}

export interface DeleteBadgeDefinitionRequest {
    removal: BadgeDefinitionRemoval;
    intent: ChangeIntent;
}

export interface DeleteBadgeDefinitionResponse {
    result: Result;
}

export interface DeleteManyBadgeDefinitionsRequest {
    removals: BadgeDefinitionRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyBadgeDefinitionsResponse {
    result: Result;
}

export interface ListBadgeDefinitionVersionsRequest {
    key: BadgeDefinitionKey;
    offset: number;
    limit: number;
    order: Order;
    filter: BadgeDefinitionVersionsFilter | null;
}

export interface ListBadgeDefinitionVersionsResponse {
    result: Result;
    versions: BadgeDefinition[];
    total: number;
}

export interface GetBadgeDefinitionVersionRequest {
    key: BadgeDefinitionVersionKey;
}

export interface GetBadgeDefinitionVersionResponse {
    result: Result;
    version: BadgeDefinition;
}

export const subjects = {
    list_badge_definitions_request: "dq.v1.badge_definitions.list",
    get_badge_definition_request: "dq.v1.badge_definitions.get",
    get_many_badge_definitions_request: "dq.v1.badge_definitions.get_many",
    put_badge_definition_request: "dq.v1.badge_definitions.put",
    put_many_badge_definitions_request: "dq.v1.badge_definitions.put_many",
    delete_badge_definition_request: "dq.v1.badge_definitions.delete",
    delete_many_badge_definitions_request: "dq.v1.badge_definitions.delete_many",
    list_badge_definition_versions_request: "dq.v1.badge_definitions_versions.list",
    get_badge_definition_version_request: "dq.v1.badge_definitions_versions.get",
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_badge_definitions_request: true,
    get_badge_definition_request: true,
    get_many_badge_definitions_request: true,
    put_badge_definition_request: true,
    put_many_badge_definitions_request: true,
    delete_badge_definition_request: true,
    delete_many_badge_definitions_request: true,
    list_badge_definition_versions_request: true,
    get_badge_definition_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: "dq.v1.badge_definitions_events.created",
    updated: "dq.v1.badge_definitions_events.updated",
    deleted: "dq.v1.badge_definitions_events.deleted",
} as const;
