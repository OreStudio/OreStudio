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
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface BadgeSeverityKey {
    code: string;
}

export interface BadgeSeverityWrite {
    code: string;
    name: string;
    description: string;
    display_order: number;
}

export interface BadgeSeverityChange {
    write: BadgeSeverityWrite;
    precondition: Precondition;
}

export interface BadgeSeverityRemoval {
    key: BadgeSeverityKey;
    precondition: Precondition;
}

export interface BadgeSeverityLookup {
    key: BadgeSeverityKey;
    badge_severity: BadgeSeverity | null;
}

export interface BadgeSeverityEvent {
    event_id: string;
    key: BadgeSeverityKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface BadgeSeverityVersionKey {
    badge_severity: BadgeSeverityKey;
    version: number;
}

export interface BadgeSeverityVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListBadgeSeveritiesRequest {
    offset: number;
    limit: number;
    order: Order;
}

export interface ListBadgeSeveritiesResponse {
    result: Result;
    severities: BadgeSeverity[];
    total: number;
}

export interface GetBadgeSeverityRequest {
    key: BadgeSeverityKey;
}

export interface GetBadgeSeverityResponse {
    result: Result;
    badge_severity: BadgeSeverity | null;
}

export interface GetManyBadgeSeveritiesRequest {
    keys: BadgeSeverityKey[];
}

export interface GetManyBadgeSeveritiesResponse {
    result: Result;
    entries: BadgeSeverityLookup[];
}

export interface PutBadgeSeverityRequest {
    change: BadgeSeverityChange;
    intent: ChangeIntent;
}

export interface PutBadgeSeverityResponse {
    result: Result;
    badge_severity: BadgeSeverity;
}

export interface PutManyBadgeSeveritiesRequest {
    changes: BadgeSeverityChange[];
    intent: ChangeIntent;
}

export interface PutManyBadgeSeveritiesResponse {
    result: Result;
    severities: BadgeSeverity[];
}

export interface DeleteBadgeSeverityRequest {
    removal: BadgeSeverityRemoval;
    intent: ChangeIntent;
}

export interface DeleteBadgeSeverityResponse {
    result: Result;
}

export interface DeleteManyBadgeSeveritiesRequest {
    removals: BadgeSeverityRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyBadgeSeveritiesResponse {
    result: Result;
}

export interface ListBadgeSeverityVersionsRequest {
    key: BadgeSeverityKey;
    offset: number;
    limit: number;
    order: Order;
    filter: BadgeSeverityVersionsFilter | null;
}

export interface ListBadgeSeverityVersionsResponse {
    result: Result;
    versions: BadgeSeverity[];
    total: number;
}

export interface GetBadgeSeverityVersionRequest {
    key: BadgeSeverityVersionKey;
}

export interface GetBadgeSeverityVersionResponse {
    result: Result;
    version: BadgeSeverity;
}

export const subjects = {
    list_badge_severities_request: "dq.v1.badge_severities.list",
    get_badge_severity_request: "dq.v1.badge_severities.get",
    get_many_badge_severities_request: "dq.v1.badge_severities.get_many",
    put_badge_severity_request: "dq.v1.badge_severities.put",
    put_many_badge_severities_request: "dq.v1.badge_severities.put_many",
    delete_badge_severity_request: "dq.v1.badge_severities.delete",
    delete_many_badge_severities_request: "dq.v1.badge_severities.delete_many",
    list_badge_severity_versions_request: "dq.v1.badge_severities_versions.list",
    get_badge_severity_version_request: "dq.v1.badge_severities_versions.get",
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_badge_severities_request: true,
    get_badge_severity_request: true,
    get_many_badge_severities_request: true,
    put_badge_severity_request: true,
    put_many_badge_severities_request: true,
    delete_badge_severity_request: true,
    delete_many_badge_severities_request: true,
    list_badge_severity_versions_request: true,
    get_badge_severity_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: "dq.v1.badge_severities_events.created",
    updated: "dq.v1.badge_severities_events.updated",
    deleted: "dq.v1.badge_severities_events.deleted",
} as const;
