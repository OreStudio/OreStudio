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
import type { TenorAnchor } from '../domain/tenor_anchor.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface TenorAnchorKey {
    code: string;
}

export interface TenorAnchorWrite {
    code: string;
    description: string;
    display_order: number;
}

export interface TenorAnchorChange {
    write: TenorAnchorWrite;
    precondition: Precondition;
}

export interface TenorAnchorRemoval {
    key: TenorAnchorKey;
    precondition: Precondition;
}

export interface TenorAnchorLookup {
    key: TenorAnchorKey;
    tenor_anchor: TenorAnchor | null;
}

export interface TenorAnchorEvent {
    event_id: string;
    key: TenorAnchorKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface TenorAnchorVersionKey {
    tenor_anchor: TenorAnchorKey;
    version: number;
}

export interface TenorAnchorVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListTenorAnchorsRequest {
    offset: number;
    limit: number;
    order: Order;
}

export interface ListTenorAnchorsResponse {
    result: Result;
    anchors: TenorAnchor[];
    total: number;
}

export interface GetTenorAnchorRequest {
    key: TenorAnchorKey;
}

export interface GetTenorAnchorResponse {
    result: Result;
    tenor_anchor: TenorAnchor | null;
}

export interface GetManyTenorAnchorsRequest {
    keys: TenorAnchorKey[];
}

export interface GetManyTenorAnchorsResponse {
    result: Result;
    entries: TenorAnchorLookup[];
}

export interface PutTenorAnchorRequest {
    change: TenorAnchorChange;
    intent: ChangeIntent;
}

export interface PutTenorAnchorResponse {
    result: Result;
    tenor_anchor: TenorAnchor;
}

export interface PutManyTenorAnchorsRequest {
    changes: TenorAnchorChange[];
    intent: ChangeIntent;
}

export interface PutManyTenorAnchorsResponse {
    result: Result;
    anchors: TenorAnchor[];
}

export interface DeleteTenorAnchorRequest {
    removal: TenorAnchorRemoval;
    intent: ChangeIntent;
}

export interface DeleteTenorAnchorResponse {
    result: Result;
}

export interface DeleteManyTenorAnchorsRequest {
    removals: TenorAnchorRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyTenorAnchorsResponse {
    result: Result;
}

export interface ListTenorAnchorVersionsRequest {
    key: TenorAnchorKey;
    offset: number;
    limit: number;
    order: Order;
    filter: TenorAnchorVersionsFilter | null;
}

export interface ListTenorAnchorVersionsResponse {
    result: Result;
    versions: TenorAnchor[];
    total: number;
}

export interface GetTenorAnchorVersionRequest {
    key: TenorAnchorVersionKey;
}

export interface GetTenorAnchorVersionResponse {
    result: Result;
    version: TenorAnchor;
}

export const subjects = {
    list_tenor_anchors_request: "refdata.v1.tenor_anchors.list",
    get_tenor_anchor_request: "refdata.v1.tenor_anchors.get",
    get_many_tenor_anchors_request: "refdata.v1.tenor_anchors.get_many",
    put_tenor_anchor_request: "refdata.v1.tenor_anchors.put",
    put_many_tenor_anchors_request: "refdata.v1.tenor_anchors.put_many",
    delete_tenor_anchor_request: "refdata.v1.tenor_anchors.delete",
    delete_many_tenor_anchors_request: "refdata.v1.tenor_anchors.delete_many",
    list_tenor_anchor_versions_request: "refdata.v1.tenor_anchors_versions.list",
    get_tenor_anchor_version_request: "refdata.v1.tenor_anchors_versions.get",
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_tenor_anchors_request: true,
    get_tenor_anchor_request: true,
    get_many_tenor_anchors_request: true,
    put_tenor_anchor_request: true,
    put_many_tenor_anchors_request: true,
    delete_tenor_anchor_request: true,
    delete_many_tenor_anchors_request: true,
    list_tenor_anchor_versions_request: true,
    get_tenor_anchor_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: "refdata.v1.tenor_anchors_events.created",
    updated: "refdata.v1.tenor_anchors_events.updated",
    deleted: "refdata.v1.tenor_anchors_events.deleted",
} as const;
