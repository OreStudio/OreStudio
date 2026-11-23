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
import type { FxVolView } from '../domain/fx_vol_view.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface FxVolViewKey {
    id: string;
}

export interface FxVolViewWrite {
    id: string;
    feed_id: string;
    contributor_id: string;
    config_id: string;
    folder_id: string | null;
    source_name: string;
    enabled: boolean;
    auto_start: boolean;
}

export interface FxVolViewChange {
    write: FxVolViewWrite;
    precondition: Precondition;
}

export interface FxVolViewRemoval {
    key: FxVolViewKey;
    precondition: Precondition;
}

export interface FxVolViewLookup {
    key: FxVolViewKey;
    fx_vol_view: FxVolView | null;
}

export interface FxVolViewsFilter {
    feed_id: string | null;
    contributor_id: string | null;
    id_one_of: string[] | null;
    feed_id_one_of: string[] | null;
    contributor_id_one_of: string[] | null;
}

export interface FxVolViewEvent {
    event_id: string;
    key: FxVolViewKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface FxVolViewVersionKey {
    fx_vol_view: FxVolViewKey;
    version: number;
}

export interface FxVolViewVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListFxVolViewsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: FxVolViewsFilter | null;
    as_of: string | null;
}

export interface ListFxVolViewsResponse {
    result: Result;
    fx_vol_views: FxVolView[];
    total: number;
}

export interface GetFxVolViewRequest {
    key: FxVolViewKey;
}

export interface GetFxVolViewResponse {
    result: Result;
    fx_vol_view: FxVolView | null;
}

export interface GetManyFxVolViewsRequest {
    keys: FxVolViewKey[];
}

export interface GetManyFxVolViewsResponse {
    result: Result;
    entries: FxVolViewLookup[];
}

export interface PutFxVolViewRequest {
    change: FxVolViewChange;
    intent: ChangeIntent;
}

export interface PutFxVolViewResponse {
    result: Result;
    fx_vol_view: FxVolView | null;
}

export interface PutManyFxVolViewsRequest {
    changes: FxVolViewChange[];
    intent: ChangeIntent;
}

export interface PutManyFxVolViewsResponse {
    result: Result;
    fx_vol_views: FxVolView[];
}

export interface DeleteFxVolViewRequest {
    removal: FxVolViewRemoval;
    intent: ChangeIntent;
}

export interface DeleteFxVolViewResponse {
    result: Result;
}

export interface DeleteManyFxVolViewsRequest {
    removals: FxVolViewRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyFxVolViewsResponse {
    result: Result;
}

export interface ListFxVolViewVersionsRequest {
    key: FxVolViewKey;
    offset: number;
    limit: number;
    order: Order;
    filter: FxVolViewVersionsFilter | null;
}

export interface ListFxVolViewVersionsResponse {
    result: Result;
    versions: FxVolView[];
    total: number;
}

export interface GetFxVolViewVersionRequest {
    key: FxVolViewVersionKey;
}

export interface GetFxVolViewVersionResponse {
    result: Result;
    version: FxVolView | null;
}

export const subjects = {
    list_fx_vol_views_request: 'synthetic.v1.fx_vol_views.list',
    get_fx_vol_view_request: 'synthetic.v1.fx_vol_views.get',
    get_many_fx_vol_views_request: 'synthetic.v1.fx_vol_views.get_many',
    put_fx_vol_view_request: 'synthetic.v1.fx_vol_views.put',
    put_many_fx_vol_views_request: 'synthetic.v1.fx_vol_views.put_many',
    delete_fx_vol_view_request: 'synthetic.v1.fx_vol_views.delete',
    delete_many_fx_vol_views_request: 'synthetic.v1.fx_vol_views.delete_many',
    list_fx_vol_view_versions_request: 'synthetic.v1.fx_vol_views_versions.list',
    get_fx_vol_view_version_request: 'synthetic.v1.fx_vol_views_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_fx_vol_views_request: true,
    get_fx_vol_view_request: true,
    get_many_fx_vol_views_request: true,
    put_fx_vol_view_request: true,
    put_many_fx_vol_views_request: true,
    delete_fx_vol_view_request: true,
    delete_many_fx_vol_views_request: true,
    list_fx_vol_view_versions_request: true,
    get_fx_vol_view_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'synthetic.v1.events.fx_vol_views.created',
    updated: 'synthetic.v1.events.fx_vol_views.updated',
    deleted: 'synthetic.v1.events.fx_vol_views.deleted',
} as const;
