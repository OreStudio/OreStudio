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
import type { Catalog } from '../domain/catalog.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface CatalogKey {
    name: string;
}

export interface CatalogWrite {
    name: string;
    description: string;
    owner: string | null;
}

export interface CatalogChange {
    write: CatalogWrite;
    precondition: Precondition;
}

export interface CatalogRemoval {
    key: CatalogKey;
    precondition: Precondition;
}

export interface CatalogLookup {
    key: CatalogKey;
    catalog: Catalog | null;
}

export interface CatalogEvent {
    event_id: string;
    key: CatalogKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface CatalogVersionKey {
    catalog: CatalogKey;
    version: number;
}

export interface CatalogVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListCatalogsRequest {
    offset: number;
    limit: number;
    order: Order;
}

export interface ListCatalogsResponse {
    result: Result;
    catalogs: Catalog[];
    total: number;
}

export interface GetCatalogRequest {
    key: CatalogKey;
}

export interface GetCatalogResponse {
    result: Result;
    catalog: Catalog | null;
}

export interface GetManyCatalogsRequest {
    keys: CatalogKey[];
}

export interface GetManyCatalogsResponse {
    result: Result;
    entries: CatalogLookup[];
}

export interface PutCatalogRequest {
    change: CatalogChange;
    intent: ChangeIntent;
}

export interface PutCatalogResponse {
    result: Result;
    catalog: Catalog;
}

export interface PutManyCatalogsRequest {
    changes: CatalogChange[];
    intent: ChangeIntent;
}

export interface PutManyCatalogsResponse {
    result: Result;
    catalogs: Catalog[];
}

export interface DeleteCatalogRequest {
    removal: CatalogRemoval;
    intent: ChangeIntent;
}

export interface DeleteCatalogResponse {
    result: Result;
}

export interface DeleteManyCatalogsRequest {
    removals: CatalogRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyCatalogsResponse {
    result: Result;
}

export interface ListCatalogVersionsRequest {
    key: CatalogKey;
    offset: number;
    limit: number;
    order: Order;
    filter: CatalogVersionsFilter | null;
}

export interface ListCatalogVersionsResponse {
    result: Result;
    versions: Catalog[];
    total: number;
}

export interface GetCatalogVersionRequest {
    key: CatalogVersionKey;
}

export interface GetCatalogVersionResponse {
    result: Result;
    version: Catalog;
}

export const subjects = {
    list_catalogs_request: "dq.v1.catalogs.list",
    get_catalog_request: "dq.v1.catalogs.get",
    get_many_catalogs_request: "dq.v1.catalogs.get_many",
    put_catalog_request: "dq.v1.catalogs.put",
    put_many_catalogs_request: "dq.v1.catalogs.put_many",
    delete_catalog_request: "dq.v1.catalogs.delete",
    delete_many_catalogs_request: "dq.v1.catalogs.delete_many",
    list_catalog_versions_request: "dq.v1.catalogs_versions.list",
    get_catalog_version_request: "dq.v1.catalogs_versions.get",
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_catalogs_request: true,
    get_catalog_request: true,
    get_many_catalogs_request: true,
    put_catalog_request: true,
    put_many_catalogs_request: true,
    delete_catalog_request: true,
    delete_many_catalogs_request: true,
    list_catalog_versions_request: true,
    get_catalog_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: "dq.v1.catalogs_events.created",
    updated: "dq.v1.catalogs_events.updated",
    deleted: "dq.v1.catalogs_events.deleted",
} as const;
