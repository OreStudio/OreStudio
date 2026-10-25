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
import type { OreParameterValueSetCatalogue } from '../domain/ore_parameter_value_set_catalogue.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface OreParameterValueSetCatalogueKey {
    code: string;
}

export interface OreParameterValueSetCatalogueWrite {
    code: string;
    name: string;
    relation: string;
    description: string;
    is_active: boolean;
}

export interface OreParameterValueSetCatalogueChange {
    write: OreParameterValueSetCatalogueWrite;
    precondition: Precondition;
}

export interface OreParameterValueSetCatalogueRemoval {
    key: OreParameterValueSetCatalogueKey;
    precondition: Precondition;
}

export interface OreParameterValueSetCatalogueLookup {
    key: OreParameterValueSetCatalogueKey;
    ore_parameter_value_set_catalogue: OreParameterValueSetCatalogue | null;
}

export interface OreParameterValueSetCataloguesFilter {
    code_one_of: string[] | null;
}

export interface OreParameterValueSetCatalogueEvent {
    event_id: string;
    key: OreParameterValueSetCatalogueKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface OreParameterValueSetCatalogueVersionKey {
    ore_parameter_value_set_catalogue: OreParameterValueSetCatalogueKey;
    version: number;
}

export interface OreParameterValueSetCatalogueVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListOreParameterValueSetCataloguesRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: OreParameterValueSetCataloguesFilter | null;
    as_of: string | null;
}

export interface ListOreParameterValueSetCataloguesResponse {
    result: Result;
    catalogues: OreParameterValueSetCatalogue[];
    total: number;
}

export interface GetOreParameterValueSetCatalogueRequest {
    key: OreParameterValueSetCatalogueKey;
}

export interface GetOreParameterValueSetCatalogueResponse {
    result: Result;
    ore_parameter_value_set_catalogue: OreParameterValueSetCatalogue | null;
}

export interface GetManyOreParameterValueSetCataloguesRequest {
    keys: OreParameterValueSetCatalogueKey[];
}

export interface GetManyOreParameterValueSetCataloguesResponse {
    result: Result;
    entries: OreParameterValueSetCatalogueLookup[];
}

export interface PutOreParameterValueSetCatalogueRequest {
    change: OreParameterValueSetCatalogueChange;
    intent: ChangeIntent;
}

export interface PutOreParameterValueSetCatalogueResponse {
    result: Result;
    ore_parameter_value_set_catalogue: OreParameterValueSetCatalogue | null;
}

export interface PutManyOreParameterValueSetCataloguesRequest {
    changes: OreParameterValueSetCatalogueChange[];
    intent: ChangeIntent;
}

export interface PutManyOreParameterValueSetCataloguesResponse {
    result: Result;
    catalogues: OreParameterValueSetCatalogue[];
}

export interface DeleteOreParameterValueSetCatalogueRequest {
    removal: OreParameterValueSetCatalogueRemoval;
    intent: ChangeIntent;
}

export interface DeleteOreParameterValueSetCatalogueResponse {
    result: Result;
}

export interface DeleteManyOreParameterValueSetCataloguesRequest {
    removals: OreParameterValueSetCatalogueRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyOreParameterValueSetCataloguesResponse {
    result: Result;
}

export interface ListOreParameterValueSetCatalogueVersionsRequest {
    key: OreParameterValueSetCatalogueKey;
    offset: number;
    limit: number;
    order: Order;
    filter: OreParameterValueSetCatalogueVersionsFilter | null;
}

export interface ListOreParameterValueSetCatalogueVersionsResponse {
    result: Result;
    versions: OreParameterValueSetCatalogue[];
    total: number;
}

export interface GetOreParameterValueSetCatalogueVersionRequest {
    key: OreParameterValueSetCatalogueVersionKey;
}

export interface GetOreParameterValueSetCatalogueVersionResponse {
    result: Result;
    version: OreParameterValueSetCatalogue | null;
}

export const subjects = {
    list_ore_parameter_value_set_catalogues_request:
        'reporting.v1.ore_parameter_value_set_catalogues.list',
    get_ore_parameter_value_set_catalogue_request:
        'reporting.v1.ore_parameter_value_set_catalogues.get',
    get_many_ore_parameter_value_set_catalogues_request:
        'reporting.v1.ore_parameter_value_set_catalogues.get_many',
    put_ore_parameter_value_set_catalogue_request:
        'reporting.v1.ore_parameter_value_set_catalogues.put',
    put_many_ore_parameter_value_set_catalogues_request:
        'reporting.v1.ore_parameter_value_set_catalogues.put_many',
    delete_ore_parameter_value_set_catalogue_request:
        'reporting.v1.ore_parameter_value_set_catalogues.delete',
    delete_many_ore_parameter_value_set_catalogues_request:
        'reporting.v1.ore_parameter_value_set_catalogues.delete_many',
    list_ore_parameter_value_set_catalogue_versions_request:
        'reporting.v1.ore_parameter_value_set_catalogues_versions.list',
    get_ore_parameter_value_set_catalogue_version_request:
        'reporting.v1.ore_parameter_value_set_catalogues_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_ore_parameter_value_set_catalogues_request: true,
    get_ore_parameter_value_set_catalogue_request: true,
    get_many_ore_parameter_value_set_catalogues_request: true,
    put_ore_parameter_value_set_catalogue_request: true,
    put_many_ore_parameter_value_set_catalogues_request: true,
    delete_ore_parameter_value_set_catalogue_request: true,
    delete_many_ore_parameter_value_set_catalogues_request: true,
    list_ore_parameter_value_set_catalogue_versions_request: true,
    get_ore_parameter_value_set_catalogue_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'reporting.v1.ore_parameter_value_set_catalogues_events.created',
    updated: 'reporting.v1.ore_parameter_value_set_catalogues_events.updated',
    deleted: 'reporting.v1.ore_parameter_value_set_catalogues_events.deleted',
} as const;
