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
import type { OreParameterValueDomain } from '../domain/ore_parameter_value_domain.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface OreParameterValueDomainKey {
    code: string;
}

export interface OreParameterValueDomainWrite {
    code: string;
    name: string;
    storage_kind: string;
    referenced_entity: string;
}

export interface OreParameterValueDomainChange {
    write: OreParameterValueDomainWrite;
    precondition: Precondition;
}

export interface OreParameterValueDomainRemoval {
    key: OreParameterValueDomainKey;
    precondition: Precondition;
}

export interface OreParameterValueDomainLookup {
    key: OreParameterValueDomainKey;
    ore_parameter_value_domain: OreParameterValueDomain | null;
}

export interface OreParameterValueDomainsFilter {
    code_one_of: string[] | null;
}

export interface OreParameterValueDomainEvent {
    event_id: string;
    key: OreParameterValueDomainKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface OreParameterValueDomainVersionKey {
    ore_parameter_value_domain: OreParameterValueDomainKey;
    version: number;
}

export interface OreParameterValueDomainVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListOreParameterValueDomainsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: OreParameterValueDomainsFilter | null;
    as_of: string | null;
}

export interface ListOreParameterValueDomainsResponse {
    result: Result;
    domains: OreParameterValueDomain[];
    total: number;
}

export interface GetOreParameterValueDomainRequest {
    key: OreParameterValueDomainKey;
}

export interface GetOreParameterValueDomainResponse {
    result: Result;
    ore_parameter_value_domain: OreParameterValueDomain | null;
}

export interface GetManyOreParameterValueDomainsRequest {
    keys: OreParameterValueDomainKey[];
}

export interface GetManyOreParameterValueDomainsResponse {
    result: Result;
    entries: OreParameterValueDomainLookup[];
}

export interface PutOreParameterValueDomainRequest {
    change: OreParameterValueDomainChange;
    intent: ChangeIntent;
}

export interface PutOreParameterValueDomainResponse {
    result: Result;
    ore_parameter_value_domain: OreParameterValueDomain | null;
}

export interface PutManyOreParameterValueDomainsRequest {
    changes: OreParameterValueDomainChange[];
    intent: ChangeIntent;
}

export interface PutManyOreParameterValueDomainsResponse {
    result: Result;
    domains: OreParameterValueDomain[];
}

export interface DeleteOreParameterValueDomainRequest {
    removal: OreParameterValueDomainRemoval;
    intent: ChangeIntent;
}

export interface DeleteOreParameterValueDomainResponse {
    result: Result;
}

export interface DeleteManyOreParameterValueDomainsRequest {
    removals: OreParameterValueDomainRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyOreParameterValueDomainsResponse {
    result: Result;
}

export interface ListOreParameterValueDomainVersionsRequest {
    key: OreParameterValueDomainKey;
    offset: number;
    limit: number;
    order: Order;
    filter: OreParameterValueDomainVersionsFilter | null;
}

export interface ListOreParameterValueDomainVersionsResponse {
    result: Result;
    versions: OreParameterValueDomain[];
    total: number;
}

export interface GetOreParameterValueDomainVersionRequest {
    key: OreParameterValueDomainVersionKey;
}

export interface GetOreParameterValueDomainVersionResponse {
    result: Result;
    version: OreParameterValueDomain | null;
}

export const subjects = {
    list_ore_parameter_value_domains_request: 'reporting.v1.ore_parameter_value_domains.list',
    get_ore_parameter_value_domain_request: 'reporting.v1.ore_parameter_value_domains.get',
    get_many_ore_parameter_value_domains_request:
        'reporting.v1.ore_parameter_value_domains.get_many',
    put_ore_parameter_value_domain_request: 'reporting.v1.ore_parameter_value_domains.put',
    put_many_ore_parameter_value_domains_request:
        'reporting.v1.ore_parameter_value_domains.put_many',
    delete_ore_parameter_value_domain_request: 'reporting.v1.ore_parameter_value_domains.delete',
    delete_many_ore_parameter_value_domains_request:
        'reporting.v1.ore_parameter_value_domains.delete_many',
    list_ore_parameter_value_domain_versions_request:
        'reporting.v1.ore_parameter_value_domains_versions.list',
    get_ore_parameter_value_domain_version_request:
        'reporting.v1.ore_parameter_value_domains_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_ore_parameter_value_domains_request: true,
    get_ore_parameter_value_domain_request: true,
    get_many_ore_parameter_value_domains_request: true,
    put_ore_parameter_value_domain_request: true,
    put_many_ore_parameter_value_domains_request: true,
    delete_ore_parameter_value_domain_request: true,
    delete_many_ore_parameter_value_domains_request: true,
    list_ore_parameter_value_domain_versions_request: true,
    get_ore_parameter_value_domain_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'reporting.v1.ore_parameter_value_domains_events.created',
    updated: 'reporting.v1.ore_parameter_value_domains_events.updated',
    deleted: 'reporting.v1.ore_parameter_value_domains_events.deleted',
} as const;
