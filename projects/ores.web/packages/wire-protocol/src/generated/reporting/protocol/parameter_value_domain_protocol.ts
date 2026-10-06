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
import type { ParameterValueDomain } from '../domain/parameter_value_domain.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface ParameterValueDomainKey {
    code: string;
}

export interface ParameterValueDomainWrite {
    code: string;
    name: string;
    storage_kind: string;
    referenced_entity: string;
}

export interface ParameterValueDomainChange {
    write: ParameterValueDomainWrite;
    precondition: Precondition;
}

export interface ParameterValueDomainRemoval {
    key: ParameterValueDomainKey;
    precondition: Precondition;
}

export interface ParameterValueDomainLookup {
    key: ParameterValueDomainKey;
    parameter_value_domain: ParameterValueDomain | null;
}

export interface ParameterValueDomainsFilter {
    code_one_of: string[] | null;
}

export interface ParameterValueDomainEvent {
    event_id: string;
    key: ParameterValueDomainKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface ParameterValueDomainVersionKey {
    parameter_value_domain: ParameterValueDomainKey;
    version: number;
}

export interface ParameterValueDomainVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListParameterValueDomainsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: ParameterValueDomainsFilter | null;
    as_of: string | null;
}

export interface ListParameterValueDomainsResponse {
    result: Result;
    domains: ParameterValueDomain[];
    total: number;
}

export interface GetParameterValueDomainRequest {
    key: ParameterValueDomainKey;
}

export interface GetParameterValueDomainResponse {
    result: Result;
    parameter_value_domain: ParameterValueDomain | null;
}

export interface GetManyParameterValueDomainsRequest {
    keys: ParameterValueDomainKey[];
}

export interface GetManyParameterValueDomainsResponse {
    result: Result;
    entries: ParameterValueDomainLookup[];
}

export interface PutParameterValueDomainRequest {
    change: ParameterValueDomainChange;
    intent: ChangeIntent;
}

export interface PutParameterValueDomainResponse {
    result: Result;
    parameter_value_domain: ParameterValueDomain | null;
}

export interface PutManyParameterValueDomainsRequest {
    changes: ParameterValueDomainChange[];
    intent: ChangeIntent;
}

export interface PutManyParameterValueDomainsResponse {
    result: Result;
    domains: ParameterValueDomain[];
}

export interface DeleteParameterValueDomainRequest {
    removal: ParameterValueDomainRemoval;
    intent: ChangeIntent;
}

export interface DeleteParameterValueDomainResponse {
    result: Result;
}

export interface DeleteManyParameterValueDomainsRequest {
    removals: ParameterValueDomainRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyParameterValueDomainsResponse {
    result: Result;
}

export interface ListParameterValueDomainVersionsRequest {
    key: ParameterValueDomainKey;
    offset: number;
    limit: number;
    order: Order;
    filter: ParameterValueDomainVersionsFilter | null;
}

export interface ListParameterValueDomainVersionsResponse {
    result: Result;
    versions: ParameterValueDomain[];
    total: number;
}

export interface GetParameterValueDomainVersionRequest {
    key: ParameterValueDomainVersionKey;
}

export interface GetParameterValueDomainVersionResponse {
    result: Result;
    version: ParameterValueDomain | null;
}

export const subjects = {
    list_parameter_value_domains_request: 'reporting.v1.parameter_value_domains.list',
    get_parameter_value_domain_request: 'reporting.v1.parameter_value_domains.get',
    get_many_parameter_value_domains_request: 'reporting.v1.parameter_value_domains.get_many',
    put_parameter_value_domain_request: 'reporting.v1.parameter_value_domains.put',
    put_many_parameter_value_domains_request: 'reporting.v1.parameter_value_domains.put_many',
    delete_parameter_value_domain_request: 'reporting.v1.parameter_value_domains.delete',
    delete_many_parameter_value_domains_request: 'reporting.v1.parameter_value_domains.delete_many',
    list_parameter_value_domain_versions_request:
        'reporting.v1.parameter_value_domains_versions.list',
    get_parameter_value_domain_version_request: 'reporting.v1.parameter_value_domains_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_parameter_value_domains_request: true,
    get_parameter_value_domain_request: true,
    get_many_parameter_value_domains_request: true,
    put_parameter_value_domain_request: true,
    put_many_parameter_value_domains_request: true,
    delete_parameter_value_domain_request: true,
    delete_many_parameter_value_domains_request: true,
    list_parameter_value_domain_versions_request: true,
    get_parameter_value_domain_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'reporting.v1.parameter_value_domains_events.created',
    updated: 'reporting.v1.parameter_value_domains_events.updated',
    deleted: 'reporting.v1.parameter_value_domains_events.deleted',
} as const;
