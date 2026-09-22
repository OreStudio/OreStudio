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
import type { DataDomain } from '../domain/data_domain.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface DataDomainKey {
    name: string;
}

export interface DataDomainWrite {
    name: string;
    description: string;
}

export interface DataDomainChange {
    write: DataDomainWrite;
    precondition: Precondition;
}

export interface DataDomainRemoval {
    key: DataDomainKey;
    precondition: Precondition;
}

export interface DataDomainLookup {
    key: DataDomainKey;
    data_domain: DataDomain | null;
}

export interface DataDomainEvent {
    event_id: string;
    key: DataDomainKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface DataDomainVersionKey {
    data_domain: DataDomainKey;
    version: number;
}

export interface DataDomainVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListDataDomainsRequest {
    offset: number;
    limit: number;
    order: Order;
}

export interface ListDataDomainsResponse {
    result: Result;
    domains: DataDomain[];
    total: number;
}

export interface GetDataDomainRequest {
    key: DataDomainKey;
}

export interface GetDataDomainResponse {
    result: Result;
    data_domain: DataDomain | null;
}

export interface GetManyDataDomainsRequest {
    keys: DataDomainKey[];
}

export interface GetManyDataDomainsResponse {
    result: Result;
    entries: DataDomainLookup[];
}

export interface PutDataDomainRequest {
    change: DataDomainChange;
    intent: ChangeIntent;
}

export interface PutDataDomainResponse {
    result: Result;
    data_domain: DataDomain;
}

export interface PutManyDataDomainsRequest {
    changes: DataDomainChange[];
    intent: ChangeIntent;
}

export interface PutManyDataDomainsResponse {
    result: Result;
    domains: DataDomain[];
}

export interface DeleteDataDomainRequest {
    removal: DataDomainRemoval;
    intent: ChangeIntent;
}

export interface DeleteDataDomainResponse {
    result: Result;
}

export interface DeleteManyDataDomainsRequest {
    removals: DataDomainRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyDataDomainsResponse {
    result: Result;
}

export interface ListDataDomainVersionsRequest {
    key: DataDomainKey;
    offset: number;
    limit: number;
    order: Order;
    filter: DataDomainVersionsFilter | null;
}

export interface ListDataDomainVersionsResponse {
    result: Result;
    versions: DataDomain[];
    total: number;
}

export interface GetDataDomainVersionRequest {
    key: DataDomainVersionKey;
}

export interface GetDataDomainVersionResponse {
    result: Result;
    version: DataDomain;
}

export const subjects = {
    list_data_domains_request: "dq.v1.data_domains.list",
    get_data_domain_request: "dq.v1.data_domains.get",
    get_many_data_domains_request: "dq.v1.data_domains.get_many",
    put_data_domain_request: "dq.v1.data_domains.put",
    put_many_data_domains_request: "dq.v1.data_domains.put_many",
    delete_data_domain_request: "dq.v1.data_domains.delete",
    delete_many_data_domains_request: "dq.v1.data_domains.delete_many",
    list_data_domain_versions_request: "dq.v1.data_domains_versions.list",
    get_data_domain_version_request: "dq.v1.data_domains_versions.get",
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_data_domains_request: true,
    get_data_domain_request: true,
    get_many_data_domains_request: true,
    put_data_domain_request: true,
    put_many_data_domains_request: true,
    delete_data_domain_request: true,
    delete_many_data_domains_request: true,
    list_data_domain_versions_request: true,
    get_data_domain_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: "dq.v1.data_domains_events.created",
    updated: "dq.v1.data_domains_events.updated",
    deleted: "dq.v1.data_domains_events.deleted",
} as const;
