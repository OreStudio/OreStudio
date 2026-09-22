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
import type { CodeDomain } from '../domain/code_domain.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface CodeDomainKey {
    code: string;
}

export interface CodeDomainWrite {
    code: string;
    name: string;
    description: string;
    display_order: number;
}

export interface CodeDomainChange {
    write: CodeDomainWrite;
    precondition: Precondition;
}

export interface CodeDomainRemoval {
    key: CodeDomainKey;
    precondition: Precondition;
}

export interface CodeDomainLookup {
    key: CodeDomainKey;
    code_domain: CodeDomain | null;
}

export interface CodeDomainEvent {
    event_id: string;
    key: CodeDomainKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface CodeDomainVersionKey {
    code_domain: CodeDomainKey;
    version: number;
}

export interface CodeDomainVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListCodeDomainsRequest {
    offset: number;
    limit: number;
    order: Order;
}

export interface ListCodeDomainsResponse {
    result: Result;
    domains: CodeDomain[];
    total: number;
}

export interface GetCodeDomainRequest {
    key: CodeDomainKey;
}

export interface GetCodeDomainResponse {
    result: Result;
    code_domain: CodeDomain | null;
}

export interface GetManyCodeDomainsRequest {
    keys: CodeDomainKey[];
}

export interface GetManyCodeDomainsResponse {
    result: Result;
    entries: CodeDomainLookup[];
}

export interface PutCodeDomainRequest {
    change: CodeDomainChange;
    intent: ChangeIntent;
}

export interface PutCodeDomainResponse {
    result: Result;
    code_domain: CodeDomain;
}

export interface PutManyCodeDomainsRequest {
    changes: CodeDomainChange[];
    intent: ChangeIntent;
}

export interface PutManyCodeDomainsResponse {
    result: Result;
    domains: CodeDomain[];
}

export interface DeleteCodeDomainRequest {
    removal: CodeDomainRemoval;
    intent: ChangeIntent;
}

export interface DeleteCodeDomainResponse {
    result: Result;
}

export interface DeleteManyCodeDomainsRequest {
    removals: CodeDomainRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyCodeDomainsResponse {
    result: Result;
}

export interface ListCodeDomainVersionsRequest {
    key: CodeDomainKey;
    offset: number;
    limit: number;
    order: Order;
    filter: CodeDomainVersionsFilter | null;
}

export interface ListCodeDomainVersionsResponse {
    result: Result;
    versions: CodeDomain[];
    total: number;
}

export interface GetCodeDomainVersionRequest {
    key: CodeDomainVersionKey;
}

export interface GetCodeDomainVersionResponse {
    result: Result;
    version: CodeDomain;
}

export const subjects = {
    list_code_domains_request: "dq.v1.code_domains.list",
    get_code_domain_request: "dq.v1.code_domains.get",
    get_many_code_domains_request: "dq.v1.code_domains.get_many",
    put_code_domain_request: "dq.v1.code_domains.put",
    put_many_code_domains_request: "dq.v1.code_domains.put_many",
    delete_code_domain_request: "dq.v1.code_domains.delete",
    delete_many_code_domains_request: "dq.v1.code_domains.delete_many",
    list_code_domain_versions_request: "dq.v1.code_domains_versions.list",
    get_code_domain_version_request: "dq.v1.code_domains_versions.get",
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_code_domains_request: true,
    get_code_domain_request: true,
    get_many_code_domains_request: true,
    put_code_domain_request: true,
    put_many_code_domains_request: true,
    delete_code_domain_request: true,
    delete_many_code_domains_request: true,
    list_code_domain_versions_request: true,
    get_code_domain_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: "dq.v1.code_domains_events.created",
    updated: "dq.v1.code_domains_events.updated",
    deleted: "dq.v1.code_domains_events.deleted",
} as const;
