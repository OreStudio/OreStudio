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
import type { Host } from '../domain/host.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface HostKey {
    external_id: string;
}

export interface HostWrite {
    id: string;
    external_id: string;
    location: string;
    cpu_count: number;
    ram_mb: number;
    gpu_type: string;
    display_name: string;
    last_rpc_time: string;
    credit_total: number;
}

export interface HostChange {
    write: HostWrite;
    precondition: Precondition;
}

export interface HostRemoval {
    key: HostKey;
    precondition: Precondition;
}

export interface HostLookup {
    key: HostKey;
    host: Host | null;
}

export interface HostEvent {
    event_id: string;
    key: HostKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface HostVersionKey {
    host: HostKey;
    version: number;
}

export interface HostVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListHostsRequest {
    offset: number;
    limit: number;
    order: Order;
}

export interface ListHostsResponse {
    result: Result;
    hosts: Host[];
    total: number;
}

export interface GetHostRequest {
    key: HostKey;
}

export interface GetHostResponse {
    result: Result;
    host: Host | null;
}

export interface GetManyHostsRequest {
    keys: HostKey[];
}

export interface GetManyHostsResponse {
    result: Result;
    entries: HostLookup[];
}

export interface PutHostRequest {
    change: HostChange;
    intent: ChangeIntent;
}

export interface PutHostResponse {
    result: Result;
    host: Host;
}

export interface PutManyHostsRequest {
    changes: HostChange[];
    intent: ChangeIntent;
}

export interface PutManyHostsResponse {
    result: Result;
    hosts: Host[];
}

export interface DeleteHostRequest {
    removal: HostRemoval;
    intent: ChangeIntent;
}

export interface DeleteHostResponse {
    result: Result;
}

export interface DeleteManyHostsRequest {
    removals: HostRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyHostsResponse {
    result: Result;
}

export interface ListHostVersionsRequest {
    key: HostKey;
    offset: number;
    limit: number;
    order: Order;
    filter: HostVersionsFilter | null;
}

export interface ListHostVersionsResponse {
    result: Result;
    versions: Host[];
    total: number;
}

export interface GetHostVersionRequest {
    key: HostVersionKey;
}

export interface GetHostVersionResponse {
    result: Result;
    version: Host;
}

export const subjects = {
    list_hosts_request: "compute.v1.hosts.list",
    get_host_request: "compute.v1.hosts.get",
    get_many_hosts_request: "compute.v1.hosts.get_many",
    put_host_request: "compute.v1.hosts.put",
    put_many_hosts_request: "compute.v1.hosts.put_many",
    delete_host_request: "compute.v1.hosts.delete",
    delete_many_hosts_request: "compute.v1.hosts.delete_many",
    list_host_versions_request: "compute.v1.hosts_versions.list",
    get_host_version_request: "compute.v1.hosts_versions.get",
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_hosts_request: true,
    get_host_request: true,
    get_many_hosts_request: true,
    put_host_request: true,
    put_many_hosts_request: true,
    delete_host_request: true,
    delete_many_hosts_request: true,
    list_host_versions_request: true,
    get_host_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: "compute.v1.hosts_events.created",
    updated: "compute.v1.hosts_events.updated",
    deleted: "compute.v1.hosts_events.deleted",
} as const;
