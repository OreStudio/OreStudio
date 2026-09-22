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
import type { CrmTopologyConfig } from '../domain/crm_topology_config.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface CrmTopologyConfigKey {
    name: string;
}

export interface CrmTopologyConfigWrite {
    id: string;
    name: string;
    pivot_currency_code: string;
    enabled: boolean;
}

export interface CrmTopologyConfigChange {
    write: CrmTopologyConfigWrite;
    precondition: Precondition;
}

export interface CrmTopologyConfigRemoval {
    key: CrmTopologyConfigKey;
    precondition: Precondition;
}

export interface CrmTopologyConfigLookup {
    key: CrmTopologyConfigKey;
    crm_topology_config: CrmTopologyConfig | null;
}

export interface CrmTopologyConfigEvent {
    event_id: string;
    key: CrmTopologyConfigKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface CrmTopologyConfigVersionKey {
    crm_topology_config: CrmTopologyConfigKey;
    version: number;
}

export interface CrmTopologyConfigVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListCrmTopologyConfigsRequest {
    offset: number;
    limit: number;
    order: Order;
}

export interface ListCrmTopologyConfigsResponse {
    result: Result;
    crm_topology_configs: CrmTopologyConfig[];
    total: number;
}

export interface GetCrmTopologyConfigRequest {
    key: CrmTopologyConfigKey;
}

export interface GetCrmTopologyConfigResponse {
    result: Result;
    crm_topology_config: CrmTopologyConfig | null;
}

export interface GetManyCrmTopologyConfigsRequest {
    keys: CrmTopologyConfigKey[];
}

export interface GetManyCrmTopologyConfigsResponse {
    result: Result;
    entries: CrmTopologyConfigLookup[];
}

export interface PutCrmTopologyConfigRequest {
    change: CrmTopologyConfigChange;
    intent: ChangeIntent;
}

export interface PutCrmTopologyConfigResponse {
    result: Result;
    crm_topology_config: CrmTopologyConfig;
}

export interface PutManyCrmTopologyConfigsRequest {
    changes: CrmTopologyConfigChange[];
    intent: ChangeIntent;
}

export interface PutManyCrmTopologyConfigsResponse {
    result: Result;
    crm_topology_configs: CrmTopologyConfig[];
}

export interface DeleteCrmTopologyConfigRequest {
    removal: CrmTopologyConfigRemoval;
    intent: ChangeIntent;
}

export interface DeleteCrmTopologyConfigResponse {
    result: Result;
}

export interface DeleteManyCrmTopologyConfigsRequest {
    removals: CrmTopologyConfigRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyCrmTopologyConfigsResponse {
    result: Result;
}

export interface ListCrmTopologyConfigVersionsRequest {
    key: CrmTopologyConfigKey;
    offset: number;
    limit: number;
    order: Order;
    filter: CrmTopologyConfigVersionsFilter | null;
}

export interface ListCrmTopologyConfigVersionsResponse {
    result: Result;
    versions: CrmTopologyConfig[];
    total: number;
}

export interface GetCrmTopologyConfigVersionRequest {
    key: CrmTopologyConfigVersionKey;
}

export interface GetCrmTopologyConfigVersionResponse {
    result: Result;
    version: CrmTopologyConfig;
}

export const subjects = {
    list_crm_topology_configs_request: "refdata.v1.crm_topology_configs.list",
    get_crm_topology_config_request: "refdata.v1.crm_topology_configs.get",
    get_many_crm_topology_configs_request: "refdata.v1.crm_topology_configs.get_many",
    put_crm_topology_config_request: "refdata.v1.crm_topology_configs.put",
    put_many_crm_topology_configs_request: "refdata.v1.crm_topology_configs.put_many",
    delete_crm_topology_config_request: "refdata.v1.crm_topology_configs.delete",
    delete_many_crm_topology_configs_request: "refdata.v1.crm_topology_configs.delete_many",
    list_crm_topology_config_versions_request: "refdata.v1.crm_topology_configs_versions.list",
    get_crm_topology_config_version_request: "refdata.v1.crm_topology_configs_versions.get",
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_crm_topology_configs_request: true,
    get_crm_topology_config_request: true,
    get_many_crm_topology_configs_request: true,
    put_crm_topology_config_request: true,
    put_many_crm_topology_configs_request: true,
    delete_crm_topology_config_request: true,
    delete_many_crm_topology_configs_request: true,
    list_crm_topology_config_versions_request: true,
    get_crm_topology_config_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: "refdata.v1.crm_topology_configs_events.created",
    updated: "refdata.v1.crm_topology_configs_events.updated",
    deleted: "refdata.v1.crm_topology_configs_events.deleted",
} as const;
