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

export interface GetCrmTopologyConfigsRequest {
    offset: number;
    limit: number;
}

export interface GetCrmTopologyConfigsResponse {
    crm_topology_configs: CrmTopologyConfig[];
    total_available_count: number;
    success: boolean;
    message: string;
}

export interface SaveCrmTopologyConfigRequest {
    data: CrmTopologyConfig;
}

export interface SaveCrmTopologyConfigResponse {
    success: boolean;
    message: string;
}

export interface DeleteCrmTopologyConfigRequest {
    ids: string[];
}

export interface DeleteCrmTopologyConfigResponse {
    success: boolean;
    message: string;
}

export interface GetCrmTopologyConfigHistoryRequest {
    id: string;
}

export interface GetCrmTopologyConfigHistoryResponse {
    history: CrmTopologyConfig[];
    success: boolean;
    message: string;
}

export const subjects = {
    get_crm_topology_configs_request: "refdata.v1.crm_topology_configs.list",
    save_crm_topology_config_request: "refdata.v1.crm_topology_configs.save",
    delete_crm_topology_config_request: "refdata.v1.crm_topology_configs.delete",
    get_crm_topology_config_history_request: "refdata.v1.crm_topology_configs.history",
} as const;
