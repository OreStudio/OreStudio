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

export interface GetHostsRequest {
    offset: number;
    limit: number;
}

export interface GetHostsResponse {
    hosts: Host[];
    total_available_count: number;
    success: boolean;
    message: string;
}

export interface SaveHostRequest {
    data: Host;
}

export interface SaveHostResponse {
    success: boolean;
    message: string;
}

export interface DeleteHostRequest {
    ids: string[];
}

export interface DeleteHostResponse {
    success: boolean;
    message: string;
}

export interface GetHostHistoryRequest {
    id: string;
}

export interface GetHostHistoryResponse {
    history: Host[];
    success: boolean;
    message: string;
}

export const subjects = {
    get_hosts_request: "compute.v1.hosts.list",
    save_host_request: "compute.v1.hosts.save",
    delete_host_request: "compute.v1.hosts.delete",
    get_host_history_request: "compute.v1.hosts.history",
} as const;
