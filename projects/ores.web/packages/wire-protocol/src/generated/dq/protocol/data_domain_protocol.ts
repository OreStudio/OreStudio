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

export interface GetDataDomainsRequest {
    offset: number;
    limit: number;
}

export interface GetDataDomainsResponse {
    domains: DataDomain[];
    total_available_count: number;
    success: boolean;
    message: string;
}

export interface SaveDataDomainRequest {
    data: DataDomain;
}

export interface SaveDataDomainResponse {
    success: boolean;
    message: string;
}

export interface DeleteDataDomainRequest {
    names: string[];
}

export interface DeleteDataDomainResponse {
    success: boolean;
    message: string;
}

export interface GetDataDomainHistoryRequest {
    name: string;
}

export interface GetDataDomainHistoryResponse {
    history: DataDomain[];
    success: boolean;
    message: string;
}

export const subjects = {
    get_data_domains_request: "dq.v1.data_domains.list",
    save_data_domain_request: "dq.v1.data_domains.save",
    delete_data_domain_request: "dq.v1.data_domains.delete",
    get_data_domain_history_request: "dq.v1.data_domains.history",
} as const;
