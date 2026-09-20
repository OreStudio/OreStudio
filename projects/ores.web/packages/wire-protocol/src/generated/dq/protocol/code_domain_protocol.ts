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

export interface GetCodeDomainsRequest {
    offset: number;
    limit: number;
}

export interface GetCodeDomainsResponse {
    domains: CodeDomain[];
    total_available_count: number;
    success: boolean;
    message: string;
}

export interface SaveCodeDomainRequest {
    data: CodeDomain;
}

export interface SaveCodeDomainResponse {
    success: boolean;
    message: string;
}

export interface DeleteCodeDomainRequest {
    codes: string[];
}

export interface DeleteCodeDomainResponse {
    success: boolean;
    message: string;
}

export interface GetCodeDomainHistoryRequest {
    code: string;
}

export interface GetCodeDomainHistoryResponse {
    history: CodeDomain[];
    success: boolean;
    message: string;
}

export const subjects = {
    get_code_domains_request: "dq.v1.code_domains.list",
    save_code_domain_request: "dq.v1.code_domains.save",
    delete_code_domain_request: "dq.v1.code_domains.delete",
    get_code_domain_history_request: "dq.v1.code_domains.history",
} as const;
