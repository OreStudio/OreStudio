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
import type { ConcurrencyPolicy } from '../domain/concurrency_policy.js';

export interface GetConcurrencyPoliciesRequest {
    offset: number;
    limit: number;
}

export interface GetConcurrencyPoliciesResponse {
    policies: ConcurrencyPolicy[];
    total_available_count: number;
    success: boolean;
    message: string;
}

export interface SaveConcurrencyPolicyRequest {
    data: ConcurrencyPolicy;
}

export interface SaveConcurrencyPolicyResponse {
    success: boolean;
    message: string;
}

export interface DeleteConcurrencyPolicyRequest {
    codes: string[];
}

export interface DeleteConcurrencyPolicyResponse {
    success: boolean;
    message: string;
}

export interface GetConcurrencyPolicyHistoryRequest {
    code: string;
}

export interface GetConcurrencyPolicyHistoryResponse {
    history: ConcurrencyPolicy[];
    success: boolean;
    message: string;
}

export const subjects = {
    get_concurrency_policies_request: "reporting.v1.concurrency_policies.list",
    save_concurrency_policy_request: "reporting.v1.concurrency_policies.save",
    delete_concurrency_policy_request: "reporting.v1.concurrency_policies.delete",
    get_concurrency_policy_history_request: "reporting.v1.concurrency_policies.history",
} as const;
