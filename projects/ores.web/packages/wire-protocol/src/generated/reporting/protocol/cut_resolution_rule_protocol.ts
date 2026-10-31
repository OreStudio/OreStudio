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
import type { CutResolutionRule } from '../domain/cut_resolution_rule.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface CutResolutionRuleKey {
    code: string;
}

export interface CutResolutionRuleWrite {
    code: string;
    name: string;
    description: string;
    display_order: number;
}

export interface CutResolutionRuleChange {
    write: CutResolutionRuleWrite;
    precondition: Precondition;
}

export interface CutResolutionRuleRemoval {
    key: CutResolutionRuleKey;
    precondition: Precondition;
}

export interface CutResolutionRuleLookup {
    key: CutResolutionRuleKey;
    cut_resolution_rule: CutResolutionRule | null;
}

export interface CutResolutionRulesFilter {
    code_one_of: string[] | null;
}

export interface CutResolutionRuleEvent {
    event_id: string;
    key: CutResolutionRuleKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface CutResolutionRuleVersionKey {
    cut_resolution_rule: CutResolutionRuleKey;
    version: number;
}

export interface CutResolutionRuleVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListCutResolutionRulesRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: CutResolutionRulesFilter | null;
    as_of: string | null;
}

export interface ListCutResolutionRulesResponse {
    result: Result;
    rules: CutResolutionRule[];
    total: number;
}

export interface GetCutResolutionRuleRequest {
    key: CutResolutionRuleKey;
}

export interface GetCutResolutionRuleResponse {
    result: Result;
    cut_resolution_rule: CutResolutionRule | null;
}

export interface GetManyCutResolutionRulesRequest {
    keys: CutResolutionRuleKey[];
}

export interface GetManyCutResolutionRulesResponse {
    result: Result;
    entries: CutResolutionRuleLookup[];
}

export interface PutCutResolutionRuleRequest {
    change: CutResolutionRuleChange;
    intent: ChangeIntent;
}

export interface PutCutResolutionRuleResponse {
    result: Result;
    cut_resolution_rule: CutResolutionRule | null;
}

export interface PutManyCutResolutionRulesRequest {
    changes: CutResolutionRuleChange[];
    intent: ChangeIntent;
}

export interface PutManyCutResolutionRulesResponse {
    result: Result;
    rules: CutResolutionRule[];
}

export interface DeleteCutResolutionRuleRequest {
    removal: CutResolutionRuleRemoval;
    intent: ChangeIntent;
}

export interface DeleteCutResolutionRuleResponse {
    result: Result;
}

export interface DeleteManyCutResolutionRulesRequest {
    removals: CutResolutionRuleRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyCutResolutionRulesResponse {
    result: Result;
}

export interface ListCutResolutionRuleVersionsRequest {
    key: CutResolutionRuleKey;
    offset: number;
    limit: number;
    order: Order;
    filter: CutResolutionRuleVersionsFilter | null;
}

export interface ListCutResolutionRuleVersionsResponse {
    result: Result;
    versions: CutResolutionRule[];
    total: number;
}

export interface GetCutResolutionRuleVersionRequest {
    key: CutResolutionRuleVersionKey;
}

export interface GetCutResolutionRuleVersionResponse {
    result: Result;
    version: CutResolutionRule | null;
}

export const subjects = {
    list_cut_resolution_rules_request: 'reporting.v1.cut_resolution_rules.list',
    get_cut_resolution_rule_request: 'reporting.v1.cut_resolution_rules.get',
    get_many_cut_resolution_rules_request: 'reporting.v1.cut_resolution_rules.get_many',
    put_cut_resolution_rule_request: 'reporting.v1.cut_resolution_rules.put',
    put_many_cut_resolution_rules_request: 'reporting.v1.cut_resolution_rules.put_many',
    delete_cut_resolution_rule_request: 'reporting.v1.cut_resolution_rules.delete',
    delete_many_cut_resolution_rules_request: 'reporting.v1.cut_resolution_rules.delete_many',
    list_cut_resolution_rule_versions_request: 'reporting.v1.cut_resolution_rules_versions.list',
    get_cut_resolution_rule_version_request: 'reporting.v1.cut_resolution_rules_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_cut_resolution_rules_request: true,
    get_cut_resolution_rule_request: true,
    get_many_cut_resolution_rules_request: true,
    put_cut_resolution_rule_request: true,
    put_many_cut_resolution_rules_request: true,
    delete_cut_resolution_rule_request: true,
    delete_many_cut_resolution_rules_request: true,
    list_cut_resolution_rule_versions_request: true,
    get_cut_resolution_rule_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'reporting.v1.cut_resolution_rules_events.created',
    updated: 'reporting.v1.cut_resolution_rules_events.updated',
    deleted: 'reporting.v1.cut_resolution_rules_events.deleted',
} as const;
