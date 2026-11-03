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
import type { CurveSegmentPillarRule } from '../domain/curve_segment_pillar_rule.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface CurveSegmentPillarRuleKey {
    id: string;
}

export interface CurveSegmentPillarRuleWrite {
    id: string;
    curve_segment_id: string;
    pillar_choice: string | null;
    priority: number | null;
    min_distance: number | null;
}

export interface CurveSegmentPillarRuleChange {
    write: CurveSegmentPillarRuleWrite;
    precondition: Precondition;
}

export interface CurveSegmentPillarRuleRemoval {
    key: CurveSegmentPillarRuleKey;
    precondition: Precondition;
}

export interface CurveSegmentPillarRuleLookup {
    key: CurveSegmentPillarRuleKey;
    curve_segment_pillar_rule: CurveSegmentPillarRule | null;
}

export interface CurveSegmentPillarRulesFilter {
    id_one_of: string[] | null;
}

export interface CurveSegmentPillarRuleEvent {
    event_id: string;
    key: CurveSegmentPillarRuleKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface CurveSegmentPillarRuleVersionKey {
    curve_segment_pillar_rule: CurveSegmentPillarRuleKey;
    version: number;
}

export interface CurveSegmentPillarRuleVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListCurveSegmentPillarRulesRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: CurveSegmentPillarRulesFilter | null;
    as_of: string | null;
}

export interface ListCurveSegmentPillarRulesResponse {
    result: Result;
    segment_pillar_rules: CurveSegmentPillarRule[];
    total: number;
}

export interface GetCurveSegmentPillarRuleRequest {
    key: CurveSegmentPillarRuleKey;
}

export interface GetCurveSegmentPillarRuleResponse {
    result: Result;
    curve_segment_pillar_rule: CurveSegmentPillarRule | null;
}

export interface GetManyCurveSegmentPillarRulesRequest {
    keys: CurveSegmentPillarRuleKey[];
}

export interface GetManyCurveSegmentPillarRulesResponse {
    result: Result;
    entries: CurveSegmentPillarRuleLookup[];
}

export interface PutCurveSegmentPillarRuleRequest {
    change: CurveSegmentPillarRuleChange;
    intent: ChangeIntent;
}

export interface PutCurveSegmentPillarRuleResponse {
    result: Result;
    curve_segment_pillar_rule: CurveSegmentPillarRule | null;
}

export interface PutManyCurveSegmentPillarRulesRequest {
    changes: CurveSegmentPillarRuleChange[];
    intent: ChangeIntent;
}

export interface PutManyCurveSegmentPillarRulesResponse {
    result: Result;
    segment_pillar_rules: CurveSegmentPillarRule[];
}

export interface DeleteCurveSegmentPillarRuleRequest {
    removal: CurveSegmentPillarRuleRemoval;
    intent: ChangeIntent;
}

export interface DeleteCurveSegmentPillarRuleResponse {
    result: Result;
}

export interface DeleteManyCurveSegmentPillarRulesRequest {
    removals: CurveSegmentPillarRuleRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyCurveSegmentPillarRulesResponse {
    result: Result;
}

export interface ListCurveSegmentPillarRuleVersionsRequest {
    key: CurveSegmentPillarRuleKey;
    offset: number;
    limit: number;
    order: Order;
    filter: CurveSegmentPillarRuleVersionsFilter | null;
}

export interface ListCurveSegmentPillarRuleVersionsResponse {
    result: Result;
    versions: CurveSegmentPillarRule[];
    total: number;
}

export interface GetCurveSegmentPillarRuleVersionRequest {
    key: CurveSegmentPillarRuleVersionKey;
}

export interface GetCurveSegmentPillarRuleVersionResponse {
    result: Result;
    version: CurveSegmentPillarRule | null;
}

export const subjects = {
    list_curve_segment_pillar_rules_request: 'refdata.v1.curve_segment_pillar_rules.list',
    get_curve_segment_pillar_rule_request: 'refdata.v1.curve_segment_pillar_rules.get',
    get_many_curve_segment_pillar_rules_request: 'refdata.v1.curve_segment_pillar_rules.get_many',
    put_curve_segment_pillar_rule_request: 'refdata.v1.curve_segment_pillar_rules.put',
    put_many_curve_segment_pillar_rules_request: 'refdata.v1.curve_segment_pillar_rules.put_many',
    delete_curve_segment_pillar_rule_request: 'refdata.v1.curve_segment_pillar_rules.delete',
    delete_many_curve_segment_pillar_rules_request:
        'refdata.v1.curve_segment_pillar_rules.delete_many',
    list_curve_segment_pillar_rule_versions_request:
        'refdata.v1.curve_segment_pillar_rules_versions.list',
    get_curve_segment_pillar_rule_version_request:
        'refdata.v1.curve_segment_pillar_rules_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_curve_segment_pillar_rules_request: true,
    get_curve_segment_pillar_rule_request: true,
    get_many_curve_segment_pillar_rules_request: true,
    put_curve_segment_pillar_rule_request: true,
    put_many_curve_segment_pillar_rules_request: true,
    delete_curve_segment_pillar_rule_request: true,
    delete_many_curve_segment_pillar_rules_request: true,
    list_curve_segment_pillar_rule_versions_request: true,
    get_curve_segment_pillar_rule_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'refdata.v1.events.curve_segment_pillar_rules.created',
    updated: 'refdata.v1.events.curve_segment_pillar_rules.updated',
    deleted: 'refdata.v1.events.curve_segment_pillar_rules.deleted',
} as const;
