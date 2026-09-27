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
import type { SeriesClassificationRule } from '../domain/series_classification_rule.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface SeriesClassificationRuleKey {
    series_type: string;
    metric: string;
}

export interface SeriesClassificationRuleWrite {
    series_type: string;
    metric: string;
    asset_class_source: string;
    asset_class_code: string | null;
    series_subclass_code: string;
    description: string;
}

export interface SeriesClassificationRuleChange {
    write: SeriesClassificationRuleWrite;
    precondition: Precondition;
}

export interface SeriesClassificationRuleRemoval {
    key: SeriesClassificationRuleKey;
    precondition: Precondition;
}

export interface SeriesClassificationRuleLookup {
    key: SeriesClassificationRuleKey;
    series_classification_rule: SeriesClassificationRule | null;
}

export interface SeriesClassificationRuleEvent {
    event_id: string;
    key: SeriesClassificationRuleKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface SeriesClassificationRuleVersionKey {
    series_classification_rule: SeriesClassificationRuleKey;
    version: number;
}

export interface SeriesClassificationRuleVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListSeriesClassificationRulesRequest {
    offset: number;
    limit: number;
    order: Order;
}

export interface ListSeriesClassificationRulesResponse {
    result: Result;
    rules: SeriesClassificationRule[];
    total: number;
}

export interface GetSeriesClassificationRuleRequest {
    key: SeriesClassificationRuleKey;
}

export interface GetSeriesClassificationRuleResponse {
    result: Result;
    series_classification_rule: SeriesClassificationRule | null;
}

export interface GetManySeriesClassificationRulesRequest {
    keys: SeriesClassificationRuleKey[];
}

export interface GetManySeriesClassificationRulesResponse {
    result: Result;
    entries: SeriesClassificationRuleLookup[];
}

export interface PutSeriesClassificationRuleRequest {
    change: SeriesClassificationRuleChange;
    intent: ChangeIntent;
}

export interface PutSeriesClassificationRuleResponse {
    result: Result;
    series_classification_rule: SeriesClassificationRule;
}

export interface PutManySeriesClassificationRulesRequest {
    changes: SeriesClassificationRuleChange[];
    intent: ChangeIntent;
}

export interface PutManySeriesClassificationRulesResponse {
    result: Result;
    rules: SeriesClassificationRule[];
}

export interface DeleteSeriesClassificationRuleRequest {
    removal: SeriesClassificationRuleRemoval;
    intent: ChangeIntent;
}

export interface DeleteSeriesClassificationRuleResponse {
    result: Result;
}

export interface DeleteManySeriesClassificationRulesRequest {
    removals: SeriesClassificationRuleRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManySeriesClassificationRulesResponse {
    result: Result;
}

export interface ListSeriesClassificationRuleVersionsRequest {
    key: SeriesClassificationRuleKey;
    offset: number;
    limit: number;
    order: Order;
    filter: SeriesClassificationRuleVersionsFilter | null;
}

export interface ListSeriesClassificationRuleVersionsResponse {
    result: Result;
    versions: SeriesClassificationRule[];
    total: number;
}

export interface GetSeriesClassificationRuleVersionRequest {
    key: SeriesClassificationRuleVersionKey;
}

export interface GetSeriesClassificationRuleVersionResponse {
    result: Result;
    version: SeriesClassificationRule;
}

export const subjects = {
    list_series_classification_rules_request: 'marketdata.v1.series_classification_rules.list',
    get_series_classification_rule_request: 'marketdata.v1.series_classification_rules.get',
    get_many_series_classification_rules_request:
        'marketdata.v1.series_classification_rules.get_many',
    put_series_classification_rule_request: 'marketdata.v1.series_classification_rules.put',
    put_many_series_classification_rules_request:
        'marketdata.v1.series_classification_rules.put_many',
    delete_series_classification_rule_request: 'marketdata.v1.series_classification_rules.delete',
    delete_many_series_classification_rules_request:
        'marketdata.v1.series_classification_rules.delete_many',
    list_series_classification_rule_versions_request:
        'marketdata.v1.series_classification_rules_versions.list',
    get_series_classification_rule_version_request:
        'marketdata.v1.series_classification_rules_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_series_classification_rules_request: true,
    get_series_classification_rule_request: true,
    get_many_series_classification_rules_request: true,
    put_series_classification_rule_request: true,
    put_many_series_classification_rules_request: true,
    delete_series_classification_rule_request: true,
    delete_many_series_classification_rules_request: true,
    list_series_classification_rule_versions_request: true,
    get_series_classification_rule_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'marketdata.v1.series_classification_rules_events.created',
    updated: 'marketdata.v1.series_classification_rules_events.updated',
    deleted: 'marketdata.v1.series_classification_rules_events.deleted',
} as const;
