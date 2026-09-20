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

export interface GetSeriesClassificationRulesRequest {
    offset: number;
    limit: number;
}

export interface GetSeriesClassificationRulesResponse {
    rules: SeriesClassificationRule[];
    total_available_count: number;
    success: boolean;
    message: string;
}

export interface SaveSeriesClassificationRuleRequest {
    data: SeriesClassificationRule;
}

export interface SaveSeriesClassificationRuleResponse {
    success: boolean;
    message: string;
}

export interface DeleteSeriesClassificationRuleRequest {
    series_types: string[];
    metrics: string[];
}

export interface DeleteSeriesClassificationRuleResponse {
    success: boolean;
    message: string;
}

export interface GetSeriesClassificationRuleHistoryRequest {
    series_type: string;
}

export interface GetSeriesClassificationRuleHistoryResponse {
    history: SeriesClassificationRule[];
    success: boolean;
    message: string;
}

export const subjects = {
    get_series_classification_rules_request: "marketdata.v1.series_classification_rules.list",
    save_series_classification_rule_request: "marketdata.v1.series_classification_rules.save",
    delete_series_classification_rule_request: "marketdata.v1.series_classification_rules.delete",
    get_series_classification_rule_history_request: "marketdata.v1.series_classification_rules.history",
} as const;
