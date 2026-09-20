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
import type { CalendarRule } from '../domain/calendar_rule.js';

export interface GetCalendarRulesRequest {
    offset: number;
    limit: number;
}

export interface GetCalendarRulesResponse {
    calendar_rules: CalendarRule[];
    total_available_count: number;
    success: boolean;
    message: string;
}

export interface SaveCalendarRuleRequest {
    data: CalendarRule;
}

export interface SaveCalendarRuleResponse {
    success: boolean;
    message: string;
}

export interface DeleteCalendarRuleRequest {
    ids: string[];
}

export interface DeleteCalendarRuleResponse {
    success: boolean;
    message: string;
}

export interface GetCalendarRuleHistoryRequest {
    id: string;
}

export interface GetCalendarRuleHistoryResponse {
    history: CalendarRule[];
    success: boolean;
    message: string;
}

export interface GetCalendarRulesByCalendarCodeRequest {
    calendar_code: string;
    offset: number;
    limit: number;
}

export interface GetCalendarRulesByCalendarCodeResponse {
    calendar_rules: CalendarRule[];
    total_available_count: number;
    success: boolean;
    message: string;
}

export const subjects = {
    get_calendar_rules_request: "refdata.v1.calendar_rules.list",
    save_calendar_rule_request: "refdata.v1.calendar_rules.save",
    delete_calendar_rule_request: "refdata.v1.calendar_rules.delete",
    get_calendar_rule_history_request: "refdata.v1.calendar_rules.history",
    get_calendar_rules_by_calendar_code_request: "refdata.v1.calendar_rules.list_by_calendar_code",
} as const;
