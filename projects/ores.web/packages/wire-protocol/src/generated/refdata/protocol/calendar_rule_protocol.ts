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
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';
import type { Scope } from '../../../utility/protocol.js';

export interface CalendarRuleKey {
    id: string;
}

export interface CalendarRuleWrite {
    id: string;
    calendar_code: string;
    kind: string;
    month: number | null;
    day: number | null;
    weekday: number | null;
    occurrence: number | null;
    day_offset: number | null;
    shift: string;
    effective_from: number | null;
    effective_to: number | null;
}

export interface CalendarRuleChange {
    write: CalendarRuleWrite;
    precondition: Precondition;
}

export interface CalendarRuleRemoval {
    key: CalendarRuleKey;
    precondition: Precondition;
}

export interface CalendarRuleLookup {
    key: CalendarRuleKey;
    calendar_rule: CalendarRule | null;
}

export interface CalendarRulesFilter {
    calendar_code: string | null;
}

export interface CalendarRuleEvent {
    event_id: string;
    key: CalendarRuleKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface CalendarRuleVersionKey {
    calendar_rule: CalendarRuleKey;
    version: number;
}

export interface CalendarRuleVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListCalendarRulesRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: CalendarRulesFilter | null;
}

export interface ListCalendarRulesResponse {
    result: Result;
    calendar_rules: CalendarRule[];
    total: number;
}

export interface GetCalendarRuleRequest {
    key: CalendarRuleKey;
}

export interface GetCalendarRuleResponse {
    result: Result;
    calendar_rule: CalendarRule | null;
}

export interface GetManyCalendarRulesRequest {
    keys: CalendarRuleKey[];
}

export interface GetManyCalendarRulesResponse {
    result: Result;
    entries: CalendarRuleLookup[];
}

export interface PutCalendarRuleRequest {
    change: CalendarRuleChange;
    intent: ChangeIntent;
}

export interface PutCalendarRuleResponse {
    result: Result;
    calendar_rule: CalendarRule;
}

export interface PutManyCalendarRulesRequest {
    changes: CalendarRuleChange[];
    intent: ChangeIntent;
}

export interface PutManyCalendarRulesResponse {
    result: Result;
    calendar_rules: CalendarRule[];
}

export interface DeleteCalendarRuleRequest {
    removal: CalendarRuleRemoval;
    intent: ChangeIntent;
}

export interface DeleteCalendarRuleResponse {
    result: Result;
}

export interface DeleteManyCalendarRulesRequest {
    removals: CalendarRuleRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyCalendarRulesResponse {
    result: Result;
}

export interface ListByCalendarCodeCalendarRulesRequest {
    calendar_code: string;
    scope: Scope;
    offset: number;
    limit: number;
    order: Order;
    filter: CalendarRulesFilter | null;
}

export interface ListByCalendarCodeCalendarRulesResponse {
    result: Result;
    calendar_rules: CalendarRule[];
    total: number;
}

export interface ListCalendarRuleVersionsRequest {
    key: CalendarRuleKey;
    offset: number;
    limit: number;
    order: Order;
    filter: CalendarRuleVersionsFilter | null;
}

export interface ListCalendarRuleVersionsResponse {
    result: Result;
    versions: CalendarRule[];
    total: number;
}

export interface GetCalendarRuleVersionRequest {
    key: CalendarRuleVersionKey;
}

export interface GetCalendarRuleVersionResponse {
    result: Result;
    version: CalendarRule;
}

export const subjects = {
    list_calendar_rules_request: "refdata.v1.calendar_rules.list",
    get_calendar_rule_request: "refdata.v1.calendar_rules.get",
    get_many_calendar_rules_request: "refdata.v1.calendar_rules.get_many",
    put_calendar_rule_request: "refdata.v1.calendar_rules.put",
    put_many_calendar_rules_request: "refdata.v1.calendar_rules.put_many",
    delete_calendar_rule_request: "refdata.v1.calendar_rules.delete",
    delete_many_calendar_rules_request: "refdata.v1.calendar_rules.delete_many",
    list_by_calendar_code_calendar_rules_request: "refdata.v1.calendar_rules.list_by_calendar_code",
    list_calendar_rule_versions_request: "refdata.v1.calendar_rules_versions.list",
    get_calendar_rule_version_request: "refdata.v1.calendar_rules_versions.get",
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_calendar_rules_request: true,
    get_calendar_rule_request: true,
    get_many_calendar_rules_request: true,
    put_calendar_rule_request: true,
    put_many_calendar_rules_request: true,
    delete_calendar_rule_request: true,
    delete_many_calendar_rules_request: true,
    list_by_calendar_code_calendar_rules_request: true,
    list_calendar_rule_versions_request: true,
    get_calendar_rule_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: "refdata.v1.calendar_rules_events.created",
    updated: "refdata.v1.calendar_rules_events.updated",
    deleted: "refdata.v1.calendar_rules_events.deleted",
} as const;
