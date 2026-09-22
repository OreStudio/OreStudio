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
/**
 * @brief Overrides applied to a named ORE calendar.
 *
 * A calendar adjustment patches a built-in calendar with institution-
 * or date-specific exceptions: additional holidays, and additional
 * business days. Dates are ISO-8601 strings ("YYYY-MM-DD") verbatim
 * from the source file, so a round trip preserves them exactly.
 */
export interface CalendarAdjustment {
    version: number;
    tenant_id: string;
    calendar_name: string;
    base_calendar: string | null;
    additional_holidays: string[];
    additional_business_days: string[];
    modified_by: string;
    change_reason_code: string;
    change_commentary: string;
    performed_by: string;
    recorded_at: string;
}

/**
 * @brief Requests the transient calendar_adjustment DTOs ORE needs for
 * source='user' calendar templates, assembled server-side from
 * calendar + calendar_exception rows -- never itself persisted. The
 * caller turns the response into a CalendarAdjustments XML file.
 */
export interface GetCalendarAdjustmentsRequest {
    calendar_codes: string[];
}

export interface GetCalendarAdjustmentsResponse {
    adjustments: CalendarAdjustment[];
    success: boolean;
    message: string;
}

export const subjects = {
    get_calendar_adjustments_request: "refdata.v1.calendar_adjustments.export",
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    get_calendar_adjustments_request: true,
} as const;
