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
 * @brief On-demand "regenerate up to <year>" command for calendar_dates.
 *
 * Extends (never rewrites below the existing watermark) a single
 * calendar's materialised calendar_dates, or every calendar's, up to
 * end_year. No cron job triggers this -- it is user- or save-action-
 * triggered only.
 */
export interface RegenerateCalendarDatesRequest {
    calendar_code: string | null;
    end_year: number | null;
    in_use_only: boolean;
}

export interface RegenerateCalendarDatesResponse {
    success: boolean;
    message: string;
    rows_written: number;
}

/**
 * @brief Removes a tenant's copies of the days the installation owns.
 *
 * The days of a calendar the installation owns are held once, in the system
 * tenant, and every tenant reads them there. A tenant that was provisioned
 * before that rule keeps its own rows for the same calendar, and the party read
 * prefers them, so they mask the installation's later days. This command removes
 * the caller's rows for each such calendar and leaves the installation's copy
 * alone. A calendar the caller owns is not touched: its days are its own.
 *
 * It is idempotent: a second run finds nothing to remove. The delete policy of
 * the table confines it to the caller's tenant, so a tenant purges only its own
 * copies.
 */
export interface PurgeCalendarDateCopiesRequest {
    calendar_code: string | null;
}

export interface PurgeCalendarDateCopiesResponse {
    success: boolean;
    message: string;
    rows_removed: number;
    calendars_purged: number;
}

export const subjects = {
    regenerate_calendar_dates_request: 'refdata.v1.ops.regenerate_calendar_dates',
    purge_calendar_date_copies_request: 'refdata.v1.ops.purge_calendar_date_copies',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    regenerate_calendar_dates_request: true,
    purge_calendar_date_copies_request: true,
} as const;
