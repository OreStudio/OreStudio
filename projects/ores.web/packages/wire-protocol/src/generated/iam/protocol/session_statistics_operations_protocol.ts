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
 * @brief One day's session statistics, for one account.
 *
 * The duration is in seconds and the byte counts are totals over the day's
 * ended sessions. A day with no ended sessions has no row.
 */
export interface SessionStatisticsRow {
    day: string;
    account_id: string;
    session_count: number;
    avg_duration_seconds: number;
    total_bytes_sent: number;
    total_bytes_received: number;
    avg_bytes_sent: number;
    avg_bytes_received: number;
    unique_countries: number;
}

/**
 * @brief A window over the caller's tenant session statistics.
 *
 * An empty filter does not filter. The window is newest first, so the screen
 * reads the most recent days without asking for an order.
 */
export interface GetSessionStatisticsRequest {
    account_id: string;
    from_time: string;
    to_time: string;
    limit: number;
    offset: number;
}

export interface GetSessionStatisticsResponse {
    rows: SessionStatisticsRow[];
    success: boolean;
    message: string;
}

export const subjects = {
    get_session_statistics_request: 'iam.v1.ops.get_session_statistics',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    get_session_statistics_request: true,
} as const;
