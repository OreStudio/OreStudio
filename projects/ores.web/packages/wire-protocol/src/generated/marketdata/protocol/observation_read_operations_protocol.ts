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
import type { Result } from '../../../utility/protocol.js';

/**
 * @brief Asks for every observation of a series in the days up to a date.
 */
export interface GetObservationWindowRequest {
    /**
     * @brief The series' oresmd URI.
     */
    item: string;
    /**
     * @brief The provider to read, or empty for the one provider that carries the series.
     *
     * A vendor and the derived source can both carry one series. A series that more than
     * one provider carries is refused as ambiguous unless the request names one.
     */
    provider_code: string;
    /**
     * @brief Whose observations to read, or empty for the parties the caller can see.
     *
     * An observation is entitled by party, as a fixing is: a service names the party
     * whose data it needs, and a session that sees no party is refused.
     */
    party_id: string;
    /**
     * @brief The last date of the window, included.
     */
    to: string;
    /**
     * @brief How many calendar days the window spans, ending on the last date.
     */
    days: number;
}

export interface GetObservationWindowResponse {
    /**
     * @brief How the call ended: a session with no party, an ambiguous series, and a
     * window that is empty or over the ceiling are refused here.
     */
    result: Result;
    /**
     * @brief The provider of the series.
     */
    provider_code: string;
    /**
     * @brief How many calendar days the window spans.
     */
    days: number;
    /**
     * @brief How many observations the window holds.
     */
    used: number;
    /**
     * @brief The time each observation is for, oldest first.
     */
    observed_at: string[];
    /**
     * @brief The oresmd URI of each observation's point of the series.
     */
    coordinates: string[];
    /**
     * @brief The value of each observation, in decimal text.
     */
    values: string[];
    /**
     * @brief The source of each observation.
     */
    sources: string[];
}

export const subjects = {
    get_observation_window_request: 'marketdata.v1.ops.get_observation_window',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    get_observation_window_request: true,
} as const;
