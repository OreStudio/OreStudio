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
 * @brief Asks for the latest fixing of an item on or before a date.
 */
export interface GetFixingRequest {
    /**
     * @brief The fixing's oresmd URI, which names the ORE index.
     */
    item: string;
    /**
     * @brief The provider to read, or empty for the one provider that carries the item.
     *
     * Two providers can deliver one ORE index, the ECB rate from the ECB itself and from
     * a vendor. An item that more than one provider carries is refused as ambiguous
     * unless the request names one.
     */
    provider_code: string;
    /**
     * @brief The last date the fixing may fall on.
     */
    on_or_before: string;
}

export interface GetFixingResponse {
    /**
     * @brief How the call ended: a session with no party and an item that names the
     * series of more than one party are refused here.
     */
    result: Result;
    /**
     * @brief The provider that published the item.
     */
    provider_code: string;
    /**
     * @brief The scheme of the vendor id, or empty when no mapping names the item.
     */
    vendor_scheme: string;
    /**
     * @brief The provider's own id of the item, or empty when no mapping names it.
     */
    vendor_id: string;
    /**
     * @brief The date of the fixing, which is the publisher's own time for a daily
     * fixing; absent when the item has no fixing on or before the date.
     */
    fixing_date: string | null;
    /**
     * @brief The value as stored, in decimal text.
     */
    value: string;
    /**
     * @brief The source the fixing came through.
     */
    source: string;
    /**
     * @brief When the service stored the fixing.
     */
    received_at: string | null;
}

/**
 * @brief Asks for every fixing of an item in the days up to a date.
 */
export interface GetFixingWindowRequest {
    /**
     * @brief The fixing's oresmd URI, which names the ORE index.
     */
    item: string;
    /**
     * @brief The provider to read, or empty for the one provider that carries the item.
     *
     * Two providers can deliver one ORE index, the ECB rate from the ECB itself and from
     * a vendor. An item that more than one provider carries is refused as ambiguous
     * unless the request names one.
     */
    provider_code: string;
    /**
     * @brief The last date of the window, included.
     */
    to: string;
    /**
     * @brief How many calendar days the window spans, ending on the last date.
     */
    days: number;
}

export interface GetFixingWindowResponse {
    /**
     * @brief How the call ended: a session with no party, an ambiguous item, and a
     * window that is empty or over the ceiling are refused here.
     */
    result: Result;
    /**
     * @brief The provider that published the item.
     */
    provider_code: string;
    /**
     * @brief The scheme of the vendor id, or empty when no mapping names the item.
     */
    vendor_scheme: string;
    /**
     * @brief The provider's own id of the item, or empty when no mapping names it.
     */
    vendor_id: string;
    /**
     * @brief How many calendar days the window spans.
     */
    days: number;
    /**
     * @brief How many of those days hold a fixing; the days without one are skipped.
     */
    used: number;
    /**
     * @brief The date of each fixing, oldest first.
     */
    dates: string[];
    /**
     * @brief The value of each fixing, in decimal text.
     */
    values: string[];
    /**
     * @brief The source of each fixing.
     */
    sources: string[];
    /**
     * @brief When the service stored each fixing.
     */
    received_at: string[];
}

export const subjects = {
    get_fixing_request: 'marketdata.v1.ops.get_fixing',
    get_fixing_window_request: 'marketdata.v1.ops.get_fixing_window',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    get_fixing_request: true,
    get_fixing_window_request: true,
} as const;
