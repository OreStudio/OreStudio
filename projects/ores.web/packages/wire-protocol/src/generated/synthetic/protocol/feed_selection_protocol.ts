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
 * @brief Start the picked feeds, or the feeds a filter matches.
 */
export interface StartSelectedFeedsRequest {
    /** @brief The folder whose feeds are wanted, with those of the folders under it; empty for the feeds of every folder. Used when no feed ids are named. */
    folder_id: string;
    /** @brief Only the feeds of this provider; empty for every provider. Used when no feed ids are named. */
    provider_code: string;
    /** @brief Only the feeds that are enabled, start on their own, and are not running now. Used when no feed ids are named. */
    should_run: boolean;
    /** @brief Words that must all appear in the feed's label, source name or URI. Used when no feed ids are named. */
    search: string;
    /** @brief Which feeds: all, started or stopped; empty means all. Used when no feed ids are named. */
    state: string;
    /** @brief Only the feeds of this asset class code; empty for every class. Used when no feed ids are named. */
    asset_class: string;
    /** @brief The feeds a person picked one by one. When it is not empty, exactly these feeds are meant and the filter is ignored. */
    feed_ids: string[];
    /** @brief The feeds of the filter a person left out. Used when no feed ids are named. */
    excluded_ids: string[];
}

/**
 * @brief What a start of selected feeds did.
 */
export interface StartSelectedFeedsResponse {
    /** @brief Whether the request ran. */
    success: boolean;
    /** @brief What happened in words, or why it did not run. */
    message: string;
    /** @brief How many feeds started. */
    started: number;
    /** @brief How many feeds already ran. */
    already_running: number;
    /** @brief How many feeds could not start: disabled, or another running feed holds the series. */
    skipped: number;
    /** @brief How many feeds stopped. */
    stopped: number;
}

/**
 * @brief Stop the picked feeds, or the feeds a filter matches.
 */
export interface StopSelectedFeedsRequest {
    /** @brief The folder whose feeds are wanted, with those of the folders under it; empty for the feeds of every folder. Used when no feed ids are named. */
    folder_id: string;
    /** @brief Only the feeds of this provider; empty for every provider. Used when no feed ids are named. */
    provider_code: string;
    /** @brief Only the feeds that are enabled, start on their own, and are not running now. Used when no feed ids are named. */
    should_run: boolean;
    /** @brief Words that must all appear in the feed's label, source name or URI. Used when no feed ids are named. */
    search: string;
    /** @brief Which feeds: all, started or stopped; empty means all. Used when no feed ids are named. */
    state: string;
    /** @brief Only the feeds of this asset class code; empty for every class. Used when no feed ids are named. */
    asset_class: string;
    /** @brief The feeds a person picked one by one. When it is not empty, exactly these feeds are meant and the filter is ignored. */
    feed_ids: string[];
    /** @brief The feeds of the filter a person left out. Used when no feed ids are named. */
    excluded_ids: string[];
}

/**
 * @brief What a stop of selected feeds did.
 */
export interface StopSelectedFeedsResponse {
    /** @brief Whether the request ran. */
    success: boolean;
    /** @brief What happened in words, or why it did not run. */
    message: string;
    /** @brief How many feeds started. */
    started: number;
    /** @brief How many feeds already ran. */
    already_running: number;
    /** @brief How many feeds could not start: disabled, or another running feed holds the series. */
    skipped: number;
    /** @brief How many feeds stopped. */
    stopped: number;
}

export const subjects = {
    start_selected_feeds_request: 'synthetic.v1.ops.start_selected_feeds',
    stop_selected_feeds_request: 'synthetic.v1.ops.stop_selected_feeds',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    start_selected_feeds_request: true,
    stop_selected_feeds_request: true,
} as const;
