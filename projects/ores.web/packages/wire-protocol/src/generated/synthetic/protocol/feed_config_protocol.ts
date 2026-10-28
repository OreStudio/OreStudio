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
 * @brief Request to start one feed on demand, keyed by config_id.
 *
 * One kind-agnostic request shape for every asset class: the server resolves
 * the config (of whichever kind it is), its children, and the refdata context
 * from config_id and builds the feed via the producer factory -- the IR curve
 * pattern, applied uniformly. The client never names a source_name or supplies
 * producer parameters; a config's own identity is the whole request. Replaces
 * the per-kind start requests (market_feed_config's client-supplied-params
 * pass-through and ir_curve_feed_config's config_id request).
 */
export interface StartFeedRequest {
    config_id: string;
}

/**
 * @brief Whether the feed started, and why not when it did not.
 */
export interface StartFeedResponse {
    success: boolean;
    message: string;
}

/**
 * @brief Request to stop running feed(s), identified by config_id or source_name.
 *
 * config_id is preferred: it is resolved server-side to the config's
 * source_name, the same way start resolves it -- the client never needs to
 * know the "synthetic.<pair>" / "ir_curve.<ccy>.<idx>" naming conventions. If
 * both config_id and source_name are empty, stops all running feeds of every
 * kind.
 */
export interface StopFeedRequest {
    /** @brief Preferred: resolved server-side to source_name. */
    config_id: string;
    /** @brief Used only if config_id is empty; empty too stops every feed. */
    source_name: string;
}

/**
 * @brief Whether the feed stopped, and why not when it did not.
 */
export interface StopFeedResponse {
    success: boolean;
    message: string;
}

/**
 * @brief Where a feed's starting value came from, or that it kept its fixed level.
 *
 * The record belongs to the run, not to the config: a feed that stops and
 * starts again records where the new run started.
 */
export interface FeedStart {
    /** @brief True when a fixing set the level; false when the feed kept its fixed level. */
    from_fixing: boolean;
    /** @brief The fixing's item, as the oresmd URI that names it. */
    item: string;
    /** @brief The provider that published the fixing. */
    provider_code: string;
    /** @brief The provider's own id of the item, or empty when no mapping names it. */
    vendor_id: string;
    /** @brief The fixing's value as the publisher wrote it, in decimal text. */
    value: string;
    /** @brief True when the item quotes the feed's pair the other way round. */
    inverted: boolean;
    /** @brief The date of the fixing, which is the publisher's own time for a daily fixing. */
    fixing_date: string | null;
    /** @brief When the service stored the fixing. */
    received_at: string | null;
    /** @brief How many calendar days of fixings the window covered. */
    window_days: number;
    /** @brief How many fixings of the window measured the spread. */
    used: number;
    /** @brief The measured hourly spread, when the window had one. */
    hourly_spread: number | null;
    /** @brief Why the feed kept its fixed level, or why its spread was not measured. */
    note: string;
}

/**
 * @brief One feed the service runs now, and where its starting value came from.
 *
 * A feed kind that starts from no published value carries no start, so it reads
 * as running and says nothing of where it started.
 */
export interface RunningFeed {
    /** @brief The source name the run publishes under. */
    source_name: string;
    /** @brief Where the run's starting value came from; absent when the kind has no such start. */
    start: FeedStart | null;
}

/**
 * @brief Request the feeds that run now, every kind, with where each started.
 */
export interface ListFeedsRequest {}

/**
 * @brief The feeds the service runs now, each with where it started, and whether
 * the read succeeded.
 */
export interface ListFeedsResponse {
    success: boolean;
    /** @brief The feeds that run, in source name order. */
    running_feeds: RunningFeed[];
}

export const subjects = {
    start_feed_request: 'synthetic.v1.ops.start_feed',
    stop_feed_request: 'synthetic.v1.ops.stop_feed',
    list_feeds_request: 'synthetic.v1.feed_configs.list',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    start_feed_request: true,
    stop_feed_request: true,
    list_feeds_request: true,
} as const;
