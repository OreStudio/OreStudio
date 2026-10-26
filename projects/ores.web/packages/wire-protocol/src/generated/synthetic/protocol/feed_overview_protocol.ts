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
 * @brief One folder of the feed hierarchy, with what runs under it.
 *
 * A collection folder also carries the provider the collection stands for.
 */
export interface FeedFolder {
    /** @brief The folder's id. */
    id: string;
    /** @brief The parent folder's id; empty for a root. */
    parent_id: string;
    /** @brief The folder's display name. */
    name: string;
    /** @brief What the folder is: root, collection, asset_class or instrument_type. */
    kind: string;
    /** @brief For a collection, the provider it stands for; empty for any other folder. */
    provider_code: string;
    /** @brief The FX spot feeds under the folder, at any depth. */
    fx_feeds: number;
    /** @brief The IR curve feeds under the folder, at any depth. */
    ir_feeds: number;
    /** @brief How many of those feeds are running now. */
    running_feeds: number;
    /** @brief How many of those feeds are enabled, start on their own, and are not running now. */
    idle_auto_feeds: number;
    /** @brief Whether the folder has folders under it. */
    has_children: boolean;
}

/**
 * @brief Request the children of one folder, or the roots, with the feed counts.
 */
export interface ListFeedFoldersRequest {
    /** @brief The folder whose children are wanted; empty for the roots. */
    parent_id: string;
}

/**
 * @brief The children of the folder asked for, and why there are none when
 * the read was refused.
 */
export interface ListFeedFoldersResponse {
    success: boolean;
    message: string;
    /** @brief The children, in name order. */
    folders: FeedFolder[];
}

/**
 * @brief One feed as the feed tree lists it: enough to name it, to open its
 * page and to tell whether it runs, and nothing of its process.
 */
export interface FolderFeed {
    /** @brief The feed's id. */
    id: string;
    /** @brief The feed kind: fx_spot or ir_curve. */
    kind: string;
    /** @brief What a person calls the feed: the pair, or the currency and the index. */
    label: string;
    /** @brief The source name the feed publishes under. */
    source_name: string;
    /** @brief The oresmd URI the feed publishes. */
    oresmd_uri: string;
    /** @brief The folder the feed sits in; empty when it sits in none. */
    folder_id: string;
    /** @brief Whether the feed is enabled. */
    enabled: boolean;
    /** @brief The provider the feed's collection stands for. */
    provider_code: string;
    /** @brief The asset class of the feed: fx or interest_rates. */
    asset_class_code: string;
    /** @brief What the feed prices: a currency pair such as EUR/USD, or a currency such as USD. */
    underlying: string;
    /** @brief The process that generates the feed's ticks, as the feed stores it. */
    engine: string;
    /** @brief How many ticks the feed makes in an hour. */
    ticks_per_hour: number;
    /** @brief Whether the feed starts on its own when the service starts. */
    auto_start: boolean;
}

/**
 * @brief Request a page of the feeds that sit in one folder, or of the feeds of
 * every folder that match some words.
 */
export interface ListFolderFeedsRequest {
    /** @brief The folder whose feeds are wanted, with those of the folders under it; empty for the feeds of every folder. */
    folder_id: string;
    /** @brief Only the feeds of this provider; empty for every provider. */
    provider_code: string;
    /** @brief Only the feeds that are enabled, start on their own, and are not running now. */
    should_run: boolean;
    /** @brief Words that must all appear in the feed's label, source name or URI; empty for no search. */
    search: string;
    /** @brief Which feeds: all, started or stopped; empty means all. */
    state: string;
    /** @brief Only the feeds of this asset class code; empty for every class. */
    asset_class: string;
    /** @brief How many feeds to skip. */
    offset: number;
    /** @brief How many feeds to return at most. */
    limit: number;
}

/**
 * @brief A page of feeds, and how many match in all.
 */
export interface ListFolderFeedsResponse {
    success: boolean;
    message: string;
    /** @brief How many feeds match, of which the page holds some. */
    total: number;
    /** @brief The page of feeds, in label order. */
    feeds: FolderFeed[];
}

export const subjects = {
    list_feed_folders_request: 'synthetic.v1.ops.list_feed_folders',
    list_folder_feeds_request: 'synthetic.v1.ops.list_folder_feeds',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_feed_folders_request: true,
    list_folder_feeds_request: true,
} as const;
