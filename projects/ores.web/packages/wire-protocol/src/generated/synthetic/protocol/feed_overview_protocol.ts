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
 * A collection folder also carries what the collection says: its scope, the
 * provider it stands for, and whether it is bound or sandboxed.
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
    /** @brief For a collection, who consumes its data: system, tenant or party; empty otherwise. */
    scope: string;
    /** @brief For a collection, the provider it stands for; empty when none. */
    provider_code: string;
    /** @brief For a collection, bound or sandboxed; empty otherwise. */
    binding_mode: string;
    /** @brief The FX spot feeds under the folder, at any depth. */
    fx_feeds: number;
    /** @brief The IR curve feeds under the folder, at any depth. */
    ir_feeds: number;
    /** @brief How many of those feeds are running now. */
    running_feeds: number;
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

export const subjects = {
    list_feed_folders_request: 'synthetic.v1.ops.list_feed_folders',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_feed_folders_request: true,
} as const;
