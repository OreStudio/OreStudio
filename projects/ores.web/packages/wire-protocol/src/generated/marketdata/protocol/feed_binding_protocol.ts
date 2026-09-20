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
import type { FeedBinding } from '../domain/feed_binding.js';

export interface GetFeedBindingsRequest {
    offset: number;
    limit: number;
}

export interface GetFeedBindingsResponse {
    feed_bindings: FeedBinding[];
    total_available_count: number;
    success: boolean;
    message: string;
}

export interface SaveFeedBindingRequest {
    data: FeedBinding;
}

export interface SaveFeedBindingResponse {
    success: boolean;
    message: string;
}

export interface DeleteFeedBindingRequest {
    ids: string[];
}

export interface DeleteFeedBindingResponse {
    success: boolean;
    message: string;
}

export interface GetFeedBindingHistoryRequest {
    id: string;
}

export interface GetFeedBindingHistoryResponse {
    history: FeedBinding[];
    success: boolean;
    message: string;
}

export const subjects = {
    get_feed_bindings_request: "marketdata.v1.feed_bindings.list",
    save_feed_binding_request: "marketdata.v1.feed_bindings.save",
    delete_feed_binding_request: "marketdata.v1.feed_bindings.delete",
    get_feed_binding_history_request: "marketdata.v1.feed_bindings.history",
} as const;
