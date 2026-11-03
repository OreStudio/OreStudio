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
import type { TradeSummary } from '../domain/trade_summary.js';
import type { Order } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';
import type { TradeSummariesFilter } from '../../../generated/trading/protocol/trade_summary_protocol.js';

/**
 * @brief One node of a rail, and the rows a filter places on it.
 */
export interface BlotterRailNode {
    /**
     * @brief Which rail this node belongs to: portfolio, book, counterparty,
     * asset class or state.
     */
    dimension: string;
    /**
     * @brief The node's own id, empty for the party at the head of a rail.
     */
    node_id: string;
    /**
     * @brief How many rows the filter places on this node. A portfolio's figure
     * counts every row below it, because that is what a person reading the tree
     * takes it to mean.
     */
    count: number;
}

/**
 * @brief The rail's counts for one filter, every dimension in one answer.
 *
 * One read rather than one per rail: the counts come from the same scan of the
 * same rows, and a screen that asks five times pays five times for it.
 */
export interface GetBlotterRailRequest {
    /**
     * @brief Which rows the rail counts. Absent counts every row of the party.
     */
    filter: TradeSummariesFilter | null;
}

/**
 * @brief The counts at each node of each rail.
 */
export interface GetBlotterRailResponse {
    /**
     * @brief Outcome of the operation.
     */
    result: Result;
    /**
     * @brief The nodes, each stating its own dimension. The party's own totals
     * come first, with an empty node id.
     */
    nodes: BlotterRailNode[];
}

/**
 * @brief One page of blotter rows, and where the next page starts.
 *
 * The page is keyed rather than offset: the caller states the last row it was
 * given, and the store starts after it. Every page then costs what the first
 * costs, which an offset cannot promise because it re-walks what it skips.
 */
export interface GetBlotterPageRequest {
    /**
     * @brief Which rows the page may hold. Absent states the whole party.
     */
    filter: TradeSummariesFilter | null;
    /**
     * @brief The column the page is ordered by. The row id is the tie-break, so
     * the order is total and a key is unambiguous.
     */
    order: Order;
    /**
     * @brief How many rows the caller will accept. The service states its own
     * ceiling, because a page is a page and not the book.
     */
    limit: number;
    /**
     * @brief The key to start after, as the previous answer stated it. Empty
     * starts at the first row. It is the row's sort value and id together, and the
     * caller does not read it: it is opaque, so the order can change without the
     * client changing.
     */
    after: string;
}

/**
 * @brief One page of rows, and the key the next page starts after.
 */
export interface GetBlotterPageResponse {
    /**
     * @brief Outcome of the operation.
     */
    result: Result;
    /**
     * @brief The page, at most the limit asked for.
     */
    rows: TradeSummary[];
    /**
     * @brief The key the next page starts after, empty when this page is the
     * last. A caller that reaches the end is told so rather than guessing from a
     * short page, because a page may be short for a reason of its own.
     */
    next: string;
}

export const subjects = {
    get_blotter_rail_request: 'trading.v1.ops.get_blotter_rail',
    get_blotter_page_request: 'trading.v1.ops.get_blotter_page',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    get_blotter_rail_request: true,
    get_blotter_page_request: true,
} as const;
