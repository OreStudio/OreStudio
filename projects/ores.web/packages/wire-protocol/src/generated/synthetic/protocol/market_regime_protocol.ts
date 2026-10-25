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
/*
 * Hand written, mirroring ores.synthetic.api/messaging/market_regime_protocol.hpp.
 *
 * The C++ side is hand written too: the market regime is a service operation
 * rather than an entity, so no model states these messages and codegen emits
 * neither header nor twin. The two files are kept in step by hand, and a change
 * to one that is not made to the other breaks the wire rather than a build.
 */

/**
 * @brief Ask what the market-wide arrival factor is doing now.
 */
export interface GetMarketRegimeRequest {}

/**
 * @brief The factor in force: its state, its multiplier and whether a scenario
 * pinned it.
 */
export interface GetMarketRegimeResponse {
    success: boolean;
    message: string;
    /** @brief "calm" or "active". */
    state: string;
    /** @brief The multiplier every series is scaled by now. */
    multiplier: number;
    /** @brief Whether a scenario pinned the state rather than the chain
     * choosing it. */
    pinned: boolean;
}

/**
 * @brief Pin the market state, or hand it back to the chain.
 */
export interface PinMarketRegimeRequest {
    /**
     * @brief What to force: "auto" hands control back to the chain, "calm" and
     * "active" pin that state.
     */
    mode: string;
}

/**
 * @brief The state in force after the request.
 */
export interface PinMarketRegimeResponse {
    success: boolean;
    message: string;
    /** @brief The state in force now, as the read reports it. */
    state: string;
    /** @brief The multiplier in force now. */
    multiplier: number;
    /** @brief Whether the state is pinned after this request. */
    pinned: boolean;
}
