/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
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
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */
#ifndef ORES_SYNTHETIC_API_MESSAGING_MARKET_REGIME_PROTOCOL_HPP
#define ORES_SYNTHETIC_API_MESSAGING_MARKET_REGIME_PROTOCOL_HPP

#include <string>
#include <string_view>

namespace ores::synthetic::messaging {

/**
 * @brief Ask what the market-wide arrival factor is doing now.
 */
struct get_market_regime_request {
    using response_type = struct get_market_regime_response;
    static constexpr std::string_view nats_subject = "synthetic.v1.ops.get_market_regime";
    /// @brief Whether the caller's session is required: it is, so the read is attributable.
    static constexpr bool requires_session = true;
};

/**
 * @brief The factor in force: its state, its multiplier and whether a scenario pinned it.
 */
struct get_market_regime_response {
    bool success = false;
    std::string message;
    /** @brief "calm" or "active". */
    std::string state;
    /** @brief The multiplier every series is scaled by now. */
    double multiplier = 1.0;
    /** @brief Whether a scenario pinned the state rather than the chain choosing it. */
    bool pinned = false;
};

/**
 * @brief Pin the market state, or hand it back to the chain.
 */
struct pin_market_regime_request {
    using response_type = struct pin_market_regime_response;
    static constexpr std::string_view nats_subject = "synthetic.v1.ops.pin_market_regime";
    static constexpr bool requires_session = true;
    /**
     * @brief What to force: "auto" hands control back to the chain, "calm" and
     * "active" pin that state.
     */
    std::string mode;
};

/**
 * @brief The state in force after the request.
 */
struct pin_market_regime_response {
    bool success = false;
    std::string message;
    /** @brief The state in force now, as the read reports it. */
    std::string state;
    /** @brief The multiplier in force now. */
    double multiplier = 1.0;
    /** @brief Whether the state is pinned after this request. */
    bool pinned = false;
};

}

#endif
