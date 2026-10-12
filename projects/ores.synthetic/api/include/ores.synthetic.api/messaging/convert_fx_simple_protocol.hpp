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
 * Template: cpp_protocol.hpp.mustache
 * To modify, update the template and regenerate.
 */
#ifndef ORES_SYNTHETIC_API_MESSAGING_CONVERT_FX_SIMPLE_PROTOCOL_HPP
#define ORES_SYNTHETIC_API_MESSAGING_CONVERT_FX_SIMPLE_PROTOCOL_HPP

#include <string>
#include <string_view>
#include <vector>

namespace ores::synthetic::messaging {

/**
 * @brief Request a mixture in the terms of the simple view, or the other way.
 *
 * Stateless: the caller names the values as they stand on its screen.
 */
struct convert_fx_simple_request {
    using response_type = struct convert_fx_simple_response;
    static constexpr std::string_view nats_subject = "synthetic.v1.ops.convert_fx_simple";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    /** @brief The engine the mixture is read by: geometric, arithmetic or ornstein_uhlenbeck. */
    std::string process_type;
    /** @brief True to read the mixture as the simple view; false to write the simple view as a
     * mixture. */
    bool to_simple = false;
    /** @brief The price that scales between log-returns and price units. */
    double initial_price = 1.0;
    /** @brief The feed's rate, which turns a figure per hour into one per update. */
    int ticks_per_hour = 3600;
    /** @brief Component means, when reading a mixture; means, stdevs and weights have equal size.
     */
    std::vector<double> gmm_means;
    /** @brief Component standard deviations, when reading a mixture. */
    std::vector<double> gmm_stdevs;
    /** @brief Component weights, when reading a mixture; the first is κ under the
     * Ornstein-Uhlenbeck engine. */
    std::vector<double> gmm_weights;
    /** @brief The simple view's drift, as a percent of the price over an hour. */
    double trend_pct_per_hour = 0.0;
    /** @brief The simple view's volatility, as a percent of the price over an hour. */
    double volatility_pct_per_hour = 0.0;
    /** @brief The percent of updates that come from the jump regime. */
    double jump_share_pct = 0.0;
    /** @brief Minutes to come half way back to the long-run price; read by Ornstein-Uhlenbeck only.
     */
    double half_life_minutes = 0.0;
};

/**
 * @brief The converted values, and why there are none when there are none.
 */
struct convert_fx_simple_response {
    bool success = false;
    std::string message;
    /** @brief False when the simple view only approximates the mixture; message then says why. */
    bool has_simple_form = false;
    /** @brief The simple view's drift, as a percent of the price over an hour. */
    double trend_pct_per_hour = 0.0;
    /** @brief The simple view's volatility, as a percent of the price over an hour. */
    double volatility_pct_per_hour = 0.0;
    /** @brief The percent of updates that come from the jump regime. */
    double jump_share_pct = 0.0;
    /** @brief Minutes to come half way back to the long-run price. */
    double half_life_minutes = 0.0;
    /** @brief The mixture's means. */
    std::vector<double> gmm_means;
    /** @brief The mixture's standard deviations. */
    std::vector<double> gmm_stdevs;
    /** @brief The mixture's weights, κ first under the Ornstein-Uhlenbeck engine. */
    std::vector<double> gmm_weights;
};

}

#endif
