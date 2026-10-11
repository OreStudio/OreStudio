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
#ifndef ORES_SYNTHETIC_API_MESSAGING_CONVERT_FX_MIXTURE_PROTOCOL_HPP
#define ORES_SYNTHETIC_API_MESSAGING_CONVERT_FX_MIXTURE_PROTOCOL_HPP

#include <string>
#include <string_view>
#include <vector>

namespace ores::synthetic::messaging {

/**
 * @brief Request the mixture an engine change leaves.
 *
 * Stateless: the caller names the mixture as it stands on its screen, so an
 * unsaved mixture converts as readily as a saved one.
 */
struct convert_fx_mixture_request {
    using response_type = struct convert_fx_mixture_response;
    static constexpr std::string_view nats_subject = "synthetic.v1.ops.convert_fx_mixture";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    /** @brief Component means; means, stdevs and weights have equal size. */
    std::vector<double> gmm_means;
    /** @brief Component standard deviations. */
    std::vector<double> gmm_stdevs;
    /** @brief Component weights; the first is κ under the Ornstein-Uhlenbeck engine. */
    std::vector<double> gmm_weights;
    /** @brief The engine the mixture is written for. */
    std::string from_process_type = "geometric";
    /** @brief The engine the mixture is converted for. */
    std::string to_process_type = "geometric";
    /** @brief The price that scales between log-returns and price units. */
    double initial_price = 1.0;
    /** @brief The feed's rate, which turns the default half-life into κ. */
    int ticks_per_hour = 3600;
};

/**
 * @brief The converted mixture, and why there is none when there is none.
 */
struct convert_fx_mixture_response {
    bool success = false;
    std::string message;
    /** @brief The converted means, one per component the new engine reads. */
    std::vector<double> gmm_means;
    /** @brief The converted standard deviations. */
    std::vector<double> gmm_stdevs;
    /** @brief The converted weights, κ first under the Ornstein-Uhlenbeck engine. */
    std::vector<double> gmm_weights;
};

}

#endif
