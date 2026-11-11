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

#include "ores.synthetic.api/export.hpp"
#include "ores.utility/codec/wire_fields.hpp"
#include "ores.utility/codec/wire_traits_fwd.hpp"
#include <cstddef>
#include <span>
#include <string>
#include <string_view>
#include <tuple>
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

    friend bool operator==(const convert_fx_simple_request&,
                           const convert_fx_simple_request&) = default;
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

    friend bool operator==(const convert_fx_simple_response&,
                           const convert_fx_simple_response&) = default;
};

/**
 * @brief The members of convert_fx_simple_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<convert_fx_simple_request>) {
    return ::ores::utility::codec::field_list{
        ::ores::utility::codec::field{"process_type", &convert_fx_simple_request::process_type},
        ::ores::utility::codec::field{"to_simple", &convert_fx_simple_request::to_simple},
        ::ores::utility::codec::field{"initial_price", &convert_fx_simple_request::initial_price},
        ::ores::utility::codec::field{"ticks_per_hour", &convert_fx_simple_request::ticks_per_hour},
        ::ores::utility::codec::field{"gmm_means", &convert_fx_simple_request::gmm_means},
        ::ores::utility::codec::field{"gmm_stdevs", &convert_fx_simple_request::gmm_stdevs},
        ::ores::utility::codec::field{"gmm_weights", &convert_fx_simple_request::gmm_weights},
        ::ores::utility::codec::field{"trend_pct_per_hour",
                                      &convert_fx_simple_request::trend_pct_per_hour},
        ::ores::utility::codec::field{"volatility_pct_per_hour",
                                      &convert_fx_simple_request::volatility_pct_per_hour},
        ::ores::utility::codec::field{"jump_share_pct", &convert_fx_simple_request::jump_share_pct},
        ::ores::utility::codec::field{"half_life_minutes",
                                      &convert_fx_simple_request::half_life_minutes},
    };
}

/**
 * @brief The members of convert_fx_simple_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<convert_fx_simple_response>) {
    return ::ores::utility::codec::field_list{
        ::ores::utility::codec::field{"success", &convert_fx_simple_response::success},
        ::ores::utility::codec::field{"message", &convert_fx_simple_response::message},
        ::ores::utility::codec::field{"has_simple_form",
                                      &convert_fx_simple_response::has_simple_form},
        ::ores::utility::codec::field{"trend_pct_per_hour",
                                      &convert_fx_simple_response::trend_pct_per_hour},
        ::ores::utility::codec::field{"volatility_pct_per_hour",
                                      &convert_fx_simple_response::volatility_pct_per_hour},
        ::ores::utility::codec::field{"jump_share_pct",
                                      &convert_fx_simple_response::jump_share_pct},
        ::ores::utility::codec::field{"half_life_minutes",
                                      &convert_fx_simple_response::half_life_minutes},
        ::ores::utility::codec::field{"gmm_means", &convert_fx_simple_response::gmm_means},
        ::ores::utility::codec::field{"gmm_stdevs", &convert_fx_simple_response::gmm_stdevs},
        ::ores::utility::codec::field{"gmm_weights", &convert_fx_simple_response::gmm_weights},
    };
}

}
namespace ores::utility::codec {

template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::convert_fx_simple_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::synthetic::messaging::convert_fx_simple_request& v);
    static ores::synthetic::messaging::convert_fx_simple_request read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::synthetic::messaging::convert_fx_simple_request& v);
    static ores::synthetic::messaging::convert_fx_simple_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::convert_fx_simple_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::synthetic::messaging::convert_fx_simple_response& v);
    static ores::synthetic::messaging::convert_fx_simple_response read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::synthetic::messaging::convert_fx_simple_response& v);
    static ores::synthetic::messaging::convert_fx_simple_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
