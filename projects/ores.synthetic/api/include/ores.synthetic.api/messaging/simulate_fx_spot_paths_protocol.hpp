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
#ifndef ORES_SYNTHETIC_API_MESSAGING_SIMULATE_FX_SPOT_PATHS_PROTOCOL_HPP
#define ORES_SYNTHETIC_API_MESSAGING_SIMULATE_FX_SPOT_PATHS_PROTOCOL_HPP

#include "ores.synthetic.api/export.hpp"
#include "ores.utility/codec/wire_fields.hpp"
#include "ores.utility/codec/wire_traits_fwd.hpp"
#include <cstddef>
#include <cstdint>
#include <span>
#include <string>
#include <string_view>
#include <tuple>
#include <vector>

namespace ores::synthetic::messaging {

/**
 * @brief Request a batch (dry-run) simulation of FX spot sample paths.
 *
 * Stateless: nothing is persisted or published, and the caller names the
 * process parameters rather than a stored config, so an unsaved editor can
 * preview what it is about to save.
 */
struct simulate_fx_spot_paths_request {
    using response_type = struct simulate_fx_spot_paths_response;
    static constexpr std::string_view nats_subject = "synthetic.v1.ops.simulate_fx_spot_paths";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    /** @brief GMM component means; means, stdevs and weights have equal size. */
    std::vector<double> gmm_means;
    /** @brief GMM component standard deviations. */
    std::vector<double> gmm_stdevs;
    /** @brief GMM component weights. */
    std::vector<double> gmm_weights;
    /**
     * @brief Price-process engine: "geometric" (GBM, log-returns) or
     * "arithmetic" (arithmetic Brownian motion, absolute increments).
     */
    std::string process_type = "geometric";
    /** @brief Starting spot price for every path. */
    double initial_price = 1.0;
    /** @brief Number of ticks (steps) to generate per path. */
    int num_ticks = 100;
    /** @brief Number of independent sample paths to generate (for contrast). */
    int num_paths = 5;
    /** @brief Base RNG seed; path i uses seed + i. Change to reseed. */
    std::uint32_t seed = 1;

    friend bool operator==(const simulate_fx_spot_paths_request&,
                           const simulate_fx_spot_paths_request&) = default;
};

/**
 * @brief The generated batch, and why it is empty when it is.
 */
struct simulate_fx_spot_paths_response {
    bool success = false;
    std::string message;
    /** @brief num_paths series, each of num_ticks prices. */
    std::vector<std::vector<double>> paths;

    friend bool operator==(const simulate_fx_spot_paths_response&,
                           const simulate_fx_spot_paths_response&) = default;
};

/**
 * @brief The members of simulate_fx_spot_paths_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const simulate_fx_spot_paths_request&) {
    return std::tuple{
        ::ores::utility::codec::field{"gmm_means", &simulate_fx_spot_paths_request::gmm_means},
        ::ores::utility::codec::field{"gmm_stdevs", &simulate_fx_spot_paths_request::gmm_stdevs},
        ::ores::utility::codec::field{"gmm_weights", &simulate_fx_spot_paths_request::gmm_weights},
        ::ores::utility::codec::field{"process_type",
                                      &simulate_fx_spot_paths_request::process_type},
        ::ores::utility::codec::field{"initial_price",
                                      &simulate_fx_spot_paths_request::initial_price},
        ::ores::utility::codec::field{"num_ticks", &simulate_fx_spot_paths_request::num_ticks},
        ::ores::utility::codec::field{"num_paths", &simulate_fx_spot_paths_request::num_paths},
        ::ores::utility::codec::field{"seed", &simulate_fx_spot_paths_request::seed},
    };
}

/**
 * @brief The members of simulate_fx_spot_paths_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const simulate_fx_spot_paths_response&) {
    return std::tuple{
        ::ores::utility::codec::field{"success", &simulate_fx_spot_paths_response::success},
        ::ores::utility::codec::field{"message", &simulate_fx_spot_paths_response::message},
        ::ores::utility::codec::field{"paths", &simulate_fx_spot_paths_response::paths},
    };
}

}
namespace ores::utility::codec {

template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::simulate_fx_spot_paths_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::synthetic::messaging::simulate_fx_spot_paths_request& v);
    static ores::synthetic::messaging::simulate_fx_spot_paths_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::synthetic::messaging::simulate_fx_spot_paths_request& v);
    static ores::synthetic::messaging::simulate_fx_spot_paths_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::simulate_fx_spot_paths_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::synthetic::messaging::simulate_fx_spot_paths_response& v);
    static ores::synthetic::messaging::simulate_fx_spot_paths_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::synthetic::messaging::simulate_fx_spot_paths_response& v);
    static ores::synthetic::messaging::simulate_fx_spot_paths_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
