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
#ifndef ORES_SYNTHETIC_API_MESSAGING_FAULT_OPERATION_PROTOCOL_HPP
#define ORES_SYNTHETIC_API_MESSAGING_FAULT_OPERATION_PROTOCOL_HPP

#include "ores.synthetic.api/export.hpp"
#include "ores.utility/codec/wire_fields.hpp"
#include "ores.utility/codec/wire_traits_fwd.hpp"
#include <cstddef>
#include <cstdint>
#include <span>
#include <string>
#include <string_view>
#include <tuple>

namespace ores::synthetic::messaging {

/**
 * @brief Replace the faults the simulator injects.
 */
struct set_faults_request {
    using response_type = struct set_faults_response;
    static constexpr std::string_view nats_subject = "synthetic.v1.ops.set_faults";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    bool connection_lost = false;
    std::uint32_t tick_delay_ms = 0;
    bool repeat_ticks = false;
    bool out_of_range = false;
    bool not_entitled = false;

    friend bool operator==(const set_faults_request&, const set_faults_request&) = default;
};

/**
 * @brief Whether the plan was set, and why not when it was not.
 */
struct set_faults_response {
    bool success = false;
    std::string message;

    friend bool operator==(const set_faults_response&, const set_faults_response&) = default;
};

/**
 * @brief The members of set_faults_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<set_faults_request>) {
    return std::tuple{
        ::ores::utility::codec::field{"connection_lost", &set_faults_request::connection_lost},
        ::ores::utility::codec::field{"tick_delay_ms", &set_faults_request::tick_delay_ms},
        ::ores::utility::codec::field{"repeat_ticks", &set_faults_request::repeat_ticks},
        ::ores::utility::codec::field{"out_of_range", &set_faults_request::out_of_range},
        ::ores::utility::codec::field{"not_entitled", &set_faults_request::not_entitled},
    };
}

/**
 * @brief The members of set_faults_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<set_faults_response>) {
    return std::tuple{
        ::ores::utility::codec::field{"success", &set_faults_response::success},
        ::ores::utility::codec::field{"message", &set_faults_response::message},
    };
}

}
namespace ores::utility::codec {

template <>
struct ORES_SYNTHETIC_API_EXPORT wire_traits<ores::synthetic::messaging::set_faults_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::synthetic::messaging::set_faults_request& v);
    static ores::synthetic::messaging::set_faults_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::synthetic::messaging::set_faults_request& v);
    static ores::synthetic::messaging::set_faults_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT wire_traits<ores::synthetic::messaging::set_faults_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::synthetic::messaging::set_faults_response& v);
    static ores::synthetic::messaging::set_faults_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::synthetic::messaging::set_faults_response& v);
    static ores::synthetic::messaging::set_faults_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
