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
#ifndef ORES_IAM_API_MESSAGING_SESSION_SAMPLES_PROTOCOL_HPP
#define ORES_IAM_API_MESSAGING_SESSION_SAMPLES_PROTOCOL_HPP

#include "ores.iam.api/export.hpp"
#include "ores.utility/codec/wire_fields.hpp"
#include "ores.utility/codec/wire_traits_fwd.hpp"
#include <cstddef>
#include <cstdint>
#include <span>
#include <string>
#include <string_view>
#include <tuple>
#include <vector>

namespace ores::iam::messaging {

/**
 * @brief Time-series sample for session telemetry.
 */
struct session_sample_dto {
    std::uint64_t sample_time_ms = 0;
    std::uint64_t bytes_sent = 0;
    std::uint64_t bytes_received = 0;

    friend bool operator==(const session_sample_dto&, const session_sample_dto&) = default;
};

struct get_session_samples_request {
    using response_type = struct get_session_samples_response;
    static constexpr std::string_view nats_subject = "iam.v1.ops.get_session_samples";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::string session_id;

    friend bool operator==(const get_session_samples_request&,
                           const get_session_samples_request&) = default;
};

struct get_session_samples_response {
    std::vector<session_sample_dto> samples;
    bool success = false;
    std::string message;

    friend bool operator==(const get_session_samples_response&,
                           const get_session_samples_response&) = default;
};

/**
 * @brief The members of session_sample_dto, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<session_sample_dto>) {
    return std::tuple{
        ::ores::utility::codec::field{"sample_time_ms", &session_sample_dto::sample_time_ms},
        ::ores::utility::codec::field{"bytes_sent", &session_sample_dto::bytes_sent},
        ::ores::utility::codec::field{"bytes_received", &session_sample_dto::bytes_received},
    };
}

/**
 * @brief The members of get_session_samples_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_session_samples_request>) {
    return std::tuple{
        ::ores::utility::codec::field{"session_id", &get_session_samples_request::session_id},
    };
}

/**
 * @brief The members of get_session_samples_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_session_samples_response>) {
    return std::tuple{
        ::ores::utility::codec::field{"samples", &get_session_samples_response::samples},
        ::ores::utility::codec::field{"success", &get_session_samples_response::success},
        ::ores::utility::codec::field{"message", &get_session_samples_response::message},
    };
}

}
namespace ores::utility::codec {

template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::session_sample_dto> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::session_sample_dto& v);
    static ores::iam::messaging::session_sample_dto read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::session_sample_dto& v);
    static ores::iam::messaging::session_sample_dto read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::get_session_samples_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::get_session_samples_request& v);
    static ores::iam::messaging::get_session_samples_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::get_session_samples_request& v);
    static ores::iam::messaging::get_session_samples_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::get_session_samples_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::get_session_samples_response& v);
    static ores::iam::messaging::get_session_samples_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::get_session_samples_response& v);
    static ores::iam::messaging::get_session_samples_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
