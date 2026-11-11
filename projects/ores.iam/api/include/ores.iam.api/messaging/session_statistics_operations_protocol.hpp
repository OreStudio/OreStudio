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
#ifndef ORES_IAM_API_MESSAGING_SESSION_STATISTICS_OPERATIONS_PROTOCOL_HPP
#define ORES_IAM_API_MESSAGING_SESSION_STATISTICS_OPERATIONS_PROTOCOL_HPP

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
 * @brief One day's session statistics, for one account.
 *
 * The duration is in seconds and the byte counts are totals over the day's
 * ended sessions. A day with no ended sessions has no row.
 */
struct session_statistics_row {
    std::string day;
    std::string account_id;
    std::uint64_t session_count = 0;
    double avg_duration_seconds = 0.0;
    std::uint64_t total_bytes_sent = 0;
    std::uint64_t total_bytes_received = 0;
    double avg_bytes_sent = 0.0;
    double avg_bytes_received = 0.0;
    std::uint64_t unique_countries = 0;

    friend bool operator==(const session_statistics_row&, const session_statistics_row&) = default;
};

/**
 * @brief A window over the caller's tenant session statistics.
 *
 * A bound the caller states no value for is null, and the read then applies no
 * bound on that side. The window is newest first, so the screen reads the most
 * recent days without asking for an order.
 */
struct get_session_statistics_request {
    using response_type = struct get_session_statistics_response;
    static constexpr std::string_view nats_subject = "iam.v1.ops.get_session_statistics";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::string account_id;
    std::optional<std::chrono::system_clock::time_point> from_time;
    std::optional<std::chrono::system_clock::time_point> to_time;
    std::uint32_t limit = 100;
    std::uint32_t offset = 0;

    friend bool operator==(const get_session_statistics_request&,
                           const get_session_statistics_request&) = default;
};

struct get_session_statistics_response {
    std::vector<session_statistics_row> rows;
    bool success = false;
    std::string message;

    friend bool operator==(const get_session_statistics_response&,
                           const get_session_statistics_response&) = default;
};

/**
 * @brief The members of session_statistics_row, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<session_statistics_row>) {
    return ::ores::utility::codec::field_list{
        ::ores::utility::codec::field{"day", &session_statistics_row::day},
        ::ores::utility::codec::field{"account_id", &session_statistics_row::account_id},
        ::ores::utility::codec::field{"session_count", &session_statistics_row::session_count},
        ::ores::utility::codec::field{"avg_duration_seconds",
                                      &session_statistics_row::avg_duration_seconds},
        ::ores::utility::codec::field{"total_bytes_sent",
                                      &session_statistics_row::total_bytes_sent},
        ::ores::utility::codec::field{"total_bytes_received",
                                      &session_statistics_row::total_bytes_received},
        ::ores::utility::codec::field{"avg_bytes_sent", &session_statistics_row::avg_bytes_sent},
        ::ores::utility::codec::field{"avg_bytes_received",
                                      &session_statistics_row::avg_bytes_received},
        ::ores::utility::codec::field{"unique_countries",
                                      &session_statistics_row::unique_countries},
    };
}

/**
 * @brief The members of get_session_statistics_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_session_statistics_request>) {
    return ::ores::utility::codec::field_list{
        ::ores::utility::codec::field{"account_id", &get_session_statistics_request::account_id},
        ::ores::utility::codec::field{"from_time", &get_session_statistics_request::from_time},
        ::ores::utility::codec::field{"to_time", &get_session_statistics_request::to_time},
        ::ores::utility::codec::field{"limit", &get_session_statistics_request::limit},
        ::ores::utility::codec::field{"offset", &get_session_statistics_request::offset},
    };
}

/**
 * @brief The members of get_session_statistics_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_session_statistics_response>) {
    return ::ores::utility::codec::field_list{
        ::ores::utility::codec::field{"rows", &get_session_statistics_response::rows},
        ::ores::utility::codec::field{"success", &get_session_statistics_response::success},
        ::ores::utility::codec::field{"message", &get_session_statistics_response::message},
    };
}

}
namespace ores::utility::codec {

template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::session_statistics_row> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::session_statistics_row& v);
    static ores::iam::messaging::session_statistics_row read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::session_statistics_row& v);
    static ores::iam::messaging::session_statistics_row
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::get_session_statistics_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::get_session_statistics_request& v);
    static ores::iam::messaging::get_session_statistics_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::get_session_statistics_request& v);
    static ores::iam::messaging::get_session_statistics_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::get_session_statistics_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::get_session_statistics_response& v);
    static ores::iam::messaging::get_session_statistics_response read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::iam::messaging::get_session_statistics_response& v);
    static ores::iam::messaging::get_session_statistics_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
