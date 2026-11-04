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
#ifndef ORES_MARKETDATA_API_MESSAGING_OBSERVATION_READ_OPERATIONS_PROTOCOL_HPP
#define ORES_MARKETDATA_API_MESSAGING_OBSERVATION_READ_OPERATIONS_PROTOCOL_HPP

#include "ores.marketdata.api/export.hpp"
#include "ores.utility/codec/wire_fields.hpp"
#include "ores.utility/codec/wire_traits_fwd.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <chrono>
#include <cstddef>
#include <span>
#include <string>
#include <string_view>
#include <tuple>
#include <vector>

namespace ores::marketdata::messaging {

/**
 * @brief Asks for every observation of a series in the days up to a date.
 */
struct get_observation_window_request {
    using response_type = struct get_observation_window_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.ops.get_observation_window";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    /**
     * @brief The series' oresmd URI.
     */
    std::string item;
    /**
     * @brief The provider to read, or empty for the one provider that carries the series.
     *
     * A vendor and the derived source can both carry one series. A series that more than
     * one provider carries is refused as ambiguous unless the request names one.
     */
    std::string provider_code;
    /**
     * @brief Whose observations to read, or empty for the parties the caller can see.
     *
     * An observation is entitled by party, as a fixing is: a service names the party
     * whose data it needs, and a session that sees no party is refused.
     */
    std::string party_id;
    /**
     * @brief The last date of the window, included.
     */
    std::chrono::year_month_day to;
    /**
     * @brief How many calendar days the window spans, ending on the last date.
     */
    int days = 0;

    friend bool operator==(const get_observation_window_request&,
                           const get_observation_window_request&) = default;
};

struct get_observation_window_response {
    /**
     * @brief How the call ended: a session with no party, an ambiguous series, and a
     * window that is empty or over the ceiling are refused here.
     */
    ores::utility::domain::result result;
    /**
     * @brief The provider of the series.
     */
    std::string provider_code;
    /**
     * @brief How many calendar days the window spans.
     */
    int days = 0;
    /**
     * @brief How many observations the window holds.
     */
    int used = 0;
    /**
     * @brief The time each observation is for, oldest first.
     */
    std::vector<std::chrono::system_clock::time_point> observed_at;
    /**
     * @brief The oresmd URI of each observation's point of the series.
     */
    std::vector<std::string> coordinates;
    /**
     * @brief The value of each observation, in decimal text.
     */
    std::vector<std::string> values;
    /**
     * @brief The source of each observation.
     */
    std::vector<std::string> sources;

    friend bool operator==(const get_observation_window_response&,
                           const get_observation_window_response&) = default;
};

/**
 * @brief The members of get_observation_window_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_observation_window_request&) {
    return std::tuple{
        ::ores::utility::codec::field{"item", &get_observation_window_request::item},
        ::ores::utility::codec::field{"provider_code",
                                      &get_observation_window_request::provider_code},
        ::ores::utility::codec::field{"party_id", &get_observation_window_request::party_id},
        ::ores::utility::codec::field{"to", &get_observation_window_request::to},
        ::ores::utility::codec::field{"days", &get_observation_window_request::days},
    };
}

/**
 * @brief The members of get_observation_window_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_observation_window_response&) {
    return std::tuple{
        ::ores::utility::codec::field{"result", &get_observation_window_response::result},
        ::ores::utility::codec::field{"provider_code",
                                      &get_observation_window_response::provider_code},
        ::ores::utility::codec::field{"days", &get_observation_window_response::days},
        ::ores::utility::codec::field{"used", &get_observation_window_response::used},
        ::ores::utility::codec::field{"observed_at", &get_observation_window_response::observed_at},
        ::ores::utility::codec::field{"coordinates", &get_observation_window_response::coordinates},
        ::ores::utility::codec::field{"values", &get_observation_window_response::values},
        ::ores::utility::codec::field{"sources", &get_observation_window_response::sources},
    };
}

}
namespace ores::utility::codec {

template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::get_observation_window_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::get_observation_window_request& v);
    static ores::marketdata::messaging::get_observation_window_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::get_observation_window_request& v);
    static ores::marketdata::messaging::get_observation_window_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::get_observation_window_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::get_observation_window_response& v);
    static ores::marketdata::messaging::get_observation_window_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::get_observation_window_response& v);
    static ores::marketdata::messaging::get_observation_window_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
