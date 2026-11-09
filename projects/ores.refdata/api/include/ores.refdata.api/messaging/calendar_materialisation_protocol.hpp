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
#ifndef ORES_REFDATA_API_MESSAGING_CALENDAR_MATERIALISATION_PROTOCOL_HPP
#define ORES_REFDATA_API_MESSAGING_CALENDAR_MATERIALISATION_PROTOCOL_HPP

#include "ores.refdata.api/export.hpp"
#include "ores.utility/codec/wire_fields.hpp"
#include "ores.utility/codec/wire_traits_fwd.hpp"
#include <cstddef>
#include <cstdint>
#include <optional>
#include <span>
#include <string>
#include <string_view>
#include <tuple>

namespace ores::refdata::messaging {

/**
 * @brief On-demand "regenerate up to <year>" command for calendar_dates.
 *
 * Extends (never rewrites below the existing watermark) a single
 * calendar's materialised calendar_dates, or every calendar's, up to
 * end_year. No cron job triggers this -- it is user- or save-action-
 * triggered only.
 */
struct regenerate_calendar_dates_request {
    using response_type = struct regenerate_calendar_dates_response;
    static constexpr std::string_view nats_subject = "refdata.v1.ops.regenerate_calendar_dates";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::optional<std::string> calendar_code;
    std::optional<int> end_year;
    bool in_use_only = false;

    friend bool operator==(const regenerate_calendar_dates_request&,
                           const regenerate_calendar_dates_request&) = default;
};

struct regenerate_calendar_dates_response {
    bool success = false;
    std::string message;
    std::uint64_t rows_written = 0;

    friend bool operator==(const regenerate_calendar_dates_response&,
                           const regenerate_calendar_dates_response&) = default;
};

/**
 * @brief The members of regenerate_calendar_dates_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<regenerate_calendar_dates_request>) {
    return std::tuple{
        ::ores::utility::codec::field{"calendar_code",
                                      &regenerate_calendar_dates_request::calendar_code},
        ::ores::utility::codec::field{"end_year", &regenerate_calendar_dates_request::end_year},
        ::ores::utility::codec::field{"in_use_only",
                                      &regenerate_calendar_dates_request::in_use_only},
    };
}

/**
 * @brief The members of regenerate_calendar_dates_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<regenerate_calendar_dates_response>) {
    return std::tuple{
        ::ores::utility::codec::field{"success", &regenerate_calendar_dates_response::success},
        ::ores::utility::codec::field{"message", &regenerate_calendar_dates_response::message},
        ::ores::utility::codec::field{"rows_written",
                                      &regenerate_calendar_dates_response::rows_written},
    };
}

}
namespace ores::utility::codec {

template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::regenerate_calendar_dates_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::regenerate_calendar_dates_request& v);
    static ores::refdata::messaging::regenerate_calendar_dates_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::regenerate_calendar_dates_request& v);
    static ores::refdata::messaging::regenerate_calendar_dates_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::regenerate_calendar_dates_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::regenerate_calendar_dates_response& v);
    static ores::refdata::messaging::regenerate_calendar_dates_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::regenerate_calendar_dates_response& v);
    static ores::refdata::messaging::regenerate_calendar_dates_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
