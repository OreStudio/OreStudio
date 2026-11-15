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
#ifndef ORES_REFDATA_API_MESSAGING_CALENDAR_ADJUSTMENT_PROTOCOL_HPP
#define ORES_REFDATA_API_MESSAGING_CALENDAR_ADJUSTMENT_PROTOCOL_HPP

#include "ores.refdata.api/export.hpp"
#include "ores.utility/codec/wire_fields.hpp"
#include "ores.utility/codec/wire_traits_fwd.hpp"
#include "ores.utility/uuid/tenant_id.hpp"
#include <chrono>
#include <cstddef>
#include <optional>
#include <span>
#include <string>
#include <string_view>
#include <tuple>
#include <vector>

namespace ores::refdata::messaging {

/**
 * @brief Overrides applied to a named ORE calendar.
 *
 * A calendar adjustment patches a built-in calendar with institution-
 * or date-specific exceptions: additional holidays, and additional
 * business days. Dates are ISO-8601 strings ("YYYY-MM-DD") verbatim
 * from the source file, so a round trip preserves them exactly.
 */
struct calendar_adjustment {
    int version = 0;
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();
    std::string calendar_name;
    std::optional<std::string> base_calendar;
    std::vector<std::string> additional_holidays;
    std::vector<std::string> additional_business_days;
    std::string modified_by;
    std::string change_reason_code;
    std::string change_commentary;
    std::string performed_by;
    std::chrono::system_clock::time_point recorded_at;

    friend bool operator==(const calendar_adjustment&, const calendar_adjustment&) = default;
};

/**
 * @brief Requests the transient calendar_adjustment DTOs ORE needs for
 * source='user' calendar templates, assembled server-side from
 * calendar + calendar_exception rows -- never itself persisted. The
 * caller turns the response into a CalendarAdjustments XML file.
 */
struct get_calendar_adjustments_request {
    using response_type = struct get_calendar_adjustments_response;
    static constexpr std::string_view nats_subject = "refdata.v1.ops.get_calendar_adjustments";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<std::string> calendar_codes;

    friend bool operator==(const get_calendar_adjustments_request&,
                           const get_calendar_adjustments_request&) = default;
};

struct get_calendar_adjustments_response {
    std::vector<calendar_adjustment> adjustments;
    bool success = false;
    std::string message;

    friend bool operator==(const get_calendar_adjustments_response&,
                           const get_calendar_adjustments_response&) = default;
};

/**
 * @brief The members of calendar_adjustment, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<calendar_adjustment>) {
    return ::ores::utility::codec::field_list{
        ::ores::utility::codec::field{"version", &calendar_adjustment::version},
        ::ores::utility::codec::field{"tenant_id", &calendar_adjustment::tenant_id},
        ::ores::utility::codec::field{"calendar_name", &calendar_adjustment::calendar_name},
        ::ores::utility::codec::field{"base_calendar", &calendar_adjustment::base_calendar},
        ::ores::utility::codec::field{"additional_holidays",
                                      &calendar_adjustment::additional_holidays},
        ::ores::utility::codec::field{"additional_business_days",
                                      &calendar_adjustment::additional_business_days},
        ::ores::utility::codec::field{"modified_by", &calendar_adjustment::modified_by},
        ::ores::utility::codec::field{"change_reason_code",
                                      &calendar_adjustment::change_reason_code},
        ::ores::utility::codec::field{"change_commentary", &calendar_adjustment::change_commentary},
        ::ores::utility::codec::field{"performed_by", &calendar_adjustment::performed_by},
        ::ores::utility::codec::field{"recorded_at", &calendar_adjustment::recorded_at},
    };
}

/**
 * @brief The members of get_calendar_adjustments_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_calendar_adjustments_request>) {
    return ::ores::utility::codec::field_list{
        ::ores::utility::codec::field{"calendar_codes",
                                      &get_calendar_adjustments_request::calendar_codes},
    };
}

/**
 * @brief The members of get_calendar_adjustments_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_calendar_adjustments_response>) {
    return ::ores::utility::codec::field_list{
        ::ores::utility::codec::field{"adjustments",
                                      &get_calendar_adjustments_response::adjustments},
        ::ores::utility::codec::field{"success", &get_calendar_adjustments_response::success},
        ::ores::utility::codec::field{"message", &get_calendar_adjustments_response::message},
    };
}

}
namespace ores::utility::codec {

template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::calendar_adjustment> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::calendar_adjustment& v);
    static ores::refdata::messaging::calendar_adjustment read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::calendar_adjustment& v);
    static ores::refdata::messaging::calendar_adjustment
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_calendar_adjustments_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::get_calendar_adjustments_request& v);
    static ores::refdata::messaging::get_calendar_adjustments_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::get_calendar_adjustments_request& v);
    static ores::refdata::messaging::get_calendar_adjustments_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_calendar_adjustments_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::get_calendar_adjustments_response& v);
    static ores::refdata::messaging::get_calendar_adjustments_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::get_calendar_adjustments_response& v);
    static ores::refdata::messaging::get_calendar_adjustments_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
