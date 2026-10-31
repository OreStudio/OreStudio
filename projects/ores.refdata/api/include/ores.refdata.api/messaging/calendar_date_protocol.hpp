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
#ifndef ORES_REFDATA_API_MESSAGING_CALENDAR_DATE_PROTOCOL_HPP
#define ORES_REFDATA_API_MESSAGING_CALENDAR_DATE_PROTOCOL_HPP

#include "ores.refdata.api/domain/calendar_date.hpp"
#include "ores.refdata.api/export.hpp"
#include "ores.utility/codec/wire_fields.hpp"
#include "ores.utility/codec/wire_traits_fwd.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <boost/uuid/uuid.hpp>
#include <cstddef>
#include <cstdint>
#include <optional>
#include <span>
#include <string>
#include <tuple>
#include <vector>

namespace ores::refdata::messaging {

struct calendar_date_key {
    std::string calendar_code;
    std::chrono::year_month_day date;
};

struct calendar_date_lookup {
    calendar_date_key key;
    std::optional<ores::refdata::domain::calendar_date> calendar_date;
};

struct calendar_dates_filter {
    std::optional<std::string> calendar_code;
    std::optional<std::vector<std::string>> calendar_code_one_of;
};

struct list_calendar_dates_request {
    using response_type = struct list_calendar_dates_response;
    static constexpr std::string_view nats_subject = "refdata.v1.calendar_dates.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<calendar_dates_filter> filter;
};

struct list_calendar_dates_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::calendar_date> calendar_dates;
    std::uint64_t total;
};

struct get_calendar_date_request {
    using response_type = struct get_calendar_date_response;
    static constexpr std::string_view nats_subject = "refdata.v1.calendar_dates.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    calendar_date_key key;
};

struct get_calendar_date_response {
    ores::utility::domain::result result;
    std::optional<ores::refdata::domain::calendar_date> calendar_date;
};

struct get_many_calendar_dates_request {
    using response_type = struct get_many_calendar_dates_response;
    static constexpr std::string_view nats_subject = "refdata.v1.calendar_dates.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<calendar_date_key> keys;
};

struct get_many_calendar_dates_response {
    ores::utility::domain::result result;
    std::vector<calendar_date_lookup> entries;
};

struct list_by_calendar_code_calendar_dates_request {
    using response_type = struct list_by_calendar_code_calendar_dates_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.calendar_dates.list_by_calendar_code";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::string calendar_code;
    ores::utility::domain::scope scope = ores::utility::domain::scope::direct;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<calendar_dates_filter> filter;
};

struct list_by_calendar_code_calendar_dates_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::calendar_date> calendar_dates;
    std::uint64_t total;
};

/**
 * @brief The members of calendar_date_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const calendar_date_key&) {
    return std::tuple{
        utility::codec::field{"calendar_code", &calendar_date_key::calendar_code},
        utility::codec::field{"date", &calendar_date_key::date},
    };
}

/**
 * @brief The members of calendar_date_lookup, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const calendar_date_lookup&) {
    return std::tuple{
        utility::codec::field{"key", &calendar_date_lookup::key},
        utility::codec::field{"calendar_date", &calendar_date_lookup::calendar_date},
    };
}

/**
 * @brief The members of calendar_dates_filter, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const calendar_dates_filter&) {
    return std::tuple{
        utility::codec::field{"calendar_code", &calendar_dates_filter::calendar_code},
        utility::codec::field{"calendar_code_one_of", &calendar_dates_filter::calendar_code_one_of},
    };
}

/**
 * @brief The members of list_calendar_dates_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_calendar_dates_request&) {
    return std::tuple{
        utility::codec::field{"offset", &list_calendar_dates_request::offset},
        utility::codec::field{"limit", &list_calendar_dates_request::limit},
        utility::codec::field{"order", &list_calendar_dates_request::order},
        utility::codec::field{"filter", &list_calendar_dates_request::filter},
    };
}

/**
 * @brief The members of list_calendar_dates_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_calendar_dates_response&) {
    return std::tuple{
        utility::codec::field{"result", &list_calendar_dates_response::result},
        utility::codec::field{"calendar_dates", &list_calendar_dates_response::calendar_dates},
        utility::codec::field{"total", &list_calendar_dates_response::total},
    };
}

/**
 * @brief The members of get_calendar_date_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_calendar_date_request&) {
    return std::tuple{
        utility::codec::field{"key", &get_calendar_date_request::key},
    };
}

/**
 * @brief The members of get_calendar_date_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_calendar_date_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_calendar_date_response::result},
        utility::codec::field{"calendar_date", &get_calendar_date_response::calendar_date},
    };
}

/**
 * @brief The members of get_many_calendar_dates_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_many_calendar_dates_request&) {
    return std::tuple{
        utility::codec::field{"keys", &get_many_calendar_dates_request::keys},
    };
}

/**
 * @brief The members of get_many_calendar_dates_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_many_calendar_dates_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_many_calendar_dates_response::result},
        utility::codec::field{"entries", &get_many_calendar_dates_response::entries},
    };
}

/**
 * @brief The members of list_by_calendar_code_calendar_dates_request, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_by_calendar_code_calendar_dates_request&) {
    return std::tuple{
        utility::codec::field{"calendar_code",
                              &list_by_calendar_code_calendar_dates_request::calendar_code},
        utility::codec::field{"scope", &list_by_calendar_code_calendar_dates_request::scope},
        utility::codec::field{"offset", &list_by_calendar_code_calendar_dates_request::offset},
        utility::codec::field{"limit", &list_by_calendar_code_calendar_dates_request::limit},
        utility::codec::field{"order", &list_by_calendar_code_calendar_dates_request::order},
        utility::codec::field{"filter", &list_by_calendar_code_calendar_dates_request::filter},
    };
}

/**
 * @brief The members of list_by_calendar_code_calendar_dates_response, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_by_calendar_code_calendar_dates_response&) {
    return std::tuple{
        utility::codec::field{"result", &list_by_calendar_code_calendar_dates_response::result},
        utility::codec::field{"calendar_dates",
                              &list_by_calendar_code_calendar_dates_response::calendar_dates},
        utility::codec::field{"total", &list_by_calendar_code_calendar_dates_response::total},
    };
}

}
namespace ores::utility::codec {

template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::calendar_date_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::calendar_date_key& v);
    static ores::refdata::messaging::calendar_date_key read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::calendar_date_key& v);
    static ores::refdata::messaging::calendar_date_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::calendar_date_lookup> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::calendar_date_lookup& v);
    static ores::refdata::messaging::calendar_date_lookup read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::calendar_date_lookup& v);
    static ores::refdata::messaging::calendar_date_lookup
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::calendar_dates_filter> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::calendar_dates_filter& v);
    static ores::refdata::messaging::calendar_dates_filter read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::calendar_dates_filter& v);
    static ores::refdata::messaging::calendar_dates_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::list_calendar_dates_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::list_calendar_dates_request& v);
    static ores::refdata::messaging::list_calendar_dates_request read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::list_calendar_dates_request& v);
    static ores::refdata::messaging::list_calendar_dates_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::list_calendar_dates_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::list_calendar_dates_response& v);
    static ores::refdata::messaging::list_calendar_dates_response read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::list_calendar_dates_response& v);
    static ores::refdata::messaging::list_calendar_dates_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::get_calendar_date_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::get_calendar_date_request& v);
    static ores::refdata::messaging::get_calendar_date_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::get_calendar_date_request& v);
    static ores::refdata::messaging::get_calendar_date_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::get_calendar_date_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::get_calendar_date_response& v);
    static ores::refdata::messaging::get_calendar_date_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::get_calendar_date_response& v);
    static ores::refdata::messaging::get_calendar_date_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_many_calendar_dates_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::get_many_calendar_dates_request& v);
    static ores::refdata::messaging::get_many_calendar_dates_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::get_many_calendar_dates_request& v);
    static ores::refdata::messaging::get_many_calendar_dates_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_many_calendar_dates_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::get_many_calendar_dates_response& v);
    static ores::refdata::messaging::get_many_calendar_dates_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::get_many_calendar_dates_response& v);
    static ores::refdata::messaging::get_many_calendar_dates_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::list_by_calendar_code_calendar_dates_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::list_by_calendar_code_calendar_dates_request& v);
    static ores::refdata::messaging::list_by_calendar_code_calendar_dates_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::list_by_calendar_code_calendar_dates_request& v);
    static ores::refdata::messaging::list_by_calendar_code_calendar_dates_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::list_by_calendar_code_calendar_dates_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::list_by_calendar_code_calendar_dates_response& v);
    static ores::refdata::messaging::list_by_calendar_code_calendar_dates_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::list_by_calendar_code_calendar_dates_response& v);
    static ores::refdata::messaging::list_by_calendar_code_calendar_dates_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
