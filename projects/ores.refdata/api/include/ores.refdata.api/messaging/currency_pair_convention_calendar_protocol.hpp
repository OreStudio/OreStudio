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
#ifndef ORES_REFDATA_API_MESSAGING_CURRENCY_PAIR_CONVENTION_CALENDAR_PROTOCOL_HPP
#define ORES_REFDATA_API_MESSAGING_CURRENCY_PAIR_CONVENTION_CALENDAR_PROTOCOL_HPP

#include "ores.refdata.api/domain/currency_pair_convention_calendar.hpp"
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

struct currency_pair_convention_calendar_key {
    std::string pair_code;
    std::string calendar_code;
};

struct currency_pair_convention_calendar_write {
    std::string pair_code;
    std::string calendar_code;
};

struct currency_pair_convention_calendar_change {
    currency_pair_convention_calendar_write write;
    ores::utility::domain::precondition precondition;
};

struct currency_pair_convention_calendar_removal {
    currency_pair_convention_calendar_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;
};

struct currency_pair_convention_calendar_lookup {
    currency_pair_convention_calendar_key key;
    std::optional<ores::refdata::domain::currency_pair_convention_calendar>
        currency_pair_convention_calendar;
};

struct currency_pair_convention_calendars_filter {
    std::optional<std::string> pair_code;
    std::optional<std::vector<std::string>> pair_code_one_of;
};

struct currency_pair_convention_calendar_event {
    boost::uuids::uuid event_id;
    currency_pair_convention_calendar_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;
};

struct list_currency_pair_convention_calendars_request {
    using response_type = struct list_currency_pair_convention_calendars_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.currency_pair_convention_calendars.list";
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
    std::optional<currency_pair_convention_calendars_filter> filter;
};

struct list_currency_pair_convention_calendars_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::currency_pair_convention_calendar>
        currency_pair_convention_calendars;
    std::uint64_t total;
};

struct get_currency_pair_convention_calendar_request {
    using response_type = struct get_currency_pair_convention_calendar_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.currency_pair_convention_calendars.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    currency_pair_convention_calendar_key key;
};

struct get_currency_pair_convention_calendar_response {
    ores::utility::domain::result result;
    std::optional<ores::refdata::domain::currency_pair_convention_calendar>
        currency_pair_convention_calendar;
};

struct get_many_currency_pair_convention_calendars_request {
    using response_type = struct get_many_currency_pair_convention_calendars_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.currency_pair_convention_calendars.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<currency_pair_convention_calendar_key> keys;
};

struct get_many_currency_pair_convention_calendars_response {
    ores::utility::domain::result result;
    std::vector<currency_pair_convention_calendar_lookup> entries;
};

struct put_currency_pair_convention_calendar_request {
    using response_type = struct put_currency_pair_convention_calendar_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.currency_pair_convention_calendars.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    currency_pair_convention_calendar_change change;
    ores::utility::domain::change_intent intent;
};

struct put_currency_pair_convention_calendar_response {
    ores::utility::domain::result result;
    std::optional<ores::refdata::domain::currency_pair_convention_calendar>
        currency_pair_convention_calendar;
};

struct put_many_currency_pair_convention_calendars_request {
    using response_type = struct put_many_currency_pair_convention_calendars_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.currency_pair_convention_calendars.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<currency_pair_convention_calendar_change> changes;
    ores::utility::domain::change_intent intent;
};

struct put_many_currency_pair_convention_calendars_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::currency_pair_convention_calendar>
        currency_pair_convention_calendars;
};

struct delete_currency_pair_convention_calendar_request {
    using response_type = struct delete_currency_pair_convention_calendar_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.currency_pair_convention_calendars.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    currency_pair_convention_calendar_removal removal;
    ores::utility::domain::change_intent intent;
};

struct delete_currency_pair_convention_calendar_response {
    ores::utility::domain::result result;
};

struct delete_many_currency_pair_convention_calendars_request {
    using response_type = struct delete_many_currency_pair_convention_calendars_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.currency_pair_convention_calendars.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<currency_pair_convention_calendar_removal> removals;
    ores::utility::domain::change_intent intent;
};

struct delete_many_currency_pair_convention_calendars_response {
    ores::utility::domain::result result;
};

struct list_by_pair_code_currency_pair_convention_calendars_request {
    using response_type = struct list_by_pair_code_currency_pair_convention_calendars_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.currency_pair_convention_calendars.list_by_pair_code";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::string pair_code;
    ores::utility::domain::scope scope = ores::utility::domain::scope::direct;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<currency_pair_convention_calendars_filter> filter;
};

struct list_by_pair_code_currency_pair_convention_calendars_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::currency_pair_convention_calendar>
        currency_pair_convention_calendars;
    std::uint64_t total;
};

/**
 * @brief The members of currency_pair_convention_calendar_key, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const currency_pair_convention_calendar_key&) {
    return std::tuple{
        utility::codec::field{"pair_code", &currency_pair_convention_calendar_key::pair_code},
        utility::codec::field{"calendar_code",
                              &currency_pair_convention_calendar_key::calendar_code},
    };
}

/**
 * @brief The members of currency_pair_convention_calendar_write, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const currency_pair_convention_calendar_write&) {
    return std::tuple{
        utility::codec::field{"pair_code", &currency_pair_convention_calendar_write::pair_code},
        utility::codec::field{"calendar_code",
                              &currency_pair_convention_calendar_write::calendar_code},
    };
}

/**
 * @brief The members of currency_pair_convention_calendar_change, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const currency_pair_convention_calendar_change&) {
    return std::tuple{
        utility::codec::field{"write", &currency_pair_convention_calendar_change::write},
        utility::codec::field{"precondition",
                              &currency_pair_convention_calendar_change::precondition},
    };
}

/**
 * @brief The members of currency_pair_convention_calendar_removal, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const currency_pair_convention_calendar_removal&) {
    return std::tuple{
        utility::codec::field{"key", &currency_pair_convention_calendar_removal::key},
        utility::codec::field{"precondition",
                              &currency_pair_convention_calendar_removal::precondition},
    };
}

/**
 * @brief The members of currency_pair_convention_calendar_lookup, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const currency_pair_convention_calendar_lookup&) {
    return std::tuple{
        utility::codec::field{"key", &currency_pair_convention_calendar_lookup::key},
        utility::codec::field{
            "currency_pair_convention_calendar",
            &currency_pair_convention_calendar_lookup::currency_pair_convention_calendar},
    };
}

/**
 * @brief The members of currency_pair_convention_calendars_filter, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const currency_pair_convention_calendars_filter&) {
    return std::tuple{
        utility::codec::field{"pair_code", &currency_pair_convention_calendars_filter::pair_code},
        utility::codec::field{"pair_code_one_of",
                              &currency_pair_convention_calendars_filter::pair_code_one_of},
    };
}

/**
 * @brief The members of currency_pair_convention_calendar_event, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const currency_pair_convention_calendar_event&) {
    return std::tuple{
        utility::codec::field{"event_id", &currency_pair_convention_calendar_event::event_id},
        utility::codec::field{"key", &currency_pair_convention_calendar_event::key},
        utility::codec::field{"action", &currency_pair_convention_calendar_event::action},
        utility::codec::field{"version", &currency_pair_convention_calendar_event::version},
        utility::codec::field{"occurred_at", &currency_pair_convention_calendar_event::occurred_at},
        utility::codec::field{"correlation_id",
                              &currency_pair_convention_calendar_event::correlation_id},
    };
}

/**
 * @brief The members of list_currency_pair_convention_calendars_request, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_currency_pair_convention_calendars_request&) {
    return std::tuple{
        utility::codec::field{"offset", &list_currency_pair_convention_calendars_request::offset},
        utility::codec::field{"limit", &list_currency_pair_convention_calendars_request::limit},
        utility::codec::field{"order", &list_currency_pair_convention_calendars_request::order},
        utility::codec::field{"filter", &list_currency_pair_convention_calendars_request::filter},
    };
}

/**
 * @brief The members of list_currency_pair_convention_calendars_response, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_currency_pair_convention_calendars_response&) {
    return std::tuple{
        utility::codec::field{"result", &list_currency_pair_convention_calendars_response::result},
        utility::codec::field{
            "currency_pair_convention_calendars",
            &list_currency_pair_convention_calendars_response::currency_pair_convention_calendars},
        utility::codec::field{"total", &list_currency_pair_convention_calendars_response::total},
    };
}

/**
 * @brief The members of get_currency_pair_convention_calendar_request, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_currency_pair_convention_calendar_request&) {
    return std::tuple{
        utility::codec::field{"key", &get_currency_pair_convention_calendar_request::key},
    };
}

/**
 * @brief The members of get_currency_pair_convention_calendar_response, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_currency_pair_convention_calendar_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_currency_pair_convention_calendar_response::result},
        utility::codec::field{
            "currency_pair_convention_calendar",
            &get_currency_pair_convention_calendar_response::currency_pair_convention_calendar},
    };
}

/**
 * @brief The members of get_many_currency_pair_convention_calendars_request, named as the wire
 * names them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(const get_many_currency_pair_convention_calendars_request&) {
    return std::tuple{
        utility::codec::field{"keys", &get_many_currency_pair_convention_calendars_request::keys},
    };
}

/**
 * @brief The members of get_many_currency_pair_convention_calendars_response, named as the wire
 * names them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(const get_many_currency_pair_convention_calendars_response&) {
    return std::tuple{
        utility::codec::field{"result",
                              &get_many_currency_pair_convention_calendars_response::result},
        utility::codec::field{"entries",
                              &get_many_currency_pair_convention_calendars_response::entries},
    };
}

/**
 * @brief The members of put_currency_pair_convention_calendar_request, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_currency_pair_convention_calendar_request&) {
    return std::tuple{
        utility::codec::field{"change", &put_currency_pair_convention_calendar_request::change},
        utility::codec::field{"intent", &put_currency_pair_convention_calendar_request::intent},
    };
}

/**
 * @brief The members of put_currency_pair_convention_calendar_response, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_currency_pair_convention_calendar_response&) {
    return std::tuple{
        utility::codec::field{"result", &put_currency_pair_convention_calendar_response::result},
        utility::codec::field{
            "currency_pair_convention_calendar",
            &put_currency_pair_convention_calendar_response::currency_pair_convention_calendar},
    };
}

/**
 * @brief The members of put_many_currency_pair_convention_calendars_request, named as the wire
 * names them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(const put_many_currency_pair_convention_calendars_request&) {
    return std::tuple{
        utility::codec::field{"changes",
                              &put_many_currency_pair_convention_calendars_request::changes},
        utility::codec::field{"intent",
                              &put_many_currency_pair_convention_calendars_request::intent},
    };
}

/**
 * @brief The members of put_many_currency_pair_convention_calendars_response, named as the wire
 * names them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(const put_many_currency_pair_convention_calendars_response&) {
    return std::tuple{
        utility::codec::field{"result",
                              &put_many_currency_pair_convention_calendars_response::result},
        utility::codec::field{"currency_pair_convention_calendars",
                              &put_many_currency_pair_convention_calendars_response::
                                  currency_pair_convention_calendars},
    };
}

/**
 * @brief The members of delete_currency_pair_convention_calendar_request, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_currency_pair_convention_calendar_request&) {
    return std::tuple{
        utility::codec::field{"removal",
                              &delete_currency_pair_convention_calendar_request::removal},
        utility::codec::field{"intent", &delete_currency_pair_convention_calendar_request::intent},
    };
}

/**
 * @brief The members of delete_currency_pair_convention_calendar_response, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_currency_pair_convention_calendar_response&) {
    return std::tuple{
        utility::codec::field{"result", &delete_currency_pair_convention_calendar_response::result},
    };
}

/**
 * @brief The members of delete_many_currency_pair_convention_calendars_request, named as the wire
 * names them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(const delete_many_currency_pair_convention_calendars_request&) {
    return std::tuple{
        utility::codec::field{"removals",
                              &delete_many_currency_pair_convention_calendars_request::removals},
        utility::codec::field{"intent",
                              &delete_many_currency_pair_convention_calendars_request::intent},
    };
}

/**
 * @brief The members of delete_many_currency_pair_convention_calendars_response, named as the wire
 * names them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(const delete_many_currency_pair_convention_calendars_response&) {
    return std::tuple{
        utility::codec::field{"result",
                              &delete_many_currency_pair_convention_calendars_response::result},
    };
}

/**
 * @brief The members of list_by_pair_code_currency_pair_convention_calendars_request, named as the
 * wire names them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(const list_by_pair_code_currency_pair_convention_calendars_request&) {
    return std::tuple{
        utility::codec::field{
            "pair_code", &list_by_pair_code_currency_pair_convention_calendars_request::pair_code},
        utility::codec::field{"scope",
                              &list_by_pair_code_currency_pair_convention_calendars_request::scope},
        utility::codec::field{
            "offset", &list_by_pair_code_currency_pair_convention_calendars_request::offset},
        utility::codec::field{"limit",
                              &list_by_pair_code_currency_pair_convention_calendars_request::limit},
        utility::codec::field{"order",
                              &list_by_pair_code_currency_pair_convention_calendars_request::order},
        utility::codec::field{
            "filter", &list_by_pair_code_currency_pair_convention_calendars_request::filter},
    };
}

/**
 * @brief The members of list_by_pair_code_currency_pair_convention_calendars_response, named as the
 * wire names them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(const list_by_pair_code_currency_pair_convention_calendars_response&) {
    return std::tuple{
        utility::codec::field{
            "result", &list_by_pair_code_currency_pair_convention_calendars_response::result},
        utility::codec::field{"currency_pair_convention_calendars",
                              &list_by_pair_code_currency_pair_convention_calendars_response::
                                  currency_pair_convention_calendars},
        utility::codec::field{
            "total", &list_by_pair_code_currency_pair_convention_calendars_response::total},
    };
}

}
namespace ores::utility::codec {

template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::currency_pair_convention_calendar_key> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::currency_pair_convention_calendar_key& v);
    static ores::refdata::messaging::currency_pair_convention_calendar_key
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::currency_pair_convention_calendar_key& v);
    static ores::refdata::messaging::currency_pair_convention_calendar_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::currency_pair_convention_calendar_write> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::currency_pair_convention_calendar_write& v);
    static ores::refdata::messaging::currency_pair_convention_calendar_write
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::currency_pair_convention_calendar_write& v);
    static ores::refdata::messaging::currency_pair_convention_calendar_write
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::currency_pair_convention_calendar_change> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::currency_pair_convention_calendar_change& v);
    static ores::refdata::messaging::currency_pair_convention_calendar_change
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::currency_pair_convention_calendar_change& v);
    static ores::refdata::messaging::currency_pair_convention_calendar_change
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::currency_pair_convention_calendar_removal> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::currency_pair_convention_calendar_removal& v);
    static ores::refdata::messaging::currency_pair_convention_calendar_removal
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::currency_pair_convention_calendar_removal& v);
    static ores::refdata::messaging::currency_pair_convention_calendar_removal
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::currency_pair_convention_calendar_lookup> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::currency_pair_convention_calendar_lookup& v);
    static ores::refdata::messaging::currency_pair_convention_calendar_lookup
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::currency_pair_convention_calendar_lookup& v);
    static ores::refdata::messaging::currency_pair_convention_calendar_lookup
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::currency_pair_convention_calendars_filter> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::currency_pair_convention_calendars_filter& v);
    static ores::refdata::messaging::currency_pair_convention_calendars_filter
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::currency_pair_convention_calendars_filter& v);
    static ores::refdata::messaging::currency_pair_convention_calendars_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::currency_pair_convention_calendar_event> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::currency_pair_convention_calendar_event& v);
    static ores::refdata::messaging::currency_pair_convention_calendar_event
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::currency_pair_convention_calendar_event& v);
    static ores::refdata::messaging::currency_pair_convention_calendar_event
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::list_currency_pair_convention_calendars_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::list_currency_pair_convention_calendars_request& v);
    static ores::refdata::messaging::list_currency_pair_convention_calendars_request
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::refdata::messaging::list_currency_pair_convention_calendars_request& v);
    static ores::refdata::messaging::list_currency_pair_convention_calendars_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::list_currency_pair_convention_calendars_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::list_currency_pair_convention_calendars_response& v);
    static ores::refdata::messaging::list_currency_pair_convention_calendars_response
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::refdata::messaging::list_currency_pair_convention_calendars_response& v);
    static ores::refdata::messaging::list_currency_pair_convention_calendars_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_currency_pair_convention_calendar_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::get_currency_pair_convention_calendar_request& v);
    static ores::refdata::messaging::get_currency_pair_convention_calendar_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::get_currency_pair_convention_calendar_request& v);
    static ores::refdata::messaging::get_currency_pair_convention_calendar_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_currency_pair_convention_calendar_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::get_currency_pair_convention_calendar_response& v);
    static ores::refdata::messaging::get_currency_pair_convention_calendar_response
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::refdata::messaging::get_currency_pair_convention_calendar_response& v);
    static ores::refdata::messaging::get_currency_pair_convention_calendar_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_many_currency_pair_convention_calendars_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::refdata::messaging::get_many_currency_pair_convention_calendars_request& v);
    static ores::refdata::messaging::get_many_currency_pair_convention_calendars_request
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::refdata::messaging::get_many_currency_pair_convention_calendars_request& v);
    static ores::refdata::messaging::get_many_currency_pair_convention_calendars_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_many_currency_pair_convention_calendars_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::refdata::messaging::get_many_currency_pair_convention_calendars_response& v);
    static ores::refdata::messaging::get_many_currency_pair_convention_calendars_response
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::refdata::messaging::get_many_currency_pair_convention_calendars_response& v);
    static ores::refdata::messaging::get_many_currency_pair_convention_calendars_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::put_currency_pair_convention_calendar_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::put_currency_pair_convention_calendar_request& v);
    static ores::refdata::messaging::put_currency_pair_convention_calendar_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::put_currency_pair_convention_calendar_request& v);
    static ores::refdata::messaging::put_currency_pair_convention_calendar_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::put_currency_pair_convention_calendar_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::put_currency_pair_convention_calendar_response& v);
    static ores::refdata::messaging::put_currency_pair_convention_calendar_response
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::refdata::messaging::put_currency_pair_convention_calendar_response& v);
    static ores::refdata::messaging::put_currency_pair_convention_calendar_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::put_many_currency_pair_convention_calendars_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::refdata::messaging::put_many_currency_pair_convention_calendars_request& v);
    static ores::refdata::messaging::put_many_currency_pair_convention_calendars_request
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::refdata::messaging::put_many_currency_pair_convention_calendars_request& v);
    static ores::refdata::messaging::put_many_currency_pair_convention_calendars_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::put_many_currency_pair_convention_calendars_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::refdata::messaging::put_many_currency_pair_convention_calendars_response& v);
    static ores::refdata::messaging::put_many_currency_pair_convention_calendars_response
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::refdata::messaging::put_many_currency_pair_convention_calendars_response& v);
    static ores::refdata::messaging::put_many_currency_pair_convention_calendars_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::delete_currency_pair_convention_calendar_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::delete_currency_pair_convention_calendar_request& v);
    static ores::refdata::messaging::delete_currency_pair_convention_calendar_request
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::refdata::messaging::delete_currency_pair_convention_calendar_request& v);
    static ores::refdata::messaging::delete_currency_pair_convention_calendar_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::delete_currency_pair_convention_calendar_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::refdata::messaging::delete_currency_pair_convention_calendar_response& v);
    static ores::refdata::messaging::delete_currency_pair_convention_calendar_response
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::refdata::messaging::delete_currency_pair_convention_calendar_response& v);
    static ores::refdata::messaging::delete_currency_pair_convention_calendar_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::delete_many_currency_pair_convention_calendars_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::refdata::messaging::delete_many_currency_pair_convention_calendars_request& v);
    static ores::refdata::messaging::delete_many_currency_pair_convention_calendars_request
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::refdata::messaging::delete_many_currency_pair_convention_calendars_request& v);
    static ores::refdata::messaging::delete_many_currency_pair_convention_calendars_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::delete_many_currency_pair_convention_calendars_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::refdata::messaging::delete_many_currency_pair_convention_calendars_response& v);
    static ores::refdata::messaging::delete_many_currency_pair_convention_calendars_response
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::refdata::messaging::delete_many_currency_pair_convention_calendars_response& v);
    static ores::refdata::messaging::delete_many_currency_pair_convention_calendars_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<
    ores::refdata::messaging::list_by_pair_code_currency_pair_convention_calendars_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::
                   list_by_pair_code_currency_pair_convention_calendars_request& v);
    static ores::refdata::messaging::list_by_pair_code_currency_pair_convention_calendars_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::
                      list_by_pair_code_currency_pair_convention_calendars_request& v);
    static ores::refdata::messaging::list_by_pair_code_currency_pair_convention_calendars_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<
    ores::refdata::messaging::list_by_pair_code_currency_pair_convention_calendars_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::
                   list_by_pair_code_currency_pair_convention_calendars_response& v);
    static ores::refdata::messaging::list_by_pair_code_currency_pair_convention_calendars_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::
                      list_by_pair_code_currency_pair_convention_calendars_response& v);
    static ores::refdata::messaging::list_by_pair_code_currency_pair_convention_calendars_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
