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
#ifndef ORES_REFDATA_API_MESSAGING_CALENDAR_EXCEPTION_PROTOCOL_HPP
#define ORES_REFDATA_API_MESSAGING_CALENDAR_EXCEPTION_PROTOCOL_HPP

#include "ores.refdata.api/domain/calendar_exception.hpp"
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

struct calendar_exception_key {
    boost::uuids::uuid id;
};

struct calendar_exception_write {
    boost::uuids::uuid id;
    std::string calendar_code;
    std::chrono::year_month_day exception_date;
    bool is_business_day;
    std::optional<std::string> description;
};

struct calendar_exception_change {
    calendar_exception_write write;
    ores::utility::domain::precondition precondition;
};

struct calendar_exception_removal {
    calendar_exception_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;
};

struct calendar_exception_lookup {
    calendar_exception_key key;
    std::optional<ores::refdata::domain::calendar_exception> calendar_exception;
};

struct calendar_exceptions_filter {
    std::optional<std::string> calendar_code;
    std::optional<std::vector<boost::uuids::uuid>> id_one_of;
    std::optional<std::vector<std::string>> calendar_code_one_of;
};

struct calendar_exception_event {
    boost::uuids::uuid event_id;
    calendar_exception_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;
};

struct calendar_exception_version_key {
    calendar_exception_key calendar_exception;
    std::uint32_t version;
};

struct calendar_exception_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;
};

struct list_calendar_exceptions_request {
    using response_type = struct list_calendar_exceptions_response;
    static constexpr std::string_view nats_subject = "refdata.v1.calendar_exceptions.list";
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
    std::optional<calendar_exceptions_filter> filter;
    std::optional<std::string> as_of;
};

struct list_calendar_exceptions_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::calendar_exception> calendar_exceptions;
    std::uint64_t total;
};

struct get_calendar_exception_request {
    using response_type = struct get_calendar_exception_response;
    static constexpr std::string_view nats_subject = "refdata.v1.calendar_exceptions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    calendar_exception_key key;
};

struct get_calendar_exception_response {
    ores::utility::domain::result result;
    std::optional<ores::refdata::domain::calendar_exception> calendar_exception;
};

struct get_many_calendar_exceptions_request {
    using response_type = struct get_many_calendar_exceptions_response;
    static constexpr std::string_view nats_subject = "refdata.v1.calendar_exceptions.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<calendar_exception_key> keys;
};

struct get_many_calendar_exceptions_response {
    ores::utility::domain::result result;
    std::vector<calendar_exception_lookup> entries;
};

struct put_calendar_exception_request {
    using response_type = struct put_calendar_exception_response;
    static constexpr std::string_view nats_subject = "refdata.v1.calendar_exceptions.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    calendar_exception_change change;
    ores::utility::domain::change_intent intent;
};

struct put_calendar_exception_response {
    ores::utility::domain::result result;
    std::optional<ores::refdata::domain::calendar_exception> calendar_exception;
};

struct put_many_calendar_exceptions_request {
    using response_type = struct put_many_calendar_exceptions_response;
    static constexpr std::string_view nats_subject = "refdata.v1.calendar_exceptions.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<calendar_exception_change> changes;
    ores::utility::domain::change_intent intent;
};

struct put_many_calendar_exceptions_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::calendar_exception> calendar_exceptions;
};

struct delete_calendar_exception_request {
    using response_type = struct delete_calendar_exception_response;
    static constexpr std::string_view nats_subject = "refdata.v1.calendar_exceptions.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    calendar_exception_removal removal;
    ores::utility::domain::change_intent intent;
};

struct delete_calendar_exception_response {
    ores::utility::domain::result result;
};

struct delete_many_calendar_exceptions_request {
    using response_type = struct delete_many_calendar_exceptions_response;
    static constexpr std::string_view nats_subject = "refdata.v1.calendar_exceptions.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<calendar_exception_removal> removals;
    ores::utility::domain::change_intent intent;
};

struct delete_many_calendar_exceptions_response {
    ores::utility::domain::result result;
};

struct list_by_calendar_code_calendar_exceptions_request {
    using response_type = struct list_by_calendar_code_calendar_exceptions_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.calendar_exceptions.list_by_calendar_code";
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
    std::optional<calendar_exceptions_filter> filter;
};

struct list_by_calendar_code_calendar_exceptions_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::calendar_exception> calendar_exceptions;
    std::uint64_t total;
};

struct list_calendar_exception_versions_request {
    using response_type = struct list_calendar_exception_versions_response;
    static constexpr std::string_view nats_subject = "refdata.v1.calendar_exceptions_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    calendar_exception_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<calendar_exception_versions_filter> filter;
};

struct list_calendar_exception_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::calendar_exception> versions;
    std::uint64_t total;
};

struct get_calendar_exception_version_request {
    using response_type = struct get_calendar_exception_version_response;
    static constexpr std::string_view nats_subject = "refdata.v1.calendar_exceptions_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    calendar_exception_version_key key;
};

struct get_calendar_exception_version_response {
    ores::utility::domain::result result;
    std::optional<ores::refdata::domain::calendar_exception> version;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace calendar_exception_event_subjects {
inline constexpr std::string_view created = "refdata.v1.calendar_exceptions_events.created";
inline constexpr std::string_view updated = "refdata.v1.calendar_exceptions_events.updated";
inline constexpr std::string_view deleted = "refdata.v1.calendar_exceptions_events.deleted";
}
/**
 * @brief The members of calendar_exception_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const calendar_exception_key&) {
    return std::tuple{
        utility::codec::field{"id", &calendar_exception_key::id},
    };
}

/**
 * @brief The members of calendar_exception_write, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const calendar_exception_write&) {
    return std::tuple{
        utility::codec::field{"id", &calendar_exception_write::id},
        utility::codec::field{"calendar_code", &calendar_exception_write::calendar_code},
        utility::codec::field{"exception_date", &calendar_exception_write::exception_date},
        utility::codec::field{"is_business_day", &calendar_exception_write::is_business_day},
        utility::codec::field{"description", &calendar_exception_write::description},
    };
}

/**
 * @brief The members of calendar_exception_change, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const calendar_exception_change&) {
    return std::tuple{
        utility::codec::field{"write", &calendar_exception_change::write},
        utility::codec::field{"precondition", &calendar_exception_change::precondition},
    };
}

/**
 * @brief The members of calendar_exception_removal, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const calendar_exception_removal&) {
    return std::tuple{
        utility::codec::field{"key", &calendar_exception_removal::key},
        utility::codec::field{"precondition", &calendar_exception_removal::precondition},
    };
}

/**
 * @brief The members of calendar_exception_lookup, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const calendar_exception_lookup&) {
    return std::tuple{
        utility::codec::field{"key", &calendar_exception_lookup::key},
        utility::codec::field{"calendar_exception", &calendar_exception_lookup::calendar_exception},
    };
}

/**
 * @brief The members of calendar_exceptions_filter, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const calendar_exceptions_filter&) {
    return std::tuple{
        utility::codec::field{"calendar_code", &calendar_exceptions_filter::calendar_code},
        utility::codec::field{"id_one_of", &calendar_exceptions_filter::id_one_of},
        utility::codec::field{"calendar_code_one_of",
                              &calendar_exceptions_filter::calendar_code_one_of},
    };
}

/**
 * @brief The members of calendar_exception_event, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const calendar_exception_event&) {
    return std::tuple{
        utility::codec::field{"event_id", &calendar_exception_event::event_id},
        utility::codec::field{"key", &calendar_exception_event::key},
        utility::codec::field{"action", &calendar_exception_event::action},
        utility::codec::field{"version", &calendar_exception_event::version},
        utility::codec::field{"occurred_at", &calendar_exception_event::occurred_at},
        utility::codec::field{"correlation_id", &calendar_exception_event::correlation_id},
    };
}

/**
 * @brief The members of calendar_exception_version_key, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const calendar_exception_version_key&) {
    return std::tuple{
        utility::codec::field{"calendar_exception",
                              &calendar_exception_version_key::calendar_exception},
        utility::codec::field{"version", &calendar_exception_version_key::version},
    };
}

/**
 * @brief The members of calendar_exception_versions_filter, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const calendar_exception_versions_filter&) {
    return std::tuple{
        utility::codec::field{"version", &calendar_exception_versions_filter::version},
        utility::codec::field{"from_version", &calendar_exception_versions_filter::from_version},
        utility::codec::field{"to_version", &calendar_exception_versions_filter::to_version},
    };
}

/**
 * @brief The members of list_calendar_exceptions_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_calendar_exceptions_request&) {
    return std::tuple{
        utility::codec::field{"offset", &list_calendar_exceptions_request::offset},
        utility::codec::field{"limit", &list_calendar_exceptions_request::limit},
        utility::codec::field{"order", &list_calendar_exceptions_request::order},
        utility::codec::field{"filter", &list_calendar_exceptions_request::filter},
        utility::codec::field{"as_of", &list_calendar_exceptions_request::as_of},
    };
}

/**
 * @brief The members of list_calendar_exceptions_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_calendar_exceptions_response&) {
    return std::tuple{
        utility::codec::field{"result", &list_calendar_exceptions_response::result},
        utility::codec::field{"calendar_exceptions",
                              &list_calendar_exceptions_response::calendar_exceptions},
        utility::codec::field{"total", &list_calendar_exceptions_response::total},
    };
}

/**
 * @brief The members of get_calendar_exception_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_calendar_exception_request&) {
    return std::tuple{
        utility::codec::field{"key", &get_calendar_exception_request::key},
    };
}

/**
 * @brief The members of get_calendar_exception_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_calendar_exception_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_calendar_exception_response::result},
        utility::codec::field{"calendar_exception",
                              &get_calendar_exception_response::calendar_exception},
    };
}

/**
 * @brief The members of get_many_calendar_exceptions_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_many_calendar_exceptions_request&) {
    return std::tuple{
        utility::codec::field{"keys", &get_many_calendar_exceptions_request::keys},
    };
}

/**
 * @brief The members of get_many_calendar_exceptions_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_many_calendar_exceptions_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_many_calendar_exceptions_response::result},
        utility::codec::field{"entries", &get_many_calendar_exceptions_response::entries},
    };
}

/**
 * @brief The members of put_calendar_exception_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_calendar_exception_request&) {
    return std::tuple{
        utility::codec::field{"change", &put_calendar_exception_request::change},
        utility::codec::field{"intent", &put_calendar_exception_request::intent},
    };
}

/**
 * @brief The members of put_calendar_exception_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_calendar_exception_response&) {
    return std::tuple{
        utility::codec::field{"result", &put_calendar_exception_response::result},
        utility::codec::field{"calendar_exception",
                              &put_calendar_exception_response::calendar_exception},
    };
}

/**
 * @brief The members of put_many_calendar_exceptions_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_many_calendar_exceptions_request&) {
    return std::tuple{
        utility::codec::field{"changes", &put_many_calendar_exceptions_request::changes},
        utility::codec::field{"intent", &put_many_calendar_exceptions_request::intent},
    };
}

/**
 * @brief The members of put_many_calendar_exceptions_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_many_calendar_exceptions_response&) {
    return std::tuple{
        utility::codec::field{"result", &put_many_calendar_exceptions_response::result},
        utility::codec::field{"calendar_exceptions",
                              &put_many_calendar_exceptions_response::calendar_exceptions},
    };
}

/**
 * @brief The members of delete_calendar_exception_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_calendar_exception_request&) {
    return std::tuple{
        utility::codec::field{"removal", &delete_calendar_exception_request::removal},
        utility::codec::field{"intent", &delete_calendar_exception_request::intent},
    };
}

/**
 * @brief The members of delete_calendar_exception_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_calendar_exception_response&) {
    return std::tuple{
        utility::codec::field{"result", &delete_calendar_exception_response::result},
    };
}

/**
 * @brief The members of delete_many_calendar_exceptions_request, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_many_calendar_exceptions_request&) {
    return std::tuple{
        utility::codec::field{"removals", &delete_many_calendar_exceptions_request::removals},
        utility::codec::field{"intent", &delete_many_calendar_exceptions_request::intent},
    };
}

/**
 * @brief The members of delete_many_calendar_exceptions_response, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_many_calendar_exceptions_response&) {
    return std::tuple{
        utility::codec::field{"result", &delete_many_calendar_exceptions_response::result},
    };
}

/**
 * @brief The members of list_by_calendar_code_calendar_exceptions_request, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_by_calendar_code_calendar_exceptions_request&) {
    return std::tuple{
        utility::codec::field{"calendar_code",
                              &list_by_calendar_code_calendar_exceptions_request::calendar_code},
        utility::codec::field{"scope", &list_by_calendar_code_calendar_exceptions_request::scope},
        utility::codec::field{"offset", &list_by_calendar_code_calendar_exceptions_request::offset},
        utility::codec::field{"limit", &list_by_calendar_code_calendar_exceptions_request::limit},
        utility::codec::field{"order", &list_by_calendar_code_calendar_exceptions_request::order},
        utility::codec::field{"filter", &list_by_calendar_code_calendar_exceptions_request::filter},
    };
}

/**
 * @brief The members of list_by_calendar_code_calendar_exceptions_response, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(const list_by_calendar_code_calendar_exceptions_response&) {
    return std::tuple{
        utility::codec::field{"result",
                              &list_by_calendar_code_calendar_exceptions_response::result},
        utility::codec::field{
            "calendar_exceptions",
            &list_by_calendar_code_calendar_exceptions_response::calendar_exceptions},
        utility::codec::field{"total", &list_by_calendar_code_calendar_exceptions_response::total},
    };
}

/**
 * @brief The members of list_calendar_exception_versions_request, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_calendar_exception_versions_request&) {
    return std::tuple{
        utility::codec::field{"key", &list_calendar_exception_versions_request::key},
        utility::codec::field{"offset", &list_calendar_exception_versions_request::offset},
        utility::codec::field{"limit", &list_calendar_exception_versions_request::limit},
        utility::codec::field{"order", &list_calendar_exception_versions_request::order},
        utility::codec::field{"filter", &list_calendar_exception_versions_request::filter},
    };
}

/**
 * @brief The members of list_calendar_exception_versions_response, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_calendar_exception_versions_response&) {
    return std::tuple{
        utility::codec::field{"result", &list_calendar_exception_versions_response::result},
        utility::codec::field{"versions", &list_calendar_exception_versions_response::versions},
        utility::codec::field{"total", &list_calendar_exception_versions_response::total},
    };
}

/**
 * @brief The members of get_calendar_exception_version_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_calendar_exception_version_request&) {
    return std::tuple{
        utility::codec::field{"key", &get_calendar_exception_version_request::key},
    };
}

/**
 * @brief The members of get_calendar_exception_version_response, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_calendar_exception_version_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_calendar_exception_version_response::result},
        utility::codec::field{"version", &get_calendar_exception_version_response::version},
    };
}


}
namespace ores::utility::codec {

template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::calendar_exception_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::calendar_exception_key& v);
    static ores::refdata::messaging::calendar_exception_key read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::calendar_exception_key& v);
    static ores::refdata::messaging::calendar_exception_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::calendar_exception_write> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::calendar_exception_write& v);
    static ores::refdata::messaging::calendar_exception_write read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::calendar_exception_write& v);
    static ores::refdata::messaging::calendar_exception_write
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::calendar_exception_change> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::calendar_exception_change& v);
    static ores::refdata::messaging::calendar_exception_change read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::calendar_exception_change& v);
    static ores::refdata::messaging::calendar_exception_change
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::calendar_exception_removal> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::calendar_exception_removal& v);
    static ores::refdata::messaging::calendar_exception_removal read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::calendar_exception_removal& v);
    static ores::refdata::messaging::calendar_exception_removal
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::calendar_exception_lookup> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::calendar_exception_lookup& v);
    static ores::refdata::messaging::calendar_exception_lookup read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::calendar_exception_lookup& v);
    static ores::refdata::messaging::calendar_exception_lookup
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::calendar_exceptions_filter> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::calendar_exceptions_filter& v);
    static ores::refdata::messaging::calendar_exceptions_filter read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::calendar_exceptions_filter& v);
    static ores::refdata::messaging::calendar_exceptions_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::calendar_exception_event> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::calendar_exception_event& v);
    static ores::refdata::messaging::calendar_exception_event read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::calendar_exception_event& v);
    static ores::refdata::messaging::calendar_exception_event
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::calendar_exception_version_key> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::calendar_exception_version_key& v);
    static ores::refdata::messaging::calendar_exception_version_key
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::calendar_exception_version_key& v);
    static ores::refdata::messaging::calendar_exception_version_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::calendar_exception_versions_filter> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::calendar_exception_versions_filter& v);
    static ores::refdata::messaging::calendar_exception_versions_filter
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::calendar_exception_versions_filter& v);
    static ores::refdata::messaging::calendar_exception_versions_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::list_calendar_exceptions_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::list_calendar_exceptions_request& v);
    static ores::refdata::messaging::list_calendar_exceptions_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::list_calendar_exceptions_request& v);
    static ores::refdata::messaging::list_calendar_exceptions_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::list_calendar_exceptions_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::list_calendar_exceptions_response& v);
    static ores::refdata::messaging::list_calendar_exceptions_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::list_calendar_exceptions_response& v);
    static ores::refdata::messaging::list_calendar_exceptions_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_calendar_exception_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::get_calendar_exception_request& v);
    static ores::refdata::messaging::get_calendar_exception_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::get_calendar_exception_request& v);
    static ores::refdata::messaging::get_calendar_exception_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_calendar_exception_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::get_calendar_exception_response& v);
    static ores::refdata::messaging::get_calendar_exception_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::get_calendar_exception_response& v);
    static ores::refdata::messaging::get_calendar_exception_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_many_calendar_exceptions_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::get_many_calendar_exceptions_request& v);
    static ores::refdata::messaging::get_many_calendar_exceptions_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::get_many_calendar_exceptions_request& v);
    static ores::refdata::messaging::get_many_calendar_exceptions_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_many_calendar_exceptions_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::get_many_calendar_exceptions_response& v);
    static ores::refdata::messaging::get_many_calendar_exceptions_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::get_many_calendar_exceptions_response& v);
    static ores::refdata::messaging::get_many_calendar_exceptions_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::put_calendar_exception_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::put_calendar_exception_request& v);
    static ores::refdata::messaging::put_calendar_exception_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::put_calendar_exception_request& v);
    static ores::refdata::messaging::put_calendar_exception_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::put_calendar_exception_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::put_calendar_exception_response& v);
    static ores::refdata::messaging::put_calendar_exception_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::put_calendar_exception_response& v);
    static ores::refdata::messaging::put_calendar_exception_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::put_many_calendar_exceptions_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::put_many_calendar_exceptions_request& v);
    static ores::refdata::messaging::put_many_calendar_exceptions_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::put_many_calendar_exceptions_request& v);
    static ores::refdata::messaging::put_many_calendar_exceptions_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::put_many_calendar_exceptions_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::put_many_calendar_exceptions_response& v);
    static ores::refdata::messaging::put_many_calendar_exceptions_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::put_many_calendar_exceptions_response& v);
    static ores::refdata::messaging::put_many_calendar_exceptions_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::delete_calendar_exception_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::delete_calendar_exception_request& v);
    static ores::refdata::messaging::delete_calendar_exception_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::delete_calendar_exception_request& v);
    static ores::refdata::messaging::delete_calendar_exception_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::delete_calendar_exception_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::delete_calendar_exception_response& v);
    static ores::refdata::messaging::delete_calendar_exception_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::delete_calendar_exception_response& v);
    static ores::refdata::messaging::delete_calendar_exception_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::delete_many_calendar_exceptions_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::delete_many_calendar_exceptions_request& v);
    static ores::refdata::messaging::delete_many_calendar_exceptions_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::delete_many_calendar_exceptions_request& v);
    static ores::refdata::messaging::delete_many_calendar_exceptions_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::delete_many_calendar_exceptions_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::delete_many_calendar_exceptions_response& v);
    static ores::refdata::messaging::delete_many_calendar_exceptions_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::delete_many_calendar_exceptions_response& v);
    static ores::refdata::messaging::delete_many_calendar_exceptions_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::list_by_calendar_code_calendar_exceptions_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::refdata::messaging::list_by_calendar_code_calendar_exceptions_request& v);
    static ores::refdata::messaging::list_by_calendar_code_calendar_exceptions_request
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::refdata::messaging::list_by_calendar_code_calendar_exceptions_request& v);
    static ores::refdata::messaging::list_by_calendar_code_calendar_exceptions_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::list_by_calendar_code_calendar_exceptions_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::refdata::messaging::list_by_calendar_code_calendar_exceptions_response& v);
    static ores::refdata::messaging::list_by_calendar_code_calendar_exceptions_response
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::refdata::messaging::list_by_calendar_code_calendar_exceptions_response& v);
    static ores::refdata::messaging::list_by_calendar_code_calendar_exceptions_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::list_calendar_exception_versions_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::list_calendar_exception_versions_request& v);
    static ores::refdata::messaging::list_calendar_exception_versions_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::list_calendar_exception_versions_request& v);
    static ores::refdata::messaging::list_calendar_exception_versions_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::list_calendar_exception_versions_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::list_calendar_exception_versions_response& v);
    static ores::refdata::messaging::list_calendar_exception_versions_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::list_calendar_exception_versions_response& v);
    static ores::refdata::messaging::list_calendar_exception_versions_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_calendar_exception_version_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::get_calendar_exception_version_request& v);
    static ores::refdata::messaging::get_calendar_exception_version_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::get_calendar_exception_version_request& v);
    static ores::refdata::messaging::get_calendar_exception_version_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_calendar_exception_version_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::get_calendar_exception_version_response& v);
    static ores::refdata::messaging::get_calendar_exception_version_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::get_calendar_exception_version_response& v);
    static ores::refdata::messaging::get_calendar_exception_version_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
