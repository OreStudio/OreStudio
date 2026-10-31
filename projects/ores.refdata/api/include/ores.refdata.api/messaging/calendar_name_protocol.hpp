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
#ifndef ORES_REFDATA_API_MESSAGING_CALENDAR_NAME_PROTOCOL_HPP
#define ORES_REFDATA_API_MESSAGING_CALENDAR_NAME_PROTOCOL_HPP

#include "ores.refdata.api/domain/calendar_name.hpp"
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

struct calendar_name_key {
    std::string code;
};

struct calendar_name_write {
    std::string code;
    std::string description;
};

struct calendar_name_change {
    calendar_name_write write;
    ores::utility::domain::precondition precondition;
};

struct calendar_name_removal {
    calendar_name_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;
};

struct calendar_name_lookup {
    calendar_name_key key;
    std::optional<ores::refdata::domain::calendar_name> calendar_name;
};

struct calendar_names_filter {
    std::optional<std::vector<std::string>> code_one_of;
};

struct calendar_name_event {
    boost::uuids::uuid event_id;
    calendar_name_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;
};

struct calendar_name_version_key {
    calendar_name_key calendar_name;
    std::uint32_t version;
};

struct calendar_name_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;
};

struct list_calendar_names_request {
    using response_type = struct list_calendar_names_response;
    static constexpr std::string_view nats_subject = "refdata.v1.calendar_names.list";
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
    std::optional<calendar_names_filter> filter;
    std::optional<std::string> as_of;
};

struct list_calendar_names_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::calendar_name> calendar_names;
    std::uint64_t total;
};

struct get_calendar_name_request {
    using response_type = struct get_calendar_name_response;
    static constexpr std::string_view nats_subject = "refdata.v1.calendar_names.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    calendar_name_key key;
};

struct get_calendar_name_response {
    ores::utility::domain::result result;
    std::optional<ores::refdata::domain::calendar_name> calendar_name;
};

struct get_many_calendar_names_request {
    using response_type = struct get_many_calendar_names_response;
    static constexpr std::string_view nats_subject = "refdata.v1.calendar_names.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<calendar_name_key> keys;
};

struct get_many_calendar_names_response {
    ores::utility::domain::result result;
    std::vector<calendar_name_lookup> entries;
};

struct put_calendar_name_request {
    using response_type = struct put_calendar_name_response;
    static constexpr std::string_view nats_subject = "refdata.v1.calendar_names.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    calendar_name_change change;
    ores::utility::domain::change_intent intent;
};

struct put_calendar_name_response {
    ores::utility::domain::result result;
    std::optional<ores::refdata::domain::calendar_name> calendar_name;
};

struct put_many_calendar_names_request {
    using response_type = struct put_many_calendar_names_response;
    static constexpr std::string_view nats_subject = "refdata.v1.calendar_names.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<calendar_name_change> changes;
    ores::utility::domain::change_intent intent;
};

struct put_many_calendar_names_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::calendar_name> calendar_names;
};

struct delete_calendar_name_request {
    using response_type = struct delete_calendar_name_response;
    static constexpr std::string_view nats_subject = "refdata.v1.calendar_names.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    calendar_name_removal removal;
    ores::utility::domain::change_intent intent;
};

struct delete_calendar_name_response {
    ores::utility::domain::result result;
};

struct delete_many_calendar_names_request {
    using response_type = struct delete_many_calendar_names_response;
    static constexpr std::string_view nats_subject = "refdata.v1.calendar_names.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<calendar_name_removal> removals;
    ores::utility::domain::change_intent intent;
};

struct delete_many_calendar_names_response {
    ores::utility::domain::result result;
};

struct list_calendar_name_versions_request {
    using response_type = struct list_calendar_name_versions_response;
    static constexpr std::string_view nats_subject = "refdata.v1.calendar_names_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    calendar_name_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<calendar_name_versions_filter> filter;
};

struct list_calendar_name_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::calendar_name> versions;
    std::uint64_t total;
};

struct get_calendar_name_version_request {
    using response_type = struct get_calendar_name_version_response;
    static constexpr std::string_view nats_subject = "refdata.v1.calendar_names_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    calendar_name_version_key key;
};

struct get_calendar_name_version_response {
    ores::utility::domain::result result;
    std::optional<ores::refdata::domain::calendar_name> version;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace calendar_name_event_subjects {
inline constexpr std::string_view created = "refdata.v1.calendar_names_events.created";
inline constexpr std::string_view updated = "refdata.v1.calendar_names_events.updated";
inline constexpr std::string_view deleted = "refdata.v1.calendar_names_events.deleted";
}
/**
 * @brief The members of calendar_name_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const calendar_name_key&) {
    return std::tuple{
        utility::codec::field{"code", &calendar_name_key::code},
    };
}

/**
 * @brief The members of calendar_name_write, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const calendar_name_write&) {
    return std::tuple{
        utility::codec::field{"code", &calendar_name_write::code},
        utility::codec::field{"description", &calendar_name_write::description},
    };
}

/**
 * @brief The members of calendar_name_change, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const calendar_name_change&) {
    return std::tuple{
        utility::codec::field{"write", &calendar_name_change::write},
        utility::codec::field{"precondition", &calendar_name_change::precondition},
    };
}

/**
 * @brief The members of calendar_name_removal, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const calendar_name_removal&) {
    return std::tuple{
        utility::codec::field{"key", &calendar_name_removal::key},
        utility::codec::field{"precondition", &calendar_name_removal::precondition},
    };
}

/**
 * @brief The members of calendar_name_lookup, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const calendar_name_lookup&) {
    return std::tuple{
        utility::codec::field{"key", &calendar_name_lookup::key},
        utility::codec::field{"calendar_name", &calendar_name_lookup::calendar_name},
    };
}

/**
 * @brief The members of calendar_names_filter, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const calendar_names_filter&) {
    return std::tuple{
        utility::codec::field{"code_one_of", &calendar_names_filter::code_one_of},
    };
}

/**
 * @brief The members of calendar_name_event, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const calendar_name_event&) {
    return std::tuple{
        utility::codec::field{"event_id", &calendar_name_event::event_id},
        utility::codec::field{"key", &calendar_name_event::key},
        utility::codec::field{"action", &calendar_name_event::action},
        utility::codec::field{"version", &calendar_name_event::version},
        utility::codec::field{"occurred_at", &calendar_name_event::occurred_at},
        utility::codec::field{"correlation_id", &calendar_name_event::correlation_id},
    };
}

/**
 * @brief The members of calendar_name_version_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const calendar_name_version_key&) {
    return std::tuple{
        utility::codec::field{"calendar_name", &calendar_name_version_key::calendar_name},
        utility::codec::field{"version", &calendar_name_version_key::version},
    };
}

/**
 * @brief The members of calendar_name_versions_filter, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const calendar_name_versions_filter&) {
    return std::tuple{
        utility::codec::field{"version", &calendar_name_versions_filter::version},
        utility::codec::field{"from_version", &calendar_name_versions_filter::from_version},
        utility::codec::field{"to_version", &calendar_name_versions_filter::to_version},
    };
}

/**
 * @brief The members of list_calendar_names_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_calendar_names_request&) {
    return std::tuple{
        utility::codec::field{"offset", &list_calendar_names_request::offset},
        utility::codec::field{"limit", &list_calendar_names_request::limit},
        utility::codec::field{"order", &list_calendar_names_request::order},
        utility::codec::field{"filter", &list_calendar_names_request::filter},
        utility::codec::field{"as_of", &list_calendar_names_request::as_of},
    };
}

/**
 * @brief The members of list_calendar_names_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_calendar_names_response&) {
    return std::tuple{
        utility::codec::field{"result", &list_calendar_names_response::result},
        utility::codec::field{"calendar_names", &list_calendar_names_response::calendar_names},
        utility::codec::field{"total", &list_calendar_names_response::total},
    };
}

/**
 * @brief The members of get_calendar_name_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_calendar_name_request&) {
    return std::tuple{
        utility::codec::field{"key", &get_calendar_name_request::key},
    };
}

/**
 * @brief The members of get_calendar_name_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_calendar_name_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_calendar_name_response::result},
        utility::codec::field{"calendar_name", &get_calendar_name_response::calendar_name},
    };
}

/**
 * @brief The members of get_many_calendar_names_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_many_calendar_names_request&) {
    return std::tuple{
        utility::codec::field{"keys", &get_many_calendar_names_request::keys},
    };
}

/**
 * @brief The members of get_many_calendar_names_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_many_calendar_names_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_many_calendar_names_response::result},
        utility::codec::field{"entries", &get_many_calendar_names_response::entries},
    };
}

/**
 * @brief The members of put_calendar_name_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_calendar_name_request&) {
    return std::tuple{
        utility::codec::field{"change", &put_calendar_name_request::change},
        utility::codec::field{"intent", &put_calendar_name_request::intent},
    };
}

/**
 * @brief The members of put_calendar_name_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_calendar_name_response&) {
    return std::tuple{
        utility::codec::field{"result", &put_calendar_name_response::result},
        utility::codec::field{"calendar_name", &put_calendar_name_response::calendar_name},
    };
}

/**
 * @brief The members of put_many_calendar_names_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_many_calendar_names_request&) {
    return std::tuple{
        utility::codec::field{"changes", &put_many_calendar_names_request::changes},
        utility::codec::field{"intent", &put_many_calendar_names_request::intent},
    };
}

/**
 * @brief The members of put_many_calendar_names_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_many_calendar_names_response&) {
    return std::tuple{
        utility::codec::field{"result", &put_many_calendar_names_response::result},
        utility::codec::field{"calendar_names", &put_many_calendar_names_response::calendar_names},
    };
}

/**
 * @brief The members of delete_calendar_name_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_calendar_name_request&) {
    return std::tuple{
        utility::codec::field{"removal", &delete_calendar_name_request::removal},
        utility::codec::field{"intent", &delete_calendar_name_request::intent},
    };
}

/**
 * @brief The members of delete_calendar_name_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_calendar_name_response&) {
    return std::tuple{
        utility::codec::field{"result", &delete_calendar_name_response::result},
    };
}

/**
 * @brief The members of delete_many_calendar_names_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_many_calendar_names_request&) {
    return std::tuple{
        utility::codec::field{"removals", &delete_many_calendar_names_request::removals},
        utility::codec::field{"intent", &delete_many_calendar_names_request::intent},
    };
}

/**
 * @brief The members of delete_many_calendar_names_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_many_calendar_names_response&) {
    return std::tuple{
        utility::codec::field{"result", &delete_many_calendar_names_response::result},
    };
}

/**
 * @brief The members of list_calendar_name_versions_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_calendar_name_versions_request&) {
    return std::tuple{
        utility::codec::field{"key", &list_calendar_name_versions_request::key},
        utility::codec::field{"offset", &list_calendar_name_versions_request::offset},
        utility::codec::field{"limit", &list_calendar_name_versions_request::limit},
        utility::codec::field{"order", &list_calendar_name_versions_request::order},
        utility::codec::field{"filter", &list_calendar_name_versions_request::filter},
    };
}

/**
 * @brief The members of list_calendar_name_versions_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_calendar_name_versions_response&) {
    return std::tuple{
        utility::codec::field{"result", &list_calendar_name_versions_response::result},
        utility::codec::field{"versions", &list_calendar_name_versions_response::versions},
        utility::codec::field{"total", &list_calendar_name_versions_response::total},
    };
}

/**
 * @brief The members of get_calendar_name_version_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_calendar_name_version_request&) {
    return std::tuple{
        utility::codec::field{"key", &get_calendar_name_version_request::key},
    };
}

/**
 * @brief The members of get_calendar_name_version_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_calendar_name_version_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_calendar_name_version_response::result},
        utility::codec::field{"version", &get_calendar_name_version_response::version},
    };
}


}
namespace ores::utility::codec {

template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::calendar_name_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::calendar_name_key& v);
    static ores::refdata::messaging::calendar_name_key read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::calendar_name_key& v);
    static ores::refdata::messaging::calendar_name_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::calendar_name_write> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::calendar_name_write& v);
    static ores::refdata::messaging::calendar_name_write read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::calendar_name_write& v);
    static ores::refdata::messaging::calendar_name_write
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::calendar_name_change> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::calendar_name_change& v);
    static ores::refdata::messaging::calendar_name_change read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::calendar_name_change& v);
    static ores::refdata::messaging::calendar_name_change
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::calendar_name_removal> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::calendar_name_removal& v);
    static ores::refdata::messaging::calendar_name_removal read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::calendar_name_removal& v);
    static ores::refdata::messaging::calendar_name_removal
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::calendar_name_lookup> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::calendar_name_lookup& v);
    static ores::refdata::messaging::calendar_name_lookup read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::calendar_name_lookup& v);
    static ores::refdata::messaging::calendar_name_lookup
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::calendar_names_filter> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::calendar_names_filter& v);
    static ores::refdata::messaging::calendar_names_filter read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::calendar_names_filter& v);
    static ores::refdata::messaging::calendar_names_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::calendar_name_event> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::calendar_name_event& v);
    static ores::refdata::messaging::calendar_name_event read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::calendar_name_event& v);
    static ores::refdata::messaging::calendar_name_event
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::calendar_name_version_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::calendar_name_version_key& v);
    static ores::refdata::messaging::calendar_name_version_key read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::calendar_name_version_key& v);
    static ores::refdata::messaging::calendar_name_version_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::calendar_name_versions_filter> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::calendar_name_versions_filter& v);
    static ores::refdata::messaging::calendar_name_versions_filter read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::calendar_name_versions_filter& v);
    static ores::refdata::messaging::calendar_name_versions_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::list_calendar_names_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::list_calendar_names_request& v);
    static ores::refdata::messaging::list_calendar_names_request read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::list_calendar_names_request& v);
    static ores::refdata::messaging::list_calendar_names_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::list_calendar_names_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::list_calendar_names_response& v);
    static ores::refdata::messaging::list_calendar_names_response read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::list_calendar_names_response& v);
    static ores::refdata::messaging::list_calendar_names_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::get_calendar_name_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::get_calendar_name_request& v);
    static ores::refdata::messaging::get_calendar_name_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::get_calendar_name_request& v);
    static ores::refdata::messaging::get_calendar_name_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::get_calendar_name_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::get_calendar_name_response& v);
    static ores::refdata::messaging::get_calendar_name_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::get_calendar_name_response& v);
    static ores::refdata::messaging::get_calendar_name_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_many_calendar_names_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::get_many_calendar_names_request& v);
    static ores::refdata::messaging::get_many_calendar_names_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::get_many_calendar_names_request& v);
    static ores::refdata::messaging::get_many_calendar_names_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_many_calendar_names_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::get_many_calendar_names_response& v);
    static ores::refdata::messaging::get_many_calendar_names_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::get_many_calendar_names_response& v);
    static ores::refdata::messaging::get_many_calendar_names_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::put_calendar_name_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::put_calendar_name_request& v);
    static ores::refdata::messaging::put_calendar_name_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::put_calendar_name_request& v);
    static ores::refdata::messaging::put_calendar_name_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::put_calendar_name_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::put_calendar_name_response& v);
    static ores::refdata::messaging::put_calendar_name_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::put_calendar_name_response& v);
    static ores::refdata::messaging::put_calendar_name_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::put_many_calendar_names_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::put_many_calendar_names_request& v);
    static ores::refdata::messaging::put_many_calendar_names_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::put_many_calendar_names_request& v);
    static ores::refdata::messaging::put_many_calendar_names_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::put_many_calendar_names_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::put_many_calendar_names_response& v);
    static ores::refdata::messaging::put_many_calendar_names_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::put_many_calendar_names_response& v);
    static ores::refdata::messaging::put_many_calendar_names_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::delete_calendar_name_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::delete_calendar_name_request& v);
    static ores::refdata::messaging::delete_calendar_name_request read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::delete_calendar_name_request& v);
    static ores::refdata::messaging::delete_calendar_name_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::delete_calendar_name_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::delete_calendar_name_response& v);
    static ores::refdata::messaging::delete_calendar_name_response read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::delete_calendar_name_response& v);
    static ores::refdata::messaging::delete_calendar_name_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::delete_many_calendar_names_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::delete_many_calendar_names_request& v);
    static ores::refdata::messaging::delete_many_calendar_names_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::delete_many_calendar_names_request& v);
    static ores::refdata::messaging::delete_many_calendar_names_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::delete_many_calendar_names_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::delete_many_calendar_names_response& v);
    static ores::refdata::messaging::delete_many_calendar_names_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::delete_many_calendar_names_response& v);
    static ores::refdata::messaging::delete_many_calendar_names_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::list_calendar_name_versions_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::list_calendar_name_versions_request& v);
    static ores::refdata::messaging::list_calendar_name_versions_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::list_calendar_name_versions_request& v);
    static ores::refdata::messaging::list_calendar_name_versions_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::list_calendar_name_versions_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::list_calendar_name_versions_response& v);
    static ores::refdata::messaging::list_calendar_name_versions_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::list_calendar_name_versions_response& v);
    static ores::refdata::messaging::list_calendar_name_versions_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_calendar_name_version_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::get_calendar_name_version_request& v);
    static ores::refdata::messaging::get_calendar_name_version_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::get_calendar_name_version_request& v);
    static ores::refdata::messaging::get_calendar_name_version_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_calendar_name_version_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::get_calendar_name_version_response& v);
    static ores::refdata::messaging::get_calendar_name_version_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::get_calendar_name_version_response& v);
    static ores::refdata::messaging::get_calendar_name_version_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
