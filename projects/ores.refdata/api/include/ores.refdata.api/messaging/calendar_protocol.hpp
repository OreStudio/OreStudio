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
#ifndef ORES_REFDATA_API_MESSAGING_CALENDAR_PROTOCOL_HPP
#define ORES_REFDATA_API_MESSAGING_CALENDAR_PROTOCOL_HPP

#include "ores.refdata.api/domain/calendar.hpp"
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

struct calendar_key {
    std::string code;
};

struct calendar_write {
    std::string code;
    std::string name;
    std::string calendar_type;
    std::string country_code;
    std::optional<boost::uuids::uuid> image_id;
    std::string source;
    bool is_editable;
    std::optional<std::string> base_calendar_code;
};

struct calendar_change {
    calendar_write write;
    ores::utility::domain::precondition precondition;
};

struct calendar_removal {
    calendar_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;
};

struct calendar_lookup {
    calendar_key key;
    std::optional<ores::refdata::domain::calendar> calendar;
};

struct calendars_filter {
    std::optional<std::vector<std::string>> code_one_of;
    std::optional<std::string> search;
};

struct calendar_event {
    boost::uuids::uuid event_id;
    calendar_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;
};

struct calendar_version_key {
    calendar_key calendar;
    std::uint32_t version;
};

struct calendar_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;
};

struct list_calendars_request {
    using response_type = struct list_calendars_response;
    static constexpr std::string_view nats_subject = "refdata.v1.calendars.list";
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
    std::optional<calendars_filter> filter;
    std::optional<std::string> as_of;
};

struct list_calendars_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::calendar> calendars;
    std::uint64_t total;
};

struct get_calendar_request {
    using response_type = struct get_calendar_response;
    static constexpr std::string_view nats_subject = "refdata.v1.calendars.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    calendar_key key;
};

struct get_calendar_response {
    ores::utility::domain::result result;
    std::optional<ores::refdata::domain::calendar> calendar;
};

struct get_many_calendars_request {
    using response_type = struct get_many_calendars_response;
    static constexpr std::string_view nats_subject = "refdata.v1.calendars.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<calendar_key> keys;
};

struct get_many_calendars_response {
    ores::utility::domain::result result;
    std::vector<calendar_lookup> entries;
};

struct put_calendar_request {
    using response_type = struct put_calendar_response;
    static constexpr std::string_view nats_subject = "refdata.v1.calendars.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    calendar_change change;
    ores::utility::domain::change_intent intent;
};

struct put_calendar_response {
    ores::utility::domain::result result;
    std::optional<ores::refdata::domain::calendar> calendar;
};

struct put_many_calendars_request {
    using response_type = struct put_many_calendars_response;
    static constexpr std::string_view nats_subject = "refdata.v1.calendars.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<calendar_change> changes;
    ores::utility::domain::change_intent intent;
};

struct put_many_calendars_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::calendar> calendars;
};

struct delete_calendar_request {
    using response_type = struct delete_calendar_response;
    static constexpr std::string_view nats_subject = "refdata.v1.calendars.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    calendar_removal removal;
    ores::utility::domain::change_intent intent;
};

struct delete_calendar_response {
    ores::utility::domain::result result;
};

struct delete_many_calendars_request {
    using response_type = struct delete_many_calendars_response;
    static constexpr std::string_view nats_subject = "refdata.v1.calendars.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<calendar_removal> removals;
    ores::utility::domain::change_intent intent;
};

struct delete_many_calendars_response {
    ores::utility::domain::result result;
};

struct list_calendar_versions_request {
    using response_type = struct list_calendar_versions_response;
    static constexpr std::string_view nats_subject = "refdata.v1.calendars_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    calendar_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<calendar_versions_filter> filter;
};

struct list_calendar_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::calendar> versions;
    std::uint64_t total;
};

struct get_calendar_version_request {
    using response_type = struct get_calendar_version_response;
    static constexpr std::string_view nats_subject = "refdata.v1.calendars_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    calendar_version_key key;
};

struct get_calendar_version_response {
    ores::utility::domain::result result;
    std::optional<ores::refdata::domain::calendar> version;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace calendar_event_subjects {
inline constexpr std::string_view created = "refdata.v1.calendars_events.created";
inline constexpr std::string_view updated = "refdata.v1.calendars_events.updated";
inline constexpr std::string_view deleted = "refdata.v1.calendars_events.deleted";
}
/**
 * @brief The members of calendar_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const calendar_key&) {
    return std::tuple{
        utility::codec::field{"code", &calendar_key::code},
    };
}

/**
 * @brief The members of calendar_write, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const calendar_write&) {
    return std::tuple{
        utility::codec::field{"code", &calendar_write::code},
        utility::codec::field{"name", &calendar_write::name},
        utility::codec::field{"calendar_type", &calendar_write::calendar_type},
        utility::codec::field{"country_code", &calendar_write::country_code},
        utility::codec::field{"image_id", &calendar_write::image_id},
        utility::codec::field{"source", &calendar_write::source},
        utility::codec::field{"is_editable", &calendar_write::is_editable},
        utility::codec::field{"base_calendar_code", &calendar_write::base_calendar_code},
    };
}

/**
 * @brief The members of calendar_change, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const calendar_change&) {
    return std::tuple{
        utility::codec::field{"write", &calendar_change::write},
        utility::codec::field{"precondition", &calendar_change::precondition},
    };
}

/**
 * @brief The members of calendar_removal, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const calendar_removal&) {
    return std::tuple{
        utility::codec::field{"key", &calendar_removal::key},
        utility::codec::field{"precondition", &calendar_removal::precondition},
    };
}

/**
 * @brief The members of calendar_lookup, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const calendar_lookup&) {
    return std::tuple{
        utility::codec::field{"key", &calendar_lookup::key},
        utility::codec::field{"calendar", &calendar_lookup::calendar},
    };
}

/**
 * @brief The members of calendars_filter, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const calendars_filter&) {
    return std::tuple{
        utility::codec::field{"code_one_of", &calendars_filter::code_one_of},
        utility::codec::field{"search", &calendars_filter::search},
    };
}

/**
 * @brief The members of calendar_event, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const calendar_event&) {
    return std::tuple{
        utility::codec::field{"event_id", &calendar_event::event_id},
        utility::codec::field{"key", &calendar_event::key},
        utility::codec::field{"action", &calendar_event::action},
        utility::codec::field{"version", &calendar_event::version},
        utility::codec::field{"occurred_at", &calendar_event::occurred_at},
        utility::codec::field{"correlation_id", &calendar_event::correlation_id},
    };
}

/**
 * @brief The members of calendar_version_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const calendar_version_key&) {
    return std::tuple{
        utility::codec::field{"calendar", &calendar_version_key::calendar},
        utility::codec::field{"version", &calendar_version_key::version},
    };
}

/**
 * @brief The members of calendar_versions_filter, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const calendar_versions_filter&) {
    return std::tuple{
        utility::codec::field{"version", &calendar_versions_filter::version},
        utility::codec::field{"from_version", &calendar_versions_filter::from_version},
        utility::codec::field{"to_version", &calendar_versions_filter::to_version},
    };
}

/**
 * @brief The members of list_calendars_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_calendars_request&) {
    return std::tuple{
        utility::codec::field{"offset", &list_calendars_request::offset},
        utility::codec::field{"limit", &list_calendars_request::limit},
        utility::codec::field{"order", &list_calendars_request::order},
        utility::codec::field{"filter", &list_calendars_request::filter},
        utility::codec::field{"as_of", &list_calendars_request::as_of},
    };
}

/**
 * @brief The members of list_calendars_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_calendars_response&) {
    return std::tuple{
        utility::codec::field{"result", &list_calendars_response::result},
        utility::codec::field{"calendars", &list_calendars_response::calendars},
        utility::codec::field{"total", &list_calendars_response::total},
    };
}

/**
 * @brief The members of get_calendar_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_calendar_request&) {
    return std::tuple{
        utility::codec::field{"key", &get_calendar_request::key},
    };
}

/**
 * @brief The members of get_calendar_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_calendar_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_calendar_response::result},
        utility::codec::field{"calendar", &get_calendar_response::calendar},
    };
}

/**
 * @brief The members of get_many_calendars_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_many_calendars_request&) {
    return std::tuple{
        utility::codec::field{"keys", &get_many_calendars_request::keys},
    };
}

/**
 * @brief The members of get_many_calendars_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_many_calendars_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_many_calendars_response::result},
        utility::codec::field{"entries", &get_many_calendars_response::entries},
    };
}

/**
 * @brief The members of put_calendar_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_calendar_request&) {
    return std::tuple{
        utility::codec::field{"change", &put_calendar_request::change},
        utility::codec::field{"intent", &put_calendar_request::intent},
    };
}

/**
 * @brief The members of put_calendar_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_calendar_response&) {
    return std::tuple{
        utility::codec::field{"result", &put_calendar_response::result},
        utility::codec::field{"calendar", &put_calendar_response::calendar},
    };
}

/**
 * @brief The members of put_many_calendars_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_many_calendars_request&) {
    return std::tuple{
        utility::codec::field{"changes", &put_many_calendars_request::changes},
        utility::codec::field{"intent", &put_many_calendars_request::intent},
    };
}

/**
 * @brief The members of put_many_calendars_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_many_calendars_response&) {
    return std::tuple{
        utility::codec::field{"result", &put_many_calendars_response::result},
        utility::codec::field{"calendars", &put_many_calendars_response::calendars},
    };
}

/**
 * @brief The members of delete_calendar_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_calendar_request&) {
    return std::tuple{
        utility::codec::field{"removal", &delete_calendar_request::removal},
        utility::codec::field{"intent", &delete_calendar_request::intent},
    };
}

/**
 * @brief The members of delete_calendar_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_calendar_response&) {
    return std::tuple{
        utility::codec::field{"result", &delete_calendar_response::result},
    };
}

/**
 * @brief The members of delete_many_calendars_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_many_calendars_request&) {
    return std::tuple{
        utility::codec::field{"removals", &delete_many_calendars_request::removals},
        utility::codec::field{"intent", &delete_many_calendars_request::intent},
    };
}

/**
 * @brief The members of delete_many_calendars_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_many_calendars_response&) {
    return std::tuple{
        utility::codec::field{"result", &delete_many_calendars_response::result},
    };
}

/**
 * @brief The members of list_calendar_versions_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_calendar_versions_request&) {
    return std::tuple{
        utility::codec::field{"key", &list_calendar_versions_request::key},
        utility::codec::field{"offset", &list_calendar_versions_request::offset},
        utility::codec::field{"limit", &list_calendar_versions_request::limit},
        utility::codec::field{"order", &list_calendar_versions_request::order},
        utility::codec::field{"filter", &list_calendar_versions_request::filter},
    };
}

/**
 * @brief The members of list_calendar_versions_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_calendar_versions_response&) {
    return std::tuple{
        utility::codec::field{"result", &list_calendar_versions_response::result},
        utility::codec::field{"versions", &list_calendar_versions_response::versions},
        utility::codec::field{"total", &list_calendar_versions_response::total},
    };
}

/**
 * @brief The members of get_calendar_version_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_calendar_version_request&) {
    return std::tuple{
        utility::codec::field{"key", &get_calendar_version_request::key},
    };
}

/**
 * @brief The members of get_calendar_version_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_calendar_version_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_calendar_version_response::result},
        utility::codec::field{"version", &get_calendar_version_response::version},
    };
}


}
namespace ores::utility::codec {

template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::calendar_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::calendar_key& v);
    static ores::refdata::messaging::calendar_key read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::calendar_key& v);
    static ores::refdata::messaging::calendar_key read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::calendar_write> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::calendar_write& v);
    static ores::refdata::messaging::calendar_write read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::calendar_write& v);
    static ores::refdata::messaging::calendar_write read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::calendar_change> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::calendar_change& v);
    static ores::refdata::messaging::calendar_change read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::calendar_change& v);
    static ores::refdata::messaging::calendar_change read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::calendar_removal> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::calendar_removal& v);
    static ores::refdata::messaging::calendar_removal read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::calendar_removal& v);
    static ores::refdata::messaging::calendar_removal read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::calendar_lookup> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::calendar_lookup& v);
    static ores::refdata::messaging::calendar_lookup read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::calendar_lookup& v);
    static ores::refdata::messaging::calendar_lookup read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::calendars_filter> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::calendars_filter& v);
    static ores::refdata::messaging::calendars_filter read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::calendars_filter& v);
    static ores::refdata::messaging::calendars_filter read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::calendar_event> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::calendar_event& v);
    static ores::refdata::messaging::calendar_event read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::calendar_event& v);
    static ores::refdata::messaging::calendar_event read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::calendar_version_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::calendar_version_key& v);
    static ores::refdata::messaging::calendar_version_key read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::calendar_version_key& v);
    static ores::refdata::messaging::calendar_version_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::calendar_versions_filter> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::calendar_versions_filter& v);
    static ores::refdata::messaging::calendar_versions_filter read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::calendar_versions_filter& v);
    static ores::refdata::messaging::calendar_versions_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::list_calendars_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::list_calendars_request& v);
    static ores::refdata::messaging::list_calendars_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::list_calendars_request& v);
    static ores::refdata::messaging::list_calendars_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::list_calendars_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::list_calendars_response& v);
    static ores::refdata::messaging::list_calendars_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::list_calendars_response& v);
    static ores::refdata::messaging::list_calendars_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::get_calendar_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::get_calendar_request& v);
    static ores::refdata::messaging::get_calendar_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::get_calendar_request& v);
    static ores::refdata::messaging::get_calendar_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::get_calendar_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::get_calendar_response& v);
    static ores::refdata::messaging::get_calendar_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::get_calendar_response& v);
    static ores::refdata::messaging::get_calendar_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::get_many_calendars_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::get_many_calendars_request& v);
    static ores::refdata::messaging::get_many_calendars_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::get_many_calendars_request& v);
    static ores::refdata::messaging::get_many_calendars_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::get_many_calendars_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::get_many_calendars_response& v);
    static ores::refdata::messaging::get_many_calendars_response read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::get_many_calendars_response& v);
    static ores::refdata::messaging::get_many_calendars_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::put_calendar_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::put_calendar_request& v);
    static ores::refdata::messaging::put_calendar_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::put_calendar_request& v);
    static ores::refdata::messaging::put_calendar_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::put_calendar_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::put_calendar_response& v);
    static ores::refdata::messaging::put_calendar_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::put_calendar_response& v);
    static ores::refdata::messaging::put_calendar_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::put_many_calendars_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::put_many_calendars_request& v);
    static ores::refdata::messaging::put_many_calendars_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::put_many_calendars_request& v);
    static ores::refdata::messaging::put_many_calendars_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::put_many_calendars_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::put_many_calendars_response& v);
    static ores::refdata::messaging::put_many_calendars_response read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::put_many_calendars_response& v);
    static ores::refdata::messaging::put_many_calendars_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::delete_calendar_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::delete_calendar_request& v);
    static ores::refdata::messaging::delete_calendar_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::delete_calendar_request& v);
    static ores::refdata::messaging::delete_calendar_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::delete_calendar_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::delete_calendar_response& v);
    static ores::refdata::messaging::delete_calendar_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::delete_calendar_response& v);
    static ores::refdata::messaging::delete_calendar_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::delete_many_calendars_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::delete_many_calendars_request& v);
    static ores::refdata::messaging::delete_many_calendars_request read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::delete_many_calendars_request& v);
    static ores::refdata::messaging::delete_many_calendars_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::delete_many_calendars_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::delete_many_calendars_response& v);
    static ores::refdata::messaging::delete_many_calendars_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::delete_many_calendars_response& v);
    static ores::refdata::messaging::delete_many_calendars_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::list_calendar_versions_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::list_calendar_versions_request& v);
    static ores::refdata::messaging::list_calendar_versions_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::list_calendar_versions_request& v);
    static ores::refdata::messaging::list_calendar_versions_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::list_calendar_versions_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::list_calendar_versions_response& v);
    static ores::refdata::messaging::list_calendar_versions_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::list_calendar_versions_response& v);
    static ores::refdata::messaging::list_calendar_versions_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::get_calendar_version_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::get_calendar_version_request& v);
    static ores::refdata::messaging::get_calendar_version_request read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::get_calendar_version_request& v);
    static ores::refdata::messaging::get_calendar_version_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_calendar_version_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::get_calendar_version_response& v);
    static ores::refdata::messaging::get_calendar_version_response read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::get_calendar_version_response& v);
    static ores::refdata::messaging::get_calendar_version_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
