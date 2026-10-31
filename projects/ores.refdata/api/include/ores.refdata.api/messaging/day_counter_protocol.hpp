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
#ifndef ORES_REFDATA_API_MESSAGING_DAY_COUNTER_PROTOCOL_HPP
#define ORES_REFDATA_API_MESSAGING_DAY_COUNTER_PROTOCOL_HPP

#include "ores.refdata.api/domain/day_counter.hpp"
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

struct day_counter_key {
    std::string code;
};

struct day_counter_write {
    std::string code;
    std::string description;
};

struct day_counter_change {
    day_counter_write write;
    ores::utility::domain::precondition precondition;
};

struct day_counter_removal {
    day_counter_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;
};

struct day_counter_lookup {
    day_counter_key key;
    std::optional<ores::refdata::domain::day_counter> day_counter;
};

struct day_counters_filter {
    std::optional<std::vector<std::string>> code_one_of;
};

struct day_counter_event {
    boost::uuids::uuid event_id;
    day_counter_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;
};

struct day_counter_version_key {
    day_counter_key day_counter;
    std::uint32_t version;
};

struct day_counter_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;
};

struct list_day_counters_request {
    using response_type = struct list_day_counters_response;
    static constexpr std::string_view nats_subject = "refdata.v1.day_counters.list";
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
    std::optional<day_counters_filter> filter;
    std::optional<std::string> as_of;
};

struct list_day_counters_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::day_counter> day_counters;
    std::uint64_t total;
};

struct get_day_counter_request {
    using response_type = struct get_day_counter_response;
    static constexpr std::string_view nats_subject = "refdata.v1.day_counters.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    day_counter_key key;
};

struct get_day_counter_response {
    ores::utility::domain::result result;
    std::optional<ores::refdata::domain::day_counter> day_counter;
};

struct get_many_day_counters_request {
    using response_type = struct get_many_day_counters_response;
    static constexpr std::string_view nats_subject = "refdata.v1.day_counters.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<day_counter_key> keys;
};

struct get_many_day_counters_response {
    ores::utility::domain::result result;
    std::vector<day_counter_lookup> entries;
};

struct put_day_counter_request {
    using response_type = struct put_day_counter_response;
    static constexpr std::string_view nats_subject = "refdata.v1.day_counters.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    day_counter_change change;
    ores::utility::domain::change_intent intent;
};

struct put_day_counter_response {
    ores::utility::domain::result result;
    std::optional<ores::refdata::domain::day_counter> day_counter;
};

struct put_many_day_counters_request {
    using response_type = struct put_many_day_counters_response;
    static constexpr std::string_view nats_subject = "refdata.v1.day_counters.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<day_counter_change> changes;
    ores::utility::domain::change_intent intent;
};

struct put_many_day_counters_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::day_counter> day_counters;
};

struct delete_day_counter_request {
    using response_type = struct delete_day_counter_response;
    static constexpr std::string_view nats_subject = "refdata.v1.day_counters.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    day_counter_removal removal;
    ores::utility::domain::change_intent intent;
};

struct delete_day_counter_response {
    ores::utility::domain::result result;
};

struct delete_many_day_counters_request {
    using response_type = struct delete_many_day_counters_response;
    static constexpr std::string_view nats_subject = "refdata.v1.day_counters.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<day_counter_removal> removals;
    ores::utility::domain::change_intent intent;
};

struct delete_many_day_counters_response {
    ores::utility::domain::result result;
};

struct list_day_counter_versions_request {
    using response_type = struct list_day_counter_versions_response;
    static constexpr std::string_view nats_subject = "refdata.v1.day_counters_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    day_counter_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<day_counter_versions_filter> filter;
};

struct list_day_counter_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::day_counter> versions;
    std::uint64_t total;
};

struct get_day_counter_version_request {
    using response_type = struct get_day_counter_version_response;
    static constexpr std::string_view nats_subject = "refdata.v1.day_counters_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    day_counter_version_key key;
};

struct get_day_counter_version_response {
    ores::utility::domain::result result;
    std::optional<ores::refdata::domain::day_counter> version;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace day_counter_event_subjects {
inline constexpr std::string_view created = "refdata.v1.day_counters_events.created";
inline constexpr std::string_view updated = "refdata.v1.day_counters_events.updated";
inline constexpr std::string_view deleted = "refdata.v1.day_counters_events.deleted";
}
/**
 * @brief The members of day_counter_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const day_counter_key&) {
    return std::tuple{
        utility::codec::field{"code", &day_counter_key::code},
    };
}

/**
 * @brief The members of day_counter_write, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const day_counter_write&) {
    return std::tuple{
        utility::codec::field{"code", &day_counter_write::code},
        utility::codec::field{"description", &day_counter_write::description},
    };
}

/**
 * @brief The members of day_counter_change, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const day_counter_change&) {
    return std::tuple{
        utility::codec::field{"write", &day_counter_change::write},
        utility::codec::field{"precondition", &day_counter_change::precondition},
    };
}

/**
 * @brief The members of day_counter_removal, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const day_counter_removal&) {
    return std::tuple{
        utility::codec::field{"key", &day_counter_removal::key},
        utility::codec::field{"precondition", &day_counter_removal::precondition},
    };
}

/**
 * @brief The members of day_counter_lookup, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const day_counter_lookup&) {
    return std::tuple{
        utility::codec::field{"key", &day_counter_lookup::key},
        utility::codec::field{"day_counter", &day_counter_lookup::day_counter},
    };
}

/**
 * @brief The members of day_counters_filter, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const day_counters_filter&) {
    return std::tuple{
        utility::codec::field{"code_one_of", &day_counters_filter::code_one_of},
    };
}

/**
 * @brief The members of day_counter_event, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const day_counter_event&) {
    return std::tuple{
        utility::codec::field{"event_id", &day_counter_event::event_id},
        utility::codec::field{"key", &day_counter_event::key},
        utility::codec::field{"action", &day_counter_event::action},
        utility::codec::field{"version", &day_counter_event::version},
        utility::codec::field{"occurred_at", &day_counter_event::occurred_at},
        utility::codec::field{"correlation_id", &day_counter_event::correlation_id},
    };
}

/**
 * @brief The members of day_counter_version_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const day_counter_version_key&) {
    return std::tuple{
        utility::codec::field{"day_counter", &day_counter_version_key::day_counter},
        utility::codec::field{"version", &day_counter_version_key::version},
    };
}

/**
 * @brief The members of day_counter_versions_filter, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const day_counter_versions_filter&) {
    return std::tuple{
        utility::codec::field{"version", &day_counter_versions_filter::version},
        utility::codec::field{"from_version", &day_counter_versions_filter::from_version},
        utility::codec::field{"to_version", &day_counter_versions_filter::to_version},
    };
}

/**
 * @brief The members of list_day_counters_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_day_counters_request&) {
    return std::tuple{
        utility::codec::field{"offset", &list_day_counters_request::offset},
        utility::codec::field{"limit", &list_day_counters_request::limit},
        utility::codec::field{"order", &list_day_counters_request::order},
        utility::codec::field{"filter", &list_day_counters_request::filter},
        utility::codec::field{"as_of", &list_day_counters_request::as_of},
    };
}

/**
 * @brief The members of list_day_counters_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_day_counters_response&) {
    return std::tuple{
        utility::codec::field{"result", &list_day_counters_response::result},
        utility::codec::field{"day_counters", &list_day_counters_response::day_counters},
        utility::codec::field{"total", &list_day_counters_response::total},
    };
}

/**
 * @brief The members of get_day_counter_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_day_counter_request&) {
    return std::tuple{
        utility::codec::field{"key", &get_day_counter_request::key},
    };
}

/**
 * @brief The members of get_day_counter_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_day_counter_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_day_counter_response::result},
        utility::codec::field{"day_counter", &get_day_counter_response::day_counter},
    };
}

/**
 * @brief The members of get_many_day_counters_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_many_day_counters_request&) {
    return std::tuple{
        utility::codec::field{"keys", &get_many_day_counters_request::keys},
    };
}

/**
 * @brief The members of get_many_day_counters_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_many_day_counters_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_many_day_counters_response::result},
        utility::codec::field{"entries", &get_many_day_counters_response::entries},
    };
}

/**
 * @brief The members of put_day_counter_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_day_counter_request&) {
    return std::tuple{
        utility::codec::field{"change", &put_day_counter_request::change},
        utility::codec::field{"intent", &put_day_counter_request::intent},
    };
}

/**
 * @brief The members of put_day_counter_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_day_counter_response&) {
    return std::tuple{
        utility::codec::field{"result", &put_day_counter_response::result},
        utility::codec::field{"day_counter", &put_day_counter_response::day_counter},
    };
}

/**
 * @brief The members of put_many_day_counters_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_many_day_counters_request&) {
    return std::tuple{
        utility::codec::field{"changes", &put_many_day_counters_request::changes},
        utility::codec::field{"intent", &put_many_day_counters_request::intent},
    };
}

/**
 * @brief The members of put_many_day_counters_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_many_day_counters_response&) {
    return std::tuple{
        utility::codec::field{"result", &put_many_day_counters_response::result},
        utility::codec::field{"day_counters", &put_many_day_counters_response::day_counters},
    };
}

/**
 * @brief The members of delete_day_counter_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_day_counter_request&) {
    return std::tuple{
        utility::codec::field{"removal", &delete_day_counter_request::removal},
        utility::codec::field{"intent", &delete_day_counter_request::intent},
    };
}

/**
 * @brief The members of delete_day_counter_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_day_counter_response&) {
    return std::tuple{
        utility::codec::field{"result", &delete_day_counter_response::result},
    };
}

/**
 * @brief The members of delete_many_day_counters_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_many_day_counters_request&) {
    return std::tuple{
        utility::codec::field{"removals", &delete_many_day_counters_request::removals},
        utility::codec::field{"intent", &delete_many_day_counters_request::intent},
    };
}

/**
 * @brief The members of delete_many_day_counters_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_many_day_counters_response&) {
    return std::tuple{
        utility::codec::field{"result", &delete_many_day_counters_response::result},
    };
}

/**
 * @brief The members of list_day_counter_versions_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_day_counter_versions_request&) {
    return std::tuple{
        utility::codec::field{"key", &list_day_counter_versions_request::key},
        utility::codec::field{"offset", &list_day_counter_versions_request::offset},
        utility::codec::field{"limit", &list_day_counter_versions_request::limit},
        utility::codec::field{"order", &list_day_counter_versions_request::order},
        utility::codec::field{"filter", &list_day_counter_versions_request::filter},
    };
}

/**
 * @brief The members of list_day_counter_versions_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_day_counter_versions_response&) {
    return std::tuple{
        utility::codec::field{"result", &list_day_counter_versions_response::result},
        utility::codec::field{"versions", &list_day_counter_versions_response::versions},
        utility::codec::field{"total", &list_day_counter_versions_response::total},
    };
}

/**
 * @brief The members of get_day_counter_version_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_day_counter_version_request&) {
    return std::tuple{
        utility::codec::field{"key", &get_day_counter_version_request::key},
    };
}

/**
 * @brief The members of get_day_counter_version_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_day_counter_version_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_day_counter_version_response::result},
        utility::codec::field{"version", &get_day_counter_version_response::version},
    };
}


}
namespace ores::utility::codec {

template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::day_counter_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::day_counter_key& v);
    static ores::refdata::messaging::day_counter_key read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::day_counter_key& v);
    static ores::refdata::messaging::day_counter_key read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::day_counter_write> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::day_counter_write& v);
    static ores::refdata::messaging::day_counter_write read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::day_counter_write& v);
    static ores::refdata::messaging::day_counter_write
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::day_counter_change> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::day_counter_change& v);
    static ores::refdata::messaging::day_counter_change read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::day_counter_change& v);
    static ores::refdata::messaging::day_counter_change
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::day_counter_removal> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::day_counter_removal& v);
    static ores::refdata::messaging::day_counter_removal read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::day_counter_removal& v);
    static ores::refdata::messaging::day_counter_removal
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::day_counter_lookup> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::day_counter_lookup& v);
    static ores::refdata::messaging::day_counter_lookup read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::day_counter_lookup& v);
    static ores::refdata::messaging::day_counter_lookup
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::day_counters_filter> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::day_counters_filter& v);
    static ores::refdata::messaging::day_counters_filter read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::day_counters_filter& v);
    static ores::refdata::messaging::day_counters_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::day_counter_event> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::day_counter_event& v);
    static ores::refdata::messaging::day_counter_event read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::day_counter_event& v);
    static ores::refdata::messaging::day_counter_event
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::day_counter_version_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::day_counter_version_key& v);
    static ores::refdata::messaging::day_counter_version_key read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::day_counter_version_key& v);
    static ores::refdata::messaging::day_counter_version_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::day_counter_versions_filter> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::day_counter_versions_filter& v);
    static ores::refdata::messaging::day_counter_versions_filter read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::day_counter_versions_filter& v);
    static ores::refdata::messaging::day_counter_versions_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::list_day_counters_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::list_day_counters_request& v);
    static ores::refdata::messaging::list_day_counters_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::list_day_counters_request& v);
    static ores::refdata::messaging::list_day_counters_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::list_day_counters_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::list_day_counters_response& v);
    static ores::refdata::messaging::list_day_counters_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::list_day_counters_response& v);
    static ores::refdata::messaging::list_day_counters_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::get_day_counter_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::get_day_counter_request& v);
    static ores::refdata::messaging::get_day_counter_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::get_day_counter_request& v);
    static ores::refdata::messaging::get_day_counter_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::get_day_counter_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::get_day_counter_response& v);
    static ores::refdata::messaging::get_day_counter_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::get_day_counter_response& v);
    static ores::refdata::messaging::get_day_counter_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_many_day_counters_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::get_many_day_counters_request& v);
    static ores::refdata::messaging::get_many_day_counters_request read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::get_many_day_counters_request& v);
    static ores::refdata::messaging::get_many_day_counters_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_many_day_counters_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::get_many_day_counters_response& v);
    static ores::refdata::messaging::get_many_day_counters_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::get_many_day_counters_response& v);
    static ores::refdata::messaging::get_many_day_counters_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::put_day_counter_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::put_day_counter_request& v);
    static ores::refdata::messaging::put_day_counter_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::put_day_counter_request& v);
    static ores::refdata::messaging::put_day_counter_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::put_day_counter_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::put_day_counter_response& v);
    static ores::refdata::messaging::put_day_counter_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::put_day_counter_response& v);
    static ores::refdata::messaging::put_day_counter_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::put_many_day_counters_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::put_many_day_counters_request& v);
    static ores::refdata::messaging::put_many_day_counters_request read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::put_many_day_counters_request& v);
    static ores::refdata::messaging::put_many_day_counters_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::put_many_day_counters_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::put_many_day_counters_response& v);
    static ores::refdata::messaging::put_many_day_counters_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::put_many_day_counters_response& v);
    static ores::refdata::messaging::put_many_day_counters_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::delete_day_counter_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::delete_day_counter_request& v);
    static ores::refdata::messaging::delete_day_counter_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::delete_day_counter_request& v);
    static ores::refdata::messaging::delete_day_counter_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::delete_day_counter_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::delete_day_counter_response& v);
    static ores::refdata::messaging::delete_day_counter_response read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::delete_day_counter_response& v);
    static ores::refdata::messaging::delete_day_counter_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::delete_many_day_counters_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::delete_many_day_counters_request& v);
    static ores::refdata::messaging::delete_many_day_counters_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::delete_many_day_counters_request& v);
    static ores::refdata::messaging::delete_many_day_counters_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::delete_many_day_counters_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::delete_many_day_counters_response& v);
    static ores::refdata::messaging::delete_many_day_counters_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::delete_many_day_counters_response& v);
    static ores::refdata::messaging::delete_many_day_counters_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::list_day_counter_versions_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::list_day_counter_versions_request& v);
    static ores::refdata::messaging::list_day_counter_versions_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::list_day_counter_versions_request& v);
    static ores::refdata::messaging::list_day_counter_versions_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::list_day_counter_versions_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::list_day_counter_versions_response& v);
    static ores::refdata::messaging::list_day_counter_versions_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::list_day_counter_versions_response& v);
    static ores::refdata::messaging::list_day_counter_versions_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_day_counter_version_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::get_day_counter_version_request& v);
    static ores::refdata::messaging::get_day_counter_version_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::get_day_counter_version_request& v);
    static ores::refdata::messaging::get_day_counter_version_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_day_counter_version_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::get_day_counter_version_response& v);
    static ores::refdata::messaging::get_day_counter_version_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::get_day_counter_version_response& v);
    static ores::refdata::messaging::get_day_counter_version_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
