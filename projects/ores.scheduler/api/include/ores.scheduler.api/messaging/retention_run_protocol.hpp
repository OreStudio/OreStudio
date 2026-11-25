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
#ifndef ORES_SCHEDULER_API_MESSAGING_RETENTION_RUN_PROTOCOL_HPP
#define ORES_SCHEDULER_API_MESSAGING_RETENTION_RUN_PROTOCOL_HPP

#include "ores.scheduler.api/domain/retention_run_codec.hpp"
#include "ores.scheduler.api/export.hpp"
#include "ores.utility/codec/wire_fields.hpp"
#include "ores.utility/codec/wire_traits_fwd.hpp"
#include "ores.utility/domain/protocol.hpp"
#include "ores.utility/domain/protocol_codec.hpp"
#include <boost/uuid/uuid.hpp>
#include <cstddef>
#include <cstdint>
#include <optional>
#include <span>
#include <string>
#include <tuple>
#include <vector>

namespace ores::scheduler::messaging {

struct retention_run_key {
    boost::uuids::uuid id;

    friend bool operator==(const retention_run_key&, const retention_run_key&) = default;
};

struct retention_run_lookup {
    retention_run_key key;
    std::optional<ores::scheduler::domain::retention_run> retention_run;

    friend bool operator==(const retention_run_lookup&, const retention_run_lookup&) = default;
};

struct retention_runs_filter {
    std::optional<std::string> policy_code;
    std::optional<std::string> outcome;
    std::optional<std::vector<boost::uuids::uuid>> id_one_of;
    std::optional<std::vector<std::string>> policy_code_one_of;
    std::optional<std::vector<std::string>> outcome_one_of;

    friend bool operator==(const retention_runs_filter&, const retention_runs_filter&) = default;
};

struct retention_run_event {
    boost::uuids::uuid event_id;
    retention_run_key key;
    std::string action;
    std::uint32_t version{};
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;

    friend bool operator==(const retention_run_event&, const retention_run_event&) = default;
};

struct list_retention_runs_request {
    using response_type = struct list_retention_runs_response;
    static constexpr std::string_view nats_subject = "scheduler.v1.retention_runs.list";
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
    std::optional<retention_runs_filter> filter;

    friend bool operator==(const list_retention_runs_request&,
                           const list_retention_runs_request&) = default;
};

struct list_retention_runs_response {
    ores::utility::domain::result result;
    std::vector<ores::scheduler::domain::retention_run> runs;
    std::uint64_t total{};

    friend bool operator==(const list_retention_runs_response&,
                           const list_retention_runs_response&) = default;
};

struct get_retention_run_request {
    using response_type = struct get_retention_run_response;
    static constexpr std::string_view nats_subject = "scheduler.v1.retention_runs.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    retention_run_key key;

    friend bool operator==(const get_retention_run_request&,
                           const get_retention_run_request&) = default;
};

struct get_retention_run_response {
    ores::utility::domain::result result;
    std::optional<ores::scheduler::domain::retention_run> retention_run;

    friend bool operator==(const get_retention_run_response&,
                           const get_retention_run_response&) = default;
};

struct get_many_retention_runs_request {
    using response_type = struct get_many_retention_runs_response;
    static constexpr std::string_view nats_subject = "scheduler.v1.retention_runs.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<retention_run_key> keys;

    friend bool operator==(const get_many_retention_runs_request&,
                           const get_many_retention_runs_request&) = default;
};

struct get_many_retention_runs_response {
    ores::utility::domain::result result;
    std::vector<retention_run_lookup> entries;

    friend bool operator==(const get_many_retention_runs_response&,
                           const get_many_retention_runs_response&) = default;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace retention_run_event_subjects {
inline constexpr std::string_view created = "scheduler.v1.events.retention_runs.created";
inline constexpr std::string_view updated = "scheduler.v1.events.retention_runs.updated";
inline constexpr std::string_view deleted = "scheduler.v1.events.retention_runs.deleted";
}
/**
 * @brief The members of retention_run_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<retention_run_key>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"id", &retention_run_key::id},
    };
}

/**
 * @brief The members of retention_run_lookup, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<retention_run_lookup>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"key", &retention_run_lookup::key},
        utility::codec::field{"retention_run", &retention_run_lookup::retention_run},
    };
}

/**
 * @brief The members of retention_runs_filter, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<retention_runs_filter>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"policy_code", &retention_runs_filter::policy_code},
        utility::codec::field{"outcome", &retention_runs_filter::outcome},
        utility::codec::field{"id_one_of", &retention_runs_filter::id_one_of},
        utility::codec::field{"policy_code_one_of", &retention_runs_filter::policy_code_one_of},
        utility::codec::field{"outcome_one_of", &retention_runs_filter::outcome_one_of},
    };
}

/**
 * @brief The members of retention_run_event, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<retention_run_event>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"event_id", &retention_run_event::event_id},
        utility::codec::field{"key", &retention_run_event::key},
        utility::codec::field{"action", &retention_run_event::action},
        utility::codec::field{"version", &retention_run_event::version},
        utility::codec::field{"occurred_at", &retention_run_event::occurred_at},
        utility::codec::field{"correlation_id", &retention_run_event::correlation_id},
    };
}

/**
 * @brief The members of list_retention_runs_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<list_retention_runs_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"offset", &list_retention_runs_request::offset},
        utility::codec::field{"limit", &list_retention_runs_request::limit},
        utility::codec::field{"order", &list_retention_runs_request::order},
        utility::codec::field{"filter", &list_retention_runs_request::filter},
    };
}

/**
 * @brief The members of list_retention_runs_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<list_retention_runs_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &list_retention_runs_response::result},
        utility::codec::field{"runs", &list_retention_runs_response::runs},
        utility::codec::field{"total", &list_retention_runs_response::total},
    };
}

/**
 * @brief The members of get_retention_run_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_retention_run_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"key", &get_retention_run_request::key},
    };
}

/**
 * @brief The members of get_retention_run_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_retention_run_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &get_retention_run_response::result},
        utility::codec::field{"retention_run", &get_retention_run_response::retention_run},
    };
}

/**
 * @brief The members of get_many_retention_runs_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_many_retention_runs_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"keys", &get_many_retention_runs_request::keys},
    };
}

/**
 * @brief The members of get_many_retention_runs_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_many_retention_runs_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &get_many_retention_runs_response::result},
        utility::codec::field{"entries", &get_many_retention_runs_response::entries},
    };
}


}
namespace ores::utility::codec {

template <>
struct ORES_SCHEDULER_API_EXPORT wire_traits<ores::scheduler::messaging::retention_run_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::scheduler::messaging::retention_run_key& v);
    static ores::scheduler::messaging::retention_run_key read_json(std::string_view text);
    static std::string write_msgpack(const ores::scheduler::messaging::retention_run_key& v);
    static ores::scheduler::messaging::retention_run_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SCHEDULER_API_EXPORT wire_traits<ores::scheduler::messaging::retention_run_lookup> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::scheduler::messaging::retention_run_lookup& v);
    static ores::scheduler::messaging::retention_run_lookup read_json(std::string_view text);
    static std::string write_msgpack(const ores::scheduler::messaging::retention_run_lookup& v);
    static ores::scheduler::messaging::retention_run_lookup
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SCHEDULER_API_EXPORT wire_traits<ores::scheduler::messaging::retention_runs_filter> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::scheduler::messaging::retention_runs_filter& v);
    static ores::scheduler::messaging::retention_runs_filter read_json(std::string_view text);
    static std::string write_msgpack(const ores::scheduler::messaging::retention_runs_filter& v);
    static ores::scheduler::messaging::retention_runs_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SCHEDULER_API_EXPORT wire_traits<ores::scheduler::messaging::retention_run_event> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::scheduler::messaging::retention_run_event& v);
    static ores::scheduler::messaging::retention_run_event read_json(std::string_view text);
    static std::string write_msgpack(const ores::scheduler::messaging::retention_run_event& v);
    static ores::scheduler::messaging::retention_run_event
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SCHEDULER_API_EXPORT
    wire_traits<ores::scheduler::messaging::list_retention_runs_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::scheduler::messaging::list_retention_runs_request& v);
    static ores::scheduler::messaging::list_retention_runs_request read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::scheduler::messaging::list_retention_runs_request& v);
    static ores::scheduler::messaging::list_retention_runs_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SCHEDULER_API_EXPORT
    wire_traits<ores::scheduler::messaging::list_retention_runs_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::scheduler::messaging::list_retention_runs_response& v);
    static ores::scheduler::messaging::list_retention_runs_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::scheduler::messaging::list_retention_runs_response& v);
    static ores::scheduler::messaging::list_retention_runs_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SCHEDULER_API_EXPORT
    wire_traits<ores::scheduler::messaging::get_retention_run_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::scheduler::messaging::get_retention_run_request& v);
    static ores::scheduler::messaging::get_retention_run_request read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::scheduler::messaging::get_retention_run_request& v);
    static ores::scheduler::messaging::get_retention_run_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SCHEDULER_API_EXPORT
    wire_traits<ores::scheduler::messaging::get_retention_run_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::scheduler::messaging::get_retention_run_response& v);
    static ores::scheduler::messaging::get_retention_run_response read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::scheduler::messaging::get_retention_run_response& v);
    static ores::scheduler::messaging::get_retention_run_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SCHEDULER_API_EXPORT
    wire_traits<ores::scheduler::messaging::get_many_retention_runs_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::scheduler::messaging::get_many_retention_runs_request& v);
    static ores::scheduler::messaging::get_many_retention_runs_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::scheduler::messaging::get_many_retention_runs_request& v);
    static ores::scheduler::messaging::get_many_retention_runs_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SCHEDULER_API_EXPORT
    wire_traits<ores::scheduler::messaging::get_many_retention_runs_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::scheduler::messaging::get_many_retention_runs_response& v);
    static ores::scheduler::messaging::get_many_retention_runs_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::scheduler::messaging::get_many_retention_runs_response& v);
    static ores::scheduler::messaging::get_many_retention_runs_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
