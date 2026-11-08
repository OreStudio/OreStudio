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
#ifndef ORES_REPORTING_API_MESSAGING_REPORT_ERROR_EVENT_PROTOCOL_HPP
#define ORES_REPORTING_API_MESSAGING_REPORT_ERROR_EVENT_PROTOCOL_HPP

#include "ores.reporting.api/domain/report_error_event_codec.hpp"
#include "ores.reporting.api/export.hpp"
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

namespace ores::reporting::messaging {

struct report_error_event_key {
    boost::uuids::uuid id;

    friend bool operator==(const report_error_event_key&, const report_error_event_key&) = default;
};

struct report_error_event_lookup {
    report_error_event_key key;
    std::optional<ores::reporting::domain::report_error_event> report_error_event;

    friend bool operator==(const report_error_event_lookup&,
                           const report_error_event_lookup&) = default;
};

struct report_error_events_filter {
    std::optional<boost::uuids::uuid> report_instance_id;
    std::optional<std::string> severity;
    std::optional<std::vector<boost::uuids::uuid>> id_one_of;
    std::optional<std::vector<boost::uuids::uuid>> report_instance_id_one_of;
    std::optional<std::vector<std::string>> severity_one_of;

    friend bool operator==(const report_error_events_filter&,
                           const report_error_events_filter&) = default;
};

struct report_error_event_event {
    boost::uuids::uuid event_id;
    report_error_event_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;

    friend bool operator==(const report_error_event_event&,
                           const report_error_event_event&) = default;
};

struct list_report_error_events_request {
    using response_type = struct list_report_error_events_response;
    static constexpr std::string_view nats_subject = "reporting.v1.report_error_events.list";
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
    std::optional<report_error_events_filter> filter;

    friend bool operator==(const list_report_error_events_request&,
                           const list_report_error_events_request&) = default;
};

struct list_report_error_events_response {
    ores::utility::domain::result result;
    std::vector<ores::reporting::domain::report_error_event> errors;
    std::uint64_t total;

    friend bool operator==(const list_report_error_events_response&,
                           const list_report_error_events_response&) = default;
};

struct get_report_error_event_request {
    using response_type = struct get_report_error_event_response;
    static constexpr std::string_view nats_subject = "reporting.v1.report_error_events.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    report_error_event_key key;

    friend bool operator==(const get_report_error_event_request&,
                           const get_report_error_event_request&) = default;
};

struct get_report_error_event_response {
    ores::utility::domain::result result;
    std::optional<ores::reporting::domain::report_error_event> report_error_event;

    friend bool operator==(const get_report_error_event_response&,
                           const get_report_error_event_response&) = default;
};

struct get_many_report_error_events_request {
    using response_type = struct get_many_report_error_events_response;
    static constexpr std::string_view nats_subject = "reporting.v1.report_error_events.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<report_error_event_key> keys;

    friend bool operator==(const get_many_report_error_events_request&,
                           const get_many_report_error_events_request&) = default;
};

struct get_many_report_error_events_response {
    ores::utility::domain::result result;
    std::vector<report_error_event_lookup> entries;

    friend bool operator==(const get_many_report_error_events_response&,
                           const get_many_report_error_events_response&) = default;
};

struct list_by_report_instance_id_report_error_events_request {
    using response_type = struct list_by_report_instance_id_report_error_events_response;
    static constexpr std::string_view nats_subject =
        "reporting.v1.report_error_events.list_by_report_instance_id";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    boost::uuids::uuid report_instance_id;
    ores::utility::domain::scope scope = ores::utility::domain::scope::direct;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<report_error_events_filter> filter;

    friend bool operator==(const list_by_report_instance_id_report_error_events_request&,
                           const list_by_report_instance_id_report_error_events_request&) = default;
};

struct list_by_report_instance_id_report_error_events_response {
    ores::utility::domain::result result;
    std::vector<ores::reporting::domain::report_error_event> errors;
    std::uint64_t total;

    friend bool
    operator==(const list_by_report_instance_id_report_error_events_response&,
               const list_by_report_instance_id_report_error_events_response&) = default;
};

struct report_error_count {
    /** @brief The run the counts are of. */
    boost::uuids::uuid report_instance_id;
    /** @brief How many of the run's problems are errors. */
    std::int64_t errors = 0;
    /** @brief How many are warnings. */
    std::int64_t warnings = 0;

    friend bool operator==(const report_error_count&, const report_error_count&) = default;
};

/**
 * @brief Counts the problems of each named run in one call.
 *
 * The caller asks once with the runs a read of the latest runs returned, so a
 * list of forty definitions pays one row per run rather than every error of
 * every run. The request names no party: the session scopes the answer through
 * the row-level security on the table.
 */
struct get_report_error_counts_request {
    using response_type = struct get_report_error_counts_response;
    static constexpr std::string_view nats_subject = "reporting.v1.ops.get_report_error_counts";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    /** @brief The runs to count the problems of, in any order. */
    std::vector<boost::uuids::uuid> report_instance_ids;

    friend bool operator==(const get_report_error_counts_request&,
                           const get_report_error_counts_request&) = default;
};

struct get_report_error_counts_response {
    ores::utility::domain::result result;
    /**
     * @brief One entry per run that recorded a problem, and no entry for a run
     * that recorded none.
     *
     * The absence is the screen's zero: nothing was found against that run.
     */
    std::vector<report_error_count> counts;

    friend bool operator==(const get_report_error_counts_response&,
                           const get_report_error_counts_response&) = default;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace report_error_event_event_subjects {
inline constexpr std::string_view created = "reporting.v1.events.report_error_events.created";
inline constexpr std::string_view updated = "reporting.v1.events.report_error_events.updated";
inline constexpr std::string_view deleted = "reporting.v1.events.report_error_events.deleted";
}
/**
 * @brief The members of report_error_event_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<report_error_event_key>) {
    return std::tuple{
        utility::codec::field{"id", &report_error_event_key::id},
    };
}

/**
 * @brief The members of report_error_event_lookup, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<report_error_event_lookup>) {
    return std::tuple{
        utility::codec::field{"key", &report_error_event_lookup::key},
        utility::codec::field{"report_error_event", &report_error_event_lookup::report_error_event},
    };
}

/**
 * @brief The members of report_error_events_filter, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<report_error_events_filter>) {
    return std::tuple{
        utility::codec::field{"report_instance_id",
                              &report_error_events_filter::report_instance_id},
        utility::codec::field{"severity", &report_error_events_filter::severity},
        utility::codec::field{"id_one_of", &report_error_events_filter::id_one_of},
        utility::codec::field{"report_instance_id_one_of",
                              &report_error_events_filter::report_instance_id_one_of},
        utility::codec::field{"severity_one_of", &report_error_events_filter::severity_one_of},
    };
}

/**
 * @brief The members of report_error_event_event, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<report_error_event_event>) {
    return std::tuple{
        utility::codec::field{"event_id", &report_error_event_event::event_id},
        utility::codec::field{"key", &report_error_event_event::key},
        utility::codec::field{"action", &report_error_event_event::action},
        utility::codec::field{"version", &report_error_event_event::version},
        utility::codec::field{"occurred_at", &report_error_event_event::occurred_at},
        utility::codec::field{"correlation_id", &report_error_event_event::correlation_id},
    };
}

/**
 * @brief The members of list_report_error_events_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<list_report_error_events_request>) {
    return std::tuple{
        utility::codec::field{"offset", &list_report_error_events_request::offset},
        utility::codec::field{"limit", &list_report_error_events_request::limit},
        utility::codec::field{"order", &list_report_error_events_request::order},
        utility::codec::field{"filter", &list_report_error_events_request::filter},
    };
}

/**
 * @brief The members of list_report_error_events_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<list_report_error_events_response>) {
    return std::tuple{
        utility::codec::field{"result", &list_report_error_events_response::result},
        utility::codec::field{"errors", &list_report_error_events_response::errors},
        utility::codec::field{"total", &list_report_error_events_response::total},
    };
}

/**
 * @brief The members of get_report_error_event_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_report_error_event_request>) {
    return std::tuple{
        utility::codec::field{"key", &get_report_error_event_request::key},
    };
}

/**
 * @brief The members of get_report_error_event_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_report_error_event_response>) {
    return std::tuple{
        utility::codec::field{"result", &get_report_error_event_response::result},
        utility::codec::field{"report_error_event",
                              &get_report_error_event_response::report_error_event},
    };
}

/**
 * @brief The members of get_many_report_error_events_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_many_report_error_events_request>) {
    return std::tuple{
        utility::codec::field{"keys", &get_many_report_error_events_request::keys},
    };
}

/**
 * @brief The members of get_many_report_error_events_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<get_many_report_error_events_response>) {
    return std::tuple{
        utility::codec::field{"result", &get_many_report_error_events_response::result},
        utility::codec::field{"entries", &get_many_report_error_events_response::entries},
    };
}

/**
 * @brief The members of list_by_report_instance_id_report_error_events_request, named as the wire
 * names them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<list_by_report_instance_id_report_error_events_request>) {
    return std::tuple{
        utility::codec::field{
            "report_instance_id",
            &list_by_report_instance_id_report_error_events_request::report_instance_id},
        utility::codec::field{"scope",
                              &list_by_report_instance_id_report_error_events_request::scope},
        utility::codec::field{"offset",
                              &list_by_report_instance_id_report_error_events_request::offset},
        utility::codec::field{"limit",
                              &list_by_report_instance_id_report_error_events_request::limit},
        utility::codec::field{"order",
                              &list_by_report_instance_id_report_error_events_request::order},
        utility::codec::field{"filter",
                              &list_by_report_instance_id_report_error_events_request::filter},
    };
}

/**
 * @brief The members of list_by_report_instance_id_report_error_events_response, named as the wire
 * names them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<list_by_report_instance_id_report_error_events_response>) {
    return std::tuple{
        utility::codec::field{"result",
                              &list_by_report_instance_id_report_error_events_response::result},
        utility::codec::field{"errors",
                              &list_by_report_instance_id_report_error_events_response::errors},
        utility::codec::field{"total",
                              &list_by_report_instance_id_report_error_events_response::total},
    };
}

/**
 * @brief The members of report_error_count, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<report_error_count>) {
    return std::tuple{
        utility::codec::field{"report_instance_id", &report_error_count::report_instance_id},
        utility::codec::field{"errors", &report_error_count::errors},
        utility::codec::field{"warnings", &report_error_count::warnings},
    };
}

/**
 * @brief The members of get_report_error_counts_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_report_error_counts_request>) {
    return std::tuple{
        utility::codec::field{"report_instance_ids",
                              &get_report_error_counts_request::report_instance_ids},
    };
}

/**
 * @brief The members of get_report_error_counts_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_report_error_counts_response>) {
    return std::tuple{
        utility::codec::field{"result", &get_report_error_counts_response::result},
        utility::codec::field{"counts", &get_report_error_counts_response::counts},
    };
}


}
namespace ores::utility::codec {

template <>
struct ORES_REPORTING_API_EXPORT wire_traits<ores::reporting::messaging::report_error_event_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::reporting::messaging::report_error_event_key& v);
    static ores::reporting::messaging::report_error_event_key read_json(std::string_view text);
    static std::string write_msgpack(const ores::reporting::messaging::report_error_event_key& v);
    static ores::reporting::messaging::report_error_event_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::report_error_event_lookup> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::reporting::messaging::report_error_event_lookup& v);
    static ores::reporting::messaging::report_error_event_lookup read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::report_error_event_lookup& v);
    static ores::reporting::messaging::report_error_event_lookup
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::report_error_events_filter> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::reporting::messaging::report_error_events_filter& v);
    static ores::reporting::messaging::report_error_events_filter read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::report_error_events_filter& v);
    static ores::reporting::messaging::report_error_events_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT wire_traits<ores::reporting::messaging::report_error_event_event> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::reporting::messaging::report_error_event_event& v);
    static ores::reporting::messaging::report_error_event_event read_json(std::string_view text);
    static std::string write_msgpack(const ores::reporting::messaging::report_error_event_event& v);
    static ores::reporting::messaging::report_error_event_event
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::list_report_error_events_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::list_report_error_events_request& v);
    static ores::reporting::messaging::list_report_error_events_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::list_report_error_events_request& v);
    static ores::reporting::messaging::list_report_error_events_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::list_report_error_events_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::list_report_error_events_response& v);
    static ores::reporting::messaging::list_report_error_events_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::list_report_error_events_response& v);
    static ores::reporting::messaging::list_report_error_events_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::get_report_error_event_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::get_report_error_event_request& v);
    static ores::reporting::messaging::get_report_error_event_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::get_report_error_event_request& v);
    static ores::reporting::messaging::get_report_error_event_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::get_report_error_event_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::get_report_error_event_response& v);
    static ores::reporting::messaging::get_report_error_event_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::get_report_error_event_response& v);
    static ores::reporting::messaging::get_report_error_event_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::get_many_report_error_events_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::get_many_report_error_events_request& v);
    static ores::reporting::messaging::get_many_report_error_events_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::get_many_report_error_events_request& v);
    static ores::reporting::messaging::get_many_report_error_events_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::get_many_report_error_events_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::get_many_report_error_events_response& v);
    static ores::reporting::messaging::get_many_report_error_events_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::get_many_report_error_events_response& v);
    static ores::reporting::messaging::get_many_report_error_events_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT wire_traits<
    ores::reporting::messaging::list_by_report_instance_id_report_error_events_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::reporting::messaging::list_by_report_instance_id_report_error_events_request&
            v);
    static ores::reporting::messaging::list_by_report_instance_id_report_error_events_request
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::reporting::messaging::list_by_report_instance_id_report_error_events_request&
            v);
    static ores::reporting::messaging::list_by_report_instance_id_report_error_events_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT wire_traits<
    ores::reporting::messaging::list_by_report_instance_id_report_error_events_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::reporting::messaging::list_by_report_instance_id_report_error_events_response&
            v);
    static ores::reporting::messaging::list_by_report_instance_id_report_error_events_response
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::reporting::messaging::list_by_report_instance_id_report_error_events_response&
            v);
    static ores::reporting::messaging::list_by_report_instance_id_report_error_events_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT wire_traits<ores::reporting::messaging::report_error_count> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::reporting::messaging::report_error_count& v);
    static ores::reporting::messaging::report_error_count read_json(std::string_view text);
    static std::string write_msgpack(const ores::reporting::messaging::report_error_count& v);
    static ores::reporting::messaging::report_error_count
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::get_report_error_counts_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::get_report_error_counts_request& v);
    static ores::reporting::messaging::get_report_error_counts_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::get_report_error_counts_request& v);
    static ores::reporting::messaging::get_report_error_counts_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::get_report_error_counts_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::get_report_error_counts_response& v);
    static ores::reporting::messaging::get_report_error_counts_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::get_report_error_counts_response& v);
    static ores::reporting::messaging::get_report_error_counts_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
