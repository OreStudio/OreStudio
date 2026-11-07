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
#ifndef ORES_REPORTING_API_MESSAGING_RUN_RESULT_PROTOCOL_HPP
#define ORES_REPORTING_API_MESSAGING_RUN_RESULT_PROTOCOL_HPP

#include "ores.reporting.api/domain/run_result_codec.hpp"
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

struct run_result_key {
    boost::uuids::uuid id;

    friend bool operator==(const run_result_key&, const run_result_key&) = default;
};

struct run_result_lookup {
    run_result_key key;
    std::optional<ores::reporting::domain::run_result> run_result;

    friend bool operator==(const run_result_lookup&, const run_result_lookup&) = default;
};

struct run_results_filter {
    std::optional<boost::uuids::uuid> party_id;
    std::optional<boost::uuids::uuid> report_instance_id;
    std::optional<boost::uuids::uuid> report_definition_id;
    std::optional<std::string> subject_type;
    std::optional<std::string> subject_id;
    std::optional<std::string> measure;
    std::optional<std::string> currency;
    std::optional<std::optional<boost::uuids::uuid>> counterparty_id;
    std::optional<std::vector<boost::uuids::uuid>> id_one_of;
    std::optional<std::vector<boost::uuids::uuid>> party_id_one_of;
    std::optional<std::vector<boost::uuids::uuid>> report_instance_id_one_of;
    std::optional<std::vector<boost::uuids::uuid>> report_definition_id_one_of;
    std::optional<std::vector<std::string>> subject_type_one_of;
    std::optional<std::vector<std::string>> subject_id_one_of;
    std::optional<std::vector<std::string>> measure_one_of;
    std::optional<std::vector<std::string>> currency_one_of;
    std::optional<std::vector<boost::uuids::uuid>> counterparty_id_one_of;

    friend bool operator==(const run_results_filter&, const run_results_filter&) = default;
};

struct run_result_event {
    boost::uuids::uuid event_id;
    run_result_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;

    friend bool operator==(const run_result_event&, const run_result_event&) = default;
};

struct list_run_results_request {
    using response_type = struct list_run_results_response;
    static constexpr std::string_view nats_subject = "reporting.v1.run_results.list";
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
    std::optional<run_results_filter> filter;

    friend bool operator==(const list_run_results_request&,
                           const list_run_results_request&) = default;
};

struct list_run_results_response {
    ores::utility::domain::result result;
    std::vector<ores::reporting::domain::run_result> results;
    std::uint64_t total;

    friend bool operator==(const list_run_results_response&,
                           const list_run_results_response&) = default;
};

struct get_run_result_request {
    using response_type = struct get_run_result_response;
    static constexpr std::string_view nats_subject = "reporting.v1.run_results.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    run_result_key key;

    friend bool operator==(const get_run_result_request&, const get_run_result_request&) = default;
};

struct get_run_result_response {
    ores::utility::domain::result result;
    std::optional<ores::reporting::domain::run_result> run_result;

    friend bool operator==(const get_run_result_response&,
                           const get_run_result_response&) = default;
};

struct get_many_run_results_request {
    using response_type = struct get_many_run_results_response;
    static constexpr std::string_view nats_subject = "reporting.v1.run_results.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<run_result_key> keys;

    friend bool operator==(const get_many_run_results_request&,
                           const get_many_run_results_request&) = default;
};

struct get_many_run_results_response {
    ores::utility::domain::result result;
    std::vector<run_result_lookup> entries;

    friend bool operator==(const get_many_run_results_response&,
                           const get_many_run_results_response&) = default;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace run_result_event_subjects {
inline constexpr std::string_view created = "reporting.v1.events.run_results.created";
inline constexpr std::string_view updated = "reporting.v1.events.run_results.updated";
inline constexpr std::string_view deleted = "reporting.v1.events.run_results.deleted";
}
/**
 * @brief The members of run_result_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<run_result_key>) {
    return std::tuple{
        utility::codec::field{"id", &run_result_key::id},
    };
}

/**
 * @brief The members of run_result_lookup, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<run_result_lookup>) {
    return std::tuple{
        utility::codec::field{"key", &run_result_lookup::key},
        utility::codec::field{"run_result", &run_result_lookup::run_result},
    };
}

/**
 * @brief The members of run_results_filter, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<run_results_filter>) {
    return std::tuple{
        utility::codec::field{"party_id", &run_results_filter::party_id},
        utility::codec::field{"report_instance_id", &run_results_filter::report_instance_id},
        utility::codec::field{"report_definition_id", &run_results_filter::report_definition_id},
        utility::codec::field{"subject_type", &run_results_filter::subject_type},
        utility::codec::field{"subject_id", &run_results_filter::subject_id},
        utility::codec::field{"measure", &run_results_filter::measure},
        utility::codec::field{"currency", &run_results_filter::currency},
        utility::codec::field{"counterparty_id", &run_results_filter::counterparty_id},
        utility::codec::field{"id_one_of", &run_results_filter::id_one_of},
        utility::codec::field{"party_id_one_of", &run_results_filter::party_id_one_of},
        utility::codec::field{"report_instance_id_one_of",
                              &run_results_filter::report_instance_id_one_of},
        utility::codec::field{"report_definition_id_one_of",
                              &run_results_filter::report_definition_id_one_of},
        utility::codec::field{"subject_type_one_of", &run_results_filter::subject_type_one_of},
        utility::codec::field{"subject_id_one_of", &run_results_filter::subject_id_one_of},
        utility::codec::field{"measure_one_of", &run_results_filter::measure_one_of},
        utility::codec::field{"currency_one_of", &run_results_filter::currency_one_of},
        utility::codec::field{"counterparty_id_one_of",
                              &run_results_filter::counterparty_id_one_of},
    };
}

/**
 * @brief The members of run_result_event, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<run_result_event>) {
    return std::tuple{
        utility::codec::field{"event_id", &run_result_event::event_id},
        utility::codec::field{"key", &run_result_event::key},
        utility::codec::field{"action", &run_result_event::action},
        utility::codec::field{"version", &run_result_event::version},
        utility::codec::field{"occurred_at", &run_result_event::occurred_at},
        utility::codec::field{"correlation_id", &run_result_event::correlation_id},
    };
}

/**
 * @brief The members of list_run_results_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<list_run_results_request>) {
    return std::tuple{
        utility::codec::field{"offset", &list_run_results_request::offset},
        utility::codec::field{"limit", &list_run_results_request::limit},
        utility::codec::field{"order", &list_run_results_request::order},
        utility::codec::field{"filter", &list_run_results_request::filter},
    };
}

/**
 * @brief The members of list_run_results_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<list_run_results_response>) {
    return std::tuple{
        utility::codec::field{"result", &list_run_results_response::result},
        utility::codec::field{"results", &list_run_results_response::results},
        utility::codec::field{"total", &list_run_results_response::total},
    };
}

/**
 * @brief The members of get_run_result_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_run_result_request>) {
    return std::tuple{
        utility::codec::field{"key", &get_run_result_request::key},
    };
}

/**
 * @brief The members of get_run_result_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_run_result_response>) {
    return std::tuple{
        utility::codec::field{"result", &get_run_result_response::result},
        utility::codec::field{"run_result", &get_run_result_response::run_result},
    };
}

/**
 * @brief The members of get_many_run_results_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_many_run_results_request>) {
    return std::tuple{
        utility::codec::field{"keys", &get_many_run_results_request::keys},
    };
}

/**
 * @brief The members of get_many_run_results_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_many_run_results_response>) {
    return std::tuple{
        utility::codec::field{"result", &get_many_run_results_response::result},
        utility::codec::field{"entries", &get_many_run_results_response::entries},
    };
}


}
namespace ores::utility::codec {

template <>
struct ORES_REPORTING_API_EXPORT wire_traits<ores::reporting::messaging::run_result_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::reporting::messaging::run_result_key& v);
    static ores::reporting::messaging::run_result_key read_json(std::string_view text);
    static std::string write_msgpack(const ores::reporting::messaging::run_result_key& v);
    static ores::reporting::messaging::run_result_key read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT wire_traits<ores::reporting::messaging::run_result_lookup> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::reporting::messaging::run_result_lookup& v);
    static ores::reporting::messaging::run_result_lookup read_json(std::string_view text);
    static std::string write_msgpack(const ores::reporting::messaging::run_result_lookup& v);
    static ores::reporting::messaging::run_result_lookup
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT wire_traits<ores::reporting::messaging::run_results_filter> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::reporting::messaging::run_results_filter& v);
    static ores::reporting::messaging::run_results_filter read_json(std::string_view text);
    static std::string write_msgpack(const ores::reporting::messaging::run_results_filter& v);
    static ores::reporting::messaging::run_results_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT wire_traits<ores::reporting::messaging::run_result_event> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::reporting::messaging::run_result_event& v);
    static ores::reporting::messaging::run_result_event read_json(std::string_view text);
    static std::string write_msgpack(const ores::reporting::messaging::run_result_event& v);
    static ores::reporting::messaging::run_result_event
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT wire_traits<ores::reporting::messaging::list_run_results_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::reporting::messaging::list_run_results_request& v);
    static ores::reporting::messaging::list_run_results_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::reporting::messaging::list_run_results_request& v);
    static ores::reporting::messaging::list_run_results_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::list_run_results_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::reporting::messaging::list_run_results_response& v);
    static ores::reporting::messaging::list_run_results_response read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::list_run_results_response& v);
    static ores::reporting::messaging::list_run_results_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT wire_traits<ores::reporting::messaging::get_run_result_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::reporting::messaging::get_run_result_request& v);
    static ores::reporting::messaging::get_run_result_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::reporting::messaging::get_run_result_request& v);
    static ores::reporting::messaging::get_run_result_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT wire_traits<ores::reporting::messaging::get_run_result_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::reporting::messaging::get_run_result_response& v);
    static ores::reporting::messaging::get_run_result_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::reporting::messaging::get_run_result_response& v);
    static ores::reporting::messaging::get_run_result_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::get_many_run_results_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::get_many_run_results_request& v);
    static ores::reporting::messaging::get_many_run_results_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::get_many_run_results_request& v);
    static ores::reporting::messaging::get_many_run_results_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::get_many_run_results_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::get_many_run_results_response& v);
    static ores::reporting::messaging::get_many_run_results_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::get_many_run_results_response& v);
    static ores::reporting::messaging::get_many_run_results_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
