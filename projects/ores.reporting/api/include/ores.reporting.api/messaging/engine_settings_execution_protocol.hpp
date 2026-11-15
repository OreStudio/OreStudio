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
#ifndef ORES_REPORTING_API_MESSAGING_ENGINE_SETTINGS_EXECUTION_PROTOCOL_HPP
#define ORES_REPORTING_API_MESSAGING_ENGINE_SETTINGS_EXECUTION_PROTOCOL_HPP

#include "ores.reporting.api/domain/engine_settings_execution_codec.hpp"
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

struct engine_settings_execution_key {
    boost::uuids::uuid id;

    friend bool operator==(const engine_settings_execution_key&,
                           const engine_settings_execution_key&) = default;
};

struct engine_settings_execution_write {
    boost::uuids::uuid id;
    boost::uuids::uuid engine_settings_id;
    std::optional<int> n_threads;
    std::optional<int> log_mask;
    std::optional<int> gzip_compression_level;

    friend bool operator==(const engine_settings_execution_write&,
                           const engine_settings_execution_write&) = default;
};

struct engine_settings_execution_change {
    engine_settings_execution_write write;
    ores::utility::domain::precondition precondition;

    friend bool operator==(const engine_settings_execution_change&,
                           const engine_settings_execution_change&) = default;
};

struct engine_settings_execution_removal {
    engine_settings_execution_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;

    friend bool operator==(const engine_settings_execution_removal&,
                           const engine_settings_execution_removal&) = default;
};

struct engine_settings_execution_lookup {
    engine_settings_execution_key key;
    std::optional<ores::reporting::domain::engine_settings_execution> engine_settings_execution;

    friend bool operator==(const engine_settings_execution_lookup&,
                           const engine_settings_execution_lookup&) = default;
};

struct engine_settings_execution_filter {
    std::optional<boost::uuids::uuid> engine_settings_id;
    std::optional<std::vector<boost::uuids::uuid>> id_one_of;
    std::optional<std::vector<boost::uuids::uuid>> engine_settings_id_one_of;

    friend bool operator==(const engine_settings_execution_filter&,
                           const engine_settings_execution_filter&) = default;
};

struct engine_settings_execution_event {
    boost::uuids::uuid event_id;
    engine_settings_execution_key key;
    std::string action;
    std::uint32_t version{};
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;

    friend bool operator==(const engine_settings_execution_event&,
                           const engine_settings_execution_event&) = default;
};

struct engine_settings_execution_version_key {
    engine_settings_execution_key engine_settings_execution;
    std::uint32_t version{};

    friend bool operator==(const engine_settings_execution_version_key&,
                           const engine_settings_execution_version_key&) = default;
};

struct engine_settings_execution_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;

    friend bool operator==(const engine_settings_execution_versions_filter&,
                           const engine_settings_execution_versions_filter&) = default;
};

struct list_engine_settings_execution_request {
    using response_type = struct list_engine_settings_execution_response;
    static constexpr std::string_view nats_subject = "reporting.v1.engine_settings_execution.list";
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
    std::optional<engine_settings_execution_filter> filter;
    std::optional<std::string> as_of;

    friend bool operator==(const list_engine_settings_execution_request&,
                           const list_engine_settings_execution_request&) = default;
};

struct list_engine_settings_execution_response {
    ores::utility::domain::result result;
    std::vector<ores::reporting::domain::engine_settings_execution> engine_settings_execution;
    std::uint64_t total{};

    friend bool operator==(const list_engine_settings_execution_response&,
                           const list_engine_settings_execution_response&) = default;
};

struct get_engine_settings_execution_request {
    using response_type = struct get_engine_settings_execution_response;
    static constexpr std::string_view nats_subject = "reporting.v1.engine_settings_execution.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    engine_settings_execution_key key;

    friend bool operator==(const get_engine_settings_execution_request&,
                           const get_engine_settings_execution_request&) = default;
};

struct get_engine_settings_execution_response {
    ores::utility::domain::result result;
    std::optional<ores::reporting::domain::engine_settings_execution> engine_settings_execution;

    friend bool operator==(const get_engine_settings_execution_response&,
                           const get_engine_settings_execution_response&) = default;
};

struct get_many_engine_settings_execution_request {
    using response_type = struct get_many_engine_settings_execution_response;
    static constexpr std::string_view nats_subject =
        "reporting.v1.engine_settings_execution.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<engine_settings_execution_key> keys;

    friend bool operator==(const get_many_engine_settings_execution_request&,
                           const get_many_engine_settings_execution_request&) = default;
};

struct get_many_engine_settings_execution_response {
    ores::utility::domain::result result;
    std::vector<engine_settings_execution_lookup> entries;

    friend bool operator==(const get_many_engine_settings_execution_response&,
                           const get_many_engine_settings_execution_response&) = default;
};

struct put_engine_settings_execution_request {
    using response_type = struct put_engine_settings_execution_response;
    static constexpr std::string_view nats_subject = "reporting.v1.engine_settings_execution.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    engine_settings_execution_change change;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const put_engine_settings_execution_request&,
                           const put_engine_settings_execution_request&) = default;
};

struct put_engine_settings_execution_response {
    ores::utility::domain::result result;
    std::optional<ores::reporting::domain::engine_settings_execution> engine_settings_execution;

    friend bool operator==(const put_engine_settings_execution_response&,
                           const put_engine_settings_execution_response&) = default;
};

struct put_many_engine_settings_execution_request {
    using response_type = struct put_many_engine_settings_execution_response;
    static constexpr std::string_view nats_subject =
        "reporting.v1.engine_settings_execution.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<engine_settings_execution_change> changes;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const put_many_engine_settings_execution_request&,
                           const put_many_engine_settings_execution_request&) = default;
};

struct put_many_engine_settings_execution_response {
    ores::utility::domain::result result;
    std::vector<ores::reporting::domain::engine_settings_execution> engine_settings_execution;

    friend bool operator==(const put_many_engine_settings_execution_response&,
                           const put_many_engine_settings_execution_response&) = default;
};

struct delete_engine_settings_execution_request {
    using response_type = struct delete_engine_settings_execution_response;
    static constexpr std::string_view nats_subject =
        "reporting.v1.engine_settings_execution.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    engine_settings_execution_removal removal;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const delete_engine_settings_execution_request&,
                           const delete_engine_settings_execution_request&) = default;
};

struct delete_engine_settings_execution_response {
    ores::utility::domain::result result;

    friend bool operator==(const delete_engine_settings_execution_response&,
                           const delete_engine_settings_execution_response&) = default;
};

struct delete_many_engine_settings_execution_request {
    using response_type = struct delete_many_engine_settings_execution_response;
    static constexpr std::string_view nats_subject =
        "reporting.v1.engine_settings_execution.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<engine_settings_execution_removal> removals;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const delete_many_engine_settings_execution_request&,
                           const delete_many_engine_settings_execution_request&) = default;
};

struct delete_many_engine_settings_execution_response {
    ores::utility::domain::result result;

    friend bool operator==(const delete_many_engine_settings_execution_response&,
                           const delete_many_engine_settings_execution_response&) = default;
};

struct list_by_engine_settings_id_engine_settings_execution_request {
    using response_type = struct list_by_engine_settings_id_engine_settings_execution_response;
    static constexpr std::string_view nats_subject =
        "reporting.v1.engine_settings_execution.list_by_engine_settings_id";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    boost::uuids::uuid engine_settings_id;
    ores::utility::domain::scope scope = ores::utility::domain::scope::direct;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<engine_settings_execution_filter> filter;

    friend bool
    operator==(const list_by_engine_settings_id_engine_settings_execution_request&,
               const list_by_engine_settings_id_engine_settings_execution_request&) = default;
};

struct list_by_engine_settings_id_engine_settings_execution_response {
    ores::utility::domain::result result;
    std::vector<ores::reporting::domain::engine_settings_execution> engine_settings_execution;
    std::uint64_t total{};

    friend bool
    operator==(const list_by_engine_settings_id_engine_settings_execution_response&,
               const list_by_engine_settings_id_engine_settings_execution_response&) = default;
};

struct list_engine_settings_execution_versions_request {
    using response_type = struct list_engine_settings_execution_versions_response;
    static constexpr std::string_view nats_subject =
        "reporting.v1.engine_settings_execution_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    engine_settings_execution_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<engine_settings_execution_versions_filter> filter;

    friend bool operator==(const list_engine_settings_execution_versions_request&,
                           const list_engine_settings_execution_versions_request&) = default;
};

struct list_engine_settings_execution_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::reporting::domain::engine_settings_execution> versions;
    std::uint64_t total{};

    friend bool operator==(const list_engine_settings_execution_versions_response&,
                           const list_engine_settings_execution_versions_response&) = default;
};

struct get_engine_settings_execution_version_request {
    using response_type = struct get_engine_settings_execution_version_response;
    static constexpr std::string_view nats_subject =
        "reporting.v1.engine_settings_execution_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    engine_settings_execution_version_key key;

    friend bool operator==(const get_engine_settings_execution_version_request&,
                           const get_engine_settings_execution_version_request&) = default;
};

struct get_engine_settings_execution_version_response {
    ores::utility::domain::result result;
    std::optional<ores::reporting::domain::engine_settings_execution> version;

    friend bool operator==(const get_engine_settings_execution_version_response&,
                           const get_engine_settings_execution_version_response&) = default;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace engine_settings_execution_event_subjects {
inline constexpr std::string_view created = "reporting.v1.events.engine_settings_execution.created";
inline constexpr std::string_view updated = "reporting.v1.events.engine_settings_execution.updated";
inline constexpr std::string_view deleted = "reporting.v1.events.engine_settings_execution.deleted";
}
/**
 * @brief The members of engine_settings_execution_key, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<engine_settings_execution_key>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"id", &engine_settings_execution_key::id},
    };
}

/**
 * @brief The members of engine_settings_execution_write, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<engine_settings_execution_write>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"id", &engine_settings_execution_write::id},
        utility::codec::field{"engine_settings_id",
                              &engine_settings_execution_write::engine_settings_id},
        utility::codec::field{"n_threads", &engine_settings_execution_write::n_threads},
        utility::codec::field{"log_mask", &engine_settings_execution_write::log_mask},
        utility::codec::field{"gzip_compression_level",
                              &engine_settings_execution_write::gzip_compression_level},
    };
}

/**
 * @brief The members of engine_settings_execution_change, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<engine_settings_execution_change>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"write", &engine_settings_execution_change::write},
        utility::codec::field{"precondition", &engine_settings_execution_change::precondition},
    };
}

/**
 * @brief The members of engine_settings_execution_removal, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<engine_settings_execution_removal>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"key", &engine_settings_execution_removal::key},
        utility::codec::field{"precondition", &engine_settings_execution_removal::precondition},
    };
}

/**
 * @brief The members of engine_settings_execution_lookup, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<engine_settings_execution_lookup>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"key", &engine_settings_execution_lookup::key},
        utility::codec::field{"engine_settings_execution",
                              &engine_settings_execution_lookup::engine_settings_execution},
    };
}

/**
 * @brief The members of engine_settings_execution_filter, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<engine_settings_execution_filter>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"engine_settings_id",
                              &engine_settings_execution_filter::engine_settings_id},
        utility::codec::field{"id_one_of", &engine_settings_execution_filter::id_one_of},
        utility::codec::field{"engine_settings_id_one_of",
                              &engine_settings_execution_filter::engine_settings_id_one_of},
    };
}

/**
 * @brief The members of engine_settings_execution_event, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<engine_settings_execution_event>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"event_id", &engine_settings_execution_event::event_id},
        utility::codec::field{"key", &engine_settings_execution_event::key},
        utility::codec::field{"action", &engine_settings_execution_event::action},
        utility::codec::field{"version", &engine_settings_execution_event::version},
        utility::codec::field{"occurred_at", &engine_settings_execution_event::occurred_at},
        utility::codec::field{"correlation_id", &engine_settings_execution_event::correlation_id},
    };
}

/**
 * @brief The members of engine_settings_execution_version_key, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<engine_settings_execution_version_key>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"engine_settings_execution",
                              &engine_settings_execution_version_key::engine_settings_execution},
        utility::codec::field{"version", &engine_settings_execution_version_key::version},
    };
}

/**
 * @brief The members of engine_settings_execution_versions_filter, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<engine_settings_execution_versions_filter>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"version", &engine_settings_execution_versions_filter::version},
        utility::codec::field{"from_version",
                              &engine_settings_execution_versions_filter::from_version},
        utility::codec::field{"to_version", &engine_settings_execution_versions_filter::to_version},
    };
}

/**
 * @brief The members of list_engine_settings_execution_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<list_engine_settings_execution_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"offset", &list_engine_settings_execution_request::offset},
        utility::codec::field{"limit", &list_engine_settings_execution_request::limit},
        utility::codec::field{"order", &list_engine_settings_execution_request::order},
        utility::codec::field{"filter", &list_engine_settings_execution_request::filter},
        utility::codec::field{"as_of", &list_engine_settings_execution_request::as_of},
    };
}

/**
 * @brief The members of list_engine_settings_execution_response, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<list_engine_settings_execution_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &list_engine_settings_execution_response::result},
        utility::codec::field{"engine_settings_execution",
                              &list_engine_settings_execution_response::engine_settings_execution},
        utility::codec::field{"total", &list_engine_settings_execution_response::total},
    };
}

/**
 * @brief The members of get_engine_settings_execution_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<get_engine_settings_execution_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"key", &get_engine_settings_execution_request::key},
    };
}

/**
 * @brief The members of get_engine_settings_execution_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<get_engine_settings_execution_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &get_engine_settings_execution_response::result},
        utility::codec::field{"engine_settings_execution",
                              &get_engine_settings_execution_response::engine_settings_execution},
    };
}

/**
 * @brief The members of get_many_engine_settings_execution_request, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<get_many_engine_settings_execution_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"keys", &get_many_engine_settings_execution_request::keys},
    };
}

/**
 * @brief The members of get_many_engine_settings_execution_response, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<get_many_engine_settings_execution_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &get_many_engine_settings_execution_response::result},
        utility::codec::field{"entries", &get_many_engine_settings_execution_response::entries},
    };
}

/**
 * @brief The members of put_engine_settings_execution_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<put_engine_settings_execution_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"change", &put_engine_settings_execution_request::change},
        utility::codec::field{"intent", &put_engine_settings_execution_request::intent},
    };
}

/**
 * @brief The members of put_engine_settings_execution_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<put_engine_settings_execution_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &put_engine_settings_execution_response::result},
        utility::codec::field{"engine_settings_execution",
                              &put_engine_settings_execution_response::engine_settings_execution},
    };
}

/**
 * @brief The members of put_many_engine_settings_execution_request, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<put_many_engine_settings_execution_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"changes", &put_many_engine_settings_execution_request::changes},
        utility::codec::field{"intent", &put_many_engine_settings_execution_request::intent},
    };
}

/**
 * @brief The members of put_many_engine_settings_execution_response, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<put_many_engine_settings_execution_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &put_many_engine_settings_execution_response::result},
        utility::codec::field{
            "engine_settings_execution",
            &put_many_engine_settings_execution_response::engine_settings_execution},
    };
}

/**
 * @brief The members of delete_engine_settings_execution_request, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<delete_engine_settings_execution_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"removal", &delete_engine_settings_execution_request::removal},
        utility::codec::field{"intent", &delete_engine_settings_execution_request::intent},
    };
}

/**
 * @brief The members of delete_engine_settings_execution_response, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<delete_engine_settings_execution_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &delete_engine_settings_execution_response::result},
    };
}

/**
 * @brief The members of delete_many_engine_settings_execution_request, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<delete_many_engine_settings_execution_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"removals", &delete_many_engine_settings_execution_request::removals},
        utility::codec::field{"intent", &delete_many_engine_settings_execution_request::intent},
    };
}

/**
 * @brief The members of delete_many_engine_settings_execution_response, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<delete_many_engine_settings_execution_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &delete_many_engine_settings_execution_response::result},
    };
}

/**
 * @brief The members of list_by_engine_settings_id_engine_settings_execution_request, named as the
 * wire names them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<list_by_engine_settings_id_engine_settings_execution_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{
            "engine_settings_id",
            &list_by_engine_settings_id_engine_settings_execution_request::engine_settings_id},
        utility::codec::field{"scope",
                              &list_by_engine_settings_id_engine_settings_execution_request::scope},
        utility::codec::field{
            "offset", &list_by_engine_settings_id_engine_settings_execution_request::offset},
        utility::codec::field{"limit",
                              &list_by_engine_settings_id_engine_settings_execution_request::limit},
        utility::codec::field{"order",
                              &list_by_engine_settings_id_engine_settings_execution_request::order},
        utility::codec::field{
            "filter", &list_by_engine_settings_id_engine_settings_execution_request::filter},
    };
}

/**
 * @brief The members of list_by_engine_settings_id_engine_settings_execution_response, named as the
 * wire names them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<list_by_engine_settings_id_engine_settings_execution_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{
            "result", &list_by_engine_settings_id_engine_settings_execution_response::result},
        utility::codec::field{"engine_settings_execution",
                              &list_by_engine_settings_id_engine_settings_execution_response::
                                  engine_settings_execution},
        utility::codec::field{
            "total", &list_by_engine_settings_id_engine_settings_execution_response::total},
    };
}

/**
 * @brief The members of list_engine_settings_execution_versions_request, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<list_engine_settings_execution_versions_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"key", &list_engine_settings_execution_versions_request::key},
        utility::codec::field{"offset", &list_engine_settings_execution_versions_request::offset},
        utility::codec::field{"limit", &list_engine_settings_execution_versions_request::limit},
        utility::codec::field{"order", &list_engine_settings_execution_versions_request::order},
        utility::codec::field{"filter", &list_engine_settings_execution_versions_request::filter},
    };
}

/**
 * @brief The members of list_engine_settings_execution_versions_response, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<list_engine_settings_execution_versions_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &list_engine_settings_execution_versions_response::result},
        utility::codec::field{"versions",
                              &list_engine_settings_execution_versions_response::versions},
        utility::codec::field{"total", &list_engine_settings_execution_versions_response::total},
    };
}

/**
 * @brief The members of get_engine_settings_execution_version_request, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<get_engine_settings_execution_version_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"key", &get_engine_settings_execution_version_request::key},
    };
}

/**
 * @brief The members of get_engine_settings_execution_version_response, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<get_engine_settings_execution_version_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &get_engine_settings_execution_version_response::result},
        utility::codec::field{"version", &get_engine_settings_execution_version_response::version},
    };
}


}
namespace ores::utility::codec {

template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::engine_settings_execution_key> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::engine_settings_execution_key& v);
    static ores::reporting::messaging::engine_settings_execution_key
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::engine_settings_execution_key& v);
    static ores::reporting::messaging::engine_settings_execution_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::engine_settings_execution_write> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::engine_settings_execution_write& v);
    static ores::reporting::messaging::engine_settings_execution_write
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::engine_settings_execution_write& v);
    static ores::reporting::messaging::engine_settings_execution_write
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::engine_settings_execution_change> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::engine_settings_execution_change& v);
    static ores::reporting::messaging::engine_settings_execution_change
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::engine_settings_execution_change& v);
    static ores::reporting::messaging::engine_settings_execution_change
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::engine_settings_execution_removal> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::engine_settings_execution_removal& v);
    static ores::reporting::messaging::engine_settings_execution_removal
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::engine_settings_execution_removal& v);
    static ores::reporting::messaging::engine_settings_execution_removal
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::engine_settings_execution_lookup> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::engine_settings_execution_lookup& v);
    static ores::reporting::messaging::engine_settings_execution_lookup
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::engine_settings_execution_lookup& v);
    static ores::reporting::messaging::engine_settings_execution_lookup
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::engine_settings_execution_filter> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::engine_settings_execution_filter& v);
    static ores::reporting::messaging::engine_settings_execution_filter
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::engine_settings_execution_filter& v);
    static ores::reporting::messaging::engine_settings_execution_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::engine_settings_execution_event> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::engine_settings_execution_event& v);
    static ores::reporting::messaging::engine_settings_execution_event
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::engine_settings_execution_event& v);
    static ores::reporting::messaging::engine_settings_execution_event
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::engine_settings_execution_version_key> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::engine_settings_execution_version_key& v);
    static ores::reporting::messaging::engine_settings_execution_version_key
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::engine_settings_execution_version_key& v);
    static ores::reporting::messaging::engine_settings_execution_version_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::engine_settings_execution_versions_filter> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::engine_settings_execution_versions_filter& v);
    static ores::reporting::messaging::engine_settings_execution_versions_filter
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::engine_settings_execution_versions_filter& v);
    static ores::reporting::messaging::engine_settings_execution_versions_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::list_engine_settings_execution_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::list_engine_settings_execution_request& v);
    static ores::reporting::messaging::list_engine_settings_execution_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::list_engine_settings_execution_request& v);
    static ores::reporting::messaging::list_engine_settings_execution_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::list_engine_settings_execution_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::list_engine_settings_execution_response& v);
    static ores::reporting::messaging::list_engine_settings_execution_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::list_engine_settings_execution_response& v);
    static ores::reporting::messaging::list_engine_settings_execution_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::get_engine_settings_execution_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::get_engine_settings_execution_request& v);
    static ores::reporting::messaging::get_engine_settings_execution_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::get_engine_settings_execution_request& v);
    static ores::reporting::messaging::get_engine_settings_execution_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::get_engine_settings_execution_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::get_engine_settings_execution_response& v);
    static ores::reporting::messaging::get_engine_settings_execution_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::get_engine_settings_execution_response& v);
    static ores::reporting::messaging::get_engine_settings_execution_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::get_many_engine_settings_execution_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::get_many_engine_settings_execution_request& v);
    static ores::reporting::messaging::get_many_engine_settings_execution_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::get_many_engine_settings_execution_request& v);
    static ores::reporting::messaging::get_many_engine_settings_execution_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::get_many_engine_settings_execution_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::get_many_engine_settings_execution_response& v);
    static ores::reporting::messaging::get_many_engine_settings_execution_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::get_many_engine_settings_execution_response& v);
    static ores::reporting::messaging::get_many_engine_settings_execution_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::put_engine_settings_execution_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::put_engine_settings_execution_request& v);
    static ores::reporting::messaging::put_engine_settings_execution_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::put_engine_settings_execution_request& v);
    static ores::reporting::messaging::put_engine_settings_execution_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::put_engine_settings_execution_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::put_engine_settings_execution_response& v);
    static ores::reporting::messaging::put_engine_settings_execution_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::put_engine_settings_execution_response& v);
    static ores::reporting::messaging::put_engine_settings_execution_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::put_many_engine_settings_execution_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::put_many_engine_settings_execution_request& v);
    static ores::reporting::messaging::put_many_engine_settings_execution_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::put_many_engine_settings_execution_request& v);
    static ores::reporting::messaging::put_many_engine_settings_execution_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::put_many_engine_settings_execution_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::put_many_engine_settings_execution_response& v);
    static ores::reporting::messaging::put_many_engine_settings_execution_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::put_many_engine_settings_execution_response& v);
    static ores::reporting::messaging::put_many_engine_settings_execution_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::delete_engine_settings_execution_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::delete_engine_settings_execution_request& v);
    static ores::reporting::messaging::delete_engine_settings_execution_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::delete_engine_settings_execution_request& v);
    static ores::reporting::messaging::delete_engine_settings_execution_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::delete_engine_settings_execution_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::delete_engine_settings_execution_response& v);
    static ores::reporting::messaging::delete_engine_settings_execution_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::delete_engine_settings_execution_response& v);
    static ores::reporting::messaging::delete_engine_settings_execution_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::delete_many_engine_settings_execution_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::delete_many_engine_settings_execution_request& v);
    static ores::reporting::messaging::delete_many_engine_settings_execution_request
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::reporting::messaging::delete_many_engine_settings_execution_request& v);
    static ores::reporting::messaging::delete_many_engine_settings_execution_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::delete_many_engine_settings_execution_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::delete_many_engine_settings_execution_response& v);
    static ores::reporting::messaging::delete_many_engine_settings_execution_response
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::reporting::messaging::delete_many_engine_settings_execution_response& v);
    static ores::reporting::messaging::delete_many_engine_settings_execution_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT wire_traits<
    ores::reporting::messaging::list_by_engine_settings_id_engine_settings_execution_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::
                   list_by_engine_settings_id_engine_settings_execution_request& v);
    static ores::reporting::messaging::list_by_engine_settings_id_engine_settings_execution_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::
                      list_by_engine_settings_id_engine_settings_execution_request& v);
    static ores::reporting::messaging::list_by_engine_settings_id_engine_settings_execution_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT wire_traits<
    ores::reporting::messaging::list_by_engine_settings_id_engine_settings_execution_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::
                   list_by_engine_settings_id_engine_settings_execution_response& v);
    static ores::reporting::messaging::list_by_engine_settings_id_engine_settings_execution_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::
                      list_by_engine_settings_id_engine_settings_execution_response& v);
    static ores::reporting::messaging::list_by_engine_settings_id_engine_settings_execution_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::list_engine_settings_execution_versions_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::reporting::messaging::list_engine_settings_execution_versions_request& v);
    static ores::reporting::messaging::list_engine_settings_execution_versions_request
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::reporting::messaging::list_engine_settings_execution_versions_request& v);
    static ores::reporting::messaging::list_engine_settings_execution_versions_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::list_engine_settings_execution_versions_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::reporting::messaging::list_engine_settings_execution_versions_response& v);
    static ores::reporting::messaging::list_engine_settings_execution_versions_response
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::reporting::messaging::list_engine_settings_execution_versions_response& v);
    static ores::reporting::messaging::list_engine_settings_execution_versions_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::get_engine_settings_execution_version_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::get_engine_settings_execution_version_request& v);
    static ores::reporting::messaging::get_engine_settings_execution_version_request
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::reporting::messaging::get_engine_settings_execution_version_request& v);
    static ores::reporting::messaging::get_engine_settings_execution_version_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::get_engine_settings_execution_version_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::get_engine_settings_execution_version_response& v);
    static ores::reporting::messaging::get_engine_settings_execution_version_response
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::reporting::messaging::get_engine_settings_execution_version_response& v);
    static ores::reporting::messaging::get_engine_settings_execution_version_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
