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
#ifndef ORES_COMPUTE_API_MESSAGING_RESULT_PROTOCOL_HPP
#define ORES_COMPUTE_API_MESSAGING_RESULT_PROTOCOL_HPP

#include "ores.compute.api/domain/result.hpp"
#include "ores.compute.api/export.hpp"
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

namespace ores::compute::messaging {

struct result_key {
    boost::uuids::uuid id;

    friend bool operator==(const result_key&, const result_key&) = default;
};

struct result_write {
    boost::uuids::uuid id;
    boost::uuids::uuid workunit_id;
    boost::uuids::uuid host_id;
    int server_state;
    int outcome;
    std::string output_uri;
    std::string error_message;
    std::chrono::system_clock::time_point received_at;
    std::optional<std::int64_t> duration_ms;

    friend bool operator==(const result_write&, const result_write&) = default;
};

struct result_change {
    result_write write;
    ores::utility::domain::precondition precondition;

    friend bool operator==(const result_change&, const result_change&) = default;
};

struct result_removal {
    result_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;

    friend bool operator==(const result_removal&, const result_removal&) = default;
};

struct result_lookup {
    result_key key;
    std::optional<ores::compute::domain::result> result;

    friend bool operator==(const result_lookup&, const result_lookup&) = default;
};

struct results_filter {
    std::optional<boost::uuids::uuid> workunit_id;
    std::optional<int> outcome;
    std::optional<std::vector<boost::uuids::uuid>> id_one_of;
    std::optional<std::vector<boost::uuids::uuid>> workunit_id_one_of;
    std::optional<std::vector<int>> outcome_one_of;

    friend bool operator==(const results_filter&, const results_filter&) = default;
};

struct result_event {
    boost::uuids::uuid event_id;
    result_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;

    friend bool operator==(const result_event&, const result_event&) = default;
};

struct result_version_key {
    result_key result;
    std::uint32_t version;

    friend bool operator==(const result_version_key&, const result_version_key&) = default;
};

struct result_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;

    friend bool operator==(const result_versions_filter&, const result_versions_filter&) = default;
};

struct list_results_request {
    using response_type = struct list_results_response;
    static constexpr std::string_view nats_subject = "compute.v1.results.list";
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
    std::optional<results_filter> filter;
    std::optional<std::string> as_of;

    friend bool operator==(const list_results_request&, const list_results_request&) = default;
};

struct list_results_response {
    ores::utility::domain::result result;
    std::vector<ores::compute::domain::result> results;
    std::uint64_t total;

    friend bool operator==(const list_results_response&, const list_results_response&) = default;
};

struct get_result_request {
    using response_type = struct get_result_response;
    static constexpr std::string_view nats_subject = "compute.v1.results.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    result_key key;

    friend bool operator==(const get_result_request&, const get_result_request&) = default;
};

struct get_result_response {
    ores::utility::domain::result result;
    std::optional<ores::compute::domain::result> result_value;

    friend bool operator==(const get_result_response&, const get_result_response&) = default;
};

struct get_many_results_request {
    using response_type = struct get_many_results_response;
    static constexpr std::string_view nats_subject = "compute.v1.results.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<result_key> keys;

    friend bool operator==(const get_many_results_request&,
                           const get_many_results_request&) = default;
};

struct get_many_results_response {
    ores::utility::domain::result result;
    std::vector<result_lookup> entries;

    friend bool operator==(const get_many_results_response&,
                           const get_many_results_response&) = default;
};

struct put_result_request {
    using response_type = struct put_result_response;
    static constexpr std::string_view nats_subject = "compute.v1.results.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    result_change change;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const put_result_request&, const put_result_request&) = default;
};

struct put_result_response {
    ores::utility::domain::result result;
    std::optional<ores::compute::domain::result> result_value;

    friend bool operator==(const put_result_response&, const put_result_response&) = default;
};

struct put_many_results_request {
    using response_type = struct put_many_results_response;
    static constexpr std::string_view nats_subject = "compute.v1.results.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<result_change> changes;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const put_many_results_request&,
                           const put_many_results_request&) = default;
};

struct put_many_results_response {
    ores::utility::domain::result result;
    std::vector<ores::compute::domain::result> results;

    friend bool operator==(const put_many_results_response&,
                           const put_many_results_response&) = default;
};

struct delete_result_request {
    using response_type = struct delete_result_response;
    static constexpr std::string_view nats_subject = "compute.v1.results.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    result_removal removal;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const delete_result_request&, const delete_result_request&) = default;
};

struct delete_result_response {
    ores::utility::domain::result result;

    friend bool operator==(const delete_result_response&, const delete_result_response&) = default;
};

struct delete_many_results_request {
    using response_type = struct delete_many_results_response;
    static constexpr std::string_view nats_subject = "compute.v1.results.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<result_removal> removals;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const delete_many_results_request&,
                           const delete_many_results_request&) = default;
};

struct delete_many_results_response {
    ores::utility::domain::result result;

    friend bool operator==(const delete_many_results_response&,
                           const delete_many_results_response&) = default;
};

struct list_by_workunit_id_results_request {
    using response_type = struct list_by_workunit_id_results_response;
    static constexpr std::string_view nats_subject = "compute.v1.results.list_by_workunit_id";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    boost::uuids::uuid workunit_id;
    ores::utility::domain::scope scope = ores::utility::domain::scope::direct;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<results_filter> filter;

    friend bool operator==(const list_by_workunit_id_results_request&,
                           const list_by_workunit_id_results_request&) = default;
};

struct list_by_workunit_id_results_response {
    ores::utility::domain::result result;
    std::vector<ores::compute::domain::result> results;
    std::uint64_t total;

    friend bool operator==(const list_by_workunit_id_results_response&,
                           const list_by_workunit_id_results_response&) = default;
};

struct list_result_versions_request {
    using response_type = struct list_result_versions_response;
    static constexpr std::string_view nats_subject = "compute.v1.results_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    result_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<result_versions_filter> filter;

    friend bool operator==(const list_result_versions_request&,
                           const list_result_versions_request&) = default;
};

struct list_result_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::compute::domain::result> versions;
    std::uint64_t total;

    friend bool operator==(const list_result_versions_response&,
                           const list_result_versions_response&) = default;
};

struct get_result_version_request {
    using response_type = struct get_result_version_response;
    static constexpr std::string_view nats_subject = "compute.v1.results_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    result_version_key key;

    friend bool operator==(const get_result_version_request&,
                           const get_result_version_request&) = default;
};

struct get_result_version_response {
    ores::utility::domain::result result;
    std::optional<ores::compute::domain::result> version;

    friend bool operator==(const get_result_version_response&,
                           const get_result_version_response&) = default;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace result_event_subjects {
inline constexpr std::string_view created = "compute.v1.events.results.created";
inline constexpr std::string_view updated = "compute.v1.events.results.updated";
inline constexpr std::string_view deleted = "compute.v1.events.results.deleted";
}
/**
 * @brief The members of result_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<result_key>) {
    return std::tuple{
        utility::codec::field{"id", &result_key::id},
    };
}

/**
 * @brief The members of result_write, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<result_write>) {
    return std::tuple{
        utility::codec::field{"id", &result_write::id},
        utility::codec::field{"workunit_id", &result_write::workunit_id},
        utility::codec::field{"host_id", &result_write::host_id},
        utility::codec::field{"server_state", &result_write::server_state},
        utility::codec::field{"outcome", &result_write::outcome},
        utility::codec::field{"output_uri", &result_write::output_uri},
        utility::codec::field{"error_message", &result_write::error_message},
        utility::codec::field{"received_at", &result_write::received_at},
        utility::codec::field{"duration_ms", &result_write::duration_ms},
    };
}

/**
 * @brief The members of result_change, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<result_change>) {
    return std::tuple{
        utility::codec::field{"write", &result_change::write},
        utility::codec::field{"precondition", &result_change::precondition},
    };
}

/**
 * @brief The members of result_removal, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<result_removal>) {
    return std::tuple{
        utility::codec::field{"key", &result_removal::key},
        utility::codec::field{"precondition", &result_removal::precondition},
    };
}

/**
 * @brief The members of result_lookup, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<result_lookup>) {
    return std::tuple{
        utility::codec::field{"key", &result_lookup::key},
        utility::codec::field{"result", &result_lookup::result},
    };
}

/**
 * @brief The members of results_filter, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<results_filter>) {
    return std::tuple{
        utility::codec::field{"workunit_id", &results_filter::workunit_id},
        utility::codec::field{"outcome", &results_filter::outcome},
        utility::codec::field{"id_one_of", &results_filter::id_one_of},
        utility::codec::field{"workunit_id_one_of", &results_filter::workunit_id_one_of},
        utility::codec::field{"outcome_one_of", &results_filter::outcome_one_of},
    };
}

/**
 * @brief The members of result_event, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<result_event>) {
    return std::tuple{
        utility::codec::field{"event_id", &result_event::event_id},
        utility::codec::field{"key", &result_event::key},
        utility::codec::field{"action", &result_event::action},
        utility::codec::field{"version", &result_event::version},
        utility::codec::field{"occurred_at", &result_event::occurred_at},
        utility::codec::field{"correlation_id", &result_event::correlation_id},
    };
}

/**
 * @brief The members of result_version_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<result_version_key>) {
    return std::tuple{
        utility::codec::field{"result", &result_version_key::result},
        utility::codec::field{"version", &result_version_key::version},
    };
}

/**
 * @brief The members of result_versions_filter, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<result_versions_filter>) {
    return std::tuple{
        utility::codec::field{"version", &result_versions_filter::version},
        utility::codec::field{"from_version", &result_versions_filter::from_version},
        utility::codec::field{"to_version", &result_versions_filter::to_version},
    };
}

/**
 * @brief The members of list_results_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<list_results_request>) {
    return std::tuple{
        utility::codec::field{"offset", &list_results_request::offset},
        utility::codec::field{"limit", &list_results_request::limit},
        utility::codec::field{"order", &list_results_request::order},
        utility::codec::field{"filter", &list_results_request::filter},
        utility::codec::field{"as_of", &list_results_request::as_of},
    };
}

/**
 * @brief The members of list_results_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<list_results_response>) {
    return std::tuple{
        utility::codec::field{"result", &list_results_response::result},
        utility::codec::field{"results", &list_results_response::results},
        utility::codec::field{"total", &list_results_response::total},
    };
}

/**
 * @brief The members of get_result_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_result_request>) {
    return std::tuple{
        utility::codec::field{"key", &get_result_request::key},
    };
}

/**
 * @brief The members of get_result_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_result_response>) {
    return std::tuple{
        utility::codec::field{"result", &get_result_response::result},
        utility::codec::field{"result_value", &get_result_response::result_value},
    };
}

/**
 * @brief The members of get_many_results_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_many_results_request>) {
    return std::tuple{
        utility::codec::field{"keys", &get_many_results_request::keys},
    };
}

/**
 * @brief The members of get_many_results_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_many_results_response>) {
    return std::tuple{
        utility::codec::field{"result", &get_many_results_response::result},
        utility::codec::field{"entries", &get_many_results_response::entries},
    };
}

/**
 * @brief The members of put_result_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<put_result_request>) {
    return std::tuple{
        utility::codec::field{"change", &put_result_request::change},
        utility::codec::field{"intent", &put_result_request::intent},
    };
}

/**
 * @brief The members of put_result_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<put_result_response>) {
    return std::tuple{
        utility::codec::field{"result", &put_result_response::result},
        utility::codec::field{"result_value", &put_result_response::result_value},
    };
}

/**
 * @brief The members of put_many_results_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<put_many_results_request>) {
    return std::tuple{
        utility::codec::field{"changes", &put_many_results_request::changes},
        utility::codec::field{"intent", &put_many_results_request::intent},
    };
}

/**
 * @brief The members of put_many_results_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<put_many_results_response>) {
    return std::tuple{
        utility::codec::field{"result", &put_many_results_response::result},
        utility::codec::field{"results", &put_many_results_response::results},
    };
}

/**
 * @brief The members of delete_result_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<delete_result_request>) {
    return std::tuple{
        utility::codec::field{"removal", &delete_result_request::removal},
        utility::codec::field{"intent", &delete_result_request::intent},
    };
}

/**
 * @brief The members of delete_result_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<delete_result_response>) {
    return std::tuple{
        utility::codec::field{"result", &delete_result_response::result},
    };
}

/**
 * @brief The members of delete_many_results_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<delete_many_results_request>) {
    return std::tuple{
        utility::codec::field{"removals", &delete_many_results_request::removals},
        utility::codec::field{"intent", &delete_many_results_request::intent},
    };
}

/**
 * @brief The members of delete_many_results_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<delete_many_results_response>) {
    return std::tuple{
        utility::codec::field{"result", &delete_many_results_response::result},
    };
}

/**
 * @brief The members of list_by_workunit_id_results_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<list_by_workunit_id_results_request>) {
    return std::tuple{
        utility::codec::field{"workunit_id", &list_by_workunit_id_results_request::workunit_id},
        utility::codec::field{"scope", &list_by_workunit_id_results_request::scope},
        utility::codec::field{"offset", &list_by_workunit_id_results_request::offset},
        utility::codec::field{"limit", &list_by_workunit_id_results_request::limit},
        utility::codec::field{"order", &list_by_workunit_id_results_request::order},
        utility::codec::field{"filter", &list_by_workunit_id_results_request::filter},
    };
}

/**
 * @brief The members of list_by_workunit_id_results_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<list_by_workunit_id_results_response>) {
    return std::tuple{
        utility::codec::field{"result", &list_by_workunit_id_results_response::result},
        utility::codec::field{"results", &list_by_workunit_id_results_response::results},
        utility::codec::field{"total", &list_by_workunit_id_results_response::total},
    };
}

/**
 * @brief The members of list_result_versions_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<list_result_versions_request>) {
    return std::tuple{
        utility::codec::field{"key", &list_result_versions_request::key},
        utility::codec::field{"offset", &list_result_versions_request::offset},
        utility::codec::field{"limit", &list_result_versions_request::limit},
        utility::codec::field{"order", &list_result_versions_request::order},
        utility::codec::field{"filter", &list_result_versions_request::filter},
    };
}

/**
 * @brief The members of list_result_versions_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<list_result_versions_response>) {
    return std::tuple{
        utility::codec::field{"result", &list_result_versions_response::result},
        utility::codec::field{"versions", &list_result_versions_response::versions},
        utility::codec::field{"total", &list_result_versions_response::total},
    };
}

/**
 * @brief The members of get_result_version_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_result_version_request>) {
    return std::tuple{
        utility::codec::field{"key", &get_result_version_request::key},
    };
}

/**
 * @brief The members of get_result_version_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_result_version_response>) {
    return std::tuple{
        utility::codec::field{"result", &get_result_version_response::result},
        utility::codec::field{"version", &get_result_version_response::version},
    };
}


}
namespace ores::utility::codec {

template <>
struct ORES_COMPUTE_API_EXPORT wire_traits<ores::compute::messaging::result_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::compute::messaging::result_key& v);
    static ores::compute::messaging::result_key read_json(std::string_view text);
    static std::string write_msgpack(const ores::compute::messaging::result_key& v);
    static ores::compute::messaging::result_key read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_COMPUTE_API_EXPORT wire_traits<ores::compute::messaging::result_write> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::compute::messaging::result_write& v);
    static ores::compute::messaging::result_write read_json(std::string_view text);
    static std::string write_msgpack(const ores::compute::messaging::result_write& v);
    static ores::compute::messaging::result_write read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_COMPUTE_API_EXPORT wire_traits<ores::compute::messaging::result_change> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::compute::messaging::result_change& v);
    static ores::compute::messaging::result_change read_json(std::string_view text);
    static std::string write_msgpack(const ores::compute::messaging::result_change& v);
    static ores::compute::messaging::result_change read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_COMPUTE_API_EXPORT wire_traits<ores::compute::messaging::result_removal> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::compute::messaging::result_removal& v);
    static ores::compute::messaging::result_removal read_json(std::string_view text);
    static std::string write_msgpack(const ores::compute::messaging::result_removal& v);
    static ores::compute::messaging::result_removal read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_COMPUTE_API_EXPORT wire_traits<ores::compute::messaging::result_lookup> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::compute::messaging::result_lookup& v);
    static ores::compute::messaging::result_lookup read_json(std::string_view text);
    static std::string write_msgpack(const ores::compute::messaging::result_lookup& v);
    static ores::compute::messaging::result_lookup read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_COMPUTE_API_EXPORT wire_traits<ores::compute::messaging::results_filter> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::compute::messaging::results_filter& v);
    static ores::compute::messaging::results_filter read_json(std::string_view text);
    static std::string write_msgpack(const ores::compute::messaging::results_filter& v);
    static ores::compute::messaging::results_filter read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_COMPUTE_API_EXPORT wire_traits<ores::compute::messaging::result_event> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::compute::messaging::result_event& v);
    static ores::compute::messaging::result_event read_json(std::string_view text);
    static std::string write_msgpack(const ores::compute::messaging::result_event& v);
    static ores::compute::messaging::result_event read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_COMPUTE_API_EXPORT wire_traits<ores::compute::messaging::result_version_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::compute::messaging::result_version_key& v);
    static ores::compute::messaging::result_version_key read_json(std::string_view text);
    static std::string write_msgpack(const ores::compute::messaging::result_version_key& v);
    static ores::compute::messaging::result_version_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_COMPUTE_API_EXPORT wire_traits<ores::compute::messaging::result_versions_filter> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::compute::messaging::result_versions_filter& v);
    static ores::compute::messaging::result_versions_filter read_json(std::string_view text);
    static std::string write_msgpack(const ores::compute::messaging::result_versions_filter& v);
    static ores::compute::messaging::result_versions_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_COMPUTE_API_EXPORT wire_traits<ores::compute::messaging::list_results_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::compute::messaging::list_results_request& v);
    static ores::compute::messaging::list_results_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::compute::messaging::list_results_request& v);
    static ores::compute::messaging::list_results_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_COMPUTE_API_EXPORT wire_traits<ores::compute::messaging::list_results_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::compute::messaging::list_results_response& v);
    static ores::compute::messaging::list_results_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::compute::messaging::list_results_response& v);
    static ores::compute::messaging::list_results_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_COMPUTE_API_EXPORT wire_traits<ores::compute::messaging::get_result_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::compute::messaging::get_result_request& v);
    static ores::compute::messaging::get_result_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::compute::messaging::get_result_request& v);
    static ores::compute::messaging::get_result_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_COMPUTE_API_EXPORT wire_traits<ores::compute::messaging::get_result_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::compute::messaging::get_result_response& v);
    static ores::compute::messaging::get_result_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::compute::messaging::get_result_response& v);
    static ores::compute::messaging::get_result_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_COMPUTE_API_EXPORT wire_traits<ores::compute::messaging::get_many_results_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::compute::messaging::get_many_results_request& v);
    static ores::compute::messaging::get_many_results_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::compute::messaging::get_many_results_request& v);
    static ores::compute::messaging::get_many_results_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_COMPUTE_API_EXPORT wire_traits<ores::compute::messaging::get_many_results_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::compute::messaging::get_many_results_response& v);
    static ores::compute::messaging::get_many_results_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::compute::messaging::get_many_results_response& v);
    static ores::compute::messaging::get_many_results_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_COMPUTE_API_EXPORT wire_traits<ores::compute::messaging::put_result_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::compute::messaging::put_result_request& v);
    static ores::compute::messaging::put_result_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::compute::messaging::put_result_request& v);
    static ores::compute::messaging::put_result_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_COMPUTE_API_EXPORT wire_traits<ores::compute::messaging::put_result_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::compute::messaging::put_result_response& v);
    static ores::compute::messaging::put_result_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::compute::messaging::put_result_response& v);
    static ores::compute::messaging::put_result_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_COMPUTE_API_EXPORT wire_traits<ores::compute::messaging::put_many_results_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::compute::messaging::put_many_results_request& v);
    static ores::compute::messaging::put_many_results_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::compute::messaging::put_many_results_request& v);
    static ores::compute::messaging::put_many_results_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_COMPUTE_API_EXPORT wire_traits<ores::compute::messaging::put_many_results_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::compute::messaging::put_many_results_response& v);
    static ores::compute::messaging::put_many_results_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::compute::messaging::put_many_results_response& v);
    static ores::compute::messaging::put_many_results_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_COMPUTE_API_EXPORT wire_traits<ores::compute::messaging::delete_result_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::compute::messaging::delete_result_request& v);
    static ores::compute::messaging::delete_result_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::compute::messaging::delete_result_request& v);
    static ores::compute::messaging::delete_result_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_COMPUTE_API_EXPORT wire_traits<ores::compute::messaging::delete_result_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::compute::messaging::delete_result_response& v);
    static ores::compute::messaging::delete_result_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::compute::messaging::delete_result_response& v);
    static ores::compute::messaging::delete_result_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_COMPUTE_API_EXPORT wire_traits<ores::compute::messaging::delete_many_results_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::compute::messaging::delete_many_results_request& v);
    static ores::compute::messaging::delete_many_results_request read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::compute::messaging::delete_many_results_request& v);
    static ores::compute::messaging::delete_many_results_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_COMPUTE_API_EXPORT wire_traits<ores::compute::messaging::delete_many_results_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::compute::messaging::delete_many_results_response& v);
    static ores::compute::messaging::delete_many_results_response read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::compute::messaging::delete_many_results_response& v);
    static ores::compute::messaging::delete_many_results_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_COMPUTE_API_EXPORT
    wire_traits<ores::compute::messaging::list_by_workunit_id_results_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::compute::messaging::list_by_workunit_id_results_request& v);
    static ores::compute::messaging::list_by_workunit_id_results_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::compute::messaging::list_by_workunit_id_results_request& v);
    static ores::compute::messaging::list_by_workunit_id_results_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_COMPUTE_API_EXPORT
    wire_traits<ores::compute::messaging::list_by_workunit_id_results_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::compute::messaging::list_by_workunit_id_results_response& v);
    static ores::compute::messaging::list_by_workunit_id_results_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::compute::messaging::list_by_workunit_id_results_response& v);
    static ores::compute::messaging::list_by_workunit_id_results_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_COMPUTE_API_EXPORT wire_traits<ores::compute::messaging::list_result_versions_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::compute::messaging::list_result_versions_request& v);
    static ores::compute::messaging::list_result_versions_request read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::compute::messaging::list_result_versions_request& v);
    static ores::compute::messaging::list_result_versions_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_COMPUTE_API_EXPORT
    wire_traits<ores::compute::messaging::list_result_versions_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::compute::messaging::list_result_versions_response& v);
    static ores::compute::messaging::list_result_versions_response read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::compute::messaging::list_result_versions_response& v);
    static ores::compute::messaging::list_result_versions_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_COMPUTE_API_EXPORT wire_traits<ores::compute::messaging::get_result_version_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::compute::messaging::get_result_version_request& v);
    static ores::compute::messaging::get_result_version_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::compute::messaging::get_result_version_request& v);
    static ores::compute::messaging::get_result_version_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_COMPUTE_API_EXPORT wire_traits<ores::compute::messaging::get_result_version_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::compute::messaging::get_result_version_response& v);
    static ores::compute::messaging::get_result_version_response read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::compute::messaging::get_result_version_response& v);
    static ores::compute::messaging::get_result_version_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
