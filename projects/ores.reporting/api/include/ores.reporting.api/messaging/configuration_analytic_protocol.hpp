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
#ifndef ORES_REPORTING_API_MESSAGING_CONFIGURATION_ANALYTIC_PROTOCOL_HPP
#define ORES_REPORTING_API_MESSAGING_CONFIGURATION_ANALYTIC_PROTOCOL_HPP

#include "ores.reporting.api/domain/configuration_analytic_codec.hpp"
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

struct configuration_analytic_key {
    boost::uuids::uuid id;

    friend bool operator==(const configuration_analytic_key&,
                           const configuration_analytic_key&) = default;
};

struct configuration_analytic_write {
    boost::uuids::uuid id;
    boost::uuids::uuid configuration_id;
    std::string analytic_type_code;
    int display_order{};
    std::string active;

    friend bool operator==(const configuration_analytic_write&,
                           const configuration_analytic_write&) = default;
};

struct configuration_analytic_change {
    configuration_analytic_write write;
    ores::utility::domain::precondition precondition;

    friend bool operator==(const configuration_analytic_change&,
                           const configuration_analytic_change&) = default;
};

struct configuration_analytic_removal {
    configuration_analytic_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;

    friend bool operator==(const configuration_analytic_removal&,
                           const configuration_analytic_removal&) = default;
};

struct configuration_analytic_lookup {
    configuration_analytic_key key;
    std::optional<ores::reporting::domain::configuration_analytic> configuration_analytic;

    friend bool operator==(const configuration_analytic_lookup&,
                           const configuration_analytic_lookup&) = default;
};

struct configuration_analytics_filter {
    std::optional<boost::uuids::uuid> configuration_id;
    std::optional<std::string> analytic_type_code;
    std::optional<std::vector<boost::uuids::uuid>> id_one_of;
    std::optional<std::vector<boost::uuids::uuid>> configuration_id_one_of;
    std::optional<std::vector<std::string>> analytic_type_code_one_of;

    friend bool operator==(const configuration_analytics_filter&,
                           const configuration_analytics_filter&) = default;
};

struct configuration_analytic_event {
    boost::uuids::uuid event_id;
    configuration_analytic_key key;
    std::string action;
    std::uint32_t version{};
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;

    friend bool operator==(const configuration_analytic_event&,
                           const configuration_analytic_event&) = default;
};

struct configuration_analytic_version_key {
    configuration_analytic_key configuration_analytic;
    std::uint32_t version{};

    friend bool operator==(const configuration_analytic_version_key&,
                           const configuration_analytic_version_key&) = default;
};

struct configuration_analytic_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;

    friend bool operator==(const configuration_analytic_versions_filter&,
                           const configuration_analytic_versions_filter&) = default;
};

struct list_configuration_analytics_request {
    using response_type = struct list_configuration_analytics_response;
    static constexpr std::string_view nats_subject = "reporting.v1.configuration_analytics.list";
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
    std::optional<configuration_analytics_filter> filter;
    std::optional<std::string> as_of;

    friend bool operator==(const list_configuration_analytics_request&,
                           const list_configuration_analytics_request&) = default;
};

struct list_configuration_analytics_response {
    ores::utility::domain::result result;
    std::vector<ores::reporting::domain::configuration_analytic> analytics;
    std::uint64_t total{};

    friend bool operator==(const list_configuration_analytics_response&,
                           const list_configuration_analytics_response&) = default;
};

struct get_configuration_analytic_request {
    using response_type = struct get_configuration_analytic_response;
    static constexpr std::string_view nats_subject = "reporting.v1.configuration_analytics.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    configuration_analytic_key key;

    friend bool operator==(const get_configuration_analytic_request&,
                           const get_configuration_analytic_request&) = default;
};

struct get_configuration_analytic_response {
    ores::utility::domain::result result;
    std::optional<ores::reporting::domain::configuration_analytic> configuration_analytic;

    friend bool operator==(const get_configuration_analytic_response&,
                           const get_configuration_analytic_response&) = default;
};

struct get_many_configuration_analytics_request {
    using response_type = struct get_many_configuration_analytics_response;
    static constexpr std::string_view nats_subject =
        "reporting.v1.configuration_analytics.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<configuration_analytic_key> keys;

    friend bool operator==(const get_many_configuration_analytics_request&,
                           const get_many_configuration_analytics_request&) = default;
};

struct get_many_configuration_analytics_response {
    ores::utility::domain::result result;
    std::vector<configuration_analytic_lookup> entries;

    friend bool operator==(const get_many_configuration_analytics_response&,
                           const get_many_configuration_analytics_response&) = default;
};

struct put_configuration_analytic_request {
    using response_type = struct put_configuration_analytic_response;
    static constexpr std::string_view nats_subject = "reporting.v1.configuration_analytics.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    configuration_analytic_change change;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const put_configuration_analytic_request&,
                           const put_configuration_analytic_request&) = default;
};

struct put_configuration_analytic_response {
    ores::utility::domain::result result;
    std::optional<ores::reporting::domain::configuration_analytic> configuration_analytic;

    friend bool operator==(const put_configuration_analytic_response&,
                           const put_configuration_analytic_response&) = default;
};

struct put_many_configuration_analytics_request {
    using response_type = struct put_many_configuration_analytics_response;
    static constexpr std::string_view nats_subject =
        "reporting.v1.configuration_analytics.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<configuration_analytic_change> changes;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const put_many_configuration_analytics_request&,
                           const put_many_configuration_analytics_request&) = default;
};

struct put_many_configuration_analytics_response {
    ores::utility::domain::result result;
    std::vector<ores::reporting::domain::configuration_analytic> analytics;

    friend bool operator==(const put_many_configuration_analytics_response&,
                           const put_many_configuration_analytics_response&) = default;
};

struct delete_configuration_analytic_request {
    using response_type = struct delete_configuration_analytic_response;
    static constexpr std::string_view nats_subject = "reporting.v1.configuration_analytics.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    configuration_analytic_removal removal;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const delete_configuration_analytic_request&,
                           const delete_configuration_analytic_request&) = default;
};

struct delete_configuration_analytic_response {
    ores::utility::domain::result result;

    friend bool operator==(const delete_configuration_analytic_response&,
                           const delete_configuration_analytic_response&) = default;
};

struct delete_many_configuration_analytics_request {
    using response_type = struct delete_many_configuration_analytics_response;
    static constexpr std::string_view nats_subject =
        "reporting.v1.configuration_analytics.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<configuration_analytic_removal> removals;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const delete_many_configuration_analytics_request&,
                           const delete_many_configuration_analytics_request&) = default;
};

struct delete_many_configuration_analytics_response {
    ores::utility::domain::result result;

    friend bool operator==(const delete_many_configuration_analytics_response&,
                           const delete_many_configuration_analytics_response&) = default;
};

struct list_by_configuration_id_configuration_analytics_request {
    using response_type = struct list_by_configuration_id_configuration_analytics_response;
    static constexpr std::string_view nats_subject =
        "reporting.v1.configuration_analytics.list_by_configuration_id";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    boost::uuids::uuid configuration_id;
    ores::utility::domain::scope scope = ores::utility::domain::scope::direct;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<configuration_analytics_filter> filter;

    friend bool
    operator==(const list_by_configuration_id_configuration_analytics_request&,
               const list_by_configuration_id_configuration_analytics_request&) = default;
};

struct list_by_configuration_id_configuration_analytics_response {
    ores::utility::domain::result result;
    std::vector<ores::reporting::domain::configuration_analytic> analytics;
    std::uint64_t total{};

    friend bool
    operator==(const list_by_configuration_id_configuration_analytics_response&,
               const list_by_configuration_id_configuration_analytics_response&) = default;
};

struct list_by_analytic_type_code_configuration_analytics_request {
    using response_type = struct list_by_analytic_type_code_configuration_analytics_response;
    static constexpr std::string_view nats_subject =
        "reporting.v1.configuration_analytics.list_by_analytic_type_code";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::string analytic_type_code;
    ores::utility::domain::scope scope = ores::utility::domain::scope::direct;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<configuration_analytics_filter> filter;

    friend bool
    operator==(const list_by_analytic_type_code_configuration_analytics_request&,
               const list_by_analytic_type_code_configuration_analytics_request&) = default;
};

struct list_by_analytic_type_code_configuration_analytics_response {
    ores::utility::domain::result result;
    std::vector<ores::reporting::domain::configuration_analytic> analytics;
    std::uint64_t total{};

    friend bool
    operator==(const list_by_analytic_type_code_configuration_analytics_response&,
               const list_by_analytic_type_code_configuration_analytics_response&) = default;
};

struct list_configuration_analytic_versions_request {
    using response_type = struct list_configuration_analytic_versions_response;
    static constexpr std::string_view nats_subject =
        "reporting.v1.configuration_analytics_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    configuration_analytic_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<configuration_analytic_versions_filter> filter;

    friend bool operator==(const list_configuration_analytic_versions_request&,
                           const list_configuration_analytic_versions_request&) = default;
};

struct list_configuration_analytic_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::reporting::domain::configuration_analytic> versions;
    std::uint64_t total{};

    friend bool operator==(const list_configuration_analytic_versions_response&,
                           const list_configuration_analytic_versions_response&) = default;
};

struct get_configuration_analytic_version_request {
    using response_type = struct get_configuration_analytic_version_response;
    static constexpr std::string_view nats_subject =
        "reporting.v1.configuration_analytics_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    configuration_analytic_version_key key;

    friend bool operator==(const get_configuration_analytic_version_request&,
                           const get_configuration_analytic_version_request&) = default;
};

struct get_configuration_analytic_version_response {
    ores::utility::domain::result result;
    std::optional<ores::reporting::domain::configuration_analytic> version;

    friend bool operator==(const get_configuration_analytic_version_response&,
                           const get_configuration_analytic_version_response&) = default;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace configuration_analytic_event_subjects {
inline constexpr std::string_view created = "reporting.v1.events.configuration_analytics.created";
inline constexpr std::string_view updated = "reporting.v1.events.configuration_analytics.updated";
inline constexpr std::string_view deleted = "reporting.v1.events.configuration_analytics.deleted";
}
/**
 * @brief The members of configuration_analytic_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<configuration_analytic_key>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"id", &configuration_analytic_key::id},
    };
}

/**
 * @brief The members of configuration_analytic_write, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<configuration_analytic_write>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"id", &configuration_analytic_write::id},
        utility::codec::field{"configuration_id", &configuration_analytic_write::configuration_id},
        utility::codec::field{"analytic_type_code",
                              &configuration_analytic_write::analytic_type_code},
        utility::codec::field{"display_order", &configuration_analytic_write::display_order},
        utility::codec::field{"active", &configuration_analytic_write::active},
    };
}

/**
 * @brief The members of configuration_analytic_change, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<configuration_analytic_change>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"write", &configuration_analytic_change::write},
        utility::codec::field{"precondition", &configuration_analytic_change::precondition},
    };
}

/**
 * @brief The members of configuration_analytic_removal, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<configuration_analytic_removal>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"key", &configuration_analytic_removal::key},
        utility::codec::field{"precondition", &configuration_analytic_removal::precondition},
    };
}

/**
 * @brief The members of configuration_analytic_lookup, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<configuration_analytic_lookup>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"key", &configuration_analytic_lookup::key},
        utility::codec::field{"configuration_analytic",
                              &configuration_analytic_lookup::configuration_analytic},
    };
}

/**
 * @brief The members of configuration_analytics_filter, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<configuration_analytics_filter>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"configuration_id",
                              &configuration_analytics_filter::configuration_id},
        utility::codec::field{"analytic_type_code",
                              &configuration_analytics_filter::analytic_type_code},
        utility::codec::field{"id_one_of", &configuration_analytics_filter::id_one_of},
        utility::codec::field{"configuration_id_one_of",
                              &configuration_analytics_filter::configuration_id_one_of},
        utility::codec::field{"analytic_type_code_one_of",
                              &configuration_analytics_filter::analytic_type_code_one_of},
    };
}

/**
 * @brief The members of configuration_analytic_event, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<configuration_analytic_event>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"event_id", &configuration_analytic_event::event_id},
        utility::codec::field{"key", &configuration_analytic_event::key},
        utility::codec::field{"action", &configuration_analytic_event::action},
        utility::codec::field{"version", &configuration_analytic_event::version},
        utility::codec::field{"occurred_at", &configuration_analytic_event::occurred_at},
        utility::codec::field{"correlation_id", &configuration_analytic_event::correlation_id},
    };
}

/**
 * @brief The members of configuration_analytic_version_key, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<configuration_analytic_version_key>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"configuration_analytic",
                              &configuration_analytic_version_key::configuration_analytic},
        utility::codec::field{"version", &configuration_analytic_version_key::version},
    };
}

/**
 * @brief The members of configuration_analytic_versions_filter, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<configuration_analytic_versions_filter>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"version", &configuration_analytic_versions_filter::version},
        utility::codec::field{"from_version",
                              &configuration_analytic_versions_filter::from_version},
        utility::codec::field{"to_version", &configuration_analytic_versions_filter::to_version},
    };
}

/**
 * @brief The members of list_configuration_analytics_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<list_configuration_analytics_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"offset", &list_configuration_analytics_request::offset},
        utility::codec::field{"limit", &list_configuration_analytics_request::limit},
        utility::codec::field{"order", &list_configuration_analytics_request::order},
        utility::codec::field{"filter", &list_configuration_analytics_request::filter},
        utility::codec::field{"as_of", &list_configuration_analytics_request::as_of},
    };
}

/**
 * @brief The members of list_configuration_analytics_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<list_configuration_analytics_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &list_configuration_analytics_response::result},
        utility::codec::field{"analytics", &list_configuration_analytics_response::analytics},
        utility::codec::field{"total", &list_configuration_analytics_response::total},
    };
}

/**
 * @brief The members of get_configuration_analytic_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_configuration_analytic_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"key", &get_configuration_analytic_request::key},
    };
}

/**
 * @brief The members of get_configuration_analytic_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_configuration_analytic_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &get_configuration_analytic_response::result},
        utility::codec::field{"configuration_analytic",
                              &get_configuration_analytic_response::configuration_analytic},
    };
}

/**
 * @brief The members of get_many_configuration_analytics_request, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<get_many_configuration_analytics_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"keys", &get_many_configuration_analytics_request::keys},
    };
}

/**
 * @brief The members of get_many_configuration_analytics_response, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<get_many_configuration_analytics_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &get_many_configuration_analytics_response::result},
        utility::codec::field{"entries", &get_many_configuration_analytics_response::entries},
    };
}

/**
 * @brief The members of put_configuration_analytic_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<put_configuration_analytic_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"change", &put_configuration_analytic_request::change},
        utility::codec::field{"intent", &put_configuration_analytic_request::intent},
    };
}

/**
 * @brief The members of put_configuration_analytic_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<put_configuration_analytic_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &put_configuration_analytic_response::result},
        utility::codec::field{"configuration_analytic",
                              &put_configuration_analytic_response::configuration_analytic},
    };
}

/**
 * @brief The members of put_many_configuration_analytics_request, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<put_many_configuration_analytics_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"changes", &put_many_configuration_analytics_request::changes},
        utility::codec::field{"intent", &put_many_configuration_analytics_request::intent},
    };
}

/**
 * @brief The members of put_many_configuration_analytics_response, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<put_many_configuration_analytics_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &put_many_configuration_analytics_response::result},
        utility::codec::field{"analytics", &put_many_configuration_analytics_response::analytics},
    };
}

/**
 * @brief The members of delete_configuration_analytic_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<delete_configuration_analytic_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"removal", &delete_configuration_analytic_request::removal},
        utility::codec::field{"intent", &delete_configuration_analytic_request::intent},
    };
}

/**
 * @brief The members of delete_configuration_analytic_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<delete_configuration_analytic_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &delete_configuration_analytic_response::result},
    };
}

/**
 * @brief The members of delete_many_configuration_analytics_request, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<delete_many_configuration_analytics_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"removals", &delete_many_configuration_analytics_request::removals},
        utility::codec::field{"intent", &delete_many_configuration_analytics_request::intent},
    };
}

/**
 * @brief The members of delete_many_configuration_analytics_response, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<delete_many_configuration_analytics_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &delete_many_configuration_analytics_response::result},
    };
}

/**
 * @brief The members of list_by_configuration_id_configuration_analytics_request, named as the wire
 * names them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<list_by_configuration_id_configuration_analytics_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{
            "configuration_id",
            &list_by_configuration_id_configuration_analytics_request::configuration_id},
        utility::codec::field{"scope",
                              &list_by_configuration_id_configuration_analytics_request::scope},
        utility::codec::field{"offset",
                              &list_by_configuration_id_configuration_analytics_request::offset},
        utility::codec::field{"limit",
                              &list_by_configuration_id_configuration_analytics_request::limit},
        utility::codec::field{"order",
                              &list_by_configuration_id_configuration_analytics_request::order},
        utility::codec::field{"filter",
                              &list_by_configuration_id_configuration_analytics_request::filter},
    };
}

/**
 * @brief The members of list_by_configuration_id_configuration_analytics_response, named as the
 * wire names them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<list_by_configuration_id_configuration_analytics_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result",
                              &list_by_configuration_id_configuration_analytics_response::result},
        utility::codec::field{
            "analytics", &list_by_configuration_id_configuration_analytics_response::analytics},
        utility::codec::field{"total",
                              &list_by_configuration_id_configuration_analytics_response::total},
    };
}

/**
 * @brief The members of list_by_analytic_type_code_configuration_analytics_request, named as the
 * wire names them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<list_by_analytic_type_code_configuration_analytics_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{
            "analytic_type_code",
            &list_by_analytic_type_code_configuration_analytics_request::analytic_type_code},
        utility::codec::field{"scope",
                              &list_by_analytic_type_code_configuration_analytics_request::scope},
        utility::codec::field{"offset",
                              &list_by_analytic_type_code_configuration_analytics_request::offset},
        utility::codec::field{"limit",
                              &list_by_analytic_type_code_configuration_analytics_request::limit},
        utility::codec::field{"order",
                              &list_by_analytic_type_code_configuration_analytics_request::order},
        utility::codec::field{"filter",
                              &list_by_analytic_type_code_configuration_analytics_request::filter},
    };
}

/**
 * @brief The members of list_by_analytic_type_code_configuration_analytics_response, named as the
 * wire names them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<list_by_analytic_type_code_configuration_analytics_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result",
                              &list_by_analytic_type_code_configuration_analytics_response::result},
        utility::codec::field{
            "analytics", &list_by_analytic_type_code_configuration_analytics_response::analytics},
        utility::codec::field{"total",
                              &list_by_analytic_type_code_configuration_analytics_response::total},
    };
}

/**
 * @brief The members of list_configuration_analytic_versions_request, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<list_configuration_analytic_versions_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"key", &list_configuration_analytic_versions_request::key},
        utility::codec::field{"offset", &list_configuration_analytic_versions_request::offset},
        utility::codec::field{"limit", &list_configuration_analytic_versions_request::limit},
        utility::codec::field{"order", &list_configuration_analytic_versions_request::order},
        utility::codec::field{"filter", &list_configuration_analytic_versions_request::filter},
    };
}

/**
 * @brief The members of list_configuration_analytic_versions_response, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<list_configuration_analytic_versions_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &list_configuration_analytic_versions_response::result},
        utility::codec::field{"versions", &list_configuration_analytic_versions_response::versions},
        utility::codec::field{"total", &list_configuration_analytic_versions_response::total},
    };
}

/**
 * @brief The members of get_configuration_analytic_version_request, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<get_configuration_analytic_version_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"key", &get_configuration_analytic_version_request::key},
    };
}

/**
 * @brief The members of get_configuration_analytic_version_response, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<get_configuration_analytic_version_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &get_configuration_analytic_version_response::result},
        utility::codec::field{"version", &get_configuration_analytic_version_response::version},
    };
}


}
namespace ores::utility::codec {

template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::configuration_analytic_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::reporting::messaging::configuration_analytic_key& v);
    static ores::reporting::messaging::configuration_analytic_key read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::configuration_analytic_key& v);
    static ores::reporting::messaging::configuration_analytic_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::configuration_analytic_write> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::configuration_analytic_write& v);
    static ores::reporting::messaging::configuration_analytic_write
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::configuration_analytic_write& v);
    static ores::reporting::messaging::configuration_analytic_write
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::configuration_analytic_change> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::configuration_analytic_change& v);
    static ores::reporting::messaging::configuration_analytic_change
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::configuration_analytic_change& v);
    static ores::reporting::messaging::configuration_analytic_change
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::configuration_analytic_removal> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::configuration_analytic_removal& v);
    static ores::reporting::messaging::configuration_analytic_removal
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::configuration_analytic_removal& v);
    static ores::reporting::messaging::configuration_analytic_removal
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::configuration_analytic_lookup> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::configuration_analytic_lookup& v);
    static ores::reporting::messaging::configuration_analytic_lookup
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::configuration_analytic_lookup& v);
    static ores::reporting::messaging::configuration_analytic_lookup
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::configuration_analytics_filter> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::configuration_analytics_filter& v);
    static ores::reporting::messaging::configuration_analytics_filter
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::configuration_analytics_filter& v);
    static ores::reporting::messaging::configuration_analytics_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::configuration_analytic_event> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::configuration_analytic_event& v);
    static ores::reporting::messaging::configuration_analytic_event
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::configuration_analytic_event& v);
    static ores::reporting::messaging::configuration_analytic_event
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::configuration_analytic_version_key> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::configuration_analytic_version_key& v);
    static ores::reporting::messaging::configuration_analytic_version_key
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::configuration_analytic_version_key& v);
    static ores::reporting::messaging::configuration_analytic_version_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::configuration_analytic_versions_filter> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::configuration_analytic_versions_filter& v);
    static ores::reporting::messaging::configuration_analytic_versions_filter
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::configuration_analytic_versions_filter& v);
    static ores::reporting::messaging::configuration_analytic_versions_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::list_configuration_analytics_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::list_configuration_analytics_request& v);
    static ores::reporting::messaging::list_configuration_analytics_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::list_configuration_analytics_request& v);
    static ores::reporting::messaging::list_configuration_analytics_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::list_configuration_analytics_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::list_configuration_analytics_response& v);
    static ores::reporting::messaging::list_configuration_analytics_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::list_configuration_analytics_response& v);
    static ores::reporting::messaging::list_configuration_analytics_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::get_configuration_analytic_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::get_configuration_analytic_request& v);
    static ores::reporting::messaging::get_configuration_analytic_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::get_configuration_analytic_request& v);
    static ores::reporting::messaging::get_configuration_analytic_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::get_configuration_analytic_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::get_configuration_analytic_response& v);
    static ores::reporting::messaging::get_configuration_analytic_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::get_configuration_analytic_response& v);
    static ores::reporting::messaging::get_configuration_analytic_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::get_many_configuration_analytics_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::get_many_configuration_analytics_request& v);
    static ores::reporting::messaging::get_many_configuration_analytics_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::get_many_configuration_analytics_request& v);
    static ores::reporting::messaging::get_many_configuration_analytics_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::get_many_configuration_analytics_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::get_many_configuration_analytics_response& v);
    static ores::reporting::messaging::get_many_configuration_analytics_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::get_many_configuration_analytics_response& v);
    static ores::reporting::messaging::get_many_configuration_analytics_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::put_configuration_analytic_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::put_configuration_analytic_request& v);
    static ores::reporting::messaging::put_configuration_analytic_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::put_configuration_analytic_request& v);
    static ores::reporting::messaging::put_configuration_analytic_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::put_configuration_analytic_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::put_configuration_analytic_response& v);
    static ores::reporting::messaging::put_configuration_analytic_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::put_configuration_analytic_response& v);
    static ores::reporting::messaging::put_configuration_analytic_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::put_many_configuration_analytics_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::put_many_configuration_analytics_request& v);
    static ores::reporting::messaging::put_many_configuration_analytics_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::put_many_configuration_analytics_request& v);
    static ores::reporting::messaging::put_many_configuration_analytics_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::put_many_configuration_analytics_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::put_many_configuration_analytics_response& v);
    static ores::reporting::messaging::put_many_configuration_analytics_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::put_many_configuration_analytics_response& v);
    static ores::reporting::messaging::put_many_configuration_analytics_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::delete_configuration_analytic_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::delete_configuration_analytic_request& v);
    static ores::reporting::messaging::delete_configuration_analytic_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::delete_configuration_analytic_request& v);
    static ores::reporting::messaging::delete_configuration_analytic_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::delete_configuration_analytic_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::delete_configuration_analytic_response& v);
    static ores::reporting::messaging::delete_configuration_analytic_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::delete_configuration_analytic_response& v);
    static ores::reporting::messaging::delete_configuration_analytic_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::delete_many_configuration_analytics_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::delete_many_configuration_analytics_request& v);
    static ores::reporting::messaging::delete_many_configuration_analytics_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::delete_many_configuration_analytics_request& v);
    static ores::reporting::messaging::delete_many_configuration_analytics_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::delete_many_configuration_analytics_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::delete_many_configuration_analytics_response& v);
    static ores::reporting::messaging::delete_many_configuration_analytics_response
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::reporting::messaging::delete_many_configuration_analytics_response& v);
    static ores::reporting::messaging::delete_many_configuration_analytics_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT wire_traits<
    ores::reporting::messaging::list_by_configuration_id_configuration_analytics_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::reporting::messaging::list_by_configuration_id_configuration_analytics_request&
            v);
    static ores::reporting::messaging::list_by_configuration_id_configuration_analytics_request
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::reporting::messaging::list_by_configuration_id_configuration_analytics_request&
            v);
    static ores::reporting::messaging::list_by_configuration_id_configuration_analytics_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT wire_traits<
    ores::reporting::messaging::list_by_configuration_id_configuration_analytics_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::reporting::messaging::list_by_configuration_id_configuration_analytics_response&
            v);
    static ores::reporting::messaging::list_by_configuration_id_configuration_analytics_response
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::reporting::messaging::list_by_configuration_id_configuration_analytics_response&
            v);
    static ores::reporting::messaging::list_by_configuration_id_configuration_analytics_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT wire_traits<
    ores::reporting::messaging::list_by_analytic_type_code_configuration_analytics_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::
                   list_by_analytic_type_code_configuration_analytics_request& v);
    static ores::reporting::messaging::list_by_analytic_type_code_configuration_analytics_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::
                      list_by_analytic_type_code_configuration_analytics_request& v);
    static ores::reporting::messaging::list_by_analytic_type_code_configuration_analytics_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT wire_traits<
    ores::reporting::messaging::list_by_analytic_type_code_configuration_analytics_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::
                   list_by_analytic_type_code_configuration_analytics_response& v);
    static ores::reporting::messaging::list_by_analytic_type_code_configuration_analytics_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::
                      list_by_analytic_type_code_configuration_analytics_response& v);
    static ores::reporting::messaging::list_by_analytic_type_code_configuration_analytics_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::list_configuration_analytic_versions_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::list_configuration_analytic_versions_request& v);
    static ores::reporting::messaging::list_configuration_analytic_versions_request
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::reporting::messaging::list_configuration_analytic_versions_request& v);
    static ores::reporting::messaging::list_configuration_analytic_versions_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::list_configuration_analytic_versions_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::list_configuration_analytic_versions_response& v);
    static ores::reporting::messaging::list_configuration_analytic_versions_response
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::reporting::messaging::list_configuration_analytic_versions_response& v);
    static ores::reporting::messaging::list_configuration_analytic_versions_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::get_configuration_analytic_version_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::get_configuration_analytic_version_request& v);
    static ores::reporting::messaging::get_configuration_analytic_version_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::get_configuration_analytic_version_request& v);
    static ores::reporting::messaging::get_configuration_analytic_version_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::get_configuration_analytic_version_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::get_configuration_analytic_version_response& v);
    static ores::reporting::messaging::get_configuration_analytic_version_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::get_configuration_analytic_version_response& v);
    static ores::reporting::messaging::get_configuration_analytic_version_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
