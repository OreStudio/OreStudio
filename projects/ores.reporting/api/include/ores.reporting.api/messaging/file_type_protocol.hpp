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
#ifndef ORES_REPORTING_API_MESSAGING_FILE_TYPE_PROTOCOL_HPP
#define ORES_REPORTING_API_MESSAGING_FILE_TYPE_PROTOCOL_HPP

#include "ores.reporting.api/domain/file_type.hpp"
#include "ores.reporting.api/export.hpp"
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

namespace ores::reporting::messaging {

struct file_type_key {
    std::string code;

    friend bool operator==(const file_type_key&, const file_type_key&) = default;
};

struct file_type_write {
    std::string code;
    std::optional<std::string> analytic_type_code;
    std::string ore_parameter;
    std::string direction;
    std::string kind;
    std::string ore_file_code;
    std::optional<std::string> configuration_type_code;
    std::string name_pattern;
    bool is_default;
    std::optional<std::string> produced_by_file_type_code;
    int sample_count;
    int sample_total;
    std::string description;
    int display_order;

    friend bool operator==(const file_type_write&, const file_type_write&) = default;
};

struct file_type_change {
    file_type_write write;
    ores::utility::domain::precondition precondition;

    friend bool operator==(const file_type_change&, const file_type_change&) = default;
};

struct file_type_removal {
    file_type_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;

    friend bool operator==(const file_type_removal&, const file_type_removal&) = default;
};

struct file_type_lookup {
    file_type_key key;
    std::optional<ores::reporting::domain::file_type> file_type;

    friend bool operator==(const file_type_lookup&, const file_type_lookup&) = default;
};

struct file_types_filter {
    std::optional<std::string> ore_file_code;
    std::optional<std::optional<std::string>> analytic_type_code;
    std::optional<std::optional<std::string>> configuration_type_code;
    std::optional<std::optional<std::string>> produced_by_file_type_code;
    std::optional<std::vector<std::string>> code_one_of;
    std::optional<std::vector<std::string>> ore_file_code_one_of;
    std::optional<std::vector<std::string>> analytic_type_code_one_of;
    std::optional<std::vector<std::string>> configuration_type_code_one_of;
    std::optional<std::vector<std::string>> produced_by_file_type_code_one_of;

    friend bool operator==(const file_types_filter&, const file_types_filter&) = default;
};

struct file_type_event {
    boost::uuids::uuid event_id;
    file_type_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;

    friend bool operator==(const file_type_event&, const file_type_event&) = default;
};

struct file_type_version_key {
    file_type_key file_type;
    std::uint32_t version;

    friend bool operator==(const file_type_version_key&, const file_type_version_key&) = default;
};

struct file_type_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;

    friend bool operator==(const file_type_versions_filter&,
                           const file_type_versions_filter&) = default;
};

struct list_file_types_request {
    using response_type = struct list_file_types_response;
    static constexpr std::string_view nats_subject = "reporting.v1.file_types.list";
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
    std::optional<file_types_filter> filter;
    std::optional<std::string> as_of;

    friend bool operator==(const list_file_types_request&,
                           const list_file_types_request&) = default;
};

struct list_file_types_response {
    ores::utility::domain::result result;
    std::vector<ores::reporting::domain::file_type> types;
    std::uint64_t total;

    friend bool operator==(const list_file_types_response&,
                           const list_file_types_response&) = default;
};

struct get_file_type_request {
    using response_type = struct get_file_type_response;
    static constexpr std::string_view nats_subject = "reporting.v1.file_types.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    file_type_key key;

    friend bool operator==(const get_file_type_request&, const get_file_type_request&) = default;
};

struct get_file_type_response {
    ores::utility::domain::result result;
    std::optional<ores::reporting::domain::file_type> file_type;

    friend bool operator==(const get_file_type_response&, const get_file_type_response&) = default;
};

struct get_many_file_types_request {
    using response_type = struct get_many_file_types_response;
    static constexpr std::string_view nats_subject = "reporting.v1.file_types.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<file_type_key> keys;

    friend bool operator==(const get_many_file_types_request&,
                           const get_many_file_types_request&) = default;
};

struct get_many_file_types_response {
    ores::utility::domain::result result;
    std::vector<file_type_lookup> entries;

    friend bool operator==(const get_many_file_types_response&,
                           const get_many_file_types_response&) = default;
};

struct put_file_type_request {
    using response_type = struct put_file_type_response;
    static constexpr std::string_view nats_subject = "reporting.v1.file_types.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    file_type_change change;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const put_file_type_request&, const put_file_type_request&) = default;
};

struct put_file_type_response {
    ores::utility::domain::result result;
    std::optional<ores::reporting::domain::file_type> file_type;

    friend bool operator==(const put_file_type_response&, const put_file_type_response&) = default;
};

struct put_many_file_types_request {
    using response_type = struct put_many_file_types_response;
    static constexpr std::string_view nats_subject = "reporting.v1.file_types.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<file_type_change> changes;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const put_many_file_types_request&,
                           const put_many_file_types_request&) = default;
};

struct put_many_file_types_response {
    ores::utility::domain::result result;
    std::vector<ores::reporting::domain::file_type> types;

    friend bool operator==(const put_many_file_types_response&,
                           const put_many_file_types_response&) = default;
};

struct delete_file_type_request {
    using response_type = struct delete_file_type_response;
    static constexpr std::string_view nats_subject = "reporting.v1.file_types.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    file_type_removal removal;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const delete_file_type_request&,
                           const delete_file_type_request&) = default;
};

struct delete_file_type_response {
    ores::utility::domain::result result;

    friend bool operator==(const delete_file_type_response&,
                           const delete_file_type_response&) = default;
};

struct delete_many_file_types_request {
    using response_type = struct delete_many_file_types_response;
    static constexpr std::string_view nats_subject = "reporting.v1.file_types.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<file_type_removal> removals;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const delete_many_file_types_request&,
                           const delete_many_file_types_request&) = default;
};

struct delete_many_file_types_response {
    ores::utility::domain::result result;

    friend bool operator==(const delete_many_file_types_response&,
                           const delete_many_file_types_response&) = default;
};

struct list_by_ore_file_code_file_types_request {
    using response_type = struct list_by_ore_file_code_file_types_response;
    static constexpr std::string_view nats_subject =
        "reporting.v1.file_types.list_by_ore_file_code";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::string ore_file_code;
    ores::utility::domain::scope scope = ores::utility::domain::scope::direct;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<file_types_filter> filter;
};

struct list_by_ore_file_code_file_types_response {
    ores::utility::domain::result result;
    std::vector<ores::reporting::domain::file_type> types;
    std::uint64_t total;
};

struct list_by_analytic_type_code_file_types_request {
    using response_type = struct list_by_analytic_type_code_file_types_response;
    static constexpr std::string_view nats_subject =
        "reporting.v1.file_types.list_by_analytic_type_code";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::optional<std::string> analytic_type_code;
    ores::utility::domain::scope scope = ores::utility::domain::scope::direct;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<file_types_filter> filter;

    friend bool operator==(const list_by_analytic_type_code_file_types_request&,
                           const list_by_analytic_type_code_file_types_request&) = default;
};

struct list_by_analytic_type_code_file_types_response {
    ores::utility::domain::result result;
    std::vector<ores::reporting::domain::file_type> types;
    std::uint64_t total;

    friend bool operator==(const list_by_analytic_type_code_file_types_response&,
                           const list_by_analytic_type_code_file_types_response&) = default;
};

struct list_by_configuration_type_code_file_types_request {
    using response_type = struct list_by_configuration_type_code_file_types_response;
    static constexpr std::string_view nats_subject =
        "reporting.v1.file_types.list_by_configuration_type_code";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::optional<std::string> configuration_type_code;
    ores::utility::domain::scope scope = ores::utility::domain::scope::direct;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<file_types_filter> filter;

    friend bool operator==(const list_by_configuration_type_code_file_types_request&,
                           const list_by_configuration_type_code_file_types_request&) = default;
};

struct list_by_configuration_type_code_file_types_response {
    ores::utility::domain::result result;
    std::vector<ores::reporting::domain::file_type> types;
    std::uint64_t total;

    friend bool operator==(const list_by_configuration_type_code_file_types_response&,
                           const list_by_configuration_type_code_file_types_response&) = default;
};

struct list_by_produced_by_file_type_code_file_types_request {
    using response_type = struct list_by_produced_by_file_type_code_file_types_response;
    static constexpr std::string_view nats_subject =
        "reporting.v1.file_types.list_by_produced_by_file_type_code";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::optional<std::string> produced_by_file_type_code;
    ores::utility::domain::scope scope = ores::utility::domain::scope::direct;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<file_types_filter> filter;

    friend bool operator==(const list_by_produced_by_file_type_code_file_types_request&,
                           const list_by_produced_by_file_type_code_file_types_request&) = default;
};

struct list_by_produced_by_file_type_code_file_types_response {
    ores::utility::domain::result result;
    std::vector<ores::reporting::domain::file_type> types;
    std::uint64_t total;

    friend bool operator==(const list_by_produced_by_file_type_code_file_types_response&,
                           const list_by_produced_by_file_type_code_file_types_response&) = default;
};

struct list_file_type_versions_request {
    using response_type = struct list_file_type_versions_response;
    static constexpr std::string_view nats_subject = "reporting.v1.file_types_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    file_type_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<file_type_versions_filter> filter;

    friend bool operator==(const list_file_type_versions_request&,
                           const list_file_type_versions_request&) = default;
};

struct list_file_type_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::reporting::domain::file_type> versions;
    std::uint64_t total;

    friend bool operator==(const list_file_type_versions_response&,
                           const list_file_type_versions_response&) = default;
};

struct get_file_type_version_request {
    using response_type = struct get_file_type_version_response;
    static constexpr std::string_view nats_subject = "reporting.v1.file_types_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    file_type_version_key key;

    friend bool operator==(const get_file_type_version_request&,
                           const get_file_type_version_request&) = default;
};

struct get_file_type_version_response {
    ores::utility::domain::result result;
    std::optional<ores::reporting::domain::file_type> version;

    friend bool operator==(const get_file_type_version_response&,
                           const get_file_type_version_response&) = default;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace file_type_event_subjects {
inline constexpr std::string_view created = "reporting.v1.file_types_events.created";
inline constexpr std::string_view updated = "reporting.v1.file_types_events.updated";
inline constexpr std::string_view deleted = "reporting.v1.file_types_events.deleted";
}
/**
 * @brief The members of file_type_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const file_type_key&) {
    return std::tuple{
        utility::codec::field{"code", &file_type_key::code},
    };
}

/**
 * @brief The members of file_type_write, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const file_type_write&) {
    return std::tuple{
        utility::codec::field{"code", &file_type_write::code},
        utility::codec::field{"analytic_type_code", &file_type_write::analytic_type_code},
        utility::codec::field{"ore_parameter", &file_type_write::ore_parameter},
        utility::codec::field{"direction", &file_type_write::direction},
        utility::codec::field{"kind", &file_type_write::kind},
        utility::codec::field{"ore_file_code", &file_type_write::ore_file_code},
        utility::codec::field{"configuration_type_code", &file_type_write::configuration_type_code},
        utility::codec::field{"name_pattern", &file_type_write::name_pattern},
        utility::codec::field{"is_default", &file_type_write::is_default},
        utility::codec::field{"produced_by_file_type_code",
                              &file_type_write::produced_by_file_type_code},
        utility::codec::field{"sample_count", &file_type_write::sample_count},
        utility::codec::field{"sample_total", &file_type_write::sample_total},
        utility::codec::field{"description", &file_type_write::description},
        utility::codec::field{"display_order", &file_type_write::display_order},
    };
}

/**
 * @brief The members of file_type_change, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const file_type_change&) {
    return std::tuple{
        utility::codec::field{"write", &file_type_change::write},
        utility::codec::field{"precondition", &file_type_change::precondition},
    };
}

/**
 * @brief The members of file_type_removal, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const file_type_removal&) {
    return std::tuple{
        utility::codec::field{"key", &file_type_removal::key},
        utility::codec::field{"precondition", &file_type_removal::precondition},
    };
}

/**
 * @brief The members of file_type_lookup, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const file_type_lookup&) {
    return std::tuple{
        utility::codec::field{"key", &file_type_lookup::key},
        utility::codec::field{"file_type", &file_type_lookup::file_type},
    };
}

/**
 * @brief The members of file_types_filter, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const file_types_filter&) {
    return std::tuple{
        utility::codec::field{"ore_file_code", &file_types_filter::ore_file_code},
        utility::codec::field{"analytic_type_code", &file_types_filter::analytic_type_code},
        utility::codec::field{"configuration_type_code",
                              &file_types_filter::configuration_type_code},
        utility::codec::field{"produced_by_file_type_code",
                              &file_types_filter::produced_by_file_type_code},
        utility::codec::field{"code_one_of", &file_types_filter::code_one_of},
        utility::codec::field{"ore_file_code_one_of", &file_types_filter::ore_file_code_one_of},
        utility::codec::field{"analytic_type_code_one_of",
                              &file_types_filter::analytic_type_code_one_of},
        utility::codec::field{"configuration_type_code_one_of",
                              &file_types_filter::configuration_type_code_one_of},
        utility::codec::field{"produced_by_file_type_code_one_of",
                              &file_types_filter::produced_by_file_type_code_one_of},
    };
}

/**
 * @brief The members of file_type_event, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const file_type_event&) {
    return std::tuple{
        utility::codec::field{"event_id", &file_type_event::event_id},
        utility::codec::field{"key", &file_type_event::key},
        utility::codec::field{"action", &file_type_event::action},
        utility::codec::field{"version", &file_type_event::version},
        utility::codec::field{"occurred_at", &file_type_event::occurred_at},
        utility::codec::field{"correlation_id", &file_type_event::correlation_id},
    };
}

/**
 * @brief The members of file_type_version_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const file_type_version_key&) {
    return std::tuple{
        utility::codec::field{"file_type", &file_type_version_key::file_type},
        utility::codec::field{"version", &file_type_version_key::version},
    };
}

/**
 * @brief The members of file_type_versions_filter, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const file_type_versions_filter&) {
    return std::tuple{
        utility::codec::field{"version", &file_type_versions_filter::version},
        utility::codec::field{"from_version", &file_type_versions_filter::from_version},
        utility::codec::field{"to_version", &file_type_versions_filter::to_version},
    };
}

/**
 * @brief The members of list_file_types_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_file_types_request&) {
    return std::tuple{
        utility::codec::field{"offset", &list_file_types_request::offset},
        utility::codec::field{"limit", &list_file_types_request::limit},
        utility::codec::field{"order", &list_file_types_request::order},
        utility::codec::field{"filter", &list_file_types_request::filter},
        utility::codec::field{"as_of", &list_file_types_request::as_of},
    };
}

/**
 * @brief The members of list_file_types_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_file_types_response&) {
    return std::tuple{
        utility::codec::field{"result", &list_file_types_response::result},
        utility::codec::field{"types", &list_file_types_response::types},
        utility::codec::field{"total", &list_file_types_response::total},
    };
}

/**
 * @brief The members of get_file_type_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_file_type_request&) {
    return std::tuple{
        utility::codec::field{"key", &get_file_type_request::key},
    };
}

/**
 * @brief The members of get_file_type_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_file_type_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_file_type_response::result},
        utility::codec::field{"file_type", &get_file_type_response::file_type},
    };
}

/**
 * @brief The members of get_many_file_types_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_many_file_types_request&) {
    return std::tuple{
        utility::codec::field{"keys", &get_many_file_types_request::keys},
    };
}

/**
 * @brief The members of get_many_file_types_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_many_file_types_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_many_file_types_response::result},
        utility::codec::field{"entries", &get_many_file_types_response::entries},
    };
}

/**
 * @brief The members of put_file_type_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_file_type_request&) {
    return std::tuple{
        utility::codec::field{"change", &put_file_type_request::change},
        utility::codec::field{"intent", &put_file_type_request::intent},
    };
}

/**
 * @brief The members of put_file_type_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_file_type_response&) {
    return std::tuple{
        utility::codec::field{"result", &put_file_type_response::result},
        utility::codec::field{"file_type", &put_file_type_response::file_type},
    };
}

/**
 * @brief The members of put_many_file_types_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_many_file_types_request&) {
    return std::tuple{
        utility::codec::field{"changes", &put_many_file_types_request::changes},
        utility::codec::field{"intent", &put_many_file_types_request::intent},
    };
}

/**
 * @brief The members of put_many_file_types_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_many_file_types_response&) {
    return std::tuple{
        utility::codec::field{"result", &put_many_file_types_response::result},
        utility::codec::field{"types", &put_many_file_types_response::types},
    };
}

/**
 * @brief The members of delete_file_type_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_file_type_request&) {
    return std::tuple{
        utility::codec::field{"removal", &delete_file_type_request::removal},
        utility::codec::field{"intent", &delete_file_type_request::intent},
    };
}

/**
 * @brief The members of delete_file_type_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_file_type_response&) {
    return std::tuple{
        utility::codec::field{"result", &delete_file_type_response::result},
    };
}

/**
 * @brief The members of delete_many_file_types_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_many_file_types_request&) {
    return std::tuple{
        utility::codec::field{"removals", &delete_many_file_types_request::removals},
        utility::codec::field{"intent", &delete_many_file_types_request::intent},
    };
}

/**
 * @brief The members of delete_many_file_types_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_many_file_types_response&) {
    return std::tuple{
        utility::codec::field{"result", &delete_many_file_types_response::result},
    };
}

/**
 * @brief The members of list_by_ore_file_code_file_types_request, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_by_ore_file_code_file_types_request&) {
    return std::tuple{
        utility::codec::field{"ore_file_code",
                              &list_by_ore_file_code_file_types_request::ore_file_code},
        utility::codec::field{"scope", &list_by_ore_file_code_file_types_request::scope},
        utility::codec::field{"offset", &list_by_ore_file_code_file_types_request::offset},
        utility::codec::field{"limit", &list_by_ore_file_code_file_types_request::limit},
        utility::codec::field{"order", &list_by_ore_file_code_file_types_request::order},
        utility::codec::field{"filter", &list_by_ore_file_code_file_types_request::filter},
    };
}

/**
 * @brief The members of list_by_ore_file_code_file_types_response, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_by_ore_file_code_file_types_response&) {
    return std::tuple{
        utility::codec::field{"result", &list_by_ore_file_code_file_types_response::result},
        utility::codec::field{"types", &list_by_ore_file_code_file_types_response::types},
        utility::codec::field{"total", &list_by_ore_file_code_file_types_response::total},
    };
}

/**
 * @brief The members of list_by_analytic_type_code_file_types_request, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_by_analytic_type_code_file_types_request&) {
    return std::tuple{
        utility::codec::field{"analytic_type_code",
                              &list_by_analytic_type_code_file_types_request::analytic_type_code},
        utility::codec::field{"scope", &list_by_analytic_type_code_file_types_request::scope},
        utility::codec::field{"offset", &list_by_analytic_type_code_file_types_request::offset},
        utility::codec::field{"limit", &list_by_analytic_type_code_file_types_request::limit},
        utility::codec::field{"order", &list_by_analytic_type_code_file_types_request::order},
        utility::codec::field{"filter", &list_by_analytic_type_code_file_types_request::filter},
    };
}

/**
 * @brief The members of list_by_analytic_type_code_file_types_response, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_by_analytic_type_code_file_types_response&) {
    return std::tuple{
        utility::codec::field{"result", &list_by_analytic_type_code_file_types_response::result},
        utility::codec::field{"types", &list_by_analytic_type_code_file_types_response::types},
        utility::codec::field{"total", &list_by_analytic_type_code_file_types_response::total},
    };
}

/**
 * @brief The members of list_by_configuration_type_code_file_types_request, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(const list_by_configuration_type_code_file_types_request&) {
    return std::tuple{
        utility::codec::field{
            "configuration_type_code",
            &list_by_configuration_type_code_file_types_request::configuration_type_code},
        utility::codec::field{"scope", &list_by_configuration_type_code_file_types_request::scope},
        utility::codec::field{"offset",
                              &list_by_configuration_type_code_file_types_request::offset},
        utility::codec::field{"limit", &list_by_configuration_type_code_file_types_request::limit},
        utility::codec::field{"order", &list_by_configuration_type_code_file_types_request::order},
        utility::codec::field{"filter",
                              &list_by_configuration_type_code_file_types_request::filter},
    };
}

/**
 * @brief The members of list_by_configuration_type_code_file_types_response, named as the wire
 * names them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(const list_by_configuration_type_code_file_types_response&) {
    return std::tuple{
        utility::codec::field{"result",
                              &list_by_configuration_type_code_file_types_response::result},
        utility::codec::field{"types", &list_by_configuration_type_code_file_types_response::types},
        utility::codec::field{"total", &list_by_configuration_type_code_file_types_response::total},
    };
}

/**
 * @brief The members of list_by_produced_by_file_type_code_file_types_request, named as the wire
 * names them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(const list_by_produced_by_file_type_code_file_types_request&) {
    return std::tuple{
        utility::codec::field{
            "produced_by_file_type_code",
            &list_by_produced_by_file_type_code_file_types_request::produced_by_file_type_code},
        utility::codec::field{"scope",
                              &list_by_produced_by_file_type_code_file_types_request::scope},
        utility::codec::field{"offset",
                              &list_by_produced_by_file_type_code_file_types_request::offset},
        utility::codec::field{"limit",
                              &list_by_produced_by_file_type_code_file_types_request::limit},
        utility::codec::field{"order",
                              &list_by_produced_by_file_type_code_file_types_request::order},
        utility::codec::field{"filter",
                              &list_by_produced_by_file_type_code_file_types_request::filter},
    };
}

/**
 * @brief The members of list_by_produced_by_file_type_code_file_types_response, named as the wire
 * names them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(const list_by_produced_by_file_type_code_file_types_response&) {
    return std::tuple{
        utility::codec::field{"result",
                              &list_by_produced_by_file_type_code_file_types_response::result},
        utility::codec::field{"types",
                              &list_by_produced_by_file_type_code_file_types_response::types},
        utility::codec::field{"total",
                              &list_by_produced_by_file_type_code_file_types_response::total},
    };
}

/**
 * @brief The members of list_file_type_versions_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_file_type_versions_request&) {
    return std::tuple{
        utility::codec::field{"key", &list_file_type_versions_request::key},
        utility::codec::field{"offset", &list_file_type_versions_request::offset},
        utility::codec::field{"limit", &list_file_type_versions_request::limit},
        utility::codec::field{"order", &list_file_type_versions_request::order},
        utility::codec::field{"filter", &list_file_type_versions_request::filter},
    };
}

/**
 * @brief The members of list_file_type_versions_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_file_type_versions_response&) {
    return std::tuple{
        utility::codec::field{"result", &list_file_type_versions_response::result},
        utility::codec::field{"versions", &list_file_type_versions_response::versions},
        utility::codec::field{"total", &list_file_type_versions_response::total},
    };
}

/**
 * @brief The members of get_file_type_version_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_file_type_version_request&) {
    return std::tuple{
        utility::codec::field{"key", &get_file_type_version_request::key},
    };
}

/**
 * @brief The members of get_file_type_version_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_file_type_version_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_file_type_version_response::result},
        utility::codec::field{"version", &get_file_type_version_response::version},
    };
}


}
namespace ores::utility::codec {

template <>
struct ORES_REPORTING_API_EXPORT wire_traits<ores::reporting::messaging::file_type_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::reporting::messaging::file_type_key& v);
    static ores::reporting::messaging::file_type_key read_json(std::string_view text);
    static std::string write_msgpack(const ores::reporting::messaging::file_type_key& v);
    static ores::reporting::messaging::file_type_key read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT wire_traits<ores::reporting::messaging::file_type_write> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::reporting::messaging::file_type_write& v);
    static ores::reporting::messaging::file_type_write read_json(std::string_view text);
    static std::string write_msgpack(const ores::reporting::messaging::file_type_write& v);
    static ores::reporting::messaging::file_type_write
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT wire_traits<ores::reporting::messaging::file_type_change> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::reporting::messaging::file_type_change& v);
    static ores::reporting::messaging::file_type_change read_json(std::string_view text);
    static std::string write_msgpack(const ores::reporting::messaging::file_type_change& v);
    static ores::reporting::messaging::file_type_change
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT wire_traits<ores::reporting::messaging::file_type_removal> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::reporting::messaging::file_type_removal& v);
    static ores::reporting::messaging::file_type_removal read_json(std::string_view text);
    static std::string write_msgpack(const ores::reporting::messaging::file_type_removal& v);
    static ores::reporting::messaging::file_type_removal
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT wire_traits<ores::reporting::messaging::file_type_lookup> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::reporting::messaging::file_type_lookup& v);
    static ores::reporting::messaging::file_type_lookup read_json(std::string_view text);
    static std::string write_msgpack(const ores::reporting::messaging::file_type_lookup& v);
    static ores::reporting::messaging::file_type_lookup
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT wire_traits<ores::reporting::messaging::file_types_filter> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::reporting::messaging::file_types_filter& v);
    static ores::reporting::messaging::file_types_filter read_json(std::string_view text);
    static std::string write_msgpack(const ores::reporting::messaging::file_types_filter& v);
    static ores::reporting::messaging::file_types_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT wire_traits<ores::reporting::messaging::file_type_event> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::reporting::messaging::file_type_event& v);
    static ores::reporting::messaging::file_type_event read_json(std::string_view text);
    static std::string write_msgpack(const ores::reporting::messaging::file_type_event& v);
    static ores::reporting::messaging::file_type_event
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT wire_traits<ores::reporting::messaging::file_type_version_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::reporting::messaging::file_type_version_key& v);
    static ores::reporting::messaging::file_type_version_key read_json(std::string_view text);
    static std::string write_msgpack(const ores::reporting::messaging::file_type_version_key& v);
    static ores::reporting::messaging::file_type_version_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::file_type_versions_filter> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::reporting::messaging::file_type_versions_filter& v);
    static ores::reporting::messaging::file_type_versions_filter read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::file_type_versions_filter& v);
    static ores::reporting::messaging::file_type_versions_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT wire_traits<ores::reporting::messaging::list_file_types_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::reporting::messaging::list_file_types_request& v);
    static ores::reporting::messaging::list_file_types_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::reporting::messaging::list_file_types_request& v);
    static ores::reporting::messaging::list_file_types_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT wire_traits<ores::reporting::messaging::list_file_types_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::reporting::messaging::list_file_types_response& v);
    static ores::reporting::messaging::list_file_types_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::reporting::messaging::list_file_types_response& v);
    static ores::reporting::messaging::list_file_types_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT wire_traits<ores::reporting::messaging::get_file_type_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::reporting::messaging::get_file_type_request& v);
    static ores::reporting::messaging::get_file_type_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::reporting::messaging::get_file_type_request& v);
    static ores::reporting::messaging::get_file_type_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT wire_traits<ores::reporting::messaging::get_file_type_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::reporting::messaging::get_file_type_response& v);
    static ores::reporting::messaging::get_file_type_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::reporting::messaging::get_file_type_response& v);
    static ores::reporting::messaging::get_file_type_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::get_many_file_types_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::reporting::messaging::get_many_file_types_request& v);
    static ores::reporting::messaging::get_many_file_types_request read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::get_many_file_types_request& v);
    static ores::reporting::messaging::get_many_file_types_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::get_many_file_types_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::get_many_file_types_response& v);
    static ores::reporting::messaging::get_many_file_types_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::get_many_file_types_response& v);
    static ores::reporting::messaging::get_many_file_types_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT wire_traits<ores::reporting::messaging::put_file_type_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::reporting::messaging::put_file_type_request& v);
    static ores::reporting::messaging::put_file_type_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::reporting::messaging::put_file_type_request& v);
    static ores::reporting::messaging::put_file_type_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT wire_traits<ores::reporting::messaging::put_file_type_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::reporting::messaging::put_file_type_response& v);
    static ores::reporting::messaging::put_file_type_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::reporting::messaging::put_file_type_response& v);
    static ores::reporting::messaging::put_file_type_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::put_many_file_types_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::reporting::messaging::put_many_file_types_request& v);
    static ores::reporting::messaging::put_many_file_types_request read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::put_many_file_types_request& v);
    static ores::reporting::messaging::put_many_file_types_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::put_many_file_types_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::put_many_file_types_response& v);
    static ores::reporting::messaging::put_many_file_types_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::put_many_file_types_response& v);
    static ores::reporting::messaging::put_many_file_types_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT wire_traits<ores::reporting::messaging::delete_file_type_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::reporting::messaging::delete_file_type_request& v);
    static ores::reporting::messaging::delete_file_type_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::reporting::messaging::delete_file_type_request& v);
    static ores::reporting::messaging::delete_file_type_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::delete_file_type_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::reporting::messaging::delete_file_type_response& v);
    static ores::reporting::messaging::delete_file_type_response read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::delete_file_type_response& v);
    static ores::reporting::messaging::delete_file_type_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::delete_many_file_types_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::delete_many_file_types_request& v);
    static ores::reporting::messaging::delete_many_file_types_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::delete_many_file_types_request& v);
    static ores::reporting::messaging::delete_many_file_types_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::delete_many_file_types_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::delete_many_file_types_response& v);
    static ores::reporting::messaging::delete_many_file_types_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::delete_many_file_types_response& v);
    static ores::reporting::messaging::delete_many_file_types_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::list_by_ore_file_code_file_types_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::list_by_ore_file_code_file_types_request& v);
    static ores::reporting::messaging::list_by_ore_file_code_file_types_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::list_by_ore_file_code_file_types_request& v);
    static ores::reporting::messaging::list_by_ore_file_code_file_types_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::list_by_ore_file_code_file_types_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::list_by_ore_file_code_file_types_response& v);
    static ores::reporting::messaging::list_by_ore_file_code_file_types_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::list_by_ore_file_code_file_types_response& v);
    static ores::reporting::messaging::list_by_ore_file_code_file_types_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::list_by_analytic_type_code_file_types_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::list_by_analytic_type_code_file_types_request& v);
    static ores::reporting::messaging::list_by_analytic_type_code_file_types_request
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::reporting::messaging::list_by_analytic_type_code_file_types_request& v);
    static ores::reporting::messaging::list_by_analytic_type_code_file_types_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::list_by_analytic_type_code_file_types_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::list_by_analytic_type_code_file_types_response& v);
    static ores::reporting::messaging::list_by_analytic_type_code_file_types_response
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::reporting::messaging::list_by_analytic_type_code_file_types_response& v);
    static ores::reporting::messaging::list_by_analytic_type_code_file_types_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::list_by_configuration_type_code_file_types_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::reporting::messaging::list_by_configuration_type_code_file_types_request& v);
    static ores::reporting::messaging::list_by_configuration_type_code_file_types_request
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::reporting::messaging::list_by_configuration_type_code_file_types_request& v);
    static ores::reporting::messaging::list_by_configuration_type_code_file_types_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::list_by_configuration_type_code_file_types_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::reporting::messaging::list_by_configuration_type_code_file_types_response& v);
    static ores::reporting::messaging::list_by_configuration_type_code_file_types_response
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::reporting::messaging::list_by_configuration_type_code_file_types_response& v);
    static ores::reporting::messaging::list_by_configuration_type_code_file_types_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::list_by_produced_by_file_type_code_file_types_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::reporting::messaging::list_by_produced_by_file_type_code_file_types_request& v);
    static ores::reporting::messaging::list_by_produced_by_file_type_code_file_types_request
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::reporting::messaging::list_by_produced_by_file_type_code_file_types_request& v);
    static ores::reporting::messaging::list_by_produced_by_file_type_code_file_types_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT wire_traits<
    ores::reporting::messaging::list_by_produced_by_file_type_code_file_types_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::reporting::messaging::list_by_produced_by_file_type_code_file_types_response&
            v);
    static ores::reporting::messaging::list_by_produced_by_file_type_code_file_types_response
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::reporting::messaging::list_by_produced_by_file_type_code_file_types_response&
            v);
    static ores::reporting::messaging::list_by_produced_by_file_type_code_file_types_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::list_file_type_versions_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::list_file_type_versions_request& v);
    static ores::reporting::messaging::list_file_type_versions_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::list_file_type_versions_request& v);
    static ores::reporting::messaging::list_file_type_versions_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::list_file_type_versions_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::list_file_type_versions_response& v);
    static ores::reporting::messaging::list_file_type_versions_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::list_file_type_versions_response& v);
    static ores::reporting::messaging::list_file_type_versions_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::get_file_type_version_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::get_file_type_version_request& v);
    static ores::reporting::messaging::get_file_type_version_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::get_file_type_version_request& v);
    static ores::reporting::messaging::get_file_type_version_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::get_file_type_version_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::get_file_type_version_response& v);
    static ores::reporting::messaging::get_file_type_version_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::get_file_type_version_response& v);
    static ores::reporting::messaging::get_file_type_version_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
