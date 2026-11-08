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
#ifndef ORES_REPORTING_API_MESSAGING_ORE_FILE_PROTOCOL_HPP
#define ORES_REPORTING_API_MESSAGING_ORE_FILE_PROTOCOL_HPP

#include "ores.reporting.api/domain/ore_file_codec.hpp"
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

struct ore_file_key {
    std::string code;

    friend bool operator==(const ore_file_key&, const ore_file_key&) = default;
};

struct ore_file_write {
    std::string code;
    std::string name;
    std::string category_code;
    std::optional<std::string> configuration_type_code;
    std::optional<std::string> guide_section;
    std::string origin;
    std::string format;
    std::optional<std::string> schema_ore_file_code;
    std::string typical_filename;
    std::string filename_regex;
    std::string description;
    int display_order{};

    friend bool operator==(const ore_file_write&, const ore_file_write&) = default;
};

struct ore_file_change {
    ore_file_write write;
    ores::utility::domain::precondition precondition;

    friend bool operator==(const ore_file_change&, const ore_file_change&) = default;
};

struct ore_file_removal {
    ore_file_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;

    friend bool operator==(const ore_file_removal&, const ore_file_removal&) = default;
};

struct ore_file_lookup {
    ore_file_key key;
    std::optional<ores::reporting::domain::ore_file> ore_file;

    friend bool operator==(const ore_file_lookup&, const ore_file_lookup&) = default;
};

struct ore_files_filter {
    std::optional<std::string> category_code;
    std::optional<std::optional<std::string>> configuration_type_code;
    std::optional<std::optional<std::string>> schema_ore_file_code;
    std::optional<std::vector<std::string>> code_one_of;
    std::optional<std::vector<std::string>> category_code_one_of;
    std::optional<std::vector<std::string>> configuration_type_code_one_of;
    std::optional<std::vector<std::string>> schema_ore_file_code_one_of;

    friend bool operator==(const ore_files_filter&, const ore_files_filter&) = default;
};

struct ore_file_event {
    boost::uuids::uuid event_id;
    ore_file_key key;
    std::string action;
    std::uint32_t version{};
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;

    friend bool operator==(const ore_file_event&, const ore_file_event&) = default;
};

struct ore_files_version_key {
    ore_file_key ore_file;
    std::uint32_t version{};

    friend bool operator==(const ore_files_version_key&, const ore_files_version_key&) = default;
};

struct ore_files_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;

    friend bool operator==(const ore_files_versions_filter&,
                           const ore_files_versions_filter&) = default;
};

struct list_ore_files_request {
    using response_type = struct list_ore_files_response;
    static constexpr std::string_view nats_subject = "reporting.v1.ore_files.list";
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
    std::optional<ore_files_filter> filter;
    std::optional<std::string> as_of;

    friend bool operator==(const list_ore_files_request&, const list_ore_files_request&) = default;
};

struct list_ore_files_response {
    ores::utility::domain::result result;
    std::vector<ores::reporting::domain::ore_file> files;
    std::uint64_t total{};

    friend bool operator==(const list_ore_files_response&,
                           const list_ore_files_response&) = default;
};

struct get_ore_file_request {
    using response_type = struct get_ore_file_response;
    static constexpr std::string_view nats_subject = "reporting.v1.ore_files.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    ore_file_key key;

    friend bool operator==(const get_ore_file_request&, const get_ore_file_request&) = default;
};

struct get_ore_file_response {
    ores::utility::domain::result result;
    std::optional<ores::reporting::domain::ore_file> ore_file;

    friend bool operator==(const get_ore_file_response&, const get_ore_file_response&) = default;
};

struct get_many_ore_files_request {
    using response_type = struct get_many_ore_files_response;
    static constexpr std::string_view nats_subject = "reporting.v1.ore_files.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<ore_file_key> keys;

    friend bool operator==(const get_many_ore_files_request&,
                           const get_many_ore_files_request&) = default;
};

struct get_many_ore_files_response {
    ores::utility::domain::result result;
    std::vector<ore_file_lookup> entries;

    friend bool operator==(const get_many_ore_files_response&,
                           const get_many_ore_files_response&) = default;
};

struct put_ore_file_request {
    using response_type = struct put_ore_file_response;
    static constexpr std::string_view nats_subject = "reporting.v1.ore_files.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    ore_file_change change;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const put_ore_file_request&, const put_ore_file_request&) = default;
};

struct put_ore_file_response {
    ores::utility::domain::result result;
    std::optional<ores::reporting::domain::ore_file> ore_file;

    friend bool operator==(const put_ore_file_response&, const put_ore_file_response&) = default;
};

struct put_many_ore_files_request {
    using response_type = struct put_many_ore_files_response;
    static constexpr std::string_view nats_subject = "reporting.v1.ore_files.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<ore_file_change> changes;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const put_many_ore_files_request&,
                           const put_many_ore_files_request&) = default;
};

struct put_many_ore_files_response {
    ores::utility::domain::result result;
    std::vector<ores::reporting::domain::ore_file> files;

    friend bool operator==(const put_many_ore_files_response&,
                           const put_many_ore_files_response&) = default;
};

struct delete_ore_file_request {
    using response_type = struct delete_ore_file_response;
    static constexpr std::string_view nats_subject = "reporting.v1.ore_files.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    ore_file_removal removal;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const delete_ore_file_request&,
                           const delete_ore_file_request&) = default;
};

struct delete_ore_file_response {
    ores::utility::domain::result result;

    friend bool operator==(const delete_ore_file_response&,
                           const delete_ore_file_response&) = default;
};

struct delete_many_ore_files_request {
    using response_type = struct delete_many_ore_files_response;
    static constexpr std::string_view nats_subject = "reporting.v1.ore_files.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<ore_file_removal> removals;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const delete_many_ore_files_request&,
                           const delete_many_ore_files_request&) = default;
};

struct delete_many_ore_files_response {
    ores::utility::domain::result result;

    friend bool operator==(const delete_many_ore_files_response&,
                           const delete_many_ore_files_response&) = default;
};

struct list_by_category_code_ore_files_request {
    using response_type = struct list_by_category_code_ore_files_response;
    static constexpr std::string_view nats_subject = "reporting.v1.ore_files.list_by_category_code";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::string category_code;
    ores::utility::domain::scope scope = ores::utility::domain::scope::direct;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<ore_files_filter> filter;

    friend bool operator==(const list_by_category_code_ore_files_request&,
                           const list_by_category_code_ore_files_request&) = default;
};

struct list_by_category_code_ore_files_response {
    ores::utility::domain::result result;
    std::vector<ores::reporting::domain::ore_file> files;
    std::uint64_t total{};

    friend bool operator==(const list_by_category_code_ore_files_response&,
                           const list_by_category_code_ore_files_response&) = default;
};

struct list_by_configuration_type_code_ore_files_request {
    using response_type = struct list_by_configuration_type_code_ore_files_response;
    static constexpr std::string_view nats_subject =
        "reporting.v1.ore_files.list_by_configuration_type_code";
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
    std::optional<ore_files_filter> filter;

    friend bool operator==(const list_by_configuration_type_code_ore_files_request&,
                           const list_by_configuration_type_code_ore_files_request&) = default;
};

struct list_by_configuration_type_code_ore_files_response {
    ores::utility::domain::result result;
    std::vector<ores::reporting::domain::ore_file> files;
    std::uint64_t total{};

    friend bool operator==(const list_by_configuration_type_code_ore_files_response&,
                           const list_by_configuration_type_code_ore_files_response&) = default;
};

struct list_by_schema_ore_file_code_ore_files_request {
    using response_type = struct list_by_schema_ore_file_code_ore_files_response;
    static constexpr std::string_view nats_subject =
        "reporting.v1.ore_files.list_by_schema_ore_file_code";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::optional<std::string> schema_ore_file_code;
    ores::utility::domain::scope scope = ores::utility::domain::scope::direct;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<ore_files_filter> filter;

    friend bool operator==(const list_by_schema_ore_file_code_ore_files_request&,
                           const list_by_schema_ore_file_code_ore_files_request&) = default;
};

struct list_by_schema_ore_file_code_ore_files_response {
    ores::utility::domain::result result;
    std::vector<ores::reporting::domain::ore_file> files;
    std::uint64_t total{};

    friend bool operator==(const list_by_schema_ore_file_code_ore_files_response&,
                           const list_by_schema_ore_file_code_ore_files_response&) = default;
};

struct list_ore_files_versions_request {
    using response_type = struct list_ore_files_versions_response;
    static constexpr std::string_view nats_subject = "reporting.v1.ore_files_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    ore_file_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<ore_files_versions_filter> filter;

    friend bool operator==(const list_ore_files_versions_request&,
                           const list_ore_files_versions_request&) = default;
};

struct list_ore_files_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::reporting::domain::ore_file> versions;
    std::uint64_t total{};

    friend bool operator==(const list_ore_files_versions_response&,
                           const list_ore_files_versions_response&) = default;
};

struct get_ore_files_version_request {
    using response_type = struct get_ore_files_version_response;
    static constexpr std::string_view nats_subject = "reporting.v1.ore_files_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    ore_files_version_key key;

    friend bool operator==(const get_ore_files_version_request&,
                           const get_ore_files_version_request&) = default;
};

struct get_ore_files_version_response {
    ores::utility::domain::result result;
    std::optional<ores::reporting::domain::ore_file> version;

    friend bool operator==(const get_ore_files_version_response&,
                           const get_ore_files_version_response&) = default;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace ore_file_event_subjects {
inline constexpr std::string_view created = "reporting.v1.events.ore_files.created";
inline constexpr std::string_view updated = "reporting.v1.events.ore_files.updated";
inline constexpr std::string_view deleted = "reporting.v1.events.ore_files.deleted";
}
/**
 * @brief The members of ore_file_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<ore_file_key>) {
    return std::tuple{
        utility::codec::field{"code", &ore_file_key::code},
    };
}

/**
 * @brief The members of ore_file_write, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<ore_file_write>) {
    return std::tuple{
        utility::codec::field{"code", &ore_file_write::code},
        utility::codec::field{"name", &ore_file_write::name},
        utility::codec::field{"category_code", &ore_file_write::category_code},
        utility::codec::field{"configuration_type_code", &ore_file_write::configuration_type_code},
        utility::codec::field{"guide_section", &ore_file_write::guide_section},
        utility::codec::field{"origin", &ore_file_write::origin},
        utility::codec::field{"format", &ore_file_write::format},
        utility::codec::field{"schema_ore_file_code", &ore_file_write::schema_ore_file_code},
        utility::codec::field{"typical_filename", &ore_file_write::typical_filename},
        utility::codec::field{"filename_regex", &ore_file_write::filename_regex},
        utility::codec::field{"description", &ore_file_write::description},
        utility::codec::field{"display_order", &ore_file_write::display_order},
    };
}

/**
 * @brief The members of ore_file_change, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<ore_file_change>) {
    return std::tuple{
        utility::codec::field{"write", &ore_file_change::write},
        utility::codec::field{"precondition", &ore_file_change::precondition},
    };
}

/**
 * @brief The members of ore_file_removal, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<ore_file_removal>) {
    return std::tuple{
        utility::codec::field{"key", &ore_file_removal::key},
        utility::codec::field{"precondition", &ore_file_removal::precondition},
    };
}

/**
 * @brief The members of ore_file_lookup, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<ore_file_lookup>) {
    return std::tuple{
        utility::codec::field{"key", &ore_file_lookup::key},
        utility::codec::field{"ore_file", &ore_file_lookup::ore_file},
    };
}

/**
 * @brief The members of ore_files_filter, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<ore_files_filter>) {
    return std::tuple{
        utility::codec::field{"category_code", &ore_files_filter::category_code},
        utility::codec::field{"configuration_type_code",
                              &ore_files_filter::configuration_type_code},
        utility::codec::field{"schema_ore_file_code", &ore_files_filter::schema_ore_file_code},
        utility::codec::field{"code_one_of", &ore_files_filter::code_one_of},
        utility::codec::field{"category_code_one_of", &ore_files_filter::category_code_one_of},
        utility::codec::field{"configuration_type_code_one_of",
                              &ore_files_filter::configuration_type_code_one_of},
        utility::codec::field{"schema_ore_file_code_one_of",
                              &ore_files_filter::schema_ore_file_code_one_of},
    };
}

/**
 * @brief The members of ore_file_event, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<ore_file_event>) {
    return std::tuple{
        utility::codec::field{"event_id", &ore_file_event::event_id},
        utility::codec::field{"key", &ore_file_event::key},
        utility::codec::field{"action", &ore_file_event::action},
        utility::codec::field{"version", &ore_file_event::version},
        utility::codec::field{"occurred_at", &ore_file_event::occurred_at},
        utility::codec::field{"correlation_id", &ore_file_event::correlation_id},
    };
}

/**
 * @brief The members of ore_files_version_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<ore_files_version_key>) {
    return std::tuple{
        utility::codec::field{"ore_file", &ore_files_version_key::ore_file},
        utility::codec::field{"version", &ore_files_version_key::version},
    };
}

/**
 * @brief The members of ore_files_versions_filter, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<ore_files_versions_filter>) {
    return std::tuple{
        utility::codec::field{"version", &ore_files_versions_filter::version},
        utility::codec::field{"from_version", &ore_files_versions_filter::from_version},
        utility::codec::field{"to_version", &ore_files_versions_filter::to_version},
    };
}

/**
 * @brief The members of list_ore_files_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<list_ore_files_request>) {
    return std::tuple{
        utility::codec::field{"offset", &list_ore_files_request::offset},
        utility::codec::field{"limit", &list_ore_files_request::limit},
        utility::codec::field{"order", &list_ore_files_request::order},
        utility::codec::field{"filter", &list_ore_files_request::filter},
        utility::codec::field{"as_of", &list_ore_files_request::as_of},
    };
}

/**
 * @brief The members of list_ore_files_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<list_ore_files_response>) {
    return std::tuple{
        utility::codec::field{"result", &list_ore_files_response::result},
        utility::codec::field{"files", &list_ore_files_response::files},
        utility::codec::field{"total", &list_ore_files_response::total},
    };
}

/**
 * @brief The members of get_ore_file_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_ore_file_request>) {
    return std::tuple{
        utility::codec::field{"key", &get_ore_file_request::key},
    };
}

/**
 * @brief The members of get_ore_file_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_ore_file_response>) {
    return std::tuple{
        utility::codec::field{"result", &get_ore_file_response::result},
        utility::codec::field{"ore_file", &get_ore_file_response::ore_file},
    };
}

/**
 * @brief The members of get_many_ore_files_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_many_ore_files_request>) {
    return std::tuple{
        utility::codec::field{"keys", &get_many_ore_files_request::keys},
    };
}

/**
 * @brief The members of get_many_ore_files_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_many_ore_files_response>) {
    return std::tuple{
        utility::codec::field{"result", &get_many_ore_files_response::result},
        utility::codec::field{"entries", &get_many_ore_files_response::entries},
    };
}

/**
 * @brief The members of put_ore_file_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<put_ore_file_request>) {
    return std::tuple{
        utility::codec::field{"change", &put_ore_file_request::change},
        utility::codec::field{"intent", &put_ore_file_request::intent},
    };
}

/**
 * @brief The members of put_ore_file_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<put_ore_file_response>) {
    return std::tuple{
        utility::codec::field{"result", &put_ore_file_response::result},
        utility::codec::field{"ore_file", &put_ore_file_response::ore_file},
    };
}

/**
 * @brief The members of put_many_ore_files_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<put_many_ore_files_request>) {
    return std::tuple{
        utility::codec::field{"changes", &put_many_ore_files_request::changes},
        utility::codec::field{"intent", &put_many_ore_files_request::intent},
    };
}

/**
 * @brief The members of put_many_ore_files_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<put_many_ore_files_response>) {
    return std::tuple{
        utility::codec::field{"result", &put_many_ore_files_response::result},
        utility::codec::field{"files", &put_many_ore_files_response::files},
    };
}

/**
 * @brief The members of delete_ore_file_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<delete_ore_file_request>) {
    return std::tuple{
        utility::codec::field{"removal", &delete_ore_file_request::removal},
        utility::codec::field{"intent", &delete_ore_file_request::intent},
    };
}

/**
 * @brief The members of delete_ore_file_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<delete_ore_file_response>) {
    return std::tuple{
        utility::codec::field{"result", &delete_ore_file_response::result},
    };
}

/**
 * @brief The members of delete_many_ore_files_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<delete_many_ore_files_request>) {
    return std::tuple{
        utility::codec::field{"removals", &delete_many_ore_files_request::removals},
        utility::codec::field{"intent", &delete_many_ore_files_request::intent},
    };
}

/**
 * @brief The members of delete_many_ore_files_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<delete_many_ore_files_response>) {
    return std::tuple{
        utility::codec::field{"result", &delete_many_ore_files_response::result},
    };
}

/**
 * @brief The members of list_by_category_code_ore_files_request, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<list_by_category_code_ore_files_request>) {
    return std::tuple{
        utility::codec::field{"category_code",
                              &list_by_category_code_ore_files_request::category_code},
        utility::codec::field{"scope", &list_by_category_code_ore_files_request::scope},
        utility::codec::field{"offset", &list_by_category_code_ore_files_request::offset},
        utility::codec::field{"limit", &list_by_category_code_ore_files_request::limit},
        utility::codec::field{"order", &list_by_category_code_ore_files_request::order},
        utility::codec::field{"filter", &list_by_category_code_ore_files_request::filter},
    };
}

/**
 * @brief The members of list_by_category_code_ore_files_response, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<list_by_category_code_ore_files_response>) {
    return std::tuple{
        utility::codec::field{"result", &list_by_category_code_ore_files_response::result},
        utility::codec::field{"files", &list_by_category_code_ore_files_response::files},
        utility::codec::field{"total", &list_by_category_code_ore_files_response::total},
    };
}

/**
 * @brief The members of list_by_configuration_type_code_ore_files_request, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<list_by_configuration_type_code_ore_files_request>) {
    return std::tuple{
        utility::codec::field{
            "configuration_type_code",
            &list_by_configuration_type_code_ore_files_request::configuration_type_code},
        utility::codec::field{"scope", &list_by_configuration_type_code_ore_files_request::scope},
        utility::codec::field{"offset", &list_by_configuration_type_code_ore_files_request::offset},
        utility::codec::field{"limit", &list_by_configuration_type_code_ore_files_request::limit},
        utility::codec::field{"order", &list_by_configuration_type_code_ore_files_request::order},
        utility::codec::field{"filter", &list_by_configuration_type_code_ore_files_request::filter},
    };
}

/**
 * @brief The members of list_by_configuration_type_code_ore_files_response, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<list_by_configuration_type_code_ore_files_response>) {
    return std::tuple{
        utility::codec::field{"result",
                              &list_by_configuration_type_code_ore_files_response::result},
        utility::codec::field{"files", &list_by_configuration_type_code_ore_files_response::files},
        utility::codec::field{"total", &list_by_configuration_type_code_ore_files_response::total},
    };
}

/**
 * @brief The members of list_by_schema_ore_file_code_ore_files_request, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<list_by_schema_ore_file_code_ore_files_request>) {
    return std::tuple{
        utility::codec::field{
            "schema_ore_file_code",
            &list_by_schema_ore_file_code_ore_files_request::schema_ore_file_code},
        utility::codec::field{"scope", &list_by_schema_ore_file_code_ore_files_request::scope},
        utility::codec::field{"offset", &list_by_schema_ore_file_code_ore_files_request::offset},
        utility::codec::field{"limit", &list_by_schema_ore_file_code_ore_files_request::limit},
        utility::codec::field{"order", &list_by_schema_ore_file_code_ore_files_request::order},
        utility::codec::field{"filter", &list_by_schema_ore_file_code_ore_files_request::filter},
    };
}

/**
 * @brief The members of list_by_schema_ore_file_code_ore_files_response, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<list_by_schema_ore_file_code_ore_files_response>) {
    return std::tuple{
        utility::codec::field{"result", &list_by_schema_ore_file_code_ore_files_response::result},
        utility::codec::field{"files", &list_by_schema_ore_file_code_ore_files_response::files},
        utility::codec::field{"total", &list_by_schema_ore_file_code_ore_files_response::total},
    };
}

/**
 * @brief The members of list_ore_files_versions_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<list_ore_files_versions_request>) {
    return std::tuple{
        utility::codec::field{"key", &list_ore_files_versions_request::key},
        utility::codec::field{"offset", &list_ore_files_versions_request::offset},
        utility::codec::field{"limit", &list_ore_files_versions_request::limit},
        utility::codec::field{"order", &list_ore_files_versions_request::order},
        utility::codec::field{"filter", &list_ore_files_versions_request::filter},
    };
}

/**
 * @brief The members of list_ore_files_versions_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<list_ore_files_versions_response>) {
    return std::tuple{
        utility::codec::field{"result", &list_ore_files_versions_response::result},
        utility::codec::field{"versions", &list_ore_files_versions_response::versions},
        utility::codec::field{"total", &list_ore_files_versions_response::total},
    };
}

/**
 * @brief The members of get_ore_files_version_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_ore_files_version_request>) {
    return std::tuple{
        utility::codec::field{"key", &get_ore_files_version_request::key},
    };
}

/**
 * @brief The members of get_ore_files_version_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_ore_files_version_response>) {
    return std::tuple{
        utility::codec::field{"result", &get_ore_files_version_response::result},
        utility::codec::field{"version", &get_ore_files_version_response::version},
    };
}


}
namespace ores::utility::codec {

template <>
struct ORES_REPORTING_API_EXPORT wire_traits<ores::reporting::messaging::ore_file_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::reporting::messaging::ore_file_key& v);
    static ores::reporting::messaging::ore_file_key read_json(std::string_view text);
    static std::string write_msgpack(const ores::reporting::messaging::ore_file_key& v);
    static ores::reporting::messaging::ore_file_key read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT wire_traits<ores::reporting::messaging::ore_file_write> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::reporting::messaging::ore_file_write& v);
    static ores::reporting::messaging::ore_file_write read_json(std::string_view text);
    static std::string write_msgpack(const ores::reporting::messaging::ore_file_write& v);
    static ores::reporting::messaging::ore_file_write read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT wire_traits<ores::reporting::messaging::ore_file_change> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::reporting::messaging::ore_file_change& v);
    static ores::reporting::messaging::ore_file_change read_json(std::string_view text);
    static std::string write_msgpack(const ores::reporting::messaging::ore_file_change& v);
    static ores::reporting::messaging::ore_file_change
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT wire_traits<ores::reporting::messaging::ore_file_removal> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::reporting::messaging::ore_file_removal& v);
    static ores::reporting::messaging::ore_file_removal read_json(std::string_view text);
    static std::string write_msgpack(const ores::reporting::messaging::ore_file_removal& v);
    static ores::reporting::messaging::ore_file_removal
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT wire_traits<ores::reporting::messaging::ore_file_lookup> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::reporting::messaging::ore_file_lookup& v);
    static ores::reporting::messaging::ore_file_lookup read_json(std::string_view text);
    static std::string write_msgpack(const ores::reporting::messaging::ore_file_lookup& v);
    static ores::reporting::messaging::ore_file_lookup
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT wire_traits<ores::reporting::messaging::ore_files_filter> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::reporting::messaging::ore_files_filter& v);
    static ores::reporting::messaging::ore_files_filter read_json(std::string_view text);
    static std::string write_msgpack(const ores::reporting::messaging::ore_files_filter& v);
    static ores::reporting::messaging::ore_files_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT wire_traits<ores::reporting::messaging::ore_file_event> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::reporting::messaging::ore_file_event& v);
    static ores::reporting::messaging::ore_file_event read_json(std::string_view text);
    static std::string write_msgpack(const ores::reporting::messaging::ore_file_event& v);
    static ores::reporting::messaging::ore_file_event read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT wire_traits<ores::reporting::messaging::ore_files_version_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::reporting::messaging::ore_files_version_key& v);
    static ores::reporting::messaging::ore_files_version_key read_json(std::string_view text);
    static std::string write_msgpack(const ores::reporting::messaging::ore_files_version_key& v);
    static ores::reporting::messaging::ore_files_version_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::ore_files_versions_filter> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::reporting::messaging::ore_files_versions_filter& v);
    static ores::reporting::messaging::ore_files_versions_filter read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::ore_files_versions_filter& v);
    static ores::reporting::messaging::ore_files_versions_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT wire_traits<ores::reporting::messaging::list_ore_files_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::reporting::messaging::list_ore_files_request& v);
    static ores::reporting::messaging::list_ore_files_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::reporting::messaging::list_ore_files_request& v);
    static ores::reporting::messaging::list_ore_files_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT wire_traits<ores::reporting::messaging::list_ore_files_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::reporting::messaging::list_ore_files_response& v);
    static ores::reporting::messaging::list_ore_files_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::reporting::messaging::list_ore_files_response& v);
    static ores::reporting::messaging::list_ore_files_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT wire_traits<ores::reporting::messaging::get_ore_file_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::reporting::messaging::get_ore_file_request& v);
    static ores::reporting::messaging::get_ore_file_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::reporting::messaging::get_ore_file_request& v);
    static ores::reporting::messaging::get_ore_file_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT wire_traits<ores::reporting::messaging::get_ore_file_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::reporting::messaging::get_ore_file_response& v);
    static ores::reporting::messaging::get_ore_file_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::reporting::messaging::get_ore_file_response& v);
    static ores::reporting::messaging::get_ore_file_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::get_many_ore_files_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::reporting::messaging::get_many_ore_files_request& v);
    static ores::reporting::messaging::get_many_ore_files_request read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::get_many_ore_files_request& v);
    static ores::reporting::messaging::get_many_ore_files_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::get_many_ore_files_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::reporting::messaging::get_many_ore_files_response& v);
    static ores::reporting::messaging::get_many_ore_files_response read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::get_many_ore_files_response& v);
    static ores::reporting::messaging::get_many_ore_files_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT wire_traits<ores::reporting::messaging::put_ore_file_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::reporting::messaging::put_ore_file_request& v);
    static ores::reporting::messaging::put_ore_file_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::reporting::messaging::put_ore_file_request& v);
    static ores::reporting::messaging::put_ore_file_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT wire_traits<ores::reporting::messaging::put_ore_file_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::reporting::messaging::put_ore_file_response& v);
    static ores::reporting::messaging::put_ore_file_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::reporting::messaging::put_ore_file_response& v);
    static ores::reporting::messaging::put_ore_file_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::put_many_ore_files_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::reporting::messaging::put_many_ore_files_request& v);
    static ores::reporting::messaging::put_many_ore_files_request read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::put_many_ore_files_request& v);
    static ores::reporting::messaging::put_many_ore_files_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::put_many_ore_files_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::reporting::messaging::put_many_ore_files_response& v);
    static ores::reporting::messaging::put_many_ore_files_response read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::put_many_ore_files_response& v);
    static ores::reporting::messaging::put_many_ore_files_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT wire_traits<ores::reporting::messaging::delete_ore_file_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::reporting::messaging::delete_ore_file_request& v);
    static ores::reporting::messaging::delete_ore_file_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::reporting::messaging::delete_ore_file_request& v);
    static ores::reporting::messaging::delete_ore_file_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT wire_traits<ores::reporting::messaging::delete_ore_file_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::reporting::messaging::delete_ore_file_response& v);
    static ores::reporting::messaging::delete_ore_file_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::reporting::messaging::delete_ore_file_response& v);
    static ores::reporting::messaging::delete_ore_file_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::delete_many_ore_files_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::delete_many_ore_files_request& v);
    static ores::reporting::messaging::delete_many_ore_files_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::delete_many_ore_files_request& v);
    static ores::reporting::messaging::delete_many_ore_files_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::delete_many_ore_files_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::delete_many_ore_files_response& v);
    static ores::reporting::messaging::delete_many_ore_files_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::delete_many_ore_files_response& v);
    static ores::reporting::messaging::delete_many_ore_files_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::list_by_category_code_ore_files_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::list_by_category_code_ore_files_request& v);
    static ores::reporting::messaging::list_by_category_code_ore_files_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::list_by_category_code_ore_files_request& v);
    static ores::reporting::messaging::list_by_category_code_ore_files_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::list_by_category_code_ore_files_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::list_by_category_code_ore_files_response& v);
    static ores::reporting::messaging::list_by_category_code_ore_files_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::list_by_category_code_ore_files_response& v);
    static ores::reporting::messaging::list_by_category_code_ore_files_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::list_by_configuration_type_code_ore_files_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::reporting::messaging::list_by_configuration_type_code_ore_files_request& v);
    static ores::reporting::messaging::list_by_configuration_type_code_ore_files_request
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::reporting::messaging::list_by_configuration_type_code_ore_files_request& v);
    static ores::reporting::messaging::list_by_configuration_type_code_ore_files_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::list_by_configuration_type_code_ore_files_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::reporting::messaging::list_by_configuration_type_code_ore_files_response& v);
    static ores::reporting::messaging::list_by_configuration_type_code_ore_files_response
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::reporting::messaging::list_by_configuration_type_code_ore_files_response& v);
    static ores::reporting::messaging::list_by_configuration_type_code_ore_files_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::list_by_schema_ore_file_code_ore_files_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::list_by_schema_ore_file_code_ore_files_request& v);
    static ores::reporting::messaging::list_by_schema_ore_file_code_ore_files_request
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::reporting::messaging::list_by_schema_ore_file_code_ore_files_request& v);
    static ores::reporting::messaging::list_by_schema_ore_file_code_ore_files_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::list_by_schema_ore_file_code_ore_files_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::reporting::messaging::list_by_schema_ore_file_code_ore_files_response& v);
    static ores::reporting::messaging::list_by_schema_ore_file_code_ore_files_response
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::reporting::messaging::list_by_schema_ore_file_code_ore_files_response& v);
    static ores::reporting::messaging::list_by_schema_ore_file_code_ore_files_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::list_ore_files_versions_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::list_ore_files_versions_request& v);
    static ores::reporting::messaging::list_ore_files_versions_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::list_ore_files_versions_request& v);
    static ores::reporting::messaging::list_ore_files_versions_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::list_ore_files_versions_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::list_ore_files_versions_response& v);
    static ores::reporting::messaging::list_ore_files_versions_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::list_ore_files_versions_response& v);
    static ores::reporting::messaging::list_ore_files_versions_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::get_ore_files_version_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::get_ore_files_version_request& v);
    static ores::reporting::messaging::get_ore_files_version_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::get_ore_files_version_request& v);
    static ores::reporting::messaging::get_ore_files_version_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::get_ore_files_version_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::get_ore_files_version_response& v);
    static ores::reporting::messaging::get_ore_files_version_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::get_ore_files_version_response& v);
    static ores::reporting::messaging::get_ore_files_version_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
