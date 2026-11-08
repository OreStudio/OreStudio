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
#ifndef ORES_SYNTHETIC_API_MESSAGING_YIELD_CURVE_PROCESS_PARAMETER_DEFINITION_PROTOCOL_HPP
#define ORES_SYNTHETIC_API_MESSAGING_YIELD_CURVE_PROCESS_PARAMETER_DEFINITION_PROTOCOL_HPP

#include "ores.synthetic.api/domain/yield_curve_process_parameter_definition_codec.hpp"
#include "ores.synthetic.api/export.hpp"
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

namespace ores::synthetic::messaging {

struct yield_curve_process_parameter_definition_key {
    std::string parameter_name;

    friend bool operator==(const yield_curve_process_parameter_definition_key&,
                           const yield_curve_process_parameter_definition_key&) = default;
};

struct yield_curve_process_parameter_definition_write {
    boost::uuids::uuid id;
    std::string process_type_code;
    std::string parameter_name;
    std::string display_name;
    std::optional<std::string> symbol;
    std::string short_label;
    std::string description;
    std::string data_type;
    double default_value{};
    std::optional<double> min_value;
    std::optional<double> max_value;
    int display_order{};

    friend bool operator==(const yield_curve_process_parameter_definition_write&,
                           const yield_curve_process_parameter_definition_write&) = default;
};

struct yield_curve_process_parameter_definition_change {
    yield_curve_process_parameter_definition_write write;
    ores::utility::domain::precondition precondition;

    friend bool operator==(const yield_curve_process_parameter_definition_change&,
                           const yield_curve_process_parameter_definition_change&) = default;
};

struct yield_curve_process_parameter_definition_removal {
    yield_curve_process_parameter_definition_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;

    friend bool operator==(const yield_curve_process_parameter_definition_removal&,
                           const yield_curve_process_parameter_definition_removal&) = default;
};

struct yield_curve_process_parameter_definition_lookup {
    yield_curve_process_parameter_definition_key key;
    std::optional<ores::synthetic::domain::yield_curve_process_parameter_definition>
        yield_curve_process_parameter_definition;

    friend bool operator==(const yield_curve_process_parameter_definition_lookup&,
                           const yield_curve_process_parameter_definition_lookup&) = default;
};

struct yield_curve_process_parameter_definitions_filter {
    std::optional<std::vector<boost::uuids::uuid>> id_one_of;

    friend bool operator==(const yield_curve_process_parameter_definitions_filter&,
                           const yield_curve_process_parameter_definitions_filter&) = default;
};

struct yield_curve_process_parameter_definition_event {
    boost::uuids::uuid event_id;
    yield_curve_process_parameter_definition_key key;
    std::string action;
    std::uint32_t version{};
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;

    friend bool operator==(const yield_curve_process_parameter_definition_event&,
                           const yield_curve_process_parameter_definition_event&) = default;
};

struct yield_curve_process_parameter_definition_version_key {
    yield_curve_process_parameter_definition_key yield_curve_process_parameter_definition;
    std::uint32_t version{};

    friend bool operator==(const yield_curve_process_parameter_definition_version_key&,
                           const yield_curve_process_parameter_definition_version_key&) = default;
};

struct yield_curve_process_parameter_definition_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;

    friend bool
    operator==(const yield_curve_process_parameter_definition_versions_filter&,
               const yield_curve_process_parameter_definition_versions_filter&) = default;
};

struct list_yield_curve_process_parameter_definitions_request {
    using response_type = struct list_yield_curve_process_parameter_definitions_response;
    static constexpr std::string_view nats_subject =
        "synthetic.v1.yield_curve_process_parameter_definitions.list";
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
    std::optional<yield_curve_process_parameter_definitions_filter> filter;
    std::optional<std::string> as_of;

    friend bool operator==(const list_yield_curve_process_parameter_definitions_request&,
                           const list_yield_curve_process_parameter_definitions_request&) = default;
};

struct list_yield_curve_process_parameter_definitions_response {
    ores::utility::domain::result result;
    std::vector<ores::synthetic::domain::yield_curve_process_parameter_definition>
        parameter_definitions;
    std::uint64_t total{};

    friend bool
    operator==(const list_yield_curve_process_parameter_definitions_response&,
               const list_yield_curve_process_parameter_definitions_response&) = default;
};

struct get_yield_curve_process_parameter_definition_request {
    using response_type = struct get_yield_curve_process_parameter_definition_response;
    static constexpr std::string_view nats_subject =
        "synthetic.v1.yield_curve_process_parameter_definitions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    yield_curve_process_parameter_definition_key key;

    friend bool operator==(const get_yield_curve_process_parameter_definition_request&,
                           const get_yield_curve_process_parameter_definition_request&) = default;
};

struct get_yield_curve_process_parameter_definition_response {
    ores::utility::domain::result result;
    std::optional<ores::synthetic::domain::yield_curve_process_parameter_definition>
        yield_curve_process_parameter_definition;

    friend bool operator==(const get_yield_curve_process_parameter_definition_response&,
                           const get_yield_curve_process_parameter_definition_response&) = default;
};

struct get_many_yield_curve_process_parameter_definitions_request {
    using response_type = struct get_many_yield_curve_process_parameter_definitions_response;
    static constexpr std::string_view nats_subject =
        "synthetic.v1.yield_curve_process_parameter_definitions.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<yield_curve_process_parameter_definition_key> keys;

    friend bool
    operator==(const get_many_yield_curve_process_parameter_definitions_request&,
               const get_many_yield_curve_process_parameter_definitions_request&) = default;
};

struct get_many_yield_curve_process_parameter_definitions_response {
    ores::utility::domain::result result;
    std::vector<yield_curve_process_parameter_definition_lookup> entries;

    friend bool
    operator==(const get_many_yield_curve_process_parameter_definitions_response&,
               const get_many_yield_curve_process_parameter_definitions_response&) = default;
};

struct put_yield_curve_process_parameter_definition_request {
    using response_type = struct put_yield_curve_process_parameter_definition_response;
    static constexpr std::string_view nats_subject =
        "synthetic.v1.yield_curve_process_parameter_definitions.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    yield_curve_process_parameter_definition_change change;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const put_yield_curve_process_parameter_definition_request&,
                           const put_yield_curve_process_parameter_definition_request&) = default;
};

struct put_yield_curve_process_parameter_definition_response {
    ores::utility::domain::result result;
    std::optional<ores::synthetic::domain::yield_curve_process_parameter_definition>
        yield_curve_process_parameter_definition;

    friend bool operator==(const put_yield_curve_process_parameter_definition_response&,
                           const put_yield_curve_process_parameter_definition_response&) = default;
};

struct put_many_yield_curve_process_parameter_definitions_request {
    using response_type = struct put_many_yield_curve_process_parameter_definitions_response;
    static constexpr std::string_view nats_subject =
        "synthetic.v1.yield_curve_process_parameter_definitions.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<yield_curve_process_parameter_definition_change> changes;
    ores::utility::domain::change_intent intent;

    friend bool
    operator==(const put_many_yield_curve_process_parameter_definitions_request&,
               const put_many_yield_curve_process_parameter_definitions_request&) = default;
};

struct put_many_yield_curve_process_parameter_definitions_response {
    ores::utility::domain::result result;
    std::vector<ores::synthetic::domain::yield_curve_process_parameter_definition>
        parameter_definitions;

    friend bool
    operator==(const put_many_yield_curve_process_parameter_definitions_response&,
               const put_many_yield_curve_process_parameter_definitions_response&) = default;
};

struct delete_yield_curve_process_parameter_definition_request {
    using response_type = struct delete_yield_curve_process_parameter_definition_response;
    static constexpr std::string_view nats_subject =
        "synthetic.v1.yield_curve_process_parameter_definitions.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    yield_curve_process_parameter_definition_removal removal;
    ores::utility::domain::change_intent intent;

    friend bool
    operator==(const delete_yield_curve_process_parameter_definition_request&,
               const delete_yield_curve_process_parameter_definition_request&) = default;
};

struct delete_yield_curve_process_parameter_definition_response {
    ores::utility::domain::result result;

    friend bool
    operator==(const delete_yield_curve_process_parameter_definition_response&,
               const delete_yield_curve_process_parameter_definition_response&) = default;
};

struct delete_many_yield_curve_process_parameter_definitions_request {
    using response_type = struct delete_many_yield_curve_process_parameter_definitions_response;
    static constexpr std::string_view nats_subject =
        "synthetic.v1.yield_curve_process_parameter_definitions.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<yield_curve_process_parameter_definition_removal> removals;
    ores::utility::domain::change_intent intent;

    friend bool
    operator==(const delete_many_yield_curve_process_parameter_definitions_request&,
               const delete_many_yield_curve_process_parameter_definitions_request&) = default;
};

struct delete_many_yield_curve_process_parameter_definitions_response {
    ores::utility::domain::result result;

    friend bool
    operator==(const delete_many_yield_curve_process_parameter_definitions_response&,
               const delete_many_yield_curve_process_parameter_definitions_response&) = default;
};

struct list_yield_curve_process_parameter_definition_versions_request {
    using response_type = struct list_yield_curve_process_parameter_definition_versions_response;
    static constexpr std::string_view nats_subject =
        "synthetic.v1.yield_curve_process_parameter_definitions_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    yield_curve_process_parameter_definition_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<yield_curve_process_parameter_definition_versions_filter> filter;

    friend bool
    operator==(const list_yield_curve_process_parameter_definition_versions_request&,
               const list_yield_curve_process_parameter_definition_versions_request&) = default;
};

struct list_yield_curve_process_parameter_definition_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::synthetic::domain::yield_curve_process_parameter_definition> versions;
    std::uint64_t total{};

    friend bool
    operator==(const list_yield_curve_process_parameter_definition_versions_response&,
               const list_yield_curve_process_parameter_definition_versions_response&) = default;
};

struct get_yield_curve_process_parameter_definition_version_request {
    using response_type = struct get_yield_curve_process_parameter_definition_version_response;
    static constexpr std::string_view nats_subject =
        "synthetic.v1.yield_curve_process_parameter_definitions_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    yield_curve_process_parameter_definition_version_key key;

    friend bool
    operator==(const get_yield_curve_process_parameter_definition_version_request&,
               const get_yield_curve_process_parameter_definition_version_request&) = default;
};

struct get_yield_curve_process_parameter_definition_version_response {
    ores::utility::domain::result result;
    std::optional<ores::synthetic::domain::yield_curve_process_parameter_definition> version;

    friend bool
    operator==(const get_yield_curve_process_parameter_definition_version_response&,
               const get_yield_curve_process_parameter_definition_version_response&) = default;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace yield_curve_process_parameter_definition_event_subjects {
inline constexpr std::string_view created =
    "synthetic.v1.events.yield_curve_process_parameter_definitions.created";
inline constexpr std::string_view updated =
    "synthetic.v1.events.yield_curve_process_parameter_definitions.updated";
inline constexpr std::string_view deleted =
    "synthetic.v1.events.yield_curve_process_parameter_definitions.deleted";
}
/**
 * @brief The members of yield_curve_process_parameter_definition_key, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<yield_curve_process_parameter_definition_key>) {
    return std::tuple{
        utility::codec::field{"parameter_name",
                              &yield_curve_process_parameter_definition_key::parameter_name},
    };
}

/**
 * @brief The members of yield_curve_process_parameter_definition_write, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<yield_curve_process_parameter_definition_write>) {
    return std::tuple{
        utility::codec::field{"id", &yield_curve_process_parameter_definition_write::id},
        utility::codec::field{"process_type_code",
                              &yield_curve_process_parameter_definition_write::process_type_code},
        utility::codec::field{"parameter_name",
                              &yield_curve_process_parameter_definition_write::parameter_name},
        utility::codec::field{"display_name",
                              &yield_curve_process_parameter_definition_write::display_name},
        utility::codec::field{"symbol", &yield_curve_process_parameter_definition_write::symbol},
        utility::codec::field{"short_label",
                              &yield_curve_process_parameter_definition_write::short_label},
        utility::codec::field{"description",
                              &yield_curve_process_parameter_definition_write::description},
        utility::codec::field{"data_type",
                              &yield_curve_process_parameter_definition_write::data_type},
        utility::codec::field{"default_value",
                              &yield_curve_process_parameter_definition_write::default_value},
        utility::codec::field{"min_value",
                              &yield_curve_process_parameter_definition_write::min_value},
        utility::codec::field{"max_value",
                              &yield_curve_process_parameter_definition_write::max_value},
        utility::codec::field{"display_order",
                              &yield_curve_process_parameter_definition_write::display_order},
    };
}

/**
 * @brief The members of yield_curve_process_parameter_definition_change, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<yield_curve_process_parameter_definition_change>) {
    return std::tuple{
        utility::codec::field{"write", &yield_curve_process_parameter_definition_change::write},
        utility::codec::field{"precondition",
                              &yield_curve_process_parameter_definition_change::precondition},
    };
}

/**
 * @brief The members of yield_curve_process_parameter_definition_removal, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<yield_curve_process_parameter_definition_removal>) {
    return std::tuple{
        utility::codec::field{"key", &yield_curve_process_parameter_definition_removal::key},
        utility::codec::field{"precondition",
                              &yield_curve_process_parameter_definition_removal::precondition},
    };
}

/**
 * @brief The members of yield_curve_process_parameter_definition_lookup, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<yield_curve_process_parameter_definition_lookup>) {
    return std::tuple{
        utility::codec::field{"key", &yield_curve_process_parameter_definition_lookup::key},
        utility::codec::field{"yield_curve_process_parameter_definition",
                              &yield_curve_process_parameter_definition_lookup::
                                  yield_curve_process_parameter_definition},
    };
}

/**
 * @brief The members of yield_curve_process_parameter_definitions_filter, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<yield_curve_process_parameter_definitions_filter>) {
    return std::tuple{
        utility::codec::field{"id_one_of",
                              &yield_curve_process_parameter_definitions_filter::id_one_of},
    };
}

/**
 * @brief The members of yield_curve_process_parameter_definition_event, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<yield_curve_process_parameter_definition_event>) {
    return std::tuple{
        utility::codec::field{"event_id",
                              &yield_curve_process_parameter_definition_event::event_id},
        utility::codec::field{"key", &yield_curve_process_parameter_definition_event::key},
        utility::codec::field{"action", &yield_curve_process_parameter_definition_event::action},
        utility::codec::field{"version", &yield_curve_process_parameter_definition_event::version},
        utility::codec::field{"occurred_at",
                              &yield_curve_process_parameter_definition_event::occurred_at},
        utility::codec::field{"correlation_id",
                              &yield_curve_process_parameter_definition_event::correlation_id},
    };
}

/**
 * @brief The members of yield_curve_process_parameter_definition_version_key, named as the wire
 * names them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<yield_curve_process_parameter_definition_version_key>) {
    return std::tuple{
        utility::codec::field{"yield_curve_process_parameter_definition",
                              &yield_curve_process_parameter_definition_version_key::
                                  yield_curve_process_parameter_definition},
        utility::codec::field{"version",
                              &yield_curve_process_parameter_definition_version_key::version},
    };
}

/**
 * @brief The members of yield_curve_process_parameter_definition_versions_filter, named as the wire
 * names them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<yield_curve_process_parameter_definition_versions_filter>) {
    return std::tuple{
        utility::codec::field{"version",
                              &yield_curve_process_parameter_definition_versions_filter::version},
        utility::codec::field{
            "from_version",
            &yield_curve_process_parameter_definition_versions_filter::from_version},
        utility::codec::field{
            "to_version", &yield_curve_process_parameter_definition_versions_filter::to_version},
    };
}

/**
 * @brief The members of list_yield_curve_process_parameter_definitions_request, named as the wire
 * names them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<list_yield_curve_process_parameter_definitions_request>) {
    return std::tuple{
        utility::codec::field{"offset",
                              &list_yield_curve_process_parameter_definitions_request::offset},
        utility::codec::field{"limit",
                              &list_yield_curve_process_parameter_definitions_request::limit},
        utility::codec::field{"order",
                              &list_yield_curve_process_parameter_definitions_request::order},
        utility::codec::field{"filter",
                              &list_yield_curve_process_parameter_definitions_request::filter},
        utility::codec::field{"as_of",
                              &list_yield_curve_process_parameter_definitions_request::as_of},
    };
}

/**
 * @brief The members of list_yield_curve_process_parameter_definitions_response, named as the wire
 * names them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<list_yield_curve_process_parameter_definitions_response>) {
    return std::tuple{
        utility::codec::field{"result",
                              &list_yield_curve_process_parameter_definitions_response::result},
        utility::codec::field{
            "parameter_definitions",
            &list_yield_curve_process_parameter_definitions_response::parameter_definitions},
        utility::codec::field{"total",
                              &list_yield_curve_process_parameter_definitions_response::total},
    };
}

/**
 * @brief The members of get_yield_curve_process_parameter_definition_request, named as the wire
 * names them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<get_yield_curve_process_parameter_definition_request>) {
    return std::tuple{
        utility::codec::field{"key", &get_yield_curve_process_parameter_definition_request::key},
    };
}

/**
 * @brief The members of get_yield_curve_process_parameter_definition_response, named as the wire
 * names them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<get_yield_curve_process_parameter_definition_response>) {
    return std::tuple{
        utility::codec::field{"result",
                              &get_yield_curve_process_parameter_definition_response::result},
        utility::codec::field{"yield_curve_process_parameter_definition",
                              &get_yield_curve_process_parameter_definition_response::
                                  yield_curve_process_parameter_definition},
    };
}

/**
 * @brief The members of get_many_yield_curve_process_parameter_definitions_request, named as the
 * wire names them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<get_many_yield_curve_process_parameter_definitions_request>) {
    return std::tuple{
        utility::codec::field{"keys",
                              &get_many_yield_curve_process_parameter_definitions_request::keys},
    };
}

/**
 * @brief The members of get_many_yield_curve_process_parameter_definitions_response, named as the
 * wire names them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<get_many_yield_curve_process_parameter_definitions_response>) {
    return std::tuple{
        utility::codec::field{"result",
                              &get_many_yield_curve_process_parameter_definitions_response::result},
        utility::codec::field{
            "entries", &get_many_yield_curve_process_parameter_definitions_response::entries},
    };
}

/**
 * @brief The members of put_yield_curve_process_parameter_definition_request, named as the wire
 * names them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<put_yield_curve_process_parameter_definition_request>) {
    return std::tuple{
        utility::codec::field{"change",
                              &put_yield_curve_process_parameter_definition_request::change},
        utility::codec::field{"intent",
                              &put_yield_curve_process_parameter_definition_request::intent},
    };
}

/**
 * @brief The members of put_yield_curve_process_parameter_definition_response, named as the wire
 * names them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<put_yield_curve_process_parameter_definition_response>) {
    return std::tuple{
        utility::codec::field{"result",
                              &put_yield_curve_process_parameter_definition_response::result},
        utility::codec::field{"yield_curve_process_parameter_definition",
                              &put_yield_curve_process_parameter_definition_response::
                                  yield_curve_process_parameter_definition},
    };
}

/**
 * @brief The members of put_many_yield_curve_process_parameter_definitions_request, named as the
 * wire names them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<put_many_yield_curve_process_parameter_definitions_request>) {
    return std::tuple{
        utility::codec::field{"changes",
                              &put_many_yield_curve_process_parameter_definitions_request::changes},
        utility::codec::field{"intent",
                              &put_many_yield_curve_process_parameter_definitions_request::intent},
    };
}

/**
 * @brief The members of put_many_yield_curve_process_parameter_definitions_response, named as the
 * wire names them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<put_many_yield_curve_process_parameter_definitions_response>) {
    return std::tuple{
        utility::codec::field{"result",
                              &put_many_yield_curve_process_parameter_definitions_response::result},
        utility::codec::field{
            "parameter_definitions",
            &put_many_yield_curve_process_parameter_definitions_response::parameter_definitions},
    };
}

/**
 * @brief The members of delete_yield_curve_process_parameter_definition_request, named as the wire
 * names them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<delete_yield_curve_process_parameter_definition_request>) {
    return std::tuple{
        utility::codec::field{"removal",
                              &delete_yield_curve_process_parameter_definition_request::removal},
        utility::codec::field{"intent",
                              &delete_yield_curve_process_parameter_definition_request::intent},
    };
}

/**
 * @brief The members of delete_yield_curve_process_parameter_definition_response, named as the wire
 * names them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<delete_yield_curve_process_parameter_definition_response>) {
    return std::tuple{
        utility::codec::field{"result",
                              &delete_yield_curve_process_parameter_definition_response::result},
    };
}

/**
 * @brief The members of delete_many_yield_curve_process_parameter_definitions_request, named as the
 * wire names them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<delete_many_yield_curve_process_parameter_definitions_request>) {
    return std::tuple{
        utility::codec::field{
            "removals", &delete_many_yield_curve_process_parameter_definitions_request::removals},
        utility::codec::field{
            "intent", &delete_many_yield_curve_process_parameter_definitions_request::intent},
    };
}

/**
 * @brief The members of delete_many_yield_curve_process_parameter_definitions_response, named as
 * the wire names them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<delete_many_yield_curve_process_parameter_definitions_response>) {
    return std::tuple{
        utility::codec::field{
            "result", &delete_many_yield_curve_process_parameter_definitions_response::result},
    };
}

/**
 * @brief The members of list_yield_curve_process_parameter_definition_versions_request, named as
 * the wire names them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<list_yield_curve_process_parameter_definition_versions_request>) {
    return std::tuple{
        utility::codec::field{"key",
                              &list_yield_curve_process_parameter_definition_versions_request::key},
        utility::codec::field{
            "offset", &list_yield_curve_process_parameter_definition_versions_request::offset},
        utility::codec::field{
            "limit", &list_yield_curve_process_parameter_definition_versions_request::limit},
        utility::codec::field{
            "order", &list_yield_curve_process_parameter_definition_versions_request::order},
        utility::codec::field{
            "filter", &list_yield_curve_process_parameter_definition_versions_request::filter},
    };
}

/**
 * @brief The members of list_yield_curve_process_parameter_definition_versions_response, named as
 * the wire names them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<list_yield_curve_process_parameter_definition_versions_response>) {
    return std::tuple{
        utility::codec::field{
            "result", &list_yield_curve_process_parameter_definition_versions_response::result},
        utility::codec::field{
            "versions", &list_yield_curve_process_parameter_definition_versions_response::versions},
        utility::codec::field{
            "total", &list_yield_curve_process_parameter_definition_versions_response::total},
    };
}

/**
 * @brief The members of get_yield_curve_process_parameter_definition_version_request, named as the
 * wire names them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<get_yield_curve_process_parameter_definition_version_request>) {
    return std::tuple{
        utility::codec::field{"key",
                              &get_yield_curve_process_parameter_definition_version_request::key},
    };
}

/**
 * @brief The members of get_yield_curve_process_parameter_definition_version_response, named as the
 * wire names them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<get_yield_curve_process_parameter_definition_version_response>) {
    return std::tuple{
        utility::codec::field{
            "result", &get_yield_curve_process_parameter_definition_version_response::result},
        utility::codec::field{
            "version", &get_yield_curve_process_parameter_definition_version_response::version},
    };
}


}
namespace ores::utility::codec {

template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::yield_curve_process_parameter_definition_key> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::synthetic::messaging::yield_curve_process_parameter_definition_key& v);
    static ores::synthetic::messaging::yield_curve_process_parameter_definition_key
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::synthetic::messaging::yield_curve_process_parameter_definition_key& v);
    static ores::synthetic::messaging::yield_curve_process_parameter_definition_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::yield_curve_process_parameter_definition_write> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::synthetic::messaging::yield_curve_process_parameter_definition_write& v);
    static ores::synthetic::messaging::yield_curve_process_parameter_definition_write
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::synthetic::messaging::yield_curve_process_parameter_definition_write& v);
    static ores::synthetic::messaging::yield_curve_process_parameter_definition_write
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::yield_curve_process_parameter_definition_change> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::synthetic::messaging::yield_curve_process_parameter_definition_change& v);
    static ores::synthetic::messaging::yield_curve_process_parameter_definition_change
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::synthetic::messaging::yield_curve_process_parameter_definition_change& v);
    static ores::synthetic::messaging::yield_curve_process_parameter_definition_change
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::yield_curve_process_parameter_definition_removal> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::synthetic::messaging::yield_curve_process_parameter_definition_removal& v);
    static ores::synthetic::messaging::yield_curve_process_parameter_definition_removal
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::synthetic::messaging::yield_curve_process_parameter_definition_removal& v);
    static ores::synthetic::messaging::yield_curve_process_parameter_definition_removal
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::yield_curve_process_parameter_definition_lookup> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::synthetic::messaging::yield_curve_process_parameter_definition_lookup& v);
    static ores::synthetic::messaging::yield_curve_process_parameter_definition_lookup
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::synthetic::messaging::yield_curve_process_parameter_definition_lookup& v);
    static ores::synthetic::messaging::yield_curve_process_parameter_definition_lookup
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::yield_curve_process_parameter_definitions_filter> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::synthetic::messaging::yield_curve_process_parameter_definitions_filter& v);
    static ores::synthetic::messaging::yield_curve_process_parameter_definitions_filter
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::synthetic::messaging::yield_curve_process_parameter_definitions_filter& v);
    static ores::synthetic::messaging::yield_curve_process_parameter_definitions_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::yield_curve_process_parameter_definition_event> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::synthetic::messaging::yield_curve_process_parameter_definition_event& v);
    static ores::synthetic::messaging::yield_curve_process_parameter_definition_event
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::synthetic::messaging::yield_curve_process_parameter_definition_event& v);
    static ores::synthetic::messaging::yield_curve_process_parameter_definition_event
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::yield_curve_process_parameter_definition_version_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::synthetic::messaging::yield_curve_process_parameter_definition_version_key& v);
    static ores::synthetic::messaging::yield_curve_process_parameter_definition_version_key
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::synthetic::messaging::yield_curve_process_parameter_definition_version_key& v);
    static ores::synthetic::messaging::yield_curve_process_parameter_definition_version_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT wire_traits<
    ores::synthetic::messaging::yield_curve_process_parameter_definition_versions_filter> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::synthetic::messaging::yield_curve_process_parameter_definition_versions_filter&
            v);
    static ores::synthetic::messaging::yield_curve_process_parameter_definition_versions_filter
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::synthetic::messaging::yield_curve_process_parameter_definition_versions_filter&
            v);
    static ores::synthetic::messaging::yield_curve_process_parameter_definition_versions_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT wire_traits<
    ores::synthetic::messaging::list_yield_curve_process_parameter_definitions_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::synthetic::messaging::list_yield_curve_process_parameter_definitions_request&
            v);
    static ores::synthetic::messaging::list_yield_curve_process_parameter_definitions_request
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::synthetic::messaging::list_yield_curve_process_parameter_definitions_request&
            v);
    static ores::synthetic::messaging::list_yield_curve_process_parameter_definitions_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT wire_traits<
    ores::synthetic::messaging::list_yield_curve_process_parameter_definitions_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::synthetic::messaging::list_yield_curve_process_parameter_definitions_response&
            v);
    static ores::synthetic::messaging::list_yield_curve_process_parameter_definitions_response
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::synthetic::messaging::list_yield_curve_process_parameter_definitions_response&
            v);
    static ores::synthetic::messaging::list_yield_curve_process_parameter_definitions_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::get_yield_curve_process_parameter_definition_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::synthetic::messaging::get_yield_curve_process_parameter_definition_request& v);
    static ores::synthetic::messaging::get_yield_curve_process_parameter_definition_request
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::synthetic::messaging::get_yield_curve_process_parameter_definition_request& v);
    static ores::synthetic::messaging::get_yield_curve_process_parameter_definition_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::get_yield_curve_process_parameter_definition_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::synthetic::messaging::get_yield_curve_process_parameter_definition_response& v);
    static ores::synthetic::messaging::get_yield_curve_process_parameter_definition_response
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::synthetic::messaging::get_yield_curve_process_parameter_definition_response& v);
    static ores::synthetic::messaging::get_yield_curve_process_parameter_definition_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT wire_traits<
    ores::synthetic::messaging::get_many_yield_curve_process_parameter_definitions_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::synthetic::messaging::
                   get_many_yield_curve_process_parameter_definitions_request& v);
    static ores::synthetic::messaging::get_many_yield_curve_process_parameter_definitions_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::synthetic::messaging::
                      get_many_yield_curve_process_parameter_definitions_request& v);
    static ores::synthetic::messaging::get_many_yield_curve_process_parameter_definitions_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT wire_traits<
    ores::synthetic::messaging::get_many_yield_curve_process_parameter_definitions_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::synthetic::messaging::
                   get_many_yield_curve_process_parameter_definitions_response& v);
    static ores::synthetic::messaging::get_many_yield_curve_process_parameter_definitions_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::synthetic::messaging::
                      get_many_yield_curve_process_parameter_definitions_response& v);
    static ores::synthetic::messaging::get_many_yield_curve_process_parameter_definitions_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::put_yield_curve_process_parameter_definition_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::synthetic::messaging::put_yield_curve_process_parameter_definition_request& v);
    static ores::synthetic::messaging::put_yield_curve_process_parameter_definition_request
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::synthetic::messaging::put_yield_curve_process_parameter_definition_request& v);
    static ores::synthetic::messaging::put_yield_curve_process_parameter_definition_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::put_yield_curve_process_parameter_definition_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::synthetic::messaging::put_yield_curve_process_parameter_definition_response& v);
    static ores::synthetic::messaging::put_yield_curve_process_parameter_definition_response
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::synthetic::messaging::put_yield_curve_process_parameter_definition_response& v);
    static ores::synthetic::messaging::put_yield_curve_process_parameter_definition_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT wire_traits<
    ores::synthetic::messaging::put_many_yield_curve_process_parameter_definitions_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::synthetic::messaging::
                   put_many_yield_curve_process_parameter_definitions_request& v);
    static ores::synthetic::messaging::put_many_yield_curve_process_parameter_definitions_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::synthetic::messaging::
                      put_many_yield_curve_process_parameter_definitions_request& v);
    static ores::synthetic::messaging::put_many_yield_curve_process_parameter_definitions_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT wire_traits<
    ores::synthetic::messaging::put_many_yield_curve_process_parameter_definitions_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::synthetic::messaging::
                   put_many_yield_curve_process_parameter_definitions_response& v);
    static ores::synthetic::messaging::put_many_yield_curve_process_parameter_definitions_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::synthetic::messaging::
                      put_many_yield_curve_process_parameter_definitions_response& v);
    static ores::synthetic::messaging::put_many_yield_curve_process_parameter_definitions_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT wire_traits<
    ores::synthetic::messaging::delete_yield_curve_process_parameter_definition_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::synthetic::messaging::delete_yield_curve_process_parameter_definition_request&
            v);
    static ores::synthetic::messaging::delete_yield_curve_process_parameter_definition_request
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::synthetic::messaging::delete_yield_curve_process_parameter_definition_request&
            v);
    static ores::synthetic::messaging::delete_yield_curve_process_parameter_definition_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT wire_traits<
    ores::synthetic::messaging::delete_yield_curve_process_parameter_definition_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::synthetic::messaging::delete_yield_curve_process_parameter_definition_response&
            v);
    static ores::synthetic::messaging::delete_yield_curve_process_parameter_definition_response
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::synthetic::messaging::delete_yield_curve_process_parameter_definition_response&
            v);
    static ores::synthetic::messaging::delete_yield_curve_process_parameter_definition_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT wire_traits<
    ores::synthetic::messaging::delete_many_yield_curve_process_parameter_definitions_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::synthetic::messaging::
                   delete_many_yield_curve_process_parameter_definitions_request& v);
    static ores::synthetic::messaging::delete_many_yield_curve_process_parameter_definitions_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::synthetic::messaging::
                      delete_many_yield_curve_process_parameter_definitions_request& v);
    static ores::synthetic::messaging::delete_many_yield_curve_process_parameter_definitions_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT wire_traits<
    ores::synthetic::messaging::delete_many_yield_curve_process_parameter_definitions_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::synthetic::messaging::
                   delete_many_yield_curve_process_parameter_definitions_response& v);
    static ores::synthetic::messaging::
        delete_many_yield_curve_process_parameter_definitions_response
        read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::synthetic::messaging::
                      delete_many_yield_curve_process_parameter_definitions_response& v);
    static ores::synthetic::messaging::
        delete_many_yield_curve_process_parameter_definitions_response
        read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT wire_traits<
    ores::synthetic::messaging::list_yield_curve_process_parameter_definition_versions_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::synthetic::messaging::
                   list_yield_curve_process_parameter_definition_versions_request& v);
    static ores::synthetic::messaging::
        list_yield_curve_process_parameter_definition_versions_request
        read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::synthetic::messaging::
                      list_yield_curve_process_parameter_definition_versions_request& v);
    static ores::synthetic::messaging::
        list_yield_curve_process_parameter_definition_versions_request
        read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT wire_traits<
    ores::synthetic::messaging::list_yield_curve_process_parameter_definition_versions_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::synthetic::messaging::
                   list_yield_curve_process_parameter_definition_versions_response& v);
    static ores::synthetic::messaging::
        list_yield_curve_process_parameter_definition_versions_response
        read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::synthetic::messaging::
                      list_yield_curve_process_parameter_definition_versions_response& v);
    static ores::synthetic::messaging::
        list_yield_curve_process_parameter_definition_versions_response
        read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT wire_traits<
    ores::synthetic::messaging::get_yield_curve_process_parameter_definition_version_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::synthetic::messaging::
                   get_yield_curve_process_parameter_definition_version_request& v);
    static ores::synthetic::messaging::get_yield_curve_process_parameter_definition_version_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::synthetic::messaging::
                      get_yield_curve_process_parameter_definition_version_request& v);
    static ores::synthetic::messaging::get_yield_curve_process_parameter_definition_version_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT wire_traits<
    ores::synthetic::messaging::get_yield_curve_process_parameter_definition_version_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::synthetic::messaging::
                   get_yield_curve_process_parameter_definition_version_response& v);
    static ores::synthetic::messaging::get_yield_curve_process_parameter_definition_version_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::synthetic::messaging::
                      get_yield_curve_process_parameter_definition_version_response& v);
    static ores::synthetic::messaging::get_yield_curve_process_parameter_definition_version_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
