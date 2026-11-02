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
#ifndef ORES_REPORTING_API_MESSAGING_CONFIGURATION_MARKET_BINDING_PROTOCOL_HPP
#define ORES_REPORTING_API_MESSAGING_CONFIGURATION_MARKET_BINDING_PROTOCOL_HPP

#include "ores.reporting.api/domain/configuration_market_binding.hpp"
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

struct configuration_market_binding_key {
    boost::uuids::uuid id;
};

struct configuration_market_binding_write {
    boost::uuids::uuid id;
    boost::uuids::uuid configuration_id;
    std::string role;
    std::string market_configuration_name;
    int position;
};

struct configuration_market_binding_change {
    configuration_market_binding_write write;
    ores::utility::domain::precondition precondition;
};

struct configuration_market_binding_removal {
    configuration_market_binding_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;
};

struct configuration_market_binding_lookup {
    configuration_market_binding_key key;
    std::optional<ores::reporting::domain::configuration_market_binding>
        configuration_market_binding;
};

struct configuration_market_bindings_filter {
    std::optional<boost::uuids::uuid> configuration_id;
    std::optional<std::vector<boost::uuids::uuid>> id_one_of;
    std::optional<std::vector<boost::uuids::uuid>> configuration_id_one_of;
};

struct configuration_market_binding_event {
    boost::uuids::uuid event_id;
    configuration_market_binding_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;
};

struct configuration_market_binding_version_key {
    configuration_market_binding_key configuration_market_binding;
    std::uint32_t version;
};

struct configuration_market_binding_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;
};

struct list_configuration_market_bindings_request {
    using response_type = struct list_configuration_market_bindings_response;
    static constexpr std::string_view nats_subject =
        "reporting.v1.configuration_market_bindings.list";
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
    std::optional<configuration_market_bindings_filter> filter;
    std::optional<std::string> as_of;
};

struct list_configuration_market_bindings_response {
    ores::utility::domain::result result;
    std::vector<ores::reporting::domain::configuration_market_binding> bindings;
    std::uint64_t total;
};

struct get_configuration_market_binding_request {
    using response_type = struct get_configuration_market_binding_response;
    static constexpr std::string_view nats_subject =
        "reporting.v1.configuration_market_bindings.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    configuration_market_binding_key key;
};

struct get_configuration_market_binding_response {
    ores::utility::domain::result result;
    std::optional<ores::reporting::domain::configuration_market_binding>
        configuration_market_binding;
};

struct get_many_configuration_market_bindings_request {
    using response_type = struct get_many_configuration_market_bindings_response;
    static constexpr std::string_view nats_subject =
        "reporting.v1.configuration_market_bindings.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<configuration_market_binding_key> keys;
};

struct get_many_configuration_market_bindings_response {
    ores::utility::domain::result result;
    std::vector<configuration_market_binding_lookup> entries;
};

struct put_configuration_market_binding_request {
    using response_type = struct put_configuration_market_binding_response;
    static constexpr std::string_view nats_subject =
        "reporting.v1.configuration_market_bindings.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    configuration_market_binding_change change;
    ores::utility::domain::change_intent intent;
};

struct put_configuration_market_binding_response {
    ores::utility::domain::result result;
    std::optional<ores::reporting::domain::configuration_market_binding>
        configuration_market_binding;
};

struct put_many_configuration_market_bindings_request {
    using response_type = struct put_many_configuration_market_bindings_response;
    static constexpr std::string_view nats_subject =
        "reporting.v1.configuration_market_bindings.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<configuration_market_binding_change> changes;
    ores::utility::domain::change_intent intent;
};

struct put_many_configuration_market_bindings_response {
    ores::utility::domain::result result;
    std::vector<ores::reporting::domain::configuration_market_binding> bindings;
};

struct delete_configuration_market_binding_request {
    using response_type = struct delete_configuration_market_binding_response;
    static constexpr std::string_view nats_subject =
        "reporting.v1.configuration_market_bindings.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    configuration_market_binding_removal removal;
    ores::utility::domain::change_intent intent;
};

struct delete_configuration_market_binding_response {
    ores::utility::domain::result result;
};

struct delete_many_configuration_market_bindings_request {
    using response_type = struct delete_many_configuration_market_bindings_response;
    static constexpr std::string_view nats_subject =
        "reporting.v1.configuration_market_bindings.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<configuration_market_binding_removal> removals;
    ores::utility::domain::change_intent intent;
};

struct delete_many_configuration_market_bindings_response {
    ores::utility::domain::result result;
};

struct list_by_configuration_id_configuration_market_bindings_request {
    using response_type = struct list_by_configuration_id_configuration_market_bindings_response;
    static constexpr std::string_view nats_subject =
        "reporting.v1.configuration_market_bindings.list_by_configuration_id";
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
    std::optional<configuration_market_bindings_filter> filter;
};

struct list_by_configuration_id_configuration_market_bindings_response {
    ores::utility::domain::result result;
    std::vector<ores::reporting::domain::configuration_market_binding> bindings;
    std::uint64_t total;
};

struct list_configuration_market_binding_versions_request {
    using response_type = struct list_configuration_market_binding_versions_response;
    static constexpr std::string_view nats_subject =
        "reporting.v1.configuration_market_bindings_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    configuration_market_binding_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<configuration_market_binding_versions_filter> filter;
};

struct list_configuration_market_binding_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::reporting::domain::configuration_market_binding> versions;
    std::uint64_t total;
};

struct get_configuration_market_binding_version_request {
    using response_type = struct get_configuration_market_binding_version_response;
    static constexpr std::string_view nats_subject =
        "reporting.v1.configuration_market_bindings_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    configuration_market_binding_version_key key;
};

struct get_configuration_market_binding_version_response {
    ores::utility::domain::result result;
    std::optional<ores::reporting::domain::configuration_market_binding> version;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace configuration_market_binding_event_subjects {
inline constexpr std::string_view created =
    "reporting.v1.configuration_market_bindings_events.created";
inline constexpr std::string_view updated =
    "reporting.v1.configuration_market_bindings_events.updated";
inline constexpr std::string_view deleted =
    "reporting.v1.configuration_market_bindings_events.deleted";
}
/**
 * @brief The members of configuration_market_binding_key, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const configuration_market_binding_key&) {
    return std::tuple{
        utility::codec::field{"id", &configuration_market_binding_key::id},
    };
}

/**
 * @brief The members of configuration_market_binding_write, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const configuration_market_binding_write&) {
    return std::tuple{
        utility::codec::field{"id", &configuration_market_binding_write::id},
        utility::codec::field{"configuration_id",
                              &configuration_market_binding_write::configuration_id},
        utility::codec::field{"role", &configuration_market_binding_write::role},
        utility::codec::field{"market_configuration_name",
                              &configuration_market_binding_write::market_configuration_name},
        utility::codec::field{"position", &configuration_market_binding_write::position},
    };
}

/**
 * @brief The members of configuration_market_binding_change, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const configuration_market_binding_change&) {
    return std::tuple{
        utility::codec::field{"write", &configuration_market_binding_change::write},
        utility::codec::field{"precondition", &configuration_market_binding_change::precondition},
    };
}

/**
 * @brief The members of configuration_market_binding_removal, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const configuration_market_binding_removal&) {
    return std::tuple{
        utility::codec::field{"key", &configuration_market_binding_removal::key},
        utility::codec::field{"precondition", &configuration_market_binding_removal::precondition},
    };
}

/**
 * @brief The members of configuration_market_binding_lookup, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const configuration_market_binding_lookup&) {
    return std::tuple{
        utility::codec::field{"key", &configuration_market_binding_lookup::key},
        utility::codec::field{"configuration_market_binding",
                              &configuration_market_binding_lookup::configuration_market_binding},
    };
}

/**
 * @brief The members of configuration_market_bindings_filter, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const configuration_market_bindings_filter&) {
    return std::tuple{
        utility::codec::field{"configuration_id",
                              &configuration_market_bindings_filter::configuration_id},
        utility::codec::field{"id_one_of", &configuration_market_bindings_filter::id_one_of},
        utility::codec::field{"configuration_id_one_of",
                              &configuration_market_bindings_filter::configuration_id_one_of},
    };
}

/**
 * @brief The members of configuration_market_binding_event, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const configuration_market_binding_event&) {
    return std::tuple{
        utility::codec::field{"event_id", &configuration_market_binding_event::event_id},
        utility::codec::field{"key", &configuration_market_binding_event::key},
        utility::codec::field{"action", &configuration_market_binding_event::action},
        utility::codec::field{"version", &configuration_market_binding_event::version},
        utility::codec::field{"occurred_at", &configuration_market_binding_event::occurred_at},
        utility::codec::field{"correlation_id",
                              &configuration_market_binding_event::correlation_id},
    };
}

/**
 * @brief The members of configuration_market_binding_version_key, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const configuration_market_binding_version_key&) {
    return std::tuple{
        utility::codec::field{
            "configuration_market_binding",
            &configuration_market_binding_version_key::configuration_market_binding},
        utility::codec::field{"version", &configuration_market_binding_version_key::version},
    };
}

/**
 * @brief The members of configuration_market_binding_versions_filter, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const configuration_market_binding_versions_filter&) {
    return std::tuple{
        utility::codec::field{"version", &configuration_market_binding_versions_filter::version},
        utility::codec::field{"from_version",
                              &configuration_market_binding_versions_filter::from_version},
        utility::codec::field{"to_version",
                              &configuration_market_binding_versions_filter::to_version},
    };
}

/**
 * @brief The members of list_configuration_market_bindings_request, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_configuration_market_bindings_request&) {
    return std::tuple{
        utility::codec::field{"offset", &list_configuration_market_bindings_request::offset},
        utility::codec::field{"limit", &list_configuration_market_bindings_request::limit},
        utility::codec::field{"order", &list_configuration_market_bindings_request::order},
        utility::codec::field{"filter", &list_configuration_market_bindings_request::filter},
        utility::codec::field{"as_of", &list_configuration_market_bindings_request::as_of},
    };
}

/**
 * @brief The members of list_configuration_market_bindings_response, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_configuration_market_bindings_response&) {
    return std::tuple{
        utility::codec::field{"result", &list_configuration_market_bindings_response::result},
        utility::codec::field{"bindings", &list_configuration_market_bindings_response::bindings},
        utility::codec::field{"total", &list_configuration_market_bindings_response::total},
    };
}

/**
 * @brief The members of get_configuration_market_binding_request, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_configuration_market_binding_request&) {
    return std::tuple{
        utility::codec::field{"key", &get_configuration_market_binding_request::key},
    };
}

/**
 * @brief The members of get_configuration_market_binding_response, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_configuration_market_binding_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_configuration_market_binding_response::result},
        utility::codec::field{
            "configuration_market_binding",
            &get_configuration_market_binding_response::configuration_market_binding},
    };
}

/**
 * @brief The members of get_many_configuration_market_bindings_request, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_many_configuration_market_bindings_request&) {
    return std::tuple{
        utility::codec::field{"keys", &get_many_configuration_market_bindings_request::keys},
    };
}

/**
 * @brief The members of get_many_configuration_market_bindings_response, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_many_configuration_market_bindings_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_many_configuration_market_bindings_response::result},
        utility::codec::field{"entries", &get_many_configuration_market_bindings_response::entries},
    };
}

/**
 * @brief The members of put_configuration_market_binding_request, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_configuration_market_binding_request&) {
    return std::tuple{
        utility::codec::field{"change", &put_configuration_market_binding_request::change},
        utility::codec::field{"intent", &put_configuration_market_binding_request::intent},
    };
}

/**
 * @brief The members of put_configuration_market_binding_response, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_configuration_market_binding_response&) {
    return std::tuple{
        utility::codec::field{"result", &put_configuration_market_binding_response::result},
        utility::codec::field{
            "configuration_market_binding",
            &put_configuration_market_binding_response::configuration_market_binding},
    };
}

/**
 * @brief The members of put_many_configuration_market_bindings_request, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_many_configuration_market_bindings_request&) {
    return std::tuple{
        utility::codec::field{"changes", &put_many_configuration_market_bindings_request::changes},
        utility::codec::field{"intent", &put_many_configuration_market_bindings_request::intent},
    };
}

/**
 * @brief The members of put_many_configuration_market_bindings_response, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_many_configuration_market_bindings_response&) {
    return std::tuple{
        utility::codec::field{"result", &put_many_configuration_market_bindings_response::result},
        utility::codec::field{"bindings",
                              &put_many_configuration_market_bindings_response::bindings},
    };
}

/**
 * @brief The members of delete_configuration_market_binding_request, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_configuration_market_binding_request&) {
    return std::tuple{
        utility::codec::field{"removal", &delete_configuration_market_binding_request::removal},
        utility::codec::field{"intent", &delete_configuration_market_binding_request::intent},
    };
}

/**
 * @brief The members of delete_configuration_market_binding_response, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_configuration_market_binding_response&) {
    return std::tuple{
        utility::codec::field{"result", &delete_configuration_market_binding_response::result},
    };
}

/**
 * @brief The members of delete_many_configuration_market_bindings_request, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_many_configuration_market_bindings_request&) {
    return std::tuple{
        utility::codec::field{"removals",
                              &delete_many_configuration_market_bindings_request::removals},
        utility::codec::field{"intent", &delete_many_configuration_market_bindings_request::intent},
    };
}

/**
 * @brief The members of delete_many_configuration_market_bindings_response, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(const delete_many_configuration_market_bindings_response&) {
    return std::tuple{
        utility::codec::field{"result",
                              &delete_many_configuration_market_bindings_response::result},
    };
}

/**
 * @brief The members of list_by_configuration_id_configuration_market_bindings_request, named as
 * the wire names them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(const list_by_configuration_id_configuration_market_bindings_request&) {
    return std::tuple{
        utility::codec::field{
            "configuration_id",
            &list_by_configuration_id_configuration_market_bindings_request::configuration_id},
        utility::codec::field{
            "scope", &list_by_configuration_id_configuration_market_bindings_request::scope},
        utility::codec::field{
            "offset", &list_by_configuration_id_configuration_market_bindings_request::offset},
        utility::codec::field{
            "limit", &list_by_configuration_id_configuration_market_bindings_request::limit},
        utility::codec::field{
            "order", &list_by_configuration_id_configuration_market_bindings_request::order},
        utility::codec::field{
            "filter", &list_by_configuration_id_configuration_market_bindings_request::filter},
    };
}

/**
 * @brief The members of list_by_configuration_id_configuration_market_bindings_response, named as
 * the wire names them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(const list_by_configuration_id_configuration_market_bindings_response&) {
    return std::tuple{
        utility::codec::field{
            "result", &list_by_configuration_id_configuration_market_bindings_response::result},
        utility::codec::field{
            "bindings", &list_by_configuration_id_configuration_market_bindings_response::bindings},
        utility::codec::field{
            "total", &list_by_configuration_id_configuration_market_bindings_response::total},
    };
}

/**
 * @brief The members of list_configuration_market_binding_versions_request, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(const list_configuration_market_binding_versions_request&) {
    return std::tuple{
        utility::codec::field{"key", &list_configuration_market_binding_versions_request::key},
        utility::codec::field{"offset",
                              &list_configuration_market_binding_versions_request::offset},
        utility::codec::field{"limit", &list_configuration_market_binding_versions_request::limit},
        utility::codec::field{"order", &list_configuration_market_binding_versions_request::order},
        utility::codec::field{"filter",
                              &list_configuration_market_binding_versions_request::filter},
    };
}

/**
 * @brief The members of list_configuration_market_binding_versions_response, named as the wire
 * names them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(const list_configuration_market_binding_versions_response&) {
    return std::tuple{
        utility::codec::field{"result",
                              &list_configuration_market_binding_versions_response::result},
        utility::codec::field{"versions",
                              &list_configuration_market_binding_versions_response::versions},
        utility::codec::field{"total", &list_configuration_market_binding_versions_response::total},
    };
}

/**
 * @brief The members of get_configuration_market_binding_version_request, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_configuration_market_binding_version_request&) {
    return std::tuple{
        utility::codec::field{"key", &get_configuration_market_binding_version_request::key},
    };
}

/**
 * @brief The members of get_configuration_market_binding_version_response, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_configuration_market_binding_version_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_configuration_market_binding_version_response::result},
        utility::codec::field{"version",
                              &get_configuration_market_binding_version_response::version},
    };
}


}
namespace ores::utility::codec {

template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::configuration_market_binding_key> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::configuration_market_binding_key& v);
    static ores::reporting::messaging::configuration_market_binding_key
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::configuration_market_binding_key& v);
    static ores::reporting::messaging::configuration_market_binding_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::configuration_market_binding_write> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::configuration_market_binding_write& v);
    static ores::reporting::messaging::configuration_market_binding_write
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::configuration_market_binding_write& v);
    static ores::reporting::messaging::configuration_market_binding_write
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::configuration_market_binding_change> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::configuration_market_binding_change& v);
    static ores::reporting::messaging::configuration_market_binding_change
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::configuration_market_binding_change& v);
    static ores::reporting::messaging::configuration_market_binding_change
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::configuration_market_binding_removal> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::configuration_market_binding_removal& v);
    static ores::reporting::messaging::configuration_market_binding_removal
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::configuration_market_binding_removal& v);
    static ores::reporting::messaging::configuration_market_binding_removal
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::configuration_market_binding_lookup> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::configuration_market_binding_lookup& v);
    static ores::reporting::messaging::configuration_market_binding_lookup
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::configuration_market_binding_lookup& v);
    static ores::reporting::messaging::configuration_market_binding_lookup
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::configuration_market_bindings_filter> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::configuration_market_bindings_filter& v);
    static ores::reporting::messaging::configuration_market_bindings_filter
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::configuration_market_bindings_filter& v);
    static ores::reporting::messaging::configuration_market_bindings_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::configuration_market_binding_event> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::configuration_market_binding_event& v);
    static ores::reporting::messaging::configuration_market_binding_event
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::configuration_market_binding_event& v);
    static ores::reporting::messaging::configuration_market_binding_event
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::configuration_market_binding_version_key> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::configuration_market_binding_version_key& v);
    static ores::reporting::messaging::configuration_market_binding_version_key
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::configuration_market_binding_version_key& v);
    static ores::reporting::messaging::configuration_market_binding_version_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::configuration_market_binding_versions_filter> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::configuration_market_binding_versions_filter& v);
    static ores::reporting::messaging::configuration_market_binding_versions_filter
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::reporting::messaging::configuration_market_binding_versions_filter& v);
    static ores::reporting::messaging::configuration_market_binding_versions_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::list_configuration_market_bindings_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::list_configuration_market_bindings_request& v);
    static ores::reporting::messaging::list_configuration_market_bindings_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::list_configuration_market_bindings_request& v);
    static ores::reporting::messaging::list_configuration_market_bindings_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::list_configuration_market_bindings_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::list_configuration_market_bindings_response& v);
    static ores::reporting::messaging::list_configuration_market_bindings_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::list_configuration_market_bindings_response& v);
    static ores::reporting::messaging::list_configuration_market_bindings_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::get_configuration_market_binding_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::get_configuration_market_binding_request& v);
    static ores::reporting::messaging::get_configuration_market_binding_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::get_configuration_market_binding_request& v);
    static ores::reporting::messaging::get_configuration_market_binding_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::get_configuration_market_binding_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::get_configuration_market_binding_response& v);
    static ores::reporting::messaging::get_configuration_market_binding_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::get_configuration_market_binding_response& v);
    static ores::reporting::messaging::get_configuration_market_binding_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::get_many_configuration_market_bindings_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::get_many_configuration_market_bindings_request& v);
    static ores::reporting::messaging::get_many_configuration_market_bindings_request
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::reporting::messaging::get_many_configuration_market_bindings_request& v);
    static ores::reporting::messaging::get_many_configuration_market_bindings_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::get_many_configuration_market_bindings_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::reporting::messaging::get_many_configuration_market_bindings_response& v);
    static ores::reporting::messaging::get_many_configuration_market_bindings_response
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::reporting::messaging::get_many_configuration_market_bindings_response& v);
    static ores::reporting::messaging::get_many_configuration_market_bindings_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::put_configuration_market_binding_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::put_configuration_market_binding_request& v);
    static ores::reporting::messaging::put_configuration_market_binding_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::put_configuration_market_binding_request& v);
    static ores::reporting::messaging::put_configuration_market_binding_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::put_configuration_market_binding_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::put_configuration_market_binding_response& v);
    static ores::reporting::messaging::put_configuration_market_binding_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::put_configuration_market_binding_response& v);
    static ores::reporting::messaging::put_configuration_market_binding_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::put_many_configuration_market_bindings_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::put_many_configuration_market_bindings_request& v);
    static ores::reporting::messaging::put_many_configuration_market_bindings_request
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::reporting::messaging::put_many_configuration_market_bindings_request& v);
    static ores::reporting::messaging::put_many_configuration_market_bindings_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::put_many_configuration_market_bindings_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::reporting::messaging::put_many_configuration_market_bindings_response& v);
    static ores::reporting::messaging::put_many_configuration_market_bindings_response
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::reporting::messaging::put_many_configuration_market_bindings_response& v);
    static ores::reporting::messaging::put_many_configuration_market_bindings_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::delete_configuration_market_binding_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::delete_configuration_market_binding_request& v);
    static ores::reporting::messaging::delete_configuration_market_binding_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::delete_configuration_market_binding_request& v);
    static ores::reporting::messaging::delete_configuration_market_binding_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::delete_configuration_market_binding_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::delete_configuration_market_binding_response& v);
    static ores::reporting::messaging::delete_configuration_market_binding_response
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::reporting::messaging::delete_configuration_market_binding_response& v);
    static ores::reporting::messaging::delete_configuration_market_binding_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::delete_many_configuration_market_bindings_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::reporting::messaging::delete_many_configuration_market_bindings_request& v);
    static ores::reporting::messaging::delete_many_configuration_market_bindings_request
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::reporting::messaging::delete_many_configuration_market_bindings_request& v);
    static ores::reporting::messaging::delete_many_configuration_market_bindings_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::delete_many_configuration_market_bindings_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::reporting::messaging::delete_many_configuration_market_bindings_response& v);
    static ores::reporting::messaging::delete_many_configuration_market_bindings_response
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::reporting::messaging::delete_many_configuration_market_bindings_response& v);
    static ores::reporting::messaging::delete_many_configuration_market_bindings_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT wire_traits<
    ores::reporting::messaging::list_by_configuration_id_configuration_market_bindings_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::
                   list_by_configuration_id_configuration_market_bindings_request& v);
    static ores::reporting::messaging::
        list_by_configuration_id_configuration_market_bindings_request
        read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::
                      list_by_configuration_id_configuration_market_bindings_request& v);
    static ores::reporting::messaging::
        list_by_configuration_id_configuration_market_bindings_request
        read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT wire_traits<
    ores::reporting::messaging::list_by_configuration_id_configuration_market_bindings_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::
                   list_by_configuration_id_configuration_market_bindings_response& v);
    static ores::reporting::messaging::
        list_by_configuration_id_configuration_market_bindings_response
        read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::
                      list_by_configuration_id_configuration_market_bindings_response& v);
    static ores::reporting::messaging::
        list_by_configuration_id_configuration_market_bindings_response
        read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::list_configuration_market_binding_versions_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::reporting::messaging::list_configuration_market_binding_versions_request& v);
    static ores::reporting::messaging::list_configuration_market_binding_versions_request
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::reporting::messaging::list_configuration_market_binding_versions_request& v);
    static ores::reporting::messaging::list_configuration_market_binding_versions_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::list_configuration_market_binding_versions_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::reporting::messaging::list_configuration_market_binding_versions_response& v);
    static ores::reporting::messaging::list_configuration_market_binding_versions_response
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::reporting::messaging::list_configuration_market_binding_versions_response& v);
    static ores::reporting::messaging::list_configuration_market_binding_versions_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::get_configuration_market_binding_version_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::reporting::messaging::get_configuration_market_binding_version_request& v);
    static ores::reporting::messaging::get_configuration_market_binding_version_request
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::reporting::messaging::get_configuration_market_binding_version_request& v);
    static ores::reporting::messaging::get_configuration_market_binding_version_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::get_configuration_market_binding_version_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::reporting::messaging::get_configuration_market_binding_version_response& v);
    static ores::reporting::messaging::get_configuration_market_binding_version_response
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::reporting::messaging::get_configuration_market_binding_version_response& v);
    static ores::reporting::messaging::get_configuration_market_binding_version_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
