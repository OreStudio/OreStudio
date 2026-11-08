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
#ifndef ORES_TRADING_API_MESSAGING_STRUCTURE_IDENTIFIER_PROTOCOL_HPP
#define ORES_TRADING_API_MESSAGING_STRUCTURE_IDENTIFIER_PROTOCOL_HPP

#include "ores.trading.api/domain/structure_identifier_codec.hpp"
#include "ores.trading.api/export.hpp"
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

namespace ores::trading::messaging {

struct structure_identifier_key {
    boost::uuids::uuid structure_id;
    std::string id_type;

    friend bool operator==(const structure_identifier_key&,
                           const structure_identifier_key&) = default;
};

struct structure_identifier_write {
    boost::uuids::uuid structure_id;
    std::string id_type;
    std::string id_value;
    std::optional<boost::uuids::uuid> issuing_party_id;

    friend bool operator==(const structure_identifier_write&,
                           const structure_identifier_write&) = default;
};

struct structure_identifier_change {
    structure_identifier_write write;
    ores::utility::domain::precondition precondition;

    friend bool operator==(const structure_identifier_change&,
                           const structure_identifier_change&) = default;
};

struct structure_identifier_removal {
    structure_identifier_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;

    friend bool operator==(const structure_identifier_removal&,
                           const structure_identifier_removal&) = default;
};

struct structure_identifier_lookup {
    structure_identifier_key key;
    std::optional<ores::trading::domain::structure_identifier> structure_identifier;

    friend bool operator==(const structure_identifier_lookup&,
                           const structure_identifier_lookup&) = default;
};

struct structure_identifiers_filter {
    std::optional<boost::uuids::uuid> structure_id;
    std::optional<std::vector<boost::uuids::uuid>> structure_id_one_of;

    friend bool operator==(const structure_identifiers_filter&,
                           const structure_identifiers_filter&) = default;
};

struct structure_identifier_event {
    boost::uuids::uuid event_id;
    structure_identifier_key key;
    std::string action;
    std::uint32_t version{};
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;

    friend bool operator==(const structure_identifier_event&,
                           const structure_identifier_event&) = default;
};

struct structure_identifier_version_key {
    structure_identifier_key structure_identifier;
    std::uint32_t version{};

    friend bool operator==(const structure_identifier_version_key&,
                           const structure_identifier_version_key&) = default;
};

struct structure_identifier_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;

    friend bool operator==(const structure_identifier_versions_filter&,
                           const structure_identifier_versions_filter&) = default;
};

struct list_structure_identifiers_request {
    using response_type = struct list_structure_identifiers_response;
    static constexpr std::string_view nats_subject = "trading.v1.structure_identifiers.list";
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
    std::optional<structure_identifiers_filter> filter;
    std::optional<std::string> as_of;

    friend bool operator==(const list_structure_identifiers_request&,
                           const list_structure_identifiers_request&) = default;
};

struct list_structure_identifiers_response {
    ores::utility::domain::result result;
    std::vector<ores::trading::domain::structure_identifier> identifiers;
    std::uint64_t total{};

    friend bool operator==(const list_structure_identifiers_response&,
                           const list_structure_identifiers_response&) = default;
};

struct get_structure_identifier_request {
    using response_type = struct get_structure_identifier_response;
    static constexpr std::string_view nats_subject = "trading.v1.structure_identifiers.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    structure_identifier_key key;

    friend bool operator==(const get_structure_identifier_request&,
                           const get_structure_identifier_request&) = default;
};

struct get_structure_identifier_response {
    ores::utility::domain::result result;
    std::optional<ores::trading::domain::structure_identifier> structure_identifier;

    friend bool operator==(const get_structure_identifier_response&,
                           const get_structure_identifier_response&) = default;
};

struct get_many_structure_identifiers_request {
    using response_type = struct get_many_structure_identifiers_response;
    static constexpr std::string_view nats_subject = "trading.v1.structure_identifiers.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<structure_identifier_key> keys;

    friend bool operator==(const get_many_structure_identifiers_request&,
                           const get_many_structure_identifiers_request&) = default;
};

struct get_many_structure_identifiers_response {
    ores::utility::domain::result result;
    std::vector<structure_identifier_lookup> entries;

    friend bool operator==(const get_many_structure_identifiers_response&,
                           const get_many_structure_identifiers_response&) = default;
};

struct put_structure_identifier_request {
    using response_type = struct put_structure_identifier_response;
    static constexpr std::string_view nats_subject = "trading.v1.structure_identifiers.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    structure_identifier_change change;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const put_structure_identifier_request&,
                           const put_structure_identifier_request&) = default;
};

struct put_structure_identifier_response {
    ores::utility::domain::result result;
    std::optional<ores::trading::domain::structure_identifier> structure_identifier;

    friend bool operator==(const put_structure_identifier_response&,
                           const put_structure_identifier_response&) = default;
};

struct put_many_structure_identifiers_request {
    using response_type = struct put_many_structure_identifiers_response;
    static constexpr std::string_view nats_subject = "trading.v1.structure_identifiers.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<structure_identifier_change> changes;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const put_many_structure_identifiers_request&,
                           const put_many_structure_identifiers_request&) = default;
};

struct put_many_structure_identifiers_response {
    ores::utility::domain::result result;
    std::vector<ores::trading::domain::structure_identifier> identifiers;

    friend bool operator==(const put_many_structure_identifiers_response&,
                           const put_many_structure_identifiers_response&) = default;
};

struct delete_structure_identifier_request {
    using response_type = struct delete_structure_identifier_response;
    static constexpr std::string_view nats_subject = "trading.v1.structure_identifiers.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    structure_identifier_removal removal;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const delete_structure_identifier_request&,
                           const delete_structure_identifier_request&) = default;
};

struct delete_structure_identifier_response {
    ores::utility::domain::result result;

    friend bool operator==(const delete_structure_identifier_response&,
                           const delete_structure_identifier_response&) = default;
};

struct delete_many_structure_identifiers_request {
    using response_type = struct delete_many_structure_identifiers_response;
    static constexpr std::string_view nats_subject = "trading.v1.structure_identifiers.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<structure_identifier_removal> removals;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const delete_many_structure_identifiers_request&,
                           const delete_many_structure_identifiers_request&) = default;
};

struct delete_many_structure_identifiers_response {
    ores::utility::domain::result result;

    friend bool operator==(const delete_many_structure_identifiers_response&,
                           const delete_many_structure_identifiers_response&) = default;
};

struct list_by_structure_id_structure_identifiers_request {
    using response_type = struct list_by_structure_id_structure_identifiers_response;
    static constexpr std::string_view nats_subject =
        "trading.v1.structure_identifiers.list_by_structure_id";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    boost::uuids::uuid structure_id;
    ores::utility::domain::scope scope = ores::utility::domain::scope::direct;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<structure_identifiers_filter> filter;

    friend bool operator==(const list_by_structure_id_structure_identifiers_request&,
                           const list_by_structure_id_structure_identifiers_request&) = default;
};

struct list_by_structure_id_structure_identifiers_response {
    ores::utility::domain::result result;
    std::vector<ores::trading::domain::structure_identifier> identifiers;
    std::uint64_t total{};

    friend bool operator==(const list_by_structure_id_structure_identifiers_response&,
                           const list_by_structure_id_structure_identifiers_response&) = default;
};

struct list_structure_identifier_versions_request {
    using response_type = struct list_structure_identifier_versions_response;
    static constexpr std::string_view nats_subject =
        "trading.v1.structure_identifiers_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    structure_identifier_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<structure_identifier_versions_filter> filter;

    friend bool operator==(const list_structure_identifier_versions_request&,
                           const list_structure_identifier_versions_request&) = default;
};

struct list_structure_identifier_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::trading::domain::structure_identifier> versions;
    std::uint64_t total{};

    friend bool operator==(const list_structure_identifier_versions_response&,
                           const list_structure_identifier_versions_response&) = default;
};

struct get_structure_identifier_version_request {
    using response_type = struct get_structure_identifier_version_response;
    static constexpr std::string_view nats_subject =
        "trading.v1.structure_identifiers_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    structure_identifier_version_key key;

    friend bool operator==(const get_structure_identifier_version_request&,
                           const get_structure_identifier_version_request&) = default;
};

struct get_structure_identifier_version_response {
    ores::utility::domain::result result;
    std::optional<ores::trading::domain::structure_identifier> version;

    friend bool operator==(const get_structure_identifier_version_response&,
                           const get_structure_identifier_version_response&) = default;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace structure_identifier_event_subjects {
inline constexpr std::string_view created = "trading.v1.events.structure_identifiers.created";
inline constexpr std::string_view updated = "trading.v1.events.structure_identifiers.updated";
inline constexpr std::string_view deleted = "trading.v1.events.structure_identifiers.deleted";
}
/**
 * @brief The members of structure_identifier_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<structure_identifier_key>) {
    return std::tuple{
        utility::codec::field{"structure_id", &structure_identifier_key::structure_id},
        utility::codec::field{"id_type", &structure_identifier_key::id_type},
    };
}

/**
 * @brief The members of structure_identifier_write, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<structure_identifier_write>) {
    return std::tuple{
        utility::codec::field{"structure_id", &structure_identifier_write::structure_id},
        utility::codec::field{"id_type", &structure_identifier_write::id_type},
        utility::codec::field{"id_value", &structure_identifier_write::id_value},
        utility::codec::field{"issuing_party_id", &structure_identifier_write::issuing_party_id},
    };
}

/**
 * @brief The members of structure_identifier_change, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<structure_identifier_change>) {
    return std::tuple{
        utility::codec::field{"write", &structure_identifier_change::write},
        utility::codec::field{"precondition", &structure_identifier_change::precondition},
    };
}

/**
 * @brief The members of structure_identifier_removal, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<structure_identifier_removal>) {
    return std::tuple{
        utility::codec::field{"key", &structure_identifier_removal::key},
        utility::codec::field{"precondition", &structure_identifier_removal::precondition},
    };
}

/**
 * @brief The members of structure_identifier_lookup, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<structure_identifier_lookup>) {
    return std::tuple{
        utility::codec::field{"key", &structure_identifier_lookup::key},
        utility::codec::field{"structure_identifier",
                              &structure_identifier_lookup::structure_identifier},
    };
}

/**
 * @brief The members of structure_identifiers_filter, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<structure_identifiers_filter>) {
    return std::tuple{
        utility::codec::field{"structure_id", &structure_identifiers_filter::structure_id},
        utility::codec::field{"structure_id_one_of",
                              &structure_identifiers_filter::structure_id_one_of},
    };
}

/**
 * @brief The members of structure_identifier_event, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<structure_identifier_event>) {
    return std::tuple{
        utility::codec::field{"event_id", &structure_identifier_event::event_id},
        utility::codec::field{"key", &structure_identifier_event::key},
        utility::codec::field{"action", &structure_identifier_event::action},
        utility::codec::field{"version", &structure_identifier_event::version},
        utility::codec::field{"occurred_at", &structure_identifier_event::occurred_at},
        utility::codec::field{"correlation_id", &structure_identifier_event::correlation_id},
    };
}

/**
 * @brief The members of structure_identifier_version_key, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<structure_identifier_version_key>) {
    return std::tuple{
        utility::codec::field{"structure_identifier",
                              &structure_identifier_version_key::structure_identifier},
        utility::codec::field{"version", &structure_identifier_version_key::version},
    };
}

/**
 * @brief The members of structure_identifier_versions_filter, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<structure_identifier_versions_filter>) {
    return std::tuple{
        utility::codec::field{"version", &structure_identifier_versions_filter::version},
        utility::codec::field{"from_version", &structure_identifier_versions_filter::from_version},
        utility::codec::field{"to_version", &structure_identifier_versions_filter::to_version},
    };
}

/**
 * @brief The members of list_structure_identifiers_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<list_structure_identifiers_request>) {
    return std::tuple{
        utility::codec::field{"offset", &list_structure_identifiers_request::offset},
        utility::codec::field{"limit", &list_structure_identifiers_request::limit},
        utility::codec::field{"order", &list_structure_identifiers_request::order},
        utility::codec::field{"filter", &list_structure_identifiers_request::filter},
        utility::codec::field{"as_of", &list_structure_identifiers_request::as_of},
    };
}

/**
 * @brief The members of list_structure_identifiers_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<list_structure_identifiers_response>) {
    return std::tuple{
        utility::codec::field{"result", &list_structure_identifiers_response::result},
        utility::codec::field{"identifiers", &list_structure_identifiers_response::identifiers},
        utility::codec::field{"total", &list_structure_identifiers_response::total},
    };
}

/**
 * @brief The members of get_structure_identifier_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_structure_identifier_request>) {
    return std::tuple{
        utility::codec::field{"key", &get_structure_identifier_request::key},
    };
}

/**
 * @brief The members of get_structure_identifier_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_structure_identifier_response>) {
    return std::tuple{
        utility::codec::field{"result", &get_structure_identifier_response::result},
        utility::codec::field{"structure_identifier",
                              &get_structure_identifier_response::structure_identifier},
    };
}

/**
 * @brief The members of get_many_structure_identifiers_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<get_many_structure_identifiers_request>) {
    return std::tuple{
        utility::codec::field{"keys", &get_many_structure_identifiers_request::keys},
    };
}

/**
 * @brief The members of get_many_structure_identifiers_response, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<get_many_structure_identifiers_response>) {
    return std::tuple{
        utility::codec::field{"result", &get_many_structure_identifiers_response::result},
        utility::codec::field{"entries", &get_many_structure_identifiers_response::entries},
    };
}

/**
 * @brief The members of put_structure_identifier_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<put_structure_identifier_request>) {
    return std::tuple{
        utility::codec::field{"change", &put_structure_identifier_request::change},
        utility::codec::field{"intent", &put_structure_identifier_request::intent},
    };
}

/**
 * @brief The members of put_structure_identifier_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<put_structure_identifier_response>) {
    return std::tuple{
        utility::codec::field{"result", &put_structure_identifier_response::result},
        utility::codec::field{"structure_identifier",
                              &put_structure_identifier_response::structure_identifier},
    };
}

/**
 * @brief The members of put_many_structure_identifiers_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<put_many_structure_identifiers_request>) {
    return std::tuple{
        utility::codec::field{"changes", &put_many_structure_identifiers_request::changes},
        utility::codec::field{"intent", &put_many_structure_identifiers_request::intent},
    };
}

/**
 * @brief The members of put_many_structure_identifiers_response, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<put_many_structure_identifiers_response>) {
    return std::tuple{
        utility::codec::field{"result", &put_many_structure_identifiers_response::result},
        utility::codec::field{"identifiers", &put_many_structure_identifiers_response::identifiers},
    };
}

/**
 * @brief The members of delete_structure_identifier_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<delete_structure_identifier_request>) {
    return std::tuple{
        utility::codec::field{"removal", &delete_structure_identifier_request::removal},
        utility::codec::field{"intent", &delete_structure_identifier_request::intent},
    };
}

/**
 * @brief The members of delete_structure_identifier_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<delete_structure_identifier_response>) {
    return std::tuple{
        utility::codec::field{"result", &delete_structure_identifier_response::result},
    };
}

/**
 * @brief The members of delete_many_structure_identifiers_request, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<delete_many_structure_identifiers_request>) {
    return std::tuple{
        utility::codec::field{"removals", &delete_many_structure_identifiers_request::removals},
        utility::codec::field{"intent", &delete_many_structure_identifiers_request::intent},
    };
}

/**
 * @brief The members of delete_many_structure_identifiers_response, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<delete_many_structure_identifiers_response>) {
    return std::tuple{
        utility::codec::field{"result", &delete_many_structure_identifiers_response::result},
    };
}

/**
 * @brief The members of list_by_structure_id_structure_identifiers_request, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<list_by_structure_id_structure_identifiers_request>) {
    return std::tuple{
        utility::codec::field{"structure_id",
                              &list_by_structure_id_structure_identifiers_request::structure_id},
        utility::codec::field{"scope", &list_by_structure_id_structure_identifiers_request::scope},
        utility::codec::field{"offset",
                              &list_by_structure_id_structure_identifiers_request::offset},
        utility::codec::field{"limit", &list_by_structure_id_structure_identifiers_request::limit},
        utility::codec::field{"order", &list_by_structure_id_structure_identifiers_request::order},
        utility::codec::field{"filter",
                              &list_by_structure_id_structure_identifiers_request::filter},
    };
}

/**
 * @brief The members of list_by_structure_id_structure_identifiers_response, named as the wire
 * names them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<list_by_structure_id_structure_identifiers_response>) {
    return std::tuple{
        utility::codec::field{"result",
                              &list_by_structure_id_structure_identifiers_response::result},
        utility::codec::field{"identifiers",
                              &list_by_structure_id_structure_identifiers_response::identifiers},
        utility::codec::field{"total", &list_by_structure_id_structure_identifiers_response::total},
    };
}

/**
 * @brief The members of list_structure_identifier_versions_request, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<list_structure_identifier_versions_request>) {
    return std::tuple{
        utility::codec::field{"key", &list_structure_identifier_versions_request::key},
        utility::codec::field{"offset", &list_structure_identifier_versions_request::offset},
        utility::codec::field{"limit", &list_structure_identifier_versions_request::limit},
        utility::codec::field{"order", &list_structure_identifier_versions_request::order},
        utility::codec::field{"filter", &list_structure_identifier_versions_request::filter},
    };
}

/**
 * @brief The members of list_structure_identifier_versions_response, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<list_structure_identifier_versions_response>) {
    return std::tuple{
        utility::codec::field{"result", &list_structure_identifier_versions_response::result},
        utility::codec::field{"versions", &list_structure_identifier_versions_response::versions},
        utility::codec::field{"total", &list_structure_identifier_versions_response::total},
    };
}

/**
 * @brief The members of get_structure_identifier_version_request, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<get_structure_identifier_version_request>) {
    return std::tuple{
        utility::codec::field{"key", &get_structure_identifier_version_request::key},
    };
}

/**
 * @brief The members of get_structure_identifier_version_response, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<get_structure_identifier_version_response>) {
    return std::tuple{
        utility::codec::field{"result", &get_structure_identifier_version_response::result},
        utility::codec::field{"version", &get_structure_identifier_version_response::version},
    };
}


}
namespace ores::utility::codec {

template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::structure_identifier_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::structure_identifier_key& v);
    static ores::trading::messaging::structure_identifier_key read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::structure_identifier_key& v);
    static ores::trading::messaging::structure_identifier_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::structure_identifier_write> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::structure_identifier_write& v);
    static ores::trading::messaging::structure_identifier_write read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::structure_identifier_write& v);
    static ores::trading::messaging::structure_identifier_write
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::structure_identifier_change> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::structure_identifier_change& v);
    static ores::trading::messaging::structure_identifier_change read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::structure_identifier_change& v);
    static ores::trading::messaging::structure_identifier_change
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::structure_identifier_removal> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::structure_identifier_removal& v);
    static ores::trading::messaging::structure_identifier_removal read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::structure_identifier_removal& v);
    static ores::trading::messaging::structure_identifier_removal
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::structure_identifier_lookup> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::structure_identifier_lookup& v);
    static ores::trading::messaging::structure_identifier_lookup read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::structure_identifier_lookup& v);
    static ores::trading::messaging::structure_identifier_lookup
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::structure_identifiers_filter> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::structure_identifiers_filter& v);
    static ores::trading::messaging::structure_identifiers_filter read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::structure_identifiers_filter& v);
    static ores::trading::messaging::structure_identifiers_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::structure_identifier_event> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::structure_identifier_event& v);
    static ores::trading::messaging::structure_identifier_event read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::structure_identifier_event& v);
    static ores::trading::messaging::structure_identifier_event
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::structure_identifier_version_key> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::structure_identifier_version_key& v);
    static ores::trading::messaging::structure_identifier_version_key
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::structure_identifier_version_key& v);
    static ores::trading::messaging::structure_identifier_version_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::structure_identifier_versions_filter> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::structure_identifier_versions_filter& v);
    static ores::trading::messaging::structure_identifier_versions_filter
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::structure_identifier_versions_filter& v);
    static ores::trading::messaging::structure_identifier_versions_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::list_structure_identifiers_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::list_structure_identifiers_request& v);
    static ores::trading::messaging::list_structure_identifiers_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::list_structure_identifiers_request& v);
    static ores::trading::messaging::list_structure_identifiers_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::list_structure_identifiers_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::list_structure_identifiers_response& v);
    static ores::trading::messaging::list_structure_identifiers_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::list_structure_identifiers_response& v);
    static ores::trading::messaging::list_structure_identifiers_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::get_structure_identifier_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::get_structure_identifier_request& v);
    static ores::trading::messaging::get_structure_identifier_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::get_structure_identifier_request& v);
    static ores::trading::messaging::get_structure_identifier_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::get_structure_identifier_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::get_structure_identifier_response& v);
    static ores::trading::messaging::get_structure_identifier_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::get_structure_identifier_response& v);
    static ores::trading::messaging::get_structure_identifier_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::get_many_structure_identifiers_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::get_many_structure_identifiers_request& v);
    static ores::trading::messaging::get_many_structure_identifiers_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::get_many_structure_identifiers_request& v);
    static ores::trading::messaging::get_many_structure_identifiers_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::get_many_structure_identifiers_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::get_many_structure_identifiers_response& v);
    static ores::trading::messaging::get_many_structure_identifiers_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::get_many_structure_identifiers_response& v);
    static ores::trading::messaging::get_many_structure_identifiers_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::put_structure_identifier_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::put_structure_identifier_request& v);
    static ores::trading::messaging::put_structure_identifier_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::put_structure_identifier_request& v);
    static ores::trading::messaging::put_structure_identifier_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::put_structure_identifier_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::put_structure_identifier_response& v);
    static ores::trading::messaging::put_structure_identifier_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::put_structure_identifier_response& v);
    static ores::trading::messaging::put_structure_identifier_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::put_many_structure_identifiers_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::put_many_structure_identifiers_request& v);
    static ores::trading::messaging::put_many_structure_identifiers_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::put_many_structure_identifiers_request& v);
    static ores::trading::messaging::put_many_structure_identifiers_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::put_many_structure_identifiers_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::put_many_structure_identifiers_response& v);
    static ores::trading::messaging::put_many_structure_identifiers_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::put_many_structure_identifiers_response& v);
    static ores::trading::messaging::put_many_structure_identifiers_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::delete_structure_identifier_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::delete_structure_identifier_request& v);
    static ores::trading::messaging::delete_structure_identifier_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::delete_structure_identifier_request& v);
    static ores::trading::messaging::delete_structure_identifier_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::delete_structure_identifier_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::delete_structure_identifier_response& v);
    static ores::trading::messaging::delete_structure_identifier_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::delete_structure_identifier_response& v);
    static ores::trading::messaging::delete_structure_identifier_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::delete_many_structure_identifiers_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::delete_many_structure_identifiers_request& v);
    static ores::trading::messaging::delete_many_structure_identifiers_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::delete_many_structure_identifiers_request& v);
    static ores::trading::messaging::delete_many_structure_identifiers_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::delete_many_structure_identifiers_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::delete_many_structure_identifiers_response& v);
    static ores::trading::messaging::delete_many_structure_identifiers_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::delete_many_structure_identifiers_response& v);
    static ores::trading::messaging::delete_many_structure_identifiers_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::list_by_structure_id_structure_identifiers_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::trading::messaging::list_by_structure_id_structure_identifiers_request& v);
    static ores::trading::messaging::list_by_structure_id_structure_identifiers_request
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::trading::messaging::list_by_structure_id_structure_identifiers_request& v);
    static ores::trading::messaging::list_by_structure_id_structure_identifiers_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::list_by_structure_id_structure_identifiers_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::trading::messaging::list_by_structure_id_structure_identifiers_response& v);
    static ores::trading::messaging::list_by_structure_id_structure_identifiers_response
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::trading::messaging::list_by_structure_id_structure_identifiers_response& v);
    static ores::trading::messaging::list_by_structure_id_structure_identifiers_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::list_structure_identifier_versions_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::list_structure_identifier_versions_request& v);
    static ores::trading::messaging::list_structure_identifier_versions_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::list_structure_identifier_versions_request& v);
    static ores::trading::messaging::list_structure_identifier_versions_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::list_structure_identifier_versions_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::list_structure_identifier_versions_response& v);
    static ores::trading::messaging::list_structure_identifier_versions_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::list_structure_identifier_versions_response& v);
    static ores::trading::messaging::list_structure_identifier_versions_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::get_structure_identifier_version_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::get_structure_identifier_version_request& v);
    static ores::trading::messaging::get_structure_identifier_version_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::get_structure_identifier_version_request& v);
    static ores::trading::messaging::get_structure_identifier_version_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::get_structure_identifier_version_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::get_structure_identifier_version_response& v);
    static ores::trading::messaging::get_structure_identifier_version_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::get_structure_identifier_version_response& v);
    static ores::trading::messaging::get_structure_identifier_version_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
