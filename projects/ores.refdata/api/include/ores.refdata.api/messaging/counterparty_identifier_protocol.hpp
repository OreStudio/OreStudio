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
#ifndef ORES_REFDATA_API_MESSAGING_COUNTERPARTY_IDENTIFIER_PROTOCOL_HPP
#define ORES_REFDATA_API_MESSAGING_COUNTERPARTY_IDENTIFIER_PROTOCOL_HPP

#include "ores.refdata.api/domain/counterparty_identifier_codec.hpp"
#include "ores.refdata.api/export.hpp"
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

namespace ores::refdata::messaging {

struct counterparty_identifier_key {
    std::string id_value;

    friend bool operator==(const counterparty_identifier_key&,
                           const counterparty_identifier_key&) = default;
};

struct counterparty_identifier_write {
    boost::uuids::uuid id;
    boost::uuids::uuid counterparty_id;
    std::string id_scheme;
    std::string id_value;
    std::string description;
    bool is_authoritative{};

    friend bool operator==(const counterparty_identifier_write&,
                           const counterparty_identifier_write&) = default;
};

struct counterparty_identifier_change {
    counterparty_identifier_write write;
    ores::utility::domain::precondition precondition;

    friend bool operator==(const counterparty_identifier_change&,
                           const counterparty_identifier_change&) = default;
};

struct counterparty_identifier_removal {
    counterparty_identifier_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;

    friend bool operator==(const counterparty_identifier_removal&,
                           const counterparty_identifier_removal&) = default;
};

struct counterparty_identifier_lookup {
    counterparty_identifier_key key;
    std::optional<ores::refdata::domain::counterparty_identifier> counterparty_identifier;

    friend bool operator==(const counterparty_identifier_lookup&,
                           const counterparty_identifier_lookup&) = default;
};

struct counterparty_identifiers_filter {
    std::optional<boost::uuids::uuid> counterparty_id;
    std::optional<std::vector<boost::uuids::uuid>> id_one_of;
    std::optional<std::vector<boost::uuids::uuid>> counterparty_id_one_of;

    friend bool operator==(const counterparty_identifiers_filter&,
                           const counterparty_identifiers_filter&) = default;
};

struct counterparty_identifier_event {
    boost::uuids::uuid event_id;
    counterparty_identifier_key key;
    std::string action;
    std::uint32_t version{};
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;

    friend bool operator==(const counterparty_identifier_event&,
                           const counterparty_identifier_event&) = default;
};

struct counterparty_identifier_version_key {
    counterparty_identifier_key counterparty_identifier;
    std::uint32_t version{};

    friend bool operator==(const counterparty_identifier_version_key&,
                           const counterparty_identifier_version_key&) = default;
};

struct counterparty_identifier_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;

    friend bool operator==(const counterparty_identifier_versions_filter&,
                           const counterparty_identifier_versions_filter&) = default;
};

struct list_counterparty_identifiers_request {
    using response_type = struct list_counterparty_identifiers_response;
    static constexpr std::string_view nats_subject = "refdata.v1.counterparty_identifiers.list";
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
    std::optional<counterparty_identifiers_filter> filter;
    std::optional<std::string> as_of;

    friend bool operator==(const list_counterparty_identifiers_request&,
                           const list_counterparty_identifiers_request&) = default;
};

struct list_counterparty_identifiers_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::counterparty_identifier> counterparty_identifiers;
    std::uint64_t total{};

    friend bool operator==(const list_counterparty_identifiers_response&,
                           const list_counterparty_identifiers_response&) = default;
};

struct get_counterparty_identifier_request {
    using response_type = struct get_counterparty_identifier_response;
    static constexpr std::string_view nats_subject = "refdata.v1.counterparty_identifiers.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    counterparty_identifier_key key;

    friend bool operator==(const get_counterparty_identifier_request&,
                           const get_counterparty_identifier_request&) = default;
};

struct get_counterparty_identifier_response {
    ores::utility::domain::result result;
    std::optional<ores::refdata::domain::counterparty_identifier> counterparty_identifier;

    friend bool operator==(const get_counterparty_identifier_response&,
                           const get_counterparty_identifier_response&) = default;
};

struct get_many_counterparty_identifiers_request {
    using response_type = struct get_many_counterparty_identifiers_response;
    static constexpr std::string_view nats_subject = "refdata.v1.counterparty_identifiers.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<counterparty_identifier_key> keys;

    friend bool operator==(const get_many_counterparty_identifiers_request&,
                           const get_many_counterparty_identifiers_request&) = default;
};

struct get_many_counterparty_identifiers_response {
    ores::utility::domain::result result;
    std::vector<counterparty_identifier_lookup> entries;

    friend bool operator==(const get_many_counterparty_identifiers_response&,
                           const get_many_counterparty_identifiers_response&) = default;
};

struct put_counterparty_identifier_request {
    using response_type = struct put_counterparty_identifier_response;
    static constexpr std::string_view nats_subject = "refdata.v1.counterparty_identifiers.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    counterparty_identifier_change change;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const put_counterparty_identifier_request&,
                           const put_counterparty_identifier_request&) = default;
};

struct put_counterparty_identifier_response {
    ores::utility::domain::result result;
    std::optional<ores::refdata::domain::counterparty_identifier> counterparty_identifier;

    friend bool operator==(const put_counterparty_identifier_response&,
                           const put_counterparty_identifier_response&) = default;
};

struct put_many_counterparty_identifiers_request {
    using response_type = struct put_many_counterparty_identifiers_response;
    static constexpr std::string_view nats_subject = "refdata.v1.counterparty_identifiers.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<counterparty_identifier_change> changes;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const put_many_counterparty_identifiers_request&,
                           const put_many_counterparty_identifiers_request&) = default;
};

struct put_many_counterparty_identifiers_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::counterparty_identifier> counterparty_identifiers;

    friend bool operator==(const put_many_counterparty_identifiers_response&,
                           const put_many_counterparty_identifiers_response&) = default;
};

struct delete_counterparty_identifier_request {
    using response_type = struct delete_counterparty_identifier_response;
    static constexpr std::string_view nats_subject = "refdata.v1.counterparty_identifiers.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    counterparty_identifier_removal removal;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const delete_counterparty_identifier_request&,
                           const delete_counterparty_identifier_request&) = default;
};

struct delete_counterparty_identifier_response {
    ores::utility::domain::result result;

    friend bool operator==(const delete_counterparty_identifier_response&,
                           const delete_counterparty_identifier_response&) = default;
};

struct delete_many_counterparty_identifiers_request {
    using response_type = struct delete_many_counterparty_identifiers_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.counterparty_identifiers.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<counterparty_identifier_removal> removals;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const delete_many_counterparty_identifiers_request&,
                           const delete_many_counterparty_identifiers_request&) = default;
};

struct delete_many_counterparty_identifiers_response {
    ores::utility::domain::result result;

    friend bool operator==(const delete_many_counterparty_identifiers_response&,
                           const delete_many_counterparty_identifiers_response&) = default;
};

struct list_by_counterparty_id_counterparty_identifiers_request {
    using response_type = struct list_by_counterparty_id_counterparty_identifiers_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.counterparty_identifiers.list_by_counterparty_id";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    boost::uuids::uuid counterparty_id;
    ores::utility::domain::scope scope = ores::utility::domain::scope::direct;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<counterparty_identifiers_filter> filter;

    friend bool
    operator==(const list_by_counterparty_id_counterparty_identifiers_request&,
               const list_by_counterparty_id_counterparty_identifiers_request&) = default;
};

struct list_by_counterparty_id_counterparty_identifiers_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::counterparty_identifier> counterparty_identifiers;
    std::uint64_t total{};

    friend bool
    operator==(const list_by_counterparty_id_counterparty_identifiers_response&,
               const list_by_counterparty_id_counterparty_identifiers_response&) = default;
};

struct list_counterparty_identifier_versions_request {
    using response_type = struct list_counterparty_identifier_versions_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.counterparty_identifiers_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    counterparty_identifier_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<counterparty_identifier_versions_filter> filter;

    friend bool operator==(const list_counterparty_identifier_versions_request&,
                           const list_counterparty_identifier_versions_request&) = default;
};

struct list_counterparty_identifier_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::counterparty_identifier> versions;
    std::uint64_t total{};

    friend bool operator==(const list_counterparty_identifier_versions_response&,
                           const list_counterparty_identifier_versions_response&) = default;
};

struct get_counterparty_identifier_version_request {
    using response_type = struct get_counterparty_identifier_version_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.counterparty_identifiers_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    counterparty_identifier_version_key key;

    friend bool operator==(const get_counterparty_identifier_version_request&,
                           const get_counterparty_identifier_version_request&) = default;
};

struct get_counterparty_identifier_version_response {
    ores::utility::domain::result result;
    std::optional<ores::refdata::domain::counterparty_identifier> version;

    friend bool operator==(const get_counterparty_identifier_version_response&,
                           const get_counterparty_identifier_version_response&) = default;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace counterparty_identifier_event_subjects {
inline constexpr std::string_view created = "refdata.v1.events.counterparty_identifiers.created";
inline constexpr std::string_view updated = "refdata.v1.events.counterparty_identifiers.updated";
inline constexpr std::string_view deleted = "refdata.v1.events.counterparty_identifiers.deleted";
}
/**
 * @brief The members of counterparty_identifier_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<counterparty_identifier_key>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"id_value", &counterparty_identifier_key::id_value},
    };
}

/**
 * @brief The members of counterparty_identifier_write, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<counterparty_identifier_write>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"id", &counterparty_identifier_write::id},
        utility::codec::field{"counterparty_id", &counterparty_identifier_write::counterparty_id},
        utility::codec::field{"id_scheme", &counterparty_identifier_write::id_scheme},
        utility::codec::field{"id_value", &counterparty_identifier_write::id_value},
        utility::codec::field{"description", &counterparty_identifier_write::description},
        utility::codec::field{"is_authoritative", &counterparty_identifier_write::is_authoritative},
    };
}

/**
 * @brief The members of counterparty_identifier_change, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<counterparty_identifier_change>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"write", &counterparty_identifier_change::write},
        utility::codec::field{"precondition", &counterparty_identifier_change::precondition},
    };
}

/**
 * @brief The members of counterparty_identifier_removal, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<counterparty_identifier_removal>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"key", &counterparty_identifier_removal::key},
        utility::codec::field{"precondition", &counterparty_identifier_removal::precondition},
    };
}

/**
 * @brief The members of counterparty_identifier_lookup, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<counterparty_identifier_lookup>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"key", &counterparty_identifier_lookup::key},
        utility::codec::field{"counterparty_identifier",
                              &counterparty_identifier_lookup::counterparty_identifier},
    };
}

/**
 * @brief The members of counterparty_identifiers_filter, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<counterparty_identifiers_filter>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"counterparty_id", &counterparty_identifiers_filter::counterparty_id},
        utility::codec::field{"id_one_of", &counterparty_identifiers_filter::id_one_of},
        utility::codec::field{"counterparty_id_one_of",
                              &counterparty_identifiers_filter::counterparty_id_one_of},
    };
}

/**
 * @brief The members of counterparty_identifier_event, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<counterparty_identifier_event>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"event_id", &counterparty_identifier_event::event_id},
        utility::codec::field{"key", &counterparty_identifier_event::key},
        utility::codec::field{"action", &counterparty_identifier_event::action},
        utility::codec::field{"version", &counterparty_identifier_event::version},
        utility::codec::field{"occurred_at", &counterparty_identifier_event::occurred_at},
        utility::codec::field{"correlation_id", &counterparty_identifier_event::correlation_id},
    };
}

/**
 * @brief The members of counterparty_identifier_version_key, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<counterparty_identifier_version_key>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"counterparty_identifier",
                              &counterparty_identifier_version_key::counterparty_identifier},
        utility::codec::field{"version", &counterparty_identifier_version_key::version},
    };
}

/**
 * @brief The members of counterparty_identifier_versions_filter, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<counterparty_identifier_versions_filter>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"version", &counterparty_identifier_versions_filter::version},
        utility::codec::field{"from_version",
                              &counterparty_identifier_versions_filter::from_version},
        utility::codec::field{"to_version", &counterparty_identifier_versions_filter::to_version},
    };
}

/**
 * @brief The members of list_counterparty_identifiers_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<list_counterparty_identifiers_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"offset", &list_counterparty_identifiers_request::offset},
        utility::codec::field{"limit", &list_counterparty_identifiers_request::limit},
        utility::codec::field{"order", &list_counterparty_identifiers_request::order},
        utility::codec::field{"filter", &list_counterparty_identifiers_request::filter},
        utility::codec::field{"as_of", &list_counterparty_identifiers_request::as_of},
    };
}

/**
 * @brief The members of list_counterparty_identifiers_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<list_counterparty_identifiers_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &list_counterparty_identifiers_response::result},
        utility::codec::field{"counterparty_identifiers",
                              &list_counterparty_identifiers_response::counterparty_identifiers},
        utility::codec::field{"total", &list_counterparty_identifiers_response::total},
    };
}

/**
 * @brief The members of get_counterparty_identifier_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_counterparty_identifier_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"key", &get_counterparty_identifier_request::key},
    };
}

/**
 * @brief The members of get_counterparty_identifier_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_counterparty_identifier_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &get_counterparty_identifier_response::result},
        utility::codec::field{"counterparty_identifier",
                              &get_counterparty_identifier_response::counterparty_identifier},
    };
}

/**
 * @brief The members of get_many_counterparty_identifiers_request, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<get_many_counterparty_identifiers_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"keys", &get_many_counterparty_identifiers_request::keys},
    };
}

/**
 * @brief The members of get_many_counterparty_identifiers_response, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<get_many_counterparty_identifiers_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &get_many_counterparty_identifiers_response::result},
        utility::codec::field{"entries", &get_many_counterparty_identifiers_response::entries},
    };
}

/**
 * @brief The members of put_counterparty_identifier_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<put_counterparty_identifier_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"change", &put_counterparty_identifier_request::change},
        utility::codec::field{"intent", &put_counterparty_identifier_request::intent},
    };
}

/**
 * @brief The members of put_counterparty_identifier_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<put_counterparty_identifier_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &put_counterparty_identifier_response::result},
        utility::codec::field{"counterparty_identifier",
                              &put_counterparty_identifier_response::counterparty_identifier},
    };
}

/**
 * @brief The members of put_many_counterparty_identifiers_request, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<put_many_counterparty_identifiers_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"changes", &put_many_counterparty_identifiers_request::changes},
        utility::codec::field{"intent", &put_many_counterparty_identifiers_request::intent},
    };
}

/**
 * @brief The members of put_many_counterparty_identifiers_response, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<put_many_counterparty_identifiers_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &put_many_counterparty_identifiers_response::result},
        utility::codec::field{
            "counterparty_identifiers",
            &put_many_counterparty_identifiers_response::counterparty_identifiers},
    };
}

/**
 * @brief The members of delete_counterparty_identifier_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<delete_counterparty_identifier_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"removal", &delete_counterparty_identifier_request::removal},
        utility::codec::field{"intent", &delete_counterparty_identifier_request::intent},
    };
}

/**
 * @brief The members of delete_counterparty_identifier_response, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<delete_counterparty_identifier_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &delete_counterparty_identifier_response::result},
    };
}

/**
 * @brief The members of delete_many_counterparty_identifiers_request, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<delete_many_counterparty_identifiers_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"removals", &delete_many_counterparty_identifiers_request::removals},
        utility::codec::field{"intent", &delete_many_counterparty_identifiers_request::intent},
    };
}

/**
 * @brief The members of delete_many_counterparty_identifiers_response, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<delete_many_counterparty_identifiers_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &delete_many_counterparty_identifiers_response::result},
    };
}

/**
 * @brief The members of list_by_counterparty_id_counterparty_identifiers_request, named as the wire
 * names them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<list_by_counterparty_id_counterparty_identifiers_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{
            "counterparty_id",
            &list_by_counterparty_id_counterparty_identifiers_request::counterparty_id},
        utility::codec::field{"scope",
                              &list_by_counterparty_id_counterparty_identifiers_request::scope},
        utility::codec::field{"offset",
                              &list_by_counterparty_id_counterparty_identifiers_request::offset},
        utility::codec::field{"limit",
                              &list_by_counterparty_id_counterparty_identifiers_request::limit},
        utility::codec::field{"order",
                              &list_by_counterparty_id_counterparty_identifiers_request::order},
        utility::codec::field{"filter",
                              &list_by_counterparty_id_counterparty_identifiers_request::filter},
    };
}

/**
 * @brief The members of list_by_counterparty_id_counterparty_identifiers_response, named as the
 * wire names them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<list_by_counterparty_id_counterparty_identifiers_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result",
                              &list_by_counterparty_id_counterparty_identifiers_response::result},
        utility::codec::field{
            "counterparty_identifiers",
            &list_by_counterparty_id_counterparty_identifiers_response::counterparty_identifiers},
        utility::codec::field{"total",
                              &list_by_counterparty_id_counterparty_identifiers_response::total},
    };
}

/**
 * @brief The members of list_counterparty_identifier_versions_request, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<list_counterparty_identifier_versions_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"key", &list_counterparty_identifier_versions_request::key},
        utility::codec::field{"offset", &list_counterparty_identifier_versions_request::offset},
        utility::codec::field{"limit", &list_counterparty_identifier_versions_request::limit},
        utility::codec::field{"order", &list_counterparty_identifier_versions_request::order},
        utility::codec::field{"filter", &list_counterparty_identifier_versions_request::filter},
    };
}

/**
 * @brief The members of list_counterparty_identifier_versions_response, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<list_counterparty_identifier_versions_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &list_counterparty_identifier_versions_response::result},
        utility::codec::field{"versions",
                              &list_counterparty_identifier_versions_response::versions},
        utility::codec::field{"total", &list_counterparty_identifier_versions_response::total},
    };
}

/**
 * @brief The members of get_counterparty_identifier_version_request, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<get_counterparty_identifier_version_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"key", &get_counterparty_identifier_version_request::key},
    };
}

/**
 * @brief The members of get_counterparty_identifier_version_response, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<get_counterparty_identifier_version_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &get_counterparty_identifier_version_response::result},
        utility::codec::field{"version", &get_counterparty_identifier_version_response::version},
    };
}


}
namespace ores::utility::codec {

template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::counterparty_identifier_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::counterparty_identifier_key& v);
    static ores::refdata::messaging::counterparty_identifier_key read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::counterparty_identifier_key& v);
    static ores::refdata::messaging::counterparty_identifier_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::counterparty_identifier_write> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::counterparty_identifier_write& v);
    static ores::refdata::messaging::counterparty_identifier_write read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::counterparty_identifier_write& v);
    static ores::refdata::messaging::counterparty_identifier_write
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::counterparty_identifier_change> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::counterparty_identifier_change& v);
    static ores::refdata::messaging::counterparty_identifier_change
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::counterparty_identifier_change& v);
    static ores::refdata::messaging::counterparty_identifier_change
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::counterparty_identifier_removal> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::counterparty_identifier_removal& v);
    static ores::refdata::messaging::counterparty_identifier_removal
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::counterparty_identifier_removal& v);
    static ores::refdata::messaging::counterparty_identifier_removal
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::counterparty_identifier_lookup> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::counterparty_identifier_lookup& v);
    static ores::refdata::messaging::counterparty_identifier_lookup
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::counterparty_identifier_lookup& v);
    static ores::refdata::messaging::counterparty_identifier_lookup
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::counterparty_identifiers_filter> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::counterparty_identifiers_filter& v);
    static ores::refdata::messaging::counterparty_identifiers_filter
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::counterparty_identifiers_filter& v);
    static ores::refdata::messaging::counterparty_identifiers_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::counterparty_identifier_event> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::counterparty_identifier_event& v);
    static ores::refdata::messaging::counterparty_identifier_event read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::counterparty_identifier_event& v);
    static ores::refdata::messaging::counterparty_identifier_event
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::counterparty_identifier_version_key> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::counterparty_identifier_version_key& v);
    static ores::refdata::messaging::counterparty_identifier_version_key
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::counterparty_identifier_version_key& v);
    static ores::refdata::messaging::counterparty_identifier_version_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::counterparty_identifier_versions_filter> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::counterparty_identifier_versions_filter& v);
    static ores::refdata::messaging::counterparty_identifier_versions_filter
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::counterparty_identifier_versions_filter& v);
    static ores::refdata::messaging::counterparty_identifier_versions_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::list_counterparty_identifiers_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::list_counterparty_identifiers_request& v);
    static ores::refdata::messaging::list_counterparty_identifiers_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::list_counterparty_identifiers_request& v);
    static ores::refdata::messaging::list_counterparty_identifiers_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::list_counterparty_identifiers_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::list_counterparty_identifiers_response& v);
    static ores::refdata::messaging::list_counterparty_identifiers_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::list_counterparty_identifiers_response& v);
    static ores::refdata::messaging::list_counterparty_identifiers_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_counterparty_identifier_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::get_counterparty_identifier_request& v);
    static ores::refdata::messaging::get_counterparty_identifier_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::get_counterparty_identifier_request& v);
    static ores::refdata::messaging::get_counterparty_identifier_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_counterparty_identifier_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::get_counterparty_identifier_response& v);
    static ores::refdata::messaging::get_counterparty_identifier_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::get_counterparty_identifier_response& v);
    static ores::refdata::messaging::get_counterparty_identifier_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_many_counterparty_identifiers_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::get_many_counterparty_identifiers_request& v);
    static ores::refdata::messaging::get_many_counterparty_identifiers_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::get_many_counterparty_identifiers_request& v);
    static ores::refdata::messaging::get_many_counterparty_identifiers_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_many_counterparty_identifiers_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::get_many_counterparty_identifiers_response& v);
    static ores::refdata::messaging::get_many_counterparty_identifiers_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::get_many_counterparty_identifiers_response& v);
    static ores::refdata::messaging::get_many_counterparty_identifiers_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::put_counterparty_identifier_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::put_counterparty_identifier_request& v);
    static ores::refdata::messaging::put_counterparty_identifier_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::put_counterparty_identifier_request& v);
    static ores::refdata::messaging::put_counterparty_identifier_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::put_counterparty_identifier_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::put_counterparty_identifier_response& v);
    static ores::refdata::messaging::put_counterparty_identifier_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::put_counterparty_identifier_response& v);
    static ores::refdata::messaging::put_counterparty_identifier_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::put_many_counterparty_identifiers_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::put_many_counterparty_identifiers_request& v);
    static ores::refdata::messaging::put_many_counterparty_identifiers_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::put_many_counterparty_identifiers_request& v);
    static ores::refdata::messaging::put_many_counterparty_identifiers_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::put_many_counterparty_identifiers_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::put_many_counterparty_identifiers_response& v);
    static ores::refdata::messaging::put_many_counterparty_identifiers_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::put_many_counterparty_identifiers_response& v);
    static ores::refdata::messaging::put_many_counterparty_identifiers_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::delete_counterparty_identifier_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::delete_counterparty_identifier_request& v);
    static ores::refdata::messaging::delete_counterparty_identifier_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::delete_counterparty_identifier_request& v);
    static ores::refdata::messaging::delete_counterparty_identifier_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::delete_counterparty_identifier_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::delete_counterparty_identifier_response& v);
    static ores::refdata::messaging::delete_counterparty_identifier_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::delete_counterparty_identifier_response& v);
    static ores::refdata::messaging::delete_counterparty_identifier_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::delete_many_counterparty_identifiers_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::delete_many_counterparty_identifiers_request& v);
    static ores::refdata::messaging::delete_many_counterparty_identifiers_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::delete_many_counterparty_identifiers_request& v);
    static ores::refdata::messaging::delete_many_counterparty_identifiers_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::delete_many_counterparty_identifiers_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::delete_many_counterparty_identifiers_response& v);
    static ores::refdata::messaging::delete_many_counterparty_identifiers_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::delete_many_counterparty_identifiers_response& v);
    static ores::refdata::messaging::delete_many_counterparty_identifiers_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<
    ores::refdata::messaging::list_by_counterparty_id_counterparty_identifiers_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::refdata::messaging::list_by_counterparty_id_counterparty_identifiers_request&
            v);
    static ores::refdata::messaging::list_by_counterparty_id_counterparty_identifiers_request
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::refdata::messaging::list_by_counterparty_id_counterparty_identifiers_request&
            v);
    static ores::refdata::messaging::list_by_counterparty_id_counterparty_identifiers_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<
    ores::refdata::messaging::list_by_counterparty_id_counterparty_identifiers_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::refdata::messaging::list_by_counterparty_id_counterparty_identifiers_response&
            v);
    static ores::refdata::messaging::list_by_counterparty_id_counterparty_identifiers_response
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::refdata::messaging::list_by_counterparty_id_counterparty_identifiers_response&
            v);
    static ores::refdata::messaging::list_by_counterparty_id_counterparty_identifiers_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::list_counterparty_identifier_versions_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::list_counterparty_identifier_versions_request& v);
    static ores::refdata::messaging::list_counterparty_identifier_versions_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::list_counterparty_identifier_versions_request& v);
    static ores::refdata::messaging::list_counterparty_identifier_versions_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::list_counterparty_identifier_versions_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::list_counterparty_identifier_versions_response& v);
    static ores::refdata::messaging::list_counterparty_identifier_versions_response
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::refdata::messaging::list_counterparty_identifier_versions_response& v);
    static ores::refdata::messaging::list_counterparty_identifier_versions_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_counterparty_identifier_version_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::get_counterparty_identifier_version_request& v);
    static ores::refdata::messaging::get_counterparty_identifier_version_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::get_counterparty_identifier_version_request& v);
    static ores::refdata::messaging::get_counterparty_identifier_version_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_counterparty_identifier_version_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::get_counterparty_identifier_version_response& v);
    static ores::refdata::messaging::get_counterparty_identifier_version_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::get_counterparty_identifier_version_response& v);
    static ores::refdata::messaging::get_counterparty_identifier_version_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
