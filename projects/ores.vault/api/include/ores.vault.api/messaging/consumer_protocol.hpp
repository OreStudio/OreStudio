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
#ifndef ORES_VAULT_API_MESSAGING_CONSUMER_PROTOCOL_HPP
#define ORES_VAULT_API_MESSAGING_CONSUMER_PROTOCOL_HPP

#include "ores.utility/codec/wire_fields.hpp"
#include "ores.utility/codec/wire_traits_fwd.hpp"
#include "ores.utility/domain/protocol.hpp"
#include "ores.utility/domain/protocol_codec.hpp"
#include "ores.vault.api/domain/consumer_codec.hpp"
#include "ores.vault.api/export.hpp"
#include <boost/uuid/uuid.hpp>
#include <cstddef>
#include <cstdint>
#include <optional>
#include <span>
#include <string>
#include <tuple>
#include <vector>

namespace ores::vault::messaging {

struct consumer_key {
    std::string name;

    friend bool operator==(const consumer_key&, const consumer_key&) = default;
};

struct consumer_write {
    boost::uuids::uuid id;
    std::string name;
    std::string public_key;
    bool is_enabled{};
    std::string description;

    friend bool operator==(const consumer_write&, const consumer_write&) = default;
};

struct consumer_change {
    consumer_write write;
    ores::utility::domain::precondition precondition;

    friend bool operator==(const consumer_change&, const consumer_change&) = default;
};

struct consumer_removal {
    consumer_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;

    friend bool operator==(const consumer_removal&, const consumer_removal&) = default;
};

struct consumer_lookup {
    consumer_key key;
    std::optional<ores::vault::domain::consumer> consumer;

    friend bool operator==(const consumer_lookup&, const consumer_lookup&) = default;
};

struct consumers_filter {
    std::optional<std::vector<boost::uuids::uuid>> id_one_of;
    std::optional<std::string> search;

    friend bool operator==(const consumers_filter&, const consumers_filter&) = default;
};

struct consumer_event {
    boost::uuids::uuid event_id;
    consumer_key key;
    std::string action;
    std::uint32_t version{};
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;

    friend bool operator==(const consumer_event&, const consumer_event&) = default;
};

struct consumer_version_key {
    consumer_key consumer;
    std::uint32_t version{};

    friend bool operator==(const consumer_version_key&, const consumer_version_key&) = default;
};

struct consumer_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;

    friend bool operator==(const consumer_versions_filter&,
                           const consumer_versions_filter&) = default;
};

struct list_consumers_request {
    using response_type = struct list_consumers_response;
    static constexpr std::string_view nats_subject = "vault.v1.consumers.list";
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
    std::optional<consumers_filter> filter;
    std::optional<std::string> as_of;

    friend bool operator==(const list_consumers_request&, const list_consumers_request&) = default;
};

struct list_consumers_response {
    ores::utility::domain::result result;
    std::vector<ores::vault::domain::consumer> consumers;
    std::uint64_t total{};

    friend bool operator==(const list_consumers_response&,
                           const list_consumers_response&) = default;
};

struct get_consumer_request {
    using response_type = struct get_consumer_response;
    static constexpr std::string_view nats_subject = "vault.v1.consumers.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    consumer_key key;

    friend bool operator==(const get_consumer_request&, const get_consumer_request&) = default;
};

struct get_consumer_response {
    ores::utility::domain::result result;
    std::optional<ores::vault::domain::consumer> consumer;

    friend bool operator==(const get_consumer_response&, const get_consumer_response&) = default;
};

struct get_many_consumers_request {
    using response_type = struct get_many_consumers_response;
    static constexpr std::string_view nats_subject = "vault.v1.consumers.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<consumer_key> keys;

    friend bool operator==(const get_many_consumers_request&,
                           const get_many_consumers_request&) = default;
};

struct get_many_consumers_response {
    ores::utility::domain::result result;
    std::vector<consumer_lookup> entries;

    friend bool operator==(const get_many_consumers_response&,
                           const get_many_consumers_response&) = default;
};

struct put_consumer_request {
    using response_type = struct put_consumer_response;
    static constexpr std::string_view nats_subject = "vault.v1.consumers.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    consumer_change change;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const put_consumer_request&, const put_consumer_request&) = default;
};

struct put_consumer_response {
    ores::utility::domain::result result;
    std::optional<ores::vault::domain::consumer> consumer;

    friend bool operator==(const put_consumer_response&, const put_consumer_response&) = default;
};

struct put_many_consumers_request {
    using response_type = struct put_many_consumers_response;
    static constexpr std::string_view nats_subject = "vault.v1.consumers.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<consumer_change> changes;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const put_many_consumers_request&,
                           const put_many_consumers_request&) = default;
};

struct put_many_consumers_response {
    ores::utility::domain::result result;
    std::vector<ores::vault::domain::consumer> consumers;

    friend bool operator==(const put_many_consumers_response&,
                           const put_many_consumers_response&) = default;
};

struct delete_consumer_request {
    using response_type = struct delete_consumer_response;
    static constexpr std::string_view nats_subject = "vault.v1.consumers.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    consumer_removal removal;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const delete_consumer_request&,
                           const delete_consumer_request&) = default;
};

struct delete_consumer_response {
    ores::utility::domain::result result;

    friend bool operator==(const delete_consumer_response&,
                           const delete_consumer_response&) = default;
};

struct delete_many_consumers_request {
    using response_type = struct delete_many_consumers_response;
    static constexpr std::string_view nats_subject = "vault.v1.consumers.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<consumer_removal> removals;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const delete_many_consumers_request&,
                           const delete_many_consumers_request&) = default;
};

struct delete_many_consumers_response {
    ores::utility::domain::result result;

    friend bool operator==(const delete_many_consumers_response&,
                           const delete_many_consumers_response&) = default;
};

struct list_consumer_versions_request {
    using response_type = struct list_consumer_versions_response;
    static constexpr std::string_view nats_subject = "vault.v1.consumers_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    consumer_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<consumer_versions_filter> filter;

    friend bool operator==(const list_consumer_versions_request&,
                           const list_consumer_versions_request&) = default;
};

struct list_consumer_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::vault::domain::consumer> versions;
    std::uint64_t total{};

    friend bool operator==(const list_consumer_versions_response&,
                           const list_consumer_versions_response&) = default;
};

struct get_consumer_version_request {
    using response_type = struct get_consumer_version_response;
    static constexpr std::string_view nats_subject = "vault.v1.consumers_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    consumer_version_key key;

    friend bool operator==(const get_consumer_version_request&,
                           const get_consumer_version_request&) = default;
};

struct get_consumer_version_response {
    ores::utility::domain::result result;
    std::optional<ores::vault::domain::consumer> version;

    friend bool operator==(const get_consumer_version_response&,
                           const get_consumer_version_response&) = default;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace consumer_event_subjects {
inline constexpr std::string_view created = "vault.v1.events.consumers.created";
inline constexpr std::string_view updated = "vault.v1.events.consumers.updated";
inline constexpr std::string_view deleted = "vault.v1.events.consumers.deleted";
}
/**
 * @brief The members of consumer_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<consumer_key>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"name", &consumer_key::name},
    };
}

/**
 * @brief The members of consumer_write, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<consumer_write>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"id", &consumer_write::id},
        utility::codec::field{"name", &consumer_write::name},
        utility::codec::field{"public_key", &consumer_write::public_key},
        utility::codec::field{"is_enabled", &consumer_write::is_enabled},
        utility::codec::field{"description", &consumer_write::description},
    };
}

/**
 * @brief The members of consumer_change, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<consumer_change>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"write", &consumer_change::write},
        utility::codec::field{"precondition", &consumer_change::precondition},
    };
}

/**
 * @brief The members of consumer_removal, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<consumer_removal>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"key", &consumer_removal::key},
        utility::codec::field{"precondition", &consumer_removal::precondition},
    };
}

/**
 * @brief The members of consumer_lookup, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<consumer_lookup>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"key", &consumer_lookup::key},
        utility::codec::field{"consumer", &consumer_lookup::consumer},
    };
}

/**
 * @brief The members of consumers_filter, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<consumers_filter>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"id_one_of", &consumers_filter::id_one_of},
        utility::codec::field{"search", &consumers_filter::search},
    };
}

/**
 * @brief The members of consumer_event, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<consumer_event>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"event_id", &consumer_event::event_id},
        utility::codec::field{"key", &consumer_event::key},
        utility::codec::field{"action", &consumer_event::action},
        utility::codec::field{"version", &consumer_event::version},
        utility::codec::field{"occurred_at", &consumer_event::occurred_at},
        utility::codec::field{"correlation_id", &consumer_event::correlation_id},
    };
}

/**
 * @brief The members of consumer_version_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<consumer_version_key>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"consumer", &consumer_version_key::consumer},
        utility::codec::field{"version", &consumer_version_key::version},
    };
}

/**
 * @brief The members of consumer_versions_filter, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<consumer_versions_filter>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"version", &consumer_versions_filter::version},
        utility::codec::field{"from_version", &consumer_versions_filter::from_version},
        utility::codec::field{"to_version", &consumer_versions_filter::to_version},
    };
}

/**
 * @brief The members of list_consumers_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<list_consumers_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"offset", &list_consumers_request::offset},
        utility::codec::field{"limit", &list_consumers_request::limit},
        utility::codec::field{"order", &list_consumers_request::order},
        utility::codec::field{"filter", &list_consumers_request::filter},
        utility::codec::field{"as_of", &list_consumers_request::as_of},
    };
}

/**
 * @brief The members of list_consumers_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<list_consumers_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &list_consumers_response::result},
        utility::codec::field{"consumers", &list_consumers_response::consumers},
        utility::codec::field{"total", &list_consumers_response::total},
    };
}

/**
 * @brief The members of get_consumer_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_consumer_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"key", &get_consumer_request::key},
    };
}

/**
 * @brief The members of get_consumer_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_consumer_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &get_consumer_response::result},
        utility::codec::field{"consumer", &get_consumer_response::consumer},
    };
}

/**
 * @brief The members of get_many_consumers_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_many_consumers_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"keys", &get_many_consumers_request::keys},
    };
}

/**
 * @brief The members of get_many_consumers_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_many_consumers_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &get_many_consumers_response::result},
        utility::codec::field{"entries", &get_many_consumers_response::entries},
    };
}

/**
 * @brief The members of put_consumer_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<put_consumer_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"change", &put_consumer_request::change},
        utility::codec::field{"intent", &put_consumer_request::intent},
    };
}

/**
 * @brief The members of put_consumer_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<put_consumer_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &put_consumer_response::result},
        utility::codec::field{"consumer", &put_consumer_response::consumer},
    };
}

/**
 * @brief The members of put_many_consumers_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<put_many_consumers_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"changes", &put_many_consumers_request::changes},
        utility::codec::field{"intent", &put_many_consumers_request::intent},
    };
}

/**
 * @brief The members of put_many_consumers_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<put_many_consumers_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &put_many_consumers_response::result},
        utility::codec::field{"consumers", &put_many_consumers_response::consumers},
    };
}

/**
 * @brief The members of delete_consumer_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<delete_consumer_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"removal", &delete_consumer_request::removal},
        utility::codec::field{"intent", &delete_consumer_request::intent},
    };
}

/**
 * @brief The members of delete_consumer_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<delete_consumer_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &delete_consumer_response::result},
    };
}

/**
 * @brief The members of delete_many_consumers_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<delete_many_consumers_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"removals", &delete_many_consumers_request::removals},
        utility::codec::field{"intent", &delete_many_consumers_request::intent},
    };
}

/**
 * @brief The members of delete_many_consumers_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<delete_many_consumers_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &delete_many_consumers_response::result},
    };
}

/**
 * @brief The members of list_consumer_versions_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<list_consumer_versions_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"key", &list_consumer_versions_request::key},
        utility::codec::field{"offset", &list_consumer_versions_request::offset},
        utility::codec::field{"limit", &list_consumer_versions_request::limit},
        utility::codec::field{"order", &list_consumer_versions_request::order},
        utility::codec::field{"filter", &list_consumer_versions_request::filter},
    };
}

/**
 * @brief The members of list_consumer_versions_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<list_consumer_versions_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &list_consumer_versions_response::result},
        utility::codec::field{"versions", &list_consumer_versions_response::versions},
        utility::codec::field{"total", &list_consumer_versions_response::total},
    };
}

/**
 * @brief The members of get_consumer_version_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_consumer_version_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"key", &get_consumer_version_request::key},
    };
}

/**
 * @brief The members of get_consumer_version_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_consumer_version_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &get_consumer_version_response::result},
        utility::codec::field{"version", &get_consumer_version_response::version},
    };
}


}
namespace ores::utility::codec {

template <>
struct ORES_VAULT_API_EXPORT wire_traits<ores::vault::messaging::consumer_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::vault::messaging::consumer_key& v);
    static ores::vault::messaging::consumer_key read_json(std::string_view text);
    static std::string write_msgpack(const ores::vault::messaging::consumer_key& v);
    static ores::vault::messaging::consumer_key read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_VAULT_API_EXPORT wire_traits<ores::vault::messaging::consumer_write> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::vault::messaging::consumer_write& v);
    static ores::vault::messaging::consumer_write read_json(std::string_view text);
    static std::string write_msgpack(const ores::vault::messaging::consumer_write& v);
    static ores::vault::messaging::consumer_write read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_VAULT_API_EXPORT wire_traits<ores::vault::messaging::consumer_change> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::vault::messaging::consumer_change& v);
    static ores::vault::messaging::consumer_change read_json(std::string_view text);
    static std::string write_msgpack(const ores::vault::messaging::consumer_change& v);
    static ores::vault::messaging::consumer_change read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_VAULT_API_EXPORT wire_traits<ores::vault::messaging::consumer_removal> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::vault::messaging::consumer_removal& v);
    static ores::vault::messaging::consumer_removal read_json(std::string_view text);
    static std::string write_msgpack(const ores::vault::messaging::consumer_removal& v);
    static ores::vault::messaging::consumer_removal read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_VAULT_API_EXPORT wire_traits<ores::vault::messaging::consumer_lookup> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::vault::messaging::consumer_lookup& v);
    static ores::vault::messaging::consumer_lookup read_json(std::string_view text);
    static std::string write_msgpack(const ores::vault::messaging::consumer_lookup& v);
    static ores::vault::messaging::consumer_lookup read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_VAULT_API_EXPORT wire_traits<ores::vault::messaging::consumers_filter> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::vault::messaging::consumers_filter& v);
    static ores::vault::messaging::consumers_filter read_json(std::string_view text);
    static std::string write_msgpack(const ores::vault::messaging::consumers_filter& v);
    static ores::vault::messaging::consumers_filter read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_VAULT_API_EXPORT wire_traits<ores::vault::messaging::consumer_event> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::vault::messaging::consumer_event& v);
    static ores::vault::messaging::consumer_event read_json(std::string_view text);
    static std::string write_msgpack(const ores::vault::messaging::consumer_event& v);
    static ores::vault::messaging::consumer_event read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_VAULT_API_EXPORT wire_traits<ores::vault::messaging::consumer_version_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::vault::messaging::consumer_version_key& v);
    static ores::vault::messaging::consumer_version_key read_json(std::string_view text);
    static std::string write_msgpack(const ores::vault::messaging::consumer_version_key& v);
    static ores::vault::messaging::consumer_version_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_VAULT_API_EXPORT wire_traits<ores::vault::messaging::consumer_versions_filter> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::vault::messaging::consumer_versions_filter& v);
    static ores::vault::messaging::consumer_versions_filter read_json(std::string_view text);
    static std::string write_msgpack(const ores::vault::messaging::consumer_versions_filter& v);
    static ores::vault::messaging::consumer_versions_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_VAULT_API_EXPORT wire_traits<ores::vault::messaging::list_consumers_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::vault::messaging::list_consumers_request& v);
    static ores::vault::messaging::list_consumers_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::vault::messaging::list_consumers_request& v);
    static ores::vault::messaging::list_consumers_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_VAULT_API_EXPORT wire_traits<ores::vault::messaging::list_consumers_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::vault::messaging::list_consumers_response& v);
    static ores::vault::messaging::list_consumers_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::vault::messaging::list_consumers_response& v);
    static ores::vault::messaging::list_consumers_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_VAULT_API_EXPORT wire_traits<ores::vault::messaging::get_consumer_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::vault::messaging::get_consumer_request& v);
    static ores::vault::messaging::get_consumer_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::vault::messaging::get_consumer_request& v);
    static ores::vault::messaging::get_consumer_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_VAULT_API_EXPORT wire_traits<ores::vault::messaging::get_consumer_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::vault::messaging::get_consumer_response& v);
    static ores::vault::messaging::get_consumer_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::vault::messaging::get_consumer_response& v);
    static ores::vault::messaging::get_consumer_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_VAULT_API_EXPORT wire_traits<ores::vault::messaging::get_many_consumers_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::vault::messaging::get_many_consumers_request& v);
    static ores::vault::messaging::get_many_consumers_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::vault::messaging::get_many_consumers_request& v);
    static ores::vault::messaging::get_many_consumers_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_VAULT_API_EXPORT wire_traits<ores::vault::messaging::get_many_consumers_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::vault::messaging::get_many_consumers_response& v);
    static ores::vault::messaging::get_many_consumers_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::vault::messaging::get_many_consumers_response& v);
    static ores::vault::messaging::get_many_consumers_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_VAULT_API_EXPORT wire_traits<ores::vault::messaging::put_consumer_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::vault::messaging::put_consumer_request& v);
    static ores::vault::messaging::put_consumer_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::vault::messaging::put_consumer_request& v);
    static ores::vault::messaging::put_consumer_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_VAULT_API_EXPORT wire_traits<ores::vault::messaging::put_consumer_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::vault::messaging::put_consumer_response& v);
    static ores::vault::messaging::put_consumer_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::vault::messaging::put_consumer_response& v);
    static ores::vault::messaging::put_consumer_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_VAULT_API_EXPORT wire_traits<ores::vault::messaging::put_many_consumers_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::vault::messaging::put_many_consumers_request& v);
    static ores::vault::messaging::put_many_consumers_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::vault::messaging::put_many_consumers_request& v);
    static ores::vault::messaging::put_many_consumers_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_VAULT_API_EXPORT wire_traits<ores::vault::messaging::put_many_consumers_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::vault::messaging::put_many_consumers_response& v);
    static ores::vault::messaging::put_many_consumers_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::vault::messaging::put_many_consumers_response& v);
    static ores::vault::messaging::put_many_consumers_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_VAULT_API_EXPORT wire_traits<ores::vault::messaging::delete_consumer_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::vault::messaging::delete_consumer_request& v);
    static ores::vault::messaging::delete_consumer_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::vault::messaging::delete_consumer_request& v);
    static ores::vault::messaging::delete_consumer_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_VAULT_API_EXPORT wire_traits<ores::vault::messaging::delete_consumer_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::vault::messaging::delete_consumer_response& v);
    static ores::vault::messaging::delete_consumer_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::vault::messaging::delete_consumer_response& v);
    static ores::vault::messaging::delete_consumer_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_VAULT_API_EXPORT wire_traits<ores::vault::messaging::delete_many_consumers_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::vault::messaging::delete_many_consumers_request& v);
    static ores::vault::messaging::delete_many_consumers_request read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::vault::messaging::delete_many_consumers_request& v);
    static ores::vault::messaging::delete_many_consumers_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_VAULT_API_EXPORT wire_traits<ores::vault::messaging::delete_many_consumers_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::vault::messaging::delete_many_consumers_response& v);
    static ores::vault::messaging::delete_many_consumers_response read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::vault::messaging::delete_many_consumers_response& v);
    static ores::vault::messaging::delete_many_consumers_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_VAULT_API_EXPORT wire_traits<ores::vault::messaging::list_consumer_versions_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::vault::messaging::list_consumer_versions_request& v);
    static ores::vault::messaging::list_consumer_versions_request read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::vault::messaging::list_consumer_versions_request& v);
    static ores::vault::messaging::list_consumer_versions_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_VAULT_API_EXPORT wire_traits<ores::vault::messaging::list_consumer_versions_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::vault::messaging::list_consumer_versions_response& v);
    static ores::vault::messaging::list_consumer_versions_response read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::vault::messaging::list_consumer_versions_response& v);
    static ores::vault::messaging::list_consumer_versions_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_VAULT_API_EXPORT wire_traits<ores::vault::messaging::get_consumer_version_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::vault::messaging::get_consumer_version_request& v);
    static ores::vault::messaging::get_consumer_version_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::vault::messaging::get_consumer_version_request& v);
    static ores::vault::messaging::get_consumer_version_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_VAULT_API_EXPORT wire_traits<ores::vault::messaging::get_consumer_version_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::vault::messaging::get_consumer_version_response& v);
    static ores::vault::messaging::get_consumer_version_response read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::vault::messaging::get_consumer_version_response& v);
    static ores::vault::messaging::get_consumer_version_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
