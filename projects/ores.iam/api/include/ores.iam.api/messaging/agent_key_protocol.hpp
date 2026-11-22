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
#ifndef ORES_IAM_API_MESSAGING_AGENT_KEY_PROTOCOL_HPP
#define ORES_IAM_API_MESSAGING_AGENT_KEY_PROTOCOL_HPP

#include "ores.iam.api/domain/agent_key_codec.hpp"
#include "ores.iam.api/export.hpp"
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

namespace ores::iam::messaging {

struct agent_key_key {
    boost::uuids::uuid grant_id;

    friend bool operator==(const agent_key_key&, const agent_key_key&) = default;
};

struct agent_key_lookup {
    agent_key_key key;
    std::optional<ores::iam::domain::agent_key> agent_key;

    friend bool operator==(const agent_key_lookup&, const agent_key_lookup&) = default;
};

struct agent_keys_filter {
    std::optional<boost::uuids::uuid> grant_id;
    std::optional<std::vector<boost::uuids::uuid>> id_one_of;
    std::optional<std::vector<boost::uuids::uuid>> grant_id_one_of;

    friend bool operator==(const agent_keys_filter&, const agent_keys_filter&) = default;
};

struct agent_key_event {
    boost::uuids::uuid event_id;
    agent_key_key key;
    std::string action;
    std::uint32_t version{};
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;

    friend bool operator==(const agent_key_event&, const agent_key_event&) = default;
};

struct agent_key_version_key {
    agent_key_key agent_key;
    std::uint32_t version{};

    friend bool operator==(const agent_key_version_key&, const agent_key_version_key&) = default;
};

struct agent_key_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;

    friend bool operator==(const agent_key_versions_filter&,
                           const agent_key_versions_filter&) = default;
};

struct list_agent_keys_request {
    using response_type = struct list_agent_keys_response;
    static constexpr std::string_view nats_subject = "iam.v1.agent_keys.list";
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
    std::optional<agent_keys_filter> filter;
    std::optional<std::string> as_of;

    friend bool operator==(const list_agent_keys_request&,
                           const list_agent_keys_request&) = default;
};

struct list_agent_keys_response {
    ores::utility::domain::result result;
    std::vector<ores::iam::domain::agent_key> keys;
    std::uint64_t total{};

    friend bool operator==(const list_agent_keys_response&,
                           const list_agent_keys_response&) = default;
};

struct get_agent_key_request {
    using response_type = struct get_agent_key_response;
    static constexpr std::string_view nats_subject = "iam.v1.agent_keys.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    agent_key_key key;

    friend bool operator==(const get_agent_key_request&, const get_agent_key_request&) = default;
};

struct get_agent_key_response {
    ores::utility::domain::result result;
    std::optional<ores::iam::domain::agent_key> agent_key;

    friend bool operator==(const get_agent_key_response&, const get_agent_key_response&) = default;
};

struct get_many_agent_keys_request {
    using response_type = struct get_many_agent_keys_response;
    static constexpr std::string_view nats_subject = "iam.v1.agent_keys.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<agent_key_key> keys;

    friend bool operator==(const get_many_agent_keys_request&,
                           const get_many_agent_keys_request&) = default;
};

struct get_many_agent_keys_response {
    ores::utility::domain::result result;
    std::vector<agent_key_lookup> entries;

    friend bool operator==(const get_many_agent_keys_response&,
                           const get_many_agent_keys_response&) = default;
};

struct list_by_grant_id_agent_keys_request {
    using response_type = struct list_by_grant_id_agent_keys_response;
    static constexpr std::string_view nats_subject = "iam.v1.agent_keys.list_by_grant_id";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    boost::uuids::uuid grant_id;
    ores::utility::domain::scope scope = ores::utility::domain::scope::direct;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<agent_keys_filter> filter;

    friend bool operator==(const list_by_grant_id_agent_keys_request&,
                           const list_by_grant_id_agent_keys_request&) = default;
};

struct list_by_grant_id_agent_keys_response {
    ores::utility::domain::result result;
    std::vector<ores::iam::domain::agent_key> keys;
    std::uint64_t total{};

    friend bool operator==(const list_by_grant_id_agent_keys_response&,
                           const list_by_grant_id_agent_keys_response&) = default;
};

struct list_agent_key_versions_request {
    using response_type = struct list_agent_key_versions_response;
    static constexpr std::string_view nats_subject = "iam.v1.agent_keys_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    agent_key_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<agent_key_versions_filter> filter;

    friend bool operator==(const list_agent_key_versions_request&,
                           const list_agent_key_versions_request&) = default;
};

struct list_agent_key_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::iam::domain::agent_key> versions;
    std::uint64_t total{};

    friend bool operator==(const list_agent_key_versions_response&,
                           const list_agent_key_versions_response&) = default;
};

struct get_agent_key_version_request {
    using response_type = struct get_agent_key_version_response;
    static constexpr std::string_view nats_subject = "iam.v1.agent_keys_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    agent_key_version_key key;

    friend bool operator==(const get_agent_key_version_request&,
                           const get_agent_key_version_request&) = default;
};

struct get_agent_key_version_response {
    ores::utility::domain::result result;
    std::optional<ores::iam::domain::agent_key> version;

    friend bool operator==(const get_agent_key_version_response&,
                           const get_agent_key_version_response&) = default;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace agent_key_event_subjects {
inline constexpr std::string_view created = "iam.v1.events.agent_keys.created";
inline constexpr std::string_view updated = "iam.v1.events.agent_keys.updated";
inline constexpr std::string_view deleted = "iam.v1.events.agent_keys.deleted";
}
/**
 * @brief The members of agent_key_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<agent_key_key>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"grant_id", &agent_key_key::grant_id},
    };
}

/**
 * @brief The members of agent_key_lookup, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<agent_key_lookup>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"key", &agent_key_lookup::key},
        utility::codec::field{"agent_key", &agent_key_lookup::agent_key},
    };
}

/**
 * @brief The members of agent_keys_filter, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<agent_keys_filter>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"grant_id", &agent_keys_filter::grant_id},
        utility::codec::field{"id_one_of", &agent_keys_filter::id_one_of},
        utility::codec::field{"grant_id_one_of", &agent_keys_filter::grant_id_one_of},
    };
}

/**
 * @brief The members of agent_key_event, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<agent_key_event>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"event_id", &agent_key_event::event_id},
        utility::codec::field{"key", &agent_key_event::key},
        utility::codec::field{"action", &agent_key_event::action},
        utility::codec::field{"version", &agent_key_event::version},
        utility::codec::field{"occurred_at", &agent_key_event::occurred_at},
        utility::codec::field{"correlation_id", &agent_key_event::correlation_id},
    };
}

/**
 * @brief The members of agent_key_version_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<agent_key_version_key>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"agent_key", &agent_key_version_key::agent_key},
        utility::codec::field{"version", &agent_key_version_key::version},
    };
}

/**
 * @brief The members of agent_key_versions_filter, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<agent_key_versions_filter>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"version", &agent_key_versions_filter::version},
        utility::codec::field{"from_version", &agent_key_versions_filter::from_version},
        utility::codec::field{"to_version", &agent_key_versions_filter::to_version},
    };
}

/**
 * @brief The members of list_agent_keys_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<list_agent_keys_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"offset", &list_agent_keys_request::offset},
        utility::codec::field{"limit", &list_agent_keys_request::limit},
        utility::codec::field{"order", &list_agent_keys_request::order},
        utility::codec::field{"filter", &list_agent_keys_request::filter},
        utility::codec::field{"as_of", &list_agent_keys_request::as_of},
    };
}

/**
 * @brief The members of list_agent_keys_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<list_agent_keys_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &list_agent_keys_response::result},
        utility::codec::field{"keys", &list_agent_keys_response::keys},
        utility::codec::field{"total", &list_agent_keys_response::total},
    };
}

/**
 * @brief The members of get_agent_key_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_agent_key_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"key", &get_agent_key_request::key},
    };
}

/**
 * @brief The members of get_agent_key_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_agent_key_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &get_agent_key_response::result},
        utility::codec::field{"agent_key", &get_agent_key_response::agent_key},
    };
}

/**
 * @brief The members of get_many_agent_keys_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_many_agent_keys_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"keys", &get_many_agent_keys_request::keys},
    };
}

/**
 * @brief The members of get_many_agent_keys_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_many_agent_keys_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &get_many_agent_keys_response::result},
        utility::codec::field{"entries", &get_many_agent_keys_response::entries},
    };
}

/**
 * @brief The members of list_by_grant_id_agent_keys_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<list_by_grant_id_agent_keys_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"grant_id", &list_by_grant_id_agent_keys_request::grant_id},
        utility::codec::field{"scope", &list_by_grant_id_agent_keys_request::scope},
        utility::codec::field{"offset", &list_by_grant_id_agent_keys_request::offset},
        utility::codec::field{"limit", &list_by_grant_id_agent_keys_request::limit},
        utility::codec::field{"order", &list_by_grant_id_agent_keys_request::order},
        utility::codec::field{"filter", &list_by_grant_id_agent_keys_request::filter},
    };
}

/**
 * @brief The members of list_by_grant_id_agent_keys_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<list_by_grant_id_agent_keys_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &list_by_grant_id_agent_keys_response::result},
        utility::codec::field{"keys", &list_by_grant_id_agent_keys_response::keys},
        utility::codec::field{"total", &list_by_grant_id_agent_keys_response::total},
    };
}

/**
 * @brief The members of list_agent_key_versions_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<list_agent_key_versions_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"key", &list_agent_key_versions_request::key},
        utility::codec::field{"offset", &list_agent_key_versions_request::offset},
        utility::codec::field{"limit", &list_agent_key_versions_request::limit},
        utility::codec::field{"order", &list_agent_key_versions_request::order},
        utility::codec::field{"filter", &list_agent_key_versions_request::filter},
    };
}

/**
 * @brief The members of list_agent_key_versions_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<list_agent_key_versions_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &list_agent_key_versions_response::result},
        utility::codec::field{"versions", &list_agent_key_versions_response::versions},
        utility::codec::field{"total", &list_agent_key_versions_response::total},
    };
}

/**
 * @brief The members of get_agent_key_version_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_agent_key_version_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"key", &get_agent_key_version_request::key},
    };
}

/**
 * @brief The members of get_agent_key_version_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_agent_key_version_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &get_agent_key_version_response::result},
        utility::codec::field{"version", &get_agent_key_version_response::version},
    };
}


}
namespace ores::utility::codec {

template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::agent_key_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::agent_key_key& v);
    static ores::iam::messaging::agent_key_key read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::agent_key_key& v);
    static ores::iam::messaging::agent_key_key read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::agent_key_lookup> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::agent_key_lookup& v);
    static ores::iam::messaging::agent_key_lookup read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::agent_key_lookup& v);
    static ores::iam::messaging::agent_key_lookup read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::agent_keys_filter> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::agent_keys_filter& v);
    static ores::iam::messaging::agent_keys_filter read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::agent_keys_filter& v);
    static ores::iam::messaging::agent_keys_filter read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::agent_key_event> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::agent_key_event& v);
    static ores::iam::messaging::agent_key_event read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::agent_key_event& v);
    static ores::iam::messaging::agent_key_event read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::agent_key_version_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::agent_key_version_key& v);
    static ores::iam::messaging::agent_key_version_key read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::agent_key_version_key& v);
    static ores::iam::messaging::agent_key_version_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::agent_key_versions_filter> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::agent_key_versions_filter& v);
    static ores::iam::messaging::agent_key_versions_filter read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::agent_key_versions_filter& v);
    static ores::iam::messaging::agent_key_versions_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::list_agent_keys_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::list_agent_keys_request& v);
    static ores::iam::messaging::list_agent_keys_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::list_agent_keys_request& v);
    static ores::iam::messaging::list_agent_keys_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::list_agent_keys_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::list_agent_keys_response& v);
    static ores::iam::messaging::list_agent_keys_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::list_agent_keys_response& v);
    static ores::iam::messaging::list_agent_keys_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::get_agent_key_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::get_agent_key_request& v);
    static ores::iam::messaging::get_agent_key_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::get_agent_key_request& v);
    static ores::iam::messaging::get_agent_key_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::get_agent_key_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::get_agent_key_response& v);
    static ores::iam::messaging::get_agent_key_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::get_agent_key_response& v);
    static ores::iam::messaging::get_agent_key_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::get_many_agent_keys_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::get_many_agent_keys_request& v);
    static ores::iam::messaging::get_many_agent_keys_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::get_many_agent_keys_request& v);
    static ores::iam::messaging::get_many_agent_keys_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::get_many_agent_keys_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::get_many_agent_keys_response& v);
    static ores::iam::messaging::get_many_agent_keys_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::get_many_agent_keys_response& v);
    static ores::iam::messaging::get_many_agent_keys_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::list_by_grant_id_agent_keys_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::iam::messaging::list_by_grant_id_agent_keys_request& v);
    static ores::iam::messaging::list_by_grant_id_agent_keys_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::iam::messaging::list_by_grant_id_agent_keys_request& v);
    static ores::iam::messaging::list_by_grant_id_agent_keys_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::list_by_grant_id_agent_keys_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::iam::messaging::list_by_grant_id_agent_keys_response& v);
    static ores::iam::messaging::list_by_grant_id_agent_keys_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::iam::messaging::list_by_grant_id_agent_keys_response& v);
    static ores::iam::messaging::list_by_grant_id_agent_keys_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::list_agent_key_versions_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::list_agent_key_versions_request& v);
    static ores::iam::messaging::list_agent_key_versions_request read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::iam::messaging::list_agent_key_versions_request& v);
    static ores::iam::messaging::list_agent_key_versions_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::list_agent_key_versions_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::list_agent_key_versions_response& v);
    static ores::iam::messaging::list_agent_key_versions_response read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::iam::messaging::list_agent_key_versions_response& v);
    static ores::iam::messaging::list_agent_key_versions_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::get_agent_key_version_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::get_agent_key_version_request& v);
    static ores::iam::messaging::get_agent_key_version_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::get_agent_key_version_request& v);
    static ores::iam::messaging::get_agent_key_version_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::get_agent_key_version_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::get_agent_key_version_response& v);
    static ores::iam::messaging::get_agent_key_version_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::get_agent_key_version_response& v);
    static ores::iam::messaging::get_agent_key_version_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
