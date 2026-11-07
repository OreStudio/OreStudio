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
#ifndef ORES_IAM_API_MESSAGING_RUN_GRANT_PROTOCOL_HPP
#define ORES_IAM_API_MESSAGING_RUN_GRANT_PROTOCOL_HPP

#include "ores.iam.api/domain/run_grant_codec.hpp"
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

struct run_grant_key {
    std::string resource;

    friend bool operator==(const run_grant_key&, const run_grant_key&) = default;
};

struct run_grant_lookup {
    run_grant_key key;
    std::optional<ores::iam::domain::run_grant> run_grant;

    friend bool operator==(const run_grant_lookup&, const run_grant_lookup&) = default;
};

struct run_grants_filter {
    std::optional<boost::uuids::uuid> grantor_account_id;
    std::optional<std::vector<boost::uuids::uuid>> id_one_of;
    std::optional<std::vector<boost::uuids::uuid>> grantor_account_id_one_of;

    friend bool operator==(const run_grants_filter&, const run_grants_filter&) = default;
};

struct run_grant_event {
    boost::uuids::uuid event_id;
    run_grant_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;

    friend bool operator==(const run_grant_event&, const run_grant_event&) = default;
};

struct run_grant_version_key {
    run_grant_key run_grant;
    std::uint32_t version;

    friend bool operator==(const run_grant_version_key&, const run_grant_version_key&) = default;
};

struct run_grant_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;

    friend bool operator==(const run_grant_versions_filter&,
                           const run_grant_versions_filter&) = default;
};

struct list_run_grants_request {
    using response_type = struct list_run_grants_response;
    static constexpr std::string_view nats_subject = "iam.v1.run_grants.list";
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
    std::optional<run_grants_filter> filter;
    std::optional<std::string> as_of;

    friend bool operator==(const list_run_grants_request&,
                           const list_run_grants_request&) = default;
};

struct list_run_grants_response {
    ores::utility::domain::result result;
    std::vector<ores::iam::domain::run_grant> grants;
    std::uint64_t total;

    friend bool operator==(const list_run_grants_response&,
                           const list_run_grants_response&) = default;
};

struct get_run_grant_request {
    using response_type = struct get_run_grant_response;
    static constexpr std::string_view nats_subject = "iam.v1.run_grants.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    run_grant_key key;

    friend bool operator==(const get_run_grant_request&, const get_run_grant_request&) = default;
};

struct get_run_grant_response {
    ores::utility::domain::result result;
    std::optional<ores::iam::domain::run_grant> run_grant;

    friend bool operator==(const get_run_grant_response&, const get_run_grant_response&) = default;
};

struct get_many_run_grants_request {
    using response_type = struct get_many_run_grants_response;
    static constexpr std::string_view nats_subject = "iam.v1.run_grants.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<run_grant_key> keys;

    friend bool operator==(const get_many_run_grants_request&,
                           const get_many_run_grants_request&) = default;
};

struct get_many_run_grants_response {
    ores::utility::domain::result result;
    std::vector<run_grant_lookup> entries;

    friend bool operator==(const get_many_run_grants_response&,
                           const get_many_run_grants_response&) = default;
};

struct list_by_grantor_account_id_run_grants_request {
    using response_type = struct list_by_grantor_account_id_run_grants_response;
    static constexpr std::string_view nats_subject = "iam.v1.run_grants.list_by_grantor_account_id";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    boost::uuids::uuid grantor_account_id;
    ores::utility::domain::scope scope = ores::utility::domain::scope::direct;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<run_grants_filter> filter;

    friend bool operator==(const list_by_grantor_account_id_run_grants_request&,
                           const list_by_grantor_account_id_run_grants_request&) = default;
};

struct list_by_grantor_account_id_run_grants_response {
    ores::utility::domain::result result;
    std::vector<ores::iam::domain::run_grant> grants;
    std::uint64_t total;

    friend bool operator==(const list_by_grantor_account_id_run_grants_response&,
                           const list_by_grantor_account_id_run_grants_response&) = default;
};

struct list_run_grant_versions_request {
    using response_type = struct list_run_grant_versions_response;
    static constexpr std::string_view nats_subject = "iam.v1.run_grants_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    run_grant_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<run_grant_versions_filter> filter;

    friend bool operator==(const list_run_grant_versions_request&,
                           const list_run_grant_versions_request&) = default;
};

struct list_run_grant_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::iam::domain::run_grant> versions;
    std::uint64_t total;

    friend bool operator==(const list_run_grant_versions_response&,
                           const list_run_grant_versions_response&) = default;
};

struct get_run_grant_version_request {
    using response_type = struct get_run_grant_version_response;
    static constexpr std::string_view nats_subject = "iam.v1.run_grants_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    run_grant_version_key key;

    friend bool operator==(const get_run_grant_version_request&,
                           const get_run_grant_version_request&) = default;
};

struct get_run_grant_version_response {
    ores::utility::domain::result result;
    std::optional<ores::iam::domain::run_grant> version;

    friend bool operator==(const get_run_grant_version_response&,
                           const get_run_grant_version_response&) = default;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace run_grant_event_subjects {
inline constexpr std::string_view created = "iam.v1.events.run_grants.created";
inline constexpr std::string_view updated = "iam.v1.events.run_grants.updated";
inline constexpr std::string_view deleted = "iam.v1.events.run_grants.deleted";
}
/**
 * @brief The members of run_grant_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<run_grant_key>) {
    return std::tuple{
        utility::codec::field{"resource", &run_grant_key::resource},
    };
}

/**
 * @brief The members of run_grant_lookup, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<run_grant_lookup>) {
    return std::tuple{
        utility::codec::field{"key", &run_grant_lookup::key},
        utility::codec::field{"run_grant", &run_grant_lookup::run_grant},
    };
}

/**
 * @brief The members of run_grants_filter, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<run_grants_filter>) {
    return std::tuple{
        utility::codec::field{"grantor_account_id", &run_grants_filter::grantor_account_id},
        utility::codec::field{"id_one_of", &run_grants_filter::id_one_of},
        utility::codec::field{"grantor_account_id_one_of",
                              &run_grants_filter::grantor_account_id_one_of},
    };
}

/**
 * @brief The members of run_grant_event, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<run_grant_event>) {
    return std::tuple{
        utility::codec::field{"event_id", &run_grant_event::event_id},
        utility::codec::field{"key", &run_grant_event::key},
        utility::codec::field{"action", &run_grant_event::action},
        utility::codec::field{"version", &run_grant_event::version},
        utility::codec::field{"occurred_at", &run_grant_event::occurred_at},
        utility::codec::field{"correlation_id", &run_grant_event::correlation_id},
    };
}

/**
 * @brief The members of run_grant_version_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<run_grant_version_key>) {
    return std::tuple{
        utility::codec::field{"run_grant", &run_grant_version_key::run_grant},
        utility::codec::field{"version", &run_grant_version_key::version},
    };
}

/**
 * @brief The members of run_grant_versions_filter, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<run_grant_versions_filter>) {
    return std::tuple{
        utility::codec::field{"version", &run_grant_versions_filter::version},
        utility::codec::field{"from_version", &run_grant_versions_filter::from_version},
        utility::codec::field{"to_version", &run_grant_versions_filter::to_version},
    };
}

/**
 * @brief The members of list_run_grants_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<list_run_grants_request>) {
    return std::tuple{
        utility::codec::field{"offset", &list_run_grants_request::offset},
        utility::codec::field{"limit", &list_run_grants_request::limit},
        utility::codec::field{"order", &list_run_grants_request::order},
        utility::codec::field{"filter", &list_run_grants_request::filter},
        utility::codec::field{"as_of", &list_run_grants_request::as_of},
    };
}

/**
 * @brief The members of list_run_grants_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<list_run_grants_response>) {
    return std::tuple{
        utility::codec::field{"result", &list_run_grants_response::result},
        utility::codec::field{"grants", &list_run_grants_response::grants},
        utility::codec::field{"total", &list_run_grants_response::total},
    };
}

/**
 * @brief The members of get_run_grant_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_run_grant_request>) {
    return std::tuple{
        utility::codec::field{"key", &get_run_grant_request::key},
    };
}

/**
 * @brief The members of get_run_grant_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_run_grant_response>) {
    return std::tuple{
        utility::codec::field{"result", &get_run_grant_response::result},
        utility::codec::field{"run_grant", &get_run_grant_response::run_grant},
    };
}

/**
 * @brief The members of get_many_run_grants_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_many_run_grants_request>) {
    return std::tuple{
        utility::codec::field{"keys", &get_many_run_grants_request::keys},
    };
}

/**
 * @brief The members of get_many_run_grants_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_many_run_grants_response>) {
    return std::tuple{
        utility::codec::field{"result", &get_many_run_grants_response::result},
        utility::codec::field{"entries", &get_many_run_grants_response::entries},
    };
}

/**
 * @brief The members of list_by_grantor_account_id_run_grants_request, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<list_by_grantor_account_id_run_grants_request>) {
    return std::tuple{
        utility::codec::field{"grantor_account_id",
                              &list_by_grantor_account_id_run_grants_request::grantor_account_id},
        utility::codec::field{"scope", &list_by_grantor_account_id_run_grants_request::scope},
        utility::codec::field{"offset", &list_by_grantor_account_id_run_grants_request::offset},
        utility::codec::field{"limit", &list_by_grantor_account_id_run_grants_request::limit},
        utility::codec::field{"order", &list_by_grantor_account_id_run_grants_request::order},
        utility::codec::field{"filter", &list_by_grantor_account_id_run_grants_request::filter},
    };
}

/**
 * @brief The members of list_by_grantor_account_id_run_grants_response, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<list_by_grantor_account_id_run_grants_response>) {
    return std::tuple{
        utility::codec::field{"result", &list_by_grantor_account_id_run_grants_response::result},
        utility::codec::field{"grants", &list_by_grantor_account_id_run_grants_response::grants},
        utility::codec::field{"total", &list_by_grantor_account_id_run_grants_response::total},
    };
}

/**
 * @brief The members of list_run_grant_versions_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<list_run_grant_versions_request>) {
    return std::tuple{
        utility::codec::field{"key", &list_run_grant_versions_request::key},
        utility::codec::field{"offset", &list_run_grant_versions_request::offset},
        utility::codec::field{"limit", &list_run_grant_versions_request::limit},
        utility::codec::field{"order", &list_run_grant_versions_request::order},
        utility::codec::field{"filter", &list_run_grant_versions_request::filter},
    };
}

/**
 * @brief The members of list_run_grant_versions_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<list_run_grant_versions_response>) {
    return std::tuple{
        utility::codec::field{"result", &list_run_grant_versions_response::result},
        utility::codec::field{"versions", &list_run_grant_versions_response::versions},
        utility::codec::field{"total", &list_run_grant_versions_response::total},
    };
}

/**
 * @brief The members of get_run_grant_version_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_run_grant_version_request>) {
    return std::tuple{
        utility::codec::field{"key", &get_run_grant_version_request::key},
    };
}

/**
 * @brief The members of get_run_grant_version_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_run_grant_version_response>) {
    return std::tuple{
        utility::codec::field{"result", &get_run_grant_version_response::result},
        utility::codec::field{"version", &get_run_grant_version_response::version},
    };
}


}
namespace ores::utility::codec {

template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::run_grant_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::run_grant_key& v);
    static ores::iam::messaging::run_grant_key read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::run_grant_key& v);
    static ores::iam::messaging::run_grant_key read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::run_grant_lookup> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::run_grant_lookup& v);
    static ores::iam::messaging::run_grant_lookup read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::run_grant_lookup& v);
    static ores::iam::messaging::run_grant_lookup read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::run_grants_filter> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::run_grants_filter& v);
    static ores::iam::messaging::run_grants_filter read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::run_grants_filter& v);
    static ores::iam::messaging::run_grants_filter read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::run_grant_event> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::run_grant_event& v);
    static ores::iam::messaging::run_grant_event read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::run_grant_event& v);
    static ores::iam::messaging::run_grant_event read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::run_grant_version_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::run_grant_version_key& v);
    static ores::iam::messaging::run_grant_version_key read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::run_grant_version_key& v);
    static ores::iam::messaging::run_grant_version_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::run_grant_versions_filter> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::run_grant_versions_filter& v);
    static ores::iam::messaging::run_grant_versions_filter read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::run_grant_versions_filter& v);
    static ores::iam::messaging::run_grant_versions_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::list_run_grants_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::list_run_grants_request& v);
    static ores::iam::messaging::list_run_grants_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::list_run_grants_request& v);
    static ores::iam::messaging::list_run_grants_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::list_run_grants_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::list_run_grants_response& v);
    static ores::iam::messaging::list_run_grants_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::list_run_grants_response& v);
    static ores::iam::messaging::list_run_grants_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::get_run_grant_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::get_run_grant_request& v);
    static ores::iam::messaging::get_run_grant_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::get_run_grant_request& v);
    static ores::iam::messaging::get_run_grant_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::get_run_grant_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::get_run_grant_response& v);
    static ores::iam::messaging::get_run_grant_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::get_run_grant_response& v);
    static ores::iam::messaging::get_run_grant_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::get_many_run_grants_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::get_many_run_grants_request& v);
    static ores::iam::messaging::get_many_run_grants_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::get_many_run_grants_request& v);
    static ores::iam::messaging::get_many_run_grants_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::get_many_run_grants_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::get_many_run_grants_response& v);
    static ores::iam::messaging::get_many_run_grants_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::get_many_run_grants_response& v);
    static ores::iam::messaging::get_many_run_grants_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT
    wire_traits<ores::iam::messaging::list_by_grantor_account_id_run_grants_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::iam::messaging::list_by_grantor_account_id_run_grants_request& v);
    static ores::iam::messaging::list_by_grantor_account_id_run_grants_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::iam::messaging::list_by_grantor_account_id_run_grants_request& v);
    static ores::iam::messaging::list_by_grantor_account_id_run_grants_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT
    wire_traits<ores::iam::messaging::list_by_grantor_account_id_run_grants_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::iam::messaging::list_by_grantor_account_id_run_grants_response& v);
    static ores::iam::messaging::list_by_grantor_account_id_run_grants_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::iam::messaging::list_by_grantor_account_id_run_grants_response& v);
    static ores::iam::messaging::list_by_grantor_account_id_run_grants_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::list_run_grant_versions_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::list_run_grant_versions_request& v);
    static ores::iam::messaging::list_run_grant_versions_request read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::iam::messaging::list_run_grant_versions_request& v);
    static ores::iam::messaging::list_run_grant_versions_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::list_run_grant_versions_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::list_run_grant_versions_response& v);
    static ores::iam::messaging::list_run_grant_versions_response read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::iam::messaging::list_run_grant_versions_response& v);
    static ores::iam::messaging::list_run_grant_versions_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::get_run_grant_version_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::get_run_grant_version_request& v);
    static ores::iam::messaging::get_run_grant_version_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::get_run_grant_version_request& v);
    static ores::iam::messaging::get_run_grant_version_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::get_run_grant_version_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::get_run_grant_version_response& v);
    static ores::iam::messaging::get_run_grant_version_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::get_run_grant_version_response& v);
    static ores::iam::messaging::get_run_grant_version_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
