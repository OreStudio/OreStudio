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
#ifndef ORES_IAM_API_MESSAGING_ACCOUNT_PROTOCOL_HPP
#define ORES_IAM_API_MESSAGING_ACCOUNT_PROTOCOL_HPP

#include "ores.iam.api/domain/account_codec.hpp"
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

struct account_key {
    std::string username;

    friend bool operator==(const account_key&, const account_key&) = default;
};

struct account_lookup {
    account_key key;
    std::optional<ores::iam::domain::account> account;

    friend bool operator==(const account_lookup&, const account_lookup&) = default;
};

struct accounts_filter {
    std::optional<std::vector<boost::uuids::uuid>> id_one_of;
    std::optional<std::string> search;

    friend bool operator==(const accounts_filter&, const accounts_filter&) = default;
};

struct account_event {
    boost::uuids::uuid event_id;
    account_key key;
    std::string action;
    std::uint32_t version{};
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;

    friend bool operator==(const account_event&, const account_event&) = default;
};

struct account_version_key {
    account_key account;
    std::uint32_t version{};

    friend bool operator==(const account_version_key&, const account_version_key&) = default;
};

struct account_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;

    friend bool operator==(const account_versions_filter&,
                           const account_versions_filter&) = default;
};

struct list_accounts_request {
    using response_type = struct list_accounts_response;
    static constexpr std::string_view nats_subject = "iam.v1.accounts.list";
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
    std::optional<accounts_filter> filter;
    std::optional<std::string> as_of;

    friend bool operator==(const list_accounts_request&, const list_accounts_request&) = default;
};

struct list_accounts_response {
    ores::utility::domain::result result;
    std::vector<ores::iam::domain::account> accounts;
    std::uint64_t total{};

    friend bool operator==(const list_accounts_response&, const list_accounts_response&) = default;
};

struct get_account_request {
    using response_type = struct get_account_response;
    static constexpr std::string_view nats_subject = "iam.v1.accounts.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    account_key key;

    friend bool operator==(const get_account_request&, const get_account_request&) = default;
};

struct get_account_response {
    ores::utility::domain::result result;
    std::optional<ores::iam::domain::account> account;

    friend bool operator==(const get_account_response&, const get_account_response&) = default;
};

struct get_many_accounts_request {
    using response_type = struct get_many_accounts_response;
    static constexpr std::string_view nats_subject = "iam.v1.accounts.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<account_key> keys;

    friend bool operator==(const get_many_accounts_request&,
                           const get_many_accounts_request&) = default;
};

struct get_many_accounts_response {
    ores::utility::domain::result result;
    std::vector<account_lookup> entries;

    friend bool operator==(const get_many_accounts_response&,
                           const get_many_accounts_response&) = default;
};

struct list_account_versions_request {
    using response_type = struct list_account_versions_response;
    static constexpr std::string_view nats_subject = "iam.v1.accounts_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    account_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<account_versions_filter> filter;

    friend bool operator==(const list_account_versions_request&,
                           const list_account_versions_request&) = default;
};

struct list_account_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::iam::domain::account> versions;
    std::uint64_t total{};

    friend bool operator==(const list_account_versions_response&,
                           const list_account_versions_response&) = default;
};

struct get_account_version_request {
    using response_type = struct get_account_version_response;
    static constexpr std::string_view nats_subject = "iam.v1.accounts_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    account_version_key key;

    friend bool operator==(const get_account_version_request&,
                           const get_account_version_request&) = default;
};

struct get_account_version_response {
    ores::utility::domain::result result;
    std::optional<ores::iam::domain::account> version;

    friend bool operator==(const get_account_version_response&,
                           const get_account_version_response&) = default;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace account_event_subjects {
inline constexpr std::string_view created = "iam.v1.events.accounts.created";
inline constexpr std::string_view updated = "iam.v1.events.accounts.updated";
inline constexpr std::string_view deleted = "iam.v1.events.accounts.deleted";
}
/**
 * @brief The members of account_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<account_key>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"username", &account_key::username},
    };
}

/**
 * @brief The members of account_lookup, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<account_lookup>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"key", &account_lookup::key},
        utility::codec::field{"account", &account_lookup::account},
    };
}

/**
 * @brief The members of accounts_filter, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<accounts_filter>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"id_one_of", &accounts_filter::id_one_of},
        utility::codec::field{"search", &accounts_filter::search},
    };
}

/**
 * @brief The members of account_event, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<account_event>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"event_id", &account_event::event_id},
        utility::codec::field{"key", &account_event::key},
        utility::codec::field{"action", &account_event::action},
        utility::codec::field{"version", &account_event::version},
        utility::codec::field{"occurred_at", &account_event::occurred_at},
        utility::codec::field{"correlation_id", &account_event::correlation_id},
    };
}

/**
 * @brief The members of account_version_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<account_version_key>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"account", &account_version_key::account},
        utility::codec::field{"version", &account_version_key::version},
    };
}

/**
 * @brief The members of account_versions_filter, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<account_versions_filter>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"version", &account_versions_filter::version},
        utility::codec::field{"from_version", &account_versions_filter::from_version},
        utility::codec::field{"to_version", &account_versions_filter::to_version},
    };
}

/**
 * @brief The members of list_accounts_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<list_accounts_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"offset", &list_accounts_request::offset},
        utility::codec::field{"limit", &list_accounts_request::limit},
        utility::codec::field{"order", &list_accounts_request::order},
        utility::codec::field{"filter", &list_accounts_request::filter},
        utility::codec::field{"as_of", &list_accounts_request::as_of},
    };
}

/**
 * @brief The members of list_accounts_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<list_accounts_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &list_accounts_response::result},
        utility::codec::field{"accounts", &list_accounts_response::accounts},
        utility::codec::field{"total", &list_accounts_response::total},
    };
}

/**
 * @brief The members of get_account_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_account_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"key", &get_account_request::key},
    };
}

/**
 * @brief The members of get_account_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_account_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &get_account_response::result},
        utility::codec::field{"account", &get_account_response::account},
    };
}

/**
 * @brief The members of get_many_accounts_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_many_accounts_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"keys", &get_many_accounts_request::keys},
    };
}

/**
 * @brief The members of get_many_accounts_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_many_accounts_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &get_many_accounts_response::result},
        utility::codec::field{"entries", &get_many_accounts_response::entries},
    };
}

/**
 * @brief The members of list_account_versions_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<list_account_versions_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"key", &list_account_versions_request::key},
        utility::codec::field{"offset", &list_account_versions_request::offset},
        utility::codec::field{"limit", &list_account_versions_request::limit},
        utility::codec::field{"order", &list_account_versions_request::order},
        utility::codec::field{"filter", &list_account_versions_request::filter},
    };
}

/**
 * @brief The members of list_account_versions_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<list_account_versions_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &list_account_versions_response::result},
        utility::codec::field{"versions", &list_account_versions_response::versions},
        utility::codec::field{"total", &list_account_versions_response::total},
    };
}

/**
 * @brief The members of get_account_version_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_account_version_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"key", &get_account_version_request::key},
    };
}

/**
 * @brief The members of get_account_version_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_account_version_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &get_account_version_response::result},
        utility::codec::field{"version", &get_account_version_response::version},
    };
}


}
namespace ores::utility::codec {

template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::account_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::account_key& v);
    static ores::iam::messaging::account_key read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::account_key& v);
    static ores::iam::messaging::account_key read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::account_lookup> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::account_lookup& v);
    static ores::iam::messaging::account_lookup read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::account_lookup& v);
    static ores::iam::messaging::account_lookup read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::accounts_filter> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::accounts_filter& v);
    static ores::iam::messaging::accounts_filter read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::accounts_filter& v);
    static ores::iam::messaging::accounts_filter read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::account_event> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::account_event& v);
    static ores::iam::messaging::account_event read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::account_event& v);
    static ores::iam::messaging::account_event read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::account_version_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::account_version_key& v);
    static ores::iam::messaging::account_version_key read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::account_version_key& v);
    static ores::iam::messaging::account_version_key read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::account_versions_filter> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::account_versions_filter& v);
    static ores::iam::messaging::account_versions_filter read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::account_versions_filter& v);
    static ores::iam::messaging::account_versions_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::list_accounts_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::list_accounts_request& v);
    static ores::iam::messaging::list_accounts_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::list_accounts_request& v);
    static ores::iam::messaging::list_accounts_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::list_accounts_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::list_accounts_response& v);
    static ores::iam::messaging::list_accounts_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::list_accounts_response& v);
    static ores::iam::messaging::list_accounts_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::get_account_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::get_account_request& v);
    static ores::iam::messaging::get_account_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::get_account_request& v);
    static ores::iam::messaging::get_account_request read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::get_account_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::get_account_response& v);
    static ores::iam::messaging::get_account_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::get_account_response& v);
    static ores::iam::messaging::get_account_response read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::get_many_accounts_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::get_many_accounts_request& v);
    static ores::iam::messaging::get_many_accounts_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::get_many_accounts_request& v);
    static ores::iam::messaging::get_many_accounts_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::get_many_accounts_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::get_many_accounts_response& v);
    static ores::iam::messaging::get_many_accounts_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::get_many_accounts_response& v);
    static ores::iam::messaging::get_many_accounts_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::list_account_versions_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::list_account_versions_request& v);
    static ores::iam::messaging::list_account_versions_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::list_account_versions_request& v);
    static ores::iam::messaging::list_account_versions_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::list_account_versions_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::list_account_versions_response& v);
    static ores::iam::messaging::list_account_versions_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::list_account_versions_response& v);
    static ores::iam::messaging::list_account_versions_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::get_account_version_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::get_account_version_request& v);
    static ores::iam::messaging::get_account_version_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::get_account_version_request& v);
    static ores::iam::messaging::get_account_version_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::get_account_version_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::get_account_version_response& v);
    static ores::iam::messaging::get_account_version_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::get_account_version_response& v);
    static ores::iam::messaging::get_account_version_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
