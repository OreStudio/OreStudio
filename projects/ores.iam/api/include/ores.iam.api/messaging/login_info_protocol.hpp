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
#ifndef ORES_IAM_API_MESSAGING_LOGIN_INFO_PROTOCOL_HPP
#define ORES_IAM_API_MESSAGING_LOGIN_INFO_PROTOCOL_HPP

#include "ores.iam.api/domain/login_info_codec.hpp"
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

struct login_info_key {
    boost::uuids::uuid account_id;

    friend bool operator==(const login_info_key&, const login_info_key&) = default;
};

struct login_info_lookup {
    login_info_key key;
    std::optional<ores::iam::domain::login_info> login_info;

    friend bool operator==(const login_info_lookup&, const login_info_lookup&) = default;
};

struct login_info_filter {
    std::optional<std::vector<boost::uuids::uuid>> account_id_one_of;

    friend bool operator==(const login_info_filter&, const login_info_filter&) = default;
};

struct login_info_event {
    boost::uuids::uuid event_id;
    login_info_key key;
    std::string action;
    std::uint32_t version{};
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;

    friend bool operator==(const login_info_event&, const login_info_event&) = default;
};

struct list_login_info_request {
    using response_type = struct list_login_info_response;
    static constexpr std::string_view nats_subject = "iam.v1.login_info.list";
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
    std::optional<login_info_filter> filter;

    friend bool operator==(const list_login_info_request&,
                           const list_login_info_request&) = default;
};

struct list_login_info_response {
    ores::utility::domain::result result;
    std::vector<ores::iam::domain::login_info> login_info;
    std::uint64_t total{};

    friend bool operator==(const list_login_info_response&,
                           const list_login_info_response&) = default;
};

struct get_login_info_request {
    using response_type = struct get_login_info_response;
    static constexpr std::string_view nats_subject = "iam.v1.login_info.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    login_info_key key;

    friend bool operator==(const get_login_info_request&, const get_login_info_request&) = default;
};

struct get_login_info_response {
    ores::utility::domain::result result;
    std::optional<ores::iam::domain::login_info> login_info;

    friend bool operator==(const get_login_info_response&,
                           const get_login_info_response&) = default;
};

struct get_many_login_info_request {
    using response_type = struct get_many_login_info_response;
    static constexpr std::string_view nats_subject = "iam.v1.login_info.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<login_info_key> keys;

    friend bool operator==(const get_many_login_info_request&,
                           const get_many_login_info_request&) = default;
};

struct get_many_login_info_response {
    ores::utility::domain::result result;
    std::vector<login_info_lookup> entries;

    friend bool operator==(const get_many_login_info_response&,
                           const get_many_login_info_response&) = default;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace login_info_event_subjects {
inline constexpr std::string_view created = "iam.v1.events.login_info.created";
inline constexpr std::string_view updated = "iam.v1.events.login_info.updated";
inline constexpr std::string_view deleted = "iam.v1.events.login_info.deleted";
}
/**
 * @brief The members of login_info_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<login_info_key>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"account_id", &login_info_key::account_id},
    };
}

/**
 * @brief The members of login_info_lookup, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<login_info_lookup>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"key", &login_info_lookup::key},
        utility::codec::field{"login_info", &login_info_lookup::login_info},
    };
}

/**
 * @brief The members of login_info_filter, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<login_info_filter>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"account_id_one_of", &login_info_filter::account_id_one_of},
    };
}

/**
 * @brief The members of login_info_event, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<login_info_event>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"event_id", &login_info_event::event_id},
        utility::codec::field{"key", &login_info_event::key},
        utility::codec::field{"action", &login_info_event::action},
        utility::codec::field{"version", &login_info_event::version},
        utility::codec::field{"occurred_at", &login_info_event::occurred_at},
        utility::codec::field{"correlation_id", &login_info_event::correlation_id},
    };
}

/**
 * @brief The members of list_login_info_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<list_login_info_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"offset", &list_login_info_request::offset},
        utility::codec::field{"limit", &list_login_info_request::limit},
        utility::codec::field{"order", &list_login_info_request::order},
        utility::codec::field{"filter", &list_login_info_request::filter},
    };
}

/**
 * @brief The members of list_login_info_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<list_login_info_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &list_login_info_response::result},
        utility::codec::field{"login_info", &list_login_info_response::login_info},
        utility::codec::field{"total", &list_login_info_response::total},
    };
}

/**
 * @brief The members of get_login_info_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_login_info_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"key", &get_login_info_request::key},
    };
}

/**
 * @brief The members of get_login_info_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_login_info_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &get_login_info_response::result},
        utility::codec::field{"login_info", &get_login_info_response::login_info},
    };
}

/**
 * @brief The members of get_many_login_info_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_many_login_info_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"keys", &get_many_login_info_request::keys},
    };
}

/**
 * @brief The members of get_many_login_info_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_many_login_info_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &get_many_login_info_response::result},
        utility::codec::field{"entries", &get_many_login_info_response::entries},
    };
}


}
namespace ores::utility::codec {

template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::login_info_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::login_info_key& v);
    static ores::iam::messaging::login_info_key read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::login_info_key& v);
    static ores::iam::messaging::login_info_key read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::login_info_lookup> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::login_info_lookup& v);
    static ores::iam::messaging::login_info_lookup read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::login_info_lookup& v);
    static ores::iam::messaging::login_info_lookup read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::login_info_filter> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::login_info_filter& v);
    static ores::iam::messaging::login_info_filter read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::login_info_filter& v);
    static ores::iam::messaging::login_info_filter read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::login_info_event> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::login_info_event& v);
    static ores::iam::messaging::login_info_event read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::login_info_event& v);
    static ores::iam::messaging::login_info_event read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::list_login_info_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::list_login_info_request& v);
    static ores::iam::messaging::list_login_info_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::list_login_info_request& v);
    static ores::iam::messaging::list_login_info_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::list_login_info_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::list_login_info_response& v);
    static ores::iam::messaging::list_login_info_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::list_login_info_response& v);
    static ores::iam::messaging::list_login_info_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::get_login_info_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::get_login_info_request& v);
    static ores::iam::messaging::get_login_info_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::get_login_info_request& v);
    static ores::iam::messaging::get_login_info_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::get_login_info_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::get_login_info_response& v);
    static ores::iam::messaging::get_login_info_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::get_login_info_response& v);
    static ores::iam::messaging::get_login_info_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::get_many_login_info_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::get_many_login_info_request& v);
    static ores::iam::messaging::get_many_login_info_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::get_many_login_info_request& v);
    static ores::iam::messaging::get_many_login_info_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::get_many_login_info_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::get_many_login_info_response& v);
    static ores::iam::messaging::get_many_login_info_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::get_many_login_info_response& v);
    static ores::iam::messaging::get_many_login_info_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
