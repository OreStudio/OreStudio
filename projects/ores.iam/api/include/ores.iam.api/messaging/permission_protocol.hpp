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
#ifndef ORES_IAM_API_MESSAGING_PERMISSION_PROTOCOL_HPP
#define ORES_IAM_API_MESSAGING_PERMISSION_PROTOCOL_HPP

#include "ores.iam.api/domain/permission.hpp"
#include "ores.iam.api/export.hpp"
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

namespace ores::iam::messaging {

struct permission_key {
    std::string code;
};

struct permission_write {
    boost::uuids::uuid id;
    std::string code;
    std::string description;
};

struct permission_change {
    permission_write write;
    ores::utility::domain::precondition precondition;
};

struct permission_removal {
    permission_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;
};

struct permission_lookup {
    permission_key key;
    std::optional<ores::iam::domain::permission> permission;
};

struct permissions_filter {
    std::optional<std::vector<boost::uuids::uuid>> id_one_of;
};

struct permission_event {
    boost::uuids::uuid event_id;
    permission_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;
};

struct list_permissions_request {
    using response_type = struct list_permissions_response;
    static constexpr std::string_view nats_subject = "iam.v1.permissions.list";
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
    std::optional<permissions_filter> filter;
    std::optional<std::string> as_of;
};

struct list_permissions_response {
    ores::utility::domain::result result;
    std::vector<ores::iam::domain::permission> permissions;
    std::uint64_t total;
};

struct get_permission_request {
    using response_type = struct get_permission_response;
    static constexpr std::string_view nats_subject = "iam.v1.permissions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    permission_key key;
};

struct get_permission_response {
    ores::utility::domain::result result;
    std::optional<ores::iam::domain::permission> permission;
};

struct get_many_permissions_request {
    using response_type = struct get_many_permissions_response;
    static constexpr std::string_view nats_subject = "iam.v1.permissions.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<permission_key> keys;
};

struct get_many_permissions_response {
    ores::utility::domain::result result;
    std::vector<permission_lookup> entries;
};

struct put_permission_request {
    using response_type = struct put_permission_response;
    static constexpr std::string_view nats_subject = "iam.v1.permissions.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    permission_change change;
    ores::utility::domain::change_intent intent;
};

struct put_permission_response {
    ores::utility::domain::result result;
    std::optional<ores::iam::domain::permission> permission;
};

struct put_many_permissions_request {
    using response_type = struct put_many_permissions_response;
    static constexpr std::string_view nats_subject = "iam.v1.permissions.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<permission_change> changes;
    ores::utility::domain::change_intent intent;
};

struct put_many_permissions_response {
    ores::utility::domain::result result;
    std::vector<ores::iam::domain::permission> permissions;
};

struct delete_permission_request {
    using response_type = struct delete_permission_response;
    static constexpr std::string_view nats_subject = "iam.v1.permissions.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    permission_removal removal;
    ores::utility::domain::change_intent intent;
};

struct delete_permission_response {
    ores::utility::domain::result result;
};

struct delete_many_permissions_request {
    using response_type = struct delete_many_permissions_response;
    static constexpr std::string_view nats_subject = "iam.v1.permissions.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<permission_removal> removals;
    ores::utility::domain::change_intent intent;
};

struct delete_many_permissions_response {
    ores::utility::domain::result result;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace permission_event_subjects {
inline constexpr std::string_view created = "iam.v1.permissions_events.created";
inline constexpr std::string_view updated = "iam.v1.permissions_events.updated";
inline constexpr std::string_view deleted = "iam.v1.permissions_events.deleted";
}
/**
 * @brief The members of permission_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const permission_key&) {
    return std::tuple{
        utility::codec::field{"code", &permission_key::code},
    };
}

/**
 * @brief The members of permission_write, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const permission_write&) {
    return std::tuple{
        utility::codec::field{"id", &permission_write::id},
        utility::codec::field{"code", &permission_write::code},
        utility::codec::field{"description", &permission_write::description},
    };
}

/**
 * @brief The members of permission_change, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const permission_change&) {
    return std::tuple{
        utility::codec::field{"write", &permission_change::write},
        utility::codec::field{"precondition", &permission_change::precondition},
    };
}

/**
 * @brief The members of permission_removal, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const permission_removal&) {
    return std::tuple{
        utility::codec::field{"key", &permission_removal::key},
        utility::codec::field{"precondition", &permission_removal::precondition},
    };
}

/**
 * @brief The members of permission_lookup, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const permission_lookup&) {
    return std::tuple{
        utility::codec::field{"key", &permission_lookup::key},
        utility::codec::field{"permission", &permission_lookup::permission},
    };
}

/**
 * @brief The members of permissions_filter, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const permissions_filter&) {
    return std::tuple{
        utility::codec::field{"id_one_of", &permissions_filter::id_one_of},
    };
}

/**
 * @brief The members of permission_event, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const permission_event&) {
    return std::tuple{
        utility::codec::field{"event_id", &permission_event::event_id},
        utility::codec::field{"key", &permission_event::key},
        utility::codec::field{"action", &permission_event::action},
        utility::codec::field{"version", &permission_event::version},
        utility::codec::field{"occurred_at", &permission_event::occurred_at},
        utility::codec::field{"correlation_id", &permission_event::correlation_id},
    };
}

/**
 * @brief The members of list_permissions_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_permissions_request&) {
    return std::tuple{
        utility::codec::field{"offset", &list_permissions_request::offset},
        utility::codec::field{"limit", &list_permissions_request::limit},
        utility::codec::field{"order", &list_permissions_request::order},
        utility::codec::field{"filter", &list_permissions_request::filter},
        utility::codec::field{"as_of", &list_permissions_request::as_of},
    };
}

/**
 * @brief The members of list_permissions_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_permissions_response&) {
    return std::tuple{
        utility::codec::field{"result", &list_permissions_response::result},
        utility::codec::field{"permissions", &list_permissions_response::permissions},
        utility::codec::field{"total", &list_permissions_response::total},
    };
}

/**
 * @brief The members of get_permission_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_permission_request&) {
    return std::tuple{
        utility::codec::field{"key", &get_permission_request::key},
    };
}

/**
 * @brief The members of get_permission_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_permission_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_permission_response::result},
        utility::codec::field{"permission", &get_permission_response::permission},
    };
}

/**
 * @brief The members of get_many_permissions_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_many_permissions_request&) {
    return std::tuple{
        utility::codec::field{"keys", &get_many_permissions_request::keys},
    };
}

/**
 * @brief The members of get_many_permissions_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_many_permissions_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_many_permissions_response::result},
        utility::codec::field{"entries", &get_many_permissions_response::entries},
    };
}

/**
 * @brief The members of put_permission_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_permission_request&) {
    return std::tuple{
        utility::codec::field{"change", &put_permission_request::change},
        utility::codec::field{"intent", &put_permission_request::intent},
    };
}

/**
 * @brief The members of put_permission_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_permission_response&) {
    return std::tuple{
        utility::codec::field{"result", &put_permission_response::result},
        utility::codec::field{"permission", &put_permission_response::permission},
    };
}

/**
 * @brief The members of put_many_permissions_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_many_permissions_request&) {
    return std::tuple{
        utility::codec::field{"changes", &put_many_permissions_request::changes},
        utility::codec::field{"intent", &put_many_permissions_request::intent},
    };
}

/**
 * @brief The members of put_many_permissions_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_many_permissions_response&) {
    return std::tuple{
        utility::codec::field{"result", &put_many_permissions_response::result},
        utility::codec::field{"permissions", &put_many_permissions_response::permissions},
    };
}

/**
 * @brief The members of delete_permission_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_permission_request&) {
    return std::tuple{
        utility::codec::field{"removal", &delete_permission_request::removal},
        utility::codec::field{"intent", &delete_permission_request::intent},
    };
}

/**
 * @brief The members of delete_permission_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_permission_response&) {
    return std::tuple{
        utility::codec::field{"result", &delete_permission_response::result},
    };
}

/**
 * @brief The members of delete_many_permissions_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_many_permissions_request&) {
    return std::tuple{
        utility::codec::field{"removals", &delete_many_permissions_request::removals},
        utility::codec::field{"intent", &delete_many_permissions_request::intent},
    };
}

/**
 * @brief The members of delete_many_permissions_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_many_permissions_response&) {
    return std::tuple{
        utility::codec::field{"result", &delete_many_permissions_response::result},
    };
}


}
namespace ores::utility::codec {

template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::permission_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::permission_key& v);
    static ores::iam::messaging::permission_key read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::permission_key& v);
    static ores::iam::messaging::permission_key read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::permission_write> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::permission_write& v);
    static ores::iam::messaging::permission_write read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::permission_write& v);
    static ores::iam::messaging::permission_write read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::permission_change> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::permission_change& v);
    static ores::iam::messaging::permission_change read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::permission_change& v);
    static ores::iam::messaging::permission_change read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::permission_removal> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::permission_removal& v);
    static ores::iam::messaging::permission_removal read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::permission_removal& v);
    static ores::iam::messaging::permission_removal read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::permission_lookup> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::permission_lookup& v);
    static ores::iam::messaging::permission_lookup read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::permission_lookup& v);
    static ores::iam::messaging::permission_lookup read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::permissions_filter> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::permissions_filter& v);
    static ores::iam::messaging::permissions_filter read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::permissions_filter& v);
    static ores::iam::messaging::permissions_filter read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::permission_event> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::permission_event& v);
    static ores::iam::messaging::permission_event read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::permission_event& v);
    static ores::iam::messaging::permission_event read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::list_permissions_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::list_permissions_request& v);
    static ores::iam::messaging::list_permissions_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::list_permissions_request& v);
    static ores::iam::messaging::list_permissions_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::list_permissions_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::list_permissions_response& v);
    static ores::iam::messaging::list_permissions_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::list_permissions_response& v);
    static ores::iam::messaging::list_permissions_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::get_permission_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::get_permission_request& v);
    static ores::iam::messaging::get_permission_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::get_permission_request& v);
    static ores::iam::messaging::get_permission_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::get_permission_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::get_permission_response& v);
    static ores::iam::messaging::get_permission_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::get_permission_response& v);
    static ores::iam::messaging::get_permission_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::get_many_permissions_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::get_many_permissions_request& v);
    static ores::iam::messaging::get_many_permissions_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::get_many_permissions_request& v);
    static ores::iam::messaging::get_many_permissions_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::get_many_permissions_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::get_many_permissions_response& v);
    static ores::iam::messaging::get_many_permissions_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::get_many_permissions_response& v);
    static ores::iam::messaging::get_many_permissions_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::put_permission_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::put_permission_request& v);
    static ores::iam::messaging::put_permission_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::put_permission_request& v);
    static ores::iam::messaging::put_permission_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::put_permission_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::put_permission_response& v);
    static ores::iam::messaging::put_permission_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::put_permission_response& v);
    static ores::iam::messaging::put_permission_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::put_many_permissions_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::put_many_permissions_request& v);
    static ores::iam::messaging::put_many_permissions_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::put_many_permissions_request& v);
    static ores::iam::messaging::put_many_permissions_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::put_many_permissions_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::put_many_permissions_response& v);
    static ores::iam::messaging::put_many_permissions_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::put_many_permissions_response& v);
    static ores::iam::messaging::put_many_permissions_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::delete_permission_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::delete_permission_request& v);
    static ores::iam::messaging::delete_permission_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::delete_permission_request& v);
    static ores::iam::messaging::delete_permission_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::delete_permission_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::delete_permission_response& v);
    static ores::iam::messaging::delete_permission_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::delete_permission_response& v);
    static ores::iam::messaging::delete_permission_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::delete_many_permissions_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::delete_many_permissions_request& v);
    static ores::iam::messaging::delete_many_permissions_request read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::iam::messaging::delete_many_permissions_request& v);
    static ores::iam::messaging::delete_many_permissions_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::delete_many_permissions_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::delete_many_permissions_response& v);
    static ores::iam::messaging::delete_many_permissions_response read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::iam::messaging::delete_many_permissions_response& v);
    static ores::iam::messaging::delete_many_permissions_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
