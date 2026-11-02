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
#ifndef ORES_INBOX_API_MESSAGING_NOTIFICATION_ARGUMENT_PROTOCOL_HPP
#define ORES_INBOX_API_MESSAGING_NOTIFICATION_ARGUMENT_PROTOCOL_HPP

#include "ores.inbox.api/domain/notification_argument.hpp"
#include "ores.inbox.api/export.hpp"
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

namespace ores::inbox::messaging {

struct notification_argument_key {
    boost::uuids::uuid notification_id;
    std::string name;
};

struct notification_argument_write {
    boost::uuids::uuid notification_id;
    std::string name;
    std::string value;
};

struct notification_argument_change {
    notification_argument_write write;
    ores::utility::domain::precondition precondition;
};

struct notification_argument_removal {
    notification_argument_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;
};

struct notification_argument_lookup {
    notification_argument_key key;
    std::optional<ores::inbox::domain::notification_argument> notification_argument;
};

struct notification_arguments_filter {
    std::optional<boost::uuids::uuid> notification_id;
    std::optional<std::vector<boost::uuids::uuid>> notification_id_one_of;
};

struct list_notification_arguments_request {
    using response_type = struct list_notification_arguments_response;
    static constexpr std::string_view nats_subject = "inbox.v1.notification_arguments.list";
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
    std::optional<notification_arguments_filter> filter;
};

struct list_notification_arguments_response {
    ores::utility::domain::result result;
    std::vector<ores::inbox::domain::notification_argument> notification_arguments;
    std::uint64_t total;
};

struct get_notification_argument_request {
    using response_type = struct get_notification_argument_response;
    static constexpr std::string_view nats_subject = "inbox.v1.notification_arguments.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    notification_argument_key key;
};

struct get_notification_argument_response {
    ores::utility::domain::result result;
    std::optional<ores::inbox::domain::notification_argument> notification_argument;
};

struct get_many_notification_arguments_request {
    using response_type = struct get_many_notification_arguments_response;
    static constexpr std::string_view nats_subject = "inbox.v1.notification_arguments.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<notification_argument_key> keys;
};

struct get_many_notification_arguments_response {
    ores::utility::domain::result result;
    std::vector<notification_argument_lookup> entries;
};

struct put_notification_argument_request {
    using response_type = struct put_notification_argument_response;
    static constexpr std::string_view nats_subject = "inbox.v1.notification_arguments.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    notification_argument_change change;
    ores::utility::domain::change_intent intent;
};

struct put_notification_argument_response {
    ores::utility::domain::result result;
    std::optional<ores::inbox::domain::notification_argument> notification_argument;
};

struct put_many_notification_arguments_request {
    using response_type = struct put_many_notification_arguments_response;
    static constexpr std::string_view nats_subject = "inbox.v1.notification_arguments.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<notification_argument_change> changes;
    ores::utility::domain::change_intent intent;
};

struct put_many_notification_arguments_response {
    ores::utility::domain::result result;
    std::vector<ores::inbox::domain::notification_argument> notification_arguments;
};

struct delete_notification_argument_request {
    using response_type = struct delete_notification_argument_response;
    static constexpr std::string_view nats_subject = "inbox.v1.notification_arguments.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    notification_argument_removal removal;
    ores::utility::domain::change_intent intent;
};

struct delete_notification_argument_response {
    ores::utility::domain::result result;
};

struct delete_many_notification_arguments_request {
    using response_type = struct delete_many_notification_arguments_response;
    static constexpr std::string_view nats_subject = "inbox.v1.notification_arguments.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<notification_argument_removal> removals;
    ores::utility::domain::change_intent intent;
};

struct delete_many_notification_arguments_response {
    ores::utility::domain::result result;
};

struct list_by_notification_id_notification_arguments_request {
    using response_type = struct list_by_notification_id_notification_arguments_response;
    static constexpr std::string_view nats_subject =
        "inbox.v1.notification_arguments.list_by_notification_id";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    boost::uuids::uuid notification_id;
    ores::utility::domain::scope scope = ores::utility::domain::scope::direct;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<notification_arguments_filter> filter;
};

struct list_by_notification_id_notification_arguments_response {
    ores::utility::domain::result result;
    std::vector<ores::inbox::domain::notification_argument> notification_arguments;
    std::uint64_t total;
};

/**
 * @brief The members of notification_argument_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const notification_argument_key&) {
    return std::tuple{
        utility::codec::field{"notification_id", &notification_argument_key::notification_id},
        utility::codec::field{"name", &notification_argument_key::name},
    };
}

/**
 * @brief The members of notification_argument_write, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const notification_argument_write&) {
    return std::tuple{
        utility::codec::field{"notification_id", &notification_argument_write::notification_id},
        utility::codec::field{"name", &notification_argument_write::name},
        utility::codec::field{"value", &notification_argument_write::value},
    };
}

/**
 * @brief The members of notification_argument_change, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const notification_argument_change&) {
    return std::tuple{
        utility::codec::field{"write", &notification_argument_change::write},
        utility::codec::field{"precondition", &notification_argument_change::precondition},
    };
}

/**
 * @brief The members of notification_argument_removal, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const notification_argument_removal&) {
    return std::tuple{
        utility::codec::field{"key", &notification_argument_removal::key},
        utility::codec::field{"precondition", &notification_argument_removal::precondition},
    };
}

/**
 * @brief The members of notification_argument_lookup, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const notification_argument_lookup&) {
    return std::tuple{
        utility::codec::field{"key", &notification_argument_lookup::key},
        utility::codec::field{"notification_argument",
                              &notification_argument_lookup::notification_argument},
    };
}

/**
 * @brief The members of notification_arguments_filter, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const notification_arguments_filter&) {
    return std::tuple{
        utility::codec::field{"notification_id", &notification_arguments_filter::notification_id},
        utility::codec::field{"notification_id_one_of",
                              &notification_arguments_filter::notification_id_one_of},
    };
}

/**
 * @brief The members of list_notification_arguments_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_notification_arguments_request&) {
    return std::tuple{
        utility::codec::field{"offset", &list_notification_arguments_request::offset},
        utility::codec::field{"limit", &list_notification_arguments_request::limit},
        utility::codec::field{"order", &list_notification_arguments_request::order},
        utility::codec::field{"filter", &list_notification_arguments_request::filter},
    };
}

/**
 * @brief The members of list_notification_arguments_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_notification_arguments_response&) {
    return std::tuple{
        utility::codec::field{"result", &list_notification_arguments_response::result},
        utility::codec::field{"notification_arguments",
                              &list_notification_arguments_response::notification_arguments},
        utility::codec::field{"total", &list_notification_arguments_response::total},
    };
}

/**
 * @brief The members of get_notification_argument_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_notification_argument_request&) {
    return std::tuple{
        utility::codec::field{"key", &get_notification_argument_request::key},
    };
}

/**
 * @brief The members of get_notification_argument_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_notification_argument_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_notification_argument_response::result},
        utility::codec::field{"notification_argument",
                              &get_notification_argument_response::notification_argument},
    };
}

/**
 * @brief The members of get_many_notification_arguments_request, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_many_notification_arguments_request&) {
    return std::tuple{
        utility::codec::field{"keys", &get_many_notification_arguments_request::keys},
    };
}

/**
 * @brief The members of get_many_notification_arguments_response, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_many_notification_arguments_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_many_notification_arguments_response::result},
        utility::codec::field{"entries", &get_many_notification_arguments_response::entries},
    };
}

/**
 * @brief The members of put_notification_argument_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_notification_argument_request&) {
    return std::tuple{
        utility::codec::field{"change", &put_notification_argument_request::change},
        utility::codec::field{"intent", &put_notification_argument_request::intent},
    };
}

/**
 * @brief The members of put_notification_argument_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_notification_argument_response&) {
    return std::tuple{
        utility::codec::field{"result", &put_notification_argument_response::result},
        utility::codec::field{"notification_argument",
                              &put_notification_argument_response::notification_argument},
    };
}

/**
 * @brief The members of put_many_notification_arguments_request, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_many_notification_arguments_request&) {
    return std::tuple{
        utility::codec::field{"changes", &put_many_notification_arguments_request::changes},
        utility::codec::field{"intent", &put_many_notification_arguments_request::intent},
    };
}

/**
 * @brief The members of put_many_notification_arguments_response, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_many_notification_arguments_response&) {
    return std::tuple{
        utility::codec::field{"result", &put_many_notification_arguments_response::result},
        utility::codec::field{"notification_arguments",
                              &put_many_notification_arguments_response::notification_arguments},
    };
}

/**
 * @brief The members of delete_notification_argument_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_notification_argument_request&) {
    return std::tuple{
        utility::codec::field{"removal", &delete_notification_argument_request::removal},
        utility::codec::field{"intent", &delete_notification_argument_request::intent},
    };
}

/**
 * @brief The members of delete_notification_argument_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_notification_argument_response&) {
    return std::tuple{
        utility::codec::field{"result", &delete_notification_argument_response::result},
    };
}

/**
 * @brief The members of delete_many_notification_arguments_request, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_many_notification_arguments_request&) {
    return std::tuple{
        utility::codec::field{"removals", &delete_many_notification_arguments_request::removals},
        utility::codec::field{"intent", &delete_many_notification_arguments_request::intent},
    };
}

/**
 * @brief The members of delete_many_notification_arguments_response, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_many_notification_arguments_response&) {
    return std::tuple{
        utility::codec::field{"result", &delete_many_notification_arguments_response::result},
    };
}

/**
 * @brief The members of list_by_notification_id_notification_arguments_request, named as the wire
 * names them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(const list_by_notification_id_notification_arguments_request&) {
    return std::tuple{
        utility::codec::field{
            "notification_id",
            &list_by_notification_id_notification_arguments_request::notification_id},
        utility::codec::field{"scope",
                              &list_by_notification_id_notification_arguments_request::scope},
        utility::codec::field{"offset",
                              &list_by_notification_id_notification_arguments_request::offset},
        utility::codec::field{"limit",
                              &list_by_notification_id_notification_arguments_request::limit},
        utility::codec::field{"order",
                              &list_by_notification_id_notification_arguments_request::order},
        utility::codec::field{"filter",
                              &list_by_notification_id_notification_arguments_request::filter},
    };
}

/**
 * @brief The members of list_by_notification_id_notification_arguments_response, named as the wire
 * names them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(const list_by_notification_id_notification_arguments_response&) {
    return std::tuple{
        utility::codec::field{"result",
                              &list_by_notification_id_notification_arguments_response::result},
        utility::codec::field{
            "notification_arguments",
            &list_by_notification_id_notification_arguments_response::notification_arguments},
        utility::codec::field{"total",
                              &list_by_notification_id_notification_arguments_response::total},
    };
}

}
namespace ores::utility::codec {

template <>
struct ORES_INBOX_API_EXPORT wire_traits<ores::inbox::messaging::notification_argument_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::inbox::messaging::notification_argument_key& v);
    static ores::inbox::messaging::notification_argument_key read_json(std::string_view text);
    static std::string write_msgpack(const ores::inbox::messaging::notification_argument_key& v);
    static ores::inbox::messaging::notification_argument_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_INBOX_API_EXPORT wire_traits<ores::inbox::messaging::notification_argument_write> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::inbox::messaging::notification_argument_write& v);
    static ores::inbox::messaging::notification_argument_write read_json(std::string_view text);
    static std::string write_msgpack(const ores::inbox::messaging::notification_argument_write& v);
    static ores::inbox::messaging::notification_argument_write
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_INBOX_API_EXPORT wire_traits<ores::inbox::messaging::notification_argument_change> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::inbox::messaging::notification_argument_change& v);
    static ores::inbox::messaging::notification_argument_change read_json(std::string_view text);
    static std::string write_msgpack(const ores::inbox::messaging::notification_argument_change& v);
    static ores::inbox::messaging::notification_argument_change
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_INBOX_API_EXPORT wire_traits<ores::inbox::messaging::notification_argument_removal> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::inbox::messaging::notification_argument_removal& v);
    static ores::inbox::messaging::notification_argument_removal read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::inbox::messaging::notification_argument_removal& v);
    static ores::inbox::messaging::notification_argument_removal
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_INBOX_API_EXPORT wire_traits<ores::inbox::messaging::notification_argument_lookup> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::inbox::messaging::notification_argument_lookup& v);
    static ores::inbox::messaging::notification_argument_lookup read_json(std::string_view text);
    static std::string write_msgpack(const ores::inbox::messaging::notification_argument_lookup& v);
    static ores::inbox::messaging::notification_argument_lookup
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_INBOX_API_EXPORT wire_traits<ores::inbox::messaging::notification_arguments_filter> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::inbox::messaging::notification_arguments_filter& v);
    static ores::inbox::messaging::notification_arguments_filter read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::inbox::messaging::notification_arguments_filter& v);
    static ores::inbox::messaging::notification_arguments_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_INBOX_API_EXPORT
    wire_traits<ores::inbox::messaging::list_notification_arguments_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::inbox::messaging::list_notification_arguments_request& v);
    static ores::inbox::messaging::list_notification_arguments_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::inbox::messaging::list_notification_arguments_request& v);
    static ores::inbox::messaging::list_notification_arguments_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_INBOX_API_EXPORT
    wire_traits<ores::inbox::messaging::list_notification_arguments_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::inbox::messaging::list_notification_arguments_response& v);
    static ores::inbox::messaging::list_notification_arguments_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::inbox::messaging::list_notification_arguments_response& v);
    static ores::inbox::messaging::list_notification_arguments_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_INBOX_API_EXPORT
    wire_traits<ores::inbox::messaging::get_notification_argument_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::inbox::messaging::get_notification_argument_request& v);
    static ores::inbox::messaging::get_notification_argument_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::inbox::messaging::get_notification_argument_request& v);
    static ores::inbox::messaging::get_notification_argument_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_INBOX_API_EXPORT
    wire_traits<ores::inbox::messaging::get_notification_argument_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::inbox::messaging::get_notification_argument_response& v);
    static ores::inbox::messaging::get_notification_argument_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::inbox::messaging::get_notification_argument_response& v);
    static ores::inbox::messaging::get_notification_argument_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_INBOX_API_EXPORT
    wire_traits<ores::inbox::messaging::get_many_notification_arguments_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::inbox::messaging::get_many_notification_arguments_request& v);
    static ores::inbox::messaging::get_many_notification_arguments_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::inbox::messaging::get_many_notification_arguments_request& v);
    static ores::inbox::messaging::get_many_notification_arguments_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_INBOX_API_EXPORT
    wire_traits<ores::inbox::messaging::get_many_notification_arguments_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::inbox::messaging::get_many_notification_arguments_response& v);
    static ores::inbox::messaging::get_many_notification_arguments_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::inbox::messaging::get_many_notification_arguments_response& v);
    static ores::inbox::messaging::get_many_notification_arguments_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_INBOX_API_EXPORT
    wire_traits<ores::inbox::messaging::put_notification_argument_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::inbox::messaging::put_notification_argument_request& v);
    static ores::inbox::messaging::put_notification_argument_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::inbox::messaging::put_notification_argument_request& v);
    static ores::inbox::messaging::put_notification_argument_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_INBOX_API_EXPORT
    wire_traits<ores::inbox::messaging::put_notification_argument_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::inbox::messaging::put_notification_argument_response& v);
    static ores::inbox::messaging::put_notification_argument_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::inbox::messaging::put_notification_argument_response& v);
    static ores::inbox::messaging::put_notification_argument_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_INBOX_API_EXPORT
    wire_traits<ores::inbox::messaging::put_many_notification_arguments_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::inbox::messaging::put_many_notification_arguments_request& v);
    static ores::inbox::messaging::put_many_notification_arguments_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::inbox::messaging::put_many_notification_arguments_request& v);
    static ores::inbox::messaging::put_many_notification_arguments_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_INBOX_API_EXPORT
    wire_traits<ores::inbox::messaging::put_many_notification_arguments_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::inbox::messaging::put_many_notification_arguments_response& v);
    static ores::inbox::messaging::put_many_notification_arguments_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::inbox::messaging::put_many_notification_arguments_response& v);
    static ores::inbox::messaging::put_many_notification_arguments_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_INBOX_API_EXPORT
    wire_traits<ores::inbox::messaging::delete_notification_argument_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::inbox::messaging::delete_notification_argument_request& v);
    static ores::inbox::messaging::delete_notification_argument_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::inbox::messaging::delete_notification_argument_request& v);
    static ores::inbox::messaging::delete_notification_argument_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_INBOX_API_EXPORT
    wire_traits<ores::inbox::messaging::delete_notification_argument_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::inbox::messaging::delete_notification_argument_response& v);
    static ores::inbox::messaging::delete_notification_argument_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::inbox::messaging::delete_notification_argument_response& v);
    static ores::inbox::messaging::delete_notification_argument_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_INBOX_API_EXPORT
    wire_traits<ores::inbox::messaging::delete_many_notification_arguments_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::inbox::messaging::delete_many_notification_arguments_request& v);
    static ores::inbox::messaging::delete_many_notification_arguments_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::inbox::messaging::delete_many_notification_arguments_request& v);
    static ores::inbox::messaging::delete_many_notification_arguments_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_INBOX_API_EXPORT
    wire_traits<ores::inbox::messaging::delete_many_notification_arguments_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::inbox::messaging::delete_many_notification_arguments_response& v);
    static ores::inbox::messaging::delete_many_notification_arguments_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::inbox::messaging::delete_many_notification_arguments_response& v);
    static ores::inbox::messaging::delete_many_notification_arguments_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_INBOX_API_EXPORT
    wire_traits<ores::inbox::messaging::list_by_notification_id_notification_arguments_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::inbox::messaging::list_by_notification_id_notification_arguments_request& v);
    static ores::inbox::messaging::list_by_notification_id_notification_arguments_request
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::inbox::messaging::list_by_notification_id_notification_arguments_request& v);
    static ores::inbox::messaging::list_by_notification_id_notification_arguments_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_INBOX_API_EXPORT
    wire_traits<ores::inbox::messaging::list_by_notification_id_notification_arguments_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::inbox::messaging::list_by_notification_id_notification_arguments_response& v);
    static ores::inbox::messaging::list_by_notification_id_notification_arguments_response
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::inbox::messaging::list_by_notification_id_notification_arguments_response& v);
    static ores::inbox::messaging::list_by_notification_id_notification_arguments_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
