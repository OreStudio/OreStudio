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
#ifndef ORES_INBOX_API_MESSAGING_NOTIFICATION_OPERATIONS_PROTOCOL_HPP
#define ORES_INBOX_API_MESSAGING_NOTIFICATION_OPERATIONS_PROTOCOL_HPP

#include "ores.inbox.api/export.hpp"
#include "ores.utility/codec/wire_fields.hpp"
#include "ores.utility/codec/wire_traits_fwd.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <cstddef>
#include <span>
#include <string>
#include <string_view>
#include <tuple>
#include <vector>

namespace ores::inbox::messaging {

/**
 * @brief One value a notification's message names.
 */
struct notification_argument_value {
    std::string name;
    std::string value;

    friend bool operator==(const notification_argument_value&,
                           const notification_argument_value&) = default;
};

/**
 * @brief One notification as its recipient sees it: what happened, where it
 * is dealt with, the values its message names, and the recipient's own read
 * state.
 */
struct inbox_notification {
    std::string id;
    std::string kind_code;
    /**
     * @brief The message the screen renders in the reader's language.
     */
    std::string message_key;
    /**
     * @brief The username of the account that raised it.
     */
    std::string raised_by;
    std::chrono::system_clock::time_point raised_at;
    std::string link_route;
    /**
     * @brief The thing on the linked screen, or empty when the screen is the
     * whole of the link.
     */
    std::string link_id;
    std::vector<notification_argument_value> arguments;
    /**
     * @brief When the recipient read it, or empty while it is unread.
     */
    std::optional<std::chrono::system_clock::time_point> read_at;

    friend bool operator==(const inbox_notification&, const inbox_notification&) = default;
};

/**
 * @brief Tells people that something happened.
 *
 * The audience is the named accounts, the holders of a permission in the
 * tenant, or both. The raiser is the signed-in account, a person or a
 * service. Raising never fails the activity that asks for it: a caller
 * logs an unsuccessful outcome and carries on.
 */
struct raise_notification_request {
    using response_type = struct raise_notification_response;
    static constexpr std::string_view nats_subject = "inbox.v1.ops.raise_notification";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::string kind_code;
    std::string link_route;
    std::string link_id;
    std::vector<notification_argument_value> arguments;
    /**
     * @brief The accounts told by name, as UUID strings.
     */
    std::vector<std::string> account_ids;
    /**
     * @brief The permission whose holders are told, or empty for none.
     */
    std::string audience_permission_code;

    friend bool operator==(const raise_notification_request&,
                           const raise_notification_request&) = default;
};

struct raise_notification_response {
    ores::utility::domain::result result;
    std::string notification_id;
    /**
     * @brief How many people the notification reached.
     */
    int recipient_count = 0;

    friend bool operator==(const raise_notification_response&,
                           const raise_notification_response&) = default;
};

/**
 * @brief Reads the signed-in person's notifications that they have not
 * cleared, newest first.
 */
struct list_my_notifications_request {
    using response_type = struct list_my_notifications_response;
    static constexpr std::string_view nats_subject = "inbox.v1.ops.list_my_notifications";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    bool unread_only = false;
    int offset = 0;
    int limit = 50;

    friend bool operator==(const list_my_notifications_request&,
                           const list_my_notifications_request&) = default;
};

struct list_my_notifications_response {
    ores::utility::domain::result result;
    std::vector<inbox_notification> notifications;
    int total = 0;

    friend bool operator==(const list_my_notifications_response&,
                           const list_my_notifications_response&) = default;
};

/**
 * @brief Reads how many of the signed-in person's notifications are unread,
 * for the bell on every screen.
 */
struct count_unread_notifications_request {
    using response_type = struct count_unread_notifications_response;
    static constexpr std::string_view nats_subject = "inbox.v1.ops.count_unread_notifications";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;

    friend bool operator==(const count_unread_notifications_request&,
                           const count_unread_notifications_request&) = default;
};

struct count_unread_notifications_response {
    ores::utility::domain::result result;
    int unread = 0;

    friend bool operator==(const count_unread_notifications_response&,
                           const count_unread_notifications_response&) = default;
};

/**
 * @brief Marks some or all of the signed-in person's notifications read.
 */
struct mark_notifications_read_request {
    using response_type = struct mark_notifications_read_response;
    static constexpr std::string_view nats_subject = "inbox.v1.ops.mark_notifications_read";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    /**
     * @brief The notifications to mark, or empty for every unread one.
     */
    std::vector<std::string> notification_ids;

    friend bool operator==(const mark_notifications_read_request&,
                           const mark_notifications_read_request&) = default;
};

struct mark_notifications_read_response {
    ores::utility::domain::result result;
    int marked = 0;

    friend bool operator==(const mark_notifications_read_response&,
                           const mark_notifications_read_response&) = default;
};

/**
 * @brief Removes notifications from the signed-in person's list. A cleared
 * notification is also read.
 */
struct clear_notifications_request {
    using response_type = struct clear_notifications_response;
    static constexpr std::string_view nats_subject = "inbox.v1.ops.clear_notifications";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    /**
     * @brief The notifications to clear, or empty for every read one.
     */
    std::vector<std::string> notification_ids;

    friend bool operator==(const clear_notifications_request&,
                           const clear_notifications_request&) = default;
};

struct clear_notifications_response {
    ores::utility::domain::result result;
    int cleared = 0;

    friend bool operator==(const clear_notifications_response&,
                           const clear_notifications_response&) = default;
};

/**
 * @brief The members of notification_argument_value, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<notification_argument_value>) {
    return ::ores::utility::codec::field_list{
        ::ores::utility::codec::field{"name", &notification_argument_value::name},
        ::ores::utility::codec::field{"value", &notification_argument_value::value},
    };
}

/**
 * @brief The members of inbox_notification, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<inbox_notification>) {
    return ::ores::utility::codec::field_list{
        ::ores::utility::codec::field{"id", &inbox_notification::id},
        ::ores::utility::codec::field{"kind_code", &inbox_notification::kind_code},
        ::ores::utility::codec::field{"message_key", &inbox_notification::message_key},
        ::ores::utility::codec::field{"raised_by", &inbox_notification::raised_by},
        ::ores::utility::codec::field{"raised_at", &inbox_notification::raised_at},
        ::ores::utility::codec::field{"link_route", &inbox_notification::link_route},
        ::ores::utility::codec::field{"link_id", &inbox_notification::link_id},
        ::ores::utility::codec::field{"arguments", &inbox_notification::arguments},
        ::ores::utility::codec::field{"read_at", &inbox_notification::read_at},
    };
}

/**
 * @brief The members of raise_notification_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<raise_notification_request>) {
    return ::ores::utility::codec::field_list{
        ::ores::utility::codec::field{"kind_code", &raise_notification_request::kind_code},
        ::ores::utility::codec::field{"link_route", &raise_notification_request::link_route},
        ::ores::utility::codec::field{"link_id", &raise_notification_request::link_id},
        ::ores::utility::codec::field{"arguments", &raise_notification_request::arguments},
        ::ores::utility::codec::field{"account_ids", &raise_notification_request::account_ids},
        ::ores::utility::codec::field{"audience_permission_code",
                                      &raise_notification_request::audience_permission_code},
    };
}

/**
 * @brief The members of raise_notification_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<raise_notification_response>) {
    return ::ores::utility::codec::field_list{
        ::ores::utility::codec::field{"result", &raise_notification_response::result},
        ::ores::utility::codec::field{"notification_id",
                                      &raise_notification_response::notification_id},
        ::ores::utility::codec::field{"recipient_count",
                                      &raise_notification_response::recipient_count},
    };
}

/**
 * @brief The members of list_my_notifications_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<list_my_notifications_request>) {
    return ::ores::utility::codec::field_list{
        ::ores::utility::codec::field{"unread_only", &list_my_notifications_request::unread_only},
        ::ores::utility::codec::field{"offset", &list_my_notifications_request::offset},
        ::ores::utility::codec::field{"limit", &list_my_notifications_request::limit},
    };
}

/**
 * @brief The members of list_my_notifications_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<list_my_notifications_response>) {
    return ::ores::utility::codec::field_list{
        ::ores::utility::codec::field{"result", &list_my_notifications_response::result},
        ::ores::utility::codec::field{"notifications",
                                      &list_my_notifications_response::notifications},
        ::ores::utility::codec::field{"total", &list_my_notifications_response::total},
    };
}

/**
 * @brief The members of count_unread_notifications_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<count_unread_notifications_request>) {
    return ::ores::utility::codec::field_list{};
}

/**
 * @brief The members of count_unread_notifications_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<count_unread_notifications_response>) {
    return ::ores::utility::codec::field_list{
        ::ores::utility::codec::field{"result", &count_unread_notifications_response::result},
        ::ores::utility::codec::field{"unread", &count_unread_notifications_response::unread},
    };
}

/**
 * @brief The members of mark_notifications_read_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<mark_notifications_read_request>) {
    return ::ores::utility::codec::field_list{
        ::ores::utility::codec::field{"notification_ids",
                                      &mark_notifications_read_request::notification_ids},
    };
}

/**
 * @brief The members of mark_notifications_read_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<mark_notifications_read_response>) {
    return ::ores::utility::codec::field_list{
        ::ores::utility::codec::field{"result", &mark_notifications_read_response::result},
        ::ores::utility::codec::field{"marked", &mark_notifications_read_response::marked},
    };
}

/**
 * @brief The members of clear_notifications_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<clear_notifications_request>) {
    return ::ores::utility::codec::field_list{
        ::ores::utility::codec::field{"notification_ids",
                                      &clear_notifications_request::notification_ids},
    };
}

/**
 * @brief The members of clear_notifications_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<clear_notifications_response>) {
    return ::ores::utility::codec::field_list{
        ::ores::utility::codec::field{"result", &clear_notifications_response::result},
        ::ores::utility::codec::field{"cleared", &clear_notifications_response::cleared},
    };
}

}
namespace ores::utility::codec {

template <>
struct ORES_INBOX_API_EXPORT wire_traits<ores::inbox::messaging::notification_argument_value> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::inbox::messaging::notification_argument_value& v);
    static ores::inbox::messaging::notification_argument_value read_json(std::string_view text);
    static std::string write_msgpack(const ores::inbox::messaging::notification_argument_value& v);
    static ores::inbox::messaging::notification_argument_value
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_INBOX_API_EXPORT wire_traits<ores::inbox::messaging::inbox_notification> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::inbox::messaging::inbox_notification& v);
    static ores::inbox::messaging::inbox_notification read_json(std::string_view text);
    static std::string write_msgpack(const ores::inbox::messaging::inbox_notification& v);
    static ores::inbox::messaging::inbox_notification read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_INBOX_API_EXPORT wire_traits<ores::inbox::messaging::raise_notification_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::inbox::messaging::raise_notification_request& v);
    static ores::inbox::messaging::raise_notification_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::inbox::messaging::raise_notification_request& v);
    static ores::inbox::messaging::raise_notification_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_INBOX_API_EXPORT wire_traits<ores::inbox::messaging::raise_notification_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::inbox::messaging::raise_notification_response& v);
    static ores::inbox::messaging::raise_notification_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::inbox::messaging::raise_notification_response& v);
    static ores::inbox::messaging::raise_notification_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_INBOX_API_EXPORT wire_traits<ores::inbox::messaging::list_my_notifications_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::inbox::messaging::list_my_notifications_request& v);
    static ores::inbox::messaging::list_my_notifications_request read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::inbox::messaging::list_my_notifications_request& v);
    static ores::inbox::messaging::list_my_notifications_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_INBOX_API_EXPORT wire_traits<ores::inbox::messaging::list_my_notifications_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::inbox::messaging::list_my_notifications_response& v);
    static ores::inbox::messaging::list_my_notifications_response read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::inbox::messaging::list_my_notifications_response& v);
    static ores::inbox::messaging::list_my_notifications_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_INBOX_API_EXPORT
    wire_traits<ores::inbox::messaging::count_unread_notifications_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::inbox::messaging::count_unread_notifications_request& v);
    static ores::inbox::messaging::count_unread_notifications_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::inbox::messaging::count_unread_notifications_request& v);
    static ores::inbox::messaging::count_unread_notifications_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_INBOX_API_EXPORT
    wire_traits<ores::inbox::messaging::count_unread_notifications_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::inbox::messaging::count_unread_notifications_response& v);
    static ores::inbox::messaging::count_unread_notifications_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::inbox::messaging::count_unread_notifications_response& v);
    static ores::inbox::messaging::count_unread_notifications_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_INBOX_API_EXPORT wire_traits<ores::inbox::messaging::mark_notifications_read_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::inbox::messaging::mark_notifications_read_request& v);
    static ores::inbox::messaging::mark_notifications_read_request read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::inbox::messaging::mark_notifications_read_request& v);
    static ores::inbox::messaging::mark_notifications_read_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_INBOX_API_EXPORT wire_traits<ores::inbox::messaging::mark_notifications_read_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::inbox::messaging::mark_notifications_read_response& v);
    static ores::inbox::messaging::mark_notifications_read_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::inbox::messaging::mark_notifications_read_response& v);
    static ores::inbox::messaging::mark_notifications_read_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_INBOX_API_EXPORT wire_traits<ores::inbox::messaging::clear_notifications_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::inbox::messaging::clear_notifications_request& v);
    static ores::inbox::messaging::clear_notifications_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::inbox::messaging::clear_notifications_request& v);
    static ores::inbox::messaging::clear_notifications_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_INBOX_API_EXPORT wire_traits<ores::inbox::messaging::clear_notifications_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::inbox::messaging::clear_notifications_response& v);
    static ores::inbox::messaging::clear_notifications_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::inbox::messaging::clear_notifications_response& v);
    static ores::inbox::messaging::clear_notifications_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
