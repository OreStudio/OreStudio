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

#include "ores.utility/domain/protocol.hpp"
#include <string>
#include <vector>

namespace ores::inbox::messaging {

/**
 * @brief One value a notification's message names.
 */
struct notification_argument_value {
    std::string name;
    std::string value;
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
    std::string raised_at;
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
    std::string read_at;
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
    static constexpr std::string_view nats_subject = "inbox.v1.notifications.raise";
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
};

struct raise_notification_response {
    ores::utility::domain::result result;
    std::string notification_id;
    /**
     * @brief How many people the notification reached.
     */
    int recipient_count = 0;
};

/**
 * @brief Reads the signed-in person's notifications that they have not
 * cleared, newest first.
 */
struct list_my_notifications_request {
    using response_type = struct list_my_notifications_response;
    static constexpr std::string_view nats_subject = "inbox.v1.notifications.mine";
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
};

struct list_my_notifications_response {
    ores::utility::domain::result result;
    std::vector<inbox_notification> notifications;
    int total = 0;
};

/**
 * @brief Reads how many of the signed-in person's notifications are unread,
 * for the bell on every screen.
 */
struct count_unread_notifications_request {
    using response_type = struct count_unread_notifications_response;
    static constexpr std::string_view nats_subject = "inbox.v1.notifications.unread-count";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
};

struct count_unread_notifications_response {
    ores::utility::domain::result result;
    int unread = 0;
};

/**
 * @brief Marks some or all of the signed-in person's notifications read.
 */
struct mark_notifications_read_request {
    using response_type = struct mark_notifications_read_response;
    static constexpr std::string_view nats_subject = "inbox.v1.notifications.mark-read";
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
};

struct mark_notifications_read_response {
    ores::utility::domain::result result;
    int marked = 0;
};

/**
 * @brief Removes notifications from the signed-in person's list. A cleared
 * notification is also read.
 */
struct clear_notifications_request {
    using response_type = struct clear_notifications_response;
    static constexpr std::string_view nats_subject = "inbox.v1.notifications.clear";
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
};

struct clear_notifications_response {
    ores::utility::domain::result result;
    int cleared = 0;
};

}

#endif
