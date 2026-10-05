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
#ifndef ORES_INBOX_CORE_SERVICE_NOTIFICATION_CENTER_HPP
#define ORES_INBOX_CORE_SERVICE_NOTIFICATION_CENTER_HPP

#include "ores.database/domain/context.hpp"
#include "ores.inbox.api/messaging/notification_operations_protocol.hpp"
#include "ores.inbox.core/export.hpp"
#include "ores.logging/make_logger.hpp"
#include <boost/uuid/uuid.hpp>
#include <optional>
#include <string>
#include <vector>

namespace ores::inbox::service {

/**
 * @brief What a raise came to: the notification and how many it reached.
 */
struct raise_result {
    std::string notification_id;
    int recipient_count = 0;
};

/**
 * @brief One page of a person's notifications and the size of the list.
 */
struct notification_page {
    std::vector<messaging::inbox_notification> notifications;
    int total = 0;
};

/**
 * @brief Raises notifications and serves a person's own.
 *
 * Raising writes everything in one statement in the database. A permission
 * audience is resolved here, at the moment of raising, to the people of the
 * tenant who hold the permission, with the same wildcard rules a handler's
 * permission check applies.
 */
class ORES_INBOX_CORE_EXPORT notification_center {
private:
    [[nodiscard]] static auto& lg() {
        static auto instance = ores::logging::make_logger("ores.inbox.service.notification_center");
        return instance;
    }

public:
    explicit notification_center(ores::database::context ctx);

    /**
     * @brief The account the context's actor signs in as, if any.
     */
    std::optional<boost::uuids::uuid> actor_account_id();

    /**
     * @brief Whether a kind of notification exists.
     */
    bool kind_exists(const std::string& code);

    /**
     * @brief The people of the tenant who hold a permission.
     */
    std::vector<std::string> holders_of(const std::string& permission_code);

    /**
     * @brief Writes a notification for the given people.
     */
    raise_result raise(const messaging::raise_notification_request& req,
                       const std::vector<std::string>& recipient_ids,
                       const boost::uuids::uuid& raised_by);

    /**
     * @brief A person's notifications that they have not cleared, newest
     * first.
     */
    notification_page
    mine(const boost::uuids::uuid& account_id, bool unread_only, int offset, int limit);

    /**
     * @brief How many of a person's notifications are unread.
     */
    int unread(const boost::uuids::uuid& account_id);

    /**
     * @brief Marks a person's notifications read: the ones named, or every
     * unread one.
     */
    int mark_read(const boost::uuids::uuid& account_id, const std::vector<std::string>& ids);

    /**
     * @brief Clears a person's notifications: the ones named, or every read
     * one.
     */
    int clear(const boost::uuids::uuid& account_id, const std::vector<std::string>& ids);

private:
    ores::database::context ctx_;
};

}

#endif
