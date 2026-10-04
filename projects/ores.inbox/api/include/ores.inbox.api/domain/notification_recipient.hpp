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
 * Template: cpp_domain_type_class.hpp.mustache
 * To modify, update the template and regenerate.
 */
#ifndef ORES_INBOX_DOMAIN_NOTIFICATION_RECIPIENT_HPP
#define ORES_INBOX_DOMAIN_NOTIFICATION_RECIPIENT_HPP

#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <optional>
#include <string>
#include <string_view>

namespace ores::inbox::domain {

/**
 * @brief One person a notification reaches, with their read state.
 *
 * Each person a notification reaches has their own row, kept until they read or
 * clear it, and a person reads only their own. read_at and cleared_at are the
 * person's read state: null until they read or clear it. The unread count on
 * every screen counts a person's rows with no read_at.
 *
 * A row is identified by the notification and the account.
 */
struct notification_recipient final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    std::string tenant_id;

    /**
     * @brief The notification the person receives.
     *
     * References ores_inbox_notifications_tbl.id (soft FK).
     */
    boost::uuids::uuid notification_id;

    /**
     * @brief The account that receives the notification.
     *
     * References ores_iam_accounts_tbl.id (soft FK).
     */
    boost::uuids::uuid account_id;

    /**
     * @brief When the person read it, or null while it is unread.
     */
    std::optional<std::chrono::system_clock::time_point> read_at;

    /**
     * @brief When the person cleared it from their list, or null while it is shown.
     */
    std::optional<std::chrono::system_clock::time_point> cleared_at;

    /**
     * @brief Username of the person who last modified this notification recipient.
     */
    std::string modified_by;

    /**
     * @brief Username of the account that performed this action.
     */
    std::string performed_by;

    /**
     * @brief Code identifying the reason for the change.
     *
     * References change_reasons table (soft FK).
     */
    std::string change_reason_code;

    /**
     * @brief Free-text commentary explaining the change.
     */
    std::string change_commentary;

    /**
     * @brief Timestamp when this version of the record was recorded.
     */
    std::chrono::system_clock::time_point recorded_at;

    /**
     * @brief Value equality, on the same terms as an entity's.
     */
    friend bool operator==(const notification_recipient&, const notification_recipient&) = default;
};

/**
 * @brief Dispatch-key identifier for notification_recipient, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const notification_recipient&) {
    return "ores.inbox.notification_recipient";
}

}

#endif
