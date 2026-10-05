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
#ifndef ORES_INBOX_DOMAIN_NOTIFICATION_ARGUMENT_HPP
#define ORES_INBOX_DOMAIN_NOTIFICATION_ARGUMENT_HPP

#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <string>
#include <string_view>

namespace ores::inbox::domain {

/**
 * @brief One value a notification's message names.
 *
 * The values a notification's message names, one row each: the role, the tenant,
 * the step. The screen renders the kind's message with them in the reader's
 * language, so a notification stores no finished sentence and no document.
 *
 * A row is identified by the notification and the argument's name.
 */
struct notification_argument final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    std::string tenant_id;

    /**
     * @brief The notification the argument belongs to.
     *
     * References ores_inbox_notifications_tbl.id (soft FK).
     */
    boost::uuids::uuid notification_id;

    /**
     * @brief The argument's name, as the message names it.
     *
     * Examples: 'role', 'tenant', 'step'.
     */
    std::string name;

    /**
     * @brief The argument's value, as the message shows it.
     */
    std::string value;

    /**
     * @brief Username of the person who last modified this notification argument.
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
    friend bool operator==(const notification_argument&, const notification_argument&) = default;
};

/**
 * @brief Dispatch-key identifier for notification_argument, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const notification_argument&) {
    return "ores.inbox.notification_argument";
}

}

#endif
