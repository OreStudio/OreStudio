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
#ifndef ORES_INBOX_API_DOMAIN_NOTIFICATION_DELIVERY_HPP
#define ORES_INBOX_API_DOMAIN_NOTIFICATION_DELIVERY_HPP

#include "ores.inbox.api/domain/delivery_outcome.hpp"
#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <optional>
#include <string>
#include <string_view>

namespace ores::inbox::domain {

/**
 * @brief One attempt to reach one recipient through one channel.
 *
 * One attempt to reach one recipient through one channel, and its outcome. A
 * recipient has many: a failed mail is retried, and each attempt is a row of its
 * own, so "were they told?" has an answer. A failed delivery never fails the
 * activity that raised the notification.
 *
 * outcome is pending, delivered or failed, the C++ enum
 * domain::delivery_outcome. failure_reason says why a failed attempt failed,
 * and is null otherwise.
 */
struct notification_delivery final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief UUID primary key for the delivery.
     */
    boost::uuids::uuid id;

    /**
     * @brief The notification delivered. FK reference to ores_inbox_notifications_tbl.
     */
    boost::uuids::uuid notification_id;

    /**
     * @brief The recipient reached. FK reference to ores_iam_accounts_tbl.
     */
    boost::uuids::uuid account_id;

    /**
     * @brief The channel used. FK reference to ores_inbox_notification_channels_tbl, which the
     * system tenant holds.
     */
    std::string channel_code;

    /**
     * @brief When the attempt was made.
     */
    std::chrono::system_clock::time_point attempted_at = {};

    /**
     * @brief What came of the attempt: pending, delivered or failed.
     */
    domain::delivery_outcome outcome = domain::delivery_outcome::pending;

    /**
     * @brief Why a failed attempt failed, or null when it did not fail.
     */
    std::optional<std::string> failure_reason;

    /**
     * @brief Username of the person who last modified this notification delivery.
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
     *
     * The transaction-time window's start, which the store sets from its own
     * clock. It travels with the audit members because it is only ever read
     * with them: the history builder takes a version type that carries an
     * actor *and* this timestamp, so an entity without the actor has no use
     * for the timestamp either.
     */
    std::chrono::system_clock::time_point recorded_at;

    /**
     * @brief Value equality.
     *
     * Every generated domain type is a value: two of them are equal when their
     * members are, whatever the entity means. A test that round-trips one
     * through the wire asserts exactly that, so equality is part of the shape
     * rather than something each entity decides -- an entity without it cannot
     * be round-trip tested at all, which is why the omission went unnoticed
     * until the diff payloads were the first generated types to have a test.
     */
    friend bool operator==(const notification_delivery&, const notification_delivery&) = default;
};

/**
 * @brief Dispatch-key identifier for notification_delivery, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const notification_delivery&) {
    return "ores.inbox.notification_delivery";
}

}

#endif
