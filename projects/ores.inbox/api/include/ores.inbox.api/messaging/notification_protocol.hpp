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
#ifndef ORES_INBOX_API_MESSAGING_NOTIFICATION_PROTOCOL_HPP
#define ORES_INBOX_API_MESSAGING_NOTIFICATION_PROTOCOL_HPP

#include "ores.inbox.api/domain/notification.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <boost/uuid/uuid.hpp>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::inbox::messaging {

struct notification_key {
    boost::uuids::uuid id;
};

struct notification_write {
    boost::uuids::uuid id;
    std::string kind_code;
    boost::uuids::uuid raised_by;
    std::chrono::system_clock::time_point raised_at;
    std::string link_route;
    std::optional<std::string> link_id;
    std::optional<std::string> audience_permission_code;
};

struct notification_change {
    notification_write write;
    ores::utility::domain::precondition precondition;
};

struct notification_removal {
    notification_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;
};

struct notification_lookup {
    notification_key key;
    std::optional<ores::inbox::domain::notification> notification;
};

struct notifications_filter {
    std::optional<std::string> kind_code;
    std::optional<std::vector<boost::uuids::uuid>> id_one_of;
    std::optional<std::vector<std::string>> kind_code_one_of;
};

struct notification_event {
    boost::uuids::uuid event_id;
    notification_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;
};

struct notification_version_key {
    notification_key notification;
    std::uint32_t version;
};

struct notification_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;
};

struct list_notifications_request {
    using response_type = struct list_notifications_response;
    static constexpr std::string_view nats_subject = "inbox.v1.notifications.list";
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
    std::optional<notifications_filter> filter;
};

struct list_notifications_response {
    ores::utility::domain::result result;
    std::vector<ores::inbox::domain::notification> notifications;
    std::uint64_t total;
};

struct get_notification_request {
    using response_type = struct get_notification_response;
    static constexpr std::string_view nats_subject = "inbox.v1.notifications.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    notification_key key;
};

struct get_notification_response {
    ores::utility::domain::result result;
    std::optional<ores::inbox::domain::notification> notification;
};

struct get_many_notifications_request {
    using response_type = struct get_many_notifications_response;
    static constexpr std::string_view nats_subject = "inbox.v1.notifications.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<notification_key> keys;
};

struct get_many_notifications_response {
    ores::utility::domain::result result;
    std::vector<notification_lookup> entries;
};

struct put_notification_request {
    using response_type = struct put_notification_response;
    static constexpr std::string_view nats_subject = "inbox.v1.notifications.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    notification_change change;
    ores::utility::domain::change_intent intent;
};

struct put_notification_response {
    ores::utility::domain::result result;
    std::optional<ores::inbox::domain::notification> notification;
};

struct put_many_notifications_request {
    using response_type = struct put_many_notifications_response;
    static constexpr std::string_view nats_subject = "inbox.v1.notifications.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<notification_change> changes;
    ores::utility::domain::change_intent intent;
};

struct put_many_notifications_response {
    ores::utility::domain::result result;
    std::vector<ores::inbox::domain::notification> notifications;
};

struct delete_notification_request {
    using response_type = struct delete_notification_response;
    static constexpr std::string_view nats_subject = "inbox.v1.notifications.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    notification_removal removal;
    ores::utility::domain::change_intent intent;
};

struct delete_notification_response {
    ores::utility::domain::result result;
};

struct delete_many_notifications_request {
    using response_type = struct delete_many_notifications_response;
    static constexpr std::string_view nats_subject = "inbox.v1.notifications.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<notification_removal> removals;
    ores::utility::domain::change_intent intent;
};

struct delete_many_notifications_response {
    ores::utility::domain::result result;
};

struct list_by_kind_code_notifications_request {
    using response_type = struct list_by_kind_code_notifications_response;
    static constexpr std::string_view nats_subject = "inbox.v1.notifications.list_by_kind_code";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::string kind_code;
    ores::utility::domain::scope scope = ores::utility::domain::scope::direct;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<notifications_filter> filter;
};

struct list_by_kind_code_notifications_response {
    ores::utility::domain::result result;
    std::vector<ores::inbox::domain::notification> notifications;
    std::uint64_t total;
};

struct list_notification_versions_request {
    using response_type = struct list_notification_versions_response;
    static constexpr std::string_view nats_subject = "inbox.v1.notifications_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    notification_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<notification_versions_filter> filter;
};

struct list_notification_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::inbox::domain::notification> versions;
    std::uint64_t total;
};

struct get_notification_version_request {
    using response_type = struct get_notification_version_response;
    static constexpr std::string_view nats_subject = "inbox.v1.notifications_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    notification_version_key key;
};

struct get_notification_version_response {
    ores::utility::domain::result result;
    std::optional<ores::inbox::domain::notification> version;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace notification_event_subjects {
inline constexpr std::string_view created = "inbox.v1.notifications_events.created";
inline constexpr std::string_view updated = "inbox.v1.notifications_events.updated";
inline constexpr std::string_view deleted = "inbox.v1.notifications_events.deleted";
}

}

#endif
