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
#ifndef ORES_INBOX_API_MESSAGING_NOTIFICATION_CHANNEL_PROTOCOL_HPP
#define ORES_INBOX_API_MESSAGING_NOTIFICATION_CHANNEL_PROTOCOL_HPP

#include "ores.inbox.api/domain/notification_channel.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <boost/uuid/uuid.hpp>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::inbox::messaging {

struct notification_channel_key {
    std::string code;
};

struct notification_channel_write {
    std::string code;
    std::string name;
    std::string description;
    int display_order;
};

struct notification_channel_change {
    notification_channel_write write;
    ores::utility::domain::precondition precondition;
};

struct notification_channel_removal {
    notification_channel_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;
};

struct notification_channel_lookup {
    notification_channel_key key;
    std::optional<ores::inbox::domain::notification_channel> notification_channel;
};

struct notification_channels_filter {
    std::optional<std::vector<std::string>> code_one_of;
};

struct notification_channel_event {
    boost::uuids::uuid event_id;
    notification_channel_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;
};

struct notification_channel_version_key {
    notification_channel_key notification_channel;
    std::uint32_t version;
};

struct notification_channel_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;
};

struct list_notification_channels_request {
    using response_type = struct list_notification_channels_response;
    static constexpr std::string_view nats_subject = "inbox.v1.notification_channels.list";
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
    std::optional<notification_channels_filter> filter;
    std::optional<std::string> as_of;
};

struct list_notification_channels_response {
    ores::utility::domain::result result;
    std::vector<ores::inbox::domain::notification_channel> channels;
    std::uint64_t total;
};

struct get_notification_channel_request {
    using response_type = struct get_notification_channel_response;
    static constexpr std::string_view nats_subject = "inbox.v1.notification_channels.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    notification_channel_key key;
};

struct get_notification_channel_response {
    ores::utility::domain::result result;
    std::optional<ores::inbox::domain::notification_channel> notification_channel;
};

struct get_many_notification_channels_request {
    using response_type = struct get_many_notification_channels_response;
    static constexpr std::string_view nats_subject = "inbox.v1.notification_channels.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<notification_channel_key> keys;
};

struct get_many_notification_channels_response {
    ores::utility::domain::result result;
    std::vector<notification_channel_lookup> entries;
};

struct put_notification_channel_request {
    using response_type = struct put_notification_channel_response;
    static constexpr std::string_view nats_subject = "inbox.v1.notification_channels.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    notification_channel_change change;
    ores::utility::domain::change_intent intent;
};

struct put_notification_channel_response {
    ores::utility::domain::result result;
    std::optional<ores::inbox::domain::notification_channel> notification_channel;
};

struct put_many_notification_channels_request {
    using response_type = struct put_many_notification_channels_response;
    static constexpr std::string_view nats_subject = "inbox.v1.notification_channels.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<notification_channel_change> changes;
    ores::utility::domain::change_intent intent;
};

struct put_many_notification_channels_response {
    ores::utility::domain::result result;
    std::vector<ores::inbox::domain::notification_channel> channels;
};

struct delete_notification_channel_request {
    using response_type = struct delete_notification_channel_response;
    static constexpr std::string_view nats_subject = "inbox.v1.notification_channels.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    notification_channel_removal removal;
    ores::utility::domain::change_intent intent;
};

struct delete_notification_channel_response {
    ores::utility::domain::result result;
};

struct delete_many_notification_channels_request {
    using response_type = struct delete_many_notification_channels_response;
    static constexpr std::string_view nats_subject = "inbox.v1.notification_channels.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<notification_channel_removal> removals;
    ores::utility::domain::change_intent intent;
};

struct delete_many_notification_channels_response {
    ores::utility::domain::result result;
};

struct list_notification_channel_versions_request {
    using response_type = struct list_notification_channel_versions_response;
    static constexpr std::string_view nats_subject = "inbox.v1.notification_channels_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    notification_channel_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<notification_channel_versions_filter> filter;
};

struct list_notification_channel_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::inbox::domain::notification_channel> versions;
    std::uint64_t total;
};

struct get_notification_channel_version_request {
    using response_type = struct get_notification_channel_version_response;
    static constexpr std::string_view nats_subject = "inbox.v1.notification_channels_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    notification_channel_version_key key;
};

struct get_notification_channel_version_response {
    ores::utility::domain::result result;
    std::optional<ores::inbox::domain::notification_channel> version;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace notification_channel_event_subjects {
inline constexpr std::string_view created = "inbox.v1.notification_channels_events.created";
inline constexpr std::string_view updated = "inbox.v1.notification_channels_events.updated";
inline constexpr std::string_view deleted = "inbox.v1.notification_channels_events.deleted";
}

}

#endif
