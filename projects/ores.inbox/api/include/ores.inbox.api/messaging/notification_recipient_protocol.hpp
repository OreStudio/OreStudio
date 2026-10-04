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
#ifndef ORES_INBOX_API_MESSAGING_NOTIFICATION_RECIPIENT_PROTOCOL_HPP
#define ORES_INBOX_API_MESSAGING_NOTIFICATION_RECIPIENT_PROTOCOL_HPP

#include "ores.inbox.api/domain/notification_recipient.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <boost/uuid/uuid.hpp>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::inbox::messaging {

struct notification_recipient_key {
    boost::uuids::uuid notification_id;
    boost::uuids::uuid account_id;
};

struct notification_recipient_write {
    boost::uuids::uuid notification_id;
    boost::uuids::uuid account_id;
    std::optional<std::chrono::system_clock::time_point> read_at;
    std::optional<std::chrono::system_clock::time_point> cleared_at;
};

struct notification_recipient_change {
    notification_recipient_write write;
    ores::utility::domain::precondition precondition;
};

struct notification_recipient_removal {
    notification_recipient_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;
};

struct notification_recipient_lookup {
    notification_recipient_key key;
    std::optional<ores::inbox::domain::notification_recipient> notification_recipient;
};

struct notification_recipients_filter {
    std::optional<boost::uuids::uuid> account_id;
    std::optional<std::vector<boost::uuids::uuid>> account_id_one_of;
};

struct list_notification_recipients_request {
    using response_type = struct list_notification_recipients_response;
    static constexpr std::string_view nats_subject = "inbox.v1.notification_recipients.list";
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
    std::optional<notification_recipients_filter> filter;
};

struct list_notification_recipients_response {
    ores::utility::domain::result result;
    std::vector<ores::inbox::domain::notification_recipient> notification_recipients;
    std::uint64_t total;
};

struct get_notification_recipient_request {
    using response_type = struct get_notification_recipient_response;
    static constexpr std::string_view nats_subject = "inbox.v1.notification_recipients.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    notification_recipient_key key;
};

struct get_notification_recipient_response {
    ores::utility::domain::result result;
    std::optional<ores::inbox::domain::notification_recipient> notification_recipient;
};

struct get_many_notification_recipients_request {
    using response_type = struct get_many_notification_recipients_response;
    static constexpr std::string_view nats_subject = "inbox.v1.notification_recipients.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<notification_recipient_key> keys;
};

struct get_many_notification_recipients_response {
    ores::utility::domain::result result;
    std::vector<notification_recipient_lookup> entries;
};

struct put_notification_recipient_request {
    using response_type = struct put_notification_recipient_response;
    static constexpr std::string_view nats_subject = "inbox.v1.notification_recipients.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    notification_recipient_change change;
    ores::utility::domain::change_intent intent;
};

struct put_notification_recipient_response {
    ores::utility::domain::result result;
    std::optional<ores::inbox::domain::notification_recipient> notification_recipient;
};

struct put_many_notification_recipients_request {
    using response_type = struct put_many_notification_recipients_response;
    static constexpr std::string_view nats_subject = "inbox.v1.notification_recipients.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<notification_recipient_change> changes;
    ores::utility::domain::change_intent intent;
};

struct put_many_notification_recipients_response {
    ores::utility::domain::result result;
    std::vector<ores::inbox::domain::notification_recipient> notification_recipients;
};

struct delete_notification_recipient_request {
    using response_type = struct delete_notification_recipient_response;
    static constexpr std::string_view nats_subject = "inbox.v1.notification_recipients.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    notification_recipient_removal removal;
    ores::utility::domain::change_intent intent;
};

struct delete_notification_recipient_response {
    ores::utility::domain::result result;
};

struct delete_many_notification_recipients_request {
    using response_type = struct delete_many_notification_recipients_response;
    static constexpr std::string_view nats_subject = "inbox.v1.notification_recipients.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<notification_recipient_removal> removals;
    ores::utility::domain::change_intent intent;
};

struct delete_many_notification_recipients_response {
    ores::utility::domain::result result;
};

struct list_by_account_id_notification_recipients_request {
    using response_type = struct list_by_account_id_notification_recipients_response;
    static constexpr std::string_view nats_subject =
        "inbox.v1.notification_recipients.list_by_account_id";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    boost::uuids::uuid account_id;
    ores::utility::domain::scope scope = ores::utility::domain::scope::direct;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<notification_recipients_filter> filter;
};

struct list_by_account_id_notification_recipients_response {
    ores::utility::domain::result result;
    std::vector<ores::inbox::domain::notification_recipient> notification_recipients;
    std::uint64_t total;
};

}

#endif
