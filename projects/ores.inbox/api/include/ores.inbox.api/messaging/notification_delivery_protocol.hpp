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
#ifndef ORES_INBOX_API_MESSAGING_NOTIFICATION_DELIVERY_PROTOCOL_HPP
#define ORES_INBOX_API_MESSAGING_NOTIFICATION_DELIVERY_PROTOCOL_HPP

#include "ores.inbox.api/domain/notification_delivery.hpp"
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

struct notification_delivery_key {
    boost::uuids::uuid id;

    friend bool operator==(const notification_delivery_key&,
                           const notification_delivery_key&) = default;
};

struct notification_delivery_write {
    boost::uuids::uuid id;
    boost::uuids::uuid notification_id;
    boost::uuids::uuid account_id;
    std::string channel_code;
    std::chrono::system_clock::time_point attempted_at;
    domain::delivery_outcome outcome;
    std::optional<std::string> failure_reason;

    friend bool operator==(const notification_delivery_write&,
                           const notification_delivery_write&) = default;
};

struct notification_delivery_change {
    notification_delivery_write write;
    ores::utility::domain::precondition precondition;

    friend bool operator==(const notification_delivery_change&,
                           const notification_delivery_change&) = default;
};

struct notification_delivery_removal {
    notification_delivery_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;

    friend bool operator==(const notification_delivery_removal&,
                           const notification_delivery_removal&) = default;
};

struct notification_delivery_lookup {
    notification_delivery_key key;
    std::optional<ores::inbox::domain::notification_delivery> notification_delivery;

    friend bool operator==(const notification_delivery_lookup&,
                           const notification_delivery_lookup&) = default;
};

struct notification_deliveries_filter {
    std::optional<boost::uuids::uuid> notification_id;
    std::optional<std::vector<boost::uuids::uuid>> id_one_of;
    std::optional<std::vector<boost::uuids::uuid>> notification_id_one_of;

    friend bool operator==(const notification_deliveries_filter&,
                           const notification_deliveries_filter&) = default;
};

struct notification_delivery_event {
    boost::uuids::uuid event_id;
    notification_delivery_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;

    friend bool operator==(const notification_delivery_event&,
                           const notification_delivery_event&) = default;
};

struct notification_delivery_version_key {
    notification_delivery_key notification_delivery;
    std::uint32_t version;

    friend bool operator==(const notification_delivery_version_key&,
                           const notification_delivery_version_key&) = default;
};

struct notification_delivery_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;

    friend bool operator==(const notification_delivery_versions_filter&,
                           const notification_delivery_versions_filter&) = default;
};

struct list_notification_deliveries_request {
    using response_type = struct list_notification_deliveries_response;
    static constexpr std::string_view nats_subject = "inbox.v1.notification_deliveries.list";
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
    std::optional<notification_deliveries_filter> filter;
    std::optional<std::string> as_of;

    friend bool operator==(const list_notification_deliveries_request&,
                           const list_notification_deliveries_request&) = default;
};

struct list_notification_deliveries_response {
    ores::utility::domain::result result;
    std::vector<ores::inbox::domain::notification_delivery> deliveries;
    std::uint64_t total;

    friend bool operator==(const list_notification_deliveries_response&,
                           const list_notification_deliveries_response&) = default;
};

struct get_notification_delivery_request {
    using response_type = struct get_notification_delivery_response;
    static constexpr std::string_view nats_subject = "inbox.v1.notification_deliveries.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    notification_delivery_key key;

    friend bool operator==(const get_notification_delivery_request&,
                           const get_notification_delivery_request&) = default;
};

struct get_notification_delivery_response {
    ores::utility::domain::result result;
    std::optional<ores::inbox::domain::notification_delivery> notification_delivery;

    friend bool operator==(const get_notification_delivery_response&,
                           const get_notification_delivery_response&) = default;
};

struct get_many_notification_deliveries_request {
    using response_type = struct get_many_notification_deliveries_response;
    static constexpr std::string_view nats_subject = "inbox.v1.notification_deliveries.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<notification_delivery_key> keys;

    friend bool operator==(const get_many_notification_deliveries_request&,
                           const get_many_notification_deliveries_request&) = default;
};

struct get_many_notification_deliveries_response {
    ores::utility::domain::result result;
    std::vector<notification_delivery_lookup> entries;

    friend bool operator==(const get_many_notification_deliveries_response&,
                           const get_many_notification_deliveries_response&) = default;
};

struct put_notification_delivery_request {
    using response_type = struct put_notification_delivery_response;
    static constexpr std::string_view nats_subject = "inbox.v1.notification_deliveries.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    notification_delivery_change change;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const put_notification_delivery_request&,
                           const put_notification_delivery_request&) = default;
};

struct put_notification_delivery_response {
    ores::utility::domain::result result;
    std::optional<ores::inbox::domain::notification_delivery> notification_delivery;

    friend bool operator==(const put_notification_delivery_response&,
                           const put_notification_delivery_response&) = default;
};

struct put_many_notification_deliveries_request {
    using response_type = struct put_many_notification_deliveries_response;
    static constexpr std::string_view nats_subject = "inbox.v1.notification_deliveries.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<notification_delivery_change> changes;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const put_many_notification_deliveries_request&,
                           const put_many_notification_deliveries_request&) = default;
};

struct put_many_notification_deliveries_response {
    ores::utility::domain::result result;
    std::vector<ores::inbox::domain::notification_delivery> deliveries;

    friend bool operator==(const put_many_notification_deliveries_response&,
                           const put_many_notification_deliveries_response&) = default;
};

struct delete_notification_delivery_request {
    using response_type = struct delete_notification_delivery_response;
    static constexpr std::string_view nats_subject = "inbox.v1.notification_deliveries.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    notification_delivery_removal removal;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const delete_notification_delivery_request&,
                           const delete_notification_delivery_request&) = default;
};

struct delete_notification_delivery_response {
    ores::utility::domain::result result;

    friend bool operator==(const delete_notification_delivery_response&,
                           const delete_notification_delivery_response&) = default;
};

struct delete_many_notification_deliveries_request {
    using response_type = struct delete_many_notification_deliveries_response;
    static constexpr std::string_view nats_subject = "inbox.v1.notification_deliveries.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<notification_delivery_removal> removals;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const delete_many_notification_deliveries_request&,
                           const delete_many_notification_deliveries_request&) = default;
};

struct delete_many_notification_deliveries_response {
    ores::utility::domain::result result;

    friend bool operator==(const delete_many_notification_deliveries_response&,
                           const delete_many_notification_deliveries_response&) = default;
};

struct list_by_notification_id_notification_deliveries_request {
    using response_type = struct list_by_notification_id_notification_deliveries_response;
    static constexpr std::string_view nats_subject =
        "inbox.v1.notification_deliveries.list_by_notification_id";
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
    std::optional<notification_deliveries_filter> filter;

    friend bool
    operator==(const list_by_notification_id_notification_deliveries_request&,
               const list_by_notification_id_notification_deliveries_request&) = default;
};

struct list_by_notification_id_notification_deliveries_response {
    ores::utility::domain::result result;
    std::vector<ores::inbox::domain::notification_delivery> deliveries;
    std::uint64_t total;

    friend bool
    operator==(const list_by_notification_id_notification_deliveries_response&,
               const list_by_notification_id_notification_deliveries_response&) = default;
};

struct list_notification_delivery_versions_request {
    using response_type = struct list_notification_delivery_versions_response;
    static constexpr std::string_view nats_subject =
        "inbox.v1.notification_deliveries_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    notification_delivery_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<notification_delivery_versions_filter> filter;

    friend bool operator==(const list_notification_delivery_versions_request&,
                           const list_notification_delivery_versions_request&) = default;
};

struct list_notification_delivery_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::inbox::domain::notification_delivery> versions;
    std::uint64_t total;

    friend bool operator==(const list_notification_delivery_versions_response&,
                           const list_notification_delivery_versions_response&) = default;
};

struct get_notification_delivery_version_request {
    using response_type = struct get_notification_delivery_version_response;
    static constexpr std::string_view nats_subject =
        "inbox.v1.notification_deliveries_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    notification_delivery_version_key key;

    friend bool operator==(const get_notification_delivery_version_request&,
                           const get_notification_delivery_version_request&) = default;
};

struct get_notification_delivery_version_response {
    ores::utility::domain::result result;
    std::optional<ores::inbox::domain::notification_delivery> version;

    friend bool operator==(const get_notification_delivery_version_response&,
                           const get_notification_delivery_version_response&) = default;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace notification_delivery_event_subjects {
inline constexpr std::string_view created = "inbox.v1.events.notification_deliveries.created";
inline constexpr std::string_view updated = "inbox.v1.events.notification_deliveries.updated";
inline constexpr std::string_view deleted = "inbox.v1.events.notification_deliveries.deleted";
}
/**
 * @brief The members of notification_delivery_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const notification_delivery_key&) {
    return std::tuple{
        utility::codec::field{"id", &notification_delivery_key::id},
    };
}

/**
 * @brief The members of notification_delivery_write, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const notification_delivery_write&) {
    return std::tuple{
        utility::codec::field{"id", &notification_delivery_write::id},
        utility::codec::field{"notification_id", &notification_delivery_write::notification_id},
        utility::codec::field{"account_id", &notification_delivery_write::account_id},
        utility::codec::field{"channel_code", &notification_delivery_write::channel_code},
        utility::codec::field{"attempted_at", &notification_delivery_write::attempted_at},
        utility::codec::field{"outcome", &notification_delivery_write::outcome},
        utility::codec::field{"failure_reason", &notification_delivery_write::failure_reason},
    };
}

/**
 * @brief The members of notification_delivery_change, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const notification_delivery_change&) {
    return std::tuple{
        utility::codec::field{"write", &notification_delivery_change::write},
        utility::codec::field{"precondition", &notification_delivery_change::precondition},
    };
}

/**
 * @brief The members of notification_delivery_removal, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const notification_delivery_removal&) {
    return std::tuple{
        utility::codec::field{"key", &notification_delivery_removal::key},
        utility::codec::field{"precondition", &notification_delivery_removal::precondition},
    };
}

/**
 * @brief The members of notification_delivery_lookup, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const notification_delivery_lookup&) {
    return std::tuple{
        utility::codec::field{"key", &notification_delivery_lookup::key},
        utility::codec::field{"notification_delivery",
                              &notification_delivery_lookup::notification_delivery},
    };
}

/**
 * @brief The members of notification_deliveries_filter, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const notification_deliveries_filter&) {
    return std::tuple{
        utility::codec::field{"notification_id", &notification_deliveries_filter::notification_id},
        utility::codec::field{"id_one_of", &notification_deliveries_filter::id_one_of},
        utility::codec::field{"notification_id_one_of",
                              &notification_deliveries_filter::notification_id_one_of},
    };
}

/**
 * @brief The members of notification_delivery_event, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const notification_delivery_event&) {
    return std::tuple{
        utility::codec::field{"event_id", &notification_delivery_event::event_id},
        utility::codec::field{"key", &notification_delivery_event::key},
        utility::codec::field{"action", &notification_delivery_event::action},
        utility::codec::field{"version", &notification_delivery_event::version},
        utility::codec::field{"occurred_at", &notification_delivery_event::occurred_at},
        utility::codec::field{"correlation_id", &notification_delivery_event::correlation_id},
    };
}

/**
 * @brief The members of notification_delivery_version_key, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const notification_delivery_version_key&) {
    return std::tuple{
        utility::codec::field{"notification_delivery",
                              &notification_delivery_version_key::notification_delivery},
        utility::codec::field{"version", &notification_delivery_version_key::version},
    };
}

/**
 * @brief The members of notification_delivery_versions_filter, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const notification_delivery_versions_filter&) {
    return std::tuple{
        utility::codec::field{"version", &notification_delivery_versions_filter::version},
        utility::codec::field{"from_version", &notification_delivery_versions_filter::from_version},
        utility::codec::field{"to_version", &notification_delivery_versions_filter::to_version},
    };
}

/**
 * @brief The members of list_notification_deliveries_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_notification_deliveries_request&) {
    return std::tuple{
        utility::codec::field{"offset", &list_notification_deliveries_request::offset},
        utility::codec::field{"limit", &list_notification_deliveries_request::limit},
        utility::codec::field{"order", &list_notification_deliveries_request::order},
        utility::codec::field{"filter", &list_notification_deliveries_request::filter},
        utility::codec::field{"as_of", &list_notification_deliveries_request::as_of},
    };
}

/**
 * @brief The members of list_notification_deliveries_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_notification_deliveries_response&) {
    return std::tuple{
        utility::codec::field{"result", &list_notification_deliveries_response::result},
        utility::codec::field{"deliveries", &list_notification_deliveries_response::deliveries},
        utility::codec::field{"total", &list_notification_deliveries_response::total},
    };
}

/**
 * @brief The members of get_notification_delivery_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_notification_delivery_request&) {
    return std::tuple{
        utility::codec::field{"key", &get_notification_delivery_request::key},
    };
}

/**
 * @brief The members of get_notification_delivery_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_notification_delivery_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_notification_delivery_response::result},
        utility::codec::field{"notification_delivery",
                              &get_notification_delivery_response::notification_delivery},
    };
}

/**
 * @brief The members of get_many_notification_deliveries_request, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_many_notification_deliveries_request&) {
    return std::tuple{
        utility::codec::field{"keys", &get_many_notification_deliveries_request::keys},
    };
}

/**
 * @brief The members of get_many_notification_deliveries_response, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_many_notification_deliveries_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_many_notification_deliveries_response::result},
        utility::codec::field{"entries", &get_many_notification_deliveries_response::entries},
    };
}

/**
 * @brief The members of put_notification_delivery_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_notification_delivery_request&) {
    return std::tuple{
        utility::codec::field{"change", &put_notification_delivery_request::change},
        utility::codec::field{"intent", &put_notification_delivery_request::intent},
    };
}

/**
 * @brief The members of put_notification_delivery_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_notification_delivery_response&) {
    return std::tuple{
        utility::codec::field{"result", &put_notification_delivery_response::result},
        utility::codec::field{"notification_delivery",
                              &put_notification_delivery_response::notification_delivery},
    };
}

/**
 * @brief The members of put_many_notification_deliveries_request, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_many_notification_deliveries_request&) {
    return std::tuple{
        utility::codec::field{"changes", &put_many_notification_deliveries_request::changes},
        utility::codec::field{"intent", &put_many_notification_deliveries_request::intent},
    };
}

/**
 * @brief The members of put_many_notification_deliveries_response, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_many_notification_deliveries_response&) {
    return std::tuple{
        utility::codec::field{"result", &put_many_notification_deliveries_response::result},
        utility::codec::field{"deliveries", &put_many_notification_deliveries_response::deliveries},
    };
}

/**
 * @brief The members of delete_notification_delivery_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_notification_delivery_request&) {
    return std::tuple{
        utility::codec::field{"removal", &delete_notification_delivery_request::removal},
        utility::codec::field{"intent", &delete_notification_delivery_request::intent},
    };
}

/**
 * @brief The members of delete_notification_delivery_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_notification_delivery_response&) {
    return std::tuple{
        utility::codec::field{"result", &delete_notification_delivery_response::result},
    };
}

/**
 * @brief The members of delete_many_notification_deliveries_request, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_many_notification_deliveries_request&) {
    return std::tuple{
        utility::codec::field{"removals", &delete_many_notification_deliveries_request::removals},
        utility::codec::field{"intent", &delete_many_notification_deliveries_request::intent},
    };
}

/**
 * @brief The members of delete_many_notification_deliveries_response, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_many_notification_deliveries_response&) {
    return std::tuple{
        utility::codec::field{"result", &delete_many_notification_deliveries_response::result},
    };
}

/**
 * @brief The members of list_by_notification_id_notification_deliveries_request, named as the wire
 * names them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(const list_by_notification_id_notification_deliveries_request&) {
    return std::tuple{
        utility::codec::field{
            "notification_id",
            &list_by_notification_id_notification_deliveries_request::notification_id},
        utility::codec::field{"scope",
                              &list_by_notification_id_notification_deliveries_request::scope},
        utility::codec::field{"offset",
                              &list_by_notification_id_notification_deliveries_request::offset},
        utility::codec::field{"limit",
                              &list_by_notification_id_notification_deliveries_request::limit},
        utility::codec::field{"order",
                              &list_by_notification_id_notification_deliveries_request::order},
        utility::codec::field{"filter",
                              &list_by_notification_id_notification_deliveries_request::filter},
    };
}

/**
 * @brief The members of list_by_notification_id_notification_deliveries_response, named as the wire
 * names them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(const list_by_notification_id_notification_deliveries_response&) {
    return std::tuple{
        utility::codec::field{"result",
                              &list_by_notification_id_notification_deliveries_response::result},
        utility::codec::field{
            "deliveries", &list_by_notification_id_notification_deliveries_response::deliveries},
        utility::codec::field{"total",
                              &list_by_notification_id_notification_deliveries_response::total},
    };
}

/**
 * @brief The members of list_notification_delivery_versions_request, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_notification_delivery_versions_request&) {
    return std::tuple{
        utility::codec::field{"key", &list_notification_delivery_versions_request::key},
        utility::codec::field{"offset", &list_notification_delivery_versions_request::offset},
        utility::codec::field{"limit", &list_notification_delivery_versions_request::limit},
        utility::codec::field{"order", &list_notification_delivery_versions_request::order},
        utility::codec::field{"filter", &list_notification_delivery_versions_request::filter},
    };
}

/**
 * @brief The members of list_notification_delivery_versions_response, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_notification_delivery_versions_response&) {
    return std::tuple{
        utility::codec::field{"result", &list_notification_delivery_versions_response::result},
        utility::codec::field{"versions", &list_notification_delivery_versions_response::versions},
        utility::codec::field{"total", &list_notification_delivery_versions_response::total},
    };
}

/**
 * @brief The members of get_notification_delivery_version_request, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_notification_delivery_version_request&) {
    return std::tuple{
        utility::codec::field{"key", &get_notification_delivery_version_request::key},
    };
}

/**
 * @brief The members of get_notification_delivery_version_response, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_notification_delivery_version_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_notification_delivery_version_response::result},
        utility::codec::field{"version", &get_notification_delivery_version_response::version},
    };
}


}
namespace ores::utility::codec {

template <>
struct ORES_INBOX_API_EXPORT wire_traits<ores::inbox::messaging::notification_delivery_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::inbox::messaging::notification_delivery_key& v);
    static ores::inbox::messaging::notification_delivery_key read_json(std::string_view text);
    static std::string write_msgpack(const ores::inbox::messaging::notification_delivery_key& v);
    static ores::inbox::messaging::notification_delivery_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_INBOX_API_EXPORT wire_traits<ores::inbox::messaging::notification_delivery_write> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::inbox::messaging::notification_delivery_write& v);
    static ores::inbox::messaging::notification_delivery_write read_json(std::string_view text);
    static std::string write_msgpack(const ores::inbox::messaging::notification_delivery_write& v);
    static ores::inbox::messaging::notification_delivery_write
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_INBOX_API_EXPORT wire_traits<ores::inbox::messaging::notification_delivery_change> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::inbox::messaging::notification_delivery_change& v);
    static ores::inbox::messaging::notification_delivery_change read_json(std::string_view text);
    static std::string write_msgpack(const ores::inbox::messaging::notification_delivery_change& v);
    static ores::inbox::messaging::notification_delivery_change
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_INBOX_API_EXPORT wire_traits<ores::inbox::messaging::notification_delivery_removal> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::inbox::messaging::notification_delivery_removal& v);
    static ores::inbox::messaging::notification_delivery_removal read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::inbox::messaging::notification_delivery_removal& v);
    static ores::inbox::messaging::notification_delivery_removal
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_INBOX_API_EXPORT wire_traits<ores::inbox::messaging::notification_delivery_lookup> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::inbox::messaging::notification_delivery_lookup& v);
    static ores::inbox::messaging::notification_delivery_lookup read_json(std::string_view text);
    static std::string write_msgpack(const ores::inbox::messaging::notification_delivery_lookup& v);
    static ores::inbox::messaging::notification_delivery_lookup
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_INBOX_API_EXPORT wire_traits<ores::inbox::messaging::notification_deliveries_filter> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::inbox::messaging::notification_deliveries_filter& v);
    static ores::inbox::messaging::notification_deliveries_filter read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::inbox::messaging::notification_deliveries_filter& v);
    static ores::inbox::messaging::notification_deliveries_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_INBOX_API_EXPORT wire_traits<ores::inbox::messaging::notification_delivery_event> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::inbox::messaging::notification_delivery_event& v);
    static ores::inbox::messaging::notification_delivery_event read_json(std::string_view text);
    static std::string write_msgpack(const ores::inbox::messaging::notification_delivery_event& v);
    static ores::inbox::messaging::notification_delivery_event
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_INBOX_API_EXPORT
    wire_traits<ores::inbox::messaging::notification_delivery_version_key> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::inbox::messaging::notification_delivery_version_key& v);
    static ores::inbox::messaging::notification_delivery_version_key
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::inbox::messaging::notification_delivery_version_key& v);
    static ores::inbox::messaging::notification_delivery_version_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_INBOX_API_EXPORT
    wire_traits<ores::inbox::messaging::notification_delivery_versions_filter> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::inbox::messaging::notification_delivery_versions_filter& v);
    static ores::inbox::messaging::notification_delivery_versions_filter
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::inbox::messaging::notification_delivery_versions_filter& v);
    static ores::inbox::messaging::notification_delivery_versions_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_INBOX_API_EXPORT
    wire_traits<ores::inbox::messaging::list_notification_deliveries_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::inbox::messaging::list_notification_deliveries_request& v);
    static ores::inbox::messaging::list_notification_deliveries_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::inbox::messaging::list_notification_deliveries_request& v);
    static ores::inbox::messaging::list_notification_deliveries_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_INBOX_API_EXPORT
    wire_traits<ores::inbox::messaging::list_notification_deliveries_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::inbox::messaging::list_notification_deliveries_response& v);
    static ores::inbox::messaging::list_notification_deliveries_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::inbox::messaging::list_notification_deliveries_response& v);
    static ores::inbox::messaging::list_notification_deliveries_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_INBOX_API_EXPORT
    wire_traits<ores::inbox::messaging::get_notification_delivery_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::inbox::messaging::get_notification_delivery_request& v);
    static ores::inbox::messaging::get_notification_delivery_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::inbox::messaging::get_notification_delivery_request& v);
    static ores::inbox::messaging::get_notification_delivery_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_INBOX_API_EXPORT
    wire_traits<ores::inbox::messaging::get_notification_delivery_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::inbox::messaging::get_notification_delivery_response& v);
    static ores::inbox::messaging::get_notification_delivery_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::inbox::messaging::get_notification_delivery_response& v);
    static ores::inbox::messaging::get_notification_delivery_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_INBOX_API_EXPORT
    wire_traits<ores::inbox::messaging::get_many_notification_deliveries_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::inbox::messaging::get_many_notification_deliveries_request& v);
    static ores::inbox::messaging::get_many_notification_deliveries_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::inbox::messaging::get_many_notification_deliveries_request& v);
    static ores::inbox::messaging::get_many_notification_deliveries_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_INBOX_API_EXPORT
    wire_traits<ores::inbox::messaging::get_many_notification_deliveries_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::inbox::messaging::get_many_notification_deliveries_response& v);
    static ores::inbox::messaging::get_many_notification_deliveries_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::inbox::messaging::get_many_notification_deliveries_response& v);
    static ores::inbox::messaging::get_many_notification_deliveries_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_INBOX_API_EXPORT
    wire_traits<ores::inbox::messaging::put_notification_delivery_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::inbox::messaging::put_notification_delivery_request& v);
    static ores::inbox::messaging::put_notification_delivery_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::inbox::messaging::put_notification_delivery_request& v);
    static ores::inbox::messaging::put_notification_delivery_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_INBOX_API_EXPORT
    wire_traits<ores::inbox::messaging::put_notification_delivery_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::inbox::messaging::put_notification_delivery_response& v);
    static ores::inbox::messaging::put_notification_delivery_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::inbox::messaging::put_notification_delivery_response& v);
    static ores::inbox::messaging::put_notification_delivery_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_INBOX_API_EXPORT
    wire_traits<ores::inbox::messaging::put_many_notification_deliveries_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::inbox::messaging::put_many_notification_deliveries_request& v);
    static ores::inbox::messaging::put_many_notification_deliveries_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::inbox::messaging::put_many_notification_deliveries_request& v);
    static ores::inbox::messaging::put_many_notification_deliveries_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_INBOX_API_EXPORT
    wire_traits<ores::inbox::messaging::put_many_notification_deliveries_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::inbox::messaging::put_many_notification_deliveries_response& v);
    static ores::inbox::messaging::put_many_notification_deliveries_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::inbox::messaging::put_many_notification_deliveries_response& v);
    static ores::inbox::messaging::put_many_notification_deliveries_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_INBOX_API_EXPORT
    wire_traits<ores::inbox::messaging::delete_notification_delivery_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::inbox::messaging::delete_notification_delivery_request& v);
    static ores::inbox::messaging::delete_notification_delivery_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::inbox::messaging::delete_notification_delivery_request& v);
    static ores::inbox::messaging::delete_notification_delivery_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_INBOX_API_EXPORT
    wire_traits<ores::inbox::messaging::delete_notification_delivery_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::inbox::messaging::delete_notification_delivery_response& v);
    static ores::inbox::messaging::delete_notification_delivery_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::inbox::messaging::delete_notification_delivery_response& v);
    static ores::inbox::messaging::delete_notification_delivery_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_INBOX_API_EXPORT
    wire_traits<ores::inbox::messaging::delete_many_notification_deliveries_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::inbox::messaging::delete_many_notification_deliveries_request& v);
    static ores::inbox::messaging::delete_many_notification_deliveries_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::inbox::messaging::delete_many_notification_deliveries_request& v);
    static ores::inbox::messaging::delete_many_notification_deliveries_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_INBOX_API_EXPORT
    wire_traits<ores::inbox::messaging::delete_many_notification_deliveries_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::inbox::messaging::delete_many_notification_deliveries_response& v);
    static ores::inbox::messaging::delete_many_notification_deliveries_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::inbox::messaging::delete_many_notification_deliveries_response& v);
    static ores::inbox::messaging::delete_many_notification_deliveries_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_INBOX_API_EXPORT
    wire_traits<ores::inbox::messaging::list_by_notification_id_notification_deliveries_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::inbox::messaging::list_by_notification_id_notification_deliveries_request& v);
    static ores::inbox::messaging::list_by_notification_id_notification_deliveries_request
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::inbox::messaging::list_by_notification_id_notification_deliveries_request& v);
    static ores::inbox::messaging::list_by_notification_id_notification_deliveries_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_INBOX_API_EXPORT
    wire_traits<ores::inbox::messaging::list_by_notification_id_notification_deliveries_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::inbox::messaging::list_by_notification_id_notification_deliveries_response& v);
    static ores::inbox::messaging::list_by_notification_id_notification_deliveries_response
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::inbox::messaging::list_by_notification_id_notification_deliveries_response& v);
    static ores::inbox::messaging::list_by_notification_id_notification_deliveries_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_INBOX_API_EXPORT
    wire_traits<ores::inbox::messaging::list_notification_delivery_versions_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::inbox::messaging::list_notification_delivery_versions_request& v);
    static ores::inbox::messaging::list_notification_delivery_versions_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::inbox::messaging::list_notification_delivery_versions_request& v);
    static ores::inbox::messaging::list_notification_delivery_versions_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_INBOX_API_EXPORT
    wire_traits<ores::inbox::messaging::list_notification_delivery_versions_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::inbox::messaging::list_notification_delivery_versions_response& v);
    static ores::inbox::messaging::list_notification_delivery_versions_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::inbox::messaging::list_notification_delivery_versions_response& v);
    static ores::inbox::messaging::list_notification_delivery_versions_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_INBOX_API_EXPORT
    wire_traits<ores::inbox::messaging::get_notification_delivery_version_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::inbox::messaging::get_notification_delivery_version_request& v);
    static ores::inbox::messaging::get_notification_delivery_version_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::inbox::messaging::get_notification_delivery_version_request& v);
    static ores::inbox::messaging::get_notification_delivery_version_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_INBOX_API_EXPORT
    wire_traits<ores::inbox::messaging::get_notification_delivery_version_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::inbox::messaging::get_notification_delivery_version_response& v);
    static ores::inbox::messaging::get_notification_delivery_version_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::inbox::messaging::get_notification_delivery_version_response& v);
    static ores::inbox::messaging::get_notification_delivery_version_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
