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

#include "ores.inbox.api/domain/notification_codec.hpp"
#include "ores.inbox.api/export.hpp"
#include "ores.utility/codec/wire_fields.hpp"
#include "ores.utility/codec/wire_traits_fwd.hpp"
#include "ores.utility/domain/protocol.hpp"
#include "ores.utility/domain/protocol_codec.hpp"
#include <boost/uuid/uuid.hpp>
#include <cstddef>
#include <cstdint>
#include <optional>
#include <span>
#include <string>
#include <tuple>
#include <vector>

namespace ores::inbox::messaging {

struct notification_key {
    boost::uuids::uuid id;

    friend bool operator==(const notification_key&, const notification_key&) = default;
};

struct notification_write {
    boost::uuids::uuid id;
    std::string kind_code;
    boost::uuids::uuid raised_by;
    std::chrono::system_clock::time_point raised_at;
    std::string link_route;
    std::optional<std::string> link_id;
    std::optional<std::string> audience_permission_code;

    friend bool operator==(const notification_write&, const notification_write&) = default;
};

struct notification_change {
    notification_write write;
    ores::utility::domain::precondition precondition;

    friend bool operator==(const notification_change&, const notification_change&) = default;
};

struct notification_removal {
    notification_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;

    friend bool operator==(const notification_removal&, const notification_removal&) = default;
};

struct notification_lookup {
    notification_key key;
    std::optional<ores::inbox::domain::notification> notification;

    friend bool operator==(const notification_lookup&, const notification_lookup&) = default;
};

struct notifications_filter {
    std::optional<std::string> kind_code;
    std::optional<std::vector<boost::uuids::uuid>> id_one_of;
    std::optional<std::vector<std::string>> kind_code_one_of;

    friend bool operator==(const notifications_filter&, const notifications_filter&) = default;
};

struct notification_event {
    boost::uuids::uuid event_id;
    notification_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;

    friend bool operator==(const notification_event&, const notification_event&) = default;
};

struct notification_version_key {
    notification_key notification;
    std::uint32_t version;

    friend bool operator==(const notification_version_key&,
                           const notification_version_key&) = default;
};

struct notification_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;

    friend bool operator==(const notification_versions_filter&,
                           const notification_versions_filter&) = default;
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
    std::optional<std::string> as_of;

    friend bool operator==(const list_notifications_request&,
                           const list_notifications_request&) = default;
};

struct list_notifications_response {
    ores::utility::domain::result result;
    std::vector<ores::inbox::domain::notification> notifications;
    std::uint64_t total;

    friend bool operator==(const list_notifications_response&,
                           const list_notifications_response&) = default;
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

    friend bool operator==(const get_notification_request&,
                           const get_notification_request&) = default;
};

struct get_notification_response {
    ores::utility::domain::result result;
    std::optional<ores::inbox::domain::notification> notification;

    friend bool operator==(const get_notification_response&,
                           const get_notification_response&) = default;
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

    friend bool operator==(const get_many_notifications_request&,
                           const get_many_notifications_request&) = default;
};

struct get_many_notifications_response {
    ores::utility::domain::result result;
    std::vector<notification_lookup> entries;

    friend bool operator==(const get_many_notifications_response&,
                           const get_many_notifications_response&) = default;
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

    friend bool operator==(const put_notification_request&,
                           const put_notification_request&) = default;
};

struct put_notification_response {
    ores::utility::domain::result result;
    std::optional<ores::inbox::domain::notification> notification;

    friend bool operator==(const put_notification_response&,
                           const put_notification_response&) = default;
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

    friend bool operator==(const put_many_notifications_request&,
                           const put_many_notifications_request&) = default;
};

struct put_many_notifications_response {
    ores::utility::domain::result result;
    std::vector<ores::inbox::domain::notification> notifications;

    friend bool operator==(const put_many_notifications_response&,
                           const put_many_notifications_response&) = default;
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

    friend bool operator==(const delete_notification_request&,
                           const delete_notification_request&) = default;
};

struct delete_notification_response {
    ores::utility::domain::result result;

    friend bool operator==(const delete_notification_response&,
                           const delete_notification_response&) = default;
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

    friend bool operator==(const delete_many_notifications_request&,
                           const delete_many_notifications_request&) = default;
};

struct delete_many_notifications_response {
    ores::utility::domain::result result;

    friend bool operator==(const delete_many_notifications_response&,
                           const delete_many_notifications_response&) = default;
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

    friend bool operator==(const list_by_kind_code_notifications_request&,
                           const list_by_kind_code_notifications_request&) = default;
};

struct list_by_kind_code_notifications_response {
    ores::utility::domain::result result;
    std::vector<ores::inbox::domain::notification> notifications;
    std::uint64_t total;

    friend bool operator==(const list_by_kind_code_notifications_response&,
                           const list_by_kind_code_notifications_response&) = default;
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

    friend bool operator==(const list_notification_versions_request&,
                           const list_notification_versions_request&) = default;
};

struct list_notification_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::inbox::domain::notification> versions;
    std::uint64_t total;

    friend bool operator==(const list_notification_versions_response&,
                           const list_notification_versions_response&) = default;
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

    friend bool operator==(const get_notification_version_request&,
                           const get_notification_version_request&) = default;
};

struct get_notification_version_response {
    ores::utility::domain::result result;
    std::optional<ores::inbox::domain::notification> version;

    friend bool operator==(const get_notification_version_response&,
                           const get_notification_version_response&) = default;
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
inline constexpr std::string_view created = "inbox.v1.events.notifications.created";
inline constexpr std::string_view updated = "inbox.v1.events.notifications.updated";
inline constexpr std::string_view deleted = "inbox.v1.events.notifications.deleted";
}
/**
 * @brief The members of notification_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<notification_key>) {
    return std::tuple{
        utility::codec::field{"id", &notification_key::id},
    };
}

/**
 * @brief The members of notification_write, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<notification_write>) {
    return std::tuple{
        utility::codec::field{"id", &notification_write::id},
        utility::codec::field{"kind_code", &notification_write::kind_code},
        utility::codec::field{"raised_by", &notification_write::raised_by},
        utility::codec::field{"raised_at", &notification_write::raised_at},
        utility::codec::field{"link_route", &notification_write::link_route},
        utility::codec::field{"link_id", &notification_write::link_id},
        utility::codec::field{"audience_permission_code",
                              &notification_write::audience_permission_code},
    };
}

/**
 * @brief The members of notification_change, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<notification_change>) {
    return std::tuple{
        utility::codec::field{"write", &notification_change::write},
        utility::codec::field{"precondition", &notification_change::precondition},
    };
}

/**
 * @brief The members of notification_removal, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<notification_removal>) {
    return std::tuple{
        utility::codec::field{"key", &notification_removal::key},
        utility::codec::field{"precondition", &notification_removal::precondition},
    };
}

/**
 * @brief The members of notification_lookup, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<notification_lookup>) {
    return std::tuple{
        utility::codec::field{"key", &notification_lookup::key},
        utility::codec::field{"notification", &notification_lookup::notification},
    };
}

/**
 * @brief The members of notifications_filter, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<notifications_filter>) {
    return std::tuple{
        utility::codec::field{"kind_code", &notifications_filter::kind_code},
        utility::codec::field{"id_one_of", &notifications_filter::id_one_of},
        utility::codec::field{"kind_code_one_of", &notifications_filter::kind_code_one_of},
    };
}

/**
 * @brief The members of notification_event, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<notification_event>) {
    return std::tuple{
        utility::codec::field{"event_id", &notification_event::event_id},
        utility::codec::field{"key", &notification_event::key},
        utility::codec::field{"action", &notification_event::action},
        utility::codec::field{"version", &notification_event::version},
        utility::codec::field{"occurred_at", &notification_event::occurred_at},
        utility::codec::field{"correlation_id", &notification_event::correlation_id},
    };
}

/**
 * @brief The members of notification_version_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<notification_version_key>) {
    return std::tuple{
        utility::codec::field{"notification", &notification_version_key::notification},
        utility::codec::field{"version", &notification_version_key::version},
    };
}

/**
 * @brief The members of notification_versions_filter, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<notification_versions_filter>) {
    return std::tuple{
        utility::codec::field{"version", &notification_versions_filter::version},
        utility::codec::field{"from_version", &notification_versions_filter::from_version},
        utility::codec::field{"to_version", &notification_versions_filter::to_version},
    };
}

/**
 * @brief The members of list_notifications_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<list_notifications_request>) {
    return std::tuple{
        utility::codec::field{"offset", &list_notifications_request::offset},
        utility::codec::field{"limit", &list_notifications_request::limit},
        utility::codec::field{"order", &list_notifications_request::order},
        utility::codec::field{"filter", &list_notifications_request::filter},
        utility::codec::field{"as_of", &list_notifications_request::as_of},
    };
}

/**
 * @brief The members of list_notifications_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<list_notifications_response>) {
    return std::tuple{
        utility::codec::field{"result", &list_notifications_response::result},
        utility::codec::field{"notifications", &list_notifications_response::notifications},
        utility::codec::field{"total", &list_notifications_response::total},
    };
}

/**
 * @brief The members of get_notification_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_notification_request>) {
    return std::tuple{
        utility::codec::field{"key", &get_notification_request::key},
    };
}

/**
 * @brief The members of get_notification_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_notification_response>) {
    return std::tuple{
        utility::codec::field{"result", &get_notification_response::result},
        utility::codec::field{"notification", &get_notification_response::notification},
    };
}

/**
 * @brief The members of get_many_notifications_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_many_notifications_request>) {
    return std::tuple{
        utility::codec::field{"keys", &get_many_notifications_request::keys},
    };
}

/**
 * @brief The members of get_many_notifications_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_many_notifications_response>) {
    return std::tuple{
        utility::codec::field{"result", &get_many_notifications_response::result},
        utility::codec::field{"entries", &get_many_notifications_response::entries},
    };
}

/**
 * @brief The members of put_notification_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<put_notification_request>) {
    return std::tuple{
        utility::codec::field{"change", &put_notification_request::change},
        utility::codec::field{"intent", &put_notification_request::intent},
    };
}

/**
 * @brief The members of put_notification_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<put_notification_response>) {
    return std::tuple{
        utility::codec::field{"result", &put_notification_response::result},
        utility::codec::field{"notification", &put_notification_response::notification},
    };
}

/**
 * @brief The members of put_many_notifications_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<put_many_notifications_request>) {
    return std::tuple{
        utility::codec::field{"changes", &put_many_notifications_request::changes},
        utility::codec::field{"intent", &put_many_notifications_request::intent},
    };
}

/**
 * @brief The members of put_many_notifications_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<put_many_notifications_response>) {
    return std::tuple{
        utility::codec::field{"result", &put_many_notifications_response::result},
        utility::codec::field{"notifications", &put_many_notifications_response::notifications},
    };
}

/**
 * @brief The members of delete_notification_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<delete_notification_request>) {
    return std::tuple{
        utility::codec::field{"removal", &delete_notification_request::removal},
        utility::codec::field{"intent", &delete_notification_request::intent},
    };
}

/**
 * @brief The members of delete_notification_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<delete_notification_response>) {
    return std::tuple{
        utility::codec::field{"result", &delete_notification_response::result},
    };
}

/**
 * @brief The members of delete_many_notifications_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<delete_many_notifications_request>) {
    return std::tuple{
        utility::codec::field{"removals", &delete_many_notifications_request::removals},
        utility::codec::field{"intent", &delete_many_notifications_request::intent},
    };
}

/**
 * @brief The members of delete_many_notifications_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<delete_many_notifications_response>) {
    return std::tuple{
        utility::codec::field{"result", &delete_many_notifications_response::result},
    };
}

/**
 * @brief The members of list_by_kind_code_notifications_request, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<list_by_kind_code_notifications_request>) {
    return std::tuple{
        utility::codec::field{"kind_code", &list_by_kind_code_notifications_request::kind_code},
        utility::codec::field{"scope", &list_by_kind_code_notifications_request::scope},
        utility::codec::field{"offset", &list_by_kind_code_notifications_request::offset},
        utility::codec::field{"limit", &list_by_kind_code_notifications_request::limit},
        utility::codec::field{"order", &list_by_kind_code_notifications_request::order},
        utility::codec::field{"filter", &list_by_kind_code_notifications_request::filter},
    };
}

/**
 * @brief The members of list_by_kind_code_notifications_response, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<list_by_kind_code_notifications_response>) {
    return std::tuple{
        utility::codec::field{"result", &list_by_kind_code_notifications_response::result},
        utility::codec::field{"notifications",
                              &list_by_kind_code_notifications_response::notifications},
        utility::codec::field{"total", &list_by_kind_code_notifications_response::total},
    };
}

/**
 * @brief The members of list_notification_versions_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<list_notification_versions_request>) {
    return std::tuple{
        utility::codec::field{"key", &list_notification_versions_request::key},
        utility::codec::field{"offset", &list_notification_versions_request::offset},
        utility::codec::field{"limit", &list_notification_versions_request::limit},
        utility::codec::field{"order", &list_notification_versions_request::order},
        utility::codec::field{"filter", &list_notification_versions_request::filter},
    };
}

/**
 * @brief The members of list_notification_versions_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<list_notification_versions_response>) {
    return std::tuple{
        utility::codec::field{"result", &list_notification_versions_response::result},
        utility::codec::field{"versions", &list_notification_versions_response::versions},
        utility::codec::field{"total", &list_notification_versions_response::total},
    };
}

/**
 * @brief The members of get_notification_version_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_notification_version_request>) {
    return std::tuple{
        utility::codec::field{"key", &get_notification_version_request::key},
    };
}

/**
 * @brief The members of get_notification_version_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_notification_version_response>) {
    return std::tuple{
        utility::codec::field{"result", &get_notification_version_response::result},
        utility::codec::field{"version", &get_notification_version_response::version},
    };
}


}
namespace ores::utility::codec {

template <>
struct ORES_INBOX_API_EXPORT wire_traits<ores::inbox::messaging::notification_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::inbox::messaging::notification_key& v);
    static ores::inbox::messaging::notification_key read_json(std::string_view text);
    static std::string write_msgpack(const ores::inbox::messaging::notification_key& v);
    static ores::inbox::messaging::notification_key read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_INBOX_API_EXPORT wire_traits<ores::inbox::messaging::notification_write> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::inbox::messaging::notification_write& v);
    static ores::inbox::messaging::notification_write read_json(std::string_view text);
    static std::string write_msgpack(const ores::inbox::messaging::notification_write& v);
    static ores::inbox::messaging::notification_write read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_INBOX_API_EXPORT wire_traits<ores::inbox::messaging::notification_change> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::inbox::messaging::notification_change& v);
    static ores::inbox::messaging::notification_change read_json(std::string_view text);
    static std::string write_msgpack(const ores::inbox::messaging::notification_change& v);
    static ores::inbox::messaging::notification_change
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_INBOX_API_EXPORT wire_traits<ores::inbox::messaging::notification_removal> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::inbox::messaging::notification_removal& v);
    static ores::inbox::messaging::notification_removal read_json(std::string_view text);
    static std::string write_msgpack(const ores::inbox::messaging::notification_removal& v);
    static ores::inbox::messaging::notification_removal
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_INBOX_API_EXPORT wire_traits<ores::inbox::messaging::notification_lookup> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::inbox::messaging::notification_lookup& v);
    static ores::inbox::messaging::notification_lookup read_json(std::string_view text);
    static std::string write_msgpack(const ores::inbox::messaging::notification_lookup& v);
    static ores::inbox::messaging::notification_lookup
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_INBOX_API_EXPORT wire_traits<ores::inbox::messaging::notifications_filter> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::inbox::messaging::notifications_filter& v);
    static ores::inbox::messaging::notifications_filter read_json(std::string_view text);
    static std::string write_msgpack(const ores::inbox::messaging::notifications_filter& v);
    static ores::inbox::messaging::notifications_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_INBOX_API_EXPORT wire_traits<ores::inbox::messaging::notification_event> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::inbox::messaging::notification_event& v);
    static ores::inbox::messaging::notification_event read_json(std::string_view text);
    static std::string write_msgpack(const ores::inbox::messaging::notification_event& v);
    static ores::inbox::messaging::notification_event read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_INBOX_API_EXPORT wire_traits<ores::inbox::messaging::notification_version_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::inbox::messaging::notification_version_key& v);
    static ores::inbox::messaging::notification_version_key read_json(std::string_view text);
    static std::string write_msgpack(const ores::inbox::messaging::notification_version_key& v);
    static ores::inbox::messaging::notification_version_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_INBOX_API_EXPORT wire_traits<ores::inbox::messaging::notification_versions_filter> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::inbox::messaging::notification_versions_filter& v);
    static ores::inbox::messaging::notification_versions_filter read_json(std::string_view text);
    static std::string write_msgpack(const ores::inbox::messaging::notification_versions_filter& v);
    static ores::inbox::messaging::notification_versions_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_INBOX_API_EXPORT wire_traits<ores::inbox::messaging::list_notifications_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::inbox::messaging::list_notifications_request& v);
    static ores::inbox::messaging::list_notifications_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::inbox::messaging::list_notifications_request& v);
    static ores::inbox::messaging::list_notifications_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_INBOX_API_EXPORT wire_traits<ores::inbox::messaging::list_notifications_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::inbox::messaging::list_notifications_response& v);
    static ores::inbox::messaging::list_notifications_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::inbox::messaging::list_notifications_response& v);
    static ores::inbox::messaging::list_notifications_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_INBOX_API_EXPORT wire_traits<ores::inbox::messaging::get_notification_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::inbox::messaging::get_notification_request& v);
    static ores::inbox::messaging::get_notification_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::inbox::messaging::get_notification_request& v);
    static ores::inbox::messaging::get_notification_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_INBOX_API_EXPORT wire_traits<ores::inbox::messaging::get_notification_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::inbox::messaging::get_notification_response& v);
    static ores::inbox::messaging::get_notification_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::inbox::messaging::get_notification_response& v);
    static ores::inbox::messaging::get_notification_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_INBOX_API_EXPORT wire_traits<ores::inbox::messaging::get_many_notifications_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::inbox::messaging::get_many_notifications_request& v);
    static ores::inbox::messaging::get_many_notifications_request read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::inbox::messaging::get_many_notifications_request& v);
    static ores::inbox::messaging::get_many_notifications_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_INBOX_API_EXPORT wire_traits<ores::inbox::messaging::get_many_notifications_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::inbox::messaging::get_many_notifications_response& v);
    static ores::inbox::messaging::get_many_notifications_response read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::inbox::messaging::get_many_notifications_response& v);
    static ores::inbox::messaging::get_many_notifications_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_INBOX_API_EXPORT wire_traits<ores::inbox::messaging::put_notification_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::inbox::messaging::put_notification_request& v);
    static ores::inbox::messaging::put_notification_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::inbox::messaging::put_notification_request& v);
    static ores::inbox::messaging::put_notification_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_INBOX_API_EXPORT wire_traits<ores::inbox::messaging::put_notification_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::inbox::messaging::put_notification_response& v);
    static ores::inbox::messaging::put_notification_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::inbox::messaging::put_notification_response& v);
    static ores::inbox::messaging::put_notification_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_INBOX_API_EXPORT wire_traits<ores::inbox::messaging::put_many_notifications_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::inbox::messaging::put_many_notifications_request& v);
    static ores::inbox::messaging::put_many_notifications_request read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::inbox::messaging::put_many_notifications_request& v);
    static ores::inbox::messaging::put_many_notifications_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_INBOX_API_EXPORT wire_traits<ores::inbox::messaging::put_many_notifications_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::inbox::messaging::put_many_notifications_response& v);
    static ores::inbox::messaging::put_many_notifications_response read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::inbox::messaging::put_many_notifications_response& v);
    static ores::inbox::messaging::put_many_notifications_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_INBOX_API_EXPORT wire_traits<ores::inbox::messaging::delete_notification_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::inbox::messaging::delete_notification_request& v);
    static ores::inbox::messaging::delete_notification_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::inbox::messaging::delete_notification_request& v);
    static ores::inbox::messaging::delete_notification_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_INBOX_API_EXPORT wire_traits<ores::inbox::messaging::delete_notification_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::inbox::messaging::delete_notification_response& v);
    static ores::inbox::messaging::delete_notification_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::inbox::messaging::delete_notification_response& v);
    static ores::inbox::messaging::delete_notification_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_INBOX_API_EXPORT
    wire_traits<ores::inbox::messaging::delete_many_notifications_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::inbox::messaging::delete_many_notifications_request& v);
    static ores::inbox::messaging::delete_many_notifications_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::inbox::messaging::delete_many_notifications_request& v);
    static ores::inbox::messaging::delete_many_notifications_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_INBOX_API_EXPORT
    wire_traits<ores::inbox::messaging::delete_many_notifications_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::inbox::messaging::delete_many_notifications_response& v);
    static ores::inbox::messaging::delete_many_notifications_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::inbox::messaging::delete_many_notifications_response& v);
    static ores::inbox::messaging::delete_many_notifications_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_INBOX_API_EXPORT
    wire_traits<ores::inbox::messaging::list_by_kind_code_notifications_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::inbox::messaging::list_by_kind_code_notifications_request& v);
    static ores::inbox::messaging::list_by_kind_code_notifications_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::inbox::messaging::list_by_kind_code_notifications_request& v);
    static ores::inbox::messaging::list_by_kind_code_notifications_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_INBOX_API_EXPORT
    wire_traits<ores::inbox::messaging::list_by_kind_code_notifications_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::inbox::messaging::list_by_kind_code_notifications_response& v);
    static ores::inbox::messaging::list_by_kind_code_notifications_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::inbox::messaging::list_by_kind_code_notifications_response& v);
    static ores::inbox::messaging::list_by_kind_code_notifications_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_INBOX_API_EXPORT
    wire_traits<ores::inbox::messaging::list_notification_versions_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::inbox::messaging::list_notification_versions_request& v);
    static ores::inbox::messaging::list_notification_versions_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::inbox::messaging::list_notification_versions_request& v);
    static ores::inbox::messaging::list_notification_versions_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_INBOX_API_EXPORT
    wire_traits<ores::inbox::messaging::list_notification_versions_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::inbox::messaging::list_notification_versions_response& v);
    static ores::inbox::messaging::list_notification_versions_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::inbox::messaging::list_notification_versions_response& v);
    static ores::inbox::messaging::list_notification_versions_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_INBOX_API_EXPORT wire_traits<ores::inbox::messaging::get_notification_version_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::inbox::messaging::get_notification_version_request& v);
    static ores::inbox::messaging::get_notification_version_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::inbox::messaging::get_notification_version_request& v);
    static ores::inbox::messaging::get_notification_version_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_INBOX_API_EXPORT
    wire_traits<ores::inbox::messaging::get_notification_version_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::inbox::messaging::get_notification_version_response& v);
    static ores::inbox::messaging::get_notification_version_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::inbox::messaging::get_notification_version_response& v);
    static ores::inbox::messaging::get_notification_version_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
