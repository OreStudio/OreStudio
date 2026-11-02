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
#ifndef ORES_INBOX_API_MESSAGING_NOTIFICATION_KIND_PROTOCOL_HPP
#define ORES_INBOX_API_MESSAGING_NOTIFICATION_KIND_PROTOCOL_HPP

#include "ores.inbox.api/domain/notification_kind.hpp"
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

struct notification_kind_key {
    std::string code;
};

struct notification_kind_write {
    std::string code;
    std::string name;
    std::string description;
    std::string message_key;
    bool mail_optional;
    int display_order;
};

struct notification_kind_change {
    notification_kind_write write;
    ores::utility::domain::precondition precondition;
};

struct notification_kind_removal {
    notification_kind_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;
};

struct notification_kind_lookup {
    notification_kind_key key;
    std::optional<ores::inbox::domain::notification_kind> notification_kind;
};

struct notification_kinds_filter {
    std::optional<std::vector<std::string>> code_one_of;
};

struct notification_kind_event {
    boost::uuids::uuid event_id;
    notification_kind_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;
};

struct notification_kind_version_key {
    notification_kind_key notification_kind;
    std::uint32_t version;
};

struct notification_kind_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;
};

struct list_notification_kinds_request {
    using response_type = struct list_notification_kinds_response;
    static constexpr std::string_view nats_subject = "inbox.v1.notification_kinds.list";
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
    std::optional<notification_kinds_filter> filter;
    std::optional<std::string> as_of;
};

struct list_notification_kinds_response {
    ores::utility::domain::result result;
    std::vector<ores::inbox::domain::notification_kind> notification_kinds;
    std::uint64_t total;
};

struct get_notification_kind_request {
    using response_type = struct get_notification_kind_response;
    static constexpr std::string_view nats_subject = "inbox.v1.notification_kinds.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    notification_kind_key key;
};

struct get_notification_kind_response {
    ores::utility::domain::result result;
    std::optional<ores::inbox::domain::notification_kind> notification_kind;
};

struct get_many_notification_kinds_request {
    using response_type = struct get_many_notification_kinds_response;
    static constexpr std::string_view nats_subject = "inbox.v1.notification_kinds.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<notification_kind_key> keys;
};

struct get_many_notification_kinds_response {
    ores::utility::domain::result result;
    std::vector<notification_kind_lookup> entries;
};

struct put_notification_kind_request {
    using response_type = struct put_notification_kind_response;
    static constexpr std::string_view nats_subject = "inbox.v1.notification_kinds.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    notification_kind_change change;
    ores::utility::domain::change_intent intent;
};

struct put_notification_kind_response {
    ores::utility::domain::result result;
    std::optional<ores::inbox::domain::notification_kind> notification_kind;
};

struct put_many_notification_kinds_request {
    using response_type = struct put_many_notification_kinds_response;
    static constexpr std::string_view nats_subject = "inbox.v1.notification_kinds.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<notification_kind_change> changes;
    ores::utility::domain::change_intent intent;
};

struct put_many_notification_kinds_response {
    ores::utility::domain::result result;
    std::vector<ores::inbox::domain::notification_kind> notification_kinds;
};

struct delete_notification_kind_request {
    using response_type = struct delete_notification_kind_response;
    static constexpr std::string_view nats_subject = "inbox.v1.notification_kinds.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    notification_kind_removal removal;
    ores::utility::domain::change_intent intent;
};

struct delete_notification_kind_response {
    ores::utility::domain::result result;
};

struct delete_many_notification_kinds_request {
    using response_type = struct delete_many_notification_kinds_response;
    static constexpr std::string_view nats_subject = "inbox.v1.notification_kinds.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<notification_kind_removal> removals;
    ores::utility::domain::change_intent intent;
};

struct delete_many_notification_kinds_response {
    ores::utility::domain::result result;
};

struct list_notification_kind_versions_request {
    using response_type = struct list_notification_kind_versions_response;
    static constexpr std::string_view nats_subject = "inbox.v1.notification_kinds_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    notification_kind_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<notification_kind_versions_filter> filter;
};

struct list_notification_kind_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::inbox::domain::notification_kind> versions;
    std::uint64_t total;
};

struct get_notification_kind_version_request {
    using response_type = struct get_notification_kind_version_response;
    static constexpr std::string_view nats_subject = "inbox.v1.notification_kinds_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    notification_kind_version_key key;
};

struct get_notification_kind_version_response {
    ores::utility::domain::result result;
    std::optional<ores::inbox::domain::notification_kind> version;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace notification_kind_event_subjects {
inline constexpr std::string_view created = "inbox.v1.notification_kinds_events.created";
inline constexpr std::string_view updated = "inbox.v1.notification_kinds_events.updated";
inline constexpr std::string_view deleted = "inbox.v1.notification_kinds_events.deleted";
}
/**
 * @brief The members of notification_kind_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const notification_kind_key&) {
    return std::tuple{
        utility::codec::field{"code", &notification_kind_key::code},
    };
}

/**
 * @brief The members of notification_kind_write, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const notification_kind_write&) {
    return std::tuple{
        utility::codec::field{"code", &notification_kind_write::code},
        utility::codec::field{"name", &notification_kind_write::name},
        utility::codec::field{"description", &notification_kind_write::description},
        utility::codec::field{"message_key", &notification_kind_write::message_key},
        utility::codec::field{"mail_optional", &notification_kind_write::mail_optional},
        utility::codec::field{"display_order", &notification_kind_write::display_order},
    };
}

/**
 * @brief The members of notification_kind_change, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const notification_kind_change&) {
    return std::tuple{
        utility::codec::field{"write", &notification_kind_change::write},
        utility::codec::field{"precondition", &notification_kind_change::precondition},
    };
}

/**
 * @brief The members of notification_kind_removal, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const notification_kind_removal&) {
    return std::tuple{
        utility::codec::field{"key", &notification_kind_removal::key},
        utility::codec::field{"precondition", &notification_kind_removal::precondition},
    };
}

/**
 * @brief The members of notification_kind_lookup, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const notification_kind_lookup&) {
    return std::tuple{
        utility::codec::field{"key", &notification_kind_lookup::key},
        utility::codec::field{"notification_kind", &notification_kind_lookup::notification_kind},
    };
}

/**
 * @brief The members of notification_kinds_filter, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const notification_kinds_filter&) {
    return std::tuple{
        utility::codec::field{"code_one_of", &notification_kinds_filter::code_one_of},
    };
}

/**
 * @brief The members of notification_kind_event, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const notification_kind_event&) {
    return std::tuple{
        utility::codec::field{"event_id", &notification_kind_event::event_id},
        utility::codec::field{"key", &notification_kind_event::key},
        utility::codec::field{"action", &notification_kind_event::action},
        utility::codec::field{"version", &notification_kind_event::version},
        utility::codec::field{"occurred_at", &notification_kind_event::occurred_at},
        utility::codec::field{"correlation_id", &notification_kind_event::correlation_id},
    };
}

/**
 * @brief The members of notification_kind_version_key, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const notification_kind_version_key&) {
    return std::tuple{
        utility::codec::field{"notification_kind",
                              &notification_kind_version_key::notification_kind},
        utility::codec::field{"version", &notification_kind_version_key::version},
    };
}

/**
 * @brief The members of notification_kind_versions_filter, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const notification_kind_versions_filter&) {
    return std::tuple{
        utility::codec::field{"version", &notification_kind_versions_filter::version},
        utility::codec::field{"from_version", &notification_kind_versions_filter::from_version},
        utility::codec::field{"to_version", &notification_kind_versions_filter::to_version},
    };
}

/**
 * @brief The members of list_notification_kinds_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_notification_kinds_request&) {
    return std::tuple{
        utility::codec::field{"offset", &list_notification_kinds_request::offset},
        utility::codec::field{"limit", &list_notification_kinds_request::limit},
        utility::codec::field{"order", &list_notification_kinds_request::order},
        utility::codec::field{"filter", &list_notification_kinds_request::filter},
        utility::codec::field{"as_of", &list_notification_kinds_request::as_of},
    };
}

/**
 * @brief The members of list_notification_kinds_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_notification_kinds_response&) {
    return std::tuple{
        utility::codec::field{"result", &list_notification_kinds_response::result},
        utility::codec::field{"notification_kinds",
                              &list_notification_kinds_response::notification_kinds},
        utility::codec::field{"total", &list_notification_kinds_response::total},
    };
}

/**
 * @brief The members of get_notification_kind_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_notification_kind_request&) {
    return std::tuple{
        utility::codec::field{"key", &get_notification_kind_request::key},
    };
}

/**
 * @brief The members of get_notification_kind_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_notification_kind_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_notification_kind_response::result},
        utility::codec::field{"notification_kind",
                              &get_notification_kind_response::notification_kind},
    };
}

/**
 * @brief The members of get_many_notification_kinds_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_many_notification_kinds_request&) {
    return std::tuple{
        utility::codec::field{"keys", &get_many_notification_kinds_request::keys},
    };
}

/**
 * @brief The members of get_many_notification_kinds_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_many_notification_kinds_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_many_notification_kinds_response::result},
        utility::codec::field{"entries", &get_many_notification_kinds_response::entries},
    };
}

/**
 * @brief The members of put_notification_kind_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_notification_kind_request&) {
    return std::tuple{
        utility::codec::field{"change", &put_notification_kind_request::change},
        utility::codec::field{"intent", &put_notification_kind_request::intent},
    };
}

/**
 * @brief The members of put_notification_kind_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_notification_kind_response&) {
    return std::tuple{
        utility::codec::field{"result", &put_notification_kind_response::result},
        utility::codec::field{"notification_kind",
                              &put_notification_kind_response::notification_kind},
    };
}

/**
 * @brief The members of put_many_notification_kinds_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_many_notification_kinds_request&) {
    return std::tuple{
        utility::codec::field{"changes", &put_many_notification_kinds_request::changes},
        utility::codec::field{"intent", &put_many_notification_kinds_request::intent},
    };
}

/**
 * @brief The members of put_many_notification_kinds_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_many_notification_kinds_response&) {
    return std::tuple{
        utility::codec::field{"result", &put_many_notification_kinds_response::result},
        utility::codec::field{"notification_kinds",
                              &put_many_notification_kinds_response::notification_kinds},
    };
}

/**
 * @brief The members of delete_notification_kind_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_notification_kind_request&) {
    return std::tuple{
        utility::codec::field{"removal", &delete_notification_kind_request::removal},
        utility::codec::field{"intent", &delete_notification_kind_request::intent},
    };
}

/**
 * @brief The members of delete_notification_kind_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_notification_kind_response&) {
    return std::tuple{
        utility::codec::field{"result", &delete_notification_kind_response::result},
    };
}

/**
 * @brief The members of delete_many_notification_kinds_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_many_notification_kinds_request&) {
    return std::tuple{
        utility::codec::field{"removals", &delete_many_notification_kinds_request::removals},
        utility::codec::field{"intent", &delete_many_notification_kinds_request::intent},
    };
}

/**
 * @brief The members of delete_many_notification_kinds_response, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_many_notification_kinds_response&) {
    return std::tuple{
        utility::codec::field{"result", &delete_many_notification_kinds_response::result},
    };
}

/**
 * @brief The members of list_notification_kind_versions_request, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_notification_kind_versions_request&) {
    return std::tuple{
        utility::codec::field{"key", &list_notification_kind_versions_request::key},
        utility::codec::field{"offset", &list_notification_kind_versions_request::offset},
        utility::codec::field{"limit", &list_notification_kind_versions_request::limit},
        utility::codec::field{"order", &list_notification_kind_versions_request::order},
        utility::codec::field{"filter", &list_notification_kind_versions_request::filter},
    };
}

/**
 * @brief The members of list_notification_kind_versions_response, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_notification_kind_versions_response&) {
    return std::tuple{
        utility::codec::field{"result", &list_notification_kind_versions_response::result},
        utility::codec::field{"versions", &list_notification_kind_versions_response::versions},
        utility::codec::field{"total", &list_notification_kind_versions_response::total},
    };
}

/**
 * @brief The members of get_notification_kind_version_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_notification_kind_version_request&) {
    return std::tuple{
        utility::codec::field{"key", &get_notification_kind_version_request::key},
    };
}

/**
 * @brief The members of get_notification_kind_version_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_notification_kind_version_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_notification_kind_version_response::result},
        utility::codec::field{"version", &get_notification_kind_version_response::version},
    };
}


}
namespace ores::utility::codec {

template <>
struct ORES_INBOX_API_EXPORT wire_traits<ores::inbox::messaging::notification_kind_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::inbox::messaging::notification_kind_key& v);
    static ores::inbox::messaging::notification_kind_key read_json(std::string_view text);
    static std::string write_msgpack(const ores::inbox::messaging::notification_kind_key& v);
    static ores::inbox::messaging::notification_kind_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_INBOX_API_EXPORT wire_traits<ores::inbox::messaging::notification_kind_write> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::inbox::messaging::notification_kind_write& v);
    static ores::inbox::messaging::notification_kind_write read_json(std::string_view text);
    static std::string write_msgpack(const ores::inbox::messaging::notification_kind_write& v);
    static ores::inbox::messaging::notification_kind_write
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_INBOX_API_EXPORT wire_traits<ores::inbox::messaging::notification_kind_change> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::inbox::messaging::notification_kind_change& v);
    static ores::inbox::messaging::notification_kind_change read_json(std::string_view text);
    static std::string write_msgpack(const ores::inbox::messaging::notification_kind_change& v);
    static ores::inbox::messaging::notification_kind_change
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_INBOX_API_EXPORT wire_traits<ores::inbox::messaging::notification_kind_removal> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::inbox::messaging::notification_kind_removal& v);
    static ores::inbox::messaging::notification_kind_removal read_json(std::string_view text);
    static std::string write_msgpack(const ores::inbox::messaging::notification_kind_removal& v);
    static ores::inbox::messaging::notification_kind_removal
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_INBOX_API_EXPORT wire_traits<ores::inbox::messaging::notification_kind_lookup> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::inbox::messaging::notification_kind_lookup& v);
    static ores::inbox::messaging::notification_kind_lookup read_json(std::string_view text);
    static std::string write_msgpack(const ores::inbox::messaging::notification_kind_lookup& v);
    static ores::inbox::messaging::notification_kind_lookup
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_INBOX_API_EXPORT wire_traits<ores::inbox::messaging::notification_kinds_filter> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::inbox::messaging::notification_kinds_filter& v);
    static ores::inbox::messaging::notification_kinds_filter read_json(std::string_view text);
    static std::string write_msgpack(const ores::inbox::messaging::notification_kinds_filter& v);
    static ores::inbox::messaging::notification_kinds_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_INBOX_API_EXPORT wire_traits<ores::inbox::messaging::notification_kind_event> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::inbox::messaging::notification_kind_event& v);
    static ores::inbox::messaging::notification_kind_event read_json(std::string_view text);
    static std::string write_msgpack(const ores::inbox::messaging::notification_kind_event& v);
    static ores::inbox::messaging::notification_kind_event
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_INBOX_API_EXPORT wire_traits<ores::inbox::messaging::notification_kind_version_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::inbox::messaging::notification_kind_version_key& v);
    static ores::inbox::messaging::notification_kind_version_key read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::inbox::messaging::notification_kind_version_key& v);
    static ores::inbox::messaging::notification_kind_version_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_INBOX_API_EXPORT
    wire_traits<ores::inbox::messaging::notification_kind_versions_filter> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::inbox::messaging::notification_kind_versions_filter& v);
    static ores::inbox::messaging::notification_kind_versions_filter
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::inbox::messaging::notification_kind_versions_filter& v);
    static ores::inbox::messaging::notification_kind_versions_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_INBOX_API_EXPORT wire_traits<ores::inbox::messaging::list_notification_kinds_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::inbox::messaging::list_notification_kinds_request& v);
    static ores::inbox::messaging::list_notification_kinds_request read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::inbox::messaging::list_notification_kinds_request& v);
    static ores::inbox::messaging::list_notification_kinds_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_INBOX_API_EXPORT wire_traits<ores::inbox::messaging::list_notification_kinds_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::inbox::messaging::list_notification_kinds_response& v);
    static ores::inbox::messaging::list_notification_kinds_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::inbox::messaging::list_notification_kinds_response& v);
    static ores::inbox::messaging::list_notification_kinds_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_INBOX_API_EXPORT wire_traits<ores::inbox::messaging::get_notification_kind_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::inbox::messaging::get_notification_kind_request& v);
    static ores::inbox::messaging::get_notification_kind_request read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::inbox::messaging::get_notification_kind_request& v);
    static ores::inbox::messaging::get_notification_kind_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_INBOX_API_EXPORT wire_traits<ores::inbox::messaging::get_notification_kind_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::inbox::messaging::get_notification_kind_response& v);
    static ores::inbox::messaging::get_notification_kind_response read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::inbox::messaging::get_notification_kind_response& v);
    static ores::inbox::messaging::get_notification_kind_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_INBOX_API_EXPORT
    wire_traits<ores::inbox::messaging::get_many_notification_kinds_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::inbox::messaging::get_many_notification_kinds_request& v);
    static ores::inbox::messaging::get_many_notification_kinds_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::inbox::messaging::get_many_notification_kinds_request& v);
    static ores::inbox::messaging::get_many_notification_kinds_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_INBOX_API_EXPORT
    wire_traits<ores::inbox::messaging::get_many_notification_kinds_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::inbox::messaging::get_many_notification_kinds_response& v);
    static ores::inbox::messaging::get_many_notification_kinds_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::inbox::messaging::get_many_notification_kinds_response& v);
    static ores::inbox::messaging::get_many_notification_kinds_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_INBOX_API_EXPORT wire_traits<ores::inbox::messaging::put_notification_kind_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::inbox::messaging::put_notification_kind_request& v);
    static ores::inbox::messaging::put_notification_kind_request read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::inbox::messaging::put_notification_kind_request& v);
    static ores::inbox::messaging::put_notification_kind_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_INBOX_API_EXPORT wire_traits<ores::inbox::messaging::put_notification_kind_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::inbox::messaging::put_notification_kind_response& v);
    static ores::inbox::messaging::put_notification_kind_response read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::inbox::messaging::put_notification_kind_response& v);
    static ores::inbox::messaging::put_notification_kind_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_INBOX_API_EXPORT
    wire_traits<ores::inbox::messaging::put_many_notification_kinds_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::inbox::messaging::put_many_notification_kinds_request& v);
    static ores::inbox::messaging::put_many_notification_kinds_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::inbox::messaging::put_many_notification_kinds_request& v);
    static ores::inbox::messaging::put_many_notification_kinds_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_INBOX_API_EXPORT
    wire_traits<ores::inbox::messaging::put_many_notification_kinds_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::inbox::messaging::put_many_notification_kinds_response& v);
    static ores::inbox::messaging::put_many_notification_kinds_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::inbox::messaging::put_many_notification_kinds_response& v);
    static ores::inbox::messaging::put_many_notification_kinds_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_INBOX_API_EXPORT wire_traits<ores::inbox::messaging::delete_notification_kind_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::inbox::messaging::delete_notification_kind_request& v);
    static ores::inbox::messaging::delete_notification_kind_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::inbox::messaging::delete_notification_kind_request& v);
    static ores::inbox::messaging::delete_notification_kind_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_INBOX_API_EXPORT
    wire_traits<ores::inbox::messaging::delete_notification_kind_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::inbox::messaging::delete_notification_kind_response& v);
    static ores::inbox::messaging::delete_notification_kind_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::inbox::messaging::delete_notification_kind_response& v);
    static ores::inbox::messaging::delete_notification_kind_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_INBOX_API_EXPORT
    wire_traits<ores::inbox::messaging::delete_many_notification_kinds_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::inbox::messaging::delete_many_notification_kinds_request& v);
    static ores::inbox::messaging::delete_many_notification_kinds_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::inbox::messaging::delete_many_notification_kinds_request& v);
    static ores::inbox::messaging::delete_many_notification_kinds_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_INBOX_API_EXPORT
    wire_traits<ores::inbox::messaging::delete_many_notification_kinds_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::inbox::messaging::delete_many_notification_kinds_response& v);
    static ores::inbox::messaging::delete_many_notification_kinds_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::inbox::messaging::delete_many_notification_kinds_response& v);
    static ores::inbox::messaging::delete_many_notification_kinds_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_INBOX_API_EXPORT
    wire_traits<ores::inbox::messaging::list_notification_kind_versions_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::inbox::messaging::list_notification_kind_versions_request& v);
    static ores::inbox::messaging::list_notification_kind_versions_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::inbox::messaging::list_notification_kind_versions_request& v);
    static ores::inbox::messaging::list_notification_kind_versions_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_INBOX_API_EXPORT
    wire_traits<ores::inbox::messaging::list_notification_kind_versions_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::inbox::messaging::list_notification_kind_versions_response& v);
    static ores::inbox::messaging::list_notification_kind_versions_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::inbox::messaging::list_notification_kind_versions_response& v);
    static ores::inbox::messaging::list_notification_kind_versions_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_INBOX_API_EXPORT
    wire_traits<ores::inbox::messaging::get_notification_kind_version_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::inbox::messaging::get_notification_kind_version_request& v);
    static ores::inbox::messaging::get_notification_kind_version_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::inbox::messaging::get_notification_kind_version_request& v);
    static ores::inbox::messaging::get_notification_kind_version_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_INBOX_API_EXPORT
    wire_traits<ores::inbox::messaging::get_notification_kind_version_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::inbox::messaging::get_notification_kind_version_response& v);
    static ores::inbox::messaging::get_notification_kind_version_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::inbox::messaging::get_notification_kind_version_response& v);
    static ores::inbox::messaging::get_notification_kind_version_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
