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

struct notification_recipient_key {
    boost::uuids::uuid notification_id;
    boost::uuids::uuid account_id;

    friend bool operator==(const notification_recipient_key&,
                           const notification_recipient_key&) = default;
};

struct notification_recipient_write {
    boost::uuids::uuid notification_id;
    boost::uuids::uuid account_id;
    std::optional<std::chrono::system_clock::time_point> read_at;
    std::optional<std::chrono::system_clock::time_point> cleared_at;

    friend bool operator==(const notification_recipient_write&,
                           const notification_recipient_write&) = default;
};

struct notification_recipient_change {
    notification_recipient_write write;
    ores::utility::domain::precondition precondition;

    friend bool operator==(const notification_recipient_change&,
                           const notification_recipient_change&) = default;
};

struct notification_recipient_removal {
    notification_recipient_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;

    friend bool operator==(const notification_recipient_removal&,
                           const notification_recipient_removal&) = default;
};

struct notification_recipient_lookup {
    notification_recipient_key key;
    std::optional<ores::inbox::domain::notification_recipient> notification_recipient;

    friend bool operator==(const notification_recipient_lookup&,
                           const notification_recipient_lookup&) = default;
};

struct notification_recipients_filter {
    std::optional<boost::uuids::uuid> account_id;
    std::optional<std::vector<boost::uuids::uuid>> account_id_one_of;

    friend bool operator==(const notification_recipients_filter&,
                           const notification_recipients_filter&) = default;
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

    friend bool operator==(const list_notification_recipients_request&,
                           const list_notification_recipients_request&) = default;
};

struct list_notification_recipients_response {
    ores::utility::domain::result result;
    std::vector<ores::inbox::domain::notification_recipient> notification_recipients;
    std::uint64_t total{};

    friend bool operator==(const list_notification_recipients_response&,
                           const list_notification_recipients_response&) = default;
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

    friend bool operator==(const get_notification_recipient_request&,
                           const get_notification_recipient_request&) = default;
};

struct get_notification_recipient_response {
    ores::utility::domain::result result;
    std::optional<ores::inbox::domain::notification_recipient> notification_recipient;

    friend bool operator==(const get_notification_recipient_response&,
                           const get_notification_recipient_response&) = default;
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

    friend bool operator==(const get_many_notification_recipients_request&,
                           const get_many_notification_recipients_request&) = default;
};

struct get_many_notification_recipients_response {
    ores::utility::domain::result result;
    std::vector<notification_recipient_lookup> entries;

    friend bool operator==(const get_many_notification_recipients_response&,
                           const get_many_notification_recipients_response&) = default;
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

    friend bool operator==(const put_notification_recipient_request&,
                           const put_notification_recipient_request&) = default;
};

struct put_notification_recipient_response {
    ores::utility::domain::result result;
    std::optional<ores::inbox::domain::notification_recipient> notification_recipient;

    friend bool operator==(const put_notification_recipient_response&,
                           const put_notification_recipient_response&) = default;
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

    friend bool operator==(const put_many_notification_recipients_request&,
                           const put_many_notification_recipients_request&) = default;
};

struct put_many_notification_recipients_response {
    ores::utility::domain::result result;
    std::vector<ores::inbox::domain::notification_recipient> notification_recipients;

    friend bool operator==(const put_many_notification_recipients_response&,
                           const put_many_notification_recipients_response&) = default;
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

    friend bool operator==(const delete_notification_recipient_request&,
                           const delete_notification_recipient_request&) = default;
};

struct delete_notification_recipient_response {
    ores::utility::domain::result result;

    friend bool operator==(const delete_notification_recipient_response&,
                           const delete_notification_recipient_response&) = default;
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

    friend bool operator==(const delete_many_notification_recipients_request&,
                           const delete_many_notification_recipients_request&) = default;
};

struct delete_many_notification_recipients_response {
    ores::utility::domain::result result;

    friend bool operator==(const delete_many_notification_recipients_response&,
                           const delete_many_notification_recipients_response&) = default;
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

    friend bool operator==(const list_by_account_id_notification_recipients_request&,
                           const list_by_account_id_notification_recipients_request&) = default;
};

struct list_by_account_id_notification_recipients_response {
    ores::utility::domain::result result;
    std::vector<ores::inbox::domain::notification_recipient> notification_recipients;
    std::uint64_t total{};

    friend bool operator==(const list_by_account_id_notification_recipients_response&,
                           const list_by_account_id_notification_recipients_response&) = default;
};

/**
 * @brief The members of notification_recipient_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<notification_recipient_key>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"notification_id", &notification_recipient_key::notification_id},
        utility::codec::field{"account_id", &notification_recipient_key::account_id},
    };
}

/**
 * @brief The members of notification_recipient_write, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<notification_recipient_write>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"notification_id", &notification_recipient_write::notification_id},
        utility::codec::field{"account_id", &notification_recipient_write::account_id},
        utility::codec::field{"read_at", &notification_recipient_write::read_at},
        utility::codec::field{"cleared_at", &notification_recipient_write::cleared_at},
    };
}

/**
 * @brief The members of notification_recipient_change, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<notification_recipient_change>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"write", &notification_recipient_change::write},
        utility::codec::field{"precondition", &notification_recipient_change::precondition},
    };
}

/**
 * @brief The members of notification_recipient_removal, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<notification_recipient_removal>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"key", &notification_recipient_removal::key},
        utility::codec::field{"precondition", &notification_recipient_removal::precondition},
    };
}

/**
 * @brief The members of notification_recipient_lookup, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<notification_recipient_lookup>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"key", &notification_recipient_lookup::key},
        utility::codec::field{"notification_recipient",
                              &notification_recipient_lookup::notification_recipient},
    };
}

/**
 * @brief The members of notification_recipients_filter, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<notification_recipients_filter>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"account_id", &notification_recipients_filter::account_id},
        utility::codec::field{"account_id_one_of",
                              &notification_recipients_filter::account_id_one_of},
    };
}

/**
 * @brief The members of list_notification_recipients_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<list_notification_recipients_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"offset", &list_notification_recipients_request::offset},
        utility::codec::field{"limit", &list_notification_recipients_request::limit},
        utility::codec::field{"order", &list_notification_recipients_request::order},
        utility::codec::field{"filter", &list_notification_recipients_request::filter},
    };
}

/**
 * @brief The members of list_notification_recipients_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<list_notification_recipients_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &list_notification_recipients_response::result},
        utility::codec::field{"notification_recipients",
                              &list_notification_recipients_response::notification_recipients},
        utility::codec::field{"total", &list_notification_recipients_response::total},
    };
}

/**
 * @brief The members of get_notification_recipient_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_notification_recipient_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"key", &get_notification_recipient_request::key},
    };
}

/**
 * @brief The members of get_notification_recipient_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_notification_recipient_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &get_notification_recipient_response::result},
        utility::codec::field{"notification_recipient",
                              &get_notification_recipient_response::notification_recipient},
    };
}

/**
 * @brief The members of get_many_notification_recipients_request, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<get_many_notification_recipients_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"keys", &get_many_notification_recipients_request::keys},
    };
}

/**
 * @brief The members of get_many_notification_recipients_response, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<get_many_notification_recipients_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &get_many_notification_recipients_response::result},
        utility::codec::field{"entries", &get_many_notification_recipients_response::entries},
    };
}

/**
 * @brief The members of put_notification_recipient_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<put_notification_recipient_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"change", &put_notification_recipient_request::change},
        utility::codec::field{"intent", &put_notification_recipient_request::intent},
    };
}

/**
 * @brief The members of put_notification_recipient_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<put_notification_recipient_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &put_notification_recipient_response::result},
        utility::codec::field{"notification_recipient",
                              &put_notification_recipient_response::notification_recipient},
    };
}

/**
 * @brief The members of put_many_notification_recipients_request, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<put_many_notification_recipients_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"changes", &put_many_notification_recipients_request::changes},
        utility::codec::field{"intent", &put_many_notification_recipients_request::intent},
    };
}

/**
 * @brief The members of put_many_notification_recipients_response, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<put_many_notification_recipients_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &put_many_notification_recipients_response::result},
        utility::codec::field{"notification_recipients",
                              &put_many_notification_recipients_response::notification_recipients},
    };
}

/**
 * @brief The members of delete_notification_recipient_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<delete_notification_recipient_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"removal", &delete_notification_recipient_request::removal},
        utility::codec::field{"intent", &delete_notification_recipient_request::intent},
    };
}

/**
 * @brief The members of delete_notification_recipient_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<delete_notification_recipient_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &delete_notification_recipient_response::result},
    };
}

/**
 * @brief The members of delete_many_notification_recipients_request, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<delete_many_notification_recipients_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"removals", &delete_many_notification_recipients_request::removals},
        utility::codec::field{"intent", &delete_many_notification_recipients_request::intent},
    };
}

/**
 * @brief The members of delete_many_notification_recipients_response, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<delete_many_notification_recipients_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &delete_many_notification_recipients_response::result},
    };
}

/**
 * @brief The members of list_by_account_id_notification_recipients_request, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<list_by_account_id_notification_recipients_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"account_id",
                              &list_by_account_id_notification_recipients_request::account_id},
        utility::codec::field{"scope", &list_by_account_id_notification_recipients_request::scope},
        utility::codec::field{"offset",
                              &list_by_account_id_notification_recipients_request::offset},
        utility::codec::field{"limit", &list_by_account_id_notification_recipients_request::limit},
        utility::codec::field{"order", &list_by_account_id_notification_recipients_request::order},
        utility::codec::field{"filter",
                              &list_by_account_id_notification_recipients_request::filter},
    };
}

/**
 * @brief The members of list_by_account_id_notification_recipients_response, named as the wire
 * names them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<list_by_account_id_notification_recipients_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result",
                              &list_by_account_id_notification_recipients_response::result},
        utility::codec::field{
            "notification_recipients",
            &list_by_account_id_notification_recipients_response::notification_recipients},
        utility::codec::field{"total", &list_by_account_id_notification_recipients_response::total},
    };
}

}
namespace ores::utility::codec {

template <>
struct ORES_INBOX_API_EXPORT wire_traits<ores::inbox::messaging::notification_recipient_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::inbox::messaging::notification_recipient_key& v);
    static ores::inbox::messaging::notification_recipient_key read_json(std::string_view text);
    static std::string write_msgpack(const ores::inbox::messaging::notification_recipient_key& v);
    static ores::inbox::messaging::notification_recipient_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_INBOX_API_EXPORT wire_traits<ores::inbox::messaging::notification_recipient_write> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::inbox::messaging::notification_recipient_write& v);
    static ores::inbox::messaging::notification_recipient_write read_json(std::string_view text);
    static std::string write_msgpack(const ores::inbox::messaging::notification_recipient_write& v);
    static ores::inbox::messaging::notification_recipient_write
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_INBOX_API_EXPORT wire_traits<ores::inbox::messaging::notification_recipient_change> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::inbox::messaging::notification_recipient_change& v);
    static ores::inbox::messaging::notification_recipient_change read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::inbox::messaging::notification_recipient_change& v);
    static ores::inbox::messaging::notification_recipient_change
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_INBOX_API_EXPORT wire_traits<ores::inbox::messaging::notification_recipient_removal> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::inbox::messaging::notification_recipient_removal& v);
    static ores::inbox::messaging::notification_recipient_removal read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::inbox::messaging::notification_recipient_removal& v);
    static ores::inbox::messaging::notification_recipient_removal
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_INBOX_API_EXPORT wire_traits<ores::inbox::messaging::notification_recipient_lookup> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::inbox::messaging::notification_recipient_lookup& v);
    static ores::inbox::messaging::notification_recipient_lookup read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::inbox::messaging::notification_recipient_lookup& v);
    static ores::inbox::messaging::notification_recipient_lookup
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_INBOX_API_EXPORT wire_traits<ores::inbox::messaging::notification_recipients_filter> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::inbox::messaging::notification_recipients_filter& v);
    static ores::inbox::messaging::notification_recipients_filter read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::inbox::messaging::notification_recipients_filter& v);
    static ores::inbox::messaging::notification_recipients_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_INBOX_API_EXPORT
    wire_traits<ores::inbox::messaging::list_notification_recipients_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::inbox::messaging::list_notification_recipients_request& v);
    static ores::inbox::messaging::list_notification_recipients_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::inbox::messaging::list_notification_recipients_request& v);
    static ores::inbox::messaging::list_notification_recipients_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_INBOX_API_EXPORT
    wire_traits<ores::inbox::messaging::list_notification_recipients_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::inbox::messaging::list_notification_recipients_response& v);
    static ores::inbox::messaging::list_notification_recipients_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::inbox::messaging::list_notification_recipients_response& v);
    static ores::inbox::messaging::list_notification_recipients_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_INBOX_API_EXPORT
    wire_traits<ores::inbox::messaging::get_notification_recipient_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::inbox::messaging::get_notification_recipient_request& v);
    static ores::inbox::messaging::get_notification_recipient_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::inbox::messaging::get_notification_recipient_request& v);
    static ores::inbox::messaging::get_notification_recipient_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_INBOX_API_EXPORT
    wire_traits<ores::inbox::messaging::get_notification_recipient_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::inbox::messaging::get_notification_recipient_response& v);
    static ores::inbox::messaging::get_notification_recipient_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::inbox::messaging::get_notification_recipient_response& v);
    static ores::inbox::messaging::get_notification_recipient_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_INBOX_API_EXPORT
    wire_traits<ores::inbox::messaging::get_many_notification_recipients_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::inbox::messaging::get_many_notification_recipients_request& v);
    static ores::inbox::messaging::get_many_notification_recipients_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::inbox::messaging::get_many_notification_recipients_request& v);
    static ores::inbox::messaging::get_many_notification_recipients_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_INBOX_API_EXPORT
    wire_traits<ores::inbox::messaging::get_many_notification_recipients_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::inbox::messaging::get_many_notification_recipients_response& v);
    static ores::inbox::messaging::get_many_notification_recipients_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::inbox::messaging::get_many_notification_recipients_response& v);
    static ores::inbox::messaging::get_many_notification_recipients_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_INBOX_API_EXPORT
    wire_traits<ores::inbox::messaging::put_notification_recipient_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::inbox::messaging::put_notification_recipient_request& v);
    static ores::inbox::messaging::put_notification_recipient_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::inbox::messaging::put_notification_recipient_request& v);
    static ores::inbox::messaging::put_notification_recipient_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_INBOX_API_EXPORT
    wire_traits<ores::inbox::messaging::put_notification_recipient_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::inbox::messaging::put_notification_recipient_response& v);
    static ores::inbox::messaging::put_notification_recipient_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::inbox::messaging::put_notification_recipient_response& v);
    static ores::inbox::messaging::put_notification_recipient_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_INBOX_API_EXPORT
    wire_traits<ores::inbox::messaging::put_many_notification_recipients_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::inbox::messaging::put_many_notification_recipients_request& v);
    static ores::inbox::messaging::put_many_notification_recipients_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::inbox::messaging::put_many_notification_recipients_request& v);
    static ores::inbox::messaging::put_many_notification_recipients_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_INBOX_API_EXPORT
    wire_traits<ores::inbox::messaging::put_many_notification_recipients_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::inbox::messaging::put_many_notification_recipients_response& v);
    static ores::inbox::messaging::put_many_notification_recipients_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::inbox::messaging::put_many_notification_recipients_response& v);
    static ores::inbox::messaging::put_many_notification_recipients_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_INBOX_API_EXPORT
    wire_traits<ores::inbox::messaging::delete_notification_recipient_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::inbox::messaging::delete_notification_recipient_request& v);
    static ores::inbox::messaging::delete_notification_recipient_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::inbox::messaging::delete_notification_recipient_request& v);
    static ores::inbox::messaging::delete_notification_recipient_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_INBOX_API_EXPORT
    wire_traits<ores::inbox::messaging::delete_notification_recipient_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::inbox::messaging::delete_notification_recipient_response& v);
    static ores::inbox::messaging::delete_notification_recipient_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::inbox::messaging::delete_notification_recipient_response& v);
    static ores::inbox::messaging::delete_notification_recipient_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_INBOX_API_EXPORT
    wire_traits<ores::inbox::messaging::delete_many_notification_recipients_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::inbox::messaging::delete_many_notification_recipients_request& v);
    static ores::inbox::messaging::delete_many_notification_recipients_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::inbox::messaging::delete_many_notification_recipients_request& v);
    static ores::inbox::messaging::delete_many_notification_recipients_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_INBOX_API_EXPORT
    wire_traits<ores::inbox::messaging::delete_many_notification_recipients_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::inbox::messaging::delete_many_notification_recipients_response& v);
    static ores::inbox::messaging::delete_many_notification_recipients_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::inbox::messaging::delete_many_notification_recipients_response& v);
    static ores::inbox::messaging::delete_many_notification_recipients_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_INBOX_API_EXPORT
    wire_traits<ores::inbox::messaging::list_by_account_id_notification_recipients_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::inbox::messaging::list_by_account_id_notification_recipients_request& v);
    static ores::inbox::messaging::list_by_account_id_notification_recipients_request
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::inbox::messaging::list_by_account_id_notification_recipients_request& v);
    static ores::inbox::messaging::list_by_account_id_notification_recipients_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_INBOX_API_EXPORT
    wire_traits<ores::inbox::messaging::list_by_account_id_notification_recipients_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::inbox::messaging::list_by_account_id_notification_recipients_response& v);
    static ores::inbox::messaging::list_by_account_id_notification_recipients_response
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::inbox::messaging::list_by_account_id_notification_recipients_response& v);
    static ores::inbox::messaging::list_by_account_id_notification_recipients_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
