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
#ifndef ORES_IAM_API_MESSAGING_SESSION_PROTOCOL_HPP
#define ORES_IAM_API_MESSAGING_SESSION_PROTOCOL_HPP

#include "ores.iam.api/domain/session_codec.hpp"
#include "ores.iam.api/export.hpp"
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

namespace ores::iam::messaging {

struct session_key {
    boost::uuids::uuid id;

    friend bool operator==(const session_key&, const session_key&) = default;
};

struct session_write {
    boost::uuids::uuid id;
    std::chrono::system_clock::time_point start_time;
    boost::uuids::uuid account_id;
    std::optional<std::chrono::system_clock::time_point> end_time;
    boost::asio::ip::address client_ip;
    std::string client_identifier;
    std::uint16_t client_version_major;
    std::uint16_t client_version_minor;
    std::uint64_t bytes_sent;
    std::uint64_t bytes_received;
    std::string country_code;
    std::string protocol;

    friend bool operator==(const session_write&, const session_write&) = default;
};

struct session_change {
    session_write write;
    ores::utility::domain::precondition precondition;

    friend bool operator==(const session_change&, const session_change&) = default;
};

struct session_removal {
    session_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;

    friend bool operator==(const session_removal&, const session_removal&) = default;
};

struct session_lookup {
    session_key key;
    std::optional<ores::iam::domain::session> session;

    friend bool operator==(const session_lookup&, const session_lookup&) = default;
};

struct sessions_filter {
    std::optional<boost::uuids::uuid> account_id;
    std::optional<std::vector<boost::uuids::uuid>> account_id_one_of;

    friend bool operator==(const sessions_filter&, const sessions_filter&) = default;
};

struct session_event {
    boost::uuids::uuid event_id;
    session_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;

    friend bool operator==(const session_event&, const session_event&) = default;
};

struct list_sessions_request {
    using response_type = struct list_sessions_response;
    static constexpr std::string_view nats_subject = "iam.v1.sessions.list";
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
    std::optional<sessions_filter> filter;

    friend bool operator==(const list_sessions_request&, const list_sessions_request&) = default;
};

struct list_sessions_response {
    ores::utility::domain::result result;
    std::vector<ores::iam::domain::session> sessions;
    std::uint64_t total;

    friend bool operator==(const list_sessions_response&, const list_sessions_response&) = default;
};

struct get_session_request {
    using response_type = struct get_session_response;
    static constexpr std::string_view nats_subject = "iam.v1.sessions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    session_key key;

    friend bool operator==(const get_session_request&, const get_session_request&) = default;
};

struct get_session_response {
    ores::utility::domain::result result;
    std::optional<ores::iam::domain::session> session;

    friend bool operator==(const get_session_response&, const get_session_response&) = default;
};

struct get_many_sessions_request {
    using response_type = struct get_many_sessions_response;
    static constexpr std::string_view nats_subject = "iam.v1.sessions.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<session_key> keys;

    friend bool operator==(const get_many_sessions_request&,
                           const get_many_sessions_request&) = default;
};

struct get_many_sessions_response {
    ores::utility::domain::result result;
    std::vector<session_lookup> entries;

    friend bool operator==(const get_many_sessions_response&,
                           const get_many_sessions_response&) = default;
};

struct put_session_request {
    using response_type = struct put_session_response;
    static constexpr std::string_view nats_subject = "iam.v1.sessions.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    session_change change;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const put_session_request&, const put_session_request&) = default;
};

struct put_session_response {
    ores::utility::domain::result result;
    std::optional<ores::iam::domain::session> session;

    friend bool operator==(const put_session_response&, const put_session_response&) = default;
};

struct put_many_sessions_request {
    using response_type = struct put_many_sessions_response;
    static constexpr std::string_view nats_subject = "iam.v1.sessions.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<session_change> changes;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const put_many_sessions_request&,
                           const put_many_sessions_request&) = default;
};

struct put_many_sessions_response {
    ores::utility::domain::result result;
    std::vector<ores::iam::domain::session> sessions;

    friend bool operator==(const put_many_sessions_response&,
                           const put_many_sessions_response&) = default;
};

struct delete_session_request {
    using response_type = struct delete_session_response;
    static constexpr std::string_view nats_subject = "iam.v1.sessions.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    session_removal removal;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const delete_session_request&, const delete_session_request&) = default;
};

struct delete_session_response {
    ores::utility::domain::result result;

    friend bool operator==(const delete_session_response&,
                           const delete_session_response&) = default;
};

struct delete_many_sessions_request {
    using response_type = struct delete_many_sessions_response;
    static constexpr std::string_view nats_subject = "iam.v1.sessions.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<session_removal> removals;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const delete_many_sessions_request&,
                           const delete_many_sessions_request&) = default;
};

struct delete_many_sessions_response {
    ores::utility::domain::result result;

    friend bool operator==(const delete_many_sessions_response&,
                           const delete_many_sessions_response&) = default;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace session_event_subjects {
inline constexpr std::string_view created = "iam.v1.events.sessions.created";
inline constexpr std::string_view updated = "iam.v1.events.sessions.updated";
inline constexpr std::string_view deleted = "iam.v1.events.sessions.deleted";
}
/**
 * @brief The members of session_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<session_key>) {
    return std::tuple{
        utility::codec::field{"id", &session_key::id},
    };
}

/**
 * @brief The members of session_write, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<session_write>) {
    return std::tuple{
        utility::codec::field{"id", &session_write::id},
        utility::codec::field{"start_time", &session_write::start_time},
        utility::codec::field{"account_id", &session_write::account_id},
        utility::codec::field{"end_time", &session_write::end_time},
        utility::codec::field{"client_ip", &session_write::client_ip},
        utility::codec::field{"client_identifier", &session_write::client_identifier},
        utility::codec::field{"client_version_major", &session_write::client_version_major},
        utility::codec::field{"client_version_minor", &session_write::client_version_minor},
        utility::codec::field{"bytes_sent", &session_write::bytes_sent},
        utility::codec::field{"bytes_received", &session_write::bytes_received},
        utility::codec::field{"country_code", &session_write::country_code},
        utility::codec::field{"protocol", &session_write::protocol},
    };
}

/**
 * @brief The members of session_change, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<session_change>) {
    return std::tuple{
        utility::codec::field{"write", &session_change::write},
        utility::codec::field{"precondition", &session_change::precondition},
    };
}

/**
 * @brief The members of session_removal, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<session_removal>) {
    return std::tuple{
        utility::codec::field{"key", &session_removal::key},
        utility::codec::field{"precondition", &session_removal::precondition},
    };
}

/**
 * @brief The members of session_lookup, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<session_lookup>) {
    return std::tuple{
        utility::codec::field{"key", &session_lookup::key},
        utility::codec::field{"session", &session_lookup::session},
    };
}

/**
 * @brief The members of sessions_filter, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<sessions_filter>) {
    return std::tuple{
        utility::codec::field{"account_id", &sessions_filter::account_id},
        utility::codec::field{"account_id_one_of", &sessions_filter::account_id_one_of},
    };
}

/**
 * @brief The members of session_event, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<session_event>) {
    return std::tuple{
        utility::codec::field{"event_id", &session_event::event_id},
        utility::codec::field{"key", &session_event::key},
        utility::codec::field{"action", &session_event::action},
        utility::codec::field{"version", &session_event::version},
        utility::codec::field{"occurred_at", &session_event::occurred_at},
        utility::codec::field{"correlation_id", &session_event::correlation_id},
    };
}

/**
 * @brief The members of list_sessions_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<list_sessions_request>) {
    return std::tuple{
        utility::codec::field{"offset", &list_sessions_request::offset},
        utility::codec::field{"limit", &list_sessions_request::limit},
        utility::codec::field{"order", &list_sessions_request::order},
        utility::codec::field{"filter", &list_sessions_request::filter},
    };
}

/**
 * @brief The members of list_sessions_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<list_sessions_response>) {
    return std::tuple{
        utility::codec::field{"result", &list_sessions_response::result},
        utility::codec::field{"sessions", &list_sessions_response::sessions},
        utility::codec::field{"total", &list_sessions_response::total},
    };
}

/**
 * @brief The members of get_session_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_session_request>) {
    return std::tuple{
        utility::codec::field{"key", &get_session_request::key},
    };
}

/**
 * @brief The members of get_session_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_session_response>) {
    return std::tuple{
        utility::codec::field{"result", &get_session_response::result},
        utility::codec::field{"session", &get_session_response::session},
    };
}

/**
 * @brief The members of get_many_sessions_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_many_sessions_request>) {
    return std::tuple{
        utility::codec::field{"keys", &get_many_sessions_request::keys},
    };
}

/**
 * @brief The members of get_many_sessions_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_many_sessions_response>) {
    return std::tuple{
        utility::codec::field{"result", &get_many_sessions_response::result},
        utility::codec::field{"entries", &get_many_sessions_response::entries},
    };
}

/**
 * @brief The members of put_session_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<put_session_request>) {
    return std::tuple{
        utility::codec::field{"change", &put_session_request::change},
        utility::codec::field{"intent", &put_session_request::intent},
    };
}

/**
 * @brief The members of put_session_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<put_session_response>) {
    return std::tuple{
        utility::codec::field{"result", &put_session_response::result},
        utility::codec::field{"session", &put_session_response::session},
    };
}

/**
 * @brief The members of put_many_sessions_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<put_many_sessions_request>) {
    return std::tuple{
        utility::codec::field{"changes", &put_many_sessions_request::changes},
        utility::codec::field{"intent", &put_many_sessions_request::intent},
    };
}

/**
 * @brief The members of put_many_sessions_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<put_many_sessions_response>) {
    return std::tuple{
        utility::codec::field{"result", &put_many_sessions_response::result},
        utility::codec::field{"sessions", &put_many_sessions_response::sessions},
    };
}

/**
 * @brief The members of delete_session_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<delete_session_request>) {
    return std::tuple{
        utility::codec::field{"removal", &delete_session_request::removal},
        utility::codec::field{"intent", &delete_session_request::intent},
    };
}

/**
 * @brief The members of delete_session_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<delete_session_response>) {
    return std::tuple{
        utility::codec::field{"result", &delete_session_response::result},
    };
}

/**
 * @brief The members of delete_many_sessions_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<delete_many_sessions_request>) {
    return std::tuple{
        utility::codec::field{"removals", &delete_many_sessions_request::removals},
        utility::codec::field{"intent", &delete_many_sessions_request::intent},
    };
}

/**
 * @brief The members of delete_many_sessions_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<delete_many_sessions_response>) {
    return std::tuple{
        utility::codec::field{"result", &delete_many_sessions_response::result},
    };
}


}
namespace ores::utility::codec {

template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::session_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::session_key& v);
    static ores::iam::messaging::session_key read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::session_key& v);
    static ores::iam::messaging::session_key read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::session_write> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::session_write& v);
    static ores::iam::messaging::session_write read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::session_write& v);
    static ores::iam::messaging::session_write read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::session_change> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::session_change& v);
    static ores::iam::messaging::session_change read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::session_change& v);
    static ores::iam::messaging::session_change read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::session_removal> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::session_removal& v);
    static ores::iam::messaging::session_removal read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::session_removal& v);
    static ores::iam::messaging::session_removal read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::session_lookup> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::session_lookup& v);
    static ores::iam::messaging::session_lookup read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::session_lookup& v);
    static ores::iam::messaging::session_lookup read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::sessions_filter> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::sessions_filter& v);
    static ores::iam::messaging::sessions_filter read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::sessions_filter& v);
    static ores::iam::messaging::sessions_filter read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::session_event> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::session_event& v);
    static ores::iam::messaging::session_event read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::session_event& v);
    static ores::iam::messaging::session_event read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::list_sessions_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::list_sessions_request& v);
    static ores::iam::messaging::list_sessions_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::list_sessions_request& v);
    static ores::iam::messaging::list_sessions_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::list_sessions_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::list_sessions_response& v);
    static ores::iam::messaging::list_sessions_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::list_sessions_response& v);
    static ores::iam::messaging::list_sessions_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::get_session_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::get_session_request& v);
    static ores::iam::messaging::get_session_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::get_session_request& v);
    static ores::iam::messaging::get_session_request read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::get_session_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::get_session_response& v);
    static ores::iam::messaging::get_session_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::get_session_response& v);
    static ores::iam::messaging::get_session_response read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::get_many_sessions_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::get_many_sessions_request& v);
    static ores::iam::messaging::get_many_sessions_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::get_many_sessions_request& v);
    static ores::iam::messaging::get_many_sessions_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::get_many_sessions_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::get_many_sessions_response& v);
    static ores::iam::messaging::get_many_sessions_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::get_many_sessions_response& v);
    static ores::iam::messaging::get_many_sessions_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::put_session_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::put_session_request& v);
    static ores::iam::messaging::put_session_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::put_session_request& v);
    static ores::iam::messaging::put_session_request read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::put_session_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::put_session_response& v);
    static ores::iam::messaging::put_session_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::put_session_response& v);
    static ores::iam::messaging::put_session_response read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::put_many_sessions_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::put_many_sessions_request& v);
    static ores::iam::messaging::put_many_sessions_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::put_many_sessions_request& v);
    static ores::iam::messaging::put_many_sessions_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::put_many_sessions_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::put_many_sessions_response& v);
    static ores::iam::messaging::put_many_sessions_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::put_many_sessions_response& v);
    static ores::iam::messaging::put_many_sessions_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::delete_session_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::delete_session_request& v);
    static ores::iam::messaging::delete_session_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::delete_session_request& v);
    static ores::iam::messaging::delete_session_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::delete_session_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::delete_session_response& v);
    static ores::iam::messaging::delete_session_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::delete_session_response& v);
    static ores::iam::messaging::delete_session_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::delete_many_sessions_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::delete_many_sessions_request& v);
    static ores::iam::messaging::delete_many_sessions_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::delete_many_sessions_request& v);
    static ores::iam::messaging::delete_many_sessions_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::delete_many_sessions_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::delete_many_sessions_response& v);
    static ores::iam::messaging::delete_many_sessions_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::delete_many_sessions_response& v);
    static ores::iam::messaging::delete_many_sessions_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
