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
#ifndef ORES_ORGMODE_API_MESSAGING_KNOWLEDGE_PAGE_PROTOCOL_HPP
#define ORES_ORGMODE_API_MESSAGING_KNOWLEDGE_PAGE_PROTOCOL_HPP

#include "ores.orgmode.api/domain/knowledge_page_codec.hpp"
#include "ores.orgmode.api/export.hpp"
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

namespace ores::orgmode::messaging {

struct knowledge_page_key {
    std::string id;

    friend bool operator==(const knowledge_page_key&, const knowledge_page_key&) = default;
};

struct knowledge_page_lookup {
    knowledge_page_key key;
    std::optional<ores::orgmode::domain::knowledge_page> knowledge_page;

    friend bool operator==(const knowledge_page_lookup&, const knowledge_page_lookup&) = default;
};

struct knowledge_pages_filter {
    std::optional<std::vector<std::string>> id_one_of;

    friend bool operator==(const knowledge_pages_filter&, const knowledge_pages_filter&) = default;
};

struct knowledge_page_event {
    boost::uuids::uuid event_id;
    knowledge_page_key key;
    std::string action;
    std::uint32_t version{};
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;

    friend bool operator==(const knowledge_page_event&, const knowledge_page_event&) = default;
};

struct knowledge_page_version_key {
    knowledge_page_key knowledge_page;
    std::uint32_t version{};

    friend bool operator==(const knowledge_page_version_key&,
                           const knowledge_page_version_key&) = default;
};

struct knowledge_page_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;

    friend bool operator==(const knowledge_page_versions_filter&,
                           const knowledge_page_versions_filter&) = default;
};

struct list_knowledge_pages_request {
    using response_type = struct list_knowledge_pages_response;
    static constexpr std::string_view nats_subject = "orgmode.v1.knowledge_pages.list";
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
    std::optional<knowledge_pages_filter> filter;
    std::optional<std::string> as_of;

    friend bool operator==(const list_knowledge_pages_request&,
                           const list_knowledge_pages_request&) = default;
};

struct list_knowledge_pages_response {
    ores::utility::domain::result result;
    std::vector<ores::orgmode::domain::knowledge_page> pages;
    std::uint64_t total{};

    friend bool operator==(const list_knowledge_pages_response&,
                           const list_knowledge_pages_response&) = default;
};

struct get_knowledge_page_request {
    using response_type = struct get_knowledge_page_response;
    static constexpr std::string_view nats_subject = "orgmode.v1.knowledge_pages.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    knowledge_page_key key;

    friend bool operator==(const get_knowledge_page_request&,
                           const get_knowledge_page_request&) = default;
};

struct get_knowledge_page_response {
    ores::utility::domain::result result;
    std::optional<ores::orgmode::domain::knowledge_page> knowledge_page;

    friend bool operator==(const get_knowledge_page_response&,
                           const get_knowledge_page_response&) = default;
};

struct get_many_knowledge_pages_request {
    using response_type = struct get_many_knowledge_pages_response;
    static constexpr std::string_view nats_subject = "orgmode.v1.knowledge_pages.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<knowledge_page_key> keys;

    friend bool operator==(const get_many_knowledge_pages_request&,
                           const get_many_knowledge_pages_request&) = default;
};

struct get_many_knowledge_pages_response {
    ores::utility::domain::result result;
    std::vector<knowledge_page_lookup> entries;

    friend bool operator==(const get_many_knowledge_pages_response&,
                           const get_many_knowledge_pages_response&) = default;
};

struct list_knowledge_page_versions_request {
    using response_type = struct list_knowledge_page_versions_response;
    static constexpr std::string_view nats_subject = "orgmode.v1.knowledge_pages_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    knowledge_page_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<knowledge_page_versions_filter> filter;

    friend bool operator==(const list_knowledge_page_versions_request&,
                           const list_knowledge_page_versions_request&) = default;
};

struct list_knowledge_page_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::orgmode::domain::knowledge_page> versions;
    std::uint64_t total{};

    friend bool operator==(const list_knowledge_page_versions_response&,
                           const list_knowledge_page_versions_response&) = default;
};

struct get_knowledge_page_version_request {
    using response_type = struct get_knowledge_page_version_response;
    static constexpr std::string_view nats_subject = "orgmode.v1.knowledge_pages_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    knowledge_page_version_key key;

    friend bool operator==(const get_knowledge_page_version_request&,
                           const get_knowledge_page_version_request&) = default;
};

struct get_knowledge_page_version_response {
    ores::utility::domain::result result;
    std::optional<ores::orgmode::domain::knowledge_page> version;

    friend bool operator==(const get_knowledge_page_version_response&,
                           const get_knowledge_page_version_response&) = default;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace knowledge_page_event_subjects {
inline constexpr std::string_view created = "orgmode.v1.events.knowledge_pages.created";
inline constexpr std::string_view updated = "orgmode.v1.events.knowledge_pages.updated";
inline constexpr std::string_view deleted = "orgmode.v1.events.knowledge_pages.deleted";
}
/**
 * @brief The members of knowledge_page_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<knowledge_page_key>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"id", &knowledge_page_key::id},
    };
}

/**
 * @brief The members of knowledge_page_lookup, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<knowledge_page_lookup>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"key", &knowledge_page_lookup::key},
        utility::codec::field{"knowledge_page", &knowledge_page_lookup::knowledge_page},
    };
}

/**
 * @brief The members of knowledge_pages_filter, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<knowledge_pages_filter>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"id_one_of", &knowledge_pages_filter::id_one_of},
    };
}

/**
 * @brief The members of knowledge_page_event, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<knowledge_page_event>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"event_id", &knowledge_page_event::event_id},
        utility::codec::field{"key", &knowledge_page_event::key},
        utility::codec::field{"action", &knowledge_page_event::action},
        utility::codec::field{"version", &knowledge_page_event::version},
        utility::codec::field{"occurred_at", &knowledge_page_event::occurred_at},
        utility::codec::field{"correlation_id", &knowledge_page_event::correlation_id},
    };
}

/**
 * @brief The members of knowledge_page_version_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<knowledge_page_version_key>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"knowledge_page", &knowledge_page_version_key::knowledge_page},
        utility::codec::field{"version", &knowledge_page_version_key::version},
    };
}

/**
 * @brief The members of knowledge_page_versions_filter, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<knowledge_page_versions_filter>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"version", &knowledge_page_versions_filter::version},
        utility::codec::field{"from_version", &knowledge_page_versions_filter::from_version},
        utility::codec::field{"to_version", &knowledge_page_versions_filter::to_version},
    };
}

/**
 * @brief The members of list_knowledge_pages_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<list_knowledge_pages_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"offset", &list_knowledge_pages_request::offset},
        utility::codec::field{"limit", &list_knowledge_pages_request::limit},
        utility::codec::field{"order", &list_knowledge_pages_request::order},
        utility::codec::field{"filter", &list_knowledge_pages_request::filter},
        utility::codec::field{"as_of", &list_knowledge_pages_request::as_of},
    };
}

/**
 * @brief The members of list_knowledge_pages_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<list_knowledge_pages_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &list_knowledge_pages_response::result},
        utility::codec::field{"pages", &list_knowledge_pages_response::pages},
        utility::codec::field{"total", &list_knowledge_pages_response::total},
    };
}

/**
 * @brief The members of get_knowledge_page_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_knowledge_page_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"key", &get_knowledge_page_request::key},
    };
}

/**
 * @brief The members of get_knowledge_page_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_knowledge_page_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &get_knowledge_page_response::result},
        utility::codec::field{"knowledge_page", &get_knowledge_page_response::knowledge_page},
    };
}

/**
 * @brief The members of get_many_knowledge_pages_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_many_knowledge_pages_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"keys", &get_many_knowledge_pages_request::keys},
    };
}

/**
 * @brief The members of get_many_knowledge_pages_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_many_knowledge_pages_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &get_many_knowledge_pages_response::result},
        utility::codec::field{"entries", &get_many_knowledge_pages_response::entries},
    };
}

/**
 * @brief The members of list_knowledge_page_versions_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<list_knowledge_page_versions_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"key", &list_knowledge_page_versions_request::key},
        utility::codec::field{"offset", &list_knowledge_page_versions_request::offset},
        utility::codec::field{"limit", &list_knowledge_page_versions_request::limit},
        utility::codec::field{"order", &list_knowledge_page_versions_request::order},
        utility::codec::field{"filter", &list_knowledge_page_versions_request::filter},
    };
}

/**
 * @brief The members of list_knowledge_page_versions_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<list_knowledge_page_versions_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &list_knowledge_page_versions_response::result},
        utility::codec::field{"versions", &list_knowledge_page_versions_response::versions},
        utility::codec::field{"total", &list_knowledge_page_versions_response::total},
    };
}

/**
 * @brief The members of get_knowledge_page_version_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_knowledge_page_version_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"key", &get_knowledge_page_version_request::key},
    };
}

/**
 * @brief The members of get_knowledge_page_version_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_knowledge_page_version_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &get_knowledge_page_version_response::result},
        utility::codec::field{"version", &get_knowledge_page_version_response::version},
    };
}


}
namespace ores::utility::codec {

template <>
struct ORES_ORGMODE_API_EXPORT wire_traits<ores::orgmode::messaging::knowledge_page_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::orgmode::messaging::knowledge_page_key& v);
    static ores::orgmode::messaging::knowledge_page_key read_json(std::string_view text);
    static std::string write_msgpack(const ores::orgmode::messaging::knowledge_page_key& v);
    static ores::orgmode::messaging::knowledge_page_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ORGMODE_API_EXPORT wire_traits<ores::orgmode::messaging::knowledge_page_lookup> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::orgmode::messaging::knowledge_page_lookup& v);
    static ores::orgmode::messaging::knowledge_page_lookup read_json(std::string_view text);
    static std::string write_msgpack(const ores::orgmode::messaging::knowledge_page_lookup& v);
    static ores::orgmode::messaging::knowledge_page_lookup
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ORGMODE_API_EXPORT wire_traits<ores::orgmode::messaging::knowledge_pages_filter> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::orgmode::messaging::knowledge_pages_filter& v);
    static ores::orgmode::messaging::knowledge_pages_filter read_json(std::string_view text);
    static std::string write_msgpack(const ores::orgmode::messaging::knowledge_pages_filter& v);
    static ores::orgmode::messaging::knowledge_pages_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ORGMODE_API_EXPORT wire_traits<ores::orgmode::messaging::knowledge_page_event> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::orgmode::messaging::knowledge_page_event& v);
    static ores::orgmode::messaging::knowledge_page_event read_json(std::string_view text);
    static std::string write_msgpack(const ores::orgmode::messaging::knowledge_page_event& v);
    static ores::orgmode::messaging::knowledge_page_event
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ORGMODE_API_EXPORT wire_traits<ores::orgmode::messaging::knowledge_page_version_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::orgmode::messaging::knowledge_page_version_key& v);
    static ores::orgmode::messaging::knowledge_page_version_key read_json(std::string_view text);
    static std::string write_msgpack(const ores::orgmode::messaging::knowledge_page_version_key& v);
    static ores::orgmode::messaging::knowledge_page_version_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ORGMODE_API_EXPORT
    wire_traits<ores::orgmode::messaging::knowledge_page_versions_filter> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::orgmode::messaging::knowledge_page_versions_filter& v);
    static ores::orgmode::messaging::knowledge_page_versions_filter
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::orgmode::messaging::knowledge_page_versions_filter& v);
    static ores::orgmode::messaging::knowledge_page_versions_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ORGMODE_API_EXPORT wire_traits<ores::orgmode::messaging::list_knowledge_pages_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::orgmode::messaging::list_knowledge_pages_request& v);
    static ores::orgmode::messaging::list_knowledge_pages_request read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::orgmode::messaging::list_knowledge_pages_request& v);
    static ores::orgmode::messaging::list_knowledge_pages_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ORGMODE_API_EXPORT
    wire_traits<ores::orgmode::messaging::list_knowledge_pages_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::orgmode::messaging::list_knowledge_pages_response& v);
    static ores::orgmode::messaging::list_knowledge_pages_response read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::orgmode::messaging::list_knowledge_pages_response& v);
    static ores::orgmode::messaging::list_knowledge_pages_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ORGMODE_API_EXPORT wire_traits<ores::orgmode::messaging::get_knowledge_page_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::orgmode::messaging::get_knowledge_page_request& v);
    static ores::orgmode::messaging::get_knowledge_page_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::orgmode::messaging::get_knowledge_page_request& v);
    static ores::orgmode::messaging::get_knowledge_page_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ORGMODE_API_EXPORT wire_traits<ores::orgmode::messaging::get_knowledge_page_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::orgmode::messaging::get_knowledge_page_response& v);
    static ores::orgmode::messaging::get_knowledge_page_response read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::orgmode::messaging::get_knowledge_page_response& v);
    static ores::orgmode::messaging::get_knowledge_page_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ORGMODE_API_EXPORT
    wire_traits<ores::orgmode::messaging::get_many_knowledge_pages_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::orgmode::messaging::get_many_knowledge_pages_request& v);
    static ores::orgmode::messaging::get_many_knowledge_pages_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::orgmode::messaging::get_many_knowledge_pages_request& v);
    static ores::orgmode::messaging::get_many_knowledge_pages_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ORGMODE_API_EXPORT
    wire_traits<ores::orgmode::messaging::get_many_knowledge_pages_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::orgmode::messaging::get_many_knowledge_pages_response& v);
    static ores::orgmode::messaging::get_many_knowledge_pages_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::orgmode::messaging::get_many_knowledge_pages_response& v);
    static ores::orgmode::messaging::get_many_knowledge_pages_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ORGMODE_API_EXPORT
    wire_traits<ores::orgmode::messaging::list_knowledge_page_versions_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::orgmode::messaging::list_knowledge_page_versions_request& v);
    static ores::orgmode::messaging::list_knowledge_page_versions_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::orgmode::messaging::list_knowledge_page_versions_request& v);
    static ores::orgmode::messaging::list_knowledge_page_versions_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ORGMODE_API_EXPORT
    wire_traits<ores::orgmode::messaging::list_knowledge_page_versions_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::orgmode::messaging::list_knowledge_page_versions_response& v);
    static ores::orgmode::messaging::list_knowledge_page_versions_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::orgmode::messaging::list_knowledge_page_versions_response& v);
    static ores::orgmode::messaging::list_knowledge_page_versions_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ORGMODE_API_EXPORT
    wire_traits<ores::orgmode::messaging::get_knowledge_page_version_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::orgmode::messaging::get_knowledge_page_version_request& v);
    static ores::orgmode::messaging::get_knowledge_page_version_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::orgmode::messaging::get_knowledge_page_version_request& v);
    static ores::orgmode::messaging::get_knowledge_page_version_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ORGMODE_API_EXPORT
    wire_traits<ores::orgmode::messaging::get_knowledge_page_version_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::orgmode::messaging::get_knowledge_page_version_response& v);
    static ores::orgmode::messaging::get_knowledge_page_version_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::orgmode::messaging::get_knowledge_page_version_response& v);
    static ores::orgmode::messaging::get_knowledge_page_version_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
