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
#ifndef ORES_DQ_API_MESSAGING_LEI_BIC_PROTOCOL_HPP
#define ORES_DQ_API_MESSAGING_LEI_BIC_PROTOCOL_HPP

#include "ores.dq.api/domain/lei_bic.hpp"
#include "ores.dq.api/export.hpp"
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

namespace ores::dq::messaging {

struct lei_bic_key {
    std::string lei;
};

struct lei_bic_lookup {
    lei_bic_key key;
    std::optional<ores::dq::domain::lei_bic> lei_bic;
};

struct lei_bic_filter {
    std::optional<std::vector<std::string>> lei_one_of;
};

struct lei_bic_event {
    boost::uuids::uuid event_id;
    lei_bic_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;
};

struct list_lei_bic_request {
    using response_type = struct list_lei_bic_response;
    static constexpr std::string_view nats_subject = "dq.v1.lei_bic.list";
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
    std::optional<lei_bic_filter> filter;
};

struct list_lei_bic_response {
    ores::utility::domain::result result;
    std::vector<ores::dq::domain::lei_bic> lei_bic;
    std::uint64_t total;
};

struct get_lei_bic_request {
    using response_type = struct get_lei_bic_response;
    static constexpr std::string_view nats_subject = "dq.v1.lei_bic.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    lei_bic_key key;
};

struct get_lei_bic_response {
    ores::utility::domain::result result;
    std::optional<ores::dq::domain::lei_bic> lei_bic;
};

struct get_many_lei_bic_request {
    using response_type = struct get_many_lei_bic_response;
    static constexpr std::string_view nats_subject = "dq.v1.lei_bic.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<lei_bic_key> keys;
};

struct get_many_lei_bic_response {
    ores::utility::domain::result result;
    std::vector<lei_bic_lookup> entries;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace lei_bic_event_subjects {
inline constexpr std::string_view created = "dq.v1.lei_bic_events.created";
inline constexpr std::string_view updated = "dq.v1.lei_bic_events.updated";
inline constexpr std::string_view deleted = "dq.v1.lei_bic_events.deleted";
}
/**
 * @brief The members of lei_bic_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const lei_bic_key&) {
    return std::tuple{
        utility::codec::field{"lei", &lei_bic_key::lei},
    };
}

/**
 * @brief The members of lei_bic_lookup, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const lei_bic_lookup&) {
    return std::tuple{
        utility::codec::field{"key", &lei_bic_lookup::key},
        utility::codec::field{"lei_bic", &lei_bic_lookup::lei_bic},
    };
}

/**
 * @brief The members of lei_bic_filter, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const lei_bic_filter&) {
    return std::tuple{
        utility::codec::field{"lei_one_of", &lei_bic_filter::lei_one_of},
    };
}

/**
 * @brief The members of lei_bic_event, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const lei_bic_event&) {
    return std::tuple{
        utility::codec::field{"event_id", &lei_bic_event::event_id},
        utility::codec::field{"key", &lei_bic_event::key},
        utility::codec::field{"action", &lei_bic_event::action},
        utility::codec::field{"version", &lei_bic_event::version},
        utility::codec::field{"occurred_at", &lei_bic_event::occurred_at},
        utility::codec::field{"correlation_id", &lei_bic_event::correlation_id},
    };
}

/**
 * @brief The members of list_lei_bic_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_lei_bic_request&) {
    return std::tuple{
        utility::codec::field{"offset", &list_lei_bic_request::offset},
        utility::codec::field{"limit", &list_lei_bic_request::limit},
        utility::codec::field{"order", &list_lei_bic_request::order},
        utility::codec::field{"filter", &list_lei_bic_request::filter},
    };
}

/**
 * @brief The members of list_lei_bic_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_lei_bic_response&) {
    return std::tuple{
        utility::codec::field{"result", &list_lei_bic_response::result},
        utility::codec::field{"lei_bic", &list_lei_bic_response::lei_bic},
        utility::codec::field{"total", &list_lei_bic_response::total},
    };
}

/**
 * @brief The members of get_lei_bic_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_lei_bic_request&) {
    return std::tuple{
        utility::codec::field{"key", &get_lei_bic_request::key},
    };
}

/**
 * @brief The members of get_lei_bic_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_lei_bic_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_lei_bic_response::result},
        utility::codec::field{"lei_bic", &get_lei_bic_response::lei_bic},
    };
}

/**
 * @brief The members of get_many_lei_bic_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_many_lei_bic_request&) {
    return std::tuple{
        utility::codec::field{"keys", &get_many_lei_bic_request::keys},
    };
}

/**
 * @brief The members of get_many_lei_bic_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_many_lei_bic_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_many_lei_bic_response::result},
        utility::codec::field{"entries", &get_many_lei_bic_response::entries},
    };
}


}
namespace ores::utility::codec {

template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::lei_bic_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::lei_bic_key& v);
    static ores::dq::messaging::lei_bic_key read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::lei_bic_key& v);
    static ores::dq::messaging::lei_bic_key read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::lei_bic_lookup> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::lei_bic_lookup& v);
    static ores::dq::messaging::lei_bic_lookup read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::lei_bic_lookup& v);
    static ores::dq::messaging::lei_bic_lookup read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::lei_bic_filter> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::lei_bic_filter& v);
    static ores::dq::messaging::lei_bic_filter read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::lei_bic_filter& v);
    static ores::dq::messaging::lei_bic_filter read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::lei_bic_event> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::lei_bic_event& v);
    static ores::dq::messaging::lei_bic_event read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::lei_bic_event& v);
    static ores::dq::messaging::lei_bic_event read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::list_lei_bic_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::list_lei_bic_request& v);
    static ores::dq::messaging::list_lei_bic_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::list_lei_bic_request& v);
    static ores::dq::messaging::list_lei_bic_request read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::list_lei_bic_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::list_lei_bic_response& v);
    static ores::dq::messaging::list_lei_bic_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::list_lei_bic_response& v);
    static ores::dq::messaging::list_lei_bic_response read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::get_lei_bic_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::get_lei_bic_request& v);
    static ores::dq::messaging::get_lei_bic_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::get_lei_bic_request& v);
    static ores::dq::messaging::get_lei_bic_request read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::get_lei_bic_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::get_lei_bic_response& v);
    static ores::dq::messaging::get_lei_bic_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::get_lei_bic_response& v);
    static ores::dq::messaging::get_lei_bic_response read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::get_many_lei_bic_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::get_many_lei_bic_request& v);
    static ores::dq::messaging::get_many_lei_bic_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::get_many_lei_bic_request& v);
    static ores::dq::messaging::get_many_lei_bic_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::get_many_lei_bic_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::get_many_lei_bic_response& v);
    static ores::dq::messaging::get_many_lei_bic_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::get_many_lei_bic_response& v);
    static ores::dq::messaging::get_many_lei_bic_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
