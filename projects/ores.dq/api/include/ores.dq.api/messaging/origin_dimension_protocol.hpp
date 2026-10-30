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
#ifndef ORES_DQ_API_MESSAGING_ORIGIN_DIMENSION_PROTOCOL_HPP
#define ORES_DQ_API_MESSAGING_ORIGIN_DIMENSION_PROTOCOL_HPP

#include "ores.dq.api/domain/origin_dimension.hpp"
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

struct origin_dimension_key {
    std::string code;
};

struct origin_dimension_write {
    std::string code;
    std::string name;
    std::string description;
};

struct origin_dimension_change {
    origin_dimension_write write;
    ores::utility::domain::precondition precondition;
};

struct origin_dimension_removal {
    origin_dimension_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;
};

struct origin_dimension_lookup {
    origin_dimension_key key;
    std::optional<ores::dq::domain::origin_dimension> origin_dimension;
};

struct origin_dimensions_filter {
    std::optional<std::vector<std::string>> code_one_of;
};

struct origin_dimension_event {
    boost::uuids::uuid event_id;
    origin_dimension_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;
};

struct origin_dimension_version_key {
    origin_dimension_key origin_dimension;
    std::uint32_t version;
};

struct origin_dimension_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;
};

struct list_origin_dimensions_request {
    using response_type = struct list_origin_dimensions_response;
    static constexpr std::string_view nats_subject = "dq.v1.origin_dimensions.list";
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
    std::optional<origin_dimensions_filter> filter;
    std::optional<std::string> as_of;
};

struct list_origin_dimensions_response {
    ores::utility::domain::result result;
    std::vector<ores::dq::domain::origin_dimension> dimensions;
    std::uint64_t total;
};

struct get_origin_dimension_request {
    using response_type = struct get_origin_dimension_response;
    static constexpr std::string_view nats_subject = "dq.v1.origin_dimensions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    origin_dimension_key key;
};

struct get_origin_dimension_response {
    ores::utility::domain::result result;
    std::optional<ores::dq::domain::origin_dimension> origin_dimension;
};

struct get_many_origin_dimensions_request {
    using response_type = struct get_many_origin_dimensions_response;
    static constexpr std::string_view nats_subject = "dq.v1.origin_dimensions.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<origin_dimension_key> keys;
};

struct get_many_origin_dimensions_response {
    ores::utility::domain::result result;
    std::vector<origin_dimension_lookup> entries;
};

struct put_origin_dimension_request {
    using response_type = struct put_origin_dimension_response;
    static constexpr std::string_view nats_subject = "dq.v1.origin_dimensions.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    origin_dimension_change change;
    ores::utility::domain::change_intent intent;
};

struct put_origin_dimension_response {
    ores::utility::domain::result result;
    std::optional<ores::dq::domain::origin_dimension> origin_dimension;
};

struct put_many_origin_dimensions_request {
    using response_type = struct put_many_origin_dimensions_response;
    static constexpr std::string_view nats_subject = "dq.v1.origin_dimensions.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<origin_dimension_change> changes;
    ores::utility::domain::change_intent intent;
};

struct put_many_origin_dimensions_response {
    ores::utility::domain::result result;
    std::vector<ores::dq::domain::origin_dimension> dimensions;
};

struct delete_origin_dimension_request {
    using response_type = struct delete_origin_dimension_response;
    static constexpr std::string_view nats_subject = "dq.v1.origin_dimensions.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    origin_dimension_removal removal;
    ores::utility::domain::change_intent intent;
};

struct delete_origin_dimension_response {
    ores::utility::domain::result result;
};

struct delete_many_origin_dimensions_request {
    using response_type = struct delete_many_origin_dimensions_response;
    static constexpr std::string_view nats_subject = "dq.v1.origin_dimensions.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<origin_dimension_removal> removals;
    ores::utility::domain::change_intent intent;
};

struct delete_many_origin_dimensions_response {
    ores::utility::domain::result result;
};

struct list_origin_dimension_versions_request {
    using response_type = struct list_origin_dimension_versions_response;
    static constexpr std::string_view nats_subject = "dq.v1.origin_dimensions_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    origin_dimension_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<origin_dimension_versions_filter> filter;
};

struct list_origin_dimension_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::dq::domain::origin_dimension> versions;
    std::uint64_t total;
};

struct get_origin_dimension_version_request {
    using response_type = struct get_origin_dimension_version_response;
    static constexpr std::string_view nats_subject = "dq.v1.origin_dimensions_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    origin_dimension_version_key key;
};

struct get_origin_dimension_version_response {
    ores::utility::domain::result result;
    std::optional<ores::dq::domain::origin_dimension> version;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace origin_dimension_event_subjects {
inline constexpr std::string_view created = "dq.v1.origin_dimensions_events.created";
inline constexpr std::string_view updated = "dq.v1.origin_dimensions_events.updated";
inline constexpr std::string_view deleted = "dq.v1.origin_dimensions_events.deleted";
}

/**
 * @brief The members of origin_dimension_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const origin_dimension_key&) {
    return std::tuple{
        utility::codec::field{"code", &origin_dimension_key::code},
    };
}

/**
 * @brief The members of origin_dimension_write, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const origin_dimension_write&) {
    return std::tuple{
        utility::codec::field{"code", &origin_dimension_write::code},
        utility::codec::field{"name", &origin_dimension_write::name},
        utility::codec::field{"description", &origin_dimension_write::description},
    };
}

/**
 * @brief The members of origin_dimension_change, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const origin_dimension_change&) {
    return std::tuple{
        utility::codec::field{"write", &origin_dimension_change::write},
        utility::codec::field{"precondition", &origin_dimension_change::precondition},
    };
}

/**
 * @brief The members of origin_dimension_removal, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const origin_dimension_removal&) {
    return std::tuple{
        utility::codec::field{"key", &origin_dimension_removal::key},
        utility::codec::field{"precondition", &origin_dimension_removal::precondition},
    };
}

/**
 * @brief The members of origin_dimension_lookup, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const origin_dimension_lookup&) {
    return std::tuple{
        utility::codec::field{"key", &origin_dimension_lookup::key},
        utility::codec::field{"origin_dimension", &origin_dimension_lookup::origin_dimension},
    };
}

/**
 * @brief The members of origin_dimensions_filter, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const origin_dimensions_filter&) {
    return std::tuple{
        utility::codec::field{"code_one_of", &origin_dimensions_filter::code_one_of},
    };
}

/**
 * @brief The members of origin_dimension_event, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const origin_dimension_event&) {
    return std::tuple{
        utility::codec::field{"event_id", &origin_dimension_event::event_id},
        utility::codec::field{"key", &origin_dimension_event::key},
        utility::codec::field{"action", &origin_dimension_event::action},
        utility::codec::field{"version", &origin_dimension_event::version},
        utility::codec::field{"occurred_at", &origin_dimension_event::occurred_at},
        utility::codec::field{"correlation_id", &origin_dimension_event::correlation_id},
    };
}

/**
 * @brief The members of origin_dimension_version_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const origin_dimension_version_key&) {
    return std::tuple{
        utility::codec::field{"origin_dimension", &origin_dimension_version_key::origin_dimension},
        utility::codec::field{"version", &origin_dimension_version_key::version},
    };
}

/**
 * @brief The members of origin_dimension_versions_filter, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const origin_dimension_versions_filter&) {
    return std::tuple{
        utility::codec::field{"version", &origin_dimension_versions_filter::version},
        utility::codec::field{"from_version", &origin_dimension_versions_filter::from_version},
        utility::codec::field{"to_version", &origin_dimension_versions_filter::to_version},
    };
}

/**
 * @brief The members of list_origin_dimensions_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_origin_dimensions_request&) {
    return std::tuple{
        utility::codec::field{"offset", &list_origin_dimensions_request::offset},
        utility::codec::field{"limit", &list_origin_dimensions_request::limit},
        utility::codec::field{"order", &list_origin_dimensions_request::order},
        utility::codec::field{"filter", &list_origin_dimensions_request::filter},
        utility::codec::field{"as_of", &list_origin_dimensions_request::as_of},
    };
}

/**
 * @brief The members of list_origin_dimensions_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_origin_dimensions_response&) {
    return std::tuple{
        utility::codec::field{"result", &list_origin_dimensions_response::result},
        utility::codec::field{"dimensions", &list_origin_dimensions_response::dimensions},
        utility::codec::field{"total", &list_origin_dimensions_response::total},
    };
}

/**
 * @brief The members of get_origin_dimension_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_origin_dimension_request&) {
    return std::tuple{
        utility::codec::field{"key", &get_origin_dimension_request::key},
    };
}

/**
 * @brief The members of get_origin_dimension_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_origin_dimension_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_origin_dimension_response::result},
        utility::codec::field{"origin_dimension", &get_origin_dimension_response::origin_dimension},
    };
}

/**
 * @brief The members of get_many_origin_dimensions_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_many_origin_dimensions_request&) {
    return std::tuple{
        utility::codec::field{"keys", &get_many_origin_dimensions_request::keys},
    };
}

/**
 * @brief The members of get_many_origin_dimensions_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_many_origin_dimensions_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_many_origin_dimensions_response::result},
        utility::codec::field{"entries", &get_many_origin_dimensions_response::entries},
    };
}

/**
 * @brief The members of put_origin_dimension_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_origin_dimension_request&) {
    return std::tuple{
        utility::codec::field{"change", &put_origin_dimension_request::change},
        utility::codec::field{"intent", &put_origin_dimension_request::intent},
    };
}

/**
 * @brief The members of put_origin_dimension_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_origin_dimension_response&) {
    return std::tuple{
        utility::codec::field{"result", &put_origin_dimension_response::result},
        utility::codec::field{"origin_dimension", &put_origin_dimension_response::origin_dimension},
    };
}

/**
 * @brief The members of put_many_origin_dimensions_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_many_origin_dimensions_request&) {
    return std::tuple{
        utility::codec::field{"changes", &put_many_origin_dimensions_request::changes},
        utility::codec::field{"intent", &put_many_origin_dimensions_request::intent},
    };
}

/**
 * @brief The members of put_many_origin_dimensions_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_many_origin_dimensions_response&) {
    return std::tuple{
        utility::codec::field{"result", &put_many_origin_dimensions_response::result},
        utility::codec::field{"dimensions", &put_many_origin_dimensions_response::dimensions},
    };
}

/**
 * @brief The members of delete_origin_dimension_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_origin_dimension_request&) {
    return std::tuple{
        utility::codec::field{"removal", &delete_origin_dimension_request::removal},
        utility::codec::field{"intent", &delete_origin_dimension_request::intent},
    };
}

/**
 * @brief The members of delete_origin_dimension_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_origin_dimension_response&) {
    return std::tuple{
        utility::codec::field{"result", &delete_origin_dimension_response::result},
    };
}

/**
 * @brief The members of delete_many_origin_dimensions_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_many_origin_dimensions_request&) {
    return std::tuple{
        utility::codec::field{"removals", &delete_many_origin_dimensions_request::removals},
        utility::codec::field{"intent", &delete_many_origin_dimensions_request::intent},
    };
}

/**
 * @brief The members of delete_many_origin_dimensions_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_many_origin_dimensions_response&) {
    return std::tuple{
        utility::codec::field{"result", &delete_many_origin_dimensions_response::result},
    };
}

/**
 * @brief The members of list_origin_dimension_versions_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_origin_dimension_versions_request&) {
    return std::tuple{
        utility::codec::field{"key", &list_origin_dimension_versions_request::key},
        utility::codec::field{"offset", &list_origin_dimension_versions_request::offset},
        utility::codec::field{"limit", &list_origin_dimension_versions_request::limit},
        utility::codec::field{"order", &list_origin_dimension_versions_request::order},
        utility::codec::field{"filter", &list_origin_dimension_versions_request::filter},
    };
}

/**
 * @brief The members of list_origin_dimension_versions_response, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_origin_dimension_versions_response&) {
    return std::tuple{
        utility::codec::field{"result", &list_origin_dimension_versions_response::result},
        utility::codec::field{"versions", &list_origin_dimension_versions_response::versions},
        utility::codec::field{"total", &list_origin_dimension_versions_response::total},
    };
}

/**
 * @brief The members of get_origin_dimension_version_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_origin_dimension_version_request&) {
    return std::tuple{
        utility::codec::field{"key", &get_origin_dimension_version_request::key},
    };
}

/**
 * @brief The members of get_origin_dimension_version_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_origin_dimension_version_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_origin_dimension_version_response::result},
        utility::codec::field{"version", &get_origin_dimension_version_response::version},
    };
}

}

namespace ores::utility::codec {

template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::origin_dimension_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::origin_dimension_key& v);
    static ores::dq::messaging::origin_dimension_key read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::origin_dimension_key& v);
    static ores::dq::messaging::origin_dimension_key read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::origin_dimension_write> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::origin_dimension_write& v);
    static ores::dq::messaging::origin_dimension_write read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::origin_dimension_write& v);
    static ores::dq::messaging::origin_dimension_write
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::origin_dimension_change> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::origin_dimension_change& v);
    static ores::dq::messaging::origin_dimension_change read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::origin_dimension_change& v);
    static ores::dq::messaging::origin_dimension_change
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::origin_dimension_removal> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::origin_dimension_removal& v);
    static ores::dq::messaging::origin_dimension_removal read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::origin_dimension_removal& v);
    static ores::dq::messaging::origin_dimension_removal
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::origin_dimension_lookup> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::origin_dimension_lookup& v);
    static ores::dq::messaging::origin_dimension_lookup read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::origin_dimension_lookup& v);
    static ores::dq::messaging::origin_dimension_lookup
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::origin_dimensions_filter> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::origin_dimensions_filter& v);
    static ores::dq::messaging::origin_dimensions_filter read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::origin_dimensions_filter& v);
    static ores::dq::messaging::origin_dimensions_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::origin_dimension_event> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::origin_dimension_event& v);
    static ores::dq::messaging::origin_dimension_event read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::origin_dimension_event& v);
    static ores::dq::messaging::origin_dimension_event
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::origin_dimension_version_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::origin_dimension_version_key& v);
    static ores::dq::messaging::origin_dimension_version_key read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::origin_dimension_version_key& v);
    static ores::dq::messaging::origin_dimension_version_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::origin_dimension_versions_filter> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::origin_dimension_versions_filter& v);
    static ores::dq::messaging::origin_dimension_versions_filter read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::dq::messaging::origin_dimension_versions_filter& v);
    static ores::dq::messaging::origin_dimension_versions_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::list_origin_dimensions_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::list_origin_dimensions_request& v);
    static ores::dq::messaging::list_origin_dimensions_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::list_origin_dimensions_request& v);
    static ores::dq::messaging::list_origin_dimensions_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::list_origin_dimensions_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::list_origin_dimensions_response& v);
    static ores::dq::messaging::list_origin_dimensions_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::list_origin_dimensions_response& v);
    static ores::dq::messaging::list_origin_dimensions_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::get_origin_dimension_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::get_origin_dimension_request& v);
    static ores::dq::messaging::get_origin_dimension_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::get_origin_dimension_request& v);
    static ores::dq::messaging::get_origin_dimension_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::get_origin_dimension_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::get_origin_dimension_response& v);
    static ores::dq::messaging::get_origin_dimension_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::get_origin_dimension_response& v);
    static ores::dq::messaging::get_origin_dimension_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::get_many_origin_dimensions_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::get_many_origin_dimensions_request& v);
    static ores::dq::messaging::get_many_origin_dimensions_request read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::dq::messaging::get_many_origin_dimensions_request& v);
    static ores::dq::messaging::get_many_origin_dimensions_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::get_many_origin_dimensions_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::dq::messaging::get_many_origin_dimensions_response& v);
    static ores::dq::messaging::get_many_origin_dimensions_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::dq::messaging::get_many_origin_dimensions_response& v);
    static ores::dq::messaging::get_many_origin_dimensions_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::put_origin_dimension_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::put_origin_dimension_request& v);
    static ores::dq::messaging::put_origin_dimension_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::put_origin_dimension_request& v);
    static ores::dq::messaging::put_origin_dimension_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::put_origin_dimension_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::put_origin_dimension_response& v);
    static ores::dq::messaging::put_origin_dimension_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::put_origin_dimension_response& v);
    static ores::dq::messaging::put_origin_dimension_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::put_many_origin_dimensions_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::put_many_origin_dimensions_request& v);
    static ores::dq::messaging::put_many_origin_dimensions_request read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::dq::messaging::put_many_origin_dimensions_request& v);
    static ores::dq::messaging::put_many_origin_dimensions_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::put_many_origin_dimensions_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::dq::messaging::put_many_origin_dimensions_response& v);
    static ores::dq::messaging::put_many_origin_dimensions_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::dq::messaging::put_many_origin_dimensions_response& v);
    static ores::dq::messaging::put_many_origin_dimensions_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::delete_origin_dimension_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::delete_origin_dimension_request& v);
    static ores::dq::messaging::delete_origin_dimension_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::delete_origin_dimension_request& v);
    static ores::dq::messaging::delete_origin_dimension_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::delete_origin_dimension_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::delete_origin_dimension_response& v);
    static ores::dq::messaging::delete_origin_dimension_response read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::dq::messaging::delete_origin_dimension_response& v);
    static ores::dq::messaging::delete_origin_dimension_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::delete_many_origin_dimensions_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::dq::messaging::delete_many_origin_dimensions_request& v);
    static ores::dq::messaging::delete_many_origin_dimensions_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::dq::messaging::delete_many_origin_dimensions_request& v);
    static ores::dq::messaging::delete_many_origin_dimensions_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::delete_many_origin_dimensions_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::dq::messaging::delete_many_origin_dimensions_response& v);
    static ores::dq::messaging::delete_many_origin_dimensions_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::dq::messaging::delete_many_origin_dimensions_response& v);
    static ores::dq::messaging::delete_many_origin_dimensions_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::list_origin_dimension_versions_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::dq::messaging::list_origin_dimension_versions_request& v);
    static ores::dq::messaging::list_origin_dimension_versions_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::dq::messaging::list_origin_dimension_versions_request& v);
    static ores::dq::messaging::list_origin_dimension_versions_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT
    wire_traits<ores::dq::messaging::list_origin_dimension_versions_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::dq::messaging::list_origin_dimension_versions_response& v);
    static ores::dq::messaging::list_origin_dimension_versions_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::dq::messaging::list_origin_dimension_versions_response& v);
    static ores::dq::messaging::list_origin_dimension_versions_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::get_origin_dimension_version_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::dq::messaging::get_origin_dimension_version_request& v);
    static ores::dq::messaging::get_origin_dimension_version_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::dq::messaging::get_origin_dimension_version_request& v);
    static ores::dq::messaging::get_origin_dimension_version_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::get_origin_dimension_version_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::dq::messaging::get_origin_dimension_version_response& v);
    static ores::dq::messaging::get_origin_dimension_version_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::dq::messaging::get_origin_dimension_version_response& v);
    static ores::dq::messaging::get_origin_dimension_version_response
    read_msgpack(std::span<const std::byte> data);
};
}

#endif
