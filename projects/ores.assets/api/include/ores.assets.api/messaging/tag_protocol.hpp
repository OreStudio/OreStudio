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
#ifndef ORES_ASSETS_API_MESSAGING_TAG_PROTOCOL_HPP
#define ORES_ASSETS_API_MESSAGING_TAG_PROTOCOL_HPP

#include "ores.assets.api/domain/tag.hpp"
#include "ores.assets.api/export.hpp"
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

namespace ores::assets::messaging {

struct tag_key {
    std::string name;
};

struct tag_write {
    boost::uuids::uuid id;
    std::string name;
    std::string description;
};

struct tag_change {
    tag_write write;
    ores::utility::domain::precondition precondition;
};

struct tag_removal {
    tag_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;
};

struct tag_lookup {
    tag_key key;
    std::optional<ores::assets::domain::tag> tag;
};

struct tags_filter {
    std::optional<std::vector<boost::uuids::uuid>> id_one_of;
};

struct tag_event {
    boost::uuids::uuid event_id;
    tag_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;
};

struct tag_version_key {
    tag_key tag;
    std::uint32_t version;
};

struct tag_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;
};

struct list_tags_request {
    using response_type = struct list_tags_response;
    static constexpr std::string_view nats_subject = "assets.v1.tags.list";
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
    std::optional<tags_filter> filter;
    std::optional<std::string> as_of;
};

struct list_tags_response {
    ores::utility::domain::result result;
    std::vector<ores::assets::domain::tag> tags;
    std::uint64_t total;
};

struct get_tag_request {
    using response_type = struct get_tag_response;
    static constexpr std::string_view nats_subject = "assets.v1.tags.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    tag_key key;
};

struct get_tag_response {
    ores::utility::domain::result result;
    std::optional<ores::assets::domain::tag> tag;
};

struct get_many_tags_request {
    using response_type = struct get_many_tags_response;
    static constexpr std::string_view nats_subject = "assets.v1.tags.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<tag_key> keys;
};

struct get_many_tags_response {
    ores::utility::domain::result result;
    std::vector<tag_lookup> entries;
};

struct put_tag_request {
    using response_type = struct put_tag_response;
    static constexpr std::string_view nats_subject = "assets.v1.tags.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    tag_change change;
    ores::utility::domain::change_intent intent;
};

struct put_tag_response {
    ores::utility::domain::result result;
    std::optional<ores::assets::domain::tag> tag;
};

struct put_many_tags_request {
    using response_type = struct put_many_tags_response;
    static constexpr std::string_view nats_subject = "assets.v1.tags.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<tag_change> changes;
    ores::utility::domain::change_intent intent;
};

struct put_many_tags_response {
    ores::utility::domain::result result;
    std::vector<ores::assets::domain::tag> tags;
};

struct delete_tag_request {
    using response_type = struct delete_tag_response;
    static constexpr std::string_view nats_subject = "assets.v1.tags.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    tag_removal removal;
    ores::utility::domain::change_intent intent;
};

struct delete_tag_response {
    ores::utility::domain::result result;
};

struct delete_many_tags_request {
    using response_type = struct delete_many_tags_response;
    static constexpr std::string_view nats_subject = "assets.v1.tags.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<tag_removal> removals;
    ores::utility::domain::change_intent intent;
};

struct delete_many_tags_response {
    ores::utility::domain::result result;
};

struct list_tag_versions_request {
    using response_type = struct list_tag_versions_response;
    static constexpr std::string_view nats_subject = "assets.v1.tags_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    tag_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<tag_versions_filter> filter;
};

struct list_tag_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::assets::domain::tag> versions;
    std::uint64_t total;
};

struct get_tag_version_request {
    using response_type = struct get_tag_version_response;
    static constexpr std::string_view nats_subject = "assets.v1.tags_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    tag_version_key key;
};

struct get_tag_version_response {
    ores::utility::domain::result result;
    std::optional<ores::assets::domain::tag> version;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace tag_event_subjects {
inline constexpr std::string_view created = "assets.v1.tags_events.created";
inline constexpr std::string_view updated = "assets.v1.tags_events.updated";
inline constexpr std::string_view deleted = "assets.v1.tags_events.deleted";
}
/**
 * @brief The members of tag_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const tag_key&) {
    return std::tuple{
        utility::codec::field{"name", &tag_key::name},
    };
}

/**
 * @brief The members of tag_write, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const tag_write&) {
    return std::tuple{
        utility::codec::field{"id", &tag_write::id},
        utility::codec::field{"name", &tag_write::name},
        utility::codec::field{"description", &tag_write::description},
    };
}

/**
 * @brief The members of tag_change, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const tag_change&) {
    return std::tuple{
        utility::codec::field{"write", &tag_change::write},
        utility::codec::field{"precondition", &tag_change::precondition},
    };
}

/**
 * @brief The members of tag_removal, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const tag_removal&) {
    return std::tuple{
        utility::codec::field{"key", &tag_removal::key},
        utility::codec::field{"precondition", &tag_removal::precondition},
    };
}

/**
 * @brief The members of tag_lookup, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const tag_lookup&) {
    return std::tuple{
        utility::codec::field{"key", &tag_lookup::key},
        utility::codec::field{"tag", &tag_lookup::tag},
    };
}

/**
 * @brief The members of tags_filter, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const tags_filter&) {
    return std::tuple{
        utility::codec::field{"id_one_of", &tags_filter::id_one_of},
    };
}

/**
 * @brief The members of tag_event, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const tag_event&) {
    return std::tuple{
        utility::codec::field{"event_id", &tag_event::event_id},
        utility::codec::field{"key", &tag_event::key},
        utility::codec::field{"action", &tag_event::action},
        utility::codec::field{"version", &tag_event::version},
        utility::codec::field{"occurred_at", &tag_event::occurred_at},
        utility::codec::field{"correlation_id", &tag_event::correlation_id},
    };
}

/**
 * @brief The members of tag_version_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const tag_version_key&) {
    return std::tuple{
        utility::codec::field{"tag", &tag_version_key::tag},
        utility::codec::field{"version", &tag_version_key::version},
    };
}

/**
 * @brief The members of tag_versions_filter, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const tag_versions_filter&) {
    return std::tuple{
        utility::codec::field{"version", &tag_versions_filter::version},
        utility::codec::field{"from_version", &tag_versions_filter::from_version},
        utility::codec::field{"to_version", &tag_versions_filter::to_version},
    };
}

/**
 * @brief The members of list_tags_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_tags_request&) {
    return std::tuple{
        utility::codec::field{"offset", &list_tags_request::offset},
        utility::codec::field{"limit", &list_tags_request::limit},
        utility::codec::field{"order", &list_tags_request::order},
        utility::codec::field{"filter", &list_tags_request::filter},
        utility::codec::field{"as_of", &list_tags_request::as_of},
    };
}

/**
 * @brief The members of list_tags_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_tags_response&) {
    return std::tuple{
        utility::codec::field{"result", &list_tags_response::result},
        utility::codec::field{"tags", &list_tags_response::tags},
        utility::codec::field{"total", &list_tags_response::total},
    };
}

/**
 * @brief The members of get_tag_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_tag_request&) {
    return std::tuple{
        utility::codec::field{"key", &get_tag_request::key},
    };
}

/**
 * @brief The members of get_tag_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_tag_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_tag_response::result},
        utility::codec::field{"tag", &get_tag_response::tag},
    };
}

/**
 * @brief The members of get_many_tags_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_many_tags_request&) {
    return std::tuple{
        utility::codec::field{"keys", &get_many_tags_request::keys},
    };
}

/**
 * @brief The members of get_many_tags_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_many_tags_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_many_tags_response::result},
        utility::codec::field{"entries", &get_many_tags_response::entries},
    };
}

/**
 * @brief The members of put_tag_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_tag_request&) {
    return std::tuple{
        utility::codec::field{"change", &put_tag_request::change},
        utility::codec::field{"intent", &put_tag_request::intent},
    };
}

/**
 * @brief The members of put_tag_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_tag_response&) {
    return std::tuple{
        utility::codec::field{"result", &put_tag_response::result},
        utility::codec::field{"tag", &put_tag_response::tag},
    };
}

/**
 * @brief The members of put_many_tags_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_many_tags_request&) {
    return std::tuple{
        utility::codec::field{"changes", &put_many_tags_request::changes},
        utility::codec::field{"intent", &put_many_tags_request::intent},
    };
}

/**
 * @brief The members of put_many_tags_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_many_tags_response&) {
    return std::tuple{
        utility::codec::field{"result", &put_many_tags_response::result},
        utility::codec::field{"tags", &put_many_tags_response::tags},
    };
}

/**
 * @brief The members of delete_tag_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_tag_request&) {
    return std::tuple{
        utility::codec::field{"removal", &delete_tag_request::removal},
        utility::codec::field{"intent", &delete_tag_request::intent},
    };
}

/**
 * @brief The members of delete_tag_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_tag_response&) {
    return std::tuple{
        utility::codec::field{"result", &delete_tag_response::result},
    };
}

/**
 * @brief The members of delete_many_tags_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_many_tags_request&) {
    return std::tuple{
        utility::codec::field{"removals", &delete_many_tags_request::removals},
        utility::codec::field{"intent", &delete_many_tags_request::intent},
    };
}

/**
 * @brief The members of delete_many_tags_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_many_tags_response&) {
    return std::tuple{
        utility::codec::field{"result", &delete_many_tags_response::result},
    };
}

/**
 * @brief The members of list_tag_versions_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_tag_versions_request&) {
    return std::tuple{
        utility::codec::field{"key", &list_tag_versions_request::key},
        utility::codec::field{"offset", &list_tag_versions_request::offset},
        utility::codec::field{"limit", &list_tag_versions_request::limit},
        utility::codec::field{"order", &list_tag_versions_request::order},
        utility::codec::field{"filter", &list_tag_versions_request::filter},
    };
}

/**
 * @brief The members of list_tag_versions_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_tag_versions_response&) {
    return std::tuple{
        utility::codec::field{"result", &list_tag_versions_response::result},
        utility::codec::field{"versions", &list_tag_versions_response::versions},
        utility::codec::field{"total", &list_tag_versions_response::total},
    };
}

/**
 * @brief The members of get_tag_version_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_tag_version_request&) {
    return std::tuple{
        utility::codec::field{"key", &get_tag_version_request::key},
    };
}

/**
 * @brief The members of get_tag_version_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_tag_version_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_tag_version_response::result},
        utility::codec::field{"version", &get_tag_version_response::version},
    };
}


}
namespace ores::utility::codec {

template <>
struct ORES_ASSETS_API_EXPORT wire_traits<ores::assets::messaging::tag_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::assets::messaging::tag_key& v);
    static ores::assets::messaging::tag_key read_json(std::string_view text);
    static std::string write_msgpack(const ores::assets::messaging::tag_key& v);
    static ores::assets::messaging::tag_key read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ASSETS_API_EXPORT wire_traits<ores::assets::messaging::tag_write> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::assets::messaging::tag_write& v);
    static ores::assets::messaging::tag_write read_json(std::string_view text);
    static std::string write_msgpack(const ores::assets::messaging::tag_write& v);
    static ores::assets::messaging::tag_write read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ASSETS_API_EXPORT wire_traits<ores::assets::messaging::tag_change> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::assets::messaging::tag_change& v);
    static ores::assets::messaging::tag_change read_json(std::string_view text);
    static std::string write_msgpack(const ores::assets::messaging::tag_change& v);
    static ores::assets::messaging::tag_change read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ASSETS_API_EXPORT wire_traits<ores::assets::messaging::tag_removal> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::assets::messaging::tag_removal& v);
    static ores::assets::messaging::tag_removal read_json(std::string_view text);
    static std::string write_msgpack(const ores::assets::messaging::tag_removal& v);
    static ores::assets::messaging::tag_removal read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ASSETS_API_EXPORT wire_traits<ores::assets::messaging::tag_lookup> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::assets::messaging::tag_lookup& v);
    static ores::assets::messaging::tag_lookup read_json(std::string_view text);
    static std::string write_msgpack(const ores::assets::messaging::tag_lookup& v);
    static ores::assets::messaging::tag_lookup read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ASSETS_API_EXPORT wire_traits<ores::assets::messaging::tags_filter> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::assets::messaging::tags_filter& v);
    static ores::assets::messaging::tags_filter read_json(std::string_view text);
    static std::string write_msgpack(const ores::assets::messaging::tags_filter& v);
    static ores::assets::messaging::tags_filter read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ASSETS_API_EXPORT wire_traits<ores::assets::messaging::tag_event> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::assets::messaging::tag_event& v);
    static ores::assets::messaging::tag_event read_json(std::string_view text);
    static std::string write_msgpack(const ores::assets::messaging::tag_event& v);
    static ores::assets::messaging::tag_event read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ASSETS_API_EXPORT wire_traits<ores::assets::messaging::tag_version_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::assets::messaging::tag_version_key& v);
    static ores::assets::messaging::tag_version_key read_json(std::string_view text);
    static std::string write_msgpack(const ores::assets::messaging::tag_version_key& v);
    static ores::assets::messaging::tag_version_key read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ASSETS_API_EXPORT wire_traits<ores::assets::messaging::tag_versions_filter> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::assets::messaging::tag_versions_filter& v);
    static ores::assets::messaging::tag_versions_filter read_json(std::string_view text);
    static std::string write_msgpack(const ores::assets::messaging::tag_versions_filter& v);
    static ores::assets::messaging::tag_versions_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ASSETS_API_EXPORT wire_traits<ores::assets::messaging::list_tags_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::assets::messaging::list_tags_request& v);
    static ores::assets::messaging::list_tags_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::assets::messaging::list_tags_request& v);
    static ores::assets::messaging::list_tags_request read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ASSETS_API_EXPORT wire_traits<ores::assets::messaging::list_tags_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::assets::messaging::list_tags_response& v);
    static ores::assets::messaging::list_tags_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::assets::messaging::list_tags_response& v);
    static ores::assets::messaging::list_tags_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ASSETS_API_EXPORT wire_traits<ores::assets::messaging::get_tag_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::assets::messaging::get_tag_request& v);
    static ores::assets::messaging::get_tag_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::assets::messaging::get_tag_request& v);
    static ores::assets::messaging::get_tag_request read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ASSETS_API_EXPORT wire_traits<ores::assets::messaging::get_tag_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::assets::messaging::get_tag_response& v);
    static ores::assets::messaging::get_tag_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::assets::messaging::get_tag_response& v);
    static ores::assets::messaging::get_tag_response read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ASSETS_API_EXPORT wire_traits<ores::assets::messaging::get_many_tags_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::assets::messaging::get_many_tags_request& v);
    static ores::assets::messaging::get_many_tags_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::assets::messaging::get_many_tags_request& v);
    static ores::assets::messaging::get_many_tags_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ASSETS_API_EXPORT wire_traits<ores::assets::messaging::get_many_tags_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::assets::messaging::get_many_tags_response& v);
    static ores::assets::messaging::get_many_tags_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::assets::messaging::get_many_tags_response& v);
    static ores::assets::messaging::get_many_tags_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ASSETS_API_EXPORT wire_traits<ores::assets::messaging::put_tag_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::assets::messaging::put_tag_request& v);
    static ores::assets::messaging::put_tag_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::assets::messaging::put_tag_request& v);
    static ores::assets::messaging::put_tag_request read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ASSETS_API_EXPORT wire_traits<ores::assets::messaging::put_tag_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::assets::messaging::put_tag_response& v);
    static ores::assets::messaging::put_tag_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::assets::messaging::put_tag_response& v);
    static ores::assets::messaging::put_tag_response read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ASSETS_API_EXPORT wire_traits<ores::assets::messaging::put_many_tags_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::assets::messaging::put_many_tags_request& v);
    static ores::assets::messaging::put_many_tags_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::assets::messaging::put_many_tags_request& v);
    static ores::assets::messaging::put_many_tags_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ASSETS_API_EXPORT wire_traits<ores::assets::messaging::put_many_tags_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::assets::messaging::put_many_tags_response& v);
    static ores::assets::messaging::put_many_tags_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::assets::messaging::put_many_tags_response& v);
    static ores::assets::messaging::put_many_tags_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ASSETS_API_EXPORT wire_traits<ores::assets::messaging::delete_tag_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::assets::messaging::delete_tag_request& v);
    static ores::assets::messaging::delete_tag_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::assets::messaging::delete_tag_request& v);
    static ores::assets::messaging::delete_tag_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ASSETS_API_EXPORT wire_traits<ores::assets::messaging::delete_tag_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::assets::messaging::delete_tag_response& v);
    static ores::assets::messaging::delete_tag_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::assets::messaging::delete_tag_response& v);
    static ores::assets::messaging::delete_tag_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ASSETS_API_EXPORT wire_traits<ores::assets::messaging::delete_many_tags_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::assets::messaging::delete_many_tags_request& v);
    static ores::assets::messaging::delete_many_tags_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::assets::messaging::delete_many_tags_request& v);
    static ores::assets::messaging::delete_many_tags_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ASSETS_API_EXPORT wire_traits<ores::assets::messaging::delete_many_tags_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::assets::messaging::delete_many_tags_response& v);
    static ores::assets::messaging::delete_many_tags_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::assets::messaging::delete_many_tags_response& v);
    static ores::assets::messaging::delete_many_tags_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ASSETS_API_EXPORT wire_traits<ores::assets::messaging::list_tag_versions_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::assets::messaging::list_tag_versions_request& v);
    static ores::assets::messaging::list_tag_versions_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::assets::messaging::list_tag_versions_request& v);
    static ores::assets::messaging::list_tag_versions_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ASSETS_API_EXPORT wire_traits<ores::assets::messaging::list_tag_versions_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::assets::messaging::list_tag_versions_response& v);
    static ores::assets::messaging::list_tag_versions_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::assets::messaging::list_tag_versions_response& v);
    static ores::assets::messaging::list_tag_versions_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ASSETS_API_EXPORT wire_traits<ores::assets::messaging::get_tag_version_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::assets::messaging::get_tag_version_request& v);
    static ores::assets::messaging::get_tag_version_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::assets::messaging::get_tag_version_request& v);
    static ores::assets::messaging::get_tag_version_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ASSETS_API_EXPORT wire_traits<ores::assets::messaging::get_tag_version_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::assets::messaging::get_tag_version_response& v);
    static ores::assets::messaging::get_tag_version_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::assets::messaging::get_tag_version_response& v);
    static ores::assets::messaging::get_tag_version_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
