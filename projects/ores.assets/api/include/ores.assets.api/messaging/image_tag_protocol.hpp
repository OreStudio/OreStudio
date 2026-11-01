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
#ifndef ORES_ASSETS_API_MESSAGING_IMAGE_TAG_PROTOCOL_HPP
#define ORES_ASSETS_API_MESSAGING_IMAGE_TAG_PROTOCOL_HPP

#include "ores.assets.api/domain/image_tag.hpp"
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

struct image_tag_key {
    boost::uuids::uuid image_id;
    boost::uuids::uuid tag_id;
};

struct image_tag_write {
    boost::uuids::uuid image_id;
    boost::uuids::uuid tag_id;
    std::string assigned_by;
    std::chrono::system_clock::time_point assigned_at;
};

struct image_tag_change {
    image_tag_write write;
    ores::utility::domain::precondition precondition;
};

struct image_tag_removal {
    image_tag_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;
};

struct image_tag_lookup {
    image_tag_key key;
    std::optional<ores::assets::domain::image_tag> image_tag;
};

struct image_tags_filter {
    std::optional<boost::uuids::uuid> image_id;
    std::optional<std::vector<boost::uuids::uuid>> image_id_one_of;
};

struct list_image_tags_request {
    using response_type = struct list_image_tags_response;
    static constexpr std::string_view nats_subject = "assets.v1.image_tags.list";
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
    std::optional<image_tags_filter> filter;
};

struct list_image_tags_response {
    ores::utility::domain::result result;
    std::vector<ores::assets::domain::image_tag> image_tags;
    std::uint64_t total;
};

struct get_image_tag_request {
    using response_type = struct get_image_tag_response;
    static constexpr std::string_view nats_subject = "assets.v1.image_tags.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    image_tag_key key;
};

struct get_image_tag_response {
    ores::utility::domain::result result;
    std::optional<ores::assets::domain::image_tag> image_tag;
};

struct get_many_image_tags_request {
    using response_type = struct get_many_image_tags_response;
    static constexpr std::string_view nats_subject = "assets.v1.image_tags.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<image_tag_key> keys;
};

struct get_many_image_tags_response {
    ores::utility::domain::result result;
    std::vector<image_tag_lookup> entries;
};

struct put_image_tag_request {
    using response_type = struct put_image_tag_response;
    static constexpr std::string_view nats_subject = "assets.v1.image_tags.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    image_tag_change change;
    ores::utility::domain::change_intent intent;
};

struct put_image_tag_response {
    ores::utility::domain::result result;
    std::optional<ores::assets::domain::image_tag> image_tag;
};

struct put_many_image_tags_request {
    using response_type = struct put_many_image_tags_response;
    static constexpr std::string_view nats_subject = "assets.v1.image_tags.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<image_tag_change> changes;
    ores::utility::domain::change_intent intent;
};

struct put_many_image_tags_response {
    ores::utility::domain::result result;
    std::vector<ores::assets::domain::image_tag> image_tags;
};

struct delete_image_tag_request {
    using response_type = struct delete_image_tag_response;
    static constexpr std::string_view nats_subject = "assets.v1.image_tags.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    image_tag_removal removal;
    ores::utility::domain::change_intent intent;
};

struct delete_image_tag_response {
    ores::utility::domain::result result;
};

struct delete_many_image_tags_request {
    using response_type = struct delete_many_image_tags_response;
    static constexpr std::string_view nats_subject = "assets.v1.image_tags.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<image_tag_removal> removals;
    ores::utility::domain::change_intent intent;
};

struct delete_many_image_tags_response {
    ores::utility::domain::result result;
};

struct list_by_image_id_image_tags_request {
    using response_type = struct list_by_image_id_image_tags_response;
    static constexpr std::string_view nats_subject = "assets.v1.image_tags.list_by_image_id";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    boost::uuids::uuid image_id;
    ores::utility::domain::scope scope = ores::utility::domain::scope::direct;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<image_tags_filter> filter;
};

struct list_by_image_id_image_tags_response {
    ores::utility::domain::result result;
    std::vector<ores::assets::domain::image_tag> image_tags;
    std::uint64_t total;
};

/**
 * @brief The members of image_tag_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const image_tag_key&) {
    return std::tuple{
        utility::codec::field{"image_id", &image_tag_key::image_id},
        utility::codec::field{"tag_id", &image_tag_key::tag_id},
    };
}

/**
 * @brief The members of image_tag_write, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const image_tag_write&) {
    return std::tuple{
        utility::codec::field{"image_id", &image_tag_write::image_id},
        utility::codec::field{"tag_id", &image_tag_write::tag_id},
        utility::codec::field{"assigned_by", &image_tag_write::assigned_by},
        utility::codec::field{"assigned_at", &image_tag_write::assigned_at},
    };
}

/**
 * @brief The members of image_tag_change, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const image_tag_change&) {
    return std::tuple{
        utility::codec::field{"write", &image_tag_change::write},
        utility::codec::field{"precondition", &image_tag_change::precondition},
    };
}

/**
 * @brief The members of image_tag_removal, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const image_tag_removal&) {
    return std::tuple{
        utility::codec::field{"key", &image_tag_removal::key},
        utility::codec::field{"precondition", &image_tag_removal::precondition},
    };
}

/**
 * @brief The members of image_tag_lookup, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const image_tag_lookup&) {
    return std::tuple{
        utility::codec::field{"key", &image_tag_lookup::key},
        utility::codec::field{"image_tag", &image_tag_lookup::image_tag},
    };
}

/**
 * @brief The members of image_tags_filter, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const image_tags_filter&) {
    return std::tuple{
        utility::codec::field{"image_id", &image_tags_filter::image_id},
        utility::codec::field{"image_id_one_of", &image_tags_filter::image_id_one_of},
    };
}

/**
 * @brief The members of list_image_tags_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_image_tags_request&) {
    return std::tuple{
        utility::codec::field{"offset", &list_image_tags_request::offset},
        utility::codec::field{"limit", &list_image_tags_request::limit},
        utility::codec::field{"order", &list_image_tags_request::order},
        utility::codec::field{"filter", &list_image_tags_request::filter},
    };
}

/**
 * @brief The members of list_image_tags_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_image_tags_response&) {
    return std::tuple{
        utility::codec::field{"result", &list_image_tags_response::result},
        utility::codec::field{"image_tags", &list_image_tags_response::image_tags},
        utility::codec::field{"total", &list_image_tags_response::total},
    };
}

/**
 * @brief The members of get_image_tag_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_image_tag_request&) {
    return std::tuple{
        utility::codec::field{"key", &get_image_tag_request::key},
    };
}

/**
 * @brief The members of get_image_tag_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_image_tag_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_image_tag_response::result},
        utility::codec::field{"image_tag", &get_image_tag_response::image_tag},
    };
}

/**
 * @brief The members of get_many_image_tags_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_many_image_tags_request&) {
    return std::tuple{
        utility::codec::field{"keys", &get_many_image_tags_request::keys},
    };
}

/**
 * @brief The members of get_many_image_tags_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_many_image_tags_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_many_image_tags_response::result},
        utility::codec::field{"entries", &get_many_image_tags_response::entries},
    };
}

/**
 * @brief The members of put_image_tag_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_image_tag_request&) {
    return std::tuple{
        utility::codec::field{"change", &put_image_tag_request::change},
        utility::codec::field{"intent", &put_image_tag_request::intent},
    };
}

/**
 * @brief The members of put_image_tag_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_image_tag_response&) {
    return std::tuple{
        utility::codec::field{"result", &put_image_tag_response::result},
        utility::codec::field{"image_tag", &put_image_tag_response::image_tag},
    };
}

/**
 * @brief The members of put_many_image_tags_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_many_image_tags_request&) {
    return std::tuple{
        utility::codec::field{"changes", &put_many_image_tags_request::changes},
        utility::codec::field{"intent", &put_many_image_tags_request::intent},
    };
}

/**
 * @brief The members of put_many_image_tags_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_many_image_tags_response&) {
    return std::tuple{
        utility::codec::field{"result", &put_many_image_tags_response::result},
        utility::codec::field{"image_tags", &put_many_image_tags_response::image_tags},
    };
}

/**
 * @brief The members of delete_image_tag_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_image_tag_request&) {
    return std::tuple{
        utility::codec::field{"removal", &delete_image_tag_request::removal},
        utility::codec::field{"intent", &delete_image_tag_request::intent},
    };
}

/**
 * @brief The members of delete_image_tag_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_image_tag_response&) {
    return std::tuple{
        utility::codec::field{"result", &delete_image_tag_response::result},
    };
}

/**
 * @brief The members of delete_many_image_tags_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_many_image_tags_request&) {
    return std::tuple{
        utility::codec::field{"removals", &delete_many_image_tags_request::removals},
        utility::codec::field{"intent", &delete_many_image_tags_request::intent},
    };
}

/**
 * @brief The members of delete_many_image_tags_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_many_image_tags_response&) {
    return std::tuple{
        utility::codec::field{"result", &delete_many_image_tags_response::result},
    };
}

/**
 * @brief The members of list_by_image_id_image_tags_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_by_image_id_image_tags_request&) {
    return std::tuple{
        utility::codec::field{"image_id", &list_by_image_id_image_tags_request::image_id},
        utility::codec::field{"scope", &list_by_image_id_image_tags_request::scope},
        utility::codec::field{"offset", &list_by_image_id_image_tags_request::offset},
        utility::codec::field{"limit", &list_by_image_id_image_tags_request::limit},
        utility::codec::field{"order", &list_by_image_id_image_tags_request::order},
        utility::codec::field{"filter", &list_by_image_id_image_tags_request::filter},
    };
}

/**
 * @brief The members of list_by_image_id_image_tags_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_by_image_id_image_tags_response&) {
    return std::tuple{
        utility::codec::field{"result", &list_by_image_id_image_tags_response::result},
        utility::codec::field{"image_tags", &list_by_image_id_image_tags_response::image_tags},
        utility::codec::field{"total", &list_by_image_id_image_tags_response::total},
    };
}

}
namespace ores::utility::codec {

template <>
struct ORES_ASSETS_API_EXPORT wire_traits<ores::assets::messaging::image_tag_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::assets::messaging::image_tag_key& v);
    static ores::assets::messaging::image_tag_key read_json(std::string_view text);
    static std::string write_msgpack(const ores::assets::messaging::image_tag_key& v);
    static ores::assets::messaging::image_tag_key read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ASSETS_API_EXPORT wire_traits<ores::assets::messaging::image_tag_write> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::assets::messaging::image_tag_write& v);
    static ores::assets::messaging::image_tag_write read_json(std::string_view text);
    static std::string write_msgpack(const ores::assets::messaging::image_tag_write& v);
    static ores::assets::messaging::image_tag_write read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ASSETS_API_EXPORT wire_traits<ores::assets::messaging::image_tag_change> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::assets::messaging::image_tag_change& v);
    static ores::assets::messaging::image_tag_change read_json(std::string_view text);
    static std::string write_msgpack(const ores::assets::messaging::image_tag_change& v);
    static ores::assets::messaging::image_tag_change read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ASSETS_API_EXPORT wire_traits<ores::assets::messaging::image_tag_removal> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::assets::messaging::image_tag_removal& v);
    static ores::assets::messaging::image_tag_removal read_json(std::string_view text);
    static std::string write_msgpack(const ores::assets::messaging::image_tag_removal& v);
    static ores::assets::messaging::image_tag_removal read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ASSETS_API_EXPORT wire_traits<ores::assets::messaging::image_tag_lookup> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::assets::messaging::image_tag_lookup& v);
    static ores::assets::messaging::image_tag_lookup read_json(std::string_view text);
    static std::string write_msgpack(const ores::assets::messaging::image_tag_lookup& v);
    static ores::assets::messaging::image_tag_lookup read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ASSETS_API_EXPORT wire_traits<ores::assets::messaging::image_tags_filter> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::assets::messaging::image_tags_filter& v);
    static ores::assets::messaging::image_tags_filter read_json(std::string_view text);
    static std::string write_msgpack(const ores::assets::messaging::image_tags_filter& v);
    static ores::assets::messaging::image_tags_filter read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ASSETS_API_EXPORT wire_traits<ores::assets::messaging::list_image_tags_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::assets::messaging::list_image_tags_request& v);
    static ores::assets::messaging::list_image_tags_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::assets::messaging::list_image_tags_request& v);
    static ores::assets::messaging::list_image_tags_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ASSETS_API_EXPORT wire_traits<ores::assets::messaging::list_image_tags_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::assets::messaging::list_image_tags_response& v);
    static ores::assets::messaging::list_image_tags_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::assets::messaging::list_image_tags_response& v);
    static ores::assets::messaging::list_image_tags_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ASSETS_API_EXPORT wire_traits<ores::assets::messaging::get_image_tag_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::assets::messaging::get_image_tag_request& v);
    static ores::assets::messaging::get_image_tag_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::assets::messaging::get_image_tag_request& v);
    static ores::assets::messaging::get_image_tag_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ASSETS_API_EXPORT wire_traits<ores::assets::messaging::get_image_tag_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::assets::messaging::get_image_tag_response& v);
    static ores::assets::messaging::get_image_tag_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::assets::messaging::get_image_tag_response& v);
    static ores::assets::messaging::get_image_tag_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ASSETS_API_EXPORT wire_traits<ores::assets::messaging::get_many_image_tags_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::assets::messaging::get_many_image_tags_request& v);
    static ores::assets::messaging::get_many_image_tags_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::assets::messaging::get_many_image_tags_request& v);
    static ores::assets::messaging::get_many_image_tags_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ASSETS_API_EXPORT wire_traits<ores::assets::messaging::get_many_image_tags_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::assets::messaging::get_many_image_tags_response& v);
    static ores::assets::messaging::get_many_image_tags_response read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::assets::messaging::get_many_image_tags_response& v);
    static ores::assets::messaging::get_many_image_tags_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ASSETS_API_EXPORT wire_traits<ores::assets::messaging::put_image_tag_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::assets::messaging::put_image_tag_request& v);
    static ores::assets::messaging::put_image_tag_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::assets::messaging::put_image_tag_request& v);
    static ores::assets::messaging::put_image_tag_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ASSETS_API_EXPORT wire_traits<ores::assets::messaging::put_image_tag_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::assets::messaging::put_image_tag_response& v);
    static ores::assets::messaging::put_image_tag_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::assets::messaging::put_image_tag_response& v);
    static ores::assets::messaging::put_image_tag_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ASSETS_API_EXPORT wire_traits<ores::assets::messaging::put_many_image_tags_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::assets::messaging::put_many_image_tags_request& v);
    static ores::assets::messaging::put_many_image_tags_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::assets::messaging::put_many_image_tags_request& v);
    static ores::assets::messaging::put_many_image_tags_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ASSETS_API_EXPORT wire_traits<ores::assets::messaging::put_many_image_tags_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::assets::messaging::put_many_image_tags_response& v);
    static ores::assets::messaging::put_many_image_tags_response read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::assets::messaging::put_many_image_tags_response& v);
    static ores::assets::messaging::put_many_image_tags_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ASSETS_API_EXPORT wire_traits<ores::assets::messaging::delete_image_tag_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::assets::messaging::delete_image_tag_request& v);
    static ores::assets::messaging::delete_image_tag_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::assets::messaging::delete_image_tag_request& v);
    static ores::assets::messaging::delete_image_tag_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ASSETS_API_EXPORT wire_traits<ores::assets::messaging::delete_image_tag_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::assets::messaging::delete_image_tag_response& v);
    static ores::assets::messaging::delete_image_tag_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::assets::messaging::delete_image_tag_response& v);
    static ores::assets::messaging::delete_image_tag_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ASSETS_API_EXPORT wire_traits<ores::assets::messaging::delete_many_image_tags_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::assets::messaging::delete_many_image_tags_request& v);
    static ores::assets::messaging::delete_many_image_tags_request read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::assets::messaging::delete_many_image_tags_request& v);
    static ores::assets::messaging::delete_many_image_tags_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ASSETS_API_EXPORT
    wire_traits<ores::assets::messaging::delete_many_image_tags_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::assets::messaging::delete_many_image_tags_response& v);
    static ores::assets::messaging::delete_many_image_tags_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::assets::messaging::delete_many_image_tags_response& v);
    static ores::assets::messaging::delete_many_image_tags_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ASSETS_API_EXPORT
    wire_traits<ores::assets::messaging::list_by_image_id_image_tags_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::assets::messaging::list_by_image_id_image_tags_request& v);
    static ores::assets::messaging::list_by_image_id_image_tags_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::assets::messaging::list_by_image_id_image_tags_request& v);
    static ores::assets::messaging::list_by_image_id_image_tags_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ASSETS_API_EXPORT
    wire_traits<ores::assets::messaging::list_by_image_id_image_tags_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::assets::messaging::list_by_image_id_image_tags_response& v);
    static ores::assets::messaging::list_by_image_id_image_tags_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::assets::messaging::list_by_image_id_image_tags_response& v);
    static ores::assets::messaging::list_by_image_id_image_tags_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
