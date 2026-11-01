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
#ifndef ORES_ASSETS_API_MESSAGING_IMAGE_PROTOCOL_HPP
#define ORES_ASSETS_API_MESSAGING_IMAGE_PROTOCOL_HPP

#include "ores.assets.api/domain/image.hpp"
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

struct image_key {
    std::string code;
};

struct image_write {
    boost::uuids::uuid id;
    std::string code;
    std::string description;
    std::string mime_type;
    std::vector<std::uint8_t> data;
};

struct image_change {
    image_write write;
    ores::utility::domain::precondition precondition;
};

struct image_removal {
    image_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;
};

struct image_lookup {
    image_key key;
    std::optional<ores::assets::domain::image> image;
};

struct images_filter {
    std::optional<std::vector<boost::uuids::uuid>> id_one_of;
    std::optional<std::string> search;
};

struct image_event {
    boost::uuids::uuid event_id;
    image_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;
};

struct image_version_key {
    image_key image;
    std::uint32_t version;
};

struct image_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;
};

struct list_images_request {
    using response_type = struct list_images_response;
    static constexpr std::string_view nats_subject = "assets.v1.images.list";
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
    std::optional<images_filter> filter;
    std::optional<std::string> as_of;
};

struct list_images_response {
    ores::utility::domain::result result;
    std::vector<ores::assets::domain::image> images;
    std::uint64_t total;
};

struct get_image_request {
    using response_type = struct get_image_response;
    static constexpr std::string_view nats_subject = "assets.v1.images.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    image_key key;
};

struct get_image_response {
    ores::utility::domain::result result;
    std::optional<ores::assets::domain::image> image;
};

struct get_many_images_request {
    using response_type = struct get_many_images_response;
    static constexpr std::string_view nats_subject = "assets.v1.images.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<image_key> keys;
};

struct get_many_images_response {
    ores::utility::domain::result result;
    std::vector<image_lookup> entries;
};

struct put_image_request {
    using response_type = struct put_image_response;
    static constexpr std::string_view nats_subject = "assets.v1.images.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    image_change change;
    ores::utility::domain::change_intent intent;
};

struct put_image_response {
    ores::utility::domain::result result;
    std::optional<ores::assets::domain::image> image;
};

struct put_many_images_request {
    using response_type = struct put_many_images_response;
    static constexpr std::string_view nats_subject = "assets.v1.images.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<image_change> changes;
    ores::utility::domain::change_intent intent;
};

struct put_many_images_response {
    ores::utility::domain::result result;
    std::vector<ores::assets::domain::image> images;
};

struct delete_image_request {
    using response_type = struct delete_image_response;
    static constexpr std::string_view nats_subject = "assets.v1.images.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    image_removal removal;
    ores::utility::domain::change_intent intent;
};

struct delete_image_response {
    ores::utility::domain::result result;
};

struct delete_many_images_request {
    using response_type = struct delete_many_images_response;
    static constexpr std::string_view nats_subject = "assets.v1.images.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<image_removal> removals;
    ores::utility::domain::change_intent intent;
};

struct delete_many_images_response {
    ores::utility::domain::result result;
};

struct list_image_versions_request {
    using response_type = struct list_image_versions_response;
    static constexpr std::string_view nats_subject = "assets.v1.images_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    image_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<image_versions_filter> filter;
};

struct list_image_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::assets::domain::image> versions;
    std::uint64_t total;
};

struct get_image_version_request {
    using response_type = struct get_image_version_response;
    static constexpr std::string_view nats_subject = "assets.v1.images_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    image_version_key key;
};

struct get_image_version_response {
    ores::utility::domain::result result;
    std::optional<ores::assets::domain::image> version;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace image_event_subjects {
inline constexpr std::string_view created = "assets.v1.images_events.created";
inline constexpr std::string_view updated = "assets.v1.images_events.updated";
inline constexpr std::string_view deleted = "assets.v1.images_events.deleted";
}
/**
 * @brief The members of image_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const image_key&) {
    return std::tuple{
        utility::codec::field{"code", &image_key::code},
    };
}

/**
 * @brief The members of image_write, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const image_write&) {
    return std::tuple{
        utility::codec::field{"id", &image_write::id},
        utility::codec::field{"code", &image_write::code},
        utility::codec::field{"description", &image_write::description},
        utility::codec::field{"mime_type", &image_write::mime_type},
        utility::codec::field{"data", &image_write::data},
    };
}

/**
 * @brief The members of image_change, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const image_change&) {
    return std::tuple{
        utility::codec::field{"write", &image_change::write},
        utility::codec::field{"precondition", &image_change::precondition},
    };
}

/**
 * @brief The members of image_removal, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const image_removal&) {
    return std::tuple{
        utility::codec::field{"key", &image_removal::key},
        utility::codec::field{"precondition", &image_removal::precondition},
    };
}

/**
 * @brief The members of image_lookup, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const image_lookup&) {
    return std::tuple{
        utility::codec::field{"key", &image_lookup::key},
        utility::codec::field{"image", &image_lookup::image},
    };
}

/**
 * @brief The members of images_filter, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const images_filter&) {
    return std::tuple{
        utility::codec::field{"id_one_of", &images_filter::id_one_of},
        utility::codec::field{"search", &images_filter::search},
    };
}

/**
 * @brief The members of image_event, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const image_event&) {
    return std::tuple{
        utility::codec::field{"event_id", &image_event::event_id},
        utility::codec::field{"key", &image_event::key},
        utility::codec::field{"action", &image_event::action},
        utility::codec::field{"version", &image_event::version},
        utility::codec::field{"occurred_at", &image_event::occurred_at},
        utility::codec::field{"correlation_id", &image_event::correlation_id},
    };
}

/**
 * @brief The members of image_version_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const image_version_key&) {
    return std::tuple{
        utility::codec::field{"image", &image_version_key::image},
        utility::codec::field{"version", &image_version_key::version},
    };
}

/**
 * @brief The members of image_versions_filter, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const image_versions_filter&) {
    return std::tuple{
        utility::codec::field{"version", &image_versions_filter::version},
        utility::codec::field{"from_version", &image_versions_filter::from_version},
        utility::codec::field{"to_version", &image_versions_filter::to_version},
    };
}

/**
 * @brief The members of list_images_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_images_request&) {
    return std::tuple{
        utility::codec::field{"offset", &list_images_request::offset},
        utility::codec::field{"limit", &list_images_request::limit},
        utility::codec::field{"order", &list_images_request::order},
        utility::codec::field{"filter", &list_images_request::filter},
        utility::codec::field{"as_of", &list_images_request::as_of},
    };
}

/**
 * @brief The members of list_images_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_images_response&) {
    return std::tuple{
        utility::codec::field{"result", &list_images_response::result},
        utility::codec::field{"images", &list_images_response::images},
        utility::codec::field{"total", &list_images_response::total},
    };
}

/**
 * @brief The members of get_image_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_image_request&) {
    return std::tuple{
        utility::codec::field{"key", &get_image_request::key},
    };
}

/**
 * @brief The members of get_image_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_image_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_image_response::result},
        utility::codec::field{"image", &get_image_response::image},
    };
}

/**
 * @brief The members of get_many_images_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_many_images_request&) {
    return std::tuple{
        utility::codec::field{"keys", &get_many_images_request::keys},
    };
}

/**
 * @brief The members of get_many_images_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_many_images_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_many_images_response::result},
        utility::codec::field{"entries", &get_many_images_response::entries},
    };
}

/**
 * @brief The members of put_image_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_image_request&) {
    return std::tuple{
        utility::codec::field{"change", &put_image_request::change},
        utility::codec::field{"intent", &put_image_request::intent},
    };
}

/**
 * @brief The members of put_image_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_image_response&) {
    return std::tuple{
        utility::codec::field{"result", &put_image_response::result},
        utility::codec::field{"image", &put_image_response::image},
    };
}

/**
 * @brief The members of put_many_images_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_many_images_request&) {
    return std::tuple{
        utility::codec::field{"changes", &put_many_images_request::changes},
        utility::codec::field{"intent", &put_many_images_request::intent},
    };
}

/**
 * @brief The members of put_many_images_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_many_images_response&) {
    return std::tuple{
        utility::codec::field{"result", &put_many_images_response::result},
        utility::codec::field{"images", &put_many_images_response::images},
    };
}

/**
 * @brief The members of delete_image_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_image_request&) {
    return std::tuple{
        utility::codec::field{"removal", &delete_image_request::removal},
        utility::codec::field{"intent", &delete_image_request::intent},
    };
}

/**
 * @brief The members of delete_image_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_image_response&) {
    return std::tuple{
        utility::codec::field{"result", &delete_image_response::result},
    };
}

/**
 * @brief The members of delete_many_images_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_many_images_request&) {
    return std::tuple{
        utility::codec::field{"removals", &delete_many_images_request::removals},
        utility::codec::field{"intent", &delete_many_images_request::intent},
    };
}

/**
 * @brief The members of delete_many_images_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_many_images_response&) {
    return std::tuple{
        utility::codec::field{"result", &delete_many_images_response::result},
    };
}

/**
 * @brief The members of list_image_versions_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_image_versions_request&) {
    return std::tuple{
        utility::codec::field{"key", &list_image_versions_request::key},
        utility::codec::field{"offset", &list_image_versions_request::offset},
        utility::codec::field{"limit", &list_image_versions_request::limit},
        utility::codec::field{"order", &list_image_versions_request::order},
        utility::codec::field{"filter", &list_image_versions_request::filter},
    };
}

/**
 * @brief The members of list_image_versions_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_image_versions_response&) {
    return std::tuple{
        utility::codec::field{"result", &list_image_versions_response::result},
        utility::codec::field{"versions", &list_image_versions_response::versions},
        utility::codec::field{"total", &list_image_versions_response::total},
    };
}

/**
 * @brief The members of get_image_version_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_image_version_request&) {
    return std::tuple{
        utility::codec::field{"key", &get_image_version_request::key},
    };
}

/**
 * @brief The members of get_image_version_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_image_version_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_image_version_response::result},
        utility::codec::field{"version", &get_image_version_response::version},
    };
}


}
namespace ores::utility::codec {

template <>
struct ORES_ASSETS_API_EXPORT wire_traits<ores::assets::messaging::image_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::assets::messaging::image_key& v);
    static ores::assets::messaging::image_key read_json(std::string_view text);
    static std::string write_msgpack(const ores::assets::messaging::image_key& v);
    static ores::assets::messaging::image_key read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ASSETS_API_EXPORT wire_traits<ores::assets::messaging::image_write> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::assets::messaging::image_write& v);
    static ores::assets::messaging::image_write read_json(std::string_view text);
    static std::string write_msgpack(const ores::assets::messaging::image_write& v);
    static ores::assets::messaging::image_write read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ASSETS_API_EXPORT wire_traits<ores::assets::messaging::image_change> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::assets::messaging::image_change& v);
    static ores::assets::messaging::image_change read_json(std::string_view text);
    static std::string write_msgpack(const ores::assets::messaging::image_change& v);
    static ores::assets::messaging::image_change read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ASSETS_API_EXPORT wire_traits<ores::assets::messaging::image_removal> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::assets::messaging::image_removal& v);
    static ores::assets::messaging::image_removal read_json(std::string_view text);
    static std::string write_msgpack(const ores::assets::messaging::image_removal& v);
    static ores::assets::messaging::image_removal read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ASSETS_API_EXPORT wire_traits<ores::assets::messaging::image_lookup> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::assets::messaging::image_lookup& v);
    static ores::assets::messaging::image_lookup read_json(std::string_view text);
    static std::string write_msgpack(const ores::assets::messaging::image_lookup& v);
    static ores::assets::messaging::image_lookup read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ASSETS_API_EXPORT wire_traits<ores::assets::messaging::images_filter> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::assets::messaging::images_filter& v);
    static ores::assets::messaging::images_filter read_json(std::string_view text);
    static std::string write_msgpack(const ores::assets::messaging::images_filter& v);
    static ores::assets::messaging::images_filter read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ASSETS_API_EXPORT wire_traits<ores::assets::messaging::image_event> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::assets::messaging::image_event& v);
    static ores::assets::messaging::image_event read_json(std::string_view text);
    static std::string write_msgpack(const ores::assets::messaging::image_event& v);
    static ores::assets::messaging::image_event read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ASSETS_API_EXPORT wire_traits<ores::assets::messaging::image_version_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::assets::messaging::image_version_key& v);
    static ores::assets::messaging::image_version_key read_json(std::string_view text);
    static std::string write_msgpack(const ores::assets::messaging::image_version_key& v);
    static ores::assets::messaging::image_version_key read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ASSETS_API_EXPORT wire_traits<ores::assets::messaging::image_versions_filter> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::assets::messaging::image_versions_filter& v);
    static ores::assets::messaging::image_versions_filter read_json(std::string_view text);
    static std::string write_msgpack(const ores::assets::messaging::image_versions_filter& v);
    static ores::assets::messaging::image_versions_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ASSETS_API_EXPORT wire_traits<ores::assets::messaging::list_images_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::assets::messaging::list_images_request& v);
    static ores::assets::messaging::list_images_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::assets::messaging::list_images_request& v);
    static ores::assets::messaging::list_images_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ASSETS_API_EXPORT wire_traits<ores::assets::messaging::list_images_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::assets::messaging::list_images_response& v);
    static ores::assets::messaging::list_images_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::assets::messaging::list_images_response& v);
    static ores::assets::messaging::list_images_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ASSETS_API_EXPORT wire_traits<ores::assets::messaging::get_image_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::assets::messaging::get_image_request& v);
    static ores::assets::messaging::get_image_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::assets::messaging::get_image_request& v);
    static ores::assets::messaging::get_image_request read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ASSETS_API_EXPORT wire_traits<ores::assets::messaging::get_image_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::assets::messaging::get_image_response& v);
    static ores::assets::messaging::get_image_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::assets::messaging::get_image_response& v);
    static ores::assets::messaging::get_image_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ASSETS_API_EXPORT wire_traits<ores::assets::messaging::get_many_images_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::assets::messaging::get_many_images_request& v);
    static ores::assets::messaging::get_many_images_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::assets::messaging::get_many_images_request& v);
    static ores::assets::messaging::get_many_images_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ASSETS_API_EXPORT wire_traits<ores::assets::messaging::get_many_images_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::assets::messaging::get_many_images_response& v);
    static ores::assets::messaging::get_many_images_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::assets::messaging::get_many_images_response& v);
    static ores::assets::messaging::get_many_images_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ASSETS_API_EXPORT wire_traits<ores::assets::messaging::put_image_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::assets::messaging::put_image_request& v);
    static ores::assets::messaging::put_image_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::assets::messaging::put_image_request& v);
    static ores::assets::messaging::put_image_request read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ASSETS_API_EXPORT wire_traits<ores::assets::messaging::put_image_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::assets::messaging::put_image_response& v);
    static ores::assets::messaging::put_image_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::assets::messaging::put_image_response& v);
    static ores::assets::messaging::put_image_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ASSETS_API_EXPORT wire_traits<ores::assets::messaging::put_many_images_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::assets::messaging::put_many_images_request& v);
    static ores::assets::messaging::put_many_images_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::assets::messaging::put_many_images_request& v);
    static ores::assets::messaging::put_many_images_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ASSETS_API_EXPORT wire_traits<ores::assets::messaging::put_many_images_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::assets::messaging::put_many_images_response& v);
    static ores::assets::messaging::put_many_images_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::assets::messaging::put_many_images_response& v);
    static ores::assets::messaging::put_many_images_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ASSETS_API_EXPORT wire_traits<ores::assets::messaging::delete_image_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::assets::messaging::delete_image_request& v);
    static ores::assets::messaging::delete_image_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::assets::messaging::delete_image_request& v);
    static ores::assets::messaging::delete_image_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ASSETS_API_EXPORT wire_traits<ores::assets::messaging::delete_image_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::assets::messaging::delete_image_response& v);
    static ores::assets::messaging::delete_image_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::assets::messaging::delete_image_response& v);
    static ores::assets::messaging::delete_image_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ASSETS_API_EXPORT wire_traits<ores::assets::messaging::delete_many_images_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::assets::messaging::delete_many_images_request& v);
    static ores::assets::messaging::delete_many_images_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::assets::messaging::delete_many_images_request& v);
    static ores::assets::messaging::delete_many_images_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ASSETS_API_EXPORT wire_traits<ores::assets::messaging::delete_many_images_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::assets::messaging::delete_many_images_response& v);
    static ores::assets::messaging::delete_many_images_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::assets::messaging::delete_many_images_response& v);
    static ores::assets::messaging::delete_many_images_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ASSETS_API_EXPORT wire_traits<ores::assets::messaging::list_image_versions_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::assets::messaging::list_image_versions_request& v);
    static ores::assets::messaging::list_image_versions_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::assets::messaging::list_image_versions_request& v);
    static ores::assets::messaging::list_image_versions_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ASSETS_API_EXPORT wire_traits<ores::assets::messaging::list_image_versions_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::assets::messaging::list_image_versions_response& v);
    static ores::assets::messaging::list_image_versions_response read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::assets::messaging::list_image_versions_response& v);
    static ores::assets::messaging::list_image_versions_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ASSETS_API_EXPORT wire_traits<ores::assets::messaging::get_image_version_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::assets::messaging::get_image_version_request& v);
    static ores::assets::messaging::get_image_version_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::assets::messaging::get_image_version_request& v);
    static ores::assets::messaging::get_image_version_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ASSETS_API_EXPORT wire_traits<ores::assets::messaging::get_image_version_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::assets::messaging::get_image_version_response& v);
    static ores::assets::messaging::get_image_version_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::assets::messaging::get_image_version_response& v);
    static ores::assets::messaging::get_image_version_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
