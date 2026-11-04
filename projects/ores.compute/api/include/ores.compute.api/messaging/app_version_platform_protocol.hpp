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
#ifndef ORES_COMPUTE_API_MESSAGING_APP_VERSION_PLATFORM_PROTOCOL_HPP
#define ORES_COMPUTE_API_MESSAGING_APP_VERSION_PLATFORM_PROTOCOL_HPP

#include "ores.compute.api/domain/app_version_platform.hpp"
#include "ores.compute.api/export.hpp"
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

namespace ores::compute::messaging {

struct app_version_platform_key {
    boost::uuids::uuid app_version_id;
    boost::uuids::uuid platform_id;

    friend bool operator==(const app_version_platform_key&,
                           const app_version_platform_key&) = default;
};

struct app_version_platform_write {
    boost::uuids::uuid app_version_id;
    boost::uuids::uuid platform_id;
    std::string package_uri;
    std::string sha256;

    friend bool operator==(const app_version_platform_write&,
                           const app_version_platform_write&) = default;
};

struct app_version_platform_change {
    app_version_platform_write write;
    ores::utility::domain::precondition precondition;

    friend bool operator==(const app_version_platform_change&,
                           const app_version_platform_change&) = default;
};

struct app_version_platform_removal {
    app_version_platform_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;

    friend bool operator==(const app_version_platform_removal&,
                           const app_version_platform_removal&) = default;
};

struct app_version_platform_lookup {
    app_version_platform_key key;
    std::optional<ores::compute::domain::app_version_platform> app_version_platform;

    friend bool operator==(const app_version_platform_lookup&,
                           const app_version_platform_lookup&) = default;
};

struct app_version_platforms_filter {
    std::optional<boost::uuids::uuid> app_version_id;
    std::optional<std::vector<boost::uuids::uuid>> app_version_id_one_of;

    friend bool operator==(const app_version_platforms_filter&,
                           const app_version_platforms_filter&) = default;
};

struct list_app_version_platforms_request {
    using response_type = struct list_app_version_platforms_response;
    static constexpr std::string_view nats_subject = "compute.v1.app_version_platforms.list";
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
    std::optional<app_version_platforms_filter> filter;

    friend bool operator==(const list_app_version_platforms_request&,
                           const list_app_version_platforms_request&) = default;
};

struct list_app_version_platforms_response {
    ores::utility::domain::result result;
    std::vector<ores::compute::domain::app_version_platform> app_version_platforms;
    std::uint64_t total;

    friend bool operator==(const list_app_version_platforms_response&,
                           const list_app_version_platforms_response&) = default;
};

struct get_app_version_platform_request {
    using response_type = struct get_app_version_platform_response;
    static constexpr std::string_view nats_subject = "compute.v1.app_version_platforms.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    app_version_platform_key key;

    friend bool operator==(const get_app_version_platform_request&,
                           const get_app_version_platform_request&) = default;
};

struct get_app_version_platform_response {
    ores::utility::domain::result result;
    std::optional<ores::compute::domain::app_version_platform> app_version_platform;

    friend bool operator==(const get_app_version_platform_response&,
                           const get_app_version_platform_response&) = default;
};

struct get_many_app_version_platforms_request {
    using response_type = struct get_many_app_version_platforms_response;
    static constexpr std::string_view nats_subject = "compute.v1.app_version_platforms.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<app_version_platform_key> keys;

    friend bool operator==(const get_many_app_version_platforms_request&,
                           const get_many_app_version_platforms_request&) = default;
};

struct get_many_app_version_platforms_response {
    ores::utility::domain::result result;
    std::vector<app_version_platform_lookup> entries;

    friend bool operator==(const get_many_app_version_platforms_response&,
                           const get_many_app_version_platforms_response&) = default;
};

struct put_app_version_platform_request {
    using response_type = struct put_app_version_platform_response;
    static constexpr std::string_view nats_subject = "compute.v1.app_version_platforms.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    app_version_platform_change change;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const put_app_version_platform_request&,
                           const put_app_version_platform_request&) = default;
};

struct put_app_version_platform_response {
    ores::utility::domain::result result;
    std::optional<ores::compute::domain::app_version_platform> app_version_platform;

    friend bool operator==(const put_app_version_platform_response&,
                           const put_app_version_platform_response&) = default;
};

struct put_many_app_version_platforms_request {
    using response_type = struct put_many_app_version_platforms_response;
    static constexpr std::string_view nats_subject = "compute.v1.app_version_platforms.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<app_version_platform_change> changes;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const put_many_app_version_platforms_request&,
                           const put_many_app_version_platforms_request&) = default;
};

struct put_many_app_version_platforms_response {
    ores::utility::domain::result result;
    std::vector<ores::compute::domain::app_version_platform> app_version_platforms;

    friend bool operator==(const put_many_app_version_platforms_response&,
                           const put_many_app_version_platforms_response&) = default;
};

struct delete_app_version_platform_request {
    using response_type = struct delete_app_version_platform_response;
    static constexpr std::string_view nats_subject = "compute.v1.app_version_platforms.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    app_version_platform_removal removal;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const delete_app_version_platform_request&,
                           const delete_app_version_platform_request&) = default;
};

struct delete_app_version_platform_response {
    ores::utility::domain::result result;

    friend bool operator==(const delete_app_version_platform_response&,
                           const delete_app_version_platform_response&) = default;
};

struct delete_many_app_version_platforms_request {
    using response_type = struct delete_many_app_version_platforms_response;
    static constexpr std::string_view nats_subject = "compute.v1.app_version_platforms.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<app_version_platform_removal> removals;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const delete_many_app_version_platforms_request&,
                           const delete_many_app_version_platforms_request&) = default;
};

struct delete_many_app_version_platforms_response {
    ores::utility::domain::result result;

    friend bool operator==(const delete_many_app_version_platforms_response&,
                           const delete_many_app_version_platforms_response&) = default;
};

struct list_by_app_version_id_app_version_platforms_request {
    using response_type = struct list_by_app_version_id_app_version_platforms_response;
    static constexpr std::string_view nats_subject =
        "compute.v1.app_version_platforms.list_by_app_version_id";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    boost::uuids::uuid app_version_id;
    ores::utility::domain::scope scope = ores::utility::domain::scope::direct;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<app_version_platforms_filter> filter;

    friend bool operator==(const list_by_app_version_id_app_version_platforms_request&,
                           const list_by_app_version_id_app_version_platforms_request&) = default;
};

struct list_by_app_version_id_app_version_platforms_response {
    ores::utility::domain::result result;
    std::vector<ores::compute::domain::app_version_platform> app_version_platforms;
    std::uint64_t total;

    friend bool operator==(const list_by_app_version_id_app_version_platforms_response&,
                           const list_by_app_version_id_app_version_platforms_response&) = default;
};

/**
 * @brief The members of app_version_platform_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<app_version_platform_key>) {
    return std::tuple{
        utility::codec::field{"app_version_id", &app_version_platform_key::app_version_id},
        utility::codec::field{"platform_id", &app_version_platform_key::platform_id},
    };
}

/**
 * @brief The members of app_version_platform_write, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<app_version_platform_write>) {
    return std::tuple{
        utility::codec::field{"app_version_id", &app_version_platform_write::app_version_id},
        utility::codec::field{"platform_id", &app_version_platform_write::platform_id},
        utility::codec::field{"package_uri", &app_version_platform_write::package_uri},
        utility::codec::field{"sha256", &app_version_platform_write::sha256},
    };
}

/**
 * @brief The members of app_version_platform_change, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<app_version_platform_change>) {
    return std::tuple{
        utility::codec::field{"write", &app_version_platform_change::write},
        utility::codec::field{"precondition", &app_version_platform_change::precondition},
    };
}

/**
 * @brief The members of app_version_platform_removal, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<app_version_platform_removal>) {
    return std::tuple{
        utility::codec::field{"key", &app_version_platform_removal::key},
        utility::codec::field{"precondition", &app_version_platform_removal::precondition},
    };
}

/**
 * @brief The members of app_version_platform_lookup, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<app_version_platform_lookup>) {
    return std::tuple{
        utility::codec::field{"key", &app_version_platform_lookup::key},
        utility::codec::field{"app_version_platform",
                              &app_version_platform_lookup::app_version_platform},
    };
}

/**
 * @brief The members of app_version_platforms_filter, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<app_version_platforms_filter>) {
    return std::tuple{
        utility::codec::field{"app_version_id", &app_version_platforms_filter::app_version_id},
        utility::codec::field{"app_version_id_one_of",
                              &app_version_platforms_filter::app_version_id_one_of},
    };
}

/**
 * @brief The members of list_app_version_platforms_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<list_app_version_platforms_request>) {
    return std::tuple{
        utility::codec::field{"offset", &list_app_version_platforms_request::offset},
        utility::codec::field{"limit", &list_app_version_platforms_request::limit},
        utility::codec::field{"order", &list_app_version_platforms_request::order},
        utility::codec::field{"filter", &list_app_version_platforms_request::filter},
    };
}

/**
 * @brief The members of list_app_version_platforms_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<list_app_version_platforms_response>) {
    return std::tuple{
        utility::codec::field{"result", &list_app_version_platforms_response::result},
        utility::codec::field{"app_version_platforms",
                              &list_app_version_platforms_response::app_version_platforms},
        utility::codec::field{"total", &list_app_version_platforms_response::total},
    };
}

/**
 * @brief The members of get_app_version_platform_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_app_version_platform_request>) {
    return std::tuple{
        utility::codec::field{"key", &get_app_version_platform_request::key},
    };
}

/**
 * @brief The members of get_app_version_platform_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_app_version_platform_response>) {
    return std::tuple{
        utility::codec::field{"result", &get_app_version_platform_response::result},
        utility::codec::field{"app_version_platform",
                              &get_app_version_platform_response::app_version_platform},
    };
}

/**
 * @brief The members of get_many_app_version_platforms_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<get_many_app_version_platforms_request>) {
    return std::tuple{
        utility::codec::field{"keys", &get_many_app_version_platforms_request::keys},
    };
}

/**
 * @brief The members of get_many_app_version_platforms_response, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<get_many_app_version_platforms_response>) {
    return std::tuple{
        utility::codec::field{"result", &get_many_app_version_platforms_response::result},
        utility::codec::field{"entries", &get_many_app_version_platforms_response::entries},
    };
}

/**
 * @brief The members of put_app_version_platform_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<put_app_version_platform_request>) {
    return std::tuple{
        utility::codec::field{"change", &put_app_version_platform_request::change},
        utility::codec::field{"intent", &put_app_version_platform_request::intent},
    };
}

/**
 * @brief The members of put_app_version_platform_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<put_app_version_platform_response>) {
    return std::tuple{
        utility::codec::field{"result", &put_app_version_platform_response::result},
        utility::codec::field{"app_version_platform",
                              &put_app_version_platform_response::app_version_platform},
    };
}

/**
 * @brief The members of put_many_app_version_platforms_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<put_many_app_version_platforms_request>) {
    return std::tuple{
        utility::codec::field{"changes", &put_many_app_version_platforms_request::changes},
        utility::codec::field{"intent", &put_many_app_version_platforms_request::intent},
    };
}

/**
 * @brief The members of put_many_app_version_platforms_response, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<put_many_app_version_platforms_response>) {
    return std::tuple{
        utility::codec::field{"result", &put_many_app_version_platforms_response::result},
        utility::codec::field{"app_version_platforms",
                              &put_many_app_version_platforms_response::app_version_platforms},
    };
}

/**
 * @brief The members of delete_app_version_platform_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<delete_app_version_platform_request>) {
    return std::tuple{
        utility::codec::field{"removal", &delete_app_version_platform_request::removal},
        utility::codec::field{"intent", &delete_app_version_platform_request::intent},
    };
}

/**
 * @brief The members of delete_app_version_platform_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<delete_app_version_platform_response>) {
    return std::tuple{
        utility::codec::field{"result", &delete_app_version_platform_response::result},
    };
}

/**
 * @brief The members of delete_many_app_version_platforms_request, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<delete_many_app_version_platforms_request>) {
    return std::tuple{
        utility::codec::field{"removals", &delete_many_app_version_platforms_request::removals},
        utility::codec::field{"intent", &delete_many_app_version_platforms_request::intent},
    };
}

/**
 * @brief The members of delete_many_app_version_platforms_response, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<delete_many_app_version_platforms_response>) {
    return std::tuple{
        utility::codec::field{"result", &delete_many_app_version_platforms_response::result},
    };
}

/**
 * @brief The members of list_by_app_version_id_app_version_platforms_request, named as the wire
 * names them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<list_by_app_version_id_app_version_platforms_request>) {
    return std::tuple{
        utility::codec::field{
            "app_version_id",
            &list_by_app_version_id_app_version_platforms_request::app_version_id},
        utility::codec::field{"scope",
                              &list_by_app_version_id_app_version_platforms_request::scope},
        utility::codec::field{"offset",
                              &list_by_app_version_id_app_version_platforms_request::offset},
        utility::codec::field{"limit",
                              &list_by_app_version_id_app_version_platforms_request::limit},
        utility::codec::field{"order",
                              &list_by_app_version_id_app_version_platforms_request::order},
        utility::codec::field{"filter",
                              &list_by_app_version_id_app_version_platforms_request::filter},
    };
}

/**
 * @brief The members of list_by_app_version_id_app_version_platforms_response, named as the wire
 * names them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<list_by_app_version_id_app_version_platforms_response>) {
    return std::tuple{
        utility::codec::field{"result",
                              &list_by_app_version_id_app_version_platforms_response::result},
        utility::codec::field{
            "app_version_platforms",
            &list_by_app_version_id_app_version_platforms_response::app_version_platforms},
        utility::codec::field{"total",
                              &list_by_app_version_id_app_version_platforms_response::total},
    };
}

}
namespace ores::utility::codec {

template <>
struct ORES_COMPUTE_API_EXPORT wire_traits<ores::compute::messaging::app_version_platform_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::compute::messaging::app_version_platform_key& v);
    static ores::compute::messaging::app_version_platform_key read_json(std::string_view text);
    static std::string write_msgpack(const ores::compute::messaging::app_version_platform_key& v);
    static ores::compute::messaging::app_version_platform_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_COMPUTE_API_EXPORT wire_traits<ores::compute::messaging::app_version_platform_write> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::compute::messaging::app_version_platform_write& v);
    static ores::compute::messaging::app_version_platform_write read_json(std::string_view text);
    static std::string write_msgpack(const ores::compute::messaging::app_version_platform_write& v);
    static ores::compute::messaging::app_version_platform_write
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_COMPUTE_API_EXPORT wire_traits<ores::compute::messaging::app_version_platform_change> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::compute::messaging::app_version_platform_change& v);
    static ores::compute::messaging::app_version_platform_change read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::compute::messaging::app_version_platform_change& v);
    static ores::compute::messaging::app_version_platform_change
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_COMPUTE_API_EXPORT wire_traits<ores::compute::messaging::app_version_platform_removal> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::compute::messaging::app_version_platform_removal& v);
    static ores::compute::messaging::app_version_platform_removal read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::compute::messaging::app_version_platform_removal& v);
    static ores::compute::messaging::app_version_platform_removal
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_COMPUTE_API_EXPORT wire_traits<ores::compute::messaging::app_version_platform_lookup> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::compute::messaging::app_version_platform_lookup& v);
    static ores::compute::messaging::app_version_platform_lookup read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::compute::messaging::app_version_platform_lookup& v);
    static ores::compute::messaging::app_version_platform_lookup
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_COMPUTE_API_EXPORT wire_traits<ores::compute::messaging::app_version_platforms_filter> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::compute::messaging::app_version_platforms_filter& v);
    static ores::compute::messaging::app_version_platforms_filter read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::compute::messaging::app_version_platforms_filter& v);
    static ores::compute::messaging::app_version_platforms_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_COMPUTE_API_EXPORT
    wire_traits<ores::compute::messaging::list_app_version_platforms_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::compute::messaging::list_app_version_platforms_request& v);
    static ores::compute::messaging::list_app_version_platforms_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::compute::messaging::list_app_version_platforms_request& v);
    static ores::compute::messaging::list_app_version_platforms_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_COMPUTE_API_EXPORT
    wire_traits<ores::compute::messaging::list_app_version_platforms_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::compute::messaging::list_app_version_platforms_response& v);
    static ores::compute::messaging::list_app_version_platforms_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::compute::messaging::list_app_version_platforms_response& v);
    static ores::compute::messaging::list_app_version_platforms_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_COMPUTE_API_EXPORT
    wire_traits<ores::compute::messaging::get_app_version_platform_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::compute::messaging::get_app_version_platform_request& v);
    static ores::compute::messaging::get_app_version_platform_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::compute::messaging::get_app_version_platform_request& v);
    static ores::compute::messaging::get_app_version_platform_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_COMPUTE_API_EXPORT
    wire_traits<ores::compute::messaging::get_app_version_platform_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::compute::messaging::get_app_version_platform_response& v);
    static ores::compute::messaging::get_app_version_platform_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::compute::messaging::get_app_version_platform_response& v);
    static ores::compute::messaging::get_app_version_platform_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_COMPUTE_API_EXPORT
    wire_traits<ores::compute::messaging::get_many_app_version_platforms_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::compute::messaging::get_many_app_version_platforms_request& v);
    static ores::compute::messaging::get_many_app_version_platforms_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::compute::messaging::get_many_app_version_platforms_request& v);
    static ores::compute::messaging::get_many_app_version_platforms_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_COMPUTE_API_EXPORT
    wire_traits<ores::compute::messaging::get_many_app_version_platforms_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::compute::messaging::get_many_app_version_platforms_response& v);
    static ores::compute::messaging::get_many_app_version_platforms_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::compute::messaging::get_many_app_version_platforms_response& v);
    static ores::compute::messaging::get_many_app_version_platforms_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_COMPUTE_API_EXPORT
    wire_traits<ores::compute::messaging::put_app_version_platform_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::compute::messaging::put_app_version_platform_request& v);
    static ores::compute::messaging::put_app_version_platform_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::compute::messaging::put_app_version_platform_request& v);
    static ores::compute::messaging::put_app_version_platform_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_COMPUTE_API_EXPORT
    wire_traits<ores::compute::messaging::put_app_version_platform_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::compute::messaging::put_app_version_platform_response& v);
    static ores::compute::messaging::put_app_version_platform_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::compute::messaging::put_app_version_platform_response& v);
    static ores::compute::messaging::put_app_version_platform_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_COMPUTE_API_EXPORT
    wire_traits<ores::compute::messaging::put_many_app_version_platforms_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::compute::messaging::put_many_app_version_platforms_request& v);
    static ores::compute::messaging::put_many_app_version_platforms_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::compute::messaging::put_many_app_version_platforms_request& v);
    static ores::compute::messaging::put_many_app_version_platforms_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_COMPUTE_API_EXPORT
    wire_traits<ores::compute::messaging::put_many_app_version_platforms_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::compute::messaging::put_many_app_version_platforms_response& v);
    static ores::compute::messaging::put_many_app_version_platforms_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::compute::messaging::put_many_app_version_platforms_response& v);
    static ores::compute::messaging::put_many_app_version_platforms_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_COMPUTE_API_EXPORT
    wire_traits<ores::compute::messaging::delete_app_version_platform_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::compute::messaging::delete_app_version_platform_request& v);
    static ores::compute::messaging::delete_app_version_platform_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::compute::messaging::delete_app_version_platform_request& v);
    static ores::compute::messaging::delete_app_version_platform_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_COMPUTE_API_EXPORT
    wire_traits<ores::compute::messaging::delete_app_version_platform_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::compute::messaging::delete_app_version_platform_response& v);
    static ores::compute::messaging::delete_app_version_platform_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::compute::messaging::delete_app_version_platform_response& v);
    static ores::compute::messaging::delete_app_version_platform_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_COMPUTE_API_EXPORT
    wire_traits<ores::compute::messaging::delete_many_app_version_platforms_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::compute::messaging::delete_many_app_version_platforms_request& v);
    static ores::compute::messaging::delete_many_app_version_platforms_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::compute::messaging::delete_many_app_version_platforms_request& v);
    static ores::compute::messaging::delete_many_app_version_platforms_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_COMPUTE_API_EXPORT
    wire_traits<ores::compute::messaging::delete_many_app_version_platforms_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::compute::messaging::delete_many_app_version_platforms_response& v);
    static ores::compute::messaging::delete_many_app_version_platforms_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::compute::messaging::delete_many_app_version_platforms_response& v);
    static ores::compute::messaging::delete_many_app_version_platforms_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_COMPUTE_API_EXPORT
    wire_traits<ores::compute::messaging::list_by_app_version_id_app_version_platforms_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::compute::messaging::list_by_app_version_id_app_version_platforms_request& v);
    static ores::compute::messaging::list_by_app_version_id_app_version_platforms_request
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::compute::messaging::list_by_app_version_id_app_version_platforms_request& v);
    static ores::compute::messaging::list_by_app_version_id_app_version_platforms_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_COMPUTE_API_EXPORT
    wire_traits<ores::compute::messaging::list_by_app_version_id_app_version_platforms_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::compute::messaging::list_by_app_version_id_app_version_platforms_response& v);
    static ores::compute::messaging::list_by_app_version_id_app_version_platforms_response
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::compute::messaging::list_by_app_version_id_app_version_platforms_response& v);
    static ores::compute::messaging::list_by_app_version_id_app_version_platforms_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
