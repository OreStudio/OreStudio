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
#ifndef ORES_COMPUTE_API_MESSAGING_PLATFORM_PROTOCOL_HPP
#define ORES_COMPUTE_API_MESSAGING_PLATFORM_PROTOCOL_HPP

#include "ores.compute.api/domain/platform.hpp"
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

struct platform_key {
    std::string code;

    friend bool operator==(const platform_key&, const platform_key&) = default;
};

struct platform_write {
    boost::uuids::uuid id;
    std::string code;
    std::string display_name;
    std::string description;
    std::string os_family;
    std::string cpu_arch;
    std::string abi;
    bool is_active;

    friend bool operator==(const platform_write&, const platform_write&) = default;
};

struct platform_change {
    platform_write write;
    ores::utility::domain::precondition precondition;

    friend bool operator==(const platform_change&, const platform_change&) = default;
};

struct platform_removal {
    platform_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;

    friend bool operator==(const platform_removal&, const platform_removal&) = default;
};

struct platform_lookup {
    platform_key key;
    std::optional<ores::compute::domain::platform> platform;

    friend bool operator==(const platform_lookup&, const platform_lookup&) = default;
};

struct platforms_filter {
    std::optional<std::vector<boost::uuids::uuid>> id_one_of;

    friend bool operator==(const platforms_filter&, const platforms_filter&) = default;
};

struct platform_event {
    boost::uuids::uuid event_id;
    platform_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;

    friend bool operator==(const platform_event&, const platform_event&) = default;
};

struct platform_version_key {
    platform_key platform;
    std::uint32_t version;

    friend bool operator==(const platform_version_key&, const platform_version_key&) = default;
};

struct platform_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;

    friend bool operator==(const platform_versions_filter&,
                           const platform_versions_filter&) = default;
};

struct list_platforms_request {
    using response_type = struct list_platforms_response;
    static constexpr std::string_view nats_subject = "compute.v1.platforms.list";
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
    std::optional<platforms_filter> filter;
    std::optional<std::string> as_of;

    friend bool operator==(const list_platforms_request&, const list_platforms_request&) = default;
};

struct list_platforms_response {
    ores::utility::domain::result result;
    std::vector<ores::compute::domain::platform> platforms;
    std::uint64_t total;

    friend bool operator==(const list_platforms_response&,
                           const list_platforms_response&) = default;
};

struct get_platform_request {
    using response_type = struct get_platform_response;
    static constexpr std::string_view nats_subject = "compute.v1.platforms.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    platform_key key;

    friend bool operator==(const get_platform_request&, const get_platform_request&) = default;
};

struct get_platform_response {
    ores::utility::domain::result result;
    std::optional<ores::compute::domain::platform> platform;

    friend bool operator==(const get_platform_response&, const get_platform_response&) = default;
};

struct get_many_platforms_request {
    using response_type = struct get_many_platforms_response;
    static constexpr std::string_view nats_subject = "compute.v1.platforms.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<platform_key> keys;

    friend bool operator==(const get_many_platforms_request&,
                           const get_many_platforms_request&) = default;
};

struct get_many_platforms_response {
    ores::utility::domain::result result;
    std::vector<platform_lookup> entries;

    friend bool operator==(const get_many_platforms_response&,
                           const get_many_platforms_response&) = default;
};

struct put_platform_request {
    using response_type = struct put_platform_response;
    static constexpr std::string_view nats_subject = "compute.v1.platforms.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    platform_change change;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const put_platform_request&, const put_platform_request&) = default;
};

struct put_platform_response {
    ores::utility::domain::result result;
    std::optional<ores::compute::domain::platform> platform;

    friend bool operator==(const put_platform_response&, const put_platform_response&) = default;
};

struct put_many_platforms_request {
    using response_type = struct put_many_platforms_response;
    static constexpr std::string_view nats_subject = "compute.v1.platforms.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<platform_change> changes;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const put_many_platforms_request&,
                           const put_many_platforms_request&) = default;
};

struct put_many_platforms_response {
    ores::utility::domain::result result;
    std::vector<ores::compute::domain::platform> platforms;

    friend bool operator==(const put_many_platforms_response&,
                           const put_many_platforms_response&) = default;
};

struct delete_platform_request {
    using response_type = struct delete_platform_response;
    static constexpr std::string_view nats_subject = "compute.v1.platforms.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    platform_removal removal;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const delete_platform_request&,
                           const delete_platform_request&) = default;
};

struct delete_platform_response {
    ores::utility::domain::result result;

    friend bool operator==(const delete_platform_response&,
                           const delete_platform_response&) = default;
};

struct delete_many_platforms_request {
    using response_type = struct delete_many_platforms_response;
    static constexpr std::string_view nats_subject = "compute.v1.platforms.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<platform_removal> removals;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const delete_many_platforms_request&,
                           const delete_many_platforms_request&) = default;
};

struct delete_many_platforms_response {
    ores::utility::domain::result result;

    friend bool operator==(const delete_many_platforms_response&,
                           const delete_many_platforms_response&) = default;
};

struct list_platform_versions_request {
    using response_type = struct list_platform_versions_response;
    static constexpr std::string_view nats_subject = "compute.v1.platforms_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    platform_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<platform_versions_filter> filter;

    friend bool operator==(const list_platform_versions_request&,
                           const list_platform_versions_request&) = default;
};

struct list_platform_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::compute::domain::platform> versions;
    std::uint64_t total;

    friend bool operator==(const list_platform_versions_response&,
                           const list_platform_versions_response&) = default;
};

struct get_platform_version_request {
    using response_type = struct get_platform_version_response;
    static constexpr std::string_view nats_subject = "compute.v1.platforms_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    platform_version_key key;

    friend bool operator==(const get_platform_version_request&,
                           const get_platform_version_request&) = default;
};

struct get_platform_version_response {
    ores::utility::domain::result result;
    std::optional<ores::compute::domain::platform> version;

    friend bool operator==(const get_platform_version_response&,
                           const get_platform_version_response&) = default;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace platform_event_subjects {
inline constexpr std::string_view created = "compute.v1.platforms_events.created";
inline constexpr std::string_view updated = "compute.v1.platforms_events.updated";
inline constexpr std::string_view deleted = "compute.v1.platforms_events.deleted";
}
/**
 * @brief The members of platform_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const platform_key&) {
    return std::tuple{
        utility::codec::field{"code", &platform_key::code},
    };
}

/**
 * @brief The members of platform_write, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const platform_write&) {
    return std::tuple{
        utility::codec::field{"id", &platform_write::id},
        utility::codec::field{"code", &platform_write::code},
        utility::codec::field{"display_name", &platform_write::display_name},
        utility::codec::field{"description", &platform_write::description},
        utility::codec::field{"os_family", &platform_write::os_family},
        utility::codec::field{"cpu_arch", &platform_write::cpu_arch},
        utility::codec::field{"abi", &platform_write::abi},
        utility::codec::field{"is_active", &platform_write::is_active},
    };
}

/**
 * @brief The members of platform_change, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const platform_change&) {
    return std::tuple{
        utility::codec::field{"write", &platform_change::write},
        utility::codec::field{"precondition", &platform_change::precondition},
    };
}

/**
 * @brief The members of platform_removal, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const platform_removal&) {
    return std::tuple{
        utility::codec::field{"key", &platform_removal::key},
        utility::codec::field{"precondition", &platform_removal::precondition},
    };
}

/**
 * @brief The members of platform_lookup, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const platform_lookup&) {
    return std::tuple{
        utility::codec::field{"key", &platform_lookup::key},
        utility::codec::field{"platform", &platform_lookup::platform},
    };
}

/**
 * @brief The members of platforms_filter, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const platforms_filter&) {
    return std::tuple{
        utility::codec::field{"id_one_of", &platforms_filter::id_one_of},
    };
}

/**
 * @brief The members of platform_event, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const platform_event&) {
    return std::tuple{
        utility::codec::field{"event_id", &platform_event::event_id},
        utility::codec::field{"key", &platform_event::key},
        utility::codec::field{"action", &platform_event::action},
        utility::codec::field{"version", &platform_event::version},
        utility::codec::field{"occurred_at", &platform_event::occurred_at},
        utility::codec::field{"correlation_id", &platform_event::correlation_id},
    };
}

/**
 * @brief The members of platform_version_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const platform_version_key&) {
    return std::tuple{
        utility::codec::field{"platform", &platform_version_key::platform},
        utility::codec::field{"version", &platform_version_key::version},
    };
}

/**
 * @brief The members of platform_versions_filter, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const platform_versions_filter&) {
    return std::tuple{
        utility::codec::field{"version", &platform_versions_filter::version},
        utility::codec::field{"from_version", &platform_versions_filter::from_version},
        utility::codec::field{"to_version", &platform_versions_filter::to_version},
    };
}

/**
 * @brief The members of list_platforms_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_platforms_request&) {
    return std::tuple{
        utility::codec::field{"offset", &list_platforms_request::offset},
        utility::codec::field{"limit", &list_platforms_request::limit},
        utility::codec::field{"order", &list_platforms_request::order},
        utility::codec::field{"filter", &list_platforms_request::filter},
        utility::codec::field{"as_of", &list_platforms_request::as_of},
    };
}

/**
 * @brief The members of list_platforms_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_platforms_response&) {
    return std::tuple{
        utility::codec::field{"result", &list_platforms_response::result},
        utility::codec::field{"platforms", &list_platforms_response::platforms},
        utility::codec::field{"total", &list_platforms_response::total},
    };
}

/**
 * @brief The members of get_platform_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_platform_request&) {
    return std::tuple{
        utility::codec::field{"key", &get_platform_request::key},
    };
}

/**
 * @brief The members of get_platform_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_platform_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_platform_response::result},
        utility::codec::field{"platform", &get_platform_response::platform},
    };
}

/**
 * @brief The members of get_many_platforms_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_many_platforms_request&) {
    return std::tuple{
        utility::codec::field{"keys", &get_many_platforms_request::keys},
    };
}

/**
 * @brief The members of get_many_platforms_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_many_platforms_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_many_platforms_response::result},
        utility::codec::field{"entries", &get_many_platforms_response::entries},
    };
}

/**
 * @brief The members of put_platform_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_platform_request&) {
    return std::tuple{
        utility::codec::field{"change", &put_platform_request::change},
        utility::codec::field{"intent", &put_platform_request::intent},
    };
}

/**
 * @brief The members of put_platform_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_platform_response&) {
    return std::tuple{
        utility::codec::field{"result", &put_platform_response::result},
        utility::codec::field{"platform", &put_platform_response::platform},
    };
}

/**
 * @brief The members of put_many_platforms_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_many_platforms_request&) {
    return std::tuple{
        utility::codec::field{"changes", &put_many_platforms_request::changes},
        utility::codec::field{"intent", &put_many_platforms_request::intent},
    };
}

/**
 * @brief The members of put_many_platforms_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_many_platforms_response&) {
    return std::tuple{
        utility::codec::field{"result", &put_many_platforms_response::result},
        utility::codec::field{"platforms", &put_many_platforms_response::platforms},
    };
}

/**
 * @brief The members of delete_platform_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_platform_request&) {
    return std::tuple{
        utility::codec::field{"removal", &delete_platform_request::removal},
        utility::codec::field{"intent", &delete_platform_request::intent},
    };
}

/**
 * @brief The members of delete_platform_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_platform_response&) {
    return std::tuple{
        utility::codec::field{"result", &delete_platform_response::result},
    };
}

/**
 * @brief The members of delete_many_platforms_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_many_platforms_request&) {
    return std::tuple{
        utility::codec::field{"removals", &delete_many_platforms_request::removals},
        utility::codec::field{"intent", &delete_many_platforms_request::intent},
    };
}

/**
 * @brief The members of delete_many_platforms_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_many_platforms_response&) {
    return std::tuple{
        utility::codec::field{"result", &delete_many_platforms_response::result},
    };
}

/**
 * @brief The members of list_platform_versions_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_platform_versions_request&) {
    return std::tuple{
        utility::codec::field{"key", &list_platform_versions_request::key},
        utility::codec::field{"offset", &list_platform_versions_request::offset},
        utility::codec::field{"limit", &list_platform_versions_request::limit},
        utility::codec::field{"order", &list_platform_versions_request::order},
        utility::codec::field{"filter", &list_platform_versions_request::filter},
    };
}

/**
 * @brief The members of list_platform_versions_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_platform_versions_response&) {
    return std::tuple{
        utility::codec::field{"result", &list_platform_versions_response::result},
        utility::codec::field{"versions", &list_platform_versions_response::versions},
        utility::codec::field{"total", &list_platform_versions_response::total},
    };
}

/**
 * @brief The members of get_platform_version_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_platform_version_request&) {
    return std::tuple{
        utility::codec::field{"key", &get_platform_version_request::key},
    };
}

/**
 * @brief The members of get_platform_version_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_platform_version_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_platform_version_response::result},
        utility::codec::field{"version", &get_platform_version_response::version},
    };
}


}
namespace ores::utility::codec {

template <>
struct ORES_COMPUTE_API_EXPORT wire_traits<ores::compute::messaging::platform_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::compute::messaging::platform_key& v);
    static ores::compute::messaging::platform_key read_json(std::string_view text);
    static std::string write_msgpack(const ores::compute::messaging::platform_key& v);
    static ores::compute::messaging::platform_key read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_COMPUTE_API_EXPORT wire_traits<ores::compute::messaging::platform_write> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::compute::messaging::platform_write& v);
    static ores::compute::messaging::platform_write read_json(std::string_view text);
    static std::string write_msgpack(const ores::compute::messaging::platform_write& v);
    static ores::compute::messaging::platform_write read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_COMPUTE_API_EXPORT wire_traits<ores::compute::messaging::platform_change> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::compute::messaging::platform_change& v);
    static ores::compute::messaging::platform_change read_json(std::string_view text);
    static std::string write_msgpack(const ores::compute::messaging::platform_change& v);
    static ores::compute::messaging::platform_change read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_COMPUTE_API_EXPORT wire_traits<ores::compute::messaging::platform_removal> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::compute::messaging::platform_removal& v);
    static ores::compute::messaging::platform_removal read_json(std::string_view text);
    static std::string write_msgpack(const ores::compute::messaging::platform_removal& v);
    static ores::compute::messaging::platform_removal read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_COMPUTE_API_EXPORT wire_traits<ores::compute::messaging::platform_lookup> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::compute::messaging::platform_lookup& v);
    static ores::compute::messaging::platform_lookup read_json(std::string_view text);
    static std::string write_msgpack(const ores::compute::messaging::platform_lookup& v);
    static ores::compute::messaging::platform_lookup read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_COMPUTE_API_EXPORT wire_traits<ores::compute::messaging::platforms_filter> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::compute::messaging::platforms_filter& v);
    static ores::compute::messaging::platforms_filter read_json(std::string_view text);
    static std::string write_msgpack(const ores::compute::messaging::platforms_filter& v);
    static ores::compute::messaging::platforms_filter read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_COMPUTE_API_EXPORT wire_traits<ores::compute::messaging::platform_event> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::compute::messaging::platform_event& v);
    static ores::compute::messaging::platform_event read_json(std::string_view text);
    static std::string write_msgpack(const ores::compute::messaging::platform_event& v);
    static ores::compute::messaging::platform_event read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_COMPUTE_API_EXPORT wire_traits<ores::compute::messaging::platform_version_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::compute::messaging::platform_version_key& v);
    static ores::compute::messaging::platform_version_key read_json(std::string_view text);
    static std::string write_msgpack(const ores::compute::messaging::platform_version_key& v);
    static ores::compute::messaging::platform_version_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_COMPUTE_API_EXPORT wire_traits<ores::compute::messaging::platform_versions_filter> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::compute::messaging::platform_versions_filter& v);
    static ores::compute::messaging::platform_versions_filter read_json(std::string_view text);
    static std::string write_msgpack(const ores::compute::messaging::platform_versions_filter& v);
    static ores::compute::messaging::platform_versions_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_COMPUTE_API_EXPORT wire_traits<ores::compute::messaging::list_platforms_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::compute::messaging::list_platforms_request& v);
    static ores::compute::messaging::list_platforms_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::compute::messaging::list_platforms_request& v);
    static ores::compute::messaging::list_platforms_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_COMPUTE_API_EXPORT wire_traits<ores::compute::messaging::list_platforms_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::compute::messaging::list_platforms_response& v);
    static ores::compute::messaging::list_platforms_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::compute::messaging::list_platforms_response& v);
    static ores::compute::messaging::list_platforms_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_COMPUTE_API_EXPORT wire_traits<ores::compute::messaging::get_platform_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::compute::messaging::get_platform_request& v);
    static ores::compute::messaging::get_platform_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::compute::messaging::get_platform_request& v);
    static ores::compute::messaging::get_platform_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_COMPUTE_API_EXPORT wire_traits<ores::compute::messaging::get_platform_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::compute::messaging::get_platform_response& v);
    static ores::compute::messaging::get_platform_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::compute::messaging::get_platform_response& v);
    static ores::compute::messaging::get_platform_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_COMPUTE_API_EXPORT wire_traits<ores::compute::messaging::get_many_platforms_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::compute::messaging::get_many_platforms_request& v);
    static ores::compute::messaging::get_many_platforms_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::compute::messaging::get_many_platforms_request& v);
    static ores::compute::messaging::get_many_platforms_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_COMPUTE_API_EXPORT wire_traits<ores::compute::messaging::get_many_platforms_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::compute::messaging::get_many_platforms_response& v);
    static ores::compute::messaging::get_many_platforms_response read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::compute::messaging::get_many_platforms_response& v);
    static ores::compute::messaging::get_many_platforms_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_COMPUTE_API_EXPORT wire_traits<ores::compute::messaging::put_platform_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::compute::messaging::put_platform_request& v);
    static ores::compute::messaging::put_platform_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::compute::messaging::put_platform_request& v);
    static ores::compute::messaging::put_platform_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_COMPUTE_API_EXPORT wire_traits<ores::compute::messaging::put_platform_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::compute::messaging::put_platform_response& v);
    static ores::compute::messaging::put_platform_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::compute::messaging::put_platform_response& v);
    static ores::compute::messaging::put_platform_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_COMPUTE_API_EXPORT wire_traits<ores::compute::messaging::put_many_platforms_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::compute::messaging::put_many_platforms_request& v);
    static ores::compute::messaging::put_many_platforms_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::compute::messaging::put_many_platforms_request& v);
    static ores::compute::messaging::put_many_platforms_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_COMPUTE_API_EXPORT wire_traits<ores::compute::messaging::put_many_platforms_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::compute::messaging::put_many_platforms_response& v);
    static ores::compute::messaging::put_many_platforms_response read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::compute::messaging::put_many_platforms_response& v);
    static ores::compute::messaging::put_many_platforms_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_COMPUTE_API_EXPORT wire_traits<ores::compute::messaging::delete_platform_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::compute::messaging::delete_platform_request& v);
    static ores::compute::messaging::delete_platform_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::compute::messaging::delete_platform_request& v);
    static ores::compute::messaging::delete_platform_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_COMPUTE_API_EXPORT wire_traits<ores::compute::messaging::delete_platform_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::compute::messaging::delete_platform_response& v);
    static ores::compute::messaging::delete_platform_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::compute::messaging::delete_platform_response& v);
    static ores::compute::messaging::delete_platform_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_COMPUTE_API_EXPORT
    wire_traits<ores::compute::messaging::delete_many_platforms_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::compute::messaging::delete_many_platforms_request& v);
    static ores::compute::messaging::delete_many_platforms_request read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::compute::messaging::delete_many_platforms_request& v);
    static ores::compute::messaging::delete_many_platforms_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_COMPUTE_API_EXPORT
    wire_traits<ores::compute::messaging::delete_many_platforms_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::compute::messaging::delete_many_platforms_response& v);
    static ores::compute::messaging::delete_many_platforms_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::compute::messaging::delete_many_platforms_response& v);
    static ores::compute::messaging::delete_many_platforms_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_COMPUTE_API_EXPORT
    wire_traits<ores::compute::messaging::list_platform_versions_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::compute::messaging::list_platform_versions_request& v);
    static ores::compute::messaging::list_platform_versions_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::compute::messaging::list_platform_versions_request& v);
    static ores::compute::messaging::list_platform_versions_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_COMPUTE_API_EXPORT
    wire_traits<ores::compute::messaging::list_platform_versions_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::compute::messaging::list_platform_versions_response& v);
    static ores::compute::messaging::list_platform_versions_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::compute::messaging::list_platform_versions_response& v);
    static ores::compute::messaging::list_platform_versions_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_COMPUTE_API_EXPORT wire_traits<ores::compute::messaging::get_platform_version_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::compute::messaging::get_platform_version_request& v);
    static ores::compute::messaging::get_platform_version_request read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::compute::messaging::get_platform_version_request& v);
    static ores::compute::messaging::get_platform_version_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_COMPUTE_API_EXPORT
    wire_traits<ores::compute::messaging::get_platform_version_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::compute::messaging::get_platform_version_response& v);
    static ores::compute::messaging::get_platform_version_response read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::compute::messaging::get_platform_version_response& v);
    static ores::compute::messaging::get_platform_version_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
