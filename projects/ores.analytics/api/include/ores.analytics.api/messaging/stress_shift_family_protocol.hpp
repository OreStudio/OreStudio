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
#ifndef ORES_ANALYTICS_API_MESSAGING_STRESS_SHIFT_FAMILY_PROTOCOL_HPP
#define ORES_ANALYTICS_API_MESSAGING_STRESS_SHIFT_FAMILY_PROTOCOL_HPP

#include "ores.analytics.api/domain/stress_shift_family.hpp"
#include "ores.analytics.api/export.hpp"
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

namespace ores::analytics::messaging {

struct stress_shift_family_key {
    std::string code;

    friend bool operator==(const stress_shift_family_key&,
                           const stress_shift_family_key&) = default;
};

struct stress_shift_family_write {
    std::string code;
    std::string description;

    friend bool operator==(const stress_shift_family_write&,
                           const stress_shift_family_write&) = default;
};

struct stress_shift_family_change {
    stress_shift_family_write write;
    ores::utility::domain::precondition precondition;

    friend bool operator==(const stress_shift_family_change&,
                           const stress_shift_family_change&) = default;
};

struct stress_shift_family_removal {
    stress_shift_family_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;

    friend bool operator==(const stress_shift_family_removal&,
                           const stress_shift_family_removal&) = default;
};

struct stress_shift_family_lookup {
    stress_shift_family_key key;
    std::optional<ores::analytics::domain::stress_shift_family> stress_shift_family;

    friend bool operator==(const stress_shift_family_lookup&,
                           const stress_shift_family_lookup&) = default;
};

struct stress_shift_families_filter {
    std::optional<std::vector<std::string>> code_one_of;

    friend bool operator==(const stress_shift_families_filter&,
                           const stress_shift_families_filter&) = default;
};

struct stress_shift_family_event {
    boost::uuids::uuid event_id;
    stress_shift_family_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;

    friend bool operator==(const stress_shift_family_event&,
                           const stress_shift_family_event&) = default;
};

struct stress_shift_family_version_key {
    stress_shift_family_key stress_shift_family;
    std::uint32_t version;

    friend bool operator==(const stress_shift_family_version_key&,
                           const stress_shift_family_version_key&) = default;
};

struct stress_shift_family_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;

    friend bool operator==(const stress_shift_family_versions_filter&,
                           const stress_shift_family_versions_filter&) = default;
};

struct list_stress_shift_families_request {
    using response_type = struct list_stress_shift_families_response;
    static constexpr std::string_view nats_subject = "analytics.v1.stress_shift_families.list";
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
    std::optional<stress_shift_families_filter> filter;
    std::optional<std::string> as_of;

    friend bool operator==(const list_stress_shift_families_request&,
                           const list_stress_shift_families_request&) = default;
};

struct list_stress_shift_families_response {
    ores::utility::domain::result result;
    std::vector<ores::analytics::domain::stress_shift_family> families;
    std::uint64_t total;

    friend bool operator==(const list_stress_shift_families_response&,
                           const list_stress_shift_families_response&) = default;
};

struct get_stress_shift_family_request {
    using response_type = struct get_stress_shift_family_response;
    static constexpr std::string_view nats_subject = "analytics.v1.stress_shift_families.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    stress_shift_family_key key;

    friend bool operator==(const get_stress_shift_family_request&,
                           const get_stress_shift_family_request&) = default;
};

struct get_stress_shift_family_response {
    ores::utility::domain::result result;
    std::optional<ores::analytics::domain::stress_shift_family> stress_shift_family;

    friend bool operator==(const get_stress_shift_family_response&,
                           const get_stress_shift_family_response&) = default;
};

struct get_many_stress_shift_families_request {
    using response_type = struct get_many_stress_shift_families_response;
    static constexpr std::string_view nats_subject = "analytics.v1.stress_shift_families.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<stress_shift_family_key> keys;

    friend bool operator==(const get_many_stress_shift_families_request&,
                           const get_many_stress_shift_families_request&) = default;
};

struct get_many_stress_shift_families_response {
    ores::utility::domain::result result;
    std::vector<stress_shift_family_lookup> entries;

    friend bool operator==(const get_many_stress_shift_families_response&,
                           const get_many_stress_shift_families_response&) = default;
};

struct put_stress_shift_family_request {
    using response_type = struct put_stress_shift_family_response;
    static constexpr std::string_view nats_subject = "analytics.v1.stress_shift_families.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    stress_shift_family_change change;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const put_stress_shift_family_request&,
                           const put_stress_shift_family_request&) = default;
};

struct put_stress_shift_family_response {
    ores::utility::domain::result result;
    std::optional<ores::analytics::domain::stress_shift_family> stress_shift_family;

    friend bool operator==(const put_stress_shift_family_response&,
                           const put_stress_shift_family_response&) = default;
};

struct put_many_stress_shift_families_request {
    using response_type = struct put_many_stress_shift_families_response;
    static constexpr std::string_view nats_subject = "analytics.v1.stress_shift_families.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<stress_shift_family_change> changes;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const put_many_stress_shift_families_request&,
                           const put_many_stress_shift_families_request&) = default;
};

struct put_many_stress_shift_families_response {
    ores::utility::domain::result result;
    std::vector<ores::analytics::domain::stress_shift_family> families;

    friend bool operator==(const put_many_stress_shift_families_response&,
                           const put_many_stress_shift_families_response&) = default;
};

struct delete_stress_shift_family_request {
    using response_type = struct delete_stress_shift_family_response;
    static constexpr std::string_view nats_subject = "analytics.v1.stress_shift_families.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    stress_shift_family_removal removal;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const delete_stress_shift_family_request&,
                           const delete_stress_shift_family_request&) = default;
};

struct delete_stress_shift_family_response {
    ores::utility::domain::result result;

    friend bool operator==(const delete_stress_shift_family_response&,
                           const delete_stress_shift_family_response&) = default;
};

struct delete_many_stress_shift_families_request {
    using response_type = struct delete_many_stress_shift_families_response;
    static constexpr std::string_view nats_subject =
        "analytics.v1.stress_shift_families.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<stress_shift_family_removal> removals;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const delete_many_stress_shift_families_request&,
                           const delete_many_stress_shift_families_request&) = default;
};

struct delete_many_stress_shift_families_response {
    ores::utility::domain::result result;

    friend bool operator==(const delete_many_stress_shift_families_response&,
                           const delete_many_stress_shift_families_response&) = default;
};

struct list_stress_shift_family_versions_request {
    using response_type = struct list_stress_shift_family_versions_response;
    static constexpr std::string_view nats_subject =
        "analytics.v1.stress_shift_families_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    stress_shift_family_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<stress_shift_family_versions_filter> filter;

    friend bool operator==(const list_stress_shift_family_versions_request&,
                           const list_stress_shift_family_versions_request&) = default;
};

struct list_stress_shift_family_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::analytics::domain::stress_shift_family> versions;
    std::uint64_t total;

    friend bool operator==(const list_stress_shift_family_versions_response&,
                           const list_stress_shift_family_versions_response&) = default;
};

struct get_stress_shift_family_version_request {
    using response_type = struct get_stress_shift_family_version_response;
    static constexpr std::string_view nats_subject =
        "analytics.v1.stress_shift_families_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    stress_shift_family_version_key key;

    friend bool operator==(const get_stress_shift_family_version_request&,
                           const get_stress_shift_family_version_request&) = default;
};

struct get_stress_shift_family_version_response {
    ores::utility::domain::result result;
    std::optional<ores::analytics::domain::stress_shift_family> version;

    friend bool operator==(const get_stress_shift_family_version_response&,
                           const get_stress_shift_family_version_response&) = default;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace stress_shift_family_event_subjects {
inline constexpr std::string_view created = "analytics.v1.events.stress_shift_families.created";
inline constexpr std::string_view updated = "analytics.v1.events.stress_shift_families.updated";
inline constexpr std::string_view deleted = "analytics.v1.events.stress_shift_families.deleted";
}
/**
 * @brief The members of stress_shift_family_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<stress_shift_family_key>) {
    return std::tuple{
        utility::codec::field{"code", &stress_shift_family_key::code},
    };
}

/**
 * @brief The members of stress_shift_family_write, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<stress_shift_family_write>) {
    return std::tuple{
        utility::codec::field{"code", &stress_shift_family_write::code},
        utility::codec::field{"description", &stress_shift_family_write::description},
    };
}

/**
 * @brief The members of stress_shift_family_change, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<stress_shift_family_change>) {
    return std::tuple{
        utility::codec::field{"write", &stress_shift_family_change::write},
        utility::codec::field{"precondition", &stress_shift_family_change::precondition},
    };
}

/**
 * @brief The members of stress_shift_family_removal, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<stress_shift_family_removal>) {
    return std::tuple{
        utility::codec::field{"key", &stress_shift_family_removal::key},
        utility::codec::field{"precondition", &stress_shift_family_removal::precondition},
    };
}

/**
 * @brief The members of stress_shift_family_lookup, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<stress_shift_family_lookup>) {
    return std::tuple{
        utility::codec::field{"key", &stress_shift_family_lookup::key},
        utility::codec::field{"stress_shift_family",
                              &stress_shift_family_lookup::stress_shift_family},
    };
}

/**
 * @brief The members of stress_shift_families_filter, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<stress_shift_families_filter>) {
    return std::tuple{
        utility::codec::field{"code_one_of", &stress_shift_families_filter::code_one_of},
    };
}

/**
 * @brief The members of stress_shift_family_event, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<stress_shift_family_event>) {
    return std::tuple{
        utility::codec::field{"event_id", &stress_shift_family_event::event_id},
        utility::codec::field{"key", &stress_shift_family_event::key},
        utility::codec::field{"action", &stress_shift_family_event::action},
        utility::codec::field{"version", &stress_shift_family_event::version},
        utility::codec::field{"occurred_at", &stress_shift_family_event::occurred_at},
        utility::codec::field{"correlation_id", &stress_shift_family_event::correlation_id},
    };
}

/**
 * @brief The members of stress_shift_family_version_key, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<stress_shift_family_version_key>) {
    return std::tuple{
        utility::codec::field{"stress_shift_family",
                              &stress_shift_family_version_key::stress_shift_family},
        utility::codec::field{"version", &stress_shift_family_version_key::version},
    };
}

/**
 * @brief The members of stress_shift_family_versions_filter, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<stress_shift_family_versions_filter>) {
    return std::tuple{
        utility::codec::field{"version", &stress_shift_family_versions_filter::version},
        utility::codec::field{"from_version", &stress_shift_family_versions_filter::from_version},
        utility::codec::field{"to_version", &stress_shift_family_versions_filter::to_version},
    };
}

/**
 * @brief The members of list_stress_shift_families_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<list_stress_shift_families_request>) {
    return std::tuple{
        utility::codec::field{"offset", &list_stress_shift_families_request::offset},
        utility::codec::field{"limit", &list_stress_shift_families_request::limit},
        utility::codec::field{"order", &list_stress_shift_families_request::order},
        utility::codec::field{"filter", &list_stress_shift_families_request::filter},
        utility::codec::field{"as_of", &list_stress_shift_families_request::as_of},
    };
}

/**
 * @brief The members of list_stress_shift_families_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<list_stress_shift_families_response>) {
    return std::tuple{
        utility::codec::field{"result", &list_stress_shift_families_response::result},
        utility::codec::field{"families", &list_stress_shift_families_response::families},
        utility::codec::field{"total", &list_stress_shift_families_response::total},
    };
}

/**
 * @brief The members of get_stress_shift_family_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_stress_shift_family_request>) {
    return std::tuple{
        utility::codec::field{"key", &get_stress_shift_family_request::key},
    };
}

/**
 * @brief The members of get_stress_shift_family_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_stress_shift_family_response>) {
    return std::tuple{
        utility::codec::field{"result", &get_stress_shift_family_response::result},
        utility::codec::field{"stress_shift_family",
                              &get_stress_shift_family_response::stress_shift_family},
    };
}

/**
 * @brief The members of get_many_stress_shift_families_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<get_many_stress_shift_families_request>) {
    return std::tuple{
        utility::codec::field{"keys", &get_many_stress_shift_families_request::keys},
    };
}

/**
 * @brief The members of get_many_stress_shift_families_response, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<get_many_stress_shift_families_response>) {
    return std::tuple{
        utility::codec::field{"result", &get_many_stress_shift_families_response::result},
        utility::codec::field{"entries", &get_many_stress_shift_families_response::entries},
    };
}

/**
 * @brief The members of put_stress_shift_family_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<put_stress_shift_family_request>) {
    return std::tuple{
        utility::codec::field{"change", &put_stress_shift_family_request::change},
        utility::codec::field{"intent", &put_stress_shift_family_request::intent},
    };
}

/**
 * @brief The members of put_stress_shift_family_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<put_stress_shift_family_response>) {
    return std::tuple{
        utility::codec::field{"result", &put_stress_shift_family_response::result},
        utility::codec::field{"stress_shift_family",
                              &put_stress_shift_family_response::stress_shift_family},
    };
}

/**
 * @brief The members of put_many_stress_shift_families_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<put_many_stress_shift_families_request>) {
    return std::tuple{
        utility::codec::field{"changes", &put_many_stress_shift_families_request::changes},
        utility::codec::field{"intent", &put_many_stress_shift_families_request::intent},
    };
}

/**
 * @brief The members of put_many_stress_shift_families_response, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<put_many_stress_shift_families_response>) {
    return std::tuple{
        utility::codec::field{"result", &put_many_stress_shift_families_response::result},
        utility::codec::field{"families", &put_many_stress_shift_families_response::families},
    };
}

/**
 * @brief The members of delete_stress_shift_family_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<delete_stress_shift_family_request>) {
    return std::tuple{
        utility::codec::field{"removal", &delete_stress_shift_family_request::removal},
        utility::codec::field{"intent", &delete_stress_shift_family_request::intent},
    };
}

/**
 * @brief The members of delete_stress_shift_family_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<delete_stress_shift_family_response>) {
    return std::tuple{
        utility::codec::field{"result", &delete_stress_shift_family_response::result},
    };
}

/**
 * @brief The members of delete_many_stress_shift_families_request, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<delete_many_stress_shift_families_request>) {
    return std::tuple{
        utility::codec::field{"removals", &delete_many_stress_shift_families_request::removals},
        utility::codec::field{"intent", &delete_many_stress_shift_families_request::intent},
    };
}

/**
 * @brief The members of delete_many_stress_shift_families_response, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<delete_many_stress_shift_families_response>) {
    return std::tuple{
        utility::codec::field{"result", &delete_many_stress_shift_families_response::result},
    };
}

/**
 * @brief The members of list_stress_shift_family_versions_request, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<list_stress_shift_family_versions_request>) {
    return std::tuple{
        utility::codec::field{"key", &list_stress_shift_family_versions_request::key},
        utility::codec::field{"offset", &list_stress_shift_family_versions_request::offset},
        utility::codec::field{"limit", &list_stress_shift_family_versions_request::limit},
        utility::codec::field{"order", &list_stress_shift_family_versions_request::order},
        utility::codec::field{"filter", &list_stress_shift_family_versions_request::filter},
    };
}

/**
 * @brief The members of list_stress_shift_family_versions_response, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<list_stress_shift_family_versions_response>) {
    return std::tuple{
        utility::codec::field{"result", &list_stress_shift_family_versions_response::result},
        utility::codec::field{"versions", &list_stress_shift_family_versions_response::versions},
        utility::codec::field{"total", &list_stress_shift_family_versions_response::total},
    };
}

/**
 * @brief The members of get_stress_shift_family_version_request, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<get_stress_shift_family_version_request>) {
    return std::tuple{
        utility::codec::field{"key", &get_stress_shift_family_version_request::key},
    };
}

/**
 * @brief The members of get_stress_shift_family_version_response, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<get_stress_shift_family_version_response>) {
    return std::tuple{
        utility::codec::field{"result", &get_stress_shift_family_version_response::result},
        utility::codec::field{"version", &get_stress_shift_family_version_response::version},
    };
}


}
namespace ores::utility::codec {

template <>
struct ORES_ANALYTICS_API_EXPORT wire_traits<ores::analytics::messaging::stress_shift_family_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::analytics::messaging::stress_shift_family_key& v);
    static ores::analytics::messaging::stress_shift_family_key read_json(std::string_view text);
    static std::string write_msgpack(const ores::analytics::messaging::stress_shift_family_key& v);
    static ores::analytics::messaging::stress_shift_family_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ANALYTICS_API_EXPORT
    wire_traits<ores::analytics::messaging::stress_shift_family_write> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::analytics::messaging::stress_shift_family_write& v);
    static ores::analytics::messaging::stress_shift_family_write read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::analytics::messaging::stress_shift_family_write& v);
    static ores::analytics::messaging::stress_shift_family_write
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ANALYTICS_API_EXPORT
    wire_traits<ores::analytics::messaging::stress_shift_family_change> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::analytics::messaging::stress_shift_family_change& v);
    static ores::analytics::messaging::stress_shift_family_change read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::analytics::messaging::stress_shift_family_change& v);
    static ores::analytics::messaging::stress_shift_family_change
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ANALYTICS_API_EXPORT
    wire_traits<ores::analytics::messaging::stress_shift_family_removal> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::analytics::messaging::stress_shift_family_removal& v);
    static ores::analytics::messaging::stress_shift_family_removal read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::analytics::messaging::stress_shift_family_removal& v);
    static ores::analytics::messaging::stress_shift_family_removal
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ANALYTICS_API_EXPORT
    wire_traits<ores::analytics::messaging::stress_shift_family_lookup> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::analytics::messaging::stress_shift_family_lookup& v);
    static ores::analytics::messaging::stress_shift_family_lookup read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::analytics::messaging::stress_shift_family_lookup& v);
    static ores::analytics::messaging::stress_shift_family_lookup
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ANALYTICS_API_EXPORT
    wire_traits<ores::analytics::messaging::stress_shift_families_filter> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::analytics::messaging::stress_shift_families_filter& v);
    static ores::analytics::messaging::stress_shift_families_filter
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::analytics::messaging::stress_shift_families_filter& v);
    static ores::analytics::messaging::stress_shift_families_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ANALYTICS_API_EXPORT
    wire_traits<ores::analytics::messaging::stress_shift_family_event> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::analytics::messaging::stress_shift_family_event& v);
    static ores::analytics::messaging::stress_shift_family_event read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::analytics::messaging::stress_shift_family_event& v);
    static ores::analytics::messaging::stress_shift_family_event
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ANALYTICS_API_EXPORT
    wire_traits<ores::analytics::messaging::stress_shift_family_version_key> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::analytics::messaging::stress_shift_family_version_key& v);
    static ores::analytics::messaging::stress_shift_family_version_key
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::analytics::messaging::stress_shift_family_version_key& v);
    static ores::analytics::messaging::stress_shift_family_version_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ANALYTICS_API_EXPORT
    wire_traits<ores::analytics::messaging::stress_shift_family_versions_filter> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::analytics::messaging::stress_shift_family_versions_filter& v);
    static ores::analytics::messaging::stress_shift_family_versions_filter
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::analytics::messaging::stress_shift_family_versions_filter& v);
    static ores::analytics::messaging::stress_shift_family_versions_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ANALYTICS_API_EXPORT
    wire_traits<ores::analytics::messaging::list_stress_shift_families_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::analytics::messaging::list_stress_shift_families_request& v);
    static ores::analytics::messaging::list_stress_shift_families_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::analytics::messaging::list_stress_shift_families_request& v);
    static ores::analytics::messaging::list_stress_shift_families_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ANALYTICS_API_EXPORT
    wire_traits<ores::analytics::messaging::list_stress_shift_families_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::analytics::messaging::list_stress_shift_families_response& v);
    static ores::analytics::messaging::list_stress_shift_families_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::analytics::messaging::list_stress_shift_families_response& v);
    static ores::analytics::messaging::list_stress_shift_families_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ANALYTICS_API_EXPORT
    wire_traits<ores::analytics::messaging::get_stress_shift_family_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::analytics::messaging::get_stress_shift_family_request& v);
    static ores::analytics::messaging::get_stress_shift_family_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::analytics::messaging::get_stress_shift_family_request& v);
    static ores::analytics::messaging::get_stress_shift_family_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ANALYTICS_API_EXPORT
    wire_traits<ores::analytics::messaging::get_stress_shift_family_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::analytics::messaging::get_stress_shift_family_response& v);
    static ores::analytics::messaging::get_stress_shift_family_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::analytics::messaging::get_stress_shift_family_response& v);
    static ores::analytics::messaging::get_stress_shift_family_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ANALYTICS_API_EXPORT
    wire_traits<ores::analytics::messaging::get_many_stress_shift_families_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::analytics::messaging::get_many_stress_shift_families_request& v);
    static ores::analytics::messaging::get_many_stress_shift_families_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::analytics::messaging::get_many_stress_shift_families_request& v);
    static ores::analytics::messaging::get_many_stress_shift_families_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ANALYTICS_API_EXPORT
    wire_traits<ores::analytics::messaging::get_many_stress_shift_families_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::analytics::messaging::get_many_stress_shift_families_response& v);
    static ores::analytics::messaging::get_many_stress_shift_families_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::analytics::messaging::get_many_stress_shift_families_response& v);
    static ores::analytics::messaging::get_many_stress_shift_families_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ANALYTICS_API_EXPORT
    wire_traits<ores::analytics::messaging::put_stress_shift_family_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::analytics::messaging::put_stress_shift_family_request& v);
    static ores::analytics::messaging::put_stress_shift_family_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::analytics::messaging::put_stress_shift_family_request& v);
    static ores::analytics::messaging::put_stress_shift_family_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ANALYTICS_API_EXPORT
    wire_traits<ores::analytics::messaging::put_stress_shift_family_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::analytics::messaging::put_stress_shift_family_response& v);
    static ores::analytics::messaging::put_stress_shift_family_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::analytics::messaging::put_stress_shift_family_response& v);
    static ores::analytics::messaging::put_stress_shift_family_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ANALYTICS_API_EXPORT
    wire_traits<ores::analytics::messaging::put_many_stress_shift_families_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::analytics::messaging::put_many_stress_shift_families_request& v);
    static ores::analytics::messaging::put_many_stress_shift_families_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::analytics::messaging::put_many_stress_shift_families_request& v);
    static ores::analytics::messaging::put_many_stress_shift_families_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ANALYTICS_API_EXPORT
    wire_traits<ores::analytics::messaging::put_many_stress_shift_families_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::analytics::messaging::put_many_stress_shift_families_response& v);
    static ores::analytics::messaging::put_many_stress_shift_families_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::analytics::messaging::put_many_stress_shift_families_response& v);
    static ores::analytics::messaging::put_many_stress_shift_families_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ANALYTICS_API_EXPORT
    wire_traits<ores::analytics::messaging::delete_stress_shift_family_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::analytics::messaging::delete_stress_shift_family_request& v);
    static ores::analytics::messaging::delete_stress_shift_family_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::analytics::messaging::delete_stress_shift_family_request& v);
    static ores::analytics::messaging::delete_stress_shift_family_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ANALYTICS_API_EXPORT
    wire_traits<ores::analytics::messaging::delete_stress_shift_family_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::analytics::messaging::delete_stress_shift_family_response& v);
    static ores::analytics::messaging::delete_stress_shift_family_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::analytics::messaging::delete_stress_shift_family_response& v);
    static ores::analytics::messaging::delete_stress_shift_family_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ANALYTICS_API_EXPORT
    wire_traits<ores::analytics::messaging::delete_many_stress_shift_families_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::analytics::messaging::delete_many_stress_shift_families_request& v);
    static ores::analytics::messaging::delete_many_stress_shift_families_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::analytics::messaging::delete_many_stress_shift_families_request& v);
    static ores::analytics::messaging::delete_many_stress_shift_families_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ANALYTICS_API_EXPORT
    wire_traits<ores::analytics::messaging::delete_many_stress_shift_families_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::analytics::messaging::delete_many_stress_shift_families_response& v);
    static ores::analytics::messaging::delete_many_stress_shift_families_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::analytics::messaging::delete_many_stress_shift_families_response& v);
    static ores::analytics::messaging::delete_many_stress_shift_families_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ANALYTICS_API_EXPORT
    wire_traits<ores::analytics::messaging::list_stress_shift_family_versions_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::analytics::messaging::list_stress_shift_family_versions_request& v);
    static ores::analytics::messaging::list_stress_shift_family_versions_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::analytics::messaging::list_stress_shift_family_versions_request& v);
    static ores::analytics::messaging::list_stress_shift_family_versions_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ANALYTICS_API_EXPORT
    wire_traits<ores::analytics::messaging::list_stress_shift_family_versions_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::analytics::messaging::list_stress_shift_family_versions_response& v);
    static ores::analytics::messaging::list_stress_shift_family_versions_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::analytics::messaging::list_stress_shift_family_versions_response& v);
    static ores::analytics::messaging::list_stress_shift_family_versions_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ANALYTICS_API_EXPORT
    wire_traits<ores::analytics::messaging::get_stress_shift_family_version_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::analytics::messaging::get_stress_shift_family_version_request& v);
    static ores::analytics::messaging::get_stress_shift_family_version_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::analytics::messaging::get_stress_shift_family_version_request& v);
    static ores::analytics::messaging::get_stress_shift_family_version_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ANALYTICS_API_EXPORT
    wire_traits<ores::analytics::messaging::get_stress_shift_family_version_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::analytics::messaging::get_stress_shift_family_version_response& v);
    static ores::analytics::messaging::get_stress_shift_family_version_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::analytics::messaging::get_stress_shift_family_version_response& v);
    static ores::analytics::messaging::get_stress_shift_family_version_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
