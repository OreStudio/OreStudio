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
#ifndef ORES_ANALYTICS_API_MESSAGING_STRESS_TEST_SHIFT_PROTOCOL_HPP
#define ORES_ANALYTICS_API_MESSAGING_STRESS_TEST_SHIFT_PROTOCOL_HPP

#include "ores.analytics.api/domain/stress_test_shift_codec.hpp"
#include "ores.analytics.api/export.hpp"
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

namespace ores::analytics::messaging {

struct stress_test_shift_key {
    boost::uuids::uuid id;

    friend bool operator==(const stress_test_shift_key&, const stress_test_shift_key&) = default;
};

struct stress_test_shift_write {
    boost::uuids::uuid id;
    boost::uuids::uuid stress_test_scenario_id;
    std::string family;
    std::string object_key;
    std::optional<std::string> shift_type;
    std::optional<std::string> shifts;
    std::optional<std::string> shift_tenors;
    std::optional<std::string> shift_expiries;
    std::optional<std::string> extras;
    int position;

    friend bool operator==(const stress_test_shift_write&,
                           const stress_test_shift_write&) = default;
};

struct stress_test_shift_change {
    stress_test_shift_write write;
    ores::utility::domain::precondition precondition;

    friend bool operator==(const stress_test_shift_change&,
                           const stress_test_shift_change&) = default;
};

struct stress_test_shift_removal {
    stress_test_shift_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;

    friend bool operator==(const stress_test_shift_removal&,
                           const stress_test_shift_removal&) = default;
};

struct stress_test_shift_lookup {
    stress_test_shift_key key;
    std::optional<ores::analytics::domain::stress_test_shift> stress_test_shift;

    friend bool operator==(const stress_test_shift_lookup&,
                           const stress_test_shift_lookup&) = default;
};

struct stress_test_shifts_filter {
    std::optional<std::vector<boost::uuids::uuid>> id_one_of;

    friend bool operator==(const stress_test_shifts_filter&,
                           const stress_test_shifts_filter&) = default;
};

struct stress_test_shift_event {
    boost::uuids::uuid event_id;
    stress_test_shift_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;

    friend bool operator==(const stress_test_shift_event&,
                           const stress_test_shift_event&) = default;
};

struct stress_test_shift_version_key {
    stress_test_shift_key stress_test_shift;
    std::uint32_t version;

    friend bool operator==(const stress_test_shift_version_key&,
                           const stress_test_shift_version_key&) = default;
};

struct stress_test_shift_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;

    friend bool operator==(const stress_test_shift_versions_filter&,
                           const stress_test_shift_versions_filter&) = default;
};

struct list_stress_test_shifts_request {
    using response_type = struct list_stress_test_shifts_response;
    static constexpr std::string_view nats_subject = "analytics.v1.stress_test_shifts.list";
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
    std::optional<stress_test_shifts_filter> filter;
    std::optional<std::string> as_of;

    friend bool operator==(const list_stress_test_shifts_request&,
                           const list_stress_test_shifts_request&) = default;
};

struct list_stress_test_shifts_response {
    ores::utility::domain::result result;
    std::vector<ores::analytics::domain::stress_test_shift> stress_test_shifts;
    std::uint64_t total;

    friend bool operator==(const list_stress_test_shifts_response&,
                           const list_stress_test_shifts_response&) = default;
};

struct get_stress_test_shift_request {
    using response_type = struct get_stress_test_shift_response;
    static constexpr std::string_view nats_subject = "analytics.v1.stress_test_shifts.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    stress_test_shift_key key;

    friend bool operator==(const get_stress_test_shift_request&,
                           const get_stress_test_shift_request&) = default;
};

struct get_stress_test_shift_response {
    ores::utility::domain::result result;
    std::optional<ores::analytics::domain::stress_test_shift> stress_test_shift;

    friend bool operator==(const get_stress_test_shift_response&,
                           const get_stress_test_shift_response&) = default;
};

struct get_many_stress_test_shifts_request {
    using response_type = struct get_many_stress_test_shifts_response;
    static constexpr std::string_view nats_subject = "analytics.v1.stress_test_shifts.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<stress_test_shift_key> keys;

    friend bool operator==(const get_many_stress_test_shifts_request&,
                           const get_many_stress_test_shifts_request&) = default;
};

struct get_many_stress_test_shifts_response {
    ores::utility::domain::result result;
    std::vector<stress_test_shift_lookup> entries;

    friend bool operator==(const get_many_stress_test_shifts_response&,
                           const get_many_stress_test_shifts_response&) = default;
};

struct put_stress_test_shift_request {
    using response_type = struct put_stress_test_shift_response;
    static constexpr std::string_view nats_subject = "analytics.v1.stress_test_shifts.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    stress_test_shift_change change;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const put_stress_test_shift_request&,
                           const put_stress_test_shift_request&) = default;
};

struct put_stress_test_shift_response {
    ores::utility::domain::result result;
    std::optional<ores::analytics::domain::stress_test_shift> stress_test_shift;

    friend bool operator==(const put_stress_test_shift_response&,
                           const put_stress_test_shift_response&) = default;
};

struct put_many_stress_test_shifts_request {
    using response_type = struct put_many_stress_test_shifts_response;
    static constexpr std::string_view nats_subject = "analytics.v1.stress_test_shifts.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<stress_test_shift_change> changes;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const put_many_stress_test_shifts_request&,
                           const put_many_stress_test_shifts_request&) = default;
};

struct put_many_stress_test_shifts_response {
    ores::utility::domain::result result;
    std::vector<ores::analytics::domain::stress_test_shift> stress_test_shifts;

    friend bool operator==(const put_many_stress_test_shifts_response&,
                           const put_many_stress_test_shifts_response&) = default;
};

struct delete_stress_test_shift_request {
    using response_type = struct delete_stress_test_shift_response;
    static constexpr std::string_view nats_subject = "analytics.v1.stress_test_shifts.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    stress_test_shift_removal removal;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const delete_stress_test_shift_request&,
                           const delete_stress_test_shift_request&) = default;
};

struct delete_stress_test_shift_response {
    ores::utility::domain::result result;

    friend bool operator==(const delete_stress_test_shift_response&,
                           const delete_stress_test_shift_response&) = default;
};

struct delete_many_stress_test_shifts_request {
    using response_type = struct delete_many_stress_test_shifts_response;
    static constexpr std::string_view nats_subject = "analytics.v1.stress_test_shifts.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<stress_test_shift_removal> removals;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const delete_many_stress_test_shifts_request&,
                           const delete_many_stress_test_shifts_request&) = default;
};

struct delete_many_stress_test_shifts_response {
    ores::utility::domain::result result;

    friend bool operator==(const delete_many_stress_test_shifts_response&,
                           const delete_many_stress_test_shifts_response&) = default;
};

struct list_stress_test_shift_versions_request {
    using response_type = struct list_stress_test_shift_versions_response;
    static constexpr std::string_view nats_subject =
        "analytics.v1.stress_test_shifts_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    stress_test_shift_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<stress_test_shift_versions_filter> filter;

    friend bool operator==(const list_stress_test_shift_versions_request&,
                           const list_stress_test_shift_versions_request&) = default;
};

struct list_stress_test_shift_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::analytics::domain::stress_test_shift> versions;
    std::uint64_t total;

    friend bool operator==(const list_stress_test_shift_versions_response&,
                           const list_stress_test_shift_versions_response&) = default;
};

struct get_stress_test_shift_version_request {
    using response_type = struct get_stress_test_shift_version_response;
    static constexpr std::string_view nats_subject = "analytics.v1.stress_test_shifts_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    stress_test_shift_version_key key;

    friend bool operator==(const get_stress_test_shift_version_request&,
                           const get_stress_test_shift_version_request&) = default;
};

struct get_stress_test_shift_version_response {
    ores::utility::domain::result result;
    std::optional<ores::analytics::domain::stress_test_shift> version;

    friend bool operator==(const get_stress_test_shift_version_response&,
                           const get_stress_test_shift_version_response&) = default;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace stress_test_shift_event_subjects {
inline constexpr std::string_view created = "analytics.v1.events.stress_test_shifts.created";
inline constexpr std::string_view updated = "analytics.v1.events.stress_test_shifts.updated";
inline constexpr std::string_view deleted = "analytics.v1.events.stress_test_shifts.deleted";
}
/**
 * @brief The members of stress_test_shift_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<stress_test_shift_key>) {
    return std::tuple{
        utility::codec::field{"id", &stress_test_shift_key::id},
    };
}

/**
 * @brief The members of stress_test_shift_write, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<stress_test_shift_write>) {
    return std::tuple{
        utility::codec::field{"id", &stress_test_shift_write::id},
        utility::codec::field{"stress_test_scenario_id",
                              &stress_test_shift_write::stress_test_scenario_id},
        utility::codec::field{"family", &stress_test_shift_write::family},
        utility::codec::field{"object_key", &stress_test_shift_write::object_key},
        utility::codec::field{"shift_type", &stress_test_shift_write::shift_type},
        utility::codec::field{"shifts", &stress_test_shift_write::shifts},
        utility::codec::field{"shift_tenors", &stress_test_shift_write::shift_tenors},
        utility::codec::field{"shift_expiries", &stress_test_shift_write::shift_expiries},
        utility::codec::field{"extras", &stress_test_shift_write::extras},
        utility::codec::field{"position", &stress_test_shift_write::position},
    };
}

/**
 * @brief The members of stress_test_shift_change, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<stress_test_shift_change>) {
    return std::tuple{
        utility::codec::field{"write", &stress_test_shift_change::write},
        utility::codec::field{"precondition", &stress_test_shift_change::precondition},
    };
}

/**
 * @brief The members of stress_test_shift_removal, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<stress_test_shift_removal>) {
    return std::tuple{
        utility::codec::field{"key", &stress_test_shift_removal::key},
        utility::codec::field{"precondition", &stress_test_shift_removal::precondition},
    };
}

/**
 * @brief The members of stress_test_shift_lookup, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<stress_test_shift_lookup>) {
    return std::tuple{
        utility::codec::field{"key", &stress_test_shift_lookup::key},
        utility::codec::field{"stress_test_shift", &stress_test_shift_lookup::stress_test_shift},
    };
}

/**
 * @brief The members of stress_test_shifts_filter, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<stress_test_shifts_filter>) {
    return std::tuple{
        utility::codec::field{"id_one_of", &stress_test_shifts_filter::id_one_of},
    };
}

/**
 * @brief The members of stress_test_shift_event, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<stress_test_shift_event>) {
    return std::tuple{
        utility::codec::field{"event_id", &stress_test_shift_event::event_id},
        utility::codec::field{"key", &stress_test_shift_event::key},
        utility::codec::field{"action", &stress_test_shift_event::action},
        utility::codec::field{"version", &stress_test_shift_event::version},
        utility::codec::field{"occurred_at", &stress_test_shift_event::occurred_at},
        utility::codec::field{"correlation_id", &stress_test_shift_event::correlation_id},
    };
}

/**
 * @brief The members of stress_test_shift_version_key, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<stress_test_shift_version_key>) {
    return std::tuple{
        utility::codec::field{"stress_test_shift",
                              &stress_test_shift_version_key::stress_test_shift},
        utility::codec::field{"version", &stress_test_shift_version_key::version},
    };
}

/**
 * @brief The members of stress_test_shift_versions_filter, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<stress_test_shift_versions_filter>) {
    return std::tuple{
        utility::codec::field{"version", &stress_test_shift_versions_filter::version},
        utility::codec::field{"from_version", &stress_test_shift_versions_filter::from_version},
        utility::codec::field{"to_version", &stress_test_shift_versions_filter::to_version},
    };
}

/**
 * @brief The members of list_stress_test_shifts_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<list_stress_test_shifts_request>) {
    return std::tuple{
        utility::codec::field{"offset", &list_stress_test_shifts_request::offset},
        utility::codec::field{"limit", &list_stress_test_shifts_request::limit},
        utility::codec::field{"order", &list_stress_test_shifts_request::order},
        utility::codec::field{"filter", &list_stress_test_shifts_request::filter},
        utility::codec::field{"as_of", &list_stress_test_shifts_request::as_of},
    };
}

/**
 * @brief The members of list_stress_test_shifts_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<list_stress_test_shifts_response>) {
    return std::tuple{
        utility::codec::field{"result", &list_stress_test_shifts_response::result},
        utility::codec::field{"stress_test_shifts",
                              &list_stress_test_shifts_response::stress_test_shifts},
        utility::codec::field{"total", &list_stress_test_shifts_response::total},
    };
}

/**
 * @brief The members of get_stress_test_shift_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_stress_test_shift_request>) {
    return std::tuple{
        utility::codec::field{"key", &get_stress_test_shift_request::key},
    };
}

/**
 * @brief The members of get_stress_test_shift_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_stress_test_shift_response>) {
    return std::tuple{
        utility::codec::field{"result", &get_stress_test_shift_response::result},
        utility::codec::field{"stress_test_shift",
                              &get_stress_test_shift_response::stress_test_shift},
    };
}

/**
 * @brief The members of get_many_stress_test_shifts_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_many_stress_test_shifts_request>) {
    return std::tuple{
        utility::codec::field{"keys", &get_many_stress_test_shifts_request::keys},
    };
}

/**
 * @brief The members of get_many_stress_test_shifts_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_many_stress_test_shifts_response>) {
    return std::tuple{
        utility::codec::field{"result", &get_many_stress_test_shifts_response::result},
        utility::codec::field{"entries", &get_many_stress_test_shifts_response::entries},
    };
}

/**
 * @brief The members of put_stress_test_shift_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<put_stress_test_shift_request>) {
    return std::tuple{
        utility::codec::field{"change", &put_stress_test_shift_request::change},
        utility::codec::field{"intent", &put_stress_test_shift_request::intent},
    };
}

/**
 * @brief The members of put_stress_test_shift_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<put_stress_test_shift_response>) {
    return std::tuple{
        utility::codec::field{"result", &put_stress_test_shift_response::result},
        utility::codec::field{"stress_test_shift",
                              &put_stress_test_shift_response::stress_test_shift},
    };
}

/**
 * @brief The members of put_many_stress_test_shifts_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<put_many_stress_test_shifts_request>) {
    return std::tuple{
        utility::codec::field{"changes", &put_many_stress_test_shifts_request::changes},
        utility::codec::field{"intent", &put_many_stress_test_shifts_request::intent},
    };
}

/**
 * @brief The members of put_many_stress_test_shifts_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<put_many_stress_test_shifts_response>) {
    return std::tuple{
        utility::codec::field{"result", &put_many_stress_test_shifts_response::result},
        utility::codec::field{"stress_test_shifts",
                              &put_many_stress_test_shifts_response::stress_test_shifts},
    };
}

/**
 * @brief The members of delete_stress_test_shift_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<delete_stress_test_shift_request>) {
    return std::tuple{
        utility::codec::field{"removal", &delete_stress_test_shift_request::removal},
        utility::codec::field{"intent", &delete_stress_test_shift_request::intent},
    };
}

/**
 * @brief The members of delete_stress_test_shift_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<delete_stress_test_shift_response>) {
    return std::tuple{
        utility::codec::field{"result", &delete_stress_test_shift_response::result},
    };
}

/**
 * @brief The members of delete_many_stress_test_shifts_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<delete_many_stress_test_shifts_request>) {
    return std::tuple{
        utility::codec::field{"removals", &delete_many_stress_test_shifts_request::removals},
        utility::codec::field{"intent", &delete_many_stress_test_shifts_request::intent},
    };
}

/**
 * @brief The members of delete_many_stress_test_shifts_response, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<delete_many_stress_test_shifts_response>) {
    return std::tuple{
        utility::codec::field{"result", &delete_many_stress_test_shifts_response::result},
    };
}

/**
 * @brief The members of list_stress_test_shift_versions_request, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<list_stress_test_shift_versions_request>) {
    return std::tuple{
        utility::codec::field{"key", &list_stress_test_shift_versions_request::key},
        utility::codec::field{"offset", &list_stress_test_shift_versions_request::offset},
        utility::codec::field{"limit", &list_stress_test_shift_versions_request::limit},
        utility::codec::field{"order", &list_stress_test_shift_versions_request::order},
        utility::codec::field{"filter", &list_stress_test_shift_versions_request::filter},
    };
}

/**
 * @brief The members of list_stress_test_shift_versions_response, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<list_stress_test_shift_versions_response>) {
    return std::tuple{
        utility::codec::field{"result", &list_stress_test_shift_versions_response::result},
        utility::codec::field{"versions", &list_stress_test_shift_versions_response::versions},
        utility::codec::field{"total", &list_stress_test_shift_versions_response::total},
    };
}

/**
 * @brief The members of get_stress_test_shift_version_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<get_stress_test_shift_version_request>) {
    return std::tuple{
        utility::codec::field{"key", &get_stress_test_shift_version_request::key},
    };
}

/**
 * @brief The members of get_stress_test_shift_version_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<get_stress_test_shift_version_response>) {
    return std::tuple{
        utility::codec::field{"result", &get_stress_test_shift_version_response::result},
        utility::codec::field{"version", &get_stress_test_shift_version_response::version},
    };
}


}
namespace ores::utility::codec {

template <>
struct ORES_ANALYTICS_API_EXPORT wire_traits<ores::analytics::messaging::stress_test_shift_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::analytics::messaging::stress_test_shift_key& v);
    static ores::analytics::messaging::stress_test_shift_key read_json(std::string_view text);
    static std::string write_msgpack(const ores::analytics::messaging::stress_test_shift_key& v);
    static ores::analytics::messaging::stress_test_shift_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ANALYTICS_API_EXPORT wire_traits<ores::analytics::messaging::stress_test_shift_write> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::analytics::messaging::stress_test_shift_write& v);
    static ores::analytics::messaging::stress_test_shift_write read_json(std::string_view text);
    static std::string write_msgpack(const ores::analytics::messaging::stress_test_shift_write& v);
    static ores::analytics::messaging::stress_test_shift_write
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ANALYTICS_API_EXPORT wire_traits<ores::analytics::messaging::stress_test_shift_change> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::analytics::messaging::stress_test_shift_change& v);
    static ores::analytics::messaging::stress_test_shift_change read_json(std::string_view text);
    static std::string write_msgpack(const ores::analytics::messaging::stress_test_shift_change& v);
    static ores::analytics::messaging::stress_test_shift_change
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ANALYTICS_API_EXPORT
    wire_traits<ores::analytics::messaging::stress_test_shift_removal> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::analytics::messaging::stress_test_shift_removal& v);
    static ores::analytics::messaging::stress_test_shift_removal read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::analytics::messaging::stress_test_shift_removal& v);
    static ores::analytics::messaging::stress_test_shift_removal
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ANALYTICS_API_EXPORT wire_traits<ores::analytics::messaging::stress_test_shift_lookup> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::analytics::messaging::stress_test_shift_lookup& v);
    static ores::analytics::messaging::stress_test_shift_lookup read_json(std::string_view text);
    static std::string write_msgpack(const ores::analytics::messaging::stress_test_shift_lookup& v);
    static ores::analytics::messaging::stress_test_shift_lookup
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ANALYTICS_API_EXPORT
    wire_traits<ores::analytics::messaging::stress_test_shifts_filter> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::analytics::messaging::stress_test_shifts_filter& v);
    static ores::analytics::messaging::stress_test_shifts_filter read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::analytics::messaging::stress_test_shifts_filter& v);
    static ores::analytics::messaging::stress_test_shifts_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ANALYTICS_API_EXPORT wire_traits<ores::analytics::messaging::stress_test_shift_event> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::analytics::messaging::stress_test_shift_event& v);
    static ores::analytics::messaging::stress_test_shift_event read_json(std::string_view text);
    static std::string write_msgpack(const ores::analytics::messaging::stress_test_shift_event& v);
    static ores::analytics::messaging::stress_test_shift_event
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ANALYTICS_API_EXPORT
    wire_traits<ores::analytics::messaging::stress_test_shift_version_key> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::analytics::messaging::stress_test_shift_version_key& v);
    static ores::analytics::messaging::stress_test_shift_version_key
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::analytics::messaging::stress_test_shift_version_key& v);
    static ores::analytics::messaging::stress_test_shift_version_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ANALYTICS_API_EXPORT
    wire_traits<ores::analytics::messaging::stress_test_shift_versions_filter> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::analytics::messaging::stress_test_shift_versions_filter& v);
    static ores::analytics::messaging::stress_test_shift_versions_filter
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::analytics::messaging::stress_test_shift_versions_filter& v);
    static ores::analytics::messaging::stress_test_shift_versions_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ANALYTICS_API_EXPORT
    wire_traits<ores::analytics::messaging::list_stress_test_shifts_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::analytics::messaging::list_stress_test_shifts_request& v);
    static ores::analytics::messaging::list_stress_test_shifts_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::analytics::messaging::list_stress_test_shifts_request& v);
    static ores::analytics::messaging::list_stress_test_shifts_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ANALYTICS_API_EXPORT
    wire_traits<ores::analytics::messaging::list_stress_test_shifts_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::analytics::messaging::list_stress_test_shifts_response& v);
    static ores::analytics::messaging::list_stress_test_shifts_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::analytics::messaging::list_stress_test_shifts_response& v);
    static ores::analytics::messaging::list_stress_test_shifts_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ANALYTICS_API_EXPORT
    wire_traits<ores::analytics::messaging::get_stress_test_shift_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::analytics::messaging::get_stress_test_shift_request& v);
    static ores::analytics::messaging::get_stress_test_shift_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::analytics::messaging::get_stress_test_shift_request& v);
    static ores::analytics::messaging::get_stress_test_shift_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ANALYTICS_API_EXPORT
    wire_traits<ores::analytics::messaging::get_stress_test_shift_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::analytics::messaging::get_stress_test_shift_response& v);
    static ores::analytics::messaging::get_stress_test_shift_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::analytics::messaging::get_stress_test_shift_response& v);
    static ores::analytics::messaging::get_stress_test_shift_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ANALYTICS_API_EXPORT
    wire_traits<ores::analytics::messaging::get_many_stress_test_shifts_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::analytics::messaging::get_many_stress_test_shifts_request& v);
    static ores::analytics::messaging::get_many_stress_test_shifts_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::analytics::messaging::get_many_stress_test_shifts_request& v);
    static ores::analytics::messaging::get_many_stress_test_shifts_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ANALYTICS_API_EXPORT
    wire_traits<ores::analytics::messaging::get_many_stress_test_shifts_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::analytics::messaging::get_many_stress_test_shifts_response& v);
    static ores::analytics::messaging::get_many_stress_test_shifts_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::analytics::messaging::get_many_stress_test_shifts_response& v);
    static ores::analytics::messaging::get_many_stress_test_shifts_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ANALYTICS_API_EXPORT
    wire_traits<ores::analytics::messaging::put_stress_test_shift_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::analytics::messaging::put_stress_test_shift_request& v);
    static ores::analytics::messaging::put_stress_test_shift_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::analytics::messaging::put_stress_test_shift_request& v);
    static ores::analytics::messaging::put_stress_test_shift_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ANALYTICS_API_EXPORT
    wire_traits<ores::analytics::messaging::put_stress_test_shift_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::analytics::messaging::put_stress_test_shift_response& v);
    static ores::analytics::messaging::put_stress_test_shift_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::analytics::messaging::put_stress_test_shift_response& v);
    static ores::analytics::messaging::put_stress_test_shift_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ANALYTICS_API_EXPORT
    wire_traits<ores::analytics::messaging::put_many_stress_test_shifts_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::analytics::messaging::put_many_stress_test_shifts_request& v);
    static ores::analytics::messaging::put_many_stress_test_shifts_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::analytics::messaging::put_many_stress_test_shifts_request& v);
    static ores::analytics::messaging::put_many_stress_test_shifts_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ANALYTICS_API_EXPORT
    wire_traits<ores::analytics::messaging::put_many_stress_test_shifts_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::analytics::messaging::put_many_stress_test_shifts_response& v);
    static ores::analytics::messaging::put_many_stress_test_shifts_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::analytics::messaging::put_many_stress_test_shifts_response& v);
    static ores::analytics::messaging::put_many_stress_test_shifts_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ANALYTICS_API_EXPORT
    wire_traits<ores::analytics::messaging::delete_stress_test_shift_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::analytics::messaging::delete_stress_test_shift_request& v);
    static ores::analytics::messaging::delete_stress_test_shift_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::analytics::messaging::delete_stress_test_shift_request& v);
    static ores::analytics::messaging::delete_stress_test_shift_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ANALYTICS_API_EXPORT
    wire_traits<ores::analytics::messaging::delete_stress_test_shift_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::analytics::messaging::delete_stress_test_shift_response& v);
    static ores::analytics::messaging::delete_stress_test_shift_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::analytics::messaging::delete_stress_test_shift_response& v);
    static ores::analytics::messaging::delete_stress_test_shift_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ANALYTICS_API_EXPORT
    wire_traits<ores::analytics::messaging::delete_many_stress_test_shifts_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::analytics::messaging::delete_many_stress_test_shifts_request& v);
    static ores::analytics::messaging::delete_many_stress_test_shifts_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::analytics::messaging::delete_many_stress_test_shifts_request& v);
    static ores::analytics::messaging::delete_many_stress_test_shifts_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ANALYTICS_API_EXPORT
    wire_traits<ores::analytics::messaging::delete_many_stress_test_shifts_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::analytics::messaging::delete_many_stress_test_shifts_response& v);
    static ores::analytics::messaging::delete_many_stress_test_shifts_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::analytics::messaging::delete_many_stress_test_shifts_response& v);
    static ores::analytics::messaging::delete_many_stress_test_shifts_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ANALYTICS_API_EXPORT
    wire_traits<ores::analytics::messaging::list_stress_test_shift_versions_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::analytics::messaging::list_stress_test_shift_versions_request& v);
    static ores::analytics::messaging::list_stress_test_shift_versions_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::analytics::messaging::list_stress_test_shift_versions_request& v);
    static ores::analytics::messaging::list_stress_test_shift_versions_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ANALYTICS_API_EXPORT
    wire_traits<ores::analytics::messaging::list_stress_test_shift_versions_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::analytics::messaging::list_stress_test_shift_versions_response& v);
    static ores::analytics::messaging::list_stress_test_shift_versions_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::analytics::messaging::list_stress_test_shift_versions_response& v);
    static ores::analytics::messaging::list_stress_test_shift_versions_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ANALYTICS_API_EXPORT
    wire_traits<ores::analytics::messaging::get_stress_test_shift_version_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::analytics::messaging::get_stress_test_shift_version_request& v);
    static ores::analytics::messaging::get_stress_test_shift_version_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::analytics::messaging::get_stress_test_shift_version_request& v);
    static ores::analytics::messaging::get_stress_test_shift_version_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ANALYTICS_API_EXPORT
    wire_traits<ores::analytics::messaging::get_stress_test_shift_version_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::analytics::messaging::get_stress_test_shift_version_response& v);
    static ores::analytics::messaging::get_stress_test_shift_version_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::analytics::messaging::get_stress_test_shift_version_response& v);
    static ores::analytics::messaging::get_stress_test_shift_version_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
