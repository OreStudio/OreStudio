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
#ifndef ORES_REFDATA_API_MESSAGING_CROSS_CURRENCY_BASIS_FLAT_LEG_PROTOCOL_HPP
#define ORES_REFDATA_API_MESSAGING_CROSS_CURRENCY_BASIS_FLAT_LEG_PROTOCOL_HPP

#include "ores.refdata.api/domain/cross_currency_basis_flat_leg_codec.hpp"
#include "ores.refdata.api/export.hpp"
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

namespace ores::refdata::messaging {

struct cross_currency_basis_flat_leg_key {
    boost::uuids::uuid id;

    friend bool operator==(const cross_currency_basis_flat_leg_key&,
                           const cross_currency_basis_flat_leg_key&) = default;
};

struct cross_currency_basis_flat_leg_write {
    boost::uuids::uuid id;
    boost::uuids::uuid convention_id;
    std::string flat_index;
    std::optional<bool> flat_index_is_resettable;
    std::optional<std::string> flat_tenor;
    std::optional<int> flat_payment_lag;
    std::optional<bool> flat_include_spread;
    std::optional<std::string> flat_lookback;
    std::optional<int> flat_fixing_days;
    std::optional<int> flat_rate_cutoff;
    std::optional<bool> flat_is_averaged;
    std::optional<bool> flat_observation_shift;

    friend bool operator==(const cross_currency_basis_flat_leg_write&,
                           const cross_currency_basis_flat_leg_write&) = default;
};

struct cross_currency_basis_flat_leg_change {
    cross_currency_basis_flat_leg_write write;
    ores::utility::domain::precondition precondition;

    friend bool operator==(const cross_currency_basis_flat_leg_change&,
                           const cross_currency_basis_flat_leg_change&) = default;
};

struct cross_currency_basis_flat_leg_removal {
    cross_currency_basis_flat_leg_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;

    friend bool operator==(const cross_currency_basis_flat_leg_removal&,
                           const cross_currency_basis_flat_leg_removal&) = default;
};

struct cross_currency_basis_flat_leg_lookup {
    cross_currency_basis_flat_leg_key key;
    std::optional<ores::refdata::domain::cross_currency_basis_flat_leg>
        cross_currency_basis_flat_leg;

    friend bool operator==(const cross_currency_basis_flat_leg_lookup&,
                           const cross_currency_basis_flat_leg_lookup&) = default;
};

struct cross_currency_basis_flat_legs_filter {
    std::optional<boost::uuids::uuid> convention_id;
    std::optional<std::vector<boost::uuids::uuid>> id_one_of;
    std::optional<std::vector<boost::uuids::uuid>> convention_id_one_of;

    friend bool operator==(const cross_currency_basis_flat_legs_filter&,
                           const cross_currency_basis_flat_legs_filter&) = default;
};

struct cross_currency_basis_flat_leg_event {
    boost::uuids::uuid event_id;
    cross_currency_basis_flat_leg_key key;
    std::string action;
    std::uint32_t version{};
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;

    friend bool operator==(const cross_currency_basis_flat_leg_event&,
                           const cross_currency_basis_flat_leg_event&) = default;
};

struct cross_currency_basis_flat_leg_version_key {
    cross_currency_basis_flat_leg_key cross_currency_basis_flat_leg;
    std::uint32_t version{};

    friend bool operator==(const cross_currency_basis_flat_leg_version_key&,
                           const cross_currency_basis_flat_leg_version_key&) = default;
};

struct cross_currency_basis_flat_leg_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;

    friend bool operator==(const cross_currency_basis_flat_leg_versions_filter&,
                           const cross_currency_basis_flat_leg_versions_filter&) = default;
};

struct list_cross_currency_basis_flat_legs_request {
    using response_type = struct list_cross_currency_basis_flat_legs_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.cross_currency_basis_flat_legs.list";
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
    std::optional<cross_currency_basis_flat_legs_filter> filter;
    std::optional<std::string> as_of;

    friend bool operator==(const list_cross_currency_basis_flat_legs_request&,
                           const list_cross_currency_basis_flat_legs_request&) = default;
};

struct list_cross_currency_basis_flat_legs_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::cross_currency_basis_flat_leg>
        cross_currency_basis_flat_legs;
    std::uint64_t total{};

    friend bool operator==(const list_cross_currency_basis_flat_legs_response&,
                           const list_cross_currency_basis_flat_legs_response&) = default;
};

struct get_cross_currency_basis_flat_leg_request {
    using response_type = struct get_cross_currency_basis_flat_leg_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.cross_currency_basis_flat_legs.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    cross_currency_basis_flat_leg_key key;

    friend bool operator==(const get_cross_currency_basis_flat_leg_request&,
                           const get_cross_currency_basis_flat_leg_request&) = default;
};

struct get_cross_currency_basis_flat_leg_response {
    ores::utility::domain::result result;
    std::optional<ores::refdata::domain::cross_currency_basis_flat_leg>
        cross_currency_basis_flat_leg;

    friend bool operator==(const get_cross_currency_basis_flat_leg_response&,
                           const get_cross_currency_basis_flat_leg_response&) = default;
};

struct get_many_cross_currency_basis_flat_legs_request {
    using response_type = struct get_many_cross_currency_basis_flat_legs_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.cross_currency_basis_flat_legs.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<cross_currency_basis_flat_leg_key> keys;

    friend bool operator==(const get_many_cross_currency_basis_flat_legs_request&,
                           const get_many_cross_currency_basis_flat_legs_request&) = default;
};

struct get_many_cross_currency_basis_flat_legs_response {
    ores::utility::domain::result result;
    std::vector<cross_currency_basis_flat_leg_lookup> entries;

    friend bool operator==(const get_many_cross_currency_basis_flat_legs_response&,
                           const get_many_cross_currency_basis_flat_legs_response&) = default;
};

struct put_cross_currency_basis_flat_leg_request {
    using response_type = struct put_cross_currency_basis_flat_leg_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.cross_currency_basis_flat_legs.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    cross_currency_basis_flat_leg_change change;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const put_cross_currency_basis_flat_leg_request&,
                           const put_cross_currency_basis_flat_leg_request&) = default;
};

struct put_cross_currency_basis_flat_leg_response {
    ores::utility::domain::result result;
    std::optional<ores::refdata::domain::cross_currency_basis_flat_leg>
        cross_currency_basis_flat_leg;

    friend bool operator==(const put_cross_currency_basis_flat_leg_response&,
                           const put_cross_currency_basis_flat_leg_response&) = default;
};

struct put_many_cross_currency_basis_flat_legs_request {
    using response_type = struct put_many_cross_currency_basis_flat_legs_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.cross_currency_basis_flat_legs.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<cross_currency_basis_flat_leg_change> changes;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const put_many_cross_currency_basis_flat_legs_request&,
                           const put_many_cross_currency_basis_flat_legs_request&) = default;
};

struct put_many_cross_currency_basis_flat_legs_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::cross_currency_basis_flat_leg>
        cross_currency_basis_flat_legs;

    friend bool operator==(const put_many_cross_currency_basis_flat_legs_response&,
                           const put_many_cross_currency_basis_flat_legs_response&) = default;
};

struct delete_cross_currency_basis_flat_leg_request {
    using response_type = struct delete_cross_currency_basis_flat_leg_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.cross_currency_basis_flat_legs.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    cross_currency_basis_flat_leg_removal removal;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const delete_cross_currency_basis_flat_leg_request&,
                           const delete_cross_currency_basis_flat_leg_request&) = default;
};

struct delete_cross_currency_basis_flat_leg_response {
    ores::utility::domain::result result;

    friend bool operator==(const delete_cross_currency_basis_flat_leg_response&,
                           const delete_cross_currency_basis_flat_leg_response&) = default;
};

struct delete_many_cross_currency_basis_flat_legs_request {
    using response_type = struct delete_many_cross_currency_basis_flat_legs_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.cross_currency_basis_flat_legs.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<cross_currency_basis_flat_leg_removal> removals;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const delete_many_cross_currency_basis_flat_legs_request&,
                           const delete_many_cross_currency_basis_flat_legs_request&) = default;
};

struct delete_many_cross_currency_basis_flat_legs_response {
    ores::utility::domain::result result;

    friend bool operator==(const delete_many_cross_currency_basis_flat_legs_response&,
                           const delete_many_cross_currency_basis_flat_legs_response&) = default;
};

struct list_by_convention_id_cross_currency_basis_flat_legs_request {
    using response_type = struct list_by_convention_id_cross_currency_basis_flat_legs_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.cross_currency_basis_flat_legs.list_by_convention_id";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    boost::uuids::uuid convention_id;
    ores::utility::domain::scope scope = ores::utility::domain::scope::direct;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<cross_currency_basis_flat_legs_filter> filter;

    friend bool
    operator==(const list_by_convention_id_cross_currency_basis_flat_legs_request&,
               const list_by_convention_id_cross_currency_basis_flat_legs_request&) = default;
};

struct list_by_convention_id_cross_currency_basis_flat_legs_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::cross_currency_basis_flat_leg>
        cross_currency_basis_flat_legs;
    std::uint64_t total{};

    friend bool
    operator==(const list_by_convention_id_cross_currency_basis_flat_legs_response&,
               const list_by_convention_id_cross_currency_basis_flat_legs_response&) = default;
};

struct list_cross_currency_basis_flat_leg_versions_request {
    using response_type = struct list_cross_currency_basis_flat_leg_versions_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.cross_currency_basis_flat_legs_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    cross_currency_basis_flat_leg_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<cross_currency_basis_flat_leg_versions_filter> filter;

    friend bool operator==(const list_cross_currency_basis_flat_leg_versions_request&,
                           const list_cross_currency_basis_flat_leg_versions_request&) = default;
};

struct list_cross_currency_basis_flat_leg_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::cross_currency_basis_flat_leg> versions;
    std::uint64_t total{};

    friend bool operator==(const list_cross_currency_basis_flat_leg_versions_response&,
                           const list_cross_currency_basis_flat_leg_versions_response&) = default;
};

struct get_cross_currency_basis_flat_leg_version_request {
    using response_type = struct get_cross_currency_basis_flat_leg_version_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.cross_currency_basis_flat_legs_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    cross_currency_basis_flat_leg_version_key key;

    friend bool operator==(const get_cross_currency_basis_flat_leg_version_request&,
                           const get_cross_currency_basis_flat_leg_version_request&) = default;
};

struct get_cross_currency_basis_flat_leg_version_response {
    ores::utility::domain::result result;
    std::optional<ores::refdata::domain::cross_currency_basis_flat_leg> version;

    friend bool operator==(const get_cross_currency_basis_flat_leg_version_response&,
                           const get_cross_currency_basis_flat_leg_version_response&) = default;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace cross_currency_basis_flat_leg_event_subjects {
inline constexpr std::string_view created =
    "refdata.v1.events.cross_currency_basis_flat_legs.created";
inline constexpr std::string_view updated =
    "refdata.v1.events.cross_currency_basis_flat_legs.updated";
inline constexpr std::string_view deleted =
    "refdata.v1.events.cross_currency_basis_flat_legs.deleted";
}
/**
 * @brief The members of cross_currency_basis_flat_leg_key, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<cross_currency_basis_flat_leg_key>) {
    return std::tuple{
        utility::codec::field{"id", &cross_currency_basis_flat_leg_key::id},
    };
}

/**
 * @brief The members of cross_currency_basis_flat_leg_write, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<cross_currency_basis_flat_leg_write>) {
    return std::tuple{
        utility::codec::field{"id", &cross_currency_basis_flat_leg_write::id},
        utility::codec::field{"convention_id", &cross_currency_basis_flat_leg_write::convention_id},
        utility::codec::field{"flat_index", &cross_currency_basis_flat_leg_write::flat_index},
        utility::codec::field{"flat_index_is_resettable",
                              &cross_currency_basis_flat_leg_write::flat_index_is_resettable},
        utility::codec::field{"flat_tenor", &cross_currency_basis_flat_leg_write::flat_tenor},
        utility::codec::field{"flat_payment_lag",
                              &cross_currency_basis_flat_leg_write::flat_payment_lag},
        utility::codec::field{"flat_include_spread",
                              &cross_currency_basis_flat_leg_write::flat_include_spread},
        utility::codec::field{"flat_lookback", &cross_currency_basis_flat_leg_write::flat_lookback},
        utility::codec::field{"flat_fixing_days",
                              &cross_currency_basis_flat_leg_write::flat_fixing_days},
        utility::codec::field{"flat_rate_cutoff",
                              &cross_currency_basis_flat_leg_write::flat_rate_cutoff},
        utility::codec::field{"flat_is_averaged",
                              &cross_currency_basis_flat_leg_write::flat_is_averaged},
        utility::codec::field{"flat_observation_shift",
                              &cross_currency_basis_flat_leg_write::flat_observation_shift},
    };
}

/**
 * @brief The members of cross_currency_basis_flat_leg_change, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<cross_currency_basis_flat_leg_change>) {
    return std::tuple{
        utility::codec::field{"write", &cross_currency_basis_flat_leg_change::write},
        utility::codec::field{"precondition", &cross_currency_basis_flat_leg_change::precondition},
    };
}

/**
 * @brief The members of cross_currency_basis_flat_leg_removal, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<cross_currency_basis_flat_leg_removal>) {
    return std::tuple{
        utility::codec::field{"key", &cross_currency_basis_flat_leg_removal::key},
        utility::codec::field{"precondition", &cross_currency_basis_flat_leg_removal::precondition},
    };
}

/**
 * @brief The members of cross_currency_basis_flat_leg_lookup, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<cross_currency_basis_flat_leg_lookup>) {
    return std::tuple{
        utility::codec::field{"key", &cross_currency_basis_flat_leg_lookup::key},
        utility::codec::field{"cross_currency_basis_flat_leg",
                              &cross_currency_basis_flat_leg_lookup::cross_currency_basis_flat_leg},
    };
}

/**
 * @brief The members of cross_currency_basis_flat_legs_filter, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<cross_currency_basis_flat_legs_filter>) {
    return std::tuple{
        utility::codec::field{"convention_id",
                              &cross_currency_basis_flat_legs_filter::convention_id},
        utility::codec::field{"id_one_of", &cross_currency_basis_flat_legs_filter::id_one_of},
        utility::codec::field{"convention_id_one_of",
                              &cross_currency_basis_flat_legs_filter::convention_id_one_of},
    };
}

/**
 * @brief The members of cross_currency_basis_flat_leg_event, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<cross_currency_basis_flat_leg_event>) {
    return std::tuple{
        utility::codec::field{"event_id", &cross_currency_basis_flat_leg_event::event_id},
        utility::codec::field{"key", &cross_currency_basis_flat_leg_event::key},
        utility::codec::field{"action", &cross_currency_basis_flat_leg_event::action},
        utility::codec::field{"version", &cross_currency_basis_flat_leg_event::version},
        utility::codec::field{"occurred_at", &cross_currency_basis_flat_leg_event::occurred_at},
        utility::codec::field{"correlation_id",
                              &cross_currency_basis_flat_leg_event::correlation_id},
    };
}

/**
 * @brief The members of cross_currency_basis_flat_leg_version_key, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<cross_currency_basis_flat_leg_version_key>) {
    return std::tuple{
        utility::codec::field{
            "cross_currency_basis_flat_leg",
            &cross_currency_basis_flat_leg_version_key::cross_currency_basis_flat_leg},
        utility::codec::field{"version", &cross_currency_basis_flat_leg_version_key::version},
    };
}

/**
 * @brief The members of cross_currency_basis_flat_leg_versions_filter, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<cross_currency_basis_flat_leg_versions_filter>) {
    return std::tuple{
        utility::codec::field{"version", &cross_currency_basis_flat_leg_versions_filter::version},
        utility::codec::field{"from_version",
                              &cross_currency_basis_flat_leg_versions_filter::from_version},
        utility::codec::field{"to_version",
                              &cross_currency_basis_flat_leg_versions_filter::to_version},
    };
}

/**
 * @brief The members of list_cross_currency_basis_flat_legs_request, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<list_cross_currency_basis_flat_legs_request>) {
    return std::tuple{
        utility::codec::field{"offset", &list_cross_currency_basis_flat_legs_request::offset},
        utility::codec::field{"limit", &list_cross_currency_basis_flat_legs_request::limit},
        utility::codec::field{"order", &list_cross_currency_basis_flat_legs_request::order},
        utility::codec::field{"filter", &list_cross_currency_basis_flat_legs_request::filter},
        utility::codec::field{"as_of", &list_cross_currency_basis_flat_legs_request::as_of},
    };
}

/**
 * @brief The members of list_cross_currency_basis_flat_legs_response, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<list_cross_currency_basis_flat_legs_response>) {
    return std::tuple{
        utility::codec::field{"result", &list_cross_currency_basis_flat_legs_response::result},
        utility::codec::field{
            "cross_currency_basis_flat_legs",
            &list_cross_currency_basis_flat_legs_response::cross_currency_basis_flat_legs},
        utility::codec::field{"total", &list_cross_currency_basis_flat_legs_response::total},
    };
}

/**
 * @brief The members of get_cross_currency_basis_flat_leg_request, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<get_cross_currency_basis_flat_leg_request>) {
    return std::tuple{
        utility::codec::field{"key", &get_cross_currency_basis_flat_leg_request::key},
    };
}

/**
 * @brief The members of get_cross_currency_basis_flat_leg_response, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<get_cross_currency_basis_flat_leg_response>) {
    return std::tuple{
        utility::codec::field{"result", &get_cross_currency_basis_flat_leg_response::result},
        utility::codec::field{
            "cross_currency_basis_flat_leg",
            &get_cross_currency_basis_flat_leg_response::cross_currency_basis_flat_leg},
    };
}

/**
 * @brief The members of get_many_cross_currency_basis_flat_legs_request, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<get_many_cross_currency_basis_flat_legs_request>) {
    return std::tuple{
        utility::codec::field{"keys", &get_many_cross_currency_basis_flat_legs_request::keys},
    };
}

/**
 * @brief The members of get_many_cross_currency_basis_flat_legs_response, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<get_many_cross_currency_basis_flat_legs_response>) {
    return std::tuple{
        utility::codec::field{"result", &get_many_cross_currency_basis_flat_legs_response::result},
        utility::codec::field{"entries",
                              &get_many_cross_currency_basis_flat_legs_response::entries},
    };
}

/**
 * @brief The members of put_cross_currency_basis_flat_leg_request, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<put_cross_currency_basis_flat_leg_request>) {
    return std::tuple{
        utility::codec::field{"change", &put_cross_currency_basis_flat_leg_request::change},
        utility::codec::field{"intent", &put_cross_currency_basis_flat_leg_request::intent},
    };
}

/**
 * @brief The members of put_cross_currency_basis_flat_leg_response, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<put_cross_currency_basis_flat_leg_response>) {
    return std::tuple{
        utility::codec::field{"result", &put_cross_currency_basis_flat_leg_response::result},
        utility::codec::field{
            "cross_currency_basis_flat_leg",
            &put_cross_currency_basis_flat_leg_response::cross_currency_basis_flat_leg},
    };
}

/**
 * @brief The members of put_many_cross_currency_basis_flat_legs_request, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<put_many_cross_currency_basis_flat_legs_request>) {
    return std::tuple{
        utility::codec::field{"changes", &put_many_cross_currency_basis_flat_legs_request::changes},
        utility::codec::field{"intent", &put_many_cross_currency_basis_flat_legs_request::intent},
    };
}

/**
 * @brief The members of put_many_cross_currency_basis_flat_legs_response, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<put_many_cross_currency_basis_flat_legs_response>) {
    return std::tuple{
        utility::codec::field{"result", &put_many_cross_currency_basis_flat_legs_response::result},
        utility::codec::field{
            "cross_currency_basis_flat_legs",
            &put_many_cross_currency_basis_flat_legs_response::cross_currency_basis_flat_legs},
    };
}

/**
 * @brief The members of delete_cross_currency_basis_flat_leg_request, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<delete_cross_currency_basis_flat_leg_request>) {
    return std::tuple{
        utility::codec::field{"removal", &delete_cross_currency_basis_flat_leg_request::removal},
        utility::codec::field{"intent", &delete_cross_currency_basis_flat_leg_request::intent},
    };
}

/**
 * @brief The members of delete_cross_currency_basis_flat_leg_response, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<delete_cross_currency_basis_flat_leg_response>) {
    return std::tuple{
        utility::codec::field{"result", &delete_cross_currency_basis_flat_leg_response::result},
    };
}

/**
 * @brief The members of delete_many_cross_currency_basis_flat_legs_request, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<delete_many_cross_currency_basis_flat_legs_request>) {
    return std::tuple{
        utility::codec::field{"removals",
                              &delete_many_cross_currency_basis_flat_legs_request::removals},
        utility::codec::field{"intent",
                              &delete_many_cross_currency_basis_flat_legs_request::intent},
    };
}

/**
 * @brief The members of delete_many_cross_currency_basis_flat_legs_response, named as the wire
 * names them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<delete_many_cross_currency_basis_flat_legs_response>) {
    return std::tuple{
        utility::codec::field{"result",
                              &delete_many_cross_currency_basis_flat_legs_response::result},
    };
}

/**
 * @brief The members of list_by_convention_id_cross_currency_basis_flat_legs_request, named as the
 * wire names them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<list_by_convention_id_cross_currency_basis_flat_legs_request>) {
    return std::tuple{
        utility::codec::field{
            "convention_id",
            &list_by_convention_id_cross_currency_basis_flat_legs_request::convention_id},
        utility::codec::field{"scope",
                              &list_by_convention_id_cross_currency_basis_flat_legs_request::scope},
        utility::codec::field{
            "offset", &list_by_convention_id_cross_currency_basis_flat_legs_request::offset},
        utility::codec::field{"limit",
                              &list_by_convention_id_cross_currency_basis_flat_legs_request::limit},
        utility::codec::field{"order",
                              &list_by_convention_id_cross_currency_basis_flat_legs_request::order},
        utility::codec::field{
            "filter", &list_by_convention_id_cross_currency_basis_flat_legs_request::filter},
    };
}

/**
 * @brief The members of list_by_convention_id_cross_currency_basis_flat_legs_response, named as the
 * wire names them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<list_by_convention_id_cross_currency_basis_flat_legs_response>) {
    return std::tuple{
        utility::codec::field{
            "result", &list_by_convention_id_cross_currency_basis_flat_legs_response::result},
        utility::codec::field{"cross_currency_basis_flat_legs",
                              &list_by_convention_id_cross_currency_basis_flat_legs_response::
                                  cross_currency_basis_flat_legs},
        utility::codec::field{
            "total", &list_by_convention_id_cross_currency_basis_flat_legs_response::total},
    };
}

/**
 * @brief The members of list_cross_currency_basis_flat_leg_versions_request, named as the wire
 * names them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<list_cross_currency_basis_flat_leg_versions_request>) {
    return std::tuple{
        utility::codec::field{"key", &list_cross_currency_basis_flat_leg_versions_request::key},
        utility::codec::field{"offset",
                              &list_cross_currency_basis_flat_leg_versions_request::offset},
        utility::codec::field{"limit", &list_cross_currency_basis_flat_leg_versions_request::limit},
        utility::codec::field{"order", &list_cross_currency_basis_flat_leg_versions_request::order},
        utility::codec::field{"filter",
                              &list_cross_currency_basis_flat_leg_versions_request::filter},
    };
}

/**
 * @brief The members of list_cross_currency_basis_flat_leg_versions_response, named as the wire
 * names them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<list_cross_currency_basis_flat_leg_versions_response>) {
    return std::tuple{
        utility::codec::field{"result",
                              &list_cross_currency_basis_flat_leg_versions_response::result},
        utility::codec::field{"versions",
                              &list_cross_currency_basis_flat_leg_versions_response::versions},
        utility::codec::field{"total",
                              &list_cross_currency_basis_flat_leg_versions_response::total},
    };
}

/**
 * @brief The members of get_cross_currency_basis_flat_leg_version_request, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<get_cross_currency_basis_flat_leg_version_request>) {
    return std::tuple{
        utility::codec::field{"key", &get_cross_currency_basis_flat_leg_version_request::key},
    };
}

/**
 * @brief The members of get_cross_currency_basis_flat_leg_version_response, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<get_cross_currency_basis_flat_leg_version_response>) {
    return std::tuple{
        utility::codec::field{"result",
                              &get_cross_currency_basis_flat_leg_version_response::result},
        utility::codec::field{"version",
                              &get_cross_currency_basis_flat_leg_version_response::version},
    };
}


}
namespace ores::utility::codec {

template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::cross_currency_basis_flat_leg_key> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::cross_currency_basis_flat_leg_key& v);
    static ores::refdata::messaging::cross_currency_basis_flat_leg_key
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::cross_currency_basis_flat_leg_key& v);
    static ores::refdata::messaging::cross_currency_basis_flat_leg_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::cross_currency_basis_flat_leg_write> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::cross_currency_basis_flat_leg_write& v);
    static ores::refdata::messaging::cross_currency_basis_flat_leg_write
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::cross_currency_basis_flat_leg_write& v);
    static ores::refdata::messaging::cross_currency_basis_flat_leg_write
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::cross_currency_basis_flat_leg_change> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::cross_currency_basis_flat_leg_change& v);
    static ores::refdata::messaging::cross_currency_basis_flat_leg_change
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::cross_currency_basis_flat_leg_change& v);
    static ores::refdata::messaging::cross_currency_basis_flat_leg_change
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::cross_currency_basis_flat_leg_removal> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::cross_currency_basis_flat_leg_removal& v);
    static ores::refdata::messaging::cross_currency_basis_flat_leg_removal
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::cross_currency_basis_flat_leg_removal& v);
    static ores::refdata::messaging::cross_currency_basis_flat_leg_removal
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::cross_currency_basis_flat_leg_lookup> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::cross_currency_basis_flat_leg_lookup& v);
    static ores::refdata::messaging::cross_currency_basis_flat_leg_lookup
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::cross_currency_basis_flat_leg_lookup& v);
    static ores::refdata::messaging::cross_currency_basis_flat_leg_lookup
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::cross_currency_basis_flat_legs_filter> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::cross_currency_basis_flat_legs_filter& v);
    static ores::refdata::messaging::cross_currency_basis_flat_legs_filter
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::cross_currency_basis_flat_legs_filter& v);
    static ores::refdata::messaging::cross_currency_basis_flat_legs_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::cross_currency_basis_flat_leg_event> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::cross_currency_basis_flat_leg_event& v);
    static ores::refdata::messaging::cross_currency_basis_flat_leg_event
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::cross_currency_basis_flat_leg_event& v);
    static ores::refdata::messaging::cross_currency_basis_flat_leg_event
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::cross_currency_basis_flat_leg_version_key> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::cross_currency_basis_flat_leg_version_key& v);
    static ores::refdata::messaging::cross_currency_basis_flat_leg_version_key
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::cross_currency_basis_flat_leg_version_key& v);
    static ores::refdata::messaging::cross_currency_basis_flat_leg_version_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::cross_currency_basis_flat_leg_versions_filter> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::cross_currency_basis_flat_leg_versions_filter& v);
    static ores::refdata::messaging::cross_currency_basis_flat_leg_versions_filter
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::cross_currency_basis_flat_leg_versions_filter& v);
    static ores::refdata::messaging::cross_currency_basis_flat_leg_versions_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::list_cross_currency_basis_flat_legs_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::list_cross_currency_basis_flat_legs_request& v);
    static ores::refdata::messaging::list_cross_currency_basis_flat_legs_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::list_cross_currency_basis_flat_legs_request& v);
    static ores::refdata::messaging::list_cross_currency_basis_flat_legs_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::list_cross_currency_basis_flat_legs_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::list_cross_currency_basis_flat_legs_response& v);
    static ores::refdata::messaging::list_cross_currency_basis_flat_legs_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::list_cross_currency_basis_flat_legs_response& v);
    static ores::refdata::messaging::list_cross_currency_basis_flat_legs_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_cross_currency_basis_flat_leg_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::get_cross_currency_basis_flat_leg_request& v);
    static ores::refdata::messaging::get_cross_currency_basis_flat_leg_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::get_cross_currency_basis_flat_leg_request& v);
    static ores::refdata::messaging::get_cross_currency_basis_flat_leg_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_cross_currency_basis_flat_leg_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::get_cross_currency_basis_flat_leg_response& v);
    static ores::refdata::messaging::get_cross_currency_basis_flat_leg_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::get_cross_currency_basis_flat_leg_response& v);
    static ores::refdata::messaging::get_cross_currency_basis_flat_leg_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_many_cross_currency_basis_flat_legs_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::get_many_cross_currency_basis_flat_legs_request& v);
    static ores::refdata::messaging::get_many_cross_currency_basis_flat_legs_request
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::refdata::messaging::get_many_cross_currency_basis_flat_legs_request& v);
    static ores::refdata::messaging::get_many_cross_currency_basis_flat_legs_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_many_cross_currency_basis_flat_legs_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::get_many_cross_currency_basis_flat_legs_response& v);
    static ores::refdata::messaging::get_many_cross_currency_basis_flat_legs_response
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::refdata::messaging::get_many_cross_currency_basis_flat_legs_response& v);
    static ores::refdata::messaging::get_many_cross_currency_basis_flat_legs_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::put_cross_currency_basis_flat_leg_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::put_cross_currency_basis_flat_leg_request& v);
    static ores::refdata::messaging::put_cross_currency_basis_flat_leg_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::put_cross_currency_basis_flat_leg_request& v);
    static ores::refdata::messaging::put_cross_currency_basis_flat_leg_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::put_cross_currency_basis_flat_leg_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::put_cross_currency_basis_flat_leg_response& v);
    static ores::refdata::messaging::put_cross_currency_basis_flat_leg_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::put_cross_currency_basis_flat_leg_response& v);
    static ores::refdata::messaging::put_cross_currency_basis_flat_leg_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::put_many_cross_currency_basis_flat_legs_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::put_many_cross_currency_basis_flat_legs_request& v);
    static ores::refdata::messaging::put_many_cross_currency_basis_flat_legs_request
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::refdata::messaging::put_many_cross_currency_basis_flat_legs_request& v);
    static ores::refdata::messaging::put_many_cross_currency_basis_flat_legs_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::put_many_cross_currency_basis_flat_legs_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::put_many_cross_currency_basis_flat_legs_response& v);
    static ores::refdata::messaging::put_many_cross_currency_basis_flat_legs_response
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::refdata::messaging::put_many_cross_currency_basis_flat_legs_response& v);
    static ores::refdata::messaging::put_many_cross_currency_basis_flat_legs_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::delete_cross_currency_basis_flat_leg_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::delete_cross_currency_basis_flat_leg_request& v);
    static ores::refdata::messaging::delete_cross_currency_basis_flat_leg_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::delete_cross_currency_basis_flat_leg_request& v);
    static ores::refdata::messaging::delete_cross_currency_basis_flat_leg_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::delete_cross_currency_basis_flat_leg_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::delete_cross_currency_basis_flat_leg_response& v);
    static ores::refdata::messaging::delete_cross_currency_basis_flat_leg_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::delete_cross_currency_basis_flat_leg_response& v);
    static ores::refdata::messaging::delete_cross_currency_basis_flat_leg_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::delete_many_cross_currency_basis_flat_legs_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::refdata::messaging::delete_many_cross_currency_basis_flat_legs_request& v);
    static ores::refdata::messaging::delete_many_cross_currency_basis_flat_legs_request
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::refdata::messaging::delete_many_cross_currency_basis_flat_legs_request& v);
    static ores::refdata::messaging::delete_many_cross_currency_basis_flat_legs_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::delete_many_cross_currency_basis_flat_legs_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::refdata::messaging::delete_many_cross_currency_basis_flat_legs_response& v);
    static ores::refdata::messaging::delete_many_cross_currency_basis_flat_legs_response
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::refdata::messaging::delete_many_cross_currency_basis_flat_legs_response& v);
    static ores::refdata::messaging::delete_many_cross_currency_basis_flat_legs_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<
    ores::refdata::messaging::list_by_convention_id_cross_currency_basis_flat_legs_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::
                   list_by_convention_id_cross_currency_basis_flat_legs_request& v);
    static ores::refdata::messaging::list_by_convention_id_cross_currency_basis_flat_legs_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::
                      list_by_convention_id_cross_currency_basis_flat_legs_request& v);
    static ores::refdata::messaging::list_by_convention_id_cross_currency_basis_flat_legs_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<
    ores::refdata::messaging::list_by_convention_id_cross_currency_basis_flat_legs_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::
                   list_by_convention_id_cross_currency_basis_flat_legs_response& v);
    static ores::refdata::messaging::list_by_convention_id_cross_currency_basis_flat_legs_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::
                      list_by_convention_id_cross_currency_basis_flat_legs_response& v);
    static ores::refdata::messaging::list_by_convention_id_cross_currency_basis_flat_legs_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::list_cross_currency_basis_flat_leg_versions_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::refdata::messaging::list_cross_currency_basis_flat_leg_versions_request& v);
    static ores::refdata::messaging::list_cross_currency_basis_flat_leg_versions_request
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::refdata::messaging::list_cross_currency_basis_flat_leg_versions_request& v);
    static ores::refdata::messaging::list_cross_currency_basis_flat_leg_versions_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::list_cross_currency_basis_flat_leg_versions_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::refdata::messaging::list_cross_currency_basis_flat_leg_versions_response& v);
    static ores::refdata::messaging::list_cross_currency_basis_flat_leg_versions_response
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::refdata::messaging::list_cross_currency_basis_flat_leg_versions_response& v);
    static ores::refdata::messaging::list_cross_currency_basis_flat_leg_versions_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_cross_currency_basis_flat_leg_version_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::refdata::messaging::get_cross_currency_basis_flat_leg_version_request& v);
    static ores::refdata::messaging::get_cross_currency_basis_flat_leg_version_request
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::refdata::messaging::get_cross_currency_basis_flat_leg_version_request& v);
    static ores::refdata::messaging::get_cross_currency_basis_flat_leg_version_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_cross_currency_basis_flat_leg_version_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::refdata::messaging::get_cross_currency_basis_flat_leg_version_response& v);
    static ores::refdata::messaging::get_cross_currency_basis_flat_leg_version_response
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::refdata::messaging::get_cross_currency_basis_flat_leg_version_response& v);
    static ores::refdata::messaging::get_cross_currency_basis_flat_leg_version_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
