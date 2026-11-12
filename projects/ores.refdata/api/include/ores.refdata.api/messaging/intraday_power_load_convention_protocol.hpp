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
#ifndef ORES_REFDATA_API_MESSAGING_INTRADAY_POWER_LOAD_CONVENTION_PROTOCOL_HPP
#define ORES_REFDATA_API_MESSAGING_INTRADAY_POWER_LOAD_CONVENTION_PROTOCOL_HPP

#include "ores.refdata.api/domain/intraday_power_load_convention_codec.hpp"
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

struct intraday_power_load_convention_key {
    std::string id;

    friend bool operator==(const intraday_power_load_convention_key&,
                           const intraday_power_load_convention_key&) = default;
};

struct intraday_power_load_convention_write {
    std::string id;
    std::optional<std::string> explicit_load_profile;
    std::optional<std::string> business_day_load_rules;
    std::optional<std::string> oresmd_uri;

    friend bool operator==(const intraday_power_load_convention_write&,
                           const intraday_power_load_convention_write&) = default;
};

struct intraday_power_load_convention_change {
    intraday_power_load_convention_write write;
    ores::utility::domain::precondition precondition;

    friend bool operator==(const intraday_power_load_convention_change&,
                           const intraday_power_load_convention_change&) = default;
};

struct intraday_power_load_convention_removal {
    intraday_power_load_convention_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;

    friend bool operator==(const intraday_power_load_convention_removal&,
                           const intraday_power_load_convention_removal&) = default;
};

struct intraday_power_load_convention_lookup {
    intraday_power_load_convention_key key;
    std::optional<ores::refdata::domain::intraday_power_load_convention>
        intraday_power_load_convention;

    friend bool operator==(const intraday_power_load_convention_lookup&,
                           const intraday_power_load_convention_lookup&) = default;
};

struct intraday_power_load_conventions_filter {
    std::optional<std::vector<std::string>> id_one_of;

    friend bool operator==(const intraday_power_load_conventions_filter&,
                           const intraday_power_load_conventions_filter&) = default;
};

struct intraday_power_load_convention_event {
    boost::uuids::uuid event_id;
    intraday_power_load_convention_key key;
    std::string action;
    std::uint32_t version{};
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;

    friend bool operator==(const intraday_power_load_convention_event&,
                           const intraday_power_load_convention_event&) = default;
};

struct intraday_power_load_convention_version_key {
    intraday_power_load_convention_key intraday_power_load_convention;
    std::uint32_t version{};

    friend bool operator==(const intraday_power_load_convention_version_key&,
                           const intraday_power_load_convention_version_key&) = default;
};

struct intraday_power_load_convention_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;

    friend bool operator==(const intraday_power_load_convention_versions_filter&,
                           const intraday_power_load_convention_versions_filter&) = default;
};

struct list_intraday_power_load_conventions_request {
    using response_type = struct list_intraday_power_load_conventions_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.intraday_power_load_conventions.list";
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
    std::optional<intraday_power_load_conventions_filter> filter;
    std::optional<std::string> as_of;

    friend bool operator==(const list_intraday_power_load_conventions_request&,
                           const list_intraday_power_load_conventions_request&) = default;
};

struct list_intraday_power_load_conventions_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::intraday_power_load_convention>
        intraday_power_load_conventions;
    std::uint64_t total{};

    friend bool operator==(const list_intraday_power_load_conventions_response&,
                           const list_intraday_power_load_conventions_response&) = default;
};

struct get_intraday_power_load_convention_request {
    using response_type = struct get_intraday_power_load_convention_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.intraday_power_load_conventions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    intraday_power_load_convention_key key;

    friend bool operator==(const get_intraday_power_load_convention_request&,
                           const get_intraday_power_load_convention_request&) = default;
};

struct get_intraday_power_load_convention_response {
    ores::utility::domain::result result;
    std::optional<ores::refdata::domain::intraday_power_load_convention>
        intraday_power_load_convention;

    friend bool operator==(const get_intraday_power_load_convention_response&,
                           const get_intraday_power_load_convention_response&) = default;
};

struct get_many_intraday_power_load_conventions_request {
    using response_type = struct get_many_intraday_power_load_conventions_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.intraday_power_load_conventions.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<intraday_power_load_convention_key> keys;

    friend bool operator==(const get_many_intraday_power_load_conventions_request&,
                           const get_many_intraday_power_load_conventions_request&) = default;
};

struct get_many_intraday_power_load_conventions_response {
    ores::utility::domain::result result;
    std::vector<intraday_power_load_convention_lookup> entries;

    friend bool operator==(const get_many_intraday_power_load_conventions_response&,
                           const get_many_intraday_power_load_conventions_response&) = default;
};

struct put_intraday_power_load_convention_request {
    using response_type = struct put_intraday_power_load_convention_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.intraday_power_load_conventions.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    intraday_power_load_convention_change change;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const put_intraday_power_load_convention_request&,
                           const put_intraday_power_load_convention_request&) = default;
};

struct put_intraday_power_load_convention_response {
    ores::utility::domain::result result;
    std::optional<ores::refdata::domain::intraday_power_load_convention>
        intraday_power_load_convention;

    friend bool operator==(const put_intraday_power_load_convention_response&,
                           const put_intraday_power_load_convention_response&) = default;
};

struct put_many_intraday_power_load_conventions_request {
    using response_type = struct put_many_intraday_power_load_conventions_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.intraday_power_load_conventions.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<intraday_power_load_convention_change> changes;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const put_many_intraday_power_load_conventions_request&,
                           const put_many_intraday_power_load_conventions_request&) = default;
};

struct put_many_intraday_power_load_conventions_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::intraday_power_load_convention>
        intraday_power_load_conventions;

    friend bool operator==(const put_many_intraday_power_load_conventions_response&,
                           const put_many_intraday_power_load_conventions_response&) = default;
};

struct delete_intraday_power_load_convention_request {
    using response_type = struct delete_intraday_power_load_convention_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.intraday_power_load_conventions.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    intraday_power_load_convention_removal removal;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const delete_intraday_power_load_convention_request&,
                           const delete_intraday_power_load_convention_request&) = default;
};

struct delete_intraday_power_load_convention_response {
    ores::utility::domain::result result;

    friend bool operator==(const delete_intraday_power_load_convention_response&,
                           const delete_intraday_power_load_convention_response&) = default;
};

struct delete_many_intraday_power_load_conventions_request {
    using response_type = struct delete_many_intraday_power_load_conventions_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.intraday_power_load_conventions.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<intraday_power_load_convention_removal> removals;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const delete_many_intraday_power_load_conventions_request&,
                           const delete_many_intraday_power_load_conventions_request&) = default;
};

struct delete_many_intraday_power_load_conventions_response {
    ores::utility::domain::result result;

    friend bool operator==(const delete_many_intraday_power_load_conventions_response&,
                           const delete_many_intraday_power_load_conventions_response&) = default;
};

struct list_intraday_power_load_convention_versions_request {
    using response_type = struct list_intraday_power_load_convention_versions_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.intraday_power_load_conventions_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    intraday_power_load_convention_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<intraday_power_load_convention_versions_filter> filter;

    friend bool operator==(const list_intraday_power_load_convention_versions_request&,
                           const list_intraday_power_load_convention_versions_request&) = default;
};

struct list_intraday_power_load_convention_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::intraday_power_load_convention> versions;
    std::uint64_t total{};

    friend bool operator==(const list_intraday_power_load_convention_versions_response&,
                           const list_intraday_power_load_convention_versions_response&) = default;
};

struct get_intraday_power_load_convention_version_request {
    using response_type = struct get_intraday_power_load_convention_version_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.intraday_power_load_conventions_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    intraday_power_load_convention_version_key key;

    friend bool operator==(const get_intraday_power_load_convention_version_request&,
                           const get_intraday_power_load_convention_version_request&) = default;
};

struct get_intraday_power_load_convention_version_response {
    ores::utility::domain::result result;
    std::optional<ores::refdata::domain::intraday_power_load_convention> version;

    friend bool operator==(const get_intraday_power_load_convention_version_response&,
                           const get_intraday_power_load_convention_version_response&) = default;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace intraday_power_load_convention_event_subjects {
inline constexpr std::string_view created =
    "refdata.v1.events.intraday_power_load_conventions.created";
inline constexpr std::string_view updated =
    "refdata.v1.events.intraday_power_load_conventions.updated";
inline constexpr std::string_view deleted =
    "refdata.v1.events.intraday_power_load_conventions.deleted";
}
/**
 * @brief The members of intraday_power_load_convention_key, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<intraday_power_load_convention_key>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"id", &intraday_power_load_convention_key::id},
    };
}

/**
 * @brief The members of intraday_power_load_convention_write, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<intraday_power_load_convention_write>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"id", &intraday_power_load_convention_write::id},
        utility::codec::field{"explicit_load_profile",
                              &intraday_power_load_convention_write::explicit_load_profile},
        utility::codec::field{"business_day_load_rules",
                              &intraday_power_load_convention_write::business_day_load_rules},
        utility::codec::field{"oresmd_uri", &intraday_power_load_convention_write::oresmd_uri},
    };
}

/**
 * @brief The members of intraday_power_load_convention_change, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<intraday_power_load_convention_change>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"write", &intraday_power_load_convention_change::write},
        utility::codec::field{"precondition", &intraday_power_load_convention_change::precondition},
    };
}

/**
 * @brief The members of intraday_power_load_convention_removal, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<intraday_power_load_convention_removal>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"key", &intraday_power_load_convention_removal::key},
        utility::codec::field{"precondition",
                              &intraday_power_load_convention_removal::precondition},
    };
}

/**
 * @brief The members of intraday_power_load_convention_lookup, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<intraday_power_load_convention_lookup>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"key", &intraday_power_load_convention_lookup::key},
        utility::codec::field{
            "intraday_power_load_convention",
            &intraday_power_load_convention_lookup::intraday_power_load_convention},
    };
}

/**
 * @brief The members of intraday_power_load_conventions_filter, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<intraday_power_load_conventions_filter>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"id_one_of", &intraday_power_load_conventions_filter::id_one_of},
    };
}

/**
 * @brief The members of intraday_power_load_convention_event, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<intraday_power_load_convention_event>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"event_id", &intraday_power_load_convention_event::event_id},
        utility::codec::field{"key", &intraday_power_load_convention_event::key},
        utility::codec::field{"action", &intraday_power_load_convention_event::action},
        utility::codec::field{"version", &intraday_power_load_convention_event::version},
        utility::codec::field{"occurred_at", &intraday_power_load_convention_event::occurred_at},
        utility::codec::field{"correlation_id",
                              &intraday_power_load_convention_event::correlation_id},
    };
}

/**
 * @brief The members of intraday_power_load_convention_version_key, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<intraday_power_load_convention_version_key>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{
            "intraday_power_load_convention",
            &intraday_power_load_convention_version_key::intraday_power_load_convention},
        utility::codec::field{"version", &intraday_power_load_convention_version_key::version},
    };
}

/**
 * @brief The members of intraday_power_load_convention_versions_filter, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<intraday_power_load_convention_versions_filter>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"version", &intraday_power_load_convention_versions_filter::version},
        utility::codec::field{"from_version",
                              &intraday_power_load_convention_versions_filter::from_version},
        utility::codec::field{"to_version",
                              &intraday_power_load_convention_versions_filter::to_version},
    };
}

/**
 * @brief The members of list_intraday_power_load_conventions_request, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<list_intraday_power_load_conventions_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"offset", &list_intraday_power_load_conventions_request::offset},
        utility::codec::field{"limit", &list_intraday_power_load_conventions_request::limit},
        utility::codec::field{"order", &list_intraday_power_load_conventions_request::order},
        utility::codec::field{"filter", &list_intraday_power_load_conventions_request::filter},
        utility::codec::field{"as_of", &list_intraday_power_load_conventions_request::as_of},
    };
}

/**
 * @brief The members of list_intraday_power_load_conventions_response, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<list_intraday_power_load_conventions_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &list_intraday_power_load_conventions_response::result},
        utility::codec::field{
            "intraday_power_load_conventions",
            &list_intraday_power_load_conventions_response::intraday_power_load_conventions},
        utility::codec::field{"total", &list_intraday_power_load_conventions_response::total},
    };
}

/**
 * @brief The members of get_intraday_power_load_convention_request, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<get_intraday_power_load_convention_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"key", &get_intraday_power_load_convention_request::key},
    };
}

/**
 * @brief The members of get_intraday_power_load_convention_response, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<get_intraday_power_load_convention_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &get_intraday_power_load_convention_response::result},
        utility::codec::field{
            "intraday_power_load_convention",
            &get_intraday_power_load_convention_response::intraday_power_load_convention},
    };
}

/**
 * @brief The members of get_many_intraday_power_load_conventions_request, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<get_many_intraday_power_load_conventions_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"keys", &get_many_intraday_power_load_conventions_request::keys},
    };
}

/**
 * @brief The members of get_many_intraday_power_load_conventions_response, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<get_many_intraday_power_load_conventions_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &get_many_intraday_power_load_conventions_response::result},
        utility::codec::field{"entries",
                              &get_many_intraday_power_load_conventions_response::entries},
    };
}

/**
 * @brief The members of put_intraday_power_load_convention_request, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<put_intraday_power_load_convention_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"change", &put_intraday_power_load_convention_request::change},
        utility::codec::field{"intent", &put_intraday_power_load_convention_request::intent},
    };
}

/**
 * @brief The members of put_intraday_power_load_convention_response, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<put_intraday_power_load_convention_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &put_intraday_power_load_convention_response::result},
        utility::codec::field{
            "intraday_power_load_convention",
            &put_intraday_power_load_convention_response::intraday_power_load_convention},
    };
}

/**
 * @brief The members of put_many_intraday_power_load_conventions_request, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<put_many_intraday_power_load_conventions_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"changes",
                              &put_many_intraday_power_load_conventions_request::changes},
        utility::codec::field{"intent", &put_many_intraday_power_load_conventions_request::intent},
    };
}

/**
 * @brief The members of put_many_intraday_power_load_conventions_response, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<put_many_intraday_power_load_conventions_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &put_many_intraday_power_load_conventions_response::result},
        utility::codec::field{
            "intraday_power_load_conventions",
            &put_many_intraday_power_load_conventions_response::intraday_power_load_conventions},
    };
}

/**
 * @brief The members of delete_intraday_power_load_convention_request, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<delete_intraday_power_load_convention_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"removal", &delete_intraday_power_load_convention_request::removal},
        utility::codec::field{"intent", &delete_intraday_power_load_convention_request::intent},
    };
}

/**
 * @brief The members of delete_intraday_power_load_convention_response, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<delete_intraday_power_load_convention_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &delete_intraday_power_load_convention_response::result},
    };
}

/**
 * @brief The members of delete_many_intraday_power_load_conventions_request, named as the wire
 * names them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<delete_many_intraday_power_load_conventions_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"removals",
                              &delete_many_intraday_power_load_conventions_request::removals},
        utility::codec::field{"intent",
                              &delete_many_intraday_power_load_conventions_request::intent},
    };
}

/**
 * @brief The members of delete_many_intraday_power_load_conventions_response, named as the wire
 * names them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<delete_many_intraday_power_load_conventions_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result",
                              &delete_many_intraday_power_load_conventions_response::result},
    };
}

/**
 * @brief The members of list_intraday_power_load_convention_versions_request, named as the wire
 * names them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<list_intraday_power_load_convention_versions_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"key", &list_intraday_power_load_convention_versions_request::key},
        utility::codec::field{"offset",
                              &list_intraday_power_load_convention_versions_request::offset},
        utility::codec::field{"limit",
                              &list_intraday_power_load_convention_versions_request::limit},
        utility::codec::field{"order",
                              &list_intraday_power_load_convention_versions_request::order},
        utility::codec::field{"filter",
                              &list_intraday_power_load_convention_versions_request::filter},
    };
}

/**
 * @brief The members of list_intraday_power_load_convention_versions_response, named as the wire
 * names them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<list_intraday_power_load_convention_versions_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result",
                              &list_intraday_power_load_convention_versions_response::result},
        utility::codec::field{"versions",
                              &list_intraday_power_load_convention_versions_response::versions},
        utility::codec::field{"total",
                              &list_intraday_power_load_convention_versions_response::total},
    };
}

/**
 * @brief The members of get_intraday_power_load_convention_version_request, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<get_intraday_power_load_convention_version_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"key", &get_intraday_power_load_convention_version_request::key},
    };
}

/**
 * @brief The members of get_intraday_power_load_convention_version_response, named as the wire
 * names them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<get_intraday_power_load_convention_version_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result",
                              &get_intraday_power_load_convention_version_response::result},
        utility::codec::field{"version",
                              &get_intraday_power_load_convention_version_response::version},
    };
}


}
namespace ores::utility::codec {

template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::intraday_power_load_convention_key> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::intraday_power_load_convention_key& v);
    static ores::refdata::messaging::intraday_power_load_convention_key
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::intraday_power_load_convention_key& v);
    static ores::refdata::messaging::intraday_power_load_convention_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::intraday_power_load_convention_write> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::intraday_power_load_convention_write& v);
    static ores::refdata::messaging::intraday_power_load_convention_write
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::intraday_power_load_convention_write& v);
    static ores::refdata::messaging::intraday_power_load_convention_write
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::intraday_power_load_convention_change> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::intraday_power_load_convention_change& v);
    static ores::refdata::messaging::intraday_power_load_convention_change
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::intraday_power_load_convention_change& v);
    static ores::refdata::messaging::intraday_power_load_convention_change
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::intraday_power_load_convention_removal> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::intraday_power_load_convention_removal& v);
    static ores::refdata::messaging::intraday_power_load_convention_removal
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::intraday_power_load_convention_removal& v);
    static ores::refdata::messaging::intraday_power_load_convention_removal
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::intraday_power_load_convention_lookup> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::intraday_power_load_convention_lookup& v);
    static ores::refdata::messaging::intraday_power_load_convention_lookup
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::intraday_power_load_convention_lookup& v);
    static ores::refdata::messaging::intraday_power_load_convention_lookup
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::intraday_power_load_conventions_filter> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::intraday_power_load_conventions_filter& v);
    static ores::refdata::messaging::intraday_power_load_conventions_filter
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::intraday_power_load_conventions_filter& v);
    static ores::refdata::messaging::intraday_power_load_conventions_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::intraday_power_load_convention_event> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::intraday_power_load_convention_event& v);
    static ores::refdata::messaging::intraday_power_load_convention_event
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::intraday_power_load_convention_event& v);
    static ores::refdata::messaging::intraday_power_load_convention_event
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::intraday_power_load_convention_version_key> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::intraday_power_load_convention_version_key& v);
    static ores::refdata::messaging::intraday_power_load_convention_version_key
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::intraday_power_load_convention_version_key& v);
    static ores::refdata::messaging::intraday_power_load_convention_version_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::intraday_power_load_convention_versions_filter> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::intraday_power_load_convention_versions_filter& v);
    static ores::refdata::messaging::intraday_power_load_convention_versions_filter
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::refdata::messaging::intraday_power_load_convention_versions_filter& v);
    static ores::refdata::messaging::intraday_power_load_convention_versions_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::list_intraday_power_load_conventions_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::list_intraday_power_load_conventions_request& v);
    static ores::refdata::messaging::list_intraday_power_load_conventions_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::list_intraday_power_load_conventions_request& v);
    static ores::refdata::messaging::list_intraday_power_load_conventions_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::list_intraday_power_load_conventions_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::list_intraday_power_load_conventions_response& v);
    static ores::refdata::messaging::list_intraday_power_load_conventions_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::list_intraday_power_load_conventions_response& v);
    static ores::refdata::messaging::list_intraday_power_load_conventions_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_intraday_power_load_convention_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::get_intraday_power_load_convention_request& v);
    static ores::refdata::messaging::get_intraday_power_load_convention_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::get_intraday_power_load_convention_request& v);
    static ores::refdata::messaging::get_intraday_power_load_convention_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_intraday_power_load_convention_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::get_intraday_power_load_convention_response& v);
    static ores::refdata::messaging::get_intraday_power_load_convention_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::get_intraday_power_load_convention_response& v);
    static ores::refdata::messaging::get_intraday_power_load_convention_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_many_intraday_power_load_conventions_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::get_many_intraday_power_load_conventions_request& v);
    static ores::refdata::messaging::get_many_intraday_power_load_conventions_request
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::refdata::messaging::get_many_intraday_power_load_conventions_request& v);
    static ores::refdata::messaging::get_many_intraday_power_load_conventions_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_many_intraday_power_load_conventions_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::refdata::messaging::get_many_intraday_power_load_conventions_response& v);
    static ores::refdata::messaging::get_many_intraday_power_load_conventions_response
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::refdata::messaging::get_many_intraday_power_load_conventions_response& v);
    static ores::refdata::messaging::get_many_intraday_power_load_conventions_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::put_intraday_power_load_convention_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::put_intraday_power_load_convention_request& v);
    static ores::refdata::messaging::put_intraday_power_load_convention_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::put_intraday_power_load_convention_request& v);
    static ores::refdata::messaging::put_intraday_power_load_convention_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::put_intraday_power_load_convention_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::put_intraday_power_load_convention_response& v);
    static ores::refdata::messaging::put_intraday_power_load_convention_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::put_intraday_power_load_convention_response& v);
    static ores::refdata::messaging::put_intraday_power_load_convention_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::put_many_intraday_power_load_conventions_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::put_many_intraday_power_load_conventions_request& v);
    static ores::refdata::messaging::put_many_intraday_power_load_conventions_request
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::refdata::messaging::put_many_intraday_power_load_conventions_request& v);
    static ores::refdata::messaging::put_many_intraday_power_load_conventions_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::put_many_intraday_power_load_conventions_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::refdata::messaging::put_many_intraday_power_load_conventions_response& v);
    static ores::refdata::messaging::put_many_intraday_power_load_conventions_response
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::refdata::messaging::put_many_intraday_power_load_conventions_response& v);
    static ores::refdata::messaging::put_many_intraday_power_load_conventions_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::delete_intraday_power_load_convention_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::delete_intraday_power_load_convention_request& v);
    static ores::refdata::messaging::delete_intraday_power_load_convention_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::delete_intraday_power_load_convention_request& v);
    static ores::refdata::messaging::delete_intraday_power_load_convention_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::delete_intraday_power_load_convention_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::delete_intraday_power_load_convention_response& v);
    static ores::refdata::messaging::delete_intraday_power_load_convention_response
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::refdata::messaging::delete_intraday_power_load_convention_response& v);
    static ores::refdata::messaging::delete_intraday_power_load_convention_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::delete_many_intraday_power_load_conventions_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::refdata::messaging::delete_many_intraday_power_load_conventions_request& v);
    static ores::refdata::messaging::delete_many_intraday_power_load_conventions_request
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::refdata::messaging::delete_many_intraday_power_load_conventions_request& v);
    static ores::refdata::messaging::delete_many_intraday_power_load_conventions_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::delete_many_intraday_power_load_conventions_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::refdata::messaging::delete_many_intraday_power_load_conventions_response& v);
    static ores::refdata::messaging::delete_many_intraday_power_load_conventions_response
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::refdata::messaging::delete_many_intraday_power_load_conventions_response& v);
    static ores::refdata::messaging::delete_many_intraday_power_load_conventions_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::list_intraday_power_load_convention_versions_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::refdata::messaging::list_intraday_power_load_convention_versions_request& v);
    static ores::refdata::messaging::list_intraday_power_load_convention_versions_request
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::refdata::messaging::list_intraday_power_load_convention_versions_request& v);
    static ores::refdata::messaging::list_intraday_power_load_convention_versions_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::list_intraday_power_load_convention_versions_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::refdata::messaging::list_intraday_power_load_convention_versions_response& v);
    static ores::refdata::messaging::list_intraday_power_load_convention_versions_response
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::refdata::messaging::list_intraday_power_load_convention_versions_response& v);
    static ores::refdata::messaging::list_intraday_power_load_convention_versions_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_intraday_power_load_convention_version_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::refdata::messaging::get_intraday_power_load_convention_version_request& v);
    static ores::refdata::messaging::get_intraday_power_load_convention_version_request
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::refdata::messaging::get_intraday_power_load_convention_version_request& v);
    static ores::refdata::messaging::get_intraday_power_load_convention_version_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_intraday_power_load_convention_version_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::refdata::messaging::get_intraday_power_load_convention_version_response& v);
    static ores::refdata::messaging::get_intraday_power_load_convention_version_response
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::refdata::messaging::get_intraday_power_load_convention_version_response& v);
    static ores::refdata::messaging::get_intraday_power_load_convention_version_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
