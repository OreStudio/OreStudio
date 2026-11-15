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
#ifndef ORES_REFDATA_API_MESSAGING_COMMODITY_CURVE_DIRECT_PROTOCOL_HPP
#define ORES_REFDATA_API_MESSAGING_COMMODITY_CURVE_DIRECT_PROTOCOL_HPP

#include "ores.refdata.api/domain/commodity_curve_direct_codec.hpp"
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

struct commodity_curve_direct_key {
    boost::uuids::uuid id;

    friend bool operator==(const commodity_curve_direct_key&,
                           const commodity_curve_direct_key&) = default;
};

struct commodity_curve_direct_write {
    boost::uuids::uuid id;
    boost::uuids::uuid curve_definition_id;
    std::optional<std::string> spot_quote;
    std::optional<std::string> conventions;
    bool has_quotes{};

    friend bool operator==(const commodity_curve_direct_write&,
                           const commodity_curve_direct_write&) = default;
};

struct commodity_curve_direct_change {
    commodity_curve_direct_write write;
    ores::utility::domain::precondition precondition;

    friend bool operator==(const commodity_curve_direct_change&,
                           const commodity_curve_direct_change&) = default;
};

struct commodity_curve_direct_removal {
    commodity_curve_direct_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;

    friend bool operator==(const commodity_curve_direct_removal&,
                           const commodity_curve_direct_removal&) = default;
};

struct commodity_curve_direct_lookup {
    commodity_curve_direct_key key;
    std::optional<ores::refdata::domain::commodity_curve_direct> commodity_curve_direct;

    friend bool operator==(const commodity_curve_direct_lookup&,
                           const commodity_curve_direct_lookup&) = default;
};

struct commodity_curve_directs_filter {
    std::optional<std::vector<boost::uuids::uuid>> id_one_of;

    friend bool operator==(const commodity_curve_directs_filter&,
                           const commodity_curve_directs_filter&) = default;
};

struct commodity_curve_direct_event {
    boost::uuids::uuid event_id;
    commodity_curve_direct_key key;
    std::string action;
    std::uint32_t version{};
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;

    friend bool operator==(const commodity_curve_direct_event&,
                           const commodity_curve_direct_event&) = default;
};

struct commodity_curve_direct_version_key {
    commodity_curve_direct_key commodity_curve_direct;
    std::uint32_t version{};

    friend bool operator==(const commodity_curve_direct_version_key&,
                           const commodity_curve_direct_version_key&) = default;
};

struct commodity_curve_direct_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;

    friend bool operator==(const commodity_curve_direct_versions_filter&,
                           const commodity_curve_direct_versions_filter&) = default;
};

struct list_commodity_curve_directs_request {
    using response_type = struct list_commodity_curve_directs_response;
    static constexpr std::string_view nats_subject = "refdata.v1.commodity_curve_directs.list";
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
    std::optional<commodity_curve_directs_filter> filter;
    std::optional<std::string> as_of;

    friend bool operator==(const list_commodity_curve_directs_request&,
                           const list_commodity_curve_directs_request&) = default;
};

struct list_commodity_curve_directs_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::commodity_curve_direct> directs;
    std::uint64_t total{};

    friend bool operator==(const list_commodity_curve_directs_response&,
                           const list_commodity_curve_directs_response&) = default;
};

struct get_commodity_curve_direct_request {
    using response_type = struct get_commodity_curve_direct_response;
    static constexpr std::string_view nats_subject = "refdata.v1.commodity_curve_directs.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    commodity_curve_direct_key key;

    friend bool operator==(const get_commodity_curve_direct_request&,
                           const get_commodity_curve_direct_request&) = default;
};

struct get_commodity_curve_direct_response {
    ores::utility::domain::result result;
    std::optional<ores::refdata::domain::commodity_curve_direct> commodity_curve_direct;

    friend bool operator==(const get_commodity_curve_direct_response&,
                           const get_commodity_curve_direct_response&) = default;
};

struct get_many_commodity_curve_directs_request {
    using response_type = struct get_many_commodity_curve_directs_response;
    static constexpr std::string_view nats_subject = "refdata.v1.commodity_curve_directs.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<commodity_curve_direct_key> keys;

    friend bool operator==(const get_many_commodity_curve_directs_request&,
                           const get_many_commodity_curve_directs_request&) = default;
};

struct get_many_commodity_curve_directs_response {
    ores::utility::domain::result result;
    std::vector<commodity_curve_direct_lookup> entries;

    friend bool operator==(const get_many_commodity_curve_directs_response&,
                           const get_many_commodity_curve_directs_response&) = default;
};

struct put_commodity_curve_direct_request {
    using response_type = struct put_commodity_curve_direct_response;
    static constexpr std::string_view nats_subject = "refdata.v1.commodity_curve_directs.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    commodity_curve_direct_change change;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const put_commodity_curve_direct_request&,
                           const put_commodity_curve_direct_request&) = default;
};

struct put_commodity_curve_direct_response {
    ores::utility::domain::result result;
    std::optional<ores::refdata::domain::commodity_curve_direct> commodity_curve_direct;

    friend bool operator==(const put_commodity_curve_direct_response&,
                           const put_commodity_curve_direct_response&) = default;
};

struct put_many_commodity_curve_directs_request {
    using response_type = struct put_many_commodity_curve_directs_response;
    static constexpr std::string_view nats_subject = "refdata.v1.commodity_curve_directs.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<commodity_curve_direct_change> changes;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const put_many_commodity_curve_directs_request&,
                           const put_many_commodity_curve_directs_request&) = default;
};

struct put_many_commodity_curve_directs_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::commodity_curve_direct> directs;

    friend bool operator==(const put_many_commodity_curve_directs_response&,
                           const put_many_commodity_curve_directs_response&) = default;
};

struct delete_commodity_curve_direct_request {
    using response_type = struct delete_commodity_curve_direct_response;
    static constexpr std::string_view nats_subject = "refdata.v1.commodity_curve_directs.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    commodity_curve_direct_removal removal;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const delete_commodity_curve_direct_request&,
                           const delete_commodity_curve_direct_request&) = default;
};

struct delete_commodity_curve_direct_response {
    ores::utility::domain::result result;

    friend bool operator==(const delete_commodity_curve_direct_response&,
                           const delete_commodity_curve_direct_response&) = default;
};

struct delete_many_commodity_curve_directs_request {
    using response_type = struct delete_many_commodity_curve_directs_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.commodity_curve_directs.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<commodity_curve_direct_removal> removals;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const delete_many_commodity_curve_directs_request&,
                           const delete_many_commodity_curve_directs_request&) = default;
};

struct delete_many_commodity_curve_directs_response {
    ores::utility::domain::result result;

    friend bool operator==(const delete_many_commodity_curve_directs_response&,
                           const delete_many_commodity_curve_directs_response&) = default;
};

struct list_commodity_curve_direct_versions_request {
    using response_type = struct list_commodity_curve_direct_versions_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.commodity_curve_directs_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    commodity_curve_direct_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<commodity_curve_direct_versions_filter> filter;

    friend bool operator==(const list_commodity_curve_direct_versions_request&,
                           const list_commodity_curve_direct_versions_request&) = default;
};

struct list_commodity_curve_direct_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::commodity_curve_direct> versions;
    std::uint64_t total{};

    friend bool operator==(const list_commodity_curve_direct_versions_response&,
                           const list_commodity_curve_direct_versions_response&) = default;
};

struct get_commodity_curve_direct_version_request {
    using response_type = struct get_commodity_curve_direct_version_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.commodity_curve_directs_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    commodity_curve_direct_version_key key;

    friend bool operator==(const get_commodity_curve_direct_version_request&,
                           const get_commodity_curve_direct_version_request&) = default;
};

struct get_commodity_curve_direct_version_response {
    ores::utility::domain::result result;
    std::optional<ores::refdata::domain::commodity_curve_direct> version;

    friend bool operator==(const get_commodity_curve_direct_version_response&,
                           const get_commodity_curve_direct_version_response&) = default;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace commodity_curve_direct_event_subjects {
inline constexpr std::string_view created = "refdata.v1.events.commodity_curve_directs.created";
inline constexpr std::string_view updated = "refdata.v1.events.commodity_curve_directs.updated";
inline constexpr std::string_view deleted = "refdata.v1.events.commodity_curve_directs.deleted";
}
/**
 * @brief The members of commodity_curve_direct_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<commodity_curve_direct_key>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"id", &commodity_curve_direct_key::id},
    };
}

/**
 * @brief The members of commodity_curve_direct_write, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<commodity_curve_direct_write>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"id", &commodity_curve_direct_write::id},
        utility::codec::field{"curve_definition_id",
                              &commodity_curve_direct_write::curve_definition_id},
        utility::codec::field{"spot_quote", &commodity_curve_direct_write::spot_quote},
        utility::codec::field{"conventions", &commodity_curve_direct_write::conventions},
        utility::codec::field{"has_quotes", &commodity_curve_direct_write::has_quotes},
    };
}

/**
 * @brief The members of commodity_curve_direct_change, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<commodity_curve_direct_change>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"write", &commodity_curve_direct_change::write},
        utility::codec::field{"precondition", &commodity_curve_direct_change::precondition},
    };
}

/**
 * @brief The members of commodity_curve_direct_removal, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<commodity_curve_direct_removal>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"key", &commodity_curve_direct_removal::key},
        utility::codec::field{"precondition", &commodity_curve_direct_removal::precondition},
    };
}

/**
 * @brief The members of commodity_curve_direct_lookup, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<commodity_curve_direct_lookup>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"key", &commodity_curve_direct_lookup::key},
        utility::codec::field{"commodity_curve_direct",
                              &commodity_curve_direct_lookup::commodity_curve_direct},
    };
}

/**
 * @brief The members of commodity_curve_directs_filter, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<commodity_curve_directs_filter>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"id_one_of", &commodity_curve_directs_filter::id_one_of},
    };
}

/**
 * @brief The members of commodity_curve_direct_event, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<commodity_curve_direct_event>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"event_id", &commodity_curve_direct_event::event_id},
        utility::codec::field{"key", &commodity_curve_direct_event::key},
        utility::codec::field{"action", &commodity_curve_direct_event::action},
        utility::codec::field{"version", &commodity_curve_direct_event::version},
        utility::codec::field{"occurred_at", &commodity_curve_direct_event::occurred_at},
        utility::codec::field{"correlation_id", &commodity_curve_direct_event::correlation_id},
    };
}

/**
 * @brief The members of commodity_curve_direct_version_key, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<commodity_curve_direct_version_key>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"commodity_curve_direct",
                              &commodity_curve_direct_version_key::commodity_curve_direct},
        utility::codec::field{"version", &commodity_curve_direct_version_key::version},
    };
}

/**
 * @brief The members of commodity_curve_direct_versions_filter, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<commodity_curve_direct_versions_filter>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"version", &commodity_curve_direct_versions_filter::version},
        utility::codec::field{"from_version",
                              &commodity_curve_direct_versions_filter::from_version},
        utility::codec::field{"to_version", &commodity_curve_direct_versions_filter::to_version},
    };
}

/**
 * @brief The members of list_commodity_curve_directs_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<list_commodity_curve_directs_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"offset", &list_commodity_curve_directs_request::offset},
        utility::codec::field{"limit", &list_commodity_curve_directs_request::limit},
        utility::codec::field{"order", &list_commodity_curve_directs_request::order},
        utility::codec::field{"filter", &list_commodity_curve_directs_request::filter},
        utility::codec::field{"as_of", &list_commodity_curve_directs_request::as_of},
    };
}

/**
 * @brief The members of list_commodity_curve_directs_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<list_commodity_curve_directs_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &list_commodity_curve_directs_response::result},
        utility::codec::field{"directs", &list_commodity_curve_directs_response::directs},
        utility::codec::field{"total", &list_commodity_curve_directs_response::total},
    };
}

/**
 * @brief The members of get_commodity_curve_direct_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_commodity_curve_direct_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"key", &get_commodity_curve_direct_request::key},
    };
}

/**
 * @brief The members of get_commodity_curve_direct_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_commodity_curve_direct_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &get_commodity_curve_direct_response::result},
        utility::codec::field{"commodity_curve_direct",
                              &get_commodity_curve_direct_response::commodity_curve_direct},
    };
}

/**
 * @brief The members of get_many_commodity_curve_directs_request, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<get_many_commodity_curve_directs_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"keys", &get_many_commodity_curve_directs_request::keys},
    };
}

/**
 * @brief The members of get_many_commodity_curve_directs_response, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<get_many_commodity_curve_directs_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &get_many_commodity_curve_directs_response::result},
        utility::codec::field{"entries", &get_many_commodity_curve_directs_response::entries},
    };
}

/**
 * @brief The members of put_commodity_curve_direct_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<put_commodity_curve_direct_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"change", &put_commodity_curve_direct_request::change},
        utility::codec::field{"intent", &put_commodity_curve_direct_request::intent},
    };
}

/**
 * @brief The members of put_commodity_curve_direct_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<put_commodity_curve_direct_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &put_commodity_curve_direct_response::result},
        utility::codec::field{"commodity_curve_direct",
                              &put_commodity_curve_direct_response::commodity_curve_direct},
    };
}

/**
 * @brief The members of put_many_commodity_curve_directs_request, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<put_many_commodity_curve_directs_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"changes", &put_many_commodity_curve_directs_request::changes},
        utility::codec::field{"intent", &put_many_commodity_curve_directs_request::intent},
    };
}

/**
 * @brief The members of put_many_commodity_curve_directs_response, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<put_many_commodity_curve_directs_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &put_many_commodity_curve_directs_response::result},
        utility::codec::field{"directs", &put_many_commodity_curve_directs_response::directs},
    };
}

/**
 * @brief The members of delete_commodity_curve_direct_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<delete_commodity_curve_direct_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"removal", &delete_commodity_curve_direct_request::removal},
        utility::codec::field{"intent", &delete_commodity_curve_direct_request::intent},
    };
}

/**
 * @brief The members of delete_commodity_curve_direct_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<delete_commodity_curve_direct_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &delete_commodity_curve_direct_response::result},
    };
}

/**
 * @brief The members of delete_many_commodity_curve_directs_request, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<delete_many_commodity_curve_directs_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"removals", &delete_many_commodity_curve_directs_request::removals},
        utility::codec::field{"intent", &delete_many_commodity_curve_directs_request::intent},
    };
}

/**
 * @brief The members of delete_many_commodity_curve_directs_response, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<delete_many_commodity_curve_directs_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &delete_many_commodity_curve_directs_response::result},
    };
}

/**
 * @brief The members of list_commodity_curve_direct_versions_request, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<list_commodity_curve_direct_versions_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"key", &list_commodity_curve_direct_versions_request::key},
        utility::codec::field{"offset", &list_commodity_curve_direct_versions_request::offset},
        utility::codec::field{"limit", &list_commodity_curve_direct_versions_request::limit},
        utility::codec::field{"order", &list_commodity_curve_direct_versions_request::order},
        utility::codec::field{"filter", &list_commodity_curve_direct_versions_request::filter},
    };
}

/**
 * @brief The members of list_commodity_curve_direct_versions_response, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<list_commodity_curve_direct_versions_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &list_commodity_curve_direct_versions_response::result},
        utility::codec::field{"versions", &list_commodity_curve_direct_versions_response::versions},
        utility::codec::field{"total", &list_commodity_curve_direct_versions_response::total},
    };
}

/**
 * @brief The members of get_commodity_curve_direct_version_request, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<get_commodity_curve_direct_version_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"key", &get_commodity_curve_direct_version_request::key},
    };
}

/**
 * @brief The members of get_commodity_curve_direct_version_response, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<get_commodity_curve_direct_version_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &get_commodity_curve_direct_version_response::result},
        utility::codec::field{"version", &get_commodity_curve_direct_version_response::version},
    };
}


}
namespace ores::utility::codec {

template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::commodity_curve_direct_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::commodity_curve_direct_key& v);
    static ores::refdata::messaging::commodity_curve_direct_key read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::commodity_curve_direct_key& v);
    static ores::refdata::messaging::commodity_curve_direct_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::commodity_curve_direct_write> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::commodity_curve_direct_write& v);
    static ores::refdata::messaging::commodity_curve_direct_write read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::commodity_curve_direct_write& v);
    static ores::refdata::messaging::commodity_curve_direct_write
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::commodity_curve_direct_change> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::commodity_curve_direct_change& v);
    static ores::refdata::messaging::commodity_curve_direct_change read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::commodity_curve_direct_change& v);
    static ores::refdata::messaging::commodity_curve_direct_change
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::commodity_curve_direct_removal> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::commodity_curve_direct_removal& v);
    static ores::refdata::messaging::commodity_curve_direct_removal
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::commodity_curve_direct_removal& v);
    static ores::refdata::messaging::commodity_curve_direct_removal
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::commodity_curve_direct_lookup> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::commodity_curve_direct_lookup& v);
    static ores::refdata::messaging::commodity_curve_direct_lookup read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::commodity_curve_direct_lookup& v);
    static ores::refdata::messaging::commodity_curve_direct_lookup
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::commodity_curve_directs_filter> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::commodity_curve_directs_filter& v);
    static ores::refdata::messaging::commodity_curve_directs_filter
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::commodity_curve_directs_filter& v);
    static ores::refdata::messaging::commodity_curve_directs_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::commodity_curve_direct_event> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::commodity_curve_direct_event& v);
    static ores::refdata::messaging::commodity_curve_direct_event read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::commodity_curve_direct_event& v);
    static ores::refdata::messaging::commodity_curve_direct_event
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::commodity_curve_direct_version_key> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::commodity_curve_direct_version_key& v);
    static ores::refdata::messaging::commodity_curve_direct_version_key
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::commodity_curve_direct_version_key& v);
    static ores::refdata::messaging::commodity_curve_direct_version_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::commodity_curve_direct_versions_filter> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::commodity_curve_direct_versions_filter& v);
    static ores::refdata::messaging::commodity_curve_direct_versions_filter
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::commodity_curve_direct_versions_filter& v);
    static ores::refdata::messaging::commodity_curve_direct_versions_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::list_commodity_curve_directs_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::list_commodity_curve_directs_request& v);
    static ores::refdata::messaging::list_commodity_curve_directs_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::list_commodity_curve_directs_request& v);
    static ores::refdata::messaging::list_commodity_curve_directs_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::list_commodity_curve_directs_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::list_commodity_curve_directs_response& v);
    static ores::refdata::messaging::list_commodity_curve_directs_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::list_commodity_curve_directs_response& v);
    static ores::refdata::messaging::list_commodity_curve_directs_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_commodity_curve_direct_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::get_commodity_curve_direct_request& v);
    static ores::refdata::messaging::get_commodity_curve_direct_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::get_commodity_curve_direct_request& v);
    static ores::refdata::messaging::get_commodity_curve_direct_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_commodity_curve_direct_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::get_commodity_curve_direct_response& v);
    static ores::refdata::messaging::get_commodity_curve_direct_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::get_commodity_curve_direct_response& v);
    static ores::refdata::messaging::get_commodity_curve_direct_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_many_commodity_curve_directs_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::get_many_commodity_curve_directs_request& v);
    static ores::refdata::messaging::get_many_commodity_curve_directs_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::get_many_commodity_curve_directs_request& v);
    static ores::refdata::messaging::get_many_commodity_curve_directs_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_many_commodity_curve_directs_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::get_many_commodity_curve_directs_response& v);
    static ores::refdata::messaging::get_many_commodity_curve_directs_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::get_many_commodity_curve_directs_response& v);
    static ores::refdata::messaging::get_many_commodity_curve_directs_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::put_commodity_curve_direct_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::put_commodity_curve_direct_request& v);
    static ores::refdata::messaging::put_commodity_curve_direct_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::put_commodity_curve_direct_request& v);
    static ores::refdata::messaging::put_commodity_curve_direct_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::put_commodity_curve_direct_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::put_commodity_curve_direct_response& v);
    static ores::refdata::messaging::put_commodity_curve_direct_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::put_commodity_curve_direct_response& v);
    static ores::refdata::messaging::put_commodity_curve_direct_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::put_many_commodity_curve_directs_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::put_many_commodity_curve_directs_request& v);
    static ores::refdata::messaging::put_many_commodity_curve_directs_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::put_many_commodity_curve_directs_request& v);
    static ores::refdata::messaging::put_many_commodity_curve_directs_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::put_many_commodity_curve_directs_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::put_many_commodity_curve_directs_response& v);
    static ores::refdata::messaging::put_many_commodity_curve_directs_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::put_many_commodity_curve_directs_response& v);
    static ores::refdata::messaging::put_many_commodity_curve_directs_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::delete_commodity_curve_direct_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::delete_commodity_curve_direct_request& v);
    static ores::refdata::messaging::delete_commodity_curve_direct_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::delete_commodity_curve_direct_request& v);
    static ores::refdata::messaging::delete_commodity_curve_direct_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::delete_commodity_curve_direct_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::delete_commodity_curve_direct_response& v);
    static ores::refdata::messaging::delete_commodity_curve_direct_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::delete_commodity_curve_direct_response& v);
    static ores::refdata::messaging::delete_commodity_curve_direct_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::delete_many_commodity_curve_directs_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::delete_many_commodity_curve_directs_request& v);
    static ores::refdata::messaging::delete_many_commodity_curve_directs_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::delete_many_commodity_curve_directs_request& v);
    static ores::refdata::messaging::delete_many_commodity_curve_directs_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::delete_many_commodity_curve_directs_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::delete_many_commodity_curve_directs_response& v);
    static ores::refdata::messaging::delete_many_commodity_curve_directs_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::delete_many_commodity_curve_directs_response& v);
    static ores::refdata::messaging::delete_many_commodity_curve_directs_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::list_commodity_curve_direct_versions_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::list_commodity_curve_direct_versions_request& v);
    static ores::refdata::messaging::list_commodity_curve_direct_versions_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::list_commodity_curve_direct_versions_request& v);
    static ores::refdata::messaging::list_commodity_curve_direct_versions_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::list_commodity_curve_direct_versions_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::list_commodity_curve_direct_versions_response& v);
    static ores::refdata::messaging::list_commodity_curve_direct_versions_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::list_commodity_curve_direct_versions_response& v);
    static ores::refdata::messaging::list_commodity_curve_direct_versions_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_commodity_curve_direct_version_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::get_commodity_curve_direct_version_request& v);
    static ores::refdata::messaging::get_commodity_curve_direct_version_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::get_commodity_curve_direct_version_request& v);
    static ores::refdata::messaging::get_commodity_curve_direct_version_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_commodity_curve_direct_version_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::get_commodity_curve_direct_version_response& v);
    static ores::refdata::messaging::get_commodity_curve_direct_version_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::get_commodity_curve_direct_version_response& v);
    static ores::refdata::messaging::get_commodity_curve_direct_version_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
