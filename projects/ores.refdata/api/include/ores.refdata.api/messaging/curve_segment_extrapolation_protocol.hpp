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
#ifndef ORES_REFDATA_API_MESSAGING_CURVE_SEGMENT_EXTRAPOLATION_PROTOCOL_HPP
#define ORES_REFDATA_API_MESSAGING_CURVE_SEGMENT_EXTRAPOLATION_PROTOCOL_HPP

#include "ores.refdata.api/domain/curve_segment_extrapolation_codec.hpp"
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

struct curve_segment_extrapolation_key {
    boost::uuids::uuid id;

    friend bool operator==(const curve_segment_extrapolation_key&,
                           const curve_segment_extrapolation_key&) = default;
};

struct curve_segment_extrapolation_write {
    boost::uuids::uuid id;
    boost::uuids::uuid curve_segment_id;
    bool extrapolate_flat{};

    friend bool operator==(const curve_segment_extrapolation_write&,
                           const curve_segment_extrapolation_write&) = default;
};

struct curve_segment_extrapolation_change {
    curve_segment_extrapolation_write write;
    ores::utility::domain::precondition precondition;

    friend bool operator==(const curve_segment_extrapolation_change&,
                           const curve_segment_extrapolation_change&) = default;
};

struct curve_segment_extrapolation_removal {
    curve_segment_extrapolation_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;

    friend bool operator==(const curve_segment_extrapolation_removal&,
                           const curve_segment_extrapolation_removal&) = default;
};

struct curve_segment_extrapolation_lookup {
    curve_segment_extrapolation_key key;
    std::optional<ores::refdata::domain::curve_segment_extrapolation> curve_segment_extrapolation;

    friend bool operator==(const curve_segment_extrapolation_lookup&,
                           const curve_segment_extrapolation_lookup&) = default;
};

struct curve_segment_extrapolations_filter {
    std::optional<std::vector<boost::uuids::uuid>> id_one_of;

    friend bool operator==(const curve_segment_extrapolations_filter&,
                           const curve_segment_extrapolations_filter&) = default;
};

struct curve_segment_extrapolation_event {
    boost::uuids::uuid event_id;
    curve_segment_extrapolation_key key;
    std::string action;
    std::uint32_t version{};
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;

    friend bool operator==(const curve_segment_extrapolation_event&,
                           const curve_segment_extrapolation_event&) = default;
};

struct curve_segment_extrapolation_version_key {
    curve_segment_extrapolation_key curve_segment_extrapolation;
    std::uint32_t version{};

    friend bool operator==(const curve_segment_extrapolation_version_key&,
                           const curve_segment_extrapolation_version_key&) = default;
};

struct curve_segment_extrapolation_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;

    friend bool operator==(const curve_segment_extrapolation_versions_filter&,
                           const curve_segment_extrapolation_versions_filter&) = default;
};

struct list_curve_segment_extrapolations_request {
    using response_type = struct list_curve_segment_extrapolations_response;
    static constexpr std::string_view nats_subject = "refdata.v1.curve_segment_extrapolations.list";
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
    std::optional<curve_segment_extrapolations_filter> filter;
    std::optional<std::string> as_of;

    friend bool operator==(const list_curve_segment_extrapolations_request&,
                           const list_curve_segment_extrapolations_request&) = default;
};

struct list_curve_segment_extrapolations_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::curve_segment_extrapolation> segment_extrapolations;
    std::uint64_t total{};

    friend bool operator==(const list_curve_segment_extrapolations_response&,
                           const list_curve_segment_extrapolations_response&) = default;
};

struct get_curve_segment_extrapolation_request {
    using response_type = struct get_curve_segment_extrapolation_response;
    static constexpr std::string_view nats_subject = "refdata.v1.curve_segment_extrapolations.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    curve_segment_extrapolation_key key;

    friend bool operator==(const get_curve_segment_extrapolation_request&,
                           const get_curve_segment_extrapolation_request&) = default;
};

struct get_curve_segment_extrapolation_response {
    ores::utility::domain::result result;
    std::optional<ores::refdata::domain::curve_segment_extrapolation> curve_segment_extrapolation;

    friend bool operator==(const get_curve_segment_extrapolation_response&,
                           const get_curve_segment_extrapolation_response&) = default;
};

struct get_many_curve_segment_extrapolations_request {
    using response_type = struct get_many_curve_segment_extrapolations_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.curve_segment_extrapolations.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<curve_segment_extrapolation_key> keys;

    friend bool operator==(const get_many_curve_segment_extrapolations_request&,
                           const get_many_curve_segment_extrapolations_request&) = default;
};

struct get_many_curve_segment_extrapolations_response {
    ores::utility::domain::result result;
    std::vector<curve_segment_extrapolation_lookup> entries;

    friend bool operator==(const get_many_curve_segment_extrapolations_response&,
                           const get_many_curve_segment_extrapolations_response&) = default;
};

struct put_curve_segment_extrapolation_request {
    using response_type = struct put_curve_segment_extrapolation_response;
    static constexpr std::string_view nats_subject = "refdata.v1.curve_segment_extrapolations.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    curve_segment_extrapolation_change change;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const put_curve_segment_extrapolation_request&,
                           const put_curve_segment_extrapolation_request&) = default;
};

struct put_curve_segment_extrapolation_response {
    ores::utility::domain::result result;
    std::optional<ores::refdata::domain::curve_segment_extrapolation> curve_segment_extrapolation;

    friend bool operator==(const put_curve_segment_extrapolation_response&,
                           const put_curve_segment_extrapolation_response&) = default;
};

struct put_many_curve_segment_extrapolations_request {
    using response_type = struct put_many_curve_segment_extrapolations_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.curve_segment_extrapolations.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<curve_segment_extrapolation_change> changes;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const put_many_curve_segment_extrapolations_request&,
                           const put_many_curve_segment_extrapolations_request&) = default;
};

struct put_many_curve_segment_extrapolations_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::curve_segment_extrapolation> segment_extrapolations;

    friend bool operator==(const put_many_curve_segment_extrapolations_response&,
                           const put_many_curve_segment_extrapolations_response&) = default;
};

struct delete_curve_segment_extrapolation_request {
    using response_type = struct delete_curve_segment_extrapolation_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.curve_segment_extrapolations.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    curve_segment_extrapolation_removal removal;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const delete_curve_segment_extrapolation_request&,
                           const delete_curve_segment_extrapolation_request&) = default;
};

struct delete_curve_segment_extrapolation_response {
    ores::utility::domain::result result;

    friend bool operator==(const delete_curve_segment_extrapolation_response&,
                           const delete_curve_segment_extrapolation_response&) = default;
};

struct delete_many_curve_segment_extrapolations_request {
    using response_type = struct delete_many_curve_segment_extrapolations_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.curve_segment_extrapolations.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<curve_segment_extrapolation_removal> removals;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const delete_many_curve_segment_extrapolations_request&,
                           const delete_many_curve_segment_extrapolations_request&) = default;
};

struct delete_many_curve_segment_extrapolations_response {
    ores::utility::domain::result result;

    friend bool operator==(const delete_many_curve_segment_extrapolations_response&,
                           const delete_many_curve_segment_extrapolations_response&) = default;
};

struct list_curve_segment_extrapolation_versions_request {
    using response_type = struct list_curve_segment_extrapolation_versions_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.curve_segment_extrapolations_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    curve_segment_extrapolation_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<curve_segment_extrapolation_versions_filter> filter;

    friend bool operator==(const list_curve_segment_extrapolation_versions_request&,
                           const list_curve_segment_extrapolation_versions_request&) = default;
};

struct list_curve_segment_extrapolation_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::curve_segment_extrapolation> versions;
    std::uint64_t total{};

    friend bool operator==(const list_curve_segment_extrapolation_versions_response&,
                           const list_curve_segment_extrapolation_versions_response&) = default;
};

struct get_curve_segment_extrapolation_version_request {
    using response_type = struct get_curve_segment_extrapolation_version_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.curve_segment_extrapolations_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    curve_segment_extrapolation_version_key key;

    friend bool operator==(const get_curve_segment_extrapolation_version_request&,
                           const get_curve_segment_extrapolation_version_request&) = default;
};

struct get_curve_segment_extrapolation_version_response {
    ores::utility::domain::result result;
    std::optional<ores::refdata::domain::curve_segment_extrapolation> version;

    friend bool operator==(const get_curve_segment_extrapolation_version_response&,
                           const get_curve_segment_extrapolation_version_response&) = default;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace curve_segment_extrapolation_event_subjects {
inline constexpr std::string_view created =
    "refdata.v1.events.curve_segment_extrapolations.created";
inline constexpr std::string_view updated =
    "refdata.v1.events.curve_segment_extrapolations.updated";
inline constexpr std::string_view deleted =
    "refdata.v1.events.curve_segment_extrapolations.deleted";
}
/**
 * @brief The members of curve_segment_extrapolation_key, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<curve_segment_extrapolation_key>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"id", &curve_segment_extrapolation_key::id},
    };
}

/**
 * @brief The members of curve_segment_extrapolation_write, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<curve_segment_extrapolation_write>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"id", &curve_segment_extrapolation_write::id},
        utility::codec::field{"curve_segment_id",
                              &curve_segment_extrapolation_write::curve_segment_id},
        utility::codec::field{"extrapolate_flat",
                              &curve_segment_extrapolation_write::extrapolate_flat},
    };
}

/**
 * @brief The members of curve_segment_extrapolation_change, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<curve_segment_extrapolation_change>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"write", &curve_segment_extrapolation_change::write},
        utility::codec::field{"precondition", &curve_segment_extrapolation_change::precondition},
    };
}

/**
 * @brief The members of curve_segment_extrapolation_removal, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<curve_segment_extrapolation_removal>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"key", &curve_segment_extrapolation_removal::key},
        utility::codec::field{"precondition", &curve_segment_extrapolation_removal::precondition},
    };
}

/**
 * @brief The members of curve_segment_extrapolation_lookup, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<curve_segment_extrapolation_lookup>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"key", &curve_segment_extrapolation_lookup::key},
        utility::codec::field{"curve_segment_extrapolation",
                              &curve_segment_extrapolation_lookup::curve_segment_extrapolation},
    };
}

/**
 * @brief The members of curve_segment_extrapolations_filter, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<curve_segment_extrapolations_filter>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"id_one_of", &curve_segment_extrapolations_filter::id_one_of},
    };
}

/**
 * @brief The members of curve_segment_extrapolation_event, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<curve_segment_extrapolation_event>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"event_id", &curve_segment_extrapolation_event::event_id},
        utility::codec::field{"key", &curve_segment_extrapolation_event::key},
        utility::codec::field{"action", &curve_segment_extrapolation_event::action},
        utility::codec::field{"version", &curve_segment_extrapolation_event::version},
        utility::codec::field{"occurred_at", &curve_segment_extrapolation_event::occurred_at},
        utility::codec::field{"correlation_id", &curve_segment_extrapolation_event::correlation_id},
    };
}

/**
 * @brief The members of curve_segment_extrapolation_version_key, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<curve_segment_extrapolation_version_key>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{
            "curve_segment_extrapolation",
            &curve_segment_extrapolation_version_key::curve_segment_extrapolation},
        utility::codec::field{"version", &curve_segment_extrapolation_version_key::version},
    };
}

/**
 * @brief The members of curve_segment_extrapolation_versions_filter, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<curve_segment_extrapolation_versions_filter>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"version", &curve_segment_extrapolation_versions_filter::version},
        utility::codec::field{"from_version",
                              &curve_segment_extrapolation_versions_filter::from_version},
        utility::codec::field{"to_version",
                              &curve_segment_extrapolation_versions_filter::to_version},
    };
}

/**
 * @brief The members of list_curve_segment_extrapolations_request, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<list_curve_segment_extrapolations_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"offset", &list_curve_segment_extrapolations_request::offset},
        utility::codec::field{"limit", &list_curve_segment_extrapolations_request::limit},
        utility::codec::field{"order", &list_curve_segment_extrapolations_request::order},
        utility::codec::field{"filter", &list_curve_segment_extrapolations_request::filter},
        utility::codec::field{"as_of", &list_curve_segment_extrapolations_request::as_of},
    };
}

/**
 * @brief The members of list_curve_segment_extrapolations_response, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<list_curve_segment_extrapolations_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &list_curve_segment_extrapolations_response::result},
        utility::codec::field{"segment_extrapolations",
                              &list_curve_segment_extrapolations_response::segment_extrapolations},
        utility::codec::field{"total", &list_curve_segment_extrapolations_response::total},
    };
}

/**
 * @brief The members of get_curve_segment_extrapolation_request, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<get_curve_segment_extrapolation_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"key", &get_curve_segment_extrapolation_request::key},
    };
}

/**
 * @brief The members of get_curve_segment_extrapolation_response, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<get_curve_segment_extrapolation_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &get_curve_segment_extrapolation_response::result},
        utility::codec::field{
            "curve_segment_extrapolation",
            &get_curve_segment_extrapolation_response::curve_segment_extrapolation},
    };
}

/**
 * @brief The members of get_many_curve_segment_extrapolations_request, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<get_many_curve_segment_extrapolations_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"keys", &get_many_curve_segment_extrapolations_request::keys},
    };
}

/**
 * @brief The members of get_many_curve_segment_extrapolations_response, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<get_many_curve_segment_extrapolations_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &get_many_curve_segment_extrapolations_response::result},
        utility::codec::field{"entries", &get_many_curve_segment_extrapolations_response::entries},
    };
}

/**
 * @brief The members of put_curve_segment_extrapolation_request, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<put_curve_segment_extrapolation_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"change", &put_curve_segment_extrapolation_request::change},
        utility::codec::field{"intent", &put_curve_segment_extrapolation_request::intent},
    };
}

/**
 * @brief The members of put_curve_segment_extrapolation_response, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<put_curve_segment_extrapolation_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &put_curve_segment_extrapolation_response::result},
        utility::codec::field{
            "curve_segment_extrapolation",
            &put_curve_segment_extrapolation_response::curve_segment_extrapolation},
    };
}

/**
 * @brief The members of put_many_curve_segment_extrapolations_request, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<put_many_curve_segment_extrapolations_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"changes", &put_many_curve_segment_extrapolations_request::changes},
        utility::codec::field{"intent", &put_many_curve_segment_extrapolations_request::intent},
    };
}

/**
 * @brief The members of put_many_curve_segment_extrapolations_response, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<put_many_curve_segment_extrapolations_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &put_many_curve_segment_extrapolations_response::result},
        utility::codec::field{
            "segment_extrapolations",
            &put_many_curve_segment_extrapolations_response::segment_extrapolations},
    };
}

/**
 * @brief The members of delete_curve_segment_extrapolation_request, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<delete_curve_segment_extrapolation_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"removal", &delete_curve_segment_extrapolation_request::removal},
        utility::codec::field{"intent", &delete_curve_segment_extrapolation_request::intent},
    };
}

/**
 * @brief The members of delete_curve_segment_extrapolation_response, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<delete_curve_segment_extrapolation_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &delete_curve_segment_extrapolation_response::result},
    };
}

/**
 * @brief The members of delete_many_curve_segment_extrapolations_request, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<delete_many_curve_segment_extrapolations_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"removals",
                              &delete_many_curve_segment_extrapolations_request::removals},
        utility::codec::field{"intent", &delete_many_curve_segment_extrapolations_request::intent},
    };
}

/**
 * @brief The members of delete_many_curve_segment_extrapolations_response, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<delete_many_curve_segment_extrapolations_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &delete_many_curve_segment_extrapolations_response::result},
    };
}

/**
 * @brief The members of list_curve_segment_extrapolation_versions_request, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<list_curve_segment_extrapolation_versions_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"key", &list_curve_segment_extrapolation_versions_request::key},
        utility::codec::field{"offset", &list_curve_segment_extrapolation_versions_request::offset},
        utility::codec::field{"limit", &list_curve_segment_extrapolation_versions_request::limit},
        utility::codec::field{"order", &list_curve_segment_extrapolation_versions_request::order},
        utility::codec::field{"filter", &list_curve_segment_extrapolation_versions_request::filter},
    };
}

/**
 * @brief The members of list_curve_segment_extrapolation_versions_response, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<list_curve_segment_extrapolation_versions_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result",
                              &list_curve_segment_extrapolation_versions_response::result},
        utility::codec::field{"versions",
                              &list_curve_segment_extrapolation_versions_response::versions},
        utility::codec::field{"total", &list_curve_segment_extrapolation_versions_response::total},
    };
}

/**
 * @brief The members of get_curve_segment_extrapolation_version_request, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<get_curve_segment_extrapolation_version_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"key", &get_curve_segment_extrapolation_version_request::key},
    };
}

/**
 * @brief The members of get_curve_segment_extrapolation_version_response, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<get_curve_segment_extrapolation_version_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &get_curve_segment_extrapolation_version_response::result},
        utility::codec::field{"version",
                              &get_curve_segment_extrapolation_version_response::version},
    };
}


}
namespace ores::utility::codec {

template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::curve_segment_extrapolation_key> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::curve_segment_extrapolation_key& v);
    static ores::refdata::messaging::curve_segment_extrapolation_key
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::curve_segment_extrapolation_key& v);
    static ores::refdata::messaging::curve_segment_extrapolation_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::curve_segment_extrapolation_write> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::curve_segment_extrapolation_write& v);
    static ores::refdata::messaging::curve_segment_extrapolation_write
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::curve_segment_extrapolation_write& v);
    static ores::refdata::messaging::curve_segment_extrapolation_write
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::curve_segment_extrapolation_change> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::curve_segment_extrapolation_change& v);
    static ores::refdata::messaging::curve_segment_extrapolation_change
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::curve_segment_extrapolation_change& v);
    static ores::refdata::messaging::curve_segment_extrapolation_change
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::curve_segment_extrapolation_removal> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::curve_segment_extrapolation_removal& v);
    static ores::refdata::messaging::curve_segment_extrapolation_removal
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::curve_segment_extrapolation_removal& v);
    static ores::refdata::messaging::curve_segment_extrapolation_removal
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::curve_segment_extrapolation_lookup> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::curve_segment_extrapolation_lookup& v);
    static ores::refdata::messaging::curve_segment_extrapolation_lookup
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::curve_segment_extrapolation_lookup& v);
    static ores::refdata::messaging::curve_segment_extrapolation_lookup
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::curve_segment_extrapolations_filter> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::curve_segment_extrapolations_filter& v);
    static ores::refdata::messaging::curve_segment_extrapolations_filter
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::curve_segment_extrapolations_filter& v);
    static ores::refdata::messaging::curve_segment_extrapolations_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::curve_segment_extrapolation_event> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::curve_segment_extrapolation_event& v);
    static ores::refdata::messaging::curve_segment_extrapolation_event
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::curve_segment_extrapolation_event& v);
    static ores::refdata::messaging::curve_segment_extrapolation_event
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::curve_segment_extrapolation_version_key> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::curve_segment_extrapolation_version_key& v);
    static ores::refdata::messaging::curve_segment_extrapolation_version_key
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::curve_segment_extrapolation_version_key& v);
    static ores::refdata::messaging::curve_segment_extrapolation_version_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::curve_segment_extrapolation_versions_filter> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::curve_segment_extrapolation_versions_filter& v);
    static ores::refdata::messaging::curve_segment_extrapolation_versions_filter
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::curve_segment_extrapolation_versions_filter& v);
    static ores::refdata::messaging::curve_segment_extrapolation_versions_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::list_curve_segment_extrapolations_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::list_curve_segment_extrapolations_request& v);
    static ores::refdata::messaging::list_curve_segment_extrapolations_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::list_curve_segment_extrapolations_request& v);
    static ores::refdata::messaging::list_curve_segment_extrapolations_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::list_curve_segment_extrapolations_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::list_curve_segment_extrapolations_response& v);
    static ores::refdata::messaging::list_curve_segment_extrapolations_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::list_curve_segment_extrapolations_response& v);
    static ores::refdata::messaging::list_curve_segment_extrapolations_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_curve_segment_extrapolation_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::get_curve_segment_extrapolation_request& v);
    static ores::refdata::messaging::get_curve_segment_extrapolation_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::get_curve_segment_extrapolation_request& v);
    static ores::refdata::messaging::get_curve_segment_extrapolation_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_curve_segment_extrapolation_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::get_curve_segment_extrapolation_response& v);
    static ores::refdata::messaging::get_curve_segment_extrapolation_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::get_curve_segment_extrapolation_response& v);
    static ores::refdata::messaging::get_curve_segment_extrapolation_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_many_curve_segment_extrapolations_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::get_many_curve_segment_extrapolations_request& v);
    static ores::refdata::messaging::get_many_curve_segment_extrapolations_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::get_many_curve_segment_extrapolations_request& v);
    static ores::refdata::messaging::get_many_curve_segment_extrapolations_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_many_curve_segment_extrapolations_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::get_many_curve_segment_extrapolations_response& v);
    static ores::refdata::messaging::get_many_curve_segment_extrapolations_response
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::refdata::messaging::get_many_curve_segment_extrapolations_response& v);
    static ores::refdata::messaging::get_many_curve_segment_extrapolations_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::put_curve_segment_extrapolation_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::put_curve_segment_extrapolation_request& v);
    static ores::refdata::messaging::put_curve_segment_extrapolation_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::put_curve_segment_extrapolation_request& v);
    static ores::refdata::messaging::put_curve_segment_extrapolation_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::put_curve_segment_extrapolation_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::put_curve_segment_extrapolation_response& v);
    static ores::refdata::messaging::put_curve_segment_extrapolation_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::put_curve_segment_extrapolation_response& v);
    static ores::refdata::messaging::put_curve_segment_extrapolation_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::put_many_curve_segment_extrapolations_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::put_many_curve_segment_extrapolations_request& v);
    static ores::refdata::messaging::put_many_curve_segment_extrapolations_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::put_many_curve_segment_extrapolations_request& v);
    static ores::refdata::messaging::put_many_curve_segment_extrapolations_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::put_many_curve_segment_extrapolations_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::put_many_curve_segment_extrapolations_response& v);
    static ores::refdata::messaging::put_many_curve_segment_extrapolations_response
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::refdata::messaging::put_many_curve_segment_extrapolations_response& v);
    static ores::refdata::messaging::put_many_curve_segment_extrapolations_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::delete_curve_segment_extrapolation_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::delete_curve_segment_extrapolation_request& v);
    static ores::refdata::messaging::delete_curve_segment_extrapolation_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::delete_curve_segment_extrapolation_request& v);
    static ores::refdata::messaging::delete_curve_segment_extrapolation_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::delete_curve_segment_extrapolation_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::delete_curve_segment_extrapolation_response& v);
    static ores::refdata::messaging::delete_curve_segment_extrapolation_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::delete_curve_segment_extrapolation_response& v);
    static ores::refdata::messaging::delete_curve_segment_extrapolation_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::delete_many_curve_segment_extrapolations_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::delete_many_curve_segment_extrapolations_request& v);
    static ores::refdata::messaging::delete_many_curve_segment_extrapolations_request
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::refdata::messaging::delete_many_curve_segment_extrapolations_request& v);
    static ores::refdata::messaging::delete_many_curve_segment_extrapolations_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::delete_many_curve_segment_extrapolations_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::refdata::messaging::delete_many_curve_segment_extrapolations_response& v);
    static ores::refdata::messaging::delete_many_curve_segment_extrapolations_response
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::refdata::messaging::delete_many_curve_segment_extrapolations_response& v);
    static ores::refdata::messaging::delete_many_curve_segment_extrapolations_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::list_curve_segment_extrapolation_versions_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::refdata::messaging::list_curve_segment_extrapolation_versions_request& v);
    static ores::refdata::messaging::list_curve_segment_extrapolation_versions_request
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::refdata::messaging::list_curve_segment_extrapolation_versions_request& v);
    static ores::refdata::messaging::list_curve_segment_extrapolation_versions_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::list_curve_segment_extrapolation_versions_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::refdata::messaging::list_curve_segment_extrapolation_versions_response& v);
    static ores::refdata::messaging::list_curve_segment_extrapolation_versions_response
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::refdata::messaging::list_curve_segment_extrapolation_versions_response& v);
    static ores::refdata::messaging::list_curve_segment_extrapolation_versions_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_curve_segment_extrapolation_version_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::get_curve_segment_extrapolation_version_request& v);
    static ores::refdata::messaging::get_curve_segment_extrapolation_version_request
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::refdata::messaging::get_curve_segment_extrapolation_version_request& v);
    static ores::refdata::messaging::get_curve_segment_extrapolation_version_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_curve_segment_extrapolation_version_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::get_curve_segment_extrapolation_version_response& v);
    static ores::refdata::messaging::get_curve_segment_extrapolation_version_response
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::refdata::messaging::get_curve_segment_extrapolation_version_response& v);
    static ores::refdata::messaging::get_curve_segment_extrapolation_version_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
