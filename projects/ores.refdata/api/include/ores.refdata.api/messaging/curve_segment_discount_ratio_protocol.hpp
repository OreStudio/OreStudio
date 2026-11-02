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
#ifndef ORES_REFDATA_API_MESSAGING_CURVE_SEGMENT_DISCOUNT_RATIO_PROTOCOL_HPP
#define ORES_REFDATA_API_MESSAGING_CURVE_SEGMENT_DISCOUNT_RATIO_PROTOCOL_HPP

#include "ores.refdata.api/domain/curve_segment_discount_ratio.hpp"
#include "ores.refdata.api/export.hpp"
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

namespace ores::refdata::messaging {

struct curve_segment_discount_ratio_key {
    boost::uuids::uuid id;

    friend bool operator==(const curve_segment_discount_ratio_key&,
                           const curve_segment_discount_ratio_key&) = default;
};

struct curve_segment_discount_ratio_write {
    boost::uuids::uuid id;
    boost::uuids::uuid curve_segment_id;
    std::string base_curve;
    std::string base_curve_currency;
    std::string numerator_curve;
    std::string numerator_curve_currency;
    std::string denominator_curve;
    std::string denominator_curve_currency;

    friend bool operator==(const curve_segment_discount_ratio_write&,
                           const curve_segment_discount_ratio_write&) = default;
};

struct curve_segment_discount_ratio_change {
    curve_segment_discount_ratio_write write;
    ores::utility::domain::precondition precondition;

    friend bool operator==(const curve_segment_discount_ratio_change&,
                           const curve_segment_discount_ratio_change&) = default;
};

struct curve_segment_discount_ratio_removal {
    curve_segment_discount_ratio_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;

    friend bool operator==(const curve_segment_discount_ratio_removal&,
                           const curve_segment_discount_ratio_removal&) = default;
};

struct curve_segment_discount_ratio_lookup {
    curve_segment_discount_ratio_key key;
    std::optional<ores::refdata::domain::curve_segment_discount_ratio> curve_segment_discount_ratio;

    friend bool operator==(const curve_segment_discount_ratio_lookup&,
                           const curve_segment_discount_ratio_lookup&) = default;
};

struct curve_segment_discount_ratios_filter {
    std::optional<std::vector<boost::uuids::uuid>> id_one_of;

    friend bool operator==(const curve_segment_discount_ratios_filter&,
                           const curve_segment_discount_ratios_filter&) = default;
};

struct curve_segment_discount_ratio_event {
    boost::uuids::uuid event_id;
    curve_segment_discount_ratio_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;

    friend bool operator==(const curve_segment_discount_ratio_event&,
                           const curve_segment_discount_ratio_event&) = default;
};

struct curve_segment_discount_ratio_version_key {
    curve_segment_discount_ratio_key curve_segment_discount_ratio;
    std::uint32_t version;

    friend bool operator==(const curve_segment_discount_ratio_version_key&,
                           const curve_segment_discount_ratio_version_key&) = default;
};

struct curve_segment_discount_ratio_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;

    friend bool operator==(const curve_segment_discount_ratio_versions_filter&,
                           const curve_segment_discount_ratio_versions_filter&) = default;
};

struct list_curve_segment_discount_ratios_request {
    using response_type = struct list_curve_segment_discount_ratios_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.curve_segment_discount_ratios.list";
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
    std::optional<curve_segment_discount_ratios_filter> filter;
    std::optional<std::string> as_of;

    friend bool operator==(const list_curve_segment_discount_ratios_request&,
                           const list_curve_segment_discount_ratios_request&) = default;
};

struct list_curve_segment_discount_ratios_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::curve_segment_discount_ratio> segment_discount_ratios;
    std::uint64_t total;

    friend bool operator==(const list_curve_segment_discount_ratios_response&,
                           const list_curve_segment_discount_ratios_response&) = default;
};

struct get_curve_segment_discount_ratio_request {
    using response_type = struct get_curve_segment_discount_ratio_response;
    static constexpr std::string_view nats_subject = "refdata.v1.curve_segment_discount_ratios.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    curve_segment_discount_ratio_key key;

    friend bool operator==(const get_curve_segment_discount_ratio_request&,
                           const get_curve_segment_discount_ratio_request&) = default;
};

struct get_curve_segment_discount_ratio_response {
    ores::utility::domain::result result;
    std::optional<ores::refdata::domain::curve_segment_discount_ratio> curve_segment_discount_ratio;

    friend bool operator==(const get_curve_segment_discount_ratio_response&,
                           const get_curve_segment_discount_ratio_response&) = default;
};

struct get_many_curve_segment_discount_ratios_request {
    using response_type = struct get_many_curve_segment_discount_ratios_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.curve_segment_discount_ratios.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<curve_segment_discount_ratio_key> keys;

    friend bool operator==(const get_many_curve_segment_discount_ratios_request&,
                           const get_many_curve_segment_discount_ratios_request&) = default;
};

struct get_many_curve_segment_discount_ratios_response {
    ores::utility::domain::result result;
    std::vector<curve_segment_discount_ratio_lookup> entries;

    friend bool operator==(const get_many_curve_segment_discount_ratios_response&,
                           const get_many_curve_segment_discount_ratios_response&) = default;
};

struct put_curve_segment_discount_ratio_request {
    using response_type = struct put_curve_segment_discount_ratio_response;
    static constexpr std::string_view nats_subject = "refdata.v1.curve_segment_discount_ratios.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    curve_segment_discount_ratio_change change;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const put_curve_segment_discount_ratio_request&,
                           const put_curve_segment_discount_ratio_request&) = default;
};

struct put_curve_segment_discount_ratio_response {
    ores::utility::domain::result result;
    std::optional<ores::refdata::domain::curve_segment_discount_ratio> curve_segment_discount_ratio;

    friend bool operator==(const put_curve_segment_discount_ratio_response&,
                           const put_curve_segment_discount_ratio_response&) = default;
};

struct put_many_curve_segment_discount_ratios_request {
    using response_type = struct put_many_curve_segment_discount_ratios_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.curve_segment_discount_ratios.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<curve_segment_discount_ratio_change> changes;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const put_many_curve_segment_discount_ratios_request&,
                           const put_many_curve_segment_discount_ratios_request&) = default;
};

struct put_many_curve_segment_discount_ratios_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::curve_segment_discount_ratio> segment_discount_ratios;

    friend bool operator==(const put_many_curve_segment_discount_ratios_response&,
                           const put_many_curve_segment_discount_ratios_response&) = default;
};

struct delete_curve_segment_discount_ratio_request {
    using response_type = struct delete_curve_segment_discount_ratio_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.curve_segment_discount_ratios.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    curve_segment_discount_ratio_removal removal;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const delete_curve_segment_discount_ratio_request&,
                           const delete_curve_segment_discount_ratio_request&) = default;
};

struct delete_curve_segment_discount_ratio_response {
    ores::utility::domain::result result;

    friend bool operator==(const delete_curve_segment_discount_ratio_response&,
                           const delete_curve_segment_discount_ratio_response&) = default;
};

struct delete_many_curve_segment_discount_ratios_request {
    using response_type = struct delete_many_curve_segment_discount_ratios_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.curve_segment_discount_ratios.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<curve_segment_discount_ratio_removal> removals;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const delete_many_curve_segment_discount_ratios_request&,
                           const delete_many_curve_segment_discount_ratios_request&) = default;
};

struct delete_many_curve_segment_discount_ratios_response {
    ores::utility::domain::result result;

    friend bool operator==(const delete_many_curve_segment_discount_ratios_response&,
                           const delete_many_curve_segment_discount_ratios_response&) = default;
};

struct list_curve_segment_discount_ratio_versions_request {
    using response_type = struct list_curve_segment_discount_ratio_versions_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.curve_segment_discount_ratios_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    curve_segment_discount_ratio_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<curve_segment_discount_ratio_versions_filter> filter;

    friend bool operator==(const list_curve_segment_discount_ratio_versions_request&,
                           const list_curve_segment_discount_ratio_versions_request&) = default;
};

struct list_curve_segment_discount_ratio_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::curve_segment_discount_ratio> versions;
    std::uint64_t total;

    friend bool operator==(const list_curve_segment_discount_ratio_versions_response&,
                           const list_curve_segment_discount_ratio_versions_response&) = default;
};

struct get_curve_segment_discount_ratio_version_request {
    using response_type = struct get_curve_segment_discount_ratio_version_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.curve_segment_discount_ratios_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    curve_segment_discount_ratio_version_key key;

    friend bool operator==(const get_curve_segment_discount_ratio_version_request&,
                           const get_curve_segment_discount_ratio_version_request&) = default;
};

struct get_curve_segment_discount_ratio_version_response {
    ores::utility::domain::result result;
    std::optional<ores::refdata::domain::curve_segment_discount_ratio> version;

    friend bool operator==(const get_curve_segment_discount_ratio_version_response&,
                           const get_curve_segment_discount_ratio_version_response&) = default;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace curve_segment_discount_ratio_event_subjects {
inline constexpr std::string_view created =
    "refdata.v1.curve_segment_discount_ratios_events.created";
inline constexpr std::string_view updated =
    "refdata.v1.curve_segment_discount_ratios_events.updated";
inline constexpr std::string_view deleted =
    "refdata.v1.curve_segment_discount_ratios_events.deleted";
}
/**
 * @brief The members of curve_segment_discount_ratio_key, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const curve_segment_discount_ratio_key&) {
    return std::tuple{
        utility::codec::field{"id", &curve_segment_discount_ratio_key::id},
    };
}

/**
 * @brief The members of curve_segment_discount_ratio_write, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const curve_segment_discount_ratio_write&) {
    return std::tuple{
        utility::codec::field{"id", &curve_segment_discount_ratio_write::id},
        utility::codec::field{"curve_segment_id",
                              &curve_segment_discount_ratio_write::curve_segment_id},
        utility::codec::field{"base_curve", &curve_segment_discount_ratio_write::base_curve},
        utility::codec::field{"base_curve_currency",
                              &curve_segment_discount_ratio_write::base_curve_currency},
        utility::codec::field{"numerator_curve",
                              &curve_segment_discount_ratio_write::numerator_curve},
        utility::codec::field{"numerator_curve_currency",
                              &curve_segment_discount_ratio_write::numerator_curve_currency},
        utility::codec::field{"denominator_curve",
                              &curve_segment_discount_ratio_write::denominator_curve},
        utility::codec::field{"denominator_curve_currency",
                              &curve_segment_discount_ratio_write::denominator_curve_currency},
    };
}

/**
 * @brief The members of curve_segment_discount_ratio_change, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const curve_segment_discount_ratio_change&) {
    return std::tuple{
        utility::codec::field{"write", &curve_segment_discount_ratio_change::write},
        utility::codec::field{"precondition", &curve_segment_discount_ratio_change::precondition},
    };
}

/**
 * @brief The members of curve_segment_discount_ratio_removal, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const curve_segment_discount_ratio_removal&) {
    return std::tuple{
        utility::codec::field{"key", &curve_segment_discount_ratio_removal::key},
        utility::codec::field{"precondition", &curve_segment_discount_ratio_removal::precondition},
    };
}

/**
 * @brief The members of curve_segment_discount_ratio_lookup, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const curve_segment_discount_ratio_lookup&) {
    return std::tuple{
        utility::codec::field{"key", &curve_segment_discount_ratio_lookup::key},
        utility::codec::field{"curve_segment_discount_ratio",
                              &curve_segment_discount_ratio_lookup::curve_segment_discount_ratio},
    };
}

/**
 * @brief The members of curve_segment_discount_ratios_filter, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const curve_segment_discount_ratios_filter&) {
    return std::tuple{
        utility::codec::field{"id_one_of", &curve_segment_discount_ratios_filter::id_one_of},
    };
}

/**
 * @brief The members of curve_segment_discount_ratio_event, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const curve_segment_discount_ratio_event&) {
    return std::tuple{
        utility::codec::field{"event_id", &curve_segment_discount_ratio_event::event_id},
        utility::codec::field{"key", &curve_segment_discount_ratio_event::key},
        utility::codec::field{"action", &curve_segment_discount_ratio_event::action},
        utility::codec::field{"version", &curve_segment_discount_ratio_event::version},
        utility::codec::field{"occurred_at", &curve_segment_discount_ratio_event::occurred_at},
        utility::codec::field{"correlation_id",
                              &curve_segment_discount_ratio_event::correlation_id},
    };
}

/**
 * @brief The members of curve_segment_discount_ratio_version_key, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const curve_segment_discount_ratio_version_key&) {
    return std::tuple{
        utility::codec::field{
            "curve_segment_discount_ratio",
            &curve_segment_discount_ratio_version_key::curve_segment_discount_ratio},
        utility::codec::field{"version", &curve_segment_discount_ratio_version_key::version},
    };
}

/**
 * @brief The members of curve_segment_discount_ratio_versions_filter, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const curve_segment_discount_ratio_versions_filter&) {
    return std::tuple{
        utility::codec::field{"version", &curve_segment_discount_ratio_versions_filter::version},
        utility::codec::field{"from_version",
                              &curve_segment_discount_ratio_versions_filter::from_version},
        utility::codec::field{"to_version",
                              &curve_segment_discount_ratio_versions_filter::to_version},
    };
}

/**
 * @brief The members of list_curve_segment_discount_ratios_request, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_curve_segment_discount_ratios_request&) {
    return std::tuple{
        utility::codec::field{"offset", &list_curve_segment_discount_ratios_request::offset},
        utility::codec::field{"limit", &list_curve_segment_discount_ratios_request::limit},
        utility::codec::field{"order", &list_curve_segment_discount_ratios_request::order},
        utility::codec::field{"filter", &list_curve_segment_discount_ratios_request::filter},
        utility::codec::field{"as_of", &list_curve_segment_discount_ratios_request::as_of},
    };
}

/**
 * @brief The members of list_curve_segment_discount_ratios_response, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_curve_segment_discount_ratios_response&) {
    return std::tuple{
        utility::codec::field{"result", &list_curve_segment_discount_ratios_response::result},
        utility::codec::field{
            "segment_discount_ratios",
            &list_curve_segment_discount_ratios_response::segment_discount_ratios},
        utility::codec::field{"total", &list_curve_segment_discount_ratios_response::total},
    };
}

/**
 * @brief The members of get_curve_segment_discount_ratio_request, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_curve_segment_discount_ratio_request&) {
    return std::tuple{
        utility::codec::field{"key", &get_curve_segment_discount_ratio_request::key},
    };
}

/**
 * @brief The members of get_curve_segment_discount_ratio_response, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_curve_segment_discount_ratio_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_curve_segment_discount_ratio_response::result},
        utility::codec::field{
            "curve_segment_discount_ratio",
            &get_curve_segment_discount_ratio_response::curve_segment_discount_ratio},
    };
}

/**
 * @brief The members of get_many_curve_segment_discount_ratios_request, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_many_curve_segment_discount_ratios_request&) {
    return std::tuple{
        utility::codec::field{"keys", &get_many_curve_segment_discount_ratios_request::keys},
    };
}

/**
 * @brief The members of get_many_curve_segment_discount_ratios_response, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_many_curve_segment_discount_ratios_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_many_curve_segment_discount_ratios_response::result},
        utility::codec::field{"entries", &get_many_curve_segment_discount_ratios_response::entries},
    };
}

/**
 * @brief The members of put_curve_segment_discount_ratio_request, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_curve_segment_discount_ratio_request&) {
    return std::tuple{
        utility::codec::field{"change", &put_curve_segment_discount_ratio_request::change},
        utility::codec::field{"intent", &put_curve_segment_discount_ratio_request::intent},
    };
}

/**
 * @brief The members of put_curve_segment_discount_ratio_response, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_curve_segment_discount_ratio_response&) {
    return std::tuple{
        utility::codec::field{"result", &put_curve_segment_discount_ratio_response::result},
        utility::codec::field{
            "curve_segment_discount_ratio",
            &put_curve_segment_discount_ratio_response::curve_segment_discount_ratio},
    };
}

/**
 * @brief The members of put_many_curve_segment_discount_ratios_request, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_many_curve_segment_discount_ratios_request&) {
    return std::tuple{
        utility::codec::field{"changes", &put_many_curve_segment_discount_ratios_request::changes},
        utility::codec::field{"intent", &put_many_curve_segment_discount_ratios_request::intent},
    };
}

/**
 * @brief The members of put_many_curve_segment_discount_ratios_response, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_many_curve_segment_discount_ratios_response&) {
    return std::tuple{
        utility::codec::field{"result", &put_many_curve_segment_discount_ratios_response::result},
        utility::codec::field{
            "segment_discount_ratios",
            &put_many_curve_segment_discount_ratios_response::segment_discount_ratios},
    };
}

/**
 * @brief The members of delete_curve_segment_discount_ratio_request, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_curve_segment_discount_ratio_request&) {
    return std::tuple{
        utility::codec::field{"removal", &delete_curve_segment_discount_ratio_request::removal},
        utility::codec::field{"intent", &delete_curve_segment_discount_ratio_request::intent},
    };
}

/**
 * @brief The members of delete_curve_segment_discount_ratio_response, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_curve_segment_discount_ratio_response&) {
    return std::tuple{
        utility::codec::field{"result", &delete_curve_segment_discount_ratio_response::result},
    };
}

/**
 * @brief The members of delete_many_curve_segment_discount_ratios_request, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_many_curve_segment_discount_ratios_request&) {
    return std::tuple{
        utility::codec::field{"removals",
                              &delete_many_curve_segment_discount_ratios_request::removals},
        utility::codec::field{"intent", &delete_many_curve_segment_discount_ratios_request::intent},
    };
}

/**
 * @brief The members of delete_many_curve_segment_discount_ratios_response, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(const delete_many_curve_segment_discount_ratios_response&) {
    return std::tuple{
        utility::codec::field{"result",
                              &delete_many_curve_segment_discount_ratios_response::result},
    };
}

/**
 * @brief The members of list_curve_segment_discount_ratio_versions_request, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(const list_curve_segment_discount_ratio_versions_request&) {
    return std::tuple{
        utility::codec::field{"key", &list_curve_segment_discount_ratio_versions_request::key},
        utility::codec::field{"offset",
                              &list_curve_segment_discount_ratio_versions_request::offset},
        utility::codec::field{"limit", &list_curve_segment_discount_ratio_versions_request::limit},
        utility::codec::field{"order", &list_curve_segment_discount_ratio_versions_request::order},
        utility::codec::field{"filter",
                              &list_curve_segment_discount_ratio_versions_request::filter},
    };
}

/**
 * @brief The members of list_curve_segment_discount_ratio_versions_response, named as the wire
 * names them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(const list_curve_segment_discount_ratio_versions_response&) {
    return std::tuple{
        utility::codec::field{"result",
                              &list_curve_segment_discount_ratio_versions_response::result},
        utility::codec::field{"versions",
                              &list_curve_segment_discount_ratio_versions_response::versions},
        utility::codec::field{"total", &list_curve_segment_discount_ratio_versions_response::total},
    };
}

/**
 * @brief The members of get_curve_segment_discount_ratio_version_request, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_curve_segment_discount_ratio_version_request&) {
    return std::tuple{
        utility::codec::field{"key", &get_curve_segment_discount_ratio_version_request::key},
    };
}

/**
 * @brief The members of get_curve_segment_discount_ratio_version_response, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_curve_segment_discount_ratio_version_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_curve_segment_discount_ratio_version_response::result},
        utility::codec::field{"version",
                              &get_curve_segment_discount_ratio_version_response::version},
    };
}


}
namespace ores::utility::codec {

template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::curve_segment_discount_ratio_key> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::curve_segment_discount_ratio_key& v);
    static ores::refdata::messaging::curve_segment_discount_ratio_key
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::curve_segment_discount_ratio_key& v);
    static ores::refdata::messaging::curve_segment_discount_ratio_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::curve_segment_discount_ratio_write> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::curve_segment_discount_ratio_write& v);
    static ores::refdata::messaging::curve_segment_discount_ratio_write
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::curve_segment_discount_ratio_write& v);
    static ores::refdata::messaging::curve_segment_discount_ratio_write
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::curve_segment_discount_ratio_change> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::curve_segment_discount_ratio_change& v);
    static ores::refdata::messaging::curve_segment_discount_ratio_change
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::curve_segment_discount_ratio_change& v);
    static ores::refdata::messaging::curve_segment_discount_ratio_change
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::curve_segment_discount_ratio_removal> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::curve_segment_discount_ratio_removal& v);
    static ores::refdata::messaging::curve_segment_discount_ratio_removal
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::curve_segment_discount_ratio_removal& v);
    static ores::refdata::messaging::curve_segment_discount_ratio_removal
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::curve_segment_discount_ratio_lookup> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::curve_segment_discount_ratio_lookup& v);
    static ores::refdata::messaging::curve_segment_discount_ratio_lookup
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::curve_segment_discount_ratio_lookup& v);
    static ores::refdata::messaging::curve_segment_discount_ratio_lookup
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::curve_segment_discount_ratios_filter> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::curve_segment_discount_ratios_filter& v);
    static ores::refdata::messaging::curve_segment_discount_ratios_filter
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::curve_segment_discount_ratios_filter& v);
    static ores::refdata::messaging::curve_segment_discount_ratios_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::curve_segment_discount_ratio_event> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::curve_segment_discount_ratio_event& v);
    static ores::refdata::messaging::curve_segment_discount_ratio_event
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::curve_segment_discount_ratio_event& v);
    static ores::refdata::messaging::curve_segment_discount_ratio_event
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::curve_segment_discount_ratio_version_key> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::curve_segment_discount_ratio_version_key& v);
    static ores::refdata::messaging::curve_segment_discount_ratio_version_key
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::curve_segment_discount_ratio_version_key& v);
    static ores::refdata::messaging::curve_segment_discount_ratio_version_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::curve_segment_discount_ratio_versions_filter> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::curve_segment_discount_ratio_versions_filter& v);
    static ores::refdata::messaging::curve_segment_discount_ratio_versions_filter
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::curve_segment_discount_ratio_versions_filter& v);
    static ores::refdata::messaging::curve_segment_discount_ratio_versions_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::list_curve_segment_discount_ratios_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::list_curve_segment_discount_ratios_request& v);
    static ores::refdata::messaging::list_curve_segment_discount_ratios_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::list_curve_segment_discount_ratios_request& v);
    static ores::refdata::messaging::list_curve_segment_discount_ratios_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::list_curve_segment_discount_ratios_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::list_curve_segment_discount_ratios_response& v);
    static ores::refdata::messaging::list_curve_segment_discount_ratios_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::list_curve_segment_discount_ratios_response& v);
    static ores::refdata::messaging::list_curve_segment_discount_ratios_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_curve_segment_discount_ratio_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::get_curve_segment_discount_ratio_request& v);
    static ores::refdata::messaging::get_curve_segment_discount_ratio_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::get_curve_segment_discount_ratio_request& v);
    static ores::refdata::messaging::get_curve_segment_discount_ratio_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_curve_segment_discount_ratio_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::get_curve_segment_discount_ratio_response& v);
    static ores::refdata::messaging::get_curve_segment_discount_ratio_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::get_curve_segment_discount_ratio_response& v);
    static ores::refdata::messaging::get_curve_segment_discount_ratio_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_many_curve_segment_discount_ratios_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::get_many_curve_segment_discount_ratios_request& v);
    static ores::refdata::messaging::get_many_curve_segment_discount_ratios_request
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::refdata::messaging::get_many_curve_segment_discount_ratios_request& v);
    static ores::refdata::messaging::get_many_curve_segment_discount_ratios_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_many_curve_segment_discount_ratios_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::get_many_curve_segment_discount_ratios_response& v);
    static ores::refdata::messaging::get_many_curve_segment_discount_ratios_response
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::refdata::messaging::get_many_curve_segment_discount_ratios_response& v);
    static ores::refdata::messaging::get_many_curve_segment_discount_ratios_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::put_curve_segment_discount_ratio_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::put_curve_segment_discount_ratio_request& v);
    static ores::refdata::messaging::put_curve_segment_discount_ratio_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::put_curve_segment_discount_ratio_request& v);
    static ores::refdata::messaging::put_curve_segment_discount_ratio_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::put_curve_segment_discount_ratio_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::put_curve_segment_discount_ratio_response& v);
    static ores::refdata::messaging::put_curve_segment_discount_ratio_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::put_curve_segment_discount_ratio_response& v);
    static ores::refdata::messaging::put_curve_segment_discount_ratio_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::put_many_curve_segment_discount_ratios_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::put_many_curve_segment_discount_ratios_request& v);
    static ores::refdata::messaging::put_many_curve_segment_discount_ratios_request
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::refdata::messaging::put_many_curve_segment_discount_ratios_request& v);
    static ores::refdata::messaging::put_many_curve_segment_discount_ratios_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::put_many_curve_segment_discount_ratios_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::put_many_curve_segment_discount_ratios_response& v);
    static ores::refdata::messaging::put_many_curve_segment_discount_ratios_response
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::refdata::messaging::put_many_curve_segment_discount_ratios_response& v);
    static ores::refdata::messaging::put_many_curve_segment_discount_ratios_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::delete_curve_segment_discount_ratio_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::delete_curve_segment_discount_ratio_request& v);
    static ores::refdata::messaging::delete_curve_segment_discount_ratio_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::delete_curve_segment_discount_ratio_request& v);
    static ores::refdata::messaging::delete_curve_segment_discount_ratio_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::delete_curve_segment_discount_ratio_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::delete_curve_segment_discount_ratio_response& v);
    static ores::refdata::messaging::delete_curve_segment_discount_ratio_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::delete_curve_segment_discount_ratio_response& v);
    static ores::refdata::messaging::delete_curve_segment_discount_ratio_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::delete_many_curve_segment_discount_ratios_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::refdata::messaging::delete_many_curve_segment_discount_ratios_request& v);
    static ores::refdata::messaging::delete_many_curve_segment_discount_ratios_request
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::refdata::messaging::delete_many_curve_segment_discount_ratios_request& v);
    static ores::refdata::messaging::delete_many_curve_segment_discount_ratios_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::delete_many_curve_segment_discount_ratios_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::refdata::messaging::delete_many_curve_segment_discount_ratios_response& v);
    static ores::refdata::messaging::delete_many_curve_segment_discount_ratios_response
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::refdata::messaging::delete_many_curve_segment_discount_ratios_response& v);
    static ores::refdata::messaging::delete_many_curve_segment_discount_ratios_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::list_curve_segment_discount_ratio_versions_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::refdata::messaging::list_curve_segment_discount_ratio_versions_request& v);
    static ores::refdata::messaging::list_curve_segment_discount_ratio_versions_request
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::refdata::messaging::list_curve_segment_discount_ratio_versions_request& v);
    static ores::refdata::messaging::list_curve_segment_discount_ratio_versions_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::list_curve_segment_discount_ratio_versions_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::refdata::messaging::list_curve_segment_discount_ratio_versions_response& v);
    static ores::refdata::messaging::list_curve_segment_discount_ratio_versions_response
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::refdata::messaging::list_curve_segment_discount_ratio_versions_response& v);
    static ores::refdata::messaging::list_curve_segment_discount_ratio_versions_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_curve_segment_discount_ratio_version_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::get_curve_segment_discount_ratio_version_request& v);
    static ores::refdata::messaging::get_curve_segment_discount_ratio_version_request
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::refdata::messaging::get_curve_segment_discount_ratio_version_request& v);
    static ores::refdata::messaging::get_curve_segment_discount_ratio_version_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_curve_segment_discount_ratio_version_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::refdata::messaging::get_curve_segment_discount_ratio_version_response& v);
    static ores::refdata::messaging::get_curve_segment_discount_ratio_version_response
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::refdata::messaging::get_curve_segment_discount_ratio_version_response& v);
    static ores::refdata::messaging::get_curve_segment_discount_ratio_version_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
