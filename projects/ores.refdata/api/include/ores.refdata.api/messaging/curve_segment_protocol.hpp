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
#ifndef ORES_REFDATA_API_MESSAGING_CURVE_SEGMENT_PROTOCOL_HPP
#define ORES_REFDATA_API_MESSAGING_CURVE_SEGMENT_PROTOCOL_HPP

#include "ores.refdata.api/domain/curve_segment.hpp"
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

struct curve_segment_key {
    boost::uuids::uuid id;
};

struct curve_segment_write {
    boost::uuids::uuid id;
    boost::uuids::uuid curve_definition_id;
    std::string segment_type;
    int position;
    std::optional<std::string> conventions;
    std::optional<std::string> pillar_choice;
    std::optional<int> priority;
    std::optional<int> min_distance;
    std::optional<std::string> projection_curve;
    std::optional<std::string> discount_curve;
    std::optional<std::string> spot_rate;
    std::optional<std::string> projection_curve_domestic;
    std::optional<std::string> projection_curve_foreign;
    std::optional<std::string> projection_curve_pay;
    std::optional<std::string> projection_curve_receive;
    std::optional<std::string> projection_curve_long;
    std::optional<std::string> projection_curve_short;
    std::optional<std::string> reference_curve;
    std::optional<std::string> reference_curve_2;
    std::optional<double> weight_1;
    std::optional<double> weight_2;
    std::optional<std::string> ibor_index;
    std::optional<std::string> rfr_curve;
    std::optional<std::string> rfr_index;
    std::optional<ores::utility::decimal::decimal> spread;
    std::optional<std::string> base_curve;
    std::optional<std::string> base_curve_currency;
    std::optional<std::string> numerator_curve;
    std::optional<std::string> numerator_curve_currency;
    std::optional<std::string> denominator_curve;
    std::optional<std::string> denominator_curve_currency;
    std::optional<bool> extrapolate_flat;
};

struct curve_segment_change {
    curve_segment_write write;
    ores::utility::domain::precondition precondition;
};

struct curve_segment_removal {
    curve_segment_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;
};

struct curve_segment_lookup {
    curve_segment_key key;
    std::optional<ores::refdata::domain::curve_segment> curve_segment;
};

struct curve_segments_filter {
    std::optional<std::vector<boost::uuids::uuid>> id_one_of;
};

struct curve_segment_event {
    boost::uuids::uuid event_id;
    curve_segment_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;
};

struct curve_segment_version_key {
    curve_segment_key curve_segment;
    std::uint32_t version;
};

struct curve_segment_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;
};

struct list_curve_segments_request {
    using response_type = struct list_curve_segments_response;
    static constexpr std::string_view nats_subject = "refdata.v1.curve_segments.list";
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
    std::optional<curve_segments_filter> filter;
    std::optional<std::string> as_of;
};

struct list_curve_segments_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::curve_segment> segments;
    std::uint64_t total;
};

struct get_curve_segment_request {
    using response_type = struct get_curve_segment_response;
    static constexpr std::string_view nats_subject = "refdata.v1.curve_segments.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    curve_segment_key key;
};

struct get_curve_segment_response {
    ores::utility::domain::result result;
    std::optional<ores::refdata::domain::curve_segment> curve_segment;
};

struct get_many_curve_segments_request {
    using response_type = struct get_many_curve_segments_response;
    static constexpr std::string_view nats_subject = "refdata.v1.curve_segments.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<curve_segment_key> keys;
};

struct get_many_curve_segments_response {
    ores::utility::domain::result result;
    std::vector<curve_segment_lookup> entries;
};

struct put_curve_segment_request {
    using response_type = struct put_curve_segment_response;
    static constexpr std::string_view nats_subject = "refdata.v1.curve_segments.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    curve_segment_change change;
    ores::utility::domain::change_intent intent;
};

struct put_curve_segment_response {
    ores::utility::domain::result result;
    std::optional<ores::refdata::domain::curve_segment> curve_segment;
};

struct put_many_curve_segments_request {
    using response_type = struct put_many_curve_segments_response;
    static constexpr std::string_view nats_subject = "refdata.v1.curve_segments.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<curve_segment_change> changes;
    ores::utility::domain::change_intent intent;
};

struct put_many_curve_segments_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::curve_segment> segments;
};

struct delete_curve_segment_request {
    using response_type = struct delete_curve_segment_response;
    static constexpr std::string_view nats_subject = "refdata.v1.curve_segments.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    curve_segment_removal removal;
    ores::utility::domain::change_intent intent;
};

struct delete_curve_segment_response {
    ores::utility::domain::result result;
};

struct delete_many_curve_segments_request {
    using response_type = struct delete_many_curve_segments_response;
    static constexpr std::string_view nats_subject = "refdata.v1.curve_segments.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<curve_segment_removal> removals;
    ores::utility::domain::change_intent intent;
};

struct delete_many_curve_segments_response {
    ores::utility::domain::result result;
};

struct list_curve_segment_versions_request {
    using response_type = struct list_curve_segment_versions_response;
    static constexpr std::string_view nats_subject = "refdata.v1.curve_segments_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    curve_segment_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<curve_segment_versions_filter> filter;
};

struct list_curve_segment_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::curve_segment> versions;
    std::uint64_t total;
};

struct get_curve_segment_version_request {
    using response_type = struct get_curve_segment_version_response;
    static constexpr std::string_view nats_subject = "refdata.v1.curve_segments_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    curve_segment_version_key key;
};

struct get_curve_segment_version_response {
    ores::utility::domain::result result;
    std::optional<ores::refdata::domain::curve_segment> version;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace curve_segment_event_subjects {
inline constexpr std::string_view created = "refdata.v1.curve_segments_events.created";
inline constexpr std::string_view updated = "refdata.v1.curve_segments_events.updated";
inline constexpr std::string_view deleted = "refdata.v1.curve_segments_events.deleted";
}
/**
 * @brief The members of curve_segment_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const curve_segment_key&) {
    return std::tuple{
        utility::codec::field{"id", &curve_segment_key::id},
    };
}

/**
 * @brief The members of curve_segment_write, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const curve_segment_write&) {
    return std::tuple{
        utility::codec::field{"id", &curve_segment_write::id},
        utility::codec::field{"curve_definition_id", &curve_segment_write::curve_definition_id},
        utility::codec::field{"segment_type", &curve_segment_write::segment_type},
        utility::codec::field{"position", &curve_segment_write::position},
        utility::codec::field{"conventions", &curve_segment_write::conventions},
        utility::codec::field{"pillar_choice", &curve_segment_write::pillar_choice},
        utility::codec::field{"priority", &curve_segment_write::priority},
        utility::codec::field{"min_distance", &curve_segment_write::min_distance},
        utility::codec::field{"projection_curve", &curve_segment_write::projection_curve},
        utility::codec::field{"discount_curve", &curve_segment_write::discount_curve},
        utility::codec::field{"spot_rate", &curve_segment_write::spot_rate},
        utility::codec::field{"projection_curve_domestic",
                              &curve_segment_write::projection_curve_domestic},
        utility::codec::field{"projection_curve_foreign",
                              &curve_segment_write::projection_curve_foreign},
        utility::codec::field{"projection_curve_pay", &curve_segment_write::projection_curve_pay},
        utility::codec::field{"projection_curve_receive",
                              &curve_segment_write::projection_curve_receive},
        utility::codec::field{"projection_curve_long", &curve_segment_write::projection_curve_long},
        utility::codec::field{"projection_curve_short",
                              &curve_segment_write::projection_curve_short},
        utility::codec::field{"reference_curve", &curve_segment_write::reference_curve},
        utility::codec::field{"reference_curve_2", &curve_segment_write::reference_curve_2},
        utility::codec::field{"weight_1", &curve_segment_write::weight_1},
        utility::codec::field{"weight_2", &curve_segment_write::weight_2},
        utility::codec::field{"ibor_index", &curve_segment_write::ibor_index},
        utility::codec::field{"rfr_curve", &curve_segment_write::rfr_curve},
        utility::codec::field{"rfr_index", &curve_segment_write::rfr_index},
        utility::codec::field{"spread", &curve_segment_write::spread},
        utility::codec::field{"base_curve", &curve_segment_write::base_curve},
        utility::codec::field{"base_curve_currency", &curve_segment_write::base_curve_currency},
        utility::codec::field{"numerator_curve", &curve_segment_write::numerator_curve},
        utility::codec::field{"numerator_curve_currency",
                              &curve_segment_write::numerator_curve_currency},
        utility::codec::field{"denominator_curve", &curve_segment_write::denominator_curve},
        utility::codec::field{"denominator_curve_currency",
                              &curve_segment_write::denominator_curve_currency},
        utility::codec::field{"extrapolate_flat", &curve_segment_write::extrapolate_flat},
    };
}

/**
 * @brief The members of curve_segment_change, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const curve_segment_change&) {
    return std::tuple{
        utility::codec::field{"write", &curve_segment_change::write},
        utility::codec::field{"precondition", &curve_segment_change::precondition},
    };
}

/**
 * @brief The members of curve_segment_removal, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const curve_segment_removal&) {
    return std::tuple{
        utility::codec::field{"key", &curve_segment_removal::key},
        utility::codec::field{"precondition", &curve_segment_removal::precondition},
    };
}

/**
 * @brief The members of curve_segment_lookup, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const curve_segment_lookup&) {
    return std::tuple{
        utility::codec::field{"key", &curve_segment_lookup::key},
        utility::codec::field{"curve_segment", &curve_segment_lookup::curve_segment},
    };
}

/**
 * @brief The members of curve_segments_filter, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const curve_segments_filter&) {
    return std::tuple{
        utility::codec::field{"id_one_of", &curve_segments_filter::id_one_of},
    };
}

/**
 * @brief The members of curve_segment_event, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const curve_segment_event&) {
    return std::tuple{
        utility::codec::field{"event_id", &curve_segment_event::event_id},
        utility::codec::field{"key", &curve_segment_event::key},
        utility::codec::field{"action", &curve_segment_event::action},
        utility::codec::field{"version", &curve_segment_event::version},
        utility::codec::field{"occurred_at", &curve_segment_event::occurred_at},
        utility::codec::field{"correlation_id", &curve_segment_event::correlation_id},
    };
}

/**
 * @brief The members of curve_segment_version_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const curve_segment_version_key&) {
    return std::tuple{
        utility::codec::field{"curve_segment", &curve_segment_version_key::curve_segment},
        utility::codec::field{"version", &curve_segment_version_key::version},
    };
}

/**
 * @brief The members of curve_segment_versions_filter, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const curve_segment_versions_filter&) {
    return std::tuple{
        utility::codec::field{"version", &curve_segment_versions_filter::version},
        utility::codec::field{"from_version", &curve_segment_versions_filter::from_version},
        utility::codec::field{"to_version", &curve_segment_versions_filter::to_version},
    };
}

/**
 * @brief The members of list_curve_segments_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_curve_segments_request&) {
    return std::tuple{
        utility::codec::field{"offset", &list_curve_segments_request::offset},
        utility::codec::field{"limit", &list_curve_segments_request::limit},
        utility::codec::field{"order", &list_curve_segments_request::order},
        utility::codec::field{"filter", &list_curve_segments_request::filter},
        utility::codec::field{"as_of", &list_curve_segments_request::as_of},
    };
}

/**
 * @brief The members of list_curve_segments_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_curve_segments_response&) {
    return std::tuple{
        utility::codec::field{"result", &list_curve_segments_response::result},
        utility::codec::field{"segments", &list_curve_segments_response::segments},
        utility::codec::field{"total", &list_curve_segments_response::total},
    };
}

/**
 * @brief The members of get_curve_segment_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_curve_segment_request&) {
    return std::tuple{
        utility::codec::field{"key", &get_curve_segment_request::key},
    };
}

/**
 * @brief The members of get_curve_segment_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_curve_segment_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_curve_segment_response::result},
        utility::codec::field{"curve_segment", &get_curve_segment_response::curve_segment},
    };
}

/**
 * @brief The members of get_many_curve_segments_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_many_curve_segments_request&) {
    return std::tuple{
        utility::codec::field{"keys", &get_many_curve_segments_request::keys},
    };
}

/**
 * @brief The members of get_many_curve_segments_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_many_curve_segments_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_many_curve_segments_response::result},
        utility::codec::field{"entries", &get_many_curve_segments_response::entries},
    };
}

/**
 * @brief The members of put_curve_segment_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_curve_segment_request&) {
    return std::tuple{
        utility::codec::field{"change", &put_curve_segment_request::change},
        utility::codec::field{"intent", &put_curve_segment_request::intent},
    };
}

/**
 * @brief The members of put_curve_segment_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_curve_segment_response&) {
    return std::tuple{
        utility::codec::field{"result", &put_curve_segment_response::result},
        utility::codec::field{"curve_segment", &put_curve_segment_response::curve_segment},
    };
}

/**
 * @brief The members of put_many_curve_segments_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_many_curve_segments_request&) {
    return std::tuple{
        utility::codec::field{"changes", &put_many_curve_segments_request::changes},
        utility::codec::field{"intent", &put_many_curve_segments_request::intent},
    };
}

/**
 * @brief The members of put_many_curve_segments_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_many_curve_segments_response&) {
    return std::tuple{
        utility::codec::field{"result", &put_many_curve_segments_response::result},
        utility::codec::field{"segments", &put_many_curve_segments_response::segments},
    };
}

/**
 * @brief The members of delete_curve_segment_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_curve_segment_request&) {
    return std::tuple{
        utility::codec::field{"removal", &delete_curve_segment_request::removal},
        utility::codec::field{"intent", &delete_curve_segment_request::intent},
    };
}

/**
 * @brief The members of delete_curve_segment_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_curve_segment_response&) {
    return std::tuple{
        utility::codec::field{"result", &delete_curve_segment_response::result},
    };
}

/**
 * @brief The members of delete_many_curve_segments_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_many_curve_segments_request&) {
    return std::tuple{
        utility::codec::field{"removals", &delete_many_curve_segments_request::removals},
        utility::codec::field{"intent", &delete_many_curve_segments_request::intent},
    };
}

/**
 * @brief The members of delete_many_curve_segments_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_many_curve_segments_response&) {
    return std::tuple{
        utility::codec::field{"result", &delete_many_curve_segments_response::result},
    };
}

/**
 * @brief The members of list_curve_segment_versions_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_curve_segment_versions_request&) {
    return std::tuple{
        utility::codec::field{"key", &list_curve_segment_versions_request::key},
        utility::codec::field{"offset", &list_curve_segment_versions_request::offset},
        utility::codec::field{"limit", &list_curve_segment_versions_request::limit},
        utility::codec::field{"order", &list_curve_segment_versions_request::order},
        utility::codec::field{"filter", &list_curve_segment_versions_request::filter},
    };
}

/**
 * @brief The members of list_curve_segment_versions_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_curve_segment_versions_response&) {
    return std::tuple{
        utility::codec::field{"result", &list_curve_segment_versions_response::result},
        utility::codec::field{"versions", &list_curve_segment_versions_response::versions},
        utility::codec::field{"total", &list_curve_segment_versions_response::total},
    };
}

/**
 * @brief The members of get_curve_segment_version_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_curve_segment_version_request&) {
    return std::tuple{
        utility::codec::field{"key", &get_curve_segment_version_request::key},
    };
}

/**
 * @brief The members of get_curve_segment_version_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_curve_segment_version_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_curve_segment_version_response::result},
        utility::codec::field{"version", &get_curve_segment_version_response::version},
    };
}


}
namespace ores::utility::codec {

template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::curve_segment_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::curve_segment_key& v);
    static ores::refdata::messaging::curve_segment_key read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::curve_segment_key& v);
    static ores::refdata::messaging::curve_segment_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::curve_segment_write> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::curve_segment_write& v);
    static ores::refdata::messaging::curve_segment_write read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::curve_segment_write& v);
    static ores::refdata::messaging::curve_segment_write
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::curve_segment_change> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::curve_segment_change& v);
    static ores::refdata::messaging::curve_segment_change read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::curve_segment_change& v);
    static ores::refdata::messaging::curve_segment_change
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::curve_segment_removal> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::curve_segment_removal& v);
    static ores::refdata::messaging::curve_segment_removal read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::curve_segment_removal& v);
    static ores::refdata::messaging::curve_segment_removal
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::curve_segment_lookup> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::curve_segment_lookup& v);
    static ores::refdata::messaging::curve_segment_lookup read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::curve_segment_lookup& v);
    static ores::refdata::messaging::curve_segment_lookup
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::curve_segments_filter> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::curve_segments_filter& v);
    static ores::refdata::messaging::curve_segments_filter read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::curve_segments_filter& v);
    static ores::refdata::messaging::curve_segments_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::curve_segment_event> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::curve_segment_event& v);
    static ores::refdata::messaging::curve_segment_event read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::curve_segment_event& v);
    static ores::refdata::messaging::curve_segment_event
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::curve_segment_version_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::curve_segment_version_key& v);
    static ores::refdata::messaging::curve_segment_version_key read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::curve_segment_version_key& v);
    static ores::refdata::messaging::curve_segment_version_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::curve_segment_versions_filter> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::curve_segment_versions_filter& v);
    static ores::refdata::messaging::curve_segment_versions_filter read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::curve_segment_versions_filter& v);
    static ores::refdata::messaging::curve_segment_versions_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::list_curve_segments_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::list_curve_segments_request& v);
    static ores::refdata::messaging::list_curve_segments_request read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::list_curve_segments_request& v);
    static ores::refdata::messaging::list_curve_segments_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::list_curve_segments_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::list_curve_segments_response& v);
    static ores::refdata::messaging::list_curve_segments_response read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::list_curve_segments_response& v);
    static ores::refdata::messaging::list_curve_segments_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::get_curve_segment_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::get_curve_segment_request& v);
    static ores::refdata::messaging::get_curve_segment_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::get_curve_segment_request& v);
    static ores::refdata::messaging::get_curve_segment_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::get_curve_segment_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::get_curve_segment_response& v);
    static ores::refdata::messaging::get_curve_segment_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::get_curve_segment_response& v);
    static ores::refdata::messaging::get_curve_segment_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_many_curve_segments_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::get_many_curve_segments_request& v);
    static ores::refdata::messaging::get_many_curve_segments_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::get_many_curve_segments_request& v);
    static ores::refdata::messaging::get_many_curve_segments_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_many_curve_segments_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::get_many_curve_segments_response& v);
    static ores::refdata::messaging::get_many_curve_segments_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::get_many_curve_segments_response& v);
    static ores::refdata::messaging::get_many_curve_segments_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::put_curve_segment_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::put_curve_segment_request& v);
    static ores::refdata::messaging::put_curve_segment_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::put_curve_segment_request& v);
    static ores::refdata::messaging::put_curve_segment_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::put_curve_segment_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::put_curve_segment_response& v);
    static ores::refdata::messaging::put_curve_segment_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::put_curve_segment_response& v);
    static ores::refdata::messaging::put_curve_segment_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::put_many_curve_segments_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::put_many_curve_segments_request& v);
    static ores::refdata::messaging::put_many_curve_segments_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::put_many_curve_segments_request& v);
    static ores::refdata::messaging::put_many_curve_segments_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::put_many_curve_segments_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::put_many_curve_segments_response& v);
    static ores::refdata::messaging::put_many_curve_segments_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::put_many_curve_segments_response& v);
    static ores::refdata::messaging::put_many_curve_segments_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::delete_curve_segment_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::delete_curve_segment_request& v);
    static ores::refdata::messaging::delete_curve_segment_request read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::delete_curve_segment_request& v);
    static ores::refdata::messaging::delete_curve_segment_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::delete_curve_segment_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::delete_curve_segment_response& v);
    static ores::refdata::messaging::delete_curve_segment_response read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::delete_curve_segment_response& v);
    static ores::refdata::messaging::delete_curve_segment_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::delete_many_curve_segments_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::delete_many_curve_segments_request& v);
    static ores::refdata::messaging::delete_many_curve_segments_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::delete_many_curve_segments_request& v);
    static ores::refdata::messaging::delete_many_curve_segments_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::delete_many_curve_segments_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::delete_many_curve_segments_response& v);
    static ores::refdata::messaging::delete_many_curve_segments_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::delete_many_curve_segments_response& v);
    static ores::refdata::messaging::delete_many_curve_segments_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::list_curve_segment_versions_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::list_curve_segment_versions_request& v);
    static ores::refdata::messaging::list_curve_segment_versions_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::list_curve_segment_versions_request& v);
    static ores::refdata::messaging::list_curve_segment_versions_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::list_curve_segment_versions_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::list_curve_segment_versions_response& v);
    static ores::refdata::messaging::list_curve_segment_versions_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::list_curve_segment_versions_response& v);
    static ores::refdata::messaging::list_curve_segment_versions_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_curve_segment_version_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::get_curve_segment_version_request& v);
    static ores::refdata::messaging::get_curve_segment_version_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::get_curve_segment_version_request& v);
    static ores::refdata::messaging::get_curve_segment_version_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_curve_segment_version_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::get_curve_segment_version_response& v);
    static ores::refdata::messaging::get_curve_segment_version_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::get_curve_segment_version_response& v);
    static ores::refdata::messaging::get_curve_segment_version_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
