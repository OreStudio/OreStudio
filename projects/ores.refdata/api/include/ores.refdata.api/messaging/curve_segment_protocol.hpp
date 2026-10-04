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
#include "ores.utility/domain/protocol.hpp"
#include <boost/uuid/uuid.hpp>
#include <cstdint>
#include <optional>
#include <string>
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
    std::optional<double> spread;
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

}

#endif
