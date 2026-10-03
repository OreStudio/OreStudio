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
#ifndef ORES_REFDATA_API_MESSAGING_BASE_CORRELATION_PROTOCOL_HPP
#define ORES_REFDATA_API_MESSAGING_BASE_CORRELATION_PROTOCOL_HPP

#include "ores.refdata.api/domain/base_correlation.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <boost/uuid/uuid.hpp>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::refdata::messaging {

struct base_correlation_key {
    boost::uuids::uuid id;
};

struct base_correlation_write {
    boost::uuids::uuid id;
    boost::uuids::uuid curve_definition_id;
    std::string terms;
    std::string detachment_points;
    double settlement_days;
    std::string calendar;
    std::string business_day_convention;
    std::string day_counter;
    std::optional<std::string> extrapolate;
    std::optional<std::string> quote_name;
    std::optional<std::string> start_date;
    std::optional<std::string> rule;
    std::optional<std::string> adjust_for_losses;
    std::optional<std::string> index_term;
    std::optional<std::string> index_spread;
    std::optional<std::string> currency;
    std::optional<std::string> calibrate_constituents_to_index_spread;
    std::optional<std::string> use_assumed_recovery;
};

struct base_correlation_change {
    base_correlation_write write;
    ores::utility::domain::precondition precondition;
};

struct base_correlation_removal {
    base_correlation_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;
};

struct base_correlation_lookup {
    base_correlation_key key;
    std::optional<ores::refdata::domain::base_correlation> base_correlation;
};

struct base_correlation_event {
    boost::uuids::uuid event_id;
    base_correlation_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;
};

struct base_correlation_version_key {
    base_correlation_key base_correlation;
    std::uint32_t version;
};

struct base_correlation_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;
};

struct list_base_correlations_request {
    using response_type = struct list_base_correlations_response;
    static constexpr std::string_view nats_subject = "refdata.v1.base_correlations.list";
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
};

struct list_base_correlations_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::base_correlation> base_correlations;
    std::uint64_t total;
};

struct get_base_correlation_request {
    using response_type = struct get_base_correlation_response;
    static constexpr std::string_view nats_subject = "refdata.v1.base_correlations.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    base_correlation_key key;
};

struct get_base_correlation_response {
    ores::utility::domain::result result;
    std::optional<ores::refdata::domain::base_correlation> base_correlation;
};

struct get_many_base_correlations_request {
    using response_type = struct get_many_base_correlations_response;
    static constexpr std::string_view nats_subject = "refdata.v1.base_correlations.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<base_correlation_key> keys;
};

struct get_many_base_correlations_response {
    ores::utility::domain::result result;
    std::vector<base_correlation_lookup> entries;
};

struct put_base_correlation_request {
    using response_type = struct put_base_correlation_response;
    static constexpr std::string_view nats_subject = "refdata.v1.base_correlations.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    base_correlation_change change;
    ores::utility::domain::change_intent intent;
};

struct put_base_correlation_response {
    ores::utility::domain::result result;
    std::optional<ores::refdata::domain::base_correlation> base_correlation;
};

struct put_many_base_correlations_request {
    using response_type = struct put_many_base_correlations_response;
    static constexpr std::string_view nats_subject = "refdata.v1.base_correlations.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<base_correlation_change> changes;
    ores::utility::domain::change_intent intent;
};

struct put_many_base_correlations_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::base_correlation> base_correlations;
};

struct delete_base_correlation_request {
    using response_type = struct delete_base_correlation_response;
    static constexpr std::string_view nats_subject = "refdata.v1.base_correlations.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    base_correlation_removal removal;
    ores::utility::domain::change_intent intent;
};

struct delete_base_correlation_response {
    ores::utility::domain::result result;
};

struct delete_many_base_correlations_request {
    using response_type = struct delete_many_base_correlations_response;
    static constexpr std::string_view nats_subject = "refdata.v1.base_correlations.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<base_correlation_removal> removals;
    ores::utility::domain::change_intent intent;
};

struct delete_many_base_correlations_response {
    ores::utility::domain::result result;
};

struct list_base_correlation_versions_request {
    using response_type = struct list_base_correlation_versions_response;
    static constexpr std::string_view nats_subject = "refdata.v1.base_correlations_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    base_correlation_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<base_correlation_versions_filter> filter;
};

struct list_base_correlation_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::base_correlation> versions;
    std::uint64_t total;
};

struct get_base_correlation_version_request {
    using response_type = struct get_base_correlation_version_response;
    static constexpr std::string_view nats_subject = "refdata.v1.base_correlations_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    base_correlation_version_key key;
};

struct get_base_correlation_version_response {
    ores::utility::domain::result result;
    std::optional<ores::refdata::domain::base_correlation> version;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace base_correlation_event_subjects {
inline constexpr std::string_view created = "refdata.v1.base_correlations_events.created";
inline constexpr std::string_view updated = "refdata.v1.base_correlations_events.updated";
inline constexpr std::string_view deleted = "refdata.v1.base_correlations_events.deleted";
}

}

#endif
