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
#ifndef ORES_MARKETDATA_API_MESSAGING_DEMAND_INTERVAL_PROTOCOL_HPP
#define ORES_MARKETDATA_API_MESSAGING_DEMAND_INTERVAL_PROTOCOL_HPP

#include "ores.marketdata.api/domain/demand_interval.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <boost/uuid/uuid.hpp>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::marketdata::messaging {

struct demand_interval_key {
    std::string oresmd_uri;
};

struct demand_interval_write {
    boost::uuids::uuid id;
    boost::uuids::uuid party_id;
    std::string provider_code;
    std::string oresmd_uri;
    std::chrono::system_clock::time_point interval_from;
    boost::uuids::uuid source_id;
    std::optional<std::chrono::system_clock::time_point> interval_to;
};

struct demand_interval_change {
    demand_interval_write write;
    ores::utility::domain::precondition precondition;
};

struct demand_interval_removal {
    demand_interval_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;
};

struct demand_interval_lookup {
    demand_interval_key key;
    std::optional<ores::marketdata::domain::demand_interval> demand_interval;
};

struct demand_intervals_filter {
    std::optional<boost::uuids::uuid> party_id;
    std::optional<std::string> provider_code;
    std::optional<boost::uuids::uuid> source_id;
    std::optional<std::vector<boost::uuids::uuid>> id_one_of;
    std::optional<std::vector<boost::uuids::uuid>> party_id_one_of;
    std::optional<std::vector<std::string>> provider_code_one_of;
    std::optional<std::vector<boost::uuids::uuid>> source_id_one_of;
};

struct demand_interval_event {
    boost::uuids::uuid event_id;
    demand_interval_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;
};

struct demand_interval_version_key {
    demand_interval_key demand_interval;
    std::uint32_t version;
};

struct demand_interval_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;
};

struct list_demand_intervals_request {
    using response_type = struct list_demand_intervals_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.demand_intervals.list";
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
    std::optional<demand_intervals_filter> filter;
    std::optional<std::string> as_of;
};

struct list_demand_intervals_response {
    ores::utility::domain::result result;
    std::vector<ores::marketdata::domain::demand_interval> demand_intervals;
    std::uint64_t total;
};

struct get_demand_interval_request {
    using response_type = struct get_demand_interval_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.demand_intervals.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    demand_interval_key key;
};

struct get_demand_interval_response {
    ores::utility::domain::result result;
    std::optional<ores::marketdata::domain::demand_interval> demand_interval;
};

struct get_many_demand_intervals_request {
    using response_type = struct get_many_demand_intervals_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.demand_intervals.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<demand_interval_key> keys;
};

struct get_many_demand_intervals_response {
    ores::utility::domain::result result;
    std::vector<demand_interval_lookup> entries;
};

struct put_demand_interval_request {
    using response_type = struct put_demand_interval_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.demand_intervals.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    demand_interval_change change;
    ores::utility::domain::change_intent intent;
};

struct put_demand_interval_response {
    ores::utility::domain::result result;
    std::optional<ores::marketdata::domain::demand_interval> demand_interval;
};

struct put_many_demand_intervals_request {
    using response_type = struct put_many_demand_intervals_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.demand_intervals.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<demand_interval_change> changes;
    ores::utility::domain::change_intent intent;
};

struct put_many_demand_intervals_response {
    ores::utility::domain::result result;
    std::vector<ores::marketdata::domain::demand_interval> demand_intervals;
};

struct delete_demand_interval_request {
    using response_type = struct delete_demand_interval_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.demand_intervals.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    demand_interval_removal removal;
    ores::utility::domain::change_intent intent;
};

struct delete_demand_interval_response {
    ores::utility::domain::result result;
};

struct delete_many_demand_intervals_request {
    using response_type = struct delete_many_demand_intervals_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.demand_intervals.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<demand_interval_removal> removals;
    ores::utility::domain::change_intent intent;
};

struct delete_many_demand_intervals_response {
    ores::utility::domain::result result;
};

struct list_demand_interval_versions_request {
    using response_type = struct list_demand_interval_versions_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.demand_intervals_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    demand_interval_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<demand_interval_versions_filter> filter;
};

struct list_demand_interval_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::marketdata::domain::demand_interval> versions;
    std::uint64_t total;
};

struct get_demand_interval_version_request {
    using response_type = struct get_demand_interval_version_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.demand_intervals_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    demand_interval_version_key key;
};

struct get_demand_interval_version_response {
    ores::utility::domain::result result;
    std::optional<ores::marketdata::domain::demand_interval> version;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace demand_interval_event_subjects {
inline constexpr std::string_view created = "marketdata.v1.demand_intervals_events.created";
inline constexpr std::string_view updated = "marketdata.v1.demand_intervals_events.updated";
inline constexpr std::string_view deleted = "marketdata.v1.demand_intervals_events.deleted";
}

}

#endif
