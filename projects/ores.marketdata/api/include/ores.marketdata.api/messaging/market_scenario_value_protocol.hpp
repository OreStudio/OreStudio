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
#ifndef ORES_MARKETDATA_API_MESSAGING_MARKET_SCENARIO_VALUE_PROTOCOL_HPP
#define ORES_MARKETDATA_API_MESSAGING_MARKET_SCENARIO_VALUE_PROTOCOL_HPP

#include "ores.marketdata.api/domain/market_scenario_value.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <boost/uuid/uuid.hpp>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::marketdata::messaging {

struct market_scenario_value_key {
    boost::uuids::uuid id;
};

struct market_scenario_value_write {
    boost::uuids::uuid id;
    boost::uuids::uuid party_id;
    boost::uuids::uuid report_instance_id;
    boost::uuids::uuid result_id;
    std::string source;
    int line;
    std::chrono::system_clock::time_point as_of;
    std::string factor;
    boost::uuids::uuid report_definition_id;
    std::string family;
    std::string scenario;
    double value;
};

struct market_scenario_value_change {
    market_scenario_value_write write;
    ores::utility::domain::precondition precondition;
};

struct market_scenario_value_removal {
    market_scenario_value_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;
};

struct market_scenario_value_lookup {
    market_scenario_value_key key;
    std::optional<ores::marketdata::domain::market_scenario_value> market_scenario_value;
};

struct market_scenario_values_filter {
    std::optional<boost::uuids::uuid> report_instance_id;
    std::optional<std::string> factor;
    std::optional<std::string> family;
    std::optional<std::string> scenario;
    std::optional<std::vector<boost::uuids::uuid>> id_one_of;
    std::optional<std::vector<boost::uuids::uuid>> report_instance_id_one_of;
    std::optional<std::vector<std::string>> factor_one_of;
    std::optional<std::vector<std::string>> family_one_of;
    std::optional<std::vector<std::string>> scenario_one_of;
};

struct market_scenario_value_event {
    boost::uuids::uuid event_id;
    market_scenario_value_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;
};

struct list_market_scenario_values_request {
    using response_type = struct list_market_scenario_values_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.market_scenario_values.list";
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
    std::optional<market_scenario_values_filter> filter;
    std::optional<std::string> as_of;
};

struct list_market_scenario_values_response {
    ores::utility::domain::result result;
    std::vector<ores::marketdata::domain::market_scenario_value> values;
    std::uint64_t total;
};

struct get_market_scenario_value_request {
    using response_type = struct get_market_scenario_value_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.market_scenario_values.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    market_scenario_value_key key;
};

struct get_market_scenario_value_response {
    ores::utility::domain::result result;
    std::optional<ores::marketdata::domain::market_scenario_value> market_scenario_value;
};

struct get_many_market_scenario_values_request {
    using response_type = struct get_many_market_scenario_values_response;
    static constexpr std::string_view nats_subject =
        "marketdata.v1.market_scenario_values.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<market_scenario_value_key> keys;
};

struct get_many_market_scenario_values_response {
    ores::utility::domain::result result;
    std::vector<market_scenario_value_lookup> entries;
};

struct put_market_scenario_value_request {
    using response_type = struct put_market_scenario_value_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.market_scenario_values.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    market_scenario_value_change change;
    ores::utility::domain::change_intent intent;
};

struct put_market_scenario_value_response {
    ores::utility::domain::result result;
    std::optional<ores::marketdata::domain::market_scenario_value> market_scenario_value;
};

struct put_many_market_scenario_values_request {
    using response_type = struct put_many_market_scenario_values_response;
    static constexpr std::string_view nats_subject =
        "marketdata.v1.market_scenario_values.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<market_scenario_value_change> changes;
    ores::utility::domain::change_intent intent;
};

struct put_many_market_scenario_values_response {
    ores::utility::domain::result result;
    std::vector<ores::marketdata::domain::market_scenario_value> values;
};

struct delete_market_scenario_value_request {
    using response_type = struct delete_market_scenario_value_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.market_scenario_values.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    market_scenario_value_removal removal;
    ores::utility::domain::change_intent intent;
};

struct delete_market_scenario_value_response {
    ores::utility::domain::result result;
};

struct delete_many_market_scenario_values_request {
    using response_type = struct delete_many_market_scenario_values_response;
    static constexpr std::string_view nats_subject =
        "marketdata.v1.market_scenario_values.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<market_scenario_value_removal> removals;
    ores::utility::domain::change_intent intent;
};

struct delete_many_market_scenario_values_response {
    ores::utility::domain::result result;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace market_scenario_value_event_subjects {
inline constexpr std::string_view created = "marketdata.v1.market_scenario_values_events.created";
inline constexpr std::string_view updated = "marketdata.v1.market_scenario_values_events.updated";
inline constexpr std::string_view deleted = "marketdata.v1.market_scenario_values_events.deleted";
}

}

#endif
