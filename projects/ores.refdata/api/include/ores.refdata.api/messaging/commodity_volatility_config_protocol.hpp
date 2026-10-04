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
#ifndef ORES_REFDATA_API_MESSAGING_COMMODITY_VOLATILITY_CONFIG_PROTOCOL_HPP
#define ORES_REFDATA_API_MESSAGING_COMMODITY_VOLATILITY_CONFIG_PROTOCOL_HPP

#include "ores.refdata.api/domain/commodity_volatility_config.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <boost/uuid/uuid.hpp>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::refdata::messaging {

struct commodity_volatility_config_key {
    boost::uuids::uuid id;
};

struct commodity_volatility_config_write {
    boost::uuids::uuid id;
    boost::uuids::uuid curve_definition_id;
    std::string currency;
    std::optional<std::string> instrument_type;
    std::optional<int> calendar_spread_offset;
    std::optional<std::string> calendar_spread_underlying_name;
    std::optional<std::string> day_counter;
    std::optional<std::string> calendar;
    std::optional<std::string> future_conventions;
    std::optional<std::string> option_expiry_roll_days;
    std::optional<std::string> price_curve_id;
    std::optional<std::string> yield_curve_id;
    std::optional<std::string> quote_suffix;
    std::optional<std::string> prefer_out_of_the_money;
    bool has_volatility_config;
};

struct commodity_volatility_config_change {
    commodity_volatility_config_write write;
    ores::utility::domain::precondition precondition;
};

struct commodity_volatility_config_removal {
    commodity_volatility_config_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;
};

struct commodity_volatility_config_lookup {
    commodity_volatility_config_key key;
    std::optional<ores::refdata::domain::commodity_volatility_config> commodity_volatility_config;
};

struct commodity_volatility_configs_filter {
    std::optional<std::vector<boost::uuids::uuid>> id_one_of;
};

struct commodity_volatility_config_event {
    boost::uuids::uuid event_id;
    commodity_volatility_config_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;
};

struct commodity_volatility_config_version_key {
    commodity_volatility_config_key commodity_volatility_config;
    std::uint32_t version;
};

struct commodity_volatility_config_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;
};

struct list_commodity_volatility_configs_request {
    using response_type = struct list_commodity_volatility_configs_response;
    static constexpr std::string_view nats_subject = "refdata.v1.commodity_volatility_configs.list";
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
    std::optional<commodity_volatility_configs_filter> filter;
};

struct list_commodity_volatility_configs_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::commodity_volatility_config> commodity_volatility_configs;
    std::uint64_t total;
};

struct get_commodity_volatility_config_request {
    using response_type = struct get_commodity_volatility_config_response;
    static constexpr std::string_view nats_subject = "refdata.v1.commodity_volatility_configs.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    commodity_volatility_config_key key;
};

struct get_commodity_volatility_config_response {
    ores::utility::domain::result result;
    std::optional<ores::refdata::domain::commodity_volatility_config> commodity_volatility_config;
};

struct get_many_commodity_volatility_configs_request {
    using response_type = struct get_many_commodity_volatility_configs_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.commodity_volatility_configs.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<commodity_volatility_config_key> keys;
};

struct get_many_commodity_volatility_configs_response {
    ores::utility::domain::result result;
    std::vector<commodity_volatility_config_lookup> entries;
};

struct put_commodity_volatility_config_request {
    using response_type = struct put_commodity_volatility_config_response;
    static constexpr std::string_view nats_subject = "refdata.v1.commodity_volatility_configs.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    commodity_volatility_config_change change;
    ores::utility::domain::change_intent intent;
};

struct put_commodity_volatility_config_response {
    ores::utility::domain::result result;
    std::optional<ores::refdata::domain::commodity_volatility_config> commodity_volatility_config;
};

struct put_many_commodity_volatility_configs_request {
    using response_type = struct put_many_commodity_volatility_configs_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.commodity_volatility_configs.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<commodity_volatility_config_change> changes;
    ores::utility::domain::change_intent intent;
};

struct put_many_commodity_volatility_configs_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::commodity_volatility_config> commodity_volatility_configs;
};

struct delete_commodity_volatility_config_request {
    using response_type = struct delete_commodity_volatility_config_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.commodity_volatility_configs.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    commodity_volatility_config_removal removal;
    ores::utility::domain::change_intent intent;
};

struct delete_commodity_volatility_config_response {
    ores::utility::domain::result result;
};

struct delete_many_commodity_volatility_configs_request {
    using response_type = struct delete_many_commodity_volatility_configs_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.commodity_volatility_configs.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<commodity_volatility_config_removal> removals;
    ores::utility::domain::change_intent intent;
};

struct delete_many_commodity_volatility_configs_response {
    ores::utility::domain::result result;
};

struct list_commodity_volatility_config_versions_request {
    using response_type = struct list_commodity_volatility_config_versions_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.commodity_volatility_configs_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    commodity_volatility_config_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<commodity_volatility_config_versions_filter> filter;
};

struct list_commodity_volatility_config_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::commodity_volatility_config> versions;
    std::uint64_t total;
};

struct get_commodity_volatility_config_version_request {
    using response_type = struct get_commodity_volatility_config_version_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.commodity_volatility_configs_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    commodity_volatility_config_version_key key;
};

struct get_commodity_volatility_config_version_response {
    ores::utility::domain::result result;
    std::optional<ores::refdata::domain::commodity_volatility_config> version;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace commodity_volatility_config_event_subjects {
inline constexpr std::string_view created =
    "refdata.v1.commodity_volatility_configs_events.created";
inline constexpr std::string_view updated =
    "refdata.v1.commodity_volatility_configs_events.updated";
inline constexpr std::string_view deleted =
    "refdata.v1.commodity_volatility_configs_events.deleted";
}

}

#endif
