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
#ifndef ORES_TRADING_API_MESSAGING_COMMODITY_INSTRUMENT_PROTOCOL_HPP
#define ORES_TRADING_API_MESSAGING_COMMODITY_INSTRUMENT_PROTOCOL_HPP

#include "ores.trading.api/domain/commodity_instrument.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <boost/uuid/uuid.hpp>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::trading::messaging {

struct commodity_instrument_key {
    boost::uuids::uuid instrument_id;
};

struct commodity_instrument_write {
    boost::uuids::uuid instrument_id;
    std::string trade_type_code;
    std::optional<boost::uuids::uuid> trade_id;
    std::string commodity_code;
    std::string currency;
    double quantity;
    std::string unit;
    std::optional<std::chrono::year_month_day> start_date;
    std::optional<std::chrono::year_month_day> maturity_date;
    std::optional<ores::utility::decimal::decimal> fixed_price;
    std::string option_type;
    std::optional<ores::utility::decimal::decimal> strike_price;
    std::string exercise_type;
    std::string average_type;
    std::optional<std::chrono::year_month_day> averaging_start_date;
    std::optional<std::chrono::year_month_day> averaging_end_date;
    std::string spread_commodity_code;
    std::optional<ores::utility::decimal::decimal> spread_amount;
    std::string strip_frequency_code;
    std::optional<double> variance_strike;
    std::optional<ores::utility::decimal::decimal> accumulation_amount;
    std::optional<ores::utility::decimal::decimal> knock_out_barrier;
    std::string barrier_type;
    std::optional<ores::utility::decimal::decimal> lower_barrier;
    std::optional<ores::utility::decimal::decimal> upper_barrier;
    std::string basket_json;
    std::string day_count_code;
    std::string payment_frequency_code;
    std::optional<std::chrono::year_month_day> swaption_expiry_date;
    std::string description;
};

struct commodity_instrument_change {
    commodity_instrument_write write;
    ores::utility::domain::precondition precondition;
};

struct commodity_instrument_removal {
    commodity_instrument_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;
};

struct commodity_instrument_lookup {
    commodity_instrument_key key;
    std::optional<ores::trading::domain::commodity_instrument> commodity_instrument;
};

struct commodity_instrument_event {
    boost::uuids::uuid event_id;
    commodity_instrument_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;
};

struct commodity_instrument_version_key {
    commodity_instrument_key commodity_instrument;
    std::uint32_t version;
};

struct commodity_instrument_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;
};

struct list_commodity_instruments_request {
    using response_type = struct list_commodity_instruments_response;
    static constexpr std::string_view nats_subject = "trading.v1.commodity_instruments.list";
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

struct list_commodity_instruments_response {
    ores::utility::domain::result result;
    std::vector<ores::trading::domain::commodity_instrument> commodity_instruments;
    std::uint64_t total;
};

struct get_commodity_instrument_request {
    using response_type = struct get_commodity_instrument_response;
    static constexpr std::string_view nats_subject = "trading.v1.commodity_instruments.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    commodity_instrument_key key;
};

struct get_commodity_instrument_response {
    ores::utility::domain::result result;
    std::optional<ores::trading::domain::commodity_instrument> commodity_instrument;
};

struct get_many_commodity_instruments_request {
    using response_type = struct get_many_commodity_instruments_response;
    static constexpr std::string_view nats_subject = "trading.v1.commodity_instruments.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<commodity_instrument_key> keys;
};

struct get_many_commodity_instruments_response {
    ores::utility::domain::result result;
    std::vector<commodity_instrument_lookup> entries;
};

struct put_commodity_instrument_request {
    using response_type = struct put_commodity_instrument_response;
    static constexpr std::string_view nats_subject = "trading.v1.commodity_instruments.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    commodity_instrument_change change;
    ores::utility::domain::change_intent intent;
};

struct put_commodity_instrument_response {
    ores::utility::domain::result result;
    ores::trading::domain::commodity_instrument commodity_instrument;
};

struct put_many_commodity_instruments_request {
    using response_type = struct put_many_commodity_instruments_response;
    static constexpr std::string_view nats_subject = "trading.v1.commodity_instruments.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<commodity_instrument_change> changes;
    ores::utility::domain::change_intent intent;
};

struct put_many_commodity_instruments_response {
    ores::utility::domain::result result;
    std::vector<ores::trading::domain::commodity_instrument> commodity_instruments;
};

struct delete_commodity_instrument_request {
    using response_type = struct delete_commodity_instrument_response;
    static constexpr std::string_view nats_subject = "trading.v1.commodity_instruments.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    commodity_instrument_removal removal;
    ores::utility::domain::change_intent intent;
};

struct delete_commodity_instrument_response {
    ores::utility::domain::result result;
};

struct delete_many_commodity_instruments_request {
    using response_type = struct delete_many_commodity_instruments_response;
    static constexpr std::string_view nats_subject = "trading.v1.commodity_instruments.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<commodity_instrument_removal> removals;
    ores::utility::domain::change_intent intent;
};

struct delete_many_commodity_instruments_response {
    ores::utility::domain::result result;
};

struct list_commodity_instrument_versions_request {
    using response_type = struct list_commodity_instrument_versions_response;
    static constexpr std::string_view nats_subject =
        "trading.v1.commodity_instruments_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    commodity_instrument_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<commodity_instrument_versions_filter> filter;
};

struct list_commodity_instrument_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::trading::domain::commodity_instrument> versions;
    std::uint64_t total;
};

struct get_commodity_instrument_version_request {
    using response_type = struct get_commodity_instrument_version_response;
    static constexpr std::string_view nats_subject =
        "trading.v1.commodity_instruments_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    commodity_instrument_version_key key;
};

struct get_commodity_instrument_version_response {
    ores::utility::domain::result result;
    ores::trading::domain::commodity_instrument version;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace commodity_instrument_event_subjects {
inline constexpr std::string_view created = "trading.v1.commodity_instruments_events.created";
inline constexpr std::string_view updated = "trading.v1.commodity_instruments_events.updated";
inline constexpr std::string_view deleted = "trading.v1.commodity_instruments_events.deleted";
}

}

#endif
