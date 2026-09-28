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
#ifndef ORES_TRADING_API_MESSAGING_INSTRUMENT_OPTION_PROTOCOL_HPP
#define ORES_TRADING_API_MESSAGING_INSTRUMENT_OPTION_PROTOCOL_HPP

#include "ores.trading.api/domain/instrument_option.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <boost/uuid/uuid.hpp>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::trading::messaging {

struct instrument_option_key {
    boost::uuids::uuid instrument_id;
};

struct instrument_option_write {
    boost::uuids::uuid instrument_id;
    std::string long_short;
    std::optional<std::string> option_type;
    std::optional<std::string> payoff_type;
    std::optional<std::string> payoff_type_2;
    std::optional<std::string> style;
    std::optional<std::string> notice_period;
    std::optional<std::string> notice_calendar;
    std::optional<std::string> notice_convention;
    std::optional<std::string> mid_coupon_exercise;
    std::optional<std::string> settlement;
    std::optional<std::string> settlement_method;
    std::optional<std::string> pay_off_at_expiry;
    std::optional<std::string> premium_amount;
    std::optional<std::string> premium_currency;
    std::optional<std::string> premium_pay_date;
    std::optional<std::string> exercise_prices;
    std::optional<std::string> exercise_fee_settlement_period;
    std::optional<std::string> exercise_fee_settlement_calendar;
    std::optional<std::string> exercise_fee_settlement_convention;
    std::optional<std::string> automatic_exercise;
    bool has_exercise_data;
    std::optional<std::chrono::year_month_day> exercise_date;
    std::optional<ores::utility::decimal::decimal> exercise_price;
    bool has_payment_data;
    std::optional<std::int64_t> payment_lag;
    std::optional<std::string> payment_calendar;
    std::optional<std::string> payment_convention;
    std::optional<std::string> payment_relative_to;
    bool has_settlement_data;
    std::optional<std::string> settlement_pay_currency;
    std::optional<std::string> settlement_fx_index;
    std::optional<std::string> settlement_fixing_date;
};

struct instrument_option_change {
    instrument_option_write write;
    ores::utility::domain::precondition precondition;
};

struct instrument_option_removal {
    instrument_option_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;
};

struct instrument_option_lookup {
    instrument_option_key key;
    std::optional<ores::trading::domain::instrument_option> instrument_option;
};

struct instrument_option_event {
    boost::uuids::uuid event_id;
    instrument_option_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;
};

struct instrument_option_version_key {
    instrument_option_key instrument_option;
    std::uint32_t version;
};

struct instrument_option_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;
};

struct list_instrument_options_request {
    using response_type = struct list_instrument_options_response;
    static constexpr std::string_view nats_subject = "trading.v1.instrument_options.list";
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

struct list_instrument_options_response {
    ores::utility::domain::result result;
    std::vector<ores::trading::domain::instrument_option> instrument_options;
    std::uint64_t total;
};

struct get_instrument_option_request {
    using response_type = struct get_instrument_option_response;
    static constexpr std::string_view nats_subject = "trading.v1.instrument_options.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    instrument_option_key key;
};

struct get_instrument_option_response {
    ores::utility::domain::result result;
    std::optional<ores::trading::domain::instrument_option> instrument_option;
};

struct get_many_instrument_options_request {
    using response_type = struct get_many_instrument_options_response;
    static constexpr std::string_view nats_subject = "trading.v1.instrument_options.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<instrument_option_key> keys;
};

struct get_many_instrument_options_response {
    ores::utility::domain::result result;
    std::vector<instrument_option_lookup> entries;
};

struct put_instrument_option_request {
    using response_type = struct put_instrument_option_response;
    static constexpr std::string_view nats_subject = "trading.v1.instrument_options.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    instrument_option_change change;
    ores::utility::domain::change_intent intent;
};

struct put_instrument_option_response {
    ores::utility::domain::result result;
    ores::trading::domain::instrument_option instrument_option;
};

struct put_many_instrument_options_request {
    using response_type = struct put_many_instrument_options_response;
    static constexpr std::string_view nats_subject = "trading.v1.instrument_options.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<instrument_option_change> changes;
    ores::utility::domain::change_intent intent;
};

struct put_many_instrument_options_response {
    ores::utility::domain::result result;
    std::vector<ores::trading::domain::instrument_option> instrument_options;
};

struct delete_instrument_option_request {
    using response_type = struct delete_instrument_option_response;
    static constexpr std::string_view nats_subject = "trading.v1.instrument_options.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    instrument_option_removal removal;
    ores::utility::domain::change_intent intent;
};

struct delete_instrument_option_response {
    ores::utility::domain::result result;
};

struct delete_many_instrument_options_request {
    using response_type = struct delete_many_instrument_options_response;
    static constexpr std::string_view nats_subject = "trading.v1.instrument_options.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<instrument_option_removal> removals;
    ores::utility::domain::change_intent intent;
};

struct delete_many_instrument_options_response {
    ores::utility::domain::result result;
};

struct list_instrument_option_versions_request {
    using response_type = struct list_instrument_option_versions_response;
    static constexpr std::string_view nats_subject = "trading.v1.instrument_options_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    instrument_option_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<instrument_option_versions_filter> filter;
};

struct list_instrument_option_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::trading::domain::instrument_option> versions;
    std::uint64_t total;
};

struct get_instrument_option_version_request {
    using response_type = struct get_instrument_option_version_response;
    static constexpr std::string_view nats_subject = "trading.v1.instrument_options_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    instrument_option_version_key key;
};

struct get_instrument_option_version_response {
    ores::utility::domain::result result;
    ores::trading::domain::instrument_option version;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace instrument_option_event_subjects {
inline constexpr std::string_view created = "trading.v1.instrument_options_events.created";
inline constexpr std::string_view updated = "trading.v1.instrument_options_events.updated";
inline constexpr std::string_view deleted = "trading.v1.instrument_options_events.deleted";
}

}

#endif
