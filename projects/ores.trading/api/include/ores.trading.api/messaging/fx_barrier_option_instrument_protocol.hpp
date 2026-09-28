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
#ifndef ORES_TRADING_API_MESSAGING_FX_BARRIER_OPTION_INSTRUMENT_PROTOCOL_HPP
#define ORES_TRADING_API_MESSAGING_FX_BARRIER_OPTION_INSTRUMENT_PROTOCOL_HPP

#include "ores.trading.api/domain/fx_barrier_option_instrument.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <boost/uuid/uuid.hpp>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::trading::messaging {

struct fx_barrier_option_instrument_key {
    boost::uuids::uuid instrument_id;
};

struct fx_barrier_option_instrument_write {
    boost::uuids::uuid instrument_id;
    std::string trade_type_code;
    std::optional<boost::uuids::uuid> trade_id;
    std::string bought_currency;
    ores::utility::decimal::decimal bought_amount;
    std::string sold_currency;
    ores::utility::decimal::decimal sold_amount;
    std::string option_type;
    std::chrono::year_month_day expiry_date;
    std::string settlement;
    std::string barrier_type;
    double lower_barrier;
    std::optional<double> upper_barrier;
    std::string underlying_code;
    std::string description;
};

struct fx_barrier_option_instrument_change {
    fx_barrier_option_instrument_write write;
    ores::utility::domain::precondition precondition;
};

struct fx_barrier_option_instrument_removal {
    fx_barrier_option_instrument_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;
};

struct fx_barrier_option_instrument_lookup {
    fx_barrier_option_instrument_key key;
    std::optional<ores::trading::domain::fx_barrier_option_instrument> fx_barrier_option_instrument;
};

struct fx_barrier_option_instrument_event {
    boost::uuids::uuid event_id;
    fx_barrier_option_instrument_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;
};

struct fx_barrier_option_instrument_version_key {
    fx_barrier_option_instrument_key fx_barrier_option_instrument;
    std::uint32_t version;
};

struct fx_barrier_option_instrument_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;
};

struct list_fx_barrier_option_instruments_request {
    using response_type = struct list_fx_barrier_option_instruments_response;
    static constexpr std::string_view nats_subject =
        "trading.v1.fx_barrier_option_instruments.list";
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

struct list_fx_barrier_option_instruments_response {
    ores::utility::domain::result result;
    std::vector<ores::trading::domain::fx_barrier_option_instrument> fx_barrier_option_instruments;
    std::uint64_t total;
};

struct get_fx_barrier_option_instrument_request {
    using response_type = struct get_fx_barrier_option_instrument_response;
    static constexpr std::string_view nats_subject = "trading.v1.fx_barrier_option_instruments.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    fx_barrier_option_instrument_key key;
};

struct get_fx_barrier_option_instrument_response {
    ores::utility::domain::result result;
    std::optional<ores::trading::domain::fx_barrier_option_instrument> fx_barrier_option_instrument;
};

struct get_many_fx_barrier_option_instruments_request {
    using response_type = struct get_many_fx_barrier_option_instruments_response;
    static constexpr std::string_view nats_subject =
        "trading.v1.fx_barrier_option_instruments.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<fx_barrier_option_instrument_key> keys;
};

struct get_many_fx_barrier_option_instruments_response {
    ores::utility::domain::result result;
    std::vector<fx_barrier_option_instrument_lookup> entries;
};

struct put_fx_barrier_option_instrument_request {
    using response_type = struct put_fx_barrier_option_instrument_response;
    static constexpr std::string_view nats_subject = "trading.v1.fx_barrier_option_instruments.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    fx_barrier_option_instrument_change change;
    ores::utility::domain::change_intent intent;
};

struct put_fx_barrier_option_instrument_response {
    ores::utility::domain::result result;
    std::optional<ores::trading::domain::fx_barrier_option_instrument> fx_barrier_option_instrument;
};

struct put_many_fx_barrier_option_instruments_request {
    using response_type = struct put_many_fx_barrier_option_instruments_response;
    static constexpr std::string_view nats_subject =
        "trading.v1.fx_barrier_option_instruments.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<fx_barrier_option_instrument_change> changes;
    ores::utility::domain::change_intent intent;
};

struct put_many_fx_barrier_option_instruments_response {
    ores::utility::domain::result result;
    std::vector<ores::trading::domain::fx_barrier_option_instrument> fx_barrier_option_instruments;
};

struct delete_fx_barrier_option_instrument_request {
    using response_type = struct delete_fx_barrier_option_instrument_response;
    static constexpr std::string_view nats_subject =
        "trading.v1.fx_barrier_option_instruments.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    fx_barrier_option_instrument_removal removal;
    ores::utility::domain::change_intent intent;
};

struct delete_fx_barrier_option_instrument_response {
    ores::utility::domain::result result;
};

struct delete_many_fx_barrier_option_instruments_request {
    using response_type = struct delete_many_fx_barrier_option_instruments_response;
    static constexpr std::string_view nats_subject =
        "trading.v1.fx_barrier_option_instruments.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<fx_barrier_option_instrument_removal> removals;
    ores::utility::domain::change_intent intent;
};

struct delete_many_fx_barrier_option_instruments_response {
    ores::utility::domain::result result;
};

struct list_fx_barrier_option_instrument_versions_request {
    using response_type = struct list_fx_barrier_option_instrument_versions_response;
    static constexpr std::string_view nats_subject =
        "trading.v1.fx_barrier_option_instruments_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    fx_barrier_option_instrument_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<fx_barrier_option_instrument_versions_filter> filter;
};

struct list_fx_barrier_option_instrument_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::trading::domain::fx_barrier_option_instrument> versions;
    std::uint64_t total;
};

struct get_fx_barrier_option_instrument_version_request {
    using response_type = struct get_fx_barrier_option_instrument_version_response;
    static constexpr std::string_view nats_subject =
        "trading.v1.fx_barrier_option_instruments_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    fx_barrier_option_instrument_version_key key;
};

struct get_fx_barrier_option_instrument_version_response {
    ores::utility::domain::result result;
    std::optional<ores::trading::domain::fx_barrier_option_instrument> version;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace fx_barrier_option_instrument_event_subjects {
inline constexpr std::string_view created =
    "trading.v1.fx_barrier_option_instruments_events.created";
inline constexpr std::string_view updated =
    "trading.v1.fx_barrier_option_instruments_events.updated";
inline constexpr std::string_view deleted =
    "trading.v1.fx_barrier_option_instruments_events.deleted";
}

}

#endif
