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
 * Template: cpp_domain_type_entity.hpp.mustache
 * To modify, update the template and regenerate.
 */
#ifndef ORES_TRADING_CORE_REPOSITORY_INSTRUMENT_OPTION_ENTITY_HPP
#define ORES_TRADING_CORE_REPOSITORY_INSTRUMENT_OPTION_ENTITY_HPP

#include "ores.database/query_builder/columns.hpp"
#include "ores.database/repository/db_types.hpp"
#include "ores.utility/decimal/decimal.hpp"
#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <optional>
#include <ostream>
#include <string>

namespace ores::trading::repository {

using db_timestamp = ores::database::repository::db_timestamp;

/**
 * @brief Represents a instrument option in the database.
 *
 * Each member holds the type its column has, so the layer binds and reads it
 * in the binary format and the mapper copies it to the domain member. A column
 * whose type the layer has no codec for keeps its text, and the mapper converts
 * it.
 */
struct instrument_option_entity {
    constexpr static const char* schema = "public";
    constexpr static const char* tablename = "ores_trading_instrument_options_tbl";

    boost::uuids::uuid trade_id;
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();
    int version = 0;
    boost::uuids::uuid trade_activity_id;
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
    std::optional<ores::utility::decimal::decimal> premium_amount;
    std::optional<std::string> premium_currency;
    std::optional<std::chrono::year_month_day> premium_pay_date;
    std::optional<std::string> exercise_fee_settlement_period;
    std::optional<std::string> exercise_fee_settlement_calendar;
    std::optional<std::string> exercise_fee_settlement_convention;
    std::optional<std::string> automatic_exercise;
    bool has_exercise_data = false;
    std::optional<std::chrono::year_month_day> exercise_date;
    std::optional<ores::utility::decimal::decimal> exercise_price;
    bool has_payment_data = false;
    std::optional<int> payment_lag;
    std::optional<std::string> payment_calendar;
    std::optional<std::string> payment_convention;
    std::optional<std::string> payment_relative_to;
    bool has_settlement_data = false;
    std::optional<std::string> settlement_pay_currency;
    std::optional<std::string> settlement_fx_index;
    std::optional<std::chrono::year_month_day> settlement_fixing_date;
    std::string modified_by;
    std::string performed_by;
    std::string change_reason_code;
    std::string change_commentary;
    db_timestamp valid_from = db_timestamp::max();
    db_timestamp valid_to = db_timestamp::max();

    /**
     * @brief The columns of the table, named as the members, with the key.
     */
    static constexpr auto columns() {
        using e = instrument_option_entity;
        namespace query_builder = ores::database::query_builder;
        return query_builder::columns<e>() | query_builder::key("trade_id", &e::trade_id) |
               query_builder::bind("tenant_id", &e::tenant_id) |
               query_builder::bind("version", &e::version) |
               query_builder::bind("trade_activity_id", &e::trade_activity_id) |
               query_builder::bind("long_short", &e::long_short) |
               query_builder::bind("option_type", &e::option_type) |
               query_builder::bind("payoff_type", &e::payoff_type) |
               query_builder::bind("payoff_type_2", &e::payoff_type_2) |
               query_builder::bind("style", &e::style) |
               query_builder::bind("notice_period", &e::notice_period) |
               query_builder::bind("notice_calendar", &e::notice_calendar) |
               query_builder::bind("notice_convention", &e::notice_convention) |
               query_builder::bind("mid_coupon_exercise", &e::mid_coupon_exercise) |
               query_builder::bind("settlement", &e::settlement) |
               query_builder::bind("settlement_method", &e::settlement_method) |
               query_builder::bind("pay_off_at_expiry", &e::pay_off_at_expiry) |
               query_builder::bind("premium_amount", &e::premium_amount) |
               query_builder::bind("premium_currency", &e::premium_currency) |
               query_builder::bind("premium_pay_date", &e::premium_pay_date) |
               query_builder::bind("exercise_fee_settlement_period",
                                   &e::exercise_fee_settlement_period) |
               query_builder::bind("exercise_fee_settlement_calendar",
                                   &e::exercise_fee_settlement_calendar) |
               query_builder::bind("exercise_fee_settlement_convention",
                                   &e::exercise_fee_settlement_convention) |
               query_builder::bind("automatic_exercise", &e::automatic_exercise) |
               query_builder::bind("has_exercise_data", &e::has_exercise_data) |
               query_builder::bind("exercise_date", &e::exercise_date) |
               query_builder::bind("exercise_price", &e::exercise_price) |
               query_builder::bind("has_payment_data", &e::has_payment_data) |
               query_builder::bind("payment_lag", &e::payment_lag) |
               query_builder::bind("payment_calendar", &e::payment_calendar) |
               query_builder::bind("payment_convention", &e::payment_convention) |
               query_builder::bind("payment_relative_to", &e::payment_relative_to) |
               query_builder::bind("has_settlement_data", &e::has_settlement_data) |
               query_builder::bind("settlement_pay_currency", &e::settlement_pay_currency) |
               query_builder::bind("settlement_fx_index", &e::settlement_fx_index) |
               query_builder::bind("settlement_fixing_date", &e::settlement_fixing_date) |
               query_builder::bind("modified_by", &e::modified_by) |
               query_builder::bind("performed_by", &e::performed_by) |
               query_builder::bind("change_reason_code", &e::change_reason_code) |
               query_builder::bind("change_commentary", &e::change_commentary) |
               query_builder::bind("valid_from", &e::valid_from) |
               query_builder::bind("valid_to", &e::valid_to);
    }
};

std::ostream& operator<<(std::ostream& s, const instrument_option_entity& v);

}

#endif
