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
#ifndef ORES_TRADING_CORE_REPOSITORY_INSTRUMENT_OPTION_ENTITY_HPP
#define ORES_TRADING_CORE_REPOSITORY_INSTRUMENT_OPTION_ENTITY_HPP

#include "ores.database/repository/db_types.hpp"
#include "sqlgen/PrimaryKey.hpp"
#include <optional>
#include <ostream>
#include <string>

namespace ores::trading::repository {

using db_timestamp = ores::database::repository::db_timestamp;

/**
 * @brief Represents a instrument option in the database.
 */
struct instrument_option_entity {
    constexpr static const char* schema = "public";
    constexpr static const char* tablename = "ores_trading_instrument_options_tbl";

    sqlgen::PrimaryKey<std::string> instrument_id;
    std::string tenant_id;
    int version = 0;
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
    bool has_exercise_data = false;
    std::optional<std::string> exercise_date;
    std::optional<double> exercise_price;
    bool has_payment_data = false;
    std::optional<std::int64_t> payment_lag;
    std::optional<std::string> payment_calendar;
    std::optional<std::string> payment_convention;
    std::optional<std::string> payment_relative_to;
    bool has_settlement_data = false;
    std::optional<std::string> settlement_pay_currency;
    std::optional<std::string> settlement_fx_index;
    std::optional<std::string> settlement_fixing_date;
    std::string modified_by;
    std::string performed_by;
    std::string change_reason_code;
    std::string change_commentary;
    db_timestamp valid_from = "9999-12-31 23:59:59";
    db_timestamp valid_to = "9999-12-31 23:59:59";
};

std::ostream& operator<<(std::ostream& s, const instrument_option_entity& v);

}

#endif
