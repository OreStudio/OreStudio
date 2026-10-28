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
#ifndef ORES_TRADING_CORE_REPOSITORY_COMMODITY_INSTRUMENT_ENTITY_HPP
#define ORES_TRADING_CORE_REPOSITORY_COMMODITY_INSTRUMENT_ENTITY_HPP

#include "ores.database/query_builder/columns.hpp"
#include "ores.database/repository/db_types.hpp"
#include <optional>
#include <ostream>
#include <string>

namespace ores::trading::repository {

using db_timestamp = ores::database::repository::db_timestamp;

/**
 * @brief Represents a commodity instrument in the database.
 */
struct commodity_instrument_entity {
    constexpr static const char* schema = "public";
    constexpr static const char* tablename = "ores_trading_commodity_instruments_tbl";

    std::string trade_id;
    std::string tenant_id;
    int version = 0;
    std::string trade_type_code;
    std::string party_id;
    std::string trade_activity_id;
    std::string commodity_code;
    std::string currency;
    double quantity = 0.0;
    std::string unit;
    std::optional<std::string> start_date;
    std::optional<std::string> maturity_date;
    std::optional<std::string> fixed_price;
    std::optional<std::string> option_type;
    std::optional<std::string> strike_price;
    std::optional<std::string> exercise_type;
    std::optional<std::string> average_type;
    std::optional<std::string> averaging_start_date;
    std::optional<std::string> averaging_end_date;
    std::optional<std::string> spread_commodity_code;
    std::optional<std::string> spread_amount;
    std::optional<std::string> strip_frequency_code;
    std::optional<double> variance_strike;
    std::optional<std::string> accumulation_amount;
    std::optional<std::string> knock_out_barrier;
    std::optional<std::string> barrier_type;
    std::optional<std::string> lower_barrier;
    std::optional<std::string> upper_barrier;
    std::optional<std::string> day_count_fraction_code;
    std::optional<std::string> payment_frequency_code;
    std::optional<std::string> swaption_expiry_date;
    std::optional<std::string> description;
    std::string modified_by;
    std::string performed_by;
    std::string change_reason_code;
    std::string change_commentary;
    db_timestamp valid_from = "9999-12-31 23:59:59";
    db_timestamp valid_to = "9999-12-31 23:59:59";

    /**
     * @brief The columns of the table, named as the members, with the key.
     */
    static constexpr auto columns() {
        using e = commodity_instrument_entity;
        namespace query_builder = ores::database::query_builder;
        return query_builder::columns<e>() | query_builder::key("trade_id", &e::trade_id) |
               query_builder::bind("tenant_id", &e::tenant_id) |
               query_builder::bind("version", &e::version) |
               query_builder::bind("trade_type_code", &e::trade_type_code) |
               query_builder::bind("party_id", &e::party_id) |
               query_builder::bind("trade_activity_id", &e::trade_activity_id) |
               query_builder::bind("commodity_code", &e::commodity_code) |
               query_builder::bind("currency", &e::currency) |
               query_builder::bind("quantity", &e::quantity) |
               query_builder::bind("unit", &e::unit) |
               query_builder::bind("start_date", &e::start_date) |
               query_builder::bind("maturity_date", &e::maturity_date) |
               query_builder::bind("fixed_price", &e::fixed_price) |
               query_builder::bind("option_type", &e::option_type) |
               query_builder::bind("strike_price", &e::strike_price) |
               query_builder::bind("exercise_type", &e::exercise_type) |
               query_builder::bind("average_type", &e::average_type) |
               query_builder::bind("averaging_start_date", &e::averaging_start_date) |
               query_builder::bind("averaging_end_date", &e::averaging_end_date) |
               query_builder::bind("spread_commodity_code", &e::spread_commodity_code) |
               query_builder::bind("spread_amount", &e::spread_amount) |
               query_builder::bind("strip_frequency_code", &e::strip_frequency_code) |
               query_builder::bind("variance_strike", &e::variance_strike) |
               query_builder::bind("accumulation_amount", &e::accumulation_amount) |
               query_builder::bind("knock_out_barrier", &e::knock_out_barrier) |
               query_builder::bind("barrier_type", &e::barrier_type) |
               query_builder::bind("lower_barrier", &e::lower_barrier) |
               query_builder::bind("upper_barrier", &e::upper_barrier) |
               query_builder::bind("day_count_fraction_code", &e::day_count_fraction_code) |
               query_builder::bind("payment_frequency_code", &e::payment_frequency_code) |
               query_builder::bind("swaption_expiry_date", &e::swaption_expiry_date) |
               query_builder::bind("description", &e::description) |
               query_builder::bind("modified_by", &e::modified_by) |
               query_builder::bind("performed_by", &e::performed_by) |
               query_builder::bind("change_reason_code", &e::change_reason_code) |
               query_builder::bind("change_commentary", &e::change_commentary) |
               query_builder::bind("valid_from", &e::valid_from) |
               query_builder::bind("valid_to", &e::valid_to);
    }
};

std::ostream& operator<<(std::ostream& s, const commodity_instrument_entity& v);

}

#endif
