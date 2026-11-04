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
#ifndef ORES_TRADING_CORE_REPOSITORY_SWAP_LEG_ENTITY_HPP
#define ORES_TRADING_CORE_REPOSITORY_SWAP_LEG_ENTITY_HPP

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
 * @brief Represents a swap leg in the database.
 *
 * Each member holds the type its column has, so the layer binds and reads it
 * in the binary format and the mapper copies it to the domain member. A column
 * whose type the layer has no codec for keeps its text, and the mapper converts
 * it.
 */
struct swap_leg_entity {
    constexpr static const char* schema = "public";
    constexpr static const char* tablename = "ores_trading_swap_legs_tbl";

    boost::uuids::uuid trade_id;
    int leg_number;
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();
    int version = 0;
    boost::uuids::uuid party_id;
    boost::uuids::uuid trade_activity_id;
    std::optional<bool> payer;
    std::string leg_type_code;
    std::string day_count_fraction_code;
    std::string business_day_convention_code;
    std::string payment_frequency_code;
    std::optional<std::string> floating_index_code;
    std::string currency;
    std::optional<std::string> leg_group;
    std::optional<int> fixing_days;
    std::optional<bool> is_in_arrears;
    std::optional<bool> include_spread;
    std::optional<bool> is_averaged;
    std::optional<bool> has_sub_periods;
    std::optional<bool> notional_initial_exchange;
    std::optional<bool> notional_final_exchange;
    std::optional<std::string> payment_lag;
    std::optional<std::string> payment_calendar;
    std::optional<std::string> commodity_price_type;
    std::optional<int> future_month_offset;
    std::optional<int> daily_expiry_offset;
    std::optional<std::string> pricing_calendar;
    std::optional<std::string> equity_return_type;
    std::optional<std::string> equity_name;
    std::optional<std::string> equity_underlying_type;
    std::optional<std::string> equity_underlying_identifier_type;
    std::optional<ores::utility::decimal::decimal> equity_quantity;
    std::optional<ores::utility::decimal::decimal> equity_initial_price;
    std::optional<std::string> equity_initial_price_currency;
    std::optional<ores::utility::decimal::decimal> equity_dividend_factor;
    std::optional<bool> equity_notional_reset;
    std::optional<std::string> equity_currency;
    std::optional<std::string> fx_index;
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
        using e = swap_leg_entity;
        namespace query_builder = ores::database::query_builder;
        return query_builder::columns<e>() | query_builder::key("trade_id", &e::trade_id) |
               query_builder::key("leg_number", &e::leg_number) |
               query_builder::bind("tenant_id", &e::tenant_id) |
               query_builder::bind("version", &e::version) |
               query_builder::bind("party_id", &e::party_id) |
               query_builder::bind("trade_activity_id", &e::trade_activity_id) |
               query_builder::bind("payer", &e::payer) |
               query_builder::bind("leg_type_code", &e::leg_type_code) |
               query_builder::bind("day_count_fraction_code", &e::day_count_fraction_code) |
               query_builder::bind("business_day_convention_code",
                                   &e::business_day_convention_code) |
               query_builder::bind("payment_frequency_code", &e::payment_frequency_code) |
               query_builder::bind("floating_index_code", &e::floating_index_code) |
               query_builder::bind("currency", &e::currency) |
               query_builder::bind("leg_group", &e::leg_group) |
               query_builder::bind("fixing_days", &e::fixing_days) |
               query_builder::bind("is_in_arrears", &e::is_in_arrears) |
               query_builder::bind("include_spread", &e::include_spread) |
               query_builder::bind("is_averaged", &e::is_averaged) |
               query_builder::bind("has_sub_periods", &e::has_sub_periods) |
               query_builder::bind("notional_initial_exchange", &e::notional_initial_exchange) |
               query_builder::bind("notional_final_exchange", &e::notional_final_exchange) |
               query_builder::bind("payment_lag", &e::payment_lag) |
               query_builder::bind("payment_calendar", &e::payment_calendar) |
               query_builder::bind("commodity_price_type", &e::commodity_price_type) |
               query_builder::bind("future_month_offset", &e::future_month_offset) |
               query_builder::bind("daily_expiry_offset", &e::daily_expiry_offset) |
               query_builder::bind("pricing_calendar", &e::pricing_calendar) |
               query_builder::bind("equity_return_type", &e::equity_return_type) |
               query_builder::bind("equity_name", &e::equity_name) |
               query_builder::bind("equity_underlying_type", &e::equity_underlying_type) |
               query_builder::bind("equity_underlying_identifier_type",
                                   &e::equity_underlying_identifier_type) |
               query_builder::bind("equity_quantity", &e::equity_quantity) |
               query_builder::bind("equity_initial_price", &e::equity_initial_price) |
               query_builder::bind("equity_initial_price_currency",
                                   &e::equity_initial_price_currency) |
               query_builder::bind("equity_dividend_factor", &e::equity_dividend_factor) |
               query_builder::bind("equity_notional_reset", &e::equity_notional_reset) |
               query_builder::bind("equity_currency", &e::equity_currency) |
               query_builder::bind("fx_index", &e::fx_index) |
               query_builder::bind("modified_by", &e::modified_by) |
               query_builder::bind("performed_by", &e::performed_by) |
               query_builder::bind("change_reason_code", &e::change_reason_code) |
               query_builder::bind("change_commentary", &e::change_commentary) |
               query_builder::bind("valid_from", &e::valid_from) |
               query_builder::bind("valid_to", &e::valid_to);
    }
};

std::ostream& operator<<(std::ostream& s, const swap_leg_entity& v);

}

#endif
