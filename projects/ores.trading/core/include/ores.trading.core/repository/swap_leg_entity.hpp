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
    int leg_number{};
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();
    int version = 0;
    boost::uuids::uuid party_id;
    boost::uuids::uuid trade_activity_id;
    std::optional<bool> payer;
    std::string leg_type_code;
    std::optional<std::string> day_count_fraction_code;
    std::optional<std::string> business_day_convention_code;
    std::optional<std::string> payment_frequency_code;
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
    std::optional<double> equity_quantity;
    std::optional<ores::utility::decimal::decimal> equity_initial_price;
    std::optional<std::string> equity_initial_price_currency;
    std::optional<ores::utility::decimal::decimal> equity_dividend_factor;
    std::optional<bool> equity_notional_reset;
    std::optional<std::string> equity_currency;
    std::optional<std::string> fx_index;
    std::optional<std::string> fx_reset_foreign_currency;
    std::optional<std::string> fx_reset_start_date;
    std::optional<ores::utility::decimal::decimal> fx_reset_foreign_amount;
    std::optional<std::string> fx_reset_fx_index;
    std::optional<int> fx_reset_fixing_days;
    std::optional<std::string> fx_reset_fixing_calendar;
    std::optional<std::string> fixing_calendar;
    std::optional<std::string> digital_call_position;
    std::optional<bool> digital_is_call_atm_included;
    std::optional<std::string> digital_put_position;
    std::optional<bool> digital_is_put_atm_included;
    std::optional<ores::utility::decimal::decimal> equity_margin_initial_margin_factor;
    std::optional<ores::utility::decimal::decimal> equity_margin_multiplier;
    std::optional<std::string> commodity_pay_relative_to;
    std::optional<std::string> commodity_quantity_frequency;
    std::optional<std::string> pricing_date_rule;
    std::optional<bool> indexings_from_asset_leg;
    std::optional<std::string> floating_index_code_2;
    std::optional<int> cms_duration;
    std::optional<bool> naked_option;
    std::optional<std::string> lookback;
    std::optional<int> rate_cutoff;
    std::optional<bool> local_cap_floor;
    std::optional<std::string> observation_lag;
    std::optional<double> cpi_base;
    std::optional<std::chrono::year_month_day> cpi_start_date;
    std::optional<std::string> cpi_interpolation;
    std::optional<bool> cpi_interpolated;
    std::optional<bool> subtract_inflation_notional;
    std::optional<std::string> zero_coupon_compounding;
    std::optional<bool> zero_coupon_subtract_notional;
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
               query_builder::bind("fx_reset_foreign_currency", &e::fx_reset_foreign_currency) |
               query_builder::bind("fx_reset_start_date", &e::fx_reset_start_date) |
               query_builder::bind("fx_reset_foreign_amount", &e::fx_reset_foreign_amount) |
               query_builder::bind("fx_reset_fx_index", &e::fx_reset_fx_index) |
               query_builder::bind("fx_reset_fixing_days", &e::fx_reset_fixing_days) |
               query_builder::bind("fx_reset_fixing_calendar", &e::fx_reset_fixing_calendar) |
               query_builder::bind("fixing_calendar", &e::fixing_calendar) |
               query_builder::bind("digital_call_position", &e::digital_call_position) |
               query_builder::bind("digital_is_call_atm_included",
                                   &e::digital_is_call_atm_included) |
               query_builder::bind("digital_put_position", &e::digital_put_position) |
               query_builder::bind("digital_is_put_atm_included", &e::digital_is_put_atm_included) |
               query_builder::bind("equity_margin_initial_margin_factor",
                                   &e::equity_margin_initial_margin_factor) |
               query_builder::bind("equity_margin_multiplier", &e::equity_margin_multiplier) |
               query_builder::bind("commodity_pay_relative_to", &e::commodity_pay_relative_to) |
               query_builder::bind("commodity_quantity_frequency",
                                   &e::commodity_quantity_frequency) |
               query_builder::bind("pricing_date_rule", &e::pricing_date_rule) |
               query_builder::bind("indexings_from_asset_leg", &e::indexings_from_asset_leg) |
               query_builder::bind("floating_index_code_2", &e::floating_index_code_2) |
               query_builder::bind("cms_duration", &e::cms_duration) |
               query_builder::bind("naked_option", &e::naked_option) |
               query_builder::bind("lookback", &e::lookback) |
               query_builder::bind("rate_cutoff", &e::rate_cutoff) |
               query_builder::bind("local_cap_floor", &e::local_cap_floor) |
               query_builder::bind("observation_lag", &e::observation_lag) |
               query_builder::bind("cpi_base", &e::cpi_base) |
               query_builder::bind("cpi_start_date", &e::cpi_start_date) |
               query_builder::bind("cpi_interpolation", &e::cpi_interpolation) |
               query_builder::bind("cpi_interpolated", &e::cpi_interpolated) |
               query_builder::bind("subtract_inflation_notional", &e::subtract_inflation_notional) |
               query_builder::bind("zero_coupon_compounding", &e::zero_coupon_compounding) |
               query_builder::bind("zero_coupon_subtract_notional",
                                   &e::zero_coupon_subtract_notional) |
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
