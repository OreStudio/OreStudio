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
#ifndef ORES_TRADING_CORE_REPOSITORY_COMPOSITE_INSTRUMENT_ENTITY_HPP
#define ORES_TRADING_CORE_REPOSITORY_COMPOSITE_INSTRUMENT_ENTITY_HPP

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
 * @brief Represents a composite instrument in the database.
 *
 * Each member holds the type its column has, so the layer binds and reads it
 * in the binary format and the mapper copies it to the domain member. A column
 * whose type the layer has no codec for keeps its text, and the mapper converts
 * it.
 */
struct composite_instrument_entity {
    constexpr static const char* schema = "public";
    constexpr static const char* tablename = "ores_trading_composite_instruments_tbl";

    boost::uuids::uuid trade_id;
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();
    int version = 0;
    std::string trade_type_code;
    boost::uuids::uuid party_id;
    boost::uuids::uuid trade_activity_id;
    std::optional<std::string> description;
    std::optional<std::string> currency;
    std::optional<std::string> notional_calculation;
    std::optional<ores::utility::decimal::decimal> notional_override;
    std::optional<std::string> portfolio_basket;
    std::optional<double> index_quantity;
    std::optional<std::string> return_payer;
    std::optional<std::string> return_currency;
    std::optional<std::string> return_observation_lag;
    std::optional<std::string> return_observation_convention;
    std::optional<std::string> return_observation_calendar;
    std::optional<std::string> return_payment_lag;
    std::optional<std::string> return_payment_convention;
    std::optional<std::string> return_payment_calendar;
    std::optional<ores::utility::decimal::decimal> return_initial_price;
    std::optional<std::string> return_initial_price_currency;
    std::optional<std::string> return_fx_index;
    std::optional<std::string> return_pay_underlying_cash_flows_immediately;
    std::optional<std::string> funding_notional_types;
    std::optional<std::string> underlying_basket_name;
    std::optional<std::string> underlying_index_quantity;
    std::optional<std::string> underlying_price_is_per_unit;
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
        using e = composite_instrument_entity;
        namespace query_builder = ores::database::query_builder;
        return query_builder::columns<e>() | query_builder::key("trade_id", &e::trade_id) |
               query_builder::bind("tenant_id", &e::tenant_id) |
               query_builder::bind("version", &e::version) |
               query_builder::bind("trade_type_code", &e::trade_type_code) |
               query_builder::bind("party_id", &e::party_id) |
               query_builder::bind("trade_activity_id", &e::trade_activity_id) |
               query_builder::bind("description", &e::description) |
               query_builder::bind("currency", &e::currency) |
               query_builder::bind("notional_calculation", &e::notional_calculation) |
               query_builder::bind("notional_override", &e::notional_override) |
               query_builder::bind("portfolio_basket", &e::portfolio_basket) |
               query_builder::bind("index_quantity", &e::index_quantity) |
               query_builder::bind("return_payer", &e::return_payer) |
               query_builder::bind("return_currency", &e::return_currency) |
               query_builder::bind("return_observation_lag", &e::return_observation_lag) |
               query_builder::bind("return_observation_convention",
                                   &e::return_observation_convention) |
               query_builder::bind("return_observation_calendar", &e::return_observation_calendar) |
               query_builder::bind("return_payment_lag", &e::return_payment_lag) |
               query_builder::bind("return_payment_convention", &e::return_payment_convention) |
               query_builder::bind("return_payment_calendar", &e::return_payment_calendar) |
               query_builder::bind("return_initial_price", &e::return_initial_price) |
               query_builder::bind("return_initial_price_currency",
                                   &e::return_initial_price_currency) |
               query_builder::bind("return_fx_index", &e::return_fx_index) |
               query_builder::bind("return_pay_underlying_cash_flows_immediately",
                                   &e::return_pay_underlying_cash_flows_immediately) |
               query_builder::bind("funding_notional_types", &e::funding_notional_types) |
               query_builder::bind("underlying_basket_name", &e::underlying_basket_name) |
               query_builder::bind("underlying_index_quantity", &e::underlying_index_quantity) |
               query_builder::bind("underlying_price_is_per_unit",
                                   &e::underlying_price_is_per_unit) |
               query_builder::bind("modified_by", &e::modified_by) |
               query_builder::bind("performed_by", &e::performed_by) |
               query_builder::bind("change_reason_code", &e::change_reason_code) |
               query_builder::bind("change_commentary", &e::change_commentary) |
               query_builder::bind("valid_from", &e::valid_from) |
               query_builder::bind("valid_to", &e::valid_to);
    }
};

std::ostream& operator<<(std::ostream& s, const composite_instrument_entity& v);

}

#endif
