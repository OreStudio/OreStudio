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
#ifndef ORES_TRADING_CORE_REPOSITORY_EQUITY_PAIRWISE_VARIANCE_SWAP_INSTRUMENT_ENTITY_HPP
#define ORES_TRADING_CORE_REPOSITORY_EQUITY_PAIRWISE_VARIANCE_SWAP_INSTRUMENT_ENTITY_HPP

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
 * @brief Represents a equity pairwise variance swap instrument in the database.
 *
 * Each member holds the type its column has, so the layer binds and reads it
 * in the binary format and the mapper copies it to the domain member. A column
 * whose type the layer has no codec for keeps its text, and the mapper converts
 * it.
 */
struct equity_pairwise_variance_swap_instrument_entity {
    constexpr static const char* schema = "public";
    constexpr static const char* tablename =
        "ores_trading_equity_pairwise_var_swap_instruments_tbl";

    boost::uuids::uuid trade_id;
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();
    int version = 0;
    std::string trade_type_code;
    boost::uuids::uuid party_id;
    boost::uuids::uuid trade_activity_id;
    std::string first_underlying;
    std::string second_underlying;
    ores::utility::decimal::decimal first_strike;
    ores::utility::decimal::decimal second_strike;
    ores::utility::decimal::decimal first_notional;
    ores::utility::decimal::decimal second_notional;
    ores::utility::decimal::decimal basket_notional;
    ores::utility::decimal::decimal basket_strike;
    std::string currency;
    std::chrono::year_month_day settlement_date;
    std::string long_short;
    std::optional<int> accrual_lag;
    std::optional<ores::utility::decimal::decimal> payoff_limit;
    std::optional<ores::utility::decimal::decimal> cap;
    std::optional<ores::utility::decimal::decimal> floor;
    std::optional<std::string> description;
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
        using e = equity_pairwise_variance_swap_instrument_entity;
        namespace query_builder = ores::database::query_builder;
        return query_builder::columns<e>() | query_builder::key("trade_id", &e::trade_id) |
               query_builder::bind("tenant_id", &e::tenant_id) |
               query_builder::bind("version", &e::version) |
               query_builder::bind("trade_type_code", &e::trade_type_code) |
               query_builder::bind("party_id", &e::party_id) |
               query_builder::bind("trade_activity_id", &e::trade_activity_id) |
               query_builder::bind("first_underlying", &e::first_underlying) |
               query_builder::bind("second_underlying", &e::second_underlying) |
               query_builder::bind("first_strike", &e::first_strike) |
               query_builder::bind("second_strike", &e::second_strike) |
               query_builder::bind("first_notional", &e::first_notional) |
               query_builder::bind("second_notional", &e::second_notional) |
               query_builder::bind("basket_notional", &e::basket_notional) |
               query_builder::bind("basket_strike", &e::basket_strike) |
               query_builder::bind("currency", &e::currency) |
               query_builder::bind("settlement_date", &e::settlement_date) |
               query_builder::bind("long_short", &e::long_short) |
               query_builder::bind("accrual_lag", &e::accrual_lag) |
               query_builder::bind("payoff_limit", &e::payoff_limit) |
               query_builder::bind("cap", &e::cap) | query_builder::bind("floor", &e::floor) |
               query_builder::bind("description", &e::description) |
               query_builder::bind("modified_by", &e::modified_by) |
               query_builder::bind("performed_by", &e::performed_by) |
               query_builder::bind("change_reason_code", &e::change_reason_code) |
               query_builder::bind("change_commentary", &e::change_commentary) |
               query_builder::bind("valid_from", &e::valid_from) |
               query_builder::bind("valid_to", &e::valid_to);
    }
};

std::ostream& operator<<(std::ostream& s, const equity_pairwise_variance_swap_instrument_entity& v);

}

#endif
