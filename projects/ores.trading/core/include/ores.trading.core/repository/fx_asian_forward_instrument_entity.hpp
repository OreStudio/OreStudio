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
#ifndef ORES_TRADING_CORE_REPOSITORY_FX_ASIAN_FORWARD_INSTRUMENT_ENTITY_HPP
#define ORES_TRADING_CORE_REPOSITORY_FX_ASIAN_FORWARD_INSTRUMENT_ENTITY_HPP

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
 * @brief Represents a fx asian forward instrument in the database.
 *
 * Each member holds the type its column has, so the layer binds and reads it
 * in the binary format and the mapper copies it to the domain member. A column
 * whose type the layer has no codec for keeps its text, and the mapper converts
 * it.
 */
struct fx_asian_forward_instrument_entity {
    constexpr static const char* schema = "public";
    constexpr static const char* tablename = "ores_trading_fx_asian_forward_instruments_tbl";

    boost::uuids::uuid trade_id;
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();
    int version = 0;
    std::string trade_type_code;
    boost::uuids::uuid party_id;
    boost::uuids::uuid trade_activity_id;
    std::string fx_index;
    std::optional<std::string> reference_currency;
    std::optional<ores::utility::decimal::decimal> reference_notional;
    std::optional<std::string> settlement_currency;
    std::optional<ores::utility::decimal::decimal> settlement_notional;
    std::optional<std::chrono::year_month_day> payment_date;
    std::optional<std::string> long_short;
    std::optional<std::string> currency;
    std::optional<ores::utility::decimal::decimal> fixing_amount;
    std::optional<ores::utility::decimal::decimal> target_amount;
    std::optional<ores::utility::decimal::decimal> strike;
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
        using e = fx_asian_forward_instrument_entity;
        namespace query_builder = ores::database::query_builder;
        return query_builder::columns<e>() | query_builder::key("trade_id", &e::trade_id) |
               query_builder::bind("tenant_id", &e::tenant_id) |
               query_builder::bind("version", &e::version) |
               query_builder::bind("trade_type_code", &e::trade_type_code) |
               query_builder::bind("party_id", &e::party_id) |
               query_builder::bind("trade_activity_id", &e::trade_activity_id) |
               query_builder::bind("fx_index", &e::fx_index) |
               query_builder::bind("reference_currency", &e::reference_currency) |
               query_builder::bind("reference_notional", &e::reference_notional) |
               query_builder::bind("settlement_currency", &e::settlement_currency) |
               query_builder::bind("settlement_notional", &e::settlement_notional) |
               query_builder::bind("payment_date", &e::payment_date) |
               query_builder::bind("long_short", &e::long_short) |
               query_builder::bind("currency", &e::currency) |
               query_builder::bind("fixing_amount", &e::fixing_amount) |
               query_builder::bind("target_amount", &e::target_amount) |
               query_builder::bind("strike", &e::strike) |
               query_builder::bind("description", &e::description) |
               query_builder::bind("modified_by", &e::modified_by) |
               query_builder::bind("performed_by", &e::performed_by) |
               query_builder::bind("change_reason_code", &e::change_reason_code) |
               query_builder::bind("change_commentary", &e::change_commentary) |
               query_builder::bind("valid_from", &e::valid_from) |
               query_builder::bind("valid_to", &e::valid_to);
    }
};

std::ostream& operator<<(std::ostream& s, const fx_asian_forward_instrument_entity& v);

}

#endif
