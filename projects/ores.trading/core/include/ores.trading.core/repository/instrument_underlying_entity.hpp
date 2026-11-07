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
#ifndef ORES_TRADING_CORE_REPOSITORY_INSTRUMENT_UNDERLYING_ENTITY_HPP
#define ORES_TRADING_CORE_REPOSITORY_INSTRUMENT_UNDERLYING_ENTITY_HPP

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
 * @brief Represents a instrument underlying in the database.
 *
 * Each member holds the type its column has, so the layer binds and reads it
 * in the binary format and the mapper copies it to the domain member. A column
 * whose type the layer has no codec for keeps its text, and the mapper converts
 * it.
 */
struct instrument_underlying_entity {
    constexpr static const char* schema = "public";
    constexpr static const char* tablename = "ores_trading_instrument_underlyings_tbl";

    boost::uuids::uuid trade_id;
    std::string underlying_role;
    int sequence_number;
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();
    int version = 0;
    boost::uuids::uuid trade_activity_id;
    std::string underlying_type;
    std::string name;
    std::optional<std::string> identifier_type;
    std::optional<std::string> currency;
    std::optional<std::string> exchange;
    std::optional<double> weight;
    std::optional<std::string> price_type;
    std::optional<int> future_month_offset;
    std::optional<int> delivery_roll_days;
    std::optional<std::string> delivery_roll_calendar;
    std::optional<std::string> future_expiry_date;
    std::optional<std::string> future_contract_month;
    std::optional<std::string> interpolation;
    std::optional<ores::utility::decimal::decimal> bid_ask_adjustment;
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
        using e = instrument_underlying_entity;
        namespace query_builder = ores::database::query_builder;
        return query_builder::columns<e>() | query_builder::key("trade_id", &e::trade_id) |
               query_builder::key("underlying_role", &e::underlying_role) |
               query_builder::key("sequence_number", &e::sequence_number) |
               query_builder::bind("tenant_id", &e::tenant_id) |
               query_builder::bind("version", &e::version) |
               query_builder::bind("trade_activity_id", &e::trade_activity_id) |
               query_builder::bind("underlying_type", &e::underlying_type) |
               query_builder::bind("name", &e::name) |
               query_builder::bind("identifier_type", &e::identifier_type) |
               query_builder::bind("currency", &e::currency) |
               query_builder::bind("exchange", &e::exchange) |
               query_builder::bind("weight", &e::weight) |
               query_builder::bind("price_type", &e::price_type) |
               query_builder::bind("future_month_offset", &e::future_month_offset) |
               query_builder::bind("delivery_roll_days", &e::delivery_roll_days) |
               query_builder::bind("delivery_roll_calendar", &e::delivery_roll_calendar) |
               query_builder::bind("future_expiry_date", &e::future_expiry_date) |
               query_builder::bind("future_contract_month", &e::future_contract_month) |
               query_builder::bind("interpolation", &e::interpolation) |
               query_builder::bind("bid_ask_adjustment", &e::bid_ask_adjustment) |
               query_builder::bind("modified_by", &e::modified_by) |
               query_builder::bind("performed_by", &e::performed_by) |
               query_builder::bind("change_reason_code", &e::change_reason_code) |
               query_builder::bind("change_commentary", &e::change_commentary) |
               query_builder::bind("valid_from", &e::valid_from) |
               query_builder::bind("valid_to", &e::valid_to);
    }
};

std::ostream& operator<<(std::ostream& s, const instrument_underlying_entity& v);

}

#endif
