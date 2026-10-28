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
#ifndef ORES_REFDATA_CORE_REPOSITORY_COMMODITY_VOLATILITY_CONFIG_ENTITY_HPP
#define ORES_REFDATA_CORE_REPOSITORY_COMMODITY_VOLATILITY_CONFIG_ENTITY_HPP

#include "ores.database/query_builder/columns.hpp"
#include "ores.database/repository/db_types.hpp"
#include <optional>
#include <ostream>
#include <string>

namespace ores::refdata::repository {

using db_timestamp = ores::database::repository::db_timestamp;

/**
 * @brief Represents a commodity volatility config in the database.
 */
struct commodity_volatility_config_entity {
    constexpr static const char* schema = "public";
    constexpr static const char* tablename = "ores_refdata_commodity_volatility_configs_tbl";

    std::string id;
    std::string tenant_id;
    int version = 0;
    std::string party_id;
    std::string curve_definition_id;
    std::string currency;
    std::optional<std::string> instrument_type;
    std::optional<int> calendar_spread_offset;
    std::optional<std::string> calendar_spread_underlying_name;
    std::optional<std::string> day_counter;
    std::optional<std::string> calendar;
    std::optional<std::string> future_conventions;
    std::optional<std::string> option_expiry_roll_days;
    std::optional<std::string> price_curve_id;
    std::optional<std::string> yield_curve_id;
    std::optional<std::string> quote_suffix;
    std::optional<std::string> prefer_out_of_the_money;
    bool has_volatility_config = false;
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
        using e = commodity_volatility_config_entity;
        namespace query_builder = ores::database::query_builder;
        return query_builder::columns<e>() | query_builder::key("id", &e::id) |
               query_builder::bind("tenant_id", &e::tenant_id) |
               query_builder::bind("version", &e::version) |
               query_builder::bind("party_id", &e::party_id) |
               query_builder::bind("curve_definition_id", &e::curve_definition_id) |
               query_builder::bind("currency", &e::currency) |
               query_builder::bind("instrument_type", &e::instrument_type) |
               query_builder::bind("calendar_spread_offset", &e::calendar_spread_offset) |
               query_builder::bind("calendar_spread_underlying_name",
                                   &e::calendar_spread_underlying_name) |
               query_builder::bind("day_counter", &e::day_counter) |
               query_builder::bind("calendar", &e::calendar) |
               query_builder::bind("future_conventions", &e::future_conventions) |
               query_builder::bind("option_expiry_roll_days", &e::option_expiry_roll_days) |
               query_builder::bind("price_curve_id", &e::price_curve_id) |
               query_builder::bind("yield_curve_id", &e::yield_curve_id) |
               query_builder::bind("quote_suffix", &e::quote_suffix) |
               query_builder::bind("prefer_out_of_the_money", &e::prefer_out_of_the_money) |
               query_builder::bind("has_volatility_config", &e::has_volatility_config) |
               query_builder::bind("modified_by", &e::modified_by) |
               query_builder::bind("performed_by", &e::performed_by) |
               query_builder::bind("change_reason_code", &e::change_reason_code) |
               query_builder::bind("change_commentary", &e::change_commentary) |
               query_builder::bind("valid_from", &e::valid_from) |
               query_builder::bind("valid_to", &e::valid_to);
    }
};

std::ostream& operator<<(std::ostream& s, const commodity_volatility_config_entity& v);

}

#endif
