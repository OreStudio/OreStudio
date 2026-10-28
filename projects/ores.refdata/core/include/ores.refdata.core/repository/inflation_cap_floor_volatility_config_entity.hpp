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
#ifndef ORES_REFDATA_CORE_REPOSITORY_INFLATION_CAP_FLOOR_VOLATILITY_CONFIG_ENTITY_HPP
#define ORES_REFDATA_CORE_REPOSITORY_INFLATION_CAP_FLOOR_VOLATILITY_CONFIG_ENTITY_HPP

#include "ores.database/query_builder/columns.hpp"
#include "ores.database/repository/db_types.hpp"
#include <optional>
#include <ostream>
#include <string>

namespace ores::refdata::repository {

using db_timestamp = ores::database::repository::db_timestamp;

/**
 * @brief Represents a inflation cap floor volatility config in the database.
 */
struct inflation_cap_floor_volatility_config_entity {
    constexpr static const char* schema = "public";
    constexpr static const char* tablename =
        "ores_refdata_inflation_cap_floor_volatility_configs_tbl";

    std::string id;
    std::string tenant_id;
    int version = 0;
    std::string party_id;
    std::string curve_definition_id;
    std::string inflation_type;
    std::string quote_type;
    std::string volatility_type;
    std::string extrapolation;
    std::string tenors;
    std::optional<int> settlement_days;
    std::optional<std::string> cap_strikes;
    std::optional<std::string> floor_strikes;
    std::optional<std::string> strikes;
    std::string calendar;
    std::string day_counter;
    std::string business_day_convention;
    std::string index;
    std::string index_curve;
    std::optional<std::string> index_interpolated;
    std::string observation_lag;
    std::string yield_term_structure;
    std::optional<std::string> quote_index;
    std::optional<std::string> conventions;
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
        using e = inflation_cap_floor_volatility_config_entity;
        namespace query_builder = ores::database::query_builder;
        return query_builder::columns<e>() | query_builder::key("id", &e::id) |
               query_builder::bind("tenant_id", &e::tenant_id) |
               query_builder::bind("version", &e::version) |
               query_builder::bind("party_id", &e::party_id) |
               query_builder::bind("curve_definition_id", &e::curve_definition_id) |
               query_builder::bind("inflation_type", &e::inflation_type) |
               query_builder::bind("quote_type", &e::quote_type) |
               query_builder::bind("volatility_type", &e::volatility_type) |
               query_builder::bind("extrapolation", &e::extrapolation) |
               query_builder::bind("tenors", &e::tenors) |
               query_builder::bind("settlement_days", &e::settlement_days) |
               query_builder::bind("cap_strikes", &e::cap_strikes) |
               query_builder::bind("floor_strikes", &e::floor_strikes) |
               query_builder::bind("strikes", &e::strikes) |
               query_builder::bind("calendar", &e::calendar) |
               query_builder::bind("day_counter", &e::day_counter) |
               query_builder::bind("business_day_convention", &e::business_day_convention) |
               query_builder::bind("index", &e::index) |
               query_builder::bind("index_curve", &e::index_curve) |
               query_builder::bind("index_interpolated", &e::index_interpolated) |
               query_builder::bind("observation_lag", &e::observation_lag) |
               query_builder::bind("yield_term_structure", &e::yield_term_structure) |
               query_builder::bind("quote_index", &e::quote_index) |
               query_builder::bind("conventions", &e::conventions) |
               query_builder::bind("modified_by", &e::modified_by) |
               query_builder::bind("performed_by", &e::performed_by) |
               query_builder::bind("change_reason_code", &e::change_reason_code) |
               query_builder::bind("change_commentary", &e::change_commentary) |
               query_builder::bind("valid_from", &e::valid_from) |
               query_builder::bind("valid_to", &e::valid_to);
    }
};

std::ostream& operator<<(std::ostream& s, const inflation_cap_floor_volatility_config_entity& v);

}

#endif
