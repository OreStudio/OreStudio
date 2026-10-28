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
#ifndef ORES_REFDATA_CORE_REPOSITORY_FX_VOLATILITY_CONFIG_ENTITY_HPP
#define ORES_REFDATA_CORE_REPOSITORY_FX_VOLATILITY_CONFIG_ENTITY_HPP

#include "ores.database/query_builder/columns.hpp"
#include "ores.database/repository/db_types.hpp"
#include <optional>
#include <ostream>
#include <string>

namespace ores::refdata::repository {

using db_timestamp = ores::database::repository::db_timestamp;

/**
 * @brief Represents a fx volatility config in the database.
 */
struct fx_volatility_config_entity {
    constexpr static const char* schema = "public";
    constexpr static const char* tablename = "ores_refdata_fx_volatility_configs_tbl";

    std::string id;
    std::string tenant_id;
    int version = 0;
    std::string party_id;
    std::string curve_definition_id;
    std::string dimension;
    std::optional<std::string> smile_type;
    std::optional<std::string> smile_interpolation;
    std::optional<std::string> deltas;
    std::optional<std::string> smile_delta;
    std::optional<std::string> conventions;
    std::optional<std::string> expiries;
    std::optional<std::string> fx_spot_id;
    std::optional<std::string> fx_foreign_curve_id;
    std::optional<std::string> fx_domestic_curve_id;
    std::optional<std::string> calendar;
    std::optional<std::string> day_counter;
    std::optional<std::string> fx_index_tag;
    std::optional<std::string> base_volatility_1;
    std::optional<std::string> base_volatility_2;
    std::optional<std::string> smile_extrapolation;
    std::optional<std::string> time_interpolation;
    std::optional<std::string> time_weighting;
    std::optional<double> butterfly_error_tolerance;
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
        using e = fx_volatility_config_entity;
        namespace query_builder = ores::database::query_builder;
        return query_builder::columns<e>() | query_builder::key("id", &e::id) |
               query_builder::bind("tenant_id", &e::tenant_id) |
               query_builder::bind("version", &e::version) |
               query_builder::bind("party_id", &e::party_id) |
               query_builder::bind("curve_definition_id", &e::curve_definition_id) |
               query_builder::bind("dimension", &e::dimension) |
               query_builder::bind("smile_type", &e::smile_type) |
               query_builder::bind("smile_interpolation", &e::smile_interpolation) |
               query_builder::bind("deltas", &e::deltas) |
               query_builder::bind("smile_delta", &e::smile_delta) |
               query_builder::bind("conventions", &e::conventions) |
               query_builder::bind("expiries", &e::expiries) |
               query_builder::bind("fx_spot_id", &e::fx_spot_id) |
               query_builder::bind("fx_foreign_curve_id", &e::fx_foreign_curve_id) |
               query_builder::bind("fx_domestic_curve_id", &e::fx_domestic_curve_id) |
               query_builder::bind("calendar", &e::calendar) |
               query_builder::bind("day_counter", &e::day_counter) |
               query_builder::bind("fx_index_tag", &e::fx_index_tag) |
               query_builder::bind("base_volatility_1", &e::base_volatility_1) |
               query_builder::bind("base_volatility_2", &e::base_volatility_2) |
               query_builder::bind("smile_extrapolation", &e::smile_extrapolation) |
               query_builder::bind("time_interpolation", &e::time_interpolation) |
               query_builder::bind("time_weighting", &e::time_weighting) |
               query_builder::bind("butterfly_error_tolerance", &e::butterfly_error_tolerance) |
               query_builder::bind("modified_by", &e::modified_by) |
               query_builder::bind("performed_by", &e::performed_by) |
               query_builder::bind("change_reason_code", &e::change_reason_code) |
               query_builder::bind("change_commentary", &e::change_commentary) |
               query_builder::bind("valid_from", &e::valid_from) |
               query_builder::bind("valid_to", &e::valid_to);
    }
};

std::ostream& operator<<(std::ostream& s, const fx_volatility_config_entity& v);

}

#endif
