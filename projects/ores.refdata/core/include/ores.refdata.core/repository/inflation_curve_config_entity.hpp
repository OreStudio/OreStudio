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
#ifndef ORES_REFDATA_CORE_REPOSITORY_INFLATION_CURVE_CONFIG_ENTITY_HPP
#define ORES_REFDATA_CORE_REPOSITORY_INFLATION_CURVE_CONFIG_ENTITY_HPP

#include "ores.database/query_builder/columns.hpp"
#include "ores.database/repository/db_types.hpp"
#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <optional>
#include <ostream>
#include <string>

namespace ores::refdata::repository {

using db_timestamp = ores::database::repository::db_timestamp;

/**
 * @brief Represents a inflation curve config in the database.
 *
 * Each member holds the type its column has, so the layer binds and reads it
 * in the binary format and the mapper copies it to the domain member. A column
 * whose type the layer has no codec for keeps its text, and the mapper converts
 * it.
 */
struct inflation_curve_config_entity {
    constexpr static const char* schema = "public";
    constexpr static const char* tablename = "ores_refdata_inflation_curve_configs_tbl";

    boost::uuids::uuid id;
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();
    int version = 0;
    boost::uuids::uuid party_id;
    boost::uuids::uuid curve_definition_id;
    std::string nominal_term_structure;
    std::string inflation_type;
    std::optional<std::string> conventions;
    bool has_quotes = false;
    std::optional<std::string> extrapolation;
    std::string calendar;
    std::optional<std::string> day_counter;
    std::string lag;
    std::string frequency;
    std::optional<std::string> base_rate;
    std::optional<double> tolerance;
    bool has_seasonality = false;
    std::optional<std::string> seasonality_base_date;
    std::optional<std::string> seasonality_frequency;
    std::optional<std::string> use_last_fixing_date;
    std::optional<std::string> interpolation_variable;
    std::optional<std::string> interpolation_method;
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
        using e = inflation_curve_config_entity;
        namespace query_builder = ores::database::query_builder;
        return query_builder::columns<e>() | query_builder::key("id", &e::id) |
               query_builder::bind("tenant_id", &e::tenant_id) |
               query_builder::bind("version", &e::version) |
               query_builder::bind("party_id", &e::party_id) |
               query_builder::bind("curve_definition_id", &e::curve_definition_id) |
               query_builder::bind("nominal_term_structure", &e::nominal_term_structure) |
               query_builder::bind("inflation_type", &e::inflation_type) |
               query_builder::bind("conventions", &e::conventions) |
               query_builder::bind("has_quotes", &e::has_quotes) |
               query_builder::bind("extrapolation", &e::extrapolation) |
               query_builder::bind("calendar", &e::calendar) |
               query_builder::bind("day_counter", &e::day_counter) |
               query_builder::bind("lag", &e::lag) |
               query_builder::bind("frequency", &e::frequency) |
               query_builder::bind("base_rate", &e::base_rate) |
               query_builder::bind("tolerance", &e::tolerance) |
               query_builder::bind("has_seasonality", &e::has_seasonality) |
               query_builder::bind("seasonality_base_date", &e::seasonality_base_date) |
               query_builder::bind("seasonality_frequency", &e::seasonality_frequency) |
               query_builder::bind("use_last_fixing_date", &e::use_last_fixing_date) |
               query_builder::bind("interpolation_variable", &e::interpolation_variable) |
               query_builder::bind("interpolation_method", &e::interpolation_method) |
               query_builder::bind("modified_by", &e::modified_by) |
               query_builder::bind("performed_by", &e::performed_by) |
               query_builder::bind("change_reason_code", &e::change_reason_code) |
               query_builder::bind("change_commentary", &e::change_commentary) |
               query_builder::bind("valid_from", &e::valid_from) |
               query_builder::bind("valid_to", &e::valid_to);
    }
};

std::ostream& operator<<(std::ostream& s, const inflation_curve_config_entity& v);

}

#endif
