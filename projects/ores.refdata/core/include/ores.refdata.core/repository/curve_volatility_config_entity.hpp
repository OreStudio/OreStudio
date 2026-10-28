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
#ifndef ORES_REFDATA_CORE_REPOSITORY_CURVE_VOLATILITY_CONFIG_ENTITY_HPP
#define ORES_REFDATA_CORE_REPOSITORY_CURVE_VOLATILITY_CONFIG_ENTITY_HPP

#include "ores.database/query_builder/columns.hpp"
#include "ores.database/repository/db_types.hpp"
#include <optional>
#include <ostream>
#include <string>

namespace ores::refdata::repository {

using db_timestamp = ores::database::repository::db_timestamp;

/**
 * @brief Represents a curve volatility config in the database.
 */
struct curve_volatility_config_entity {
    constexpr static const char* schema = "public";
    constexpr static const char* tablename = "ores_refdata_curve_volatility_configs_tbl";

    std::string id;
    std::string tenant_id;
    int version = 0;
    std::string party_id;
    std::string curve_definition_id;
    std::string kind;
    bool is_wrapped = false;
    std::optional<int> priority;
    std::optional<std::string> quote_type;
    std::optional<std::string> volatility_type;
    std::optional<std::string> exercise_type;
    std::optional<std::string> strikes;
    std::optional<std::string> expiries;
    std::optional<std::string> time_interpolation;
    std::optional<std::string> strike_interpolation;
    std::optional<std::string> extrapolation;
    std::optional<std::string> time_extrapolation;
    std::optional<std::string> time_extrapolation_variance;
    std::optional<std::string> strike_extrapolation;
    std::optional<std::string> calendar;
    std::optional<std::string> quote;
    std::optional<std::string> interpolation;
    std::optional<bool> enforce_monotone_variance;
    std::optional<std::string> delta_type;
    std::optional<std::string> atm_type;
    std::optional<std::string> atm_delta_type;
    std::optional<std::string> put_deltas;
    std::optional<std::string> call_deltas;
    std::optional<std::string> future_price_correction;
    std::optional<std::string> proxy_volatility_curve;
    std::optional<std::string> fx_volatility_curve;
    std::optional<std::string> correlation_curve;
    std::optional<std::string> cds_volatility_curve;
    int position = 0;
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
        using e = curve_volatility_config_entity;
        namespace query_builder = ores::database::query_builder;
        return query_builder::columns<e>() | query_builder::key("id", &e::id) |
               query_builder::bind("tenant_id", &e::tenant_id) |
               query_builder::bind("version", &e::version) |
               query_builder::bind("party_id", &e::party_id) |
               query_builder::bind("curve_definition_id", &e::curve_definition_id) |
               query_builder::bind("kind", &e::kind) |
               query_builder::bind("is_wrapped", &e::is_wrapped) |
               query_builder::bind("priority", &e::priority) |
               query_builder::bind("quote_type", &e::quote_type) |
               query_builder::bind("volatility_type", &e::volatility_type) |
               query_builder::bind("exercise_type", &e::exercise_type) |
               query_builder::bind("strikes", &e::strikes) |
               query_builder::bind("expiries", &e::expiries) |
               query_builder::bind("time_interpolation", &e::time_interpolation) |
               query_builder::bind("strike_interpolation", &e::strike_interpolation) |
               query_builder::bind("extrapolation", &e::extrapolation) |
               query_builder::bind("time_extrapolation", &e::time_extrapolation) |
               query_builder::bind("time_extrapolation_variance", &e::time_extrapolation_variance) |
               query_builder::bind("strike_extrapolation", &e::strike_extrapolation) |
               query_builder::bind("calendar", &e::calendar) |
               query_builder::bind("quote", &e::quote) |
               query_builder::bind("interpolation", &e::interpolation) |
               query_builder::bind("enforce_monotone_variance", &e::enforce_monotone_variance) |
               query_builder::bind("delta_type", &e::delta_type) |
               query_builder::bind("atm_type", &e::atm_type) |
               query_builder::bind("atm_delta_type", &e::atm_delta_type) |
               query_builder::bind("put_deltas", &e::put_deltas) |
               query_builder::bind("call_deltas", &e::call_deltas) |
               query_builder::bind("future_price_correction", &e::future_price_correction) |
               query_builder::bind("proxy_volatility_curve", &e::proxy_volatility_curve) |
               query_builder::bind("fx_volatility_curve", &e::fx_volatility_curve) |
               query_builder::bind("correlation_curve", &e::correlation_curve) |
               query_builder::bind("cds_volatility_curve", &e::cds_volatility_curve) |
               query_builder::bind("position", &e::position) |
               query_builder::bind("modified_by", &e::modified_by) |
               query_builder::bind("performed_by", &e::performed_by) |
               query_builder::bind("change_reason_code", &e::change_reason_code) |
               query_builder::bind("change_commentary", &e::change_commentary) |
               query_builder::bind("valid_from", &e::valid_from) |
               query_builder::bind("valid_to", &e::valid_to);
    }
};

std::ostream& operator<<(std::ostream& s, const curve_volatility_config_entity& v);

}

#endif
