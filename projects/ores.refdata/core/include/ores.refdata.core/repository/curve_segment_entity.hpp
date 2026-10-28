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
#ifndef ORES_REFDATA_CORE_REPOSITORY_CURVE_SEGMENT_ENTITY_HPP
#define ORES_REFDATA_CORE_REPOSITORY_CURVE_SEGMENT_ENTITY_HPP

#include "ores.database/query_builder/columns.hpp"
#include "ores.database/repository/db_types.hpp"
#include <optional>
#include <ostream>
#include <string>

namespace ores::refdata::repository {

using db_timestamp = ores::database::repository::db_timestamp;

/**
 * @brief Represents a curve segment in the database.
 */
struct curve_segment_entity {
    constexpr static const char* schema = "public";
    constexpr static const char* tablename = "ores_refdata_curve_segments_tbl";

    std::string id;
    std::string tenant_id;
    int version = 0;
    std::string party_id;
    std::string curve_definition_id;
    std::string segment_type;
    int position = 0;
    std::optional<std::string> conventions;
    std::optional<std::string> pillar_choice;
    std::optional<int> priority;
    std::optional<int> min_distance;
    std::optional<std::string> projection_curve;
    std::optional<std::string> discount_curve;
    std::optional<std::string> spot_rate;
    std::optional<std::string> projection_curve_domestic;
    std::optional<std::string> projection_curve_foreign;
    std::optional<std::string> projection_curve_pay;
    std::optional<std::string> projection_curve_receive;
    std::optional<std::string> projection_curve_long;
    std::optional<std::string> projection_curve_short;
    std::optional<std::string> reference_curve;
    std::optional<std::string> reference_curve_2;
    std::optional<double> weight_1;
    std::optional<double> weight_2;
    std::optional<std::string> ibor_index;
    std::optional<std::string> rfr_curve;
    std::optional<std::string> rfr_index;
    std::optional<std::string> spread;
    std::optional<std::string> base_curve;
    std::optional<std::string> base_curve_currency;
    std::optional<std::string> numerator_curve;
    std::optional<std::string> numerator_curve_currency;
    std::optional<std::string> denominator_curve;
    std::optional<std::string> denominator_curve_currency;
    std::optional<bool> extrapolate_flat;
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
        using e = curve_segment_entity;
        namespace query_builder = ores::database::query_builder;
        return query_builder::columns<e>() | query_builder::key("id", &e::id) |
               query_builder::bind("tenant_id", &e::tenant_id) |
               query_builder::bind("version", &e::version) |
               query_builder::bind("party_id", &e::party_id) |
               query_builder::bind("curve_definition_id", &e::curve_definition_id) |
               query_builder::bind("segment_type", &e::segment_type) |
               query_builder::bind("position", &e::position) |
               query_builder::bind("conventions", &e::conventions) |
               query_builder::bind("pillar_choice", &e::pillar_choice) |
               query_builder::bind("priority", &e::priority) |
               query_builder::bind("min_distance", &e::min_distance) |
               query_builder::bind("projection_curve", &e::projection_curve) |
               query_builder::bind("discount_curve", &e::discount_curve) |
               query_builder::bind("spot_rate", &e::spot_rate) |
               query_builder::bind("projection_curve_domestic", &e::projection_curve_domestic) |
               query_builder::bind("projection_curve_foreign", &e::projection_curve_foreign) |
               query_builder::bind("projection_curve_pay", &e::projection_curve_pay) |
               query_builder::bind("projection_curve_receive", &e::projection_curve_receive) |
               query_builder::bind("projection_curve_long", &e::projection_curve_long) |
               query_builder::bind("projection_curve_short", &e::projection_curve_short) |
               query_builder::bind("reference_curve", &e::reference_curve) |
               query_builder::bind("reference_curve_2", &e::reference_curve_2) |
               query_builder::bind("weight_1", &e::weight_1) |
               query_builder::bind("weight_2", &e::weight_2) |
               query_builder::bind("ibor_index", &e::ibor_index) |
               query_builder::bind("rfr_curve", &e::rfr_curve) |
               query_builder::bind("rfr_index", &e::rfr_index) |
               query_builder::bind("spread", &e::spread) |
               query_builder::bind("base_curve", &e::base_curve) |
               query_builder::bind("base_curve_currency", &e::base_curve_currency) |
               query_builder::bind("numerator_curve", &e::numerator_curve) |
               query_builder::bind("numerator_curve_currency", &e::numerator_curve_currency) |
               query_builder::bind("denominator_curve", &e::denominator_curve) |
               query_builder::bind("denominator_curve_currency", &e::denominator_curve_currency) |
               query_builder::bind("extrapolate_flat", &e::extrapolate_flat) |
               query_builder::bind("modified_by", &e::modified_by) |
               query_builder::bind("performed_by", &e::performed_by) |
               query_builder::bind("change_reason_code", &e::change_reason_code) |
               query_builder::bind("change_commentary", &e::change_commentary) |
               query_builder::bind("valid_from", &e::valid_from) |
               query_builder::bind("valid_to", &e::valid_to);
    }
};

std::ostream& operator<<(std::ostream& s, const curve_segment_entity& v);

}

#endif
