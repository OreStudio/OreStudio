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
#ifndef ORES_REFDATA_CORE_REPOSITORY_CURVE_GLOBAL_REPORT_ENTITY_HPP
#define ORES_REFDATA_CORE_REPOSITORY_CURVE_GLOBAL_REPORT_ENTITY_HPP

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
 * @brief Represents a curve global report in the database.
 *
 * Each member holds the type its column has, so the layer binds and reads it
 * in the binary format and the mapper copies it to the domain member. A column
 * whose type the layer has no codec for keeps its text, and the mapper converts
 * it.
 */
struct curve_global_report_entity {
    constexpr static const char* schema = "public";
    constexpr static const char* tablename = "ores_refdata_curve_global_reports_tbl";

    boost::uuids::uuid id;
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();
    int version = 0;
    boost::uuids::uuid party_id;
    boost::uuids::uuid curve_configuration_id;
    std::string family;
    bool has_report = false;
    std::optional<std::string> report_on_delta_grid;
    std::optional<std::string> report_on_moneyness_grid;
    std::optional<std::string> report_on_strike_grid;
    std::optional<std::string> report_on_strike_spread_grid;
    std::optional<std::string> deltas;
    std::optional<std::string> moneyness;
    std::optional<std::string> strikes;
    std::optional<std::string> strike_spreads;
    std::optional<std::string> expiries;
    std::optional<std::string> pillar_dates;
    std::optional<std::string> underlying_tenors;
    std::optional<std::string> continuation_expiry;
    int position = 0;
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
        using e = curve_global_report_entity;
        namespace query_builder = ores::database::query_builder;
        return query_builder::columns<e>() | query_builder::key("id", &e::id) |
               query_builder::bind("tenant_id", &e::tenant_id) |
               query_builder::bind("version", &e::version) |
               query_builder::bind("party_id", &e::party_id) |
               query_builder::bind("curve_configuration_id", &e::curve_configuration_id) |
               query_builder::bind("family", &e::family) |
               query_builder::bind("has_report", &e::has_report) |
               query_builder::bind("report_on_delta_grid", &e::report_on_delta_grid) |
               query_builder::bind("report_on_moneyness_grid", &e::report_on_moneyness_grid) |
               query_builder::bind("report_on_strike_grid", &e::report_on_strike_grid) |
               query_builder::bind("report_on_strike_spread_grid",
                                   &e::report_on_strike_spread_grid) |
               query_builder::bind("deltas", &e::deltas) |
               query_builder::bind("moneyness", &e::moneyness) |
               query_builder::bind("strikes", &e::strikes) |
               query_builder::bind("strike_spreads", &e::strike_spreads) |
               query_builder::bind("expiries", &e::expiries) |
               query_builder::bind("pillar_dates", &e::pillar_dates) |
               query_builder::bind("underlying_tenors", &e::underlying_tenors) |
               query_builder::bind("continuation_expiry", &e::continuation_expiry) |
               query_builder::bind("position", &e::position) |
               query_builder::bind("modified_by", &e::modified_by) |
               query_builder::bind("performed_by", &e::performed_by) |
               query_builder::bind("change_reason_code", &e::change_reason_code) |
               query_builder::bind("change_commentary", &e::change_commentary) |
               query_builder::bind("valid_from", &e::valid_from) |
               query_builder::bind("valid_to", &e::valid_to);
    }
};

std::ostream& operator<<(std::ostream& s, const curve_global_report_entity& v);

}

#endif
