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
#ifndef ORES_SYNTHETIC_CORE_REPOSITORY_VARIANCE_CALIBRATION_ENTITY_HPP
#define ORES_SYNTHETIC_CORE_REPOSITORY_VARIANCE_CALIBRATION_ENTITY_HPP

#include "ores.database/query_builder/columns.hpp"
#include "ores.database/repository/db_types.hpp"
#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <optional>
#include <ostream>
#include <string>

namespace ores::synthetic::repository {

using db_timestamp = ores::database::repository::db_timestamp;

/**
 * @brief Represents a variance calibration in the database.
 *
 * Each member holds the type its column has, so the layer binds and reads it
 * in the binary format and the mapper copies it to the domain member. A column
 * whose type the layer has no codec for keeps its text, and the mapper converts
 * it.
 */
struct variance_calibration_entity {
    constexpr static const char* schema = "public";
    constexpr static const char* tablename = "ores_synthetic_variance_calibrations_tbl";

    boost::uuids::uuid id;
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();
    int version = 0;
    boost::uuids::uuid party_id;
    std::string base_currency_code;
    std::string quote_currency_code;
    boost::uuids::uuid input_id;
    std::string method;
    int method_version = 0;
    std::optional<double> alpha;
    std::optional<double> gamma;
    std::optional<double> beta;
    std::optional<double> persistence;
    std::optional<double> long_run_variance;
    std::optional<double> starting_variance_ratio;
    std::optional<double> log_likelihood;
    int returns_used = 0;
    int starts = 0;
    bool starts_agreed = false;
    std::string state = "proposed";
    std::string state_reason = "";
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
        using e = variance_calibration_entity;
        namespace query_builder = ores::database::query_builder;
        return query_builder::columns<e>() | query_builder::key("id", &e::id) |
               query_builder::bind("tenant_id", &e::tenant_id) |
               query_builder::bind("version", &e::version) |
               query_builder::bind("party_id", &e::party_id) |
               query_builder::bind("base_currency_code", &e::base_currency_code) |
               query_builder::bind("quote_currency_code", &e::quote_currency_code) |
               query_builder::bind("input_id", &e::input_id) |
               query_builder::bind("method", &e::method) |
               query_builder::bind("method_version", &e::method_version) |
               query_builder::bind("alpha", &e::alpha) | query_builder::bind("gamma", &e::gamma) |
               query_builder::bind("beta", &e::beta) |
               query_builder::bind("persistence", &e::persistence) |
               query_builder::bind("long_run_variance", &e::long_run_variance) |
               query_builder::bind("starting_variance_ratio", &e::starting_variance_ratio) |
               query_builder::bind("log_likelihood", &e::log_likelihood) |
               query_builder::bind("returns_used", &e::returns_used) |
               query_builder::bind("starts", &e::starts) |
               query_builder::bind("starts_agreed", &e::starts_agreed) |
               query_builder::bind("state", &e::state) |
               query_builder::bind("state_reason", &e::state_reason) |
               query_builder::bind("modified_by", &e::modified_by) |
               query_builder::bind("performed_by", &e::performed_by) |
               query_builder::bind("change_reason_code", &e::change_reason_code) |
               query_builder::bind("change_commentary", &e::change_commentary) |
               query_builder::bind("valid_from", &e::valid_from) |
               query_builder::bind("valid_to", &e::valid_to);
    }
};

std::ostream& operator<<(std::ostream& s, const variance_calibration_entity& v);

}

#endif
