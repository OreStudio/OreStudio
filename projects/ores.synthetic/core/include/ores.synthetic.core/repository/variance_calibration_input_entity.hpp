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
#ifndef ORES_SYNTHETIC_CORE_REPOSITORY_VARIANCE_CALIBRATION_INPUT_ENTITY_HPP
#define ORES_SYNTHETIC_CORE_REPOSITORY_VARIANCE_CALIBRATION_INPUT_ENTITY_HPP

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
 * @brief Represents a variance calibration input in the database.
 *
 * Each member holds the type its column has, so the layer binds and reads it
 * in the binary format and the mapper copies it to the domain member. A column
 * whose type the layer has no codec for keeps its text, and the mapper converts
 * it.
 */
struct variance_calibration_input_entity {
    constexpr static const char* schema = "public";
    constexpr static const char* tablename = "ores_synthetic_variance_calibration_inputs_tbl";

    boost::uuids::uuid id;
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();
    int version = 0;
    boost::uuids::uuid party_id;


    std::string base_currency_code;


    std::string quote_currency_code;


    std::string fixing_uri;

    std::chrono::year_month_day window_end;


    int window_days = 0;

    std::string provider_code;
    bool inverted = false;
    boost::uuids::uuid policy_id;
    int policy_version = 0;
    int point_count = 0;
    int used_count = 0;
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
        using e = variance_calibration_input_entity;
        namespace query_builder = ores::database::query_builder;
        return query_builder::columns<e>() | query_builder::key("id", &e::id) |
               query_builder::bind("tenant_id", &e::tenant_id) |
               query_builder::bind("version", &e::version) |
               query_builder::bind("party_id", &e::party_id) |
               query_builder::bind("base_currency_code", &e::base_currency_code) |
               query_builder::bind("quote_currency_code", &e::quote_currency_code) |
               query_builder::bind("fixing_uri", &e::fixing_uri) |
               query_builder::bind("window_end", &e::window_end) |
               query_builder::bind("window_days", &e::window_days) |
               query_builder::bind("provider_code", &e::provider_code) |
               query_builder::bind("inverted", &e::inverted) |
               query_builder::bind("policy_id", &e::policy_id) |
               query_builder::bind("policy_version", &e::policy_version) |
               query_builder::bind("point_count", &e::point_count) |
               query_builder::bind("used_count", &e::used_count) |
               query_builder::bind("modified_by", &e::modified_by) |
               query_builder::bind("performed_by", &e::performed_by) |
               query_builder::bind("change_reason_code", &e::change_reason_code) |
               query_builder::bind("change_commentary", &e::change_commentary) |
               query_builder::bind("valid_from", &e::valid_from) |
               query_builder::bind("valid_to", &e::valid_to);
    }
};

std::ostream& operator<<(std::ostream& s, const variance_calibration_input_entity& v);

}

#endif
