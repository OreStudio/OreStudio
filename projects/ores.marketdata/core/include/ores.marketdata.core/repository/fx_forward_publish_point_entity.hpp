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
#ifndef ORES_MARKETDATA_CORE_REPOSITORY_FX_FORWARD_PUBLISH_POINT_ENTITY_HPP
#define ORES_MARKETDATA_CORE_REPOSITORY_FX_FORWARD_PUBLISH_POINT_ENTITY_HPP

#include "ores.database/query_builder/columns.hpp"
#include "ores.database/repository/db_types.hpp"
#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <optional>
#include <ostream>
#include <string>

namespace ores::marketdata::repository {

using db_timestamp = ores::database::repository::db_timestamp;

/**
 * @brief Represents a fx forward publish point in the database.
 *
 * Each member holds the type its column has, so the layer binds and reads it
 * in the binary format and the mapper copies it to the domain member. A column
 * whose type the layer has no codec for keeps its text, and the mapper converts
 * it.
 */
struct fx_forward_publish_point_entity {
    constexpr static const char* schema = "public";
    constexpr static const char* tablename = "ores_marketdata_fx_forward_publish_points_tbl";

    boost::uuids::uuid id;
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();
    int version = 0;
    boost::uuids::uuid publish_id;
    int sequence_index = 0;
    std::string tenor_code;
    std::chrono::year_month_day near_date;
    std::chrono::year_month_day value_date;
    double base_discount{};
    double quote_discount{};
    double forward_rate{};
    double forward_points{};
    double swap_points{};
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
        using e = fx_forward_publish_point_entity;
        namespace query_builder = ores::database::query_builder;
        return query_builder::columns<e>() | query_builder::key("id", &e::id) |
               query_builder::bind("tenant_id", &e::tenant_id) |
               query_builder::bind("version", &e::version) |
               query_builder::bind("publish_id", &e::publish_id) |
               query_builder::bind("sequence_index", &e::sequence_index) |
               query_builder::bind("tenor_code", &e::tenor_code) |
               query_builder::bind("near_date", &e::near_date) |
               query_builder::bind("value_date", &e::value_date) |
               query_builder::bind("base_discount", &e::base_discount) |
               query_builder::bind("quote_discount", &e::quote_discount) |
               query_builder::bind("forward_rate", &e::forward_rate) |
               query_builder::bind("forward_points", &e::forward_points) |
               query_builder::bind("swap_points", &e::swap_points) |
               query_builder::bind("modified_by", &e::modified_by) |
               query_builder::bind("performed_by", &e::performed_by) |
               query_builder::bind("change_reason_code", &e::change_reason_code) |
               query_builder::bind("change_commentary", &e::change_commentary) |
               query_builder::bind("valid_from", &e::valid_from) |
               query_builder::bind("valid_to", &e::valid_to);
    }
};

std::ostream& operator<<(std::ostream& s, const fx_forward_publish_point_entity& v);

}

#endif
