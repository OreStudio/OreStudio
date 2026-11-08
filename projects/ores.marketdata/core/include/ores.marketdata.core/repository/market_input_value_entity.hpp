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
#ifndef ORES_MARKETDATA_CORE_REPOSITORY_MARKET_INPUT_VALUE_ENTITY_HPP
#define ORES_MARKETDATA_CORE_REPOSITORY_MARKET_INPUT_VALUE_ENTITY_HPP

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
 * @brief Represents a market input value in the database.
 *
 * Each member holds the type its column has, so the layer binds and reads it
 * in the binary format and the mapper copies it to the domain member. A column
 * whose type the layer has no codec for keeps its text, and the mapper converts
 * it.
 */
struct market_input_value_entity {
    constexpr static const char* schema = "public";
    constexpr static const char* tablename = "ores_marketdata_market_input_values_tbl";

    boost::uuids::uuid id;
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();
    boost::uuids::uuid party_id;

    boost::uuids::uuid report_instance_id;

    boost::uuids::uuid result_id;


    std::string source;


    int line = 0;

    db_timestamp as_of;

    boost::uuids::uuid report_definition_id;
    std::string family;
    std::string datum_id;
    double value{};
    std::optional<db_timestamp> payment_date;
    db_timestamp valid_from = db_timestamp::max();
    db_timestamp valid_to = db_timestamp::max();

    /**
     * @brief The columns of the table, named as the members, with the key.
     */
    static constexpr auto columns() {
        using e = market_input_value_entity;
        namespace query_builder = ores::database::query_builder;
        return query_builder::columns<e>() | query_builder::key("id", &e::id) |
               query_builder::bind("tenant_id", &e::tenant_id) |
               query_builder::bind("party_id", &e::party_id) |
               query_builder::bind("report_instance_id", &e::report_instance_id) |
               query_builder::bind("result_id", &e::result_id) |
               query_builder::bind("source", &e::source) | query_builder::bind("line", &e::line) |
               query_builder::bind("as_of", &e::as_of) |
               query_builder::bind("report_definition_id", &e::report_definition_id) |
               query_builder::bind("family", &e::family) |
               query_builder::bind("datum_id", &e::datum_id) |
               query_builder::bind("value", &e::value) |
               query_builder::bind("payment_date", &e::payment_date) |
               query_builder::bind("valid_from", &e::valid_from) |
               query_builder::bind("valid_to", &e::valid_to);
    }
};

std::ostream& operator<<(std::ostream& s, const market_input_value_entity& v);

}

#endif
