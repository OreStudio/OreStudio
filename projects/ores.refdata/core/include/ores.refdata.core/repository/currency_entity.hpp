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
#ifndef ORES_REFDATA_CORE_REPOSITORY_CURRENCY_ENTITY_HPP
#define ORES_REFDATA_CORE_REPOSITORY_CURRENCY_ENTITY_HPP

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
 * @brief Represents a currency in the database.
 *
 * Each member holds the type its column has, so the layer binds and reads it
 * in the binary format and the mapper copies it to the domain member. A column
 * whose type the layer has no codec for keeps its text, and the mapper converts
 * it.
 */
struct currency_entity {
    constexpr static const char* schema = "public";
    constexpr static const char* tablename = "ores_refdata_currencies_tbl";

    std::string iso_code;
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();
    int version = 0;

    std::string name;

    std::string numeric_code;
    std::string symbol;
    std::string fraction_symbol;
    int fractions_per_unit = 0;
    std::string rounding_type;
    int rounding_precision = 0;
    std::string format;
    std::string monetary_nature;
    std::string market_tier;
    std::optional<std::string> ore_currency_type;
    std::optional<boost::uuids::uuid> image_id;
    int spot_days = 0;
    std::string day_basis;
    int base_precedence = 0;
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
        using e = currency_entity;
        namespace query_builder = ores::database::query_builder;
        return query_builder::columns<e>() | query_builder::key("iso_code", &e::iso_code) |
               query_builder::bind("tenant_id", &e::tenant_id) |
               query_builder::bind("version", &e::version) | query_builder::bind("name", &e::name) |
               query_builder::bind("numeric_code", &e::numeric_code) |
               query_builder::bind("symbol", &e::symbol) |
               query_builder::bind("fraction_symbol", &e::fraction_symbol) |
               query_builder::bind("fractions_per_unit", &e::fractions_per_unit) |
               query_builder::bind("rounding_type", &e::rounding_type) |
               query_builder::bind("rounding_precision", &e::rounding_precision) |
               query_builder::bind("format", &e::format) |
               query_builder::bind("monetary_nature", &e::monetary_nature) |
               query_builder::bind("market_tier", &e::market_tier) |
               query_builder::bind("ore_currency_type", &e::ore_currency_type) |
               query_builder::bind("image_id", &e::image_id) |
               query_builder::bind("spot_days", &e::spot_days) |
               query_builder::bind("day_basis", &e::day_basis) |
               query_builder::bind("base_precedence", &e::base_precedence) |
               query_builder::bind("modified_by", &e::modified_by) |
               query_builder::bind("performed_by", &e::performed_by) |
               query_builder::bind("change_reason_code", &e::change_reason_code) |
               query_builder::bind("change_commentary", &e::change_commentary) |
               query_builder::bind("valid_from", &e::valid_from) |
               query_builder::bind("valid_to", &e::valid_to);
    }
};

std::ostream& operator<<(std::ostream& s, const currency_entity& v);

}

#endif
