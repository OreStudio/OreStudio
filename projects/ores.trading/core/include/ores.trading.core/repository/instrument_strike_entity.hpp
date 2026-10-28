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
#ifndef ORES_TRADING_CORE_REPOSITORY_INSTRUMENT_STRIKE_ENTITY_HPP
#define ORES_TRADING_CORE_REPOSITORY_INSTRUMENT_STRIKE_ENTITY_HPP

#include "ores.database/query_builder/columns.hpp"
#include "ores.database/repository/db_types.hpp"
#include <optional>
#include <ostream>
#include <string>

namespace ores::trading::repository {

using db_timestamp = ores::database::repository::db_timestamp;

/**
 * @brief Represents a instrument strike in the database.
 */
struct instrument_strike_entity {
    constexpr static const char* schema = "public";
    constexpr static const char* tablename = "ores_trading_instrument_strikes_tbl";

    std::string trade_id;
    std::string strike_role;
    std::string sequence_number;
    std::string tenant_id;
    int version = 0;
    std::string trade_activity_id;
    std::optional<std::string> price_value;
    std::optional<std::string> price_currency;
    std::optional<std::string> yield_value;
    std::optional<std::string> yield_compounding;
    std::optional<std::string> bare_value;
    std::optional<std::string> bare_currency;
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
        using e = instrument_strike_entity;
        namespace query_builder = ores::database::query_builder;
        return query_builder::columns<e>() | query_builder::key("trade_id", &e::trade_id) |
               query_builder::key("strike_role", &e::strike_role) |
               query_builder::key("sequence_number", &e::sequence_number) |
               query_builder::bind("tenant_id", &e::tenant_id) |
               query_builder::bind("version", &e::version) |
               query_builder::bind("trade_activity_id", &e::trade_activity_id) |
               query_builder::bind("price_value", &e::price_value) |
               query_builder::bind("price_currency", &e::price_currency) |
               query_builder::bind("yield_value", &e::yield_value) |
               query_builder::bind("yield_compounding", &e::yield_compounding) |
               query_builder::bind("bare_value", &e::bare_value) |
               query_builder::bind("bare_currency", &e::bare_currency) |
               query_builder::bind("modified_by", &e::modified_by) |
               query_builder::bind("performed_by", &e::performed_by) |
               query_builder::bind("change_reason_code", &e::change_reason_code) |
               query_builder::bind("change_commentary", &e::change_commentary) |
               query_builder::bind("valid_from", &e::valid_from) |
               query_builder::bind("valid_to", &e::valid_to);
    }
};

std::ostream& operator<<(std::ostream& s, const instrument_strike_entity& v);

}

#endif
