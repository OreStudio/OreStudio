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
#ifndef ORES_TRADING_CORE_REPOSITORY_BOND_FORWARD_ENTITY_HPP
#define ORES_TRADING_CORE_REPOSITORY_BOND_FORWARD_ENTITY_HPP

#include "ores.database/query_builder/columns.hpp"
#include "ores.database/repository/db_types.hpp"
#include <optional>
#include <ostream>
#include <string>

namespace ores::trading::repository {

using db_timestamp = ores::database::repository::db_timestamp;

/**
 * @brief Represents a bond forward in the database.
 */
struct bond_forward_entity {
    constexpr static const char* schema = "public";
    constexpr static const char* tablename = "ores_trading_bond_forwards_tbl";

    std::string trade_id;
    std::string tenant_id;
    int version = 0;
    std::string trade_activity_id;
    std::optional<std::string> long_in_forward;
    std::optional<std::string> forward_maturity_date;
    std::optional<std::string> forward_settlement_date;
    std::optional<std::string> settlement;
    std::optional<std::string> amount;
    std::optional<std::string> lock_rate;
    std::optional<std::string> dv01;
    std::optional<std::string> lock_rate_day_counter;
    std::optional<std::string> settlement_dirty;
    std::optional<std::string> premium_amount;
    std::optional<std::string> premium_date;
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
        using e = bond_forward_entity;
        namespace query_builder = ores::database::query_builder;
        return query_builder::columns<e>() | query_builder::key("trade_id", &e::trade_id) |
               query_builder::bind("tenant_id", &e::tenant_id) |
               query_builder::bind("version", &e::version) |
               query_builder::bind("trade_activity_id", &e::trade_activity_id) |
               query_builder::bind("long_in_forward", &e::long_in_forward) |
               query_builder::bind("forward_maturity_date", &e::forward_maturity_date) |
               query_builder::bind("forward_settlement_date", &e::forward_settlement_date) |
               query_builder::bind("settlement", &e::settlement) |
               query_builder::bind("amount", &e::amount) |
               query_builder::bind("lock_rate", &e::lock_rate) |
               query_builder::bind("dv01", &e::dv01) |
               query_builder::bind("lock_rate_day_counter", &e::lock_rate_day_counter) |
               query_builder::bind("settlement_dirty", &e::settlement_dirty) |
               query_builder::bind("premium_amount", &e::premium_amount) |
               query_builder::bind("premium_date", &e::premium_date) |
               query_builder::bind("modified_by", &e::modified_by) |
               query_builder::bind("performed_by", &e::performed_by) |
               query_builder::bind("change_reason_code", &e::change_reason_code) |
               query_builder::bind("change_commentary", &e::change_commentary) |
               query_builder::bind("valid_from", &e::valid_from) |
               query_builder::bind("valid_to", &e::valid_to);
    }
};

std::ostream& operator<<(std::ostream& s, const bond_forward_entity& v);

}

#endif
