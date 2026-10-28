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
#ifndef ORES_TRADING_CORE_REPOSITORY_BOND_ISSUE_LEG_AMORTIZATION_ENTITY_HPP
#define ORES_TRADING_CORE_REPOSITORY_BOND_ISSUE_LEG_AMORTIZATION_ENTITY_HPP

#include "ores.database/query_builder/columns.hpp"
#include "ores.database/repository/db_types.hpp"
#include <optional>
#include <ostream>
#include <string>

namespace ores::trading::repository {

using db_timestamp = ores::database::repository::db_timestamp;

/**
 * @brief Represents a bond issue leg amortization in the database.
 */
struct bond_issue_leg_amortization_entity {
    constexpr static const char* schema = "public";
    constexpr static const char* tablename = "ores_trading_bond_issue_leg_amortizations_tbl";

    std::string issue_id;
    std::string leg_number;
    std::string sequence_number;
    std::string tenant_id;
    int version = 0;
    std::string amortization_type;
    std::optional<std::string> value;
    std::optional<std::string> start_date;
    std::optional<std::string> end_date;
    std::optional<std::string> frequency;
    std::optional<bool> underflow;
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
        using e = bond_issue_leg_amortization_entity;
        namespace query_builder = ores::database::query_builder;
        return query_builder::columns<e>() | query_builder::key("issue_id", &e::issue_id) |
               query_builder::key("leg_number", &e::leg_number) |
               query_builder::key("sequence_number", &e::sequence_number) |
               query_builder::bind("tenant_id", &e::tenant_id) |
               query_builder::bind("version", &e::version) |
               query_builder::bind("amortization_type", &e::amortization_type) |
               query_builder::bind("value", &e::value) |
               query_builder::bind("start_date", &e::start_date) |
               query_builder::bind("end_date", &e::end_date) |
               query_builder::bind("frequency", &e::frequency) |
               query_builder::bind("underflow", &e::underflow) |
               query_builder::bind("modified_by", &e::modified_by) |
               query_builder::bind("performed_by", &e::performed_by) |
               query_builder::bind("change_reason_code", &e::change_reason_code) |
               query_builder::bind("change_commentary", &e::change_commentary) |
               query_builder::bind("valid_from", &e::valid_from) |
               query_builder::bind("valid_to", &e::valid_to);
    }
};

std::ostream& operator<<(std::ostream& s, const bond_issue_leg_amortization_entity& v);

}

#endif
