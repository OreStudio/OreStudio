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
#ifndef ORES_ANALYTICS_CORE_REPOSITORY_CREDIT_SIMULATION_CONFIG_ENTITY_HPP
#define ORES_ANALYTICS_CORE_REPOSITORY_CREDIT_SIMULATION_CONFIG_ENTITY_HPP

#include "ores.database/query_builder/columns.hpp"
#include "ores.database/repository/db_types.hpp"
#include <optional>
#include <ostream>
#include <string>

namespace ores::analytics::repository {

using db_timestamp = ores::database::repository::db_timestamp;

/**
 * @brief Represents a credit simulation config in the database.
 */
struct credit_simulation_config_entity {
    constexpr static const char* schema = "public";
    constexpr static const char* tablename = "ores_analytics_credit_simulation_configs_tbl";

    std::string id;
    std::string tenant_id;
    int version = 0;
    std::string party_id;
    std::string name;
    std::optional<std::string> configuration_id;
    std::optional<std::string> market;
    std::optional<std::string> credit;
    std::optional<bool> zero_market_pnl;
    std::optional<std::string> evaluation;
    std::optional<bool> double_default;
    std::optional<int> seed;
    std::optional<int> paths;
    std::optional<std::string> credit_mode;
    std::optional<std::string> loan_exposure_mode;
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
        using e = credit_simulation_config_entity;
        namespace query_builder = ores::database::query_builder;
        return query_builder::columns<e>() | query_builder::key("id", &e::id) |
               query_builder::bind("tenant_id", &e::tenant_id) |
               query_builder::bind("version", &e::version) |
               query_builder::bind("party_id", &e::party_id) |
               query_builder::bind("name", &e::name) |
               query_builder::bind("configuration_id", &e::configuration_id) |
               query_builder::bind("market", &e::market) |
               query_builder::bind("credit", &e::credit) |
               query_builder::bind("zero_market_pnl", &e::zero_market_pnl) |
               query_builder::bind("evaluation", &e::evaluation) |
               query_builder::bind("double_default", &e::double_default) |
               query_builder::bind("seed", &e::seed) | query_builder::bind("paths", &e::paths) |
               query_builder::bind("credit_mode", &e::credit_mode) |
               query_builder::bind("loan_exposure_mode", &e::loan_exposure_mode) |
               query_builder::bind("modified_by", &e::modified_by) |
               query_builder::bind("performed_by", &e::performed_by) |
               query_builder::bind("change_reason_code", &e::change_reason_code) |
               query_builder::bind("change_commentary", &e::change_commentary) |
               query_builder::bind("valid_from", &e::valid_from) |
               query_builder::bind("valid_to", &e::valid_to);
    }
};

std::ostream& operator<<(std::ostream& s, const credit_simulation_config_entity& v);

}

#endif
