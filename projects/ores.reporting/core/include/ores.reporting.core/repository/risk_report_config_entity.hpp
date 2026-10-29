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
#ifndef ORES_REPORTING_CORE_REPOSITORY_RISK_REPORT_CONFIG_ENTITY_HPP
#define ORES_REPORTING_CORE_REPOSITORY_RISK_REPORT_CONFIG_ENTITY_HPP

#include "ores.database/query_builder/columns.hpp"
#include "ores.database/repository/db_types.hpp"
#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <optional>
#include <ostream>
#include <string>

namespace ores::reporting::repository {

using db_timestamp = ores::database::repository::db_timestamp;

/**
 * @brief Represents a risk report config in the database.
 *
 * Each member holds the type its column has, so the layer binds and reads it
 * in the binary format and the mapper copies it to the domain member. A column
 * whose type the layer has no codec for keeps its text, and the mapper converts
 * it.
 */
struct risk_report_config_entity {
    constexpr static const char* schema = "public";
    constexpr static const char* tablename = "ores_reporting_risk_report_configs_tbl";

    boost::uuids::uuid id;
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();
    int version = 0;
    boost::uuids::uuid party_id;


    std::string name;

    std::optional<std::string> description;
    boost::uuids::uuid configuration_id;
    std::optional<std::string> base_currency;
    std::string market_data_type = "eod";
    std::optional<std::string> market_data_date;
    std::string input_path = "Input";
    std::optional<std::string> input_path_market;
    std::optional<std::string> input_path_portfolio;
    std::string output_path = "Output";
    std::string log_file = "log.txt";
    std::optional<std::string> portfolio_recurse_into_sub_directories;
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
        using e = risk_report_config_entity;
        namespace query_builder = ores::database::query_builder;
        return query_builder::columns<e>() | query_builder::key("id", &e::id) |
               query_builder::bind("tenant_id", &e::tenant_id) |
               query_builder::bind("version", &e::version) |
               query_builder::bind("party_id", &e::party_id) |
               query_builder::bind("name", &e::name) |
               query_builder::bind("description", &e::description) |
               query_builder::bind("configuration_id", &e::configuration_id) |
               query_builder::bind("base_currency", &e::base_currency) |
               query_builder::bind("market_data_type", &e::market_data_type) |
               query_builder::bind_text("market_data_date", &e::market_data_date) |
               query_builder::bind("input_path", &e::input_path) |
               query_builder::bind("input_path_market", &e::input_path_market) |
               query_builder::bind("input_path_portfolio", &e::input_path_portfolio) |
               query_builder::bind("output_path", &e::output_path) |
               query_builder::bind("log_file", &e::log_file) |
               query_builder::bind("portfolio_recurse_into_sub_directories",
                                   &e::portfolio_recurse_into_sub_directories) |
               query_builder::bind("modified_by", &e::modified_by) |
               query_builder::bind("performed_by", &e::performed_by) |
               query_builder::bind("change_reason_code", &e::change_reason_code) |
               query_builder::bind("change_commentary", &e::change_commentary) |
               query_builder::bind("valid_from", &e::valid_from) |
               query_builder::bind("valid_to", &e::valid_to);
    }
};

std::ostream& operator<<(std::ostream& s, const risk_report_config_entity& v);

}

#endif
