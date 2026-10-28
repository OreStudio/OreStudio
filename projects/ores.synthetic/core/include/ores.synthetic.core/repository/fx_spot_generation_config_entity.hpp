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
#ifndef ORES_SYNTHETIC_CORE_REPOSITORY_FX_SPOT_GENERATION_CONFIG_ENTITY_HPP
#define ORES_SYNTHETIC_CORE_REPOSITORY_FX_SPOT_GENERATION_CONFIG_ENTITY_HPP

#include "ores.database/query_builder/columns.hpp"
#include "ores.database/repository/db_types.hpp"
#include <optional>
#include <ostream>
#include <string>

namespace ores::synthetic::repository {

using db_timestamp = ores::database::repository::db_timestamp;

/**
 * @brief Represents a fx spot generation config in the database.
 */
struct fx_spot_generation_config_entity {
    constexpr static const char* schema = "public";
    constexpr static const char* tablename = "ores_synthetic_fx_spot_generation_configs_tbl";

    std::string id;
    std::string tenant_id;
    int version = 0;
    std::string party_id;

    std::string config_id;


    std::string base_currency_code;


    std::string quote_currency_code;

    std::string source_name;
    std::string oresmd_uri;
    std::string price_source = "vintage";
    double gmm_initial_price = 0.0;
    int ticks_per_hour = 0;
    std::string process_type = "geometric";
    bool enabled = false;
    bool auto_start = false;
    std::string vintage_source;
    std::string vintage_date;
    std::optional<std::string> folder_id;
    std::string asset_class_code = "fx";
    std::string arrival_mode = "periodic";
    double branching_ratio = 0.0;
    double decay = 1.0;
    double sensitivity = 1.0;
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
        using e = fx_spot_generation_config_entity;
        namespace query_builder = ores::database::query_builder;
        return query_builder::columns<e>() | query_builder::key("id", &e::id) |
               query_builder::bind("tenant_id", &e::tenant_id) |
               query_builder::bind("version", &e::version) |
               query_builder::bind("party_id", &e::party_id) |
               query_builder::bind("config_id", &e::config_id) |
               query_builder::bind("base_currency_code", &e::base_currency_code) |
               query_builder::bind("quote_currency_code", &e::quote_currency_code) |
               query_builder::bind("source_name", &e::source_name) |
               query_builder::bind("oresmd_uri", &e::oresmd_uri) |
               query_builder::bind("price_source", &e::price_source) |
               query_builder::bind("gmm_initial_price", &e::gmm_initial_price) |
               query_builder::bind("ticks_per_hour", &e::ticks_per_hour) |
               query_builder::bind("process_type", &e::process_type) |
               query_builder::bind("enabled", &e::enabled) |
               query_builder::bind("auto_start", &e::auto_start) |
               query_builder::bind("vintage_source", &e::vintage_source) |
               query_builder::bind("vintage_date", &e::vintage_date) |
               query_builder::bind("folder_id", &e::folder_id) |
               query_builder::bind("asset_class_code", &e::asset_class_code) |
               query_builder::bind("arrival_mode", &e::arrival_mode) |
               query_builder::bind("branching_ratio", &e::branching_ratio) |
               query_builder::bind("decay", &e::decay) |
               query_builder::bind("sensitivity", &e::sensitivity) |
               query_builder::bind("modified_by", &e::modified_by) |
               query_builder::bind("performed_by", &e::performed_by) |
               query_builder::bind("change_reason_code", &e::change_reason_code) |
               query_builder::bind("change_commentary", &e::change_commentary) |
               query_builder::bind("valid_from", &e::valid_from) |
               query_builder::bind("valid_to", &e::valid_to);
    }
};

std::ostream& operator<<(std::ostream& s, const fx_spot_generation_config_entity& v);

}

#endif
