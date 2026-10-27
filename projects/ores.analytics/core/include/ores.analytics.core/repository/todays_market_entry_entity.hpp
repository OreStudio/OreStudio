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
#ifndef ORES_ANALYTICS_CORE_REPOSITORY_TODAYS_MARKET_ENTRY_ENTITY_HPP
#define ORES_ANALYTICS_CORE_REPOSITORY_TODAYS_MARKET_ENTRY_ENTITY_HPP

#include "ores.database/query_builder/columns.hpp"
#include "ores.database/repository/db_types.hpp"
#include <optional>
#include <ostream>
#include <string>

namespace ores::analytics::repository {

using db_timestamp = ores::database::repository::db_timestamp;

/**
 * @brief Represents a todays market entry in the database.
 */
struct todays_market_entry_entity {
    constexpr static const char* schema = "public";
    constexpr static const char* tablename = "ores_analytics_todays_market_entries_tbl";

    std::string id;
    std::string tenant_id;
    int version = 0;
    std::string party_id;
    std::string todays_market_config_id;
    std::string todays_market_collection_id;
    std::optional<std::string> key_value;
    std::optional<std::string> key_value_2;
    std::string target;
    std::optional<std::string> discounting;
    int position = 0;
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
        using e = todays_market_entry_entity;
        namespace query_builder = ores::database::query_builder;
        return query_builder::columns<e>() | query_builder::key("id", &e::id) |
               query_builder::bind("tenant_id", &e::tenant_id) |
               query_builder::bind("version", &e::version) |
               query_builder::bind("party_id", &e::party_id) |
               query_builder::bind("todays_market_config_id", &e::todays_market_config_id) |
               query_builder::bind("todays_market_collection_id", &e::todays_market_collection_id) |
               query_builder::bind("key_value", &e::key_value) |
               query_builder::bind("key_value_2", &e::key_value_2) |
               query_builder::bind("target", &e::target) |
               query_builder::bind("discounting", &e::discounting) |
               query_builder::bind("position", &e::position) |
               query_builder::bind("modified_by", &e::modified_by) |
               query_builder::bind("performed_by", &e::performed_by) |
               query_builder::bind("change_reason_code", &e::change_reason_code) |
               query_builder::bind("change_commentary", &e::change_commentary) |
               query_builder::bind("valid_from", &e::valid_from) |
               query_builder::bind("valid_to", &e::valid_to);
    }
};

std::ostream& operator<<(std::ostream& s, const todays_market_entry_entity& v);

}

#endif
