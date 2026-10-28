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
#ifndef ORES_MARKETDATA_CORE_REPOSITORY_CURVE_PUBLISH_ENTITY_HPP
#define ORES_MARKETDATA_CORE_REPOSITORY_CURVE_PUBLISH_ENTITY_HPP

#include "ores.database/query_builder/columns.hpp"
#include "ores.database/repository/db_types.hpp"
#include <optional>
#include <ostream>
#include <string>

namespace ores::marketdata::repository {

using db_timestamp = ores::database::repository::db_timestamp;

/**
 * @brief Represents a curve publish in the database.
 */
struct curve_publish_entity {
    constexpr static const char* schema = "public";
    constexpr static const char* tablename = "ores_marketdata_curve_publishes_tbl";

    std::string id;
    std::string tenant_id;
    int version = 0;
    std::string request_id;

    std::string party_id;
    std::string bootstrap_config_id;
    std::string output_series_id;
    std::string as_of;
    std::string published_by = "";
    std::optional<db_timestamp> published_at;
    std::string withdrawn_by = "";
    std::optional<db_timestamp> withdrawn_at;
    std::string withdrawn_reason_code = "";
    std::optional<std::string> withdrawal_request_id;
    std::optional<db_timestamp> superseded_at;
    std::optional<std::string> superseded_by_request_id;
    std::optional<db_timestamp> superseded_as_of;
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
        using e = curve_publish_entity;
        namespace query_builder = ores::database::query_builder;
        return query_builder::columns<e>() | query_builder::key("id", &e::id) |
               query_builder::bind("tenant_id", &e::tenant_id) |
               query_builder::bind("version", &e::version) |
               query_builder::bind("request_id", &e::request_id) |
               query_builder::bind("party_id", &e::party_id) |
               query_builder::bind("bootstrap_config_id", &e::bootstrap_config_id) |
               query_builder::bind("output_series_id", &e::output_series_id) |
               query_builder::bind("as_of", &e::as_of) |
               query_builder::bind("published_by", &e::published_by) |
               query_builder::bind("published_at", &e::published_at) |
               query_builder::bind("withdrawn_by", &e::withdrawn_by) |
               query_builder::bind("withdrawn_at", &e::withdrawn_at) |
               query_builder::bind("withdrawn_reason_code", &e::withdrawn_reason_code) |
               query_builder::bind("withdrawal_request_id", &e::withdrawal_request_id) |
               query_builder::bind("superseded_at", &e::superseded_at) |
               query_builder::bind("superseded_by_request_id", &e::superseded_by_request_id) |
               query_builder::bind("superseded_as_of", &e::superseded_as_of) |
               query_builder::bind("modified_by", &e::modified_by) |
               query_builder::bind("performed_by", &e::performed_by) |
               query_builder::bind("change_reason_code", &e::change_reason_code) |
               query_builder::bind("change_commentary", &e::change_commentary) |
               query_builder::bind("valid_from", &e::valid_from) |
               query_builder::bind("valid_to", &e::valid_to);
    }
};

std::ostream& operator<<(std::ostream& s, const curve_publish_entity& v);

}

#endif
