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
#ifndef ORES_MARKETDATA_CORE_REPOSITORY_MARKET_STATE_PAYLOAD_ENTITY_HPP
#define ORES_MARKETDATA_CORE_REPOSITORY_MARKET_STATE_PAYLOAD_ENTITY_HPP

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
 * @brief Represents a market state payload in the database.
 *
 * Each member holds the type its column has, so the layer binds and reads it
 * in the binary format and the mapper copies it to the domain member. A column
 * whose type the layer has no codec for keeps its text, and the mapper converts
 * it.
 */
struct market_state_payload_entity {
    constexpr static const char* schema = "public";
    constexpr static const char* tablename = "ores_marketdata_market_state_payloads_tbl";

    boost::uuids::uuid id;
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();
    boost::uuids::uuid party_id;

    boost::uuids::uuid report_instance_id;

    boost::uuids::uuid result_id;


    std::string family;


    std::string file_name;

    db_timestamp as_of;

    boost::uuids::uuid report_definition_id;
    int row_count = 0;
    int column_count = 0;
    std::string column_names;
    std::string key_values;
    bool keys_truncated = false;
    std::string payload;
    int payload_bytes = 0;
    std::string content_sha256;
    db_timestamp valid_from = db_timestamp::max();
    db_timestamp valid_to = db_timestamp::max();

    /**
     * @brief The columns of the table, named as the members, with the key.
     */
    static constexpr auto columns() {
        using e = market_state_payload_entity;
        namespace query_builder = ores::database::query_builder;
        return query_builder::columns<e>() | query_builder::key("id", &e::id) |
               query_builder::bind("tenant_id", &e::tenant_id) |
               query_builder::bind("party_id", &e::party_id) |
               query_builder::bind("report_instance_id", &e::report_instance_id) |
               query_builder::bind("result_id", &e::result_id) |
               query_builder::bind("family", &e::family) |
               query_builder::bind("file_name", &e::file_name) |
               query_builder::bind("as_of", &e::as_of) |
               query_builder::bind("report_definition_id", &e::report_definition_id) |
               query_builder::bind("row_count", &e::row_count) |
               query_builder::bind("column_count", &e::column_count) |
               query_builder::bind("column_names", &e::column_names) |
               query_builder::bind("key_values", &e::key_values) |
               query_builder::bind("keys_truncated", &e::keys_truncated) |
               query_builder::bind("payload", &e::payload) |
               query_builder::bind("payload_bytes", &e::payload_bytes) |
               query_builder::bind("content_sha256", &e::content_sha256) |
               query_builder::bind("valid_from", &e::valid_from) |
               query_builder::bind("valid_to", &e::valid_to);
    }
};

std::ostream& operator<<(std::ostream& s, const market_state_payload_entity& v);

}

#endif
