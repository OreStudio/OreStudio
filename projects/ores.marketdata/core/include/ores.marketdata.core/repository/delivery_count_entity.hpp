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
#ifndef ORES_MARKETDATA_CORE_REPOSITORY_DELIVERY_COUNT_ENTITY_HPP
#define ORES_MARKETDATA_CORE_REPOSITORY_DELIVERY_COUNT_ENTITY_HPP

#include "ores.database/query_builder/columns.hpp"
#include "ores.database/repository/db_types.hpp"
#include "ores.marketdata.core/export.hpp"
#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <optional>
#include <ostream>
#include <string>

namespace ores::marketdata::repository {

using db_timestamp = ores::database::repository::db_timestamp;

/**
 * @brief Represents a delivery count in the database.
 *
 * Each member holds the type its column has, so the layer binds and reads it
 * in the binary format and the mapper copies it to the domain member. A column
 * whose type the layer has no codec for keeps its text, and the mapper converts
 * it.
 */
struct delivery_count_entity {
    constexpr static const char* schema = "public";
    constexpr static const char* tablename = "ores_marketdata_delivery_counts_tbl";

    boost::uuids::uuid party_id;
    std::string provider_code;
    boost::uuids::uuid feed_id;
    std::string oresmd_uri;
    db_timestamp period_start;
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();
    boost::uuids::uuid id;
    db_timestamp flushed_at;
    std::int64_t deliveries = 0;

    /**
     * @brief The columns of the table, named as the members, with the key.
     */
    static constexpr auto columns() {
        using e = delivery_count_entity;
        namespace query_builder = ores::database::query_builder;
        return query_builder::columns<e>() | query_builder::key("party_id", &e::party_id) |
               query_builder::key("provider_code", &e::provider_code) |
               query_builder::key("feed_id", &e::feed_id) |
               query_builder::key("oresmd_uri", &e::oresmd_uri) |
               query_builder::key("period_start", &e::period_start) |
               query_builder::bind("tenant_id", &e::tenant_id) | query_builder::bind("id", &e::id) |
               query_builder::bind("flushed_at", &e::flushed_at) |
               query_builder::bind("deliveries", &e::deliveries);
    }
};

ORES_MARKETDATA_CORE_EXPORT std::ostream& operator<<(std::ostream& s,
                                                     const delivery_count_entity& v);

}

#endif
