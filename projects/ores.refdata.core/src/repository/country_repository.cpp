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
#include "ores.refdata.core/repository/country_repository.hpp"

#include <sqlgen/postgres.hpp>
#include "ores.database/repository/helpers.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.refdata.api/domain/country_json_io.hpp" // IWYU pragma: keep.
#include "ores.refdata.core/repository/country_entity.hpp"
#include "ores.refdata.core/repository/country_mapper.hpp"

namespace ores::refdata::repository {

using namespace sqlgen;
using namespace sqlgen::literals;
using namespace ores::logging;
using namespace ores::database::repository;

std::string country_repository::sql() {
    return generate_create_table_sql<country_entity>(lg());
}

void country_repository::write(context ctx, const domain::country& v) {
    BOOST_LOG_SEV(lg(), debug) << "Writing country: " << v.alpha2_code;
    execute_write_query(ctx, country_mapper::map(v),
        lg(), "Writing country to database.");
}

void country_repository::write(
    context ctx, const std::vector<domain::country>& v) {
    BOOST_LOG_SEV(lg(), debug) << "Writing countries. Count: " << v.size();
    execute_write_query(ctx, country_mapper::map(v),
        lg(), "Writing countries to database.");
}

std::vector<domain::country>
country_repository::read_latest(context ctx) {
    static auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<country_entity>> |
        where("tenant_id"_c == tid && "valid_to"_c == max.value()) |
        order_by("alpha2_code"_c);

    return execute_read_query<country_entity, domain::country>(
        ctx, query,
        [](const auto& entities) { return country_mapper::map(entities); },
        lg(), "Reading latest countries");
}

std::vector<domain::country>
country_repository::read_latest(context ctx, const std::string& alpha2_code) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest country. alpha2_code: " << alpha2_code;
    static auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<country_entity>> |
        where("tenant_id"_c == tid && "alpha2_code"_c == alpha2_code && "valid_to"_c == max.value());

    return execute_read_query<country_entity, domain::country>(
        ctx, query,
        [](const auto& entities) { return country_mapper::map(entities); },
        lg(), "Reading latest country by alpha2_code.");
}

std::vector<domain::country>
country_repository::read_all(context ctx, const std::string& alpha2_code) {
    BOOST_LOG_SEV(lg(), debug) << "Reading all country versions. alpha2_code: " << alpha2_code;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<country_entity>> |
        where("tenant_id"_c == tid && "alpha2_code"_c == alpha2_code) |
        order_by("version"_c.desc());

    return execute_read_query<country_entity, domain::country>(
        ctx, query,
        [](const auto& entities) { return country_mapper::map(entities); },
        lg(), "Reading all country versions by alpha2_code.");
}

void country_repository::remove(context ctx, const std::string& alpha2_code) {
    BOOST_LOG_SEV(lg(), debug) << "Removing country: " << alpha2_code;
    static auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::delete_from<country_entity> |
        where("tenant_id"_c == tid && "alpha2_code"_c == alpha2_code && "valid_to"_c == max.value());

    execute_delete_query(ctx, query, lg(), "Removing country from database.");
}



}
