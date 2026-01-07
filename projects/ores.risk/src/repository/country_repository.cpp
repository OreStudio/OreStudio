/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2025 Marco Craveiro <marco.craveiro@gmail.com>
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
#include "ores.risk/repository/country_repository.hpp"

#include <rfl.hpp>
#include <rfl/json.hpp>
#include <boost/date_time/posix_time/posix_time.hpp>
#include "ores.database/repository/helpers.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.risk/domain/country_json_io.hpp" // IWYU pragma: keep.
#include "ores.risk/repository/country_mapper.hpp"
#include "ores.risk/repository/country_entity.hpp"

namespace ores::risk::repository {

using namespace sqlgen;
using namespace sqlgen::literals;
using namespace ores::telemetry::log;
using namespace ores::database::repository;

std::string country_repository::sql() {
    return generate_create_table_sql<country_entity>(lg());
}

void country_repository::
write(context ctx, const domain::country& country) {
    BOOST_LOG_SEV(lg(), debug) << "Writing country to database: "
                               << country;

    execute_write_query(ctx, country_mapper::map(country),
        lg(), "Writing country to database.");
}

void country_repository::
write(context ctx, const std::vector<domain::country>& countries) {
    BOOST_LOG_SEV(lg(), debug) << "Writing countries to database. Count: "
                             << countries.size();

    execute_write_query(ctx, country_mapper::map(countries),
        lg(), "Writing countries to database.");
}


std::vector<domain::country> country_repository::read_latest(context ctx) {
    static auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto query = sqlgen::read<std::vector<country_entity>> |
        where("valid_to"_c == max.value()) |
        order_by("valid_from"_c.desc());

    const auto sql = postgres::to_sql(query);
    BOOST_LOG_SEV(lg(), debug) << "Query: " << sql;

    return execute_read_query<country_entity, domain::country>(ctx, query,
        [](const auto& entities) { return country_mapper::map(entities); },
        lg(), "Reading latest countries");
}

std::vector<domain::country>
country_repository::read_latest(context ctx, const std::string& alpha2_code) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest countries. Alpha-2 code: "
                             << alpha2_code;

    static auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto query = sqlgen::read<std::vector<country_entity>> |
        where("alpha2_code"_c == alpha2_code && "valid_to"_c == max.value()) |
        order_by("valid_from"_c.desc());

    return execute_read_query<country_entity, domain::country>(ctx, query,
        [](const auto& entities) { return country_mapper::map(entities); },
        lg(), "Reading latest countries by alpha-2 code.");
}

std::vector<domain::country>
country_repository::read_latest(context ctx, std::uint32_t offset,
                                 std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest countries with offset: "
                               << offset << " and limit: " << limit;

    static auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto query = sqlgen::read<std::vector<country_entity>> |
        where("valid_to"_c == max.value()) |
        order_by("valid_from"_c.desc()) |
        sqlgen::offset(offset) |
        sqlgen::limit(limit);

    return execute_read_query<country_entity, domain::country>(ctx, query,
        [](const auto& entities) { return country_mapper::map(entities); },
        lg(), "Reading latest countries with pagination.");
}

std::uint32_t country_repository::get_total_country_count(context ctx) {
    BOOST_LOG_SEV(lg(), debug) << "Retrieving total active country count";

    static auto max(make_timestamp(MAX_TIMESTAMP, lg()));

    struct count_result {
        long long count;
    };

    const auto query = sqlgen::select_from<country_entity>(
        sqlgen::count().as<"count">()) |
        where("valid_to"_c == max.value()) |
        sqlgen::to<count_result>;

    const auto r = sqlgen::session(ctx.connection_pool()).and_then(query);
    ensure_success(r, lg());

    const auto count = static_cast<std::uint32_t>(r->count);
    BOOST_LOG_SEV(lg(), debug) << "Total active country count: " << count;
    return count;
}

std::vector<domain::country>
country_repository::read_at_timepoint(context ctx, const std::string& as_of) {
    BOOST_LOG_SEV(lg(), debug) << "Reading countries at timepoint: " << as_of;

    const auto ts = make_timestamp(as_of, lg());
    const auto query = sqlgen::read<std::vector<country_entity>> |
        where("valid_from"_c <= ts.value() && "valid_to"_c >= ts.value());

    return execute_read_query<country_entity, domain::country>(ctx, query,
        [](const auto& entities) { return country_mapper::map(entities); },
        lg(), "Reading countries at timepoint.");
}

std::vector<domain::country>
country_repository::read_at_timepoint(context ctx, const std::string& as_of,
    const std::string& alpha2_code) {

    const auto ts = make_timestamp(as_of, lg());
    const auto query = sqlgen::read<std::vector<country_entity>> |
        where("alpha2_code"_c == alpha2_code &&
            "valid_from"_c <= ts.value() && "valid_to"_c >= ts.value());

    return execute_read_query<country_entity, domain::country>(ctx, query,
        [](const auto& entities) { return country_mapper::map(entities); },
        lg(), "Reading countries at timepoint by alpha-2 code.");
}

std::vector<domain::country> country_repository::read_all(context ctx) {
    const auto query = sqlgen::read<std::vector<country_entity>> |
        order_by("valid_from"_c.desc());

    return execute_read_query<country_entity, domain::country>(ctx, query,
        [](const auto& entities) { return country_mapper::map(entities); },
        lg(), "Reading all countries.");
}

std::vector<domain::country>
country_repository::read_all(context ctx, const std::string& alpha2_code) {
    const auto query = sqlgen::read<std::vector<country_entity>> |
        where("alpha2_code"_c == alpha2_code) |
        order_by("valid_from"_c.desc());

    return execute_read_query<country_entity, domain::country>(ctx, query,
        [](const auto& entities) { return country_mapper::map(entities); },
        lg(), "Reading all countries by alpha-2 code");
}

void country_repository::remove(context ctx, const std::string& alpha2_code) {
    BOOST_LOG_SEV(lg(), debug) << "Removing country from database: " << alpha2_code;

    // Delete the country - the database trigger will close the temporal record
    // instead of actually deleting it (sets valid_to = current_timestamp)
    const auto query = sqlgen::delete_from<country_entity> |
        where("alpha2_code"_c == alpha2_code);

    execute_delete_query(ctx, query, lg(), "Removing country from database.");
}

}
