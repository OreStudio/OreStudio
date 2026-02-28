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
#include "ores.refdata/repository/currency_repository.hpp"

#include <rfl.hpp>
#include <rfl/json.hpp>
#include <boost/date_time/posix_time/posix_time.hpp>
#include "ores.database/repository/helpers.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.refdata/domain/currency_json_io.hpp" // IWYU pragma: keep.
#include "ores.refdata/repository/currency_mapper.hpp"
#include "ores.refdata/repository/currency_entity.hpp"

namespace ores::refdata::repository {

using namespace sqlgen;
using namespace sqlgen::literals;
using namespace ores::logging;
using namespace ores::database::repository;

std::string currency_repository::sql() {
    return generate_create_table_sql<currency_entity>(lg());
}

void currency_repository::
write(context ctx, const domain::currency& currency) {
    BOOST_LOG_SEV(lg(), debug) << "Writing currency to database: "
                               << currency;

    execute_write_query(ctx, currency_mapper::map(currency),
        lg(), "Writing currency to database.");
}

void currency_repository::
write(context ctx, const std::vector<domain::currency>& currencies) {
    BOOST_LOG_SEV(lg(), debug) << "Writing currencies to database. Count: "
                             << currencies.size();

    execute_write_query(ctx, currency_mapper::map(currencies),
        lg(), "Writing currencies to database.");
}


std::vector<domain::currency> currency_repository::read_latest(context ctx) {
    static auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto query = sqlgen::read<std::vector<currency_entity>> |
        where("valid_to"_c == max.value()) |
        order_by("valid_from"_c.desc());

    const auto sql = postgres::to_sql(query);
    BOOST_LOG_SEV(lg(), debug) << "Query: " << sql;

    return execute_read_query<currency_entity, domain::currency>(ctx, query,
        [](const auto& entities) { return currency_mapper::map(entities); },
        lg(), "Reading latest currencies");
}

std::vector<domain::currency>
currency_repository::read_latest(context ctx, const std::string& iso_code) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest currencies. ISO code: "
                             << iso_code;

    static auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto query = sqlgen::read<std::vector<currency_entity>> |
        where("iso_code"_c == iso_code && "valid_to"_c == max.value()) |
        order_by("valid_from"_c.desc());

    return execute_read_query<currency_entity, domain::currency>(ctx, query,
        [](const auto& entities) { return currency_mapper::map(entities); },
        lg(), "Reading latest currencies by ISO code.");
}

std::vector<domain::currency>
currency_repository::read_latest(context ctx, std::uint32_t offset,
                                 std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest currencies with offset: "
                               << offset << " and limit: " << limit;

    static auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto query = sqlgen::read<std::vector<currency_entity>> |
        where("valid_to"_c == max.value()) |
        order_by("valid_from"_c.desc()) |
        sqlgen::offset(offset) |
        sqlgen::limit(limit);

    return execute_read_query<currency_entity, domain::currency>(ctx, query,
        [](const auto& entities) { return currency_mapper::map(entities); },
        lg(), "Reading latest currencies with pagination.");
}

std::uint32_t currency_repository::get_total_currency_count(context ctx) {
    BOOST_LOG_SEV(lg(), debug) << "Retrieving total active currency count";

    static auto max(make_timestamp(MAX_TIMESTAMP, lg()));

    struct count_result {
        long long count;
    };

    const auto query = sqlgen::select_from<currency_entity>(
        sqlgen::count().as<"count">()) |
        where("valid_to"_c == max.value()) |
        sqlgen::to<count_result>;

    const auto r = sqlgen::session(ctx.connection_pool()).and_then(query);
    ensure_success(r, lg());

    const auto count = static_cast<std::uint32_t>(r->count);
    BOOST_LOG_SEV(lg(), debug) << "Total active currency count: " << count;
    return count;
}

std::vector<domain::currency>
currency_repository::read_at_timepoint(context ctx, const std::string& as_of) {
    BOOST_LOG_SEV(lg(), debug) << "Reading currencies at timepoint: " << as_of;

    const auto ts = make_timestamp(as_of, lg());
    const auto query = sqlgen::read<std::vector<currency_entity>> |
        where("valid_from"_c <= ts.value() && "valid_to"_c >= ts.value());

    return execute_read_query<currency_entity, domain::currency>(ctx, query,
        [](const auto& entities) { return currency_mapper::map(entities); },
        lg(), "Reading currencies at timepoint.");
}

std::vector<domain::currency>
currency_repository::read_at_timepoint(context ctx, const std::string& as_of,
    const std::string& iso_code) {

    const auto ts = make_timestamp(as_of, lg());
    const auto query = sqlgen::read<std::vector<currency_entity>> |
        where("iso_code"_c == iso_code &&
            "valid_from"_c <= ts.value() && "valid_to"_c >= ts.value());

    return execute_read_query<currency_entity, domain::currency>(ctx, query,
        [](const auto& entities) { return currency_mapper::map(entities); },
        lg(), "Reading currencies at timepoint by ISO code.");
}

std::vector<domain::currency> currency_repository::read_all(context ctx) {
    const auto query = sqlgen::read<std::vector<currency_entity>> |
        order_by("valid_from"_c.desc());

    return execute_read_query<currency_entity, domain::currency>(ctx, query,
        [](const auto& entities) { return currency_mapper::map(entities); },
        lg(), "Reading all currencies.");
}

std::vector<domain::currency>
currency_repository::read_all(context ctx, const std::string& iso_code) {
    const auto query = sqlgen::read<std::vector<currency_entity>> |
        where("iso_code"_c == iso_code) |
        order_by("valid_from"_c.desc());

    return execute_read_query<currency_entity, domain::currency>(ctx, query,
        [](const auto& entities) { return currency_mapper::map(entities); },
        lg(), "Reading all currencies by ISO code");
}

void currency_repository::remove(context ctx, const std::string& iso_code) {
    BOOST_LOG_SEV(lg(), debug) << "Removing currency from database: " << iso_code;

    // Delete only the current record - the database trigger will close the
    // temporal record instead of actually deleting it (sets valid_to = current_timestamp)
    static auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto query = sqlgen::delete_from<currency_entity> |
        where("iso_code"_c == iso_code && "valid_to"_c == max.value());

    execute_delete_query(ctx, query, lg(), "Removing currency from database.");
}

void currency_repository::remove(context ctx,
    const std::vector<std::string>& iso_codes) {
    const auto query = sqlgen::delete_from<currency_entity> |
        where("iso_code"_c.in(iso_codes));
    execute_delete_query(ctx, query, lg(), "batch removing currencies");
}

}
