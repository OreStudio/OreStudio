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
#include "ores.refdata/repository/business_centre_repository.hpp"

#include <rfl.hpp>
#include <rfl/json.hpp>
#include <boost/date_time/posix_time/posix_time.hpp>
#include "ores.database/repository/helpers.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.refdata/domain/business_centre_json_io.hpp" // IWYU pragma: keep.
#include "ores.refdata/repository/business_centre_mapper.hpp"
#include "ores.refdata/repository/business_centre_entity.hpp"

namespace ores::refdata::repository {

using namespace sqlgen;
using namespace sqlgen::literals;
using namespace ores::logging;
using namespace ores::database::repository;

std::string business_centre_repository::sql() {
    return generate_create_table_sql<business_centre_entity>(lg());
}

void business_centre_repository::
write(context ctx, const domain::business_centre& bc) {
    BOOST_LOG_SEV(lg(), debug) << "Writing business centre to database: "
                               << bc;

    execute_write_query(ctx, business_centre_mapper::map(bc),
        lg(), "Writing business centre to database.");
}

void business_centre_repository::
write(context ctx, const std::vector<domain::business_centre>& bcs) {
    BOOST_LOG_SEV(lg(), debug) << "Writing business centres to database. Count: "
                             << bcs.size();

    execute_write_query(ctx, business_centre_mapper::map(bcs),
        lg(), "Writing business centres to database.");
}


std::vector<domain::business_centre> business_centre_repository::read_latest(context ctx) {
    const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto query = sqlgen::read<std::vector<business_centre_entity>> |
        where("valid_to"_c == max.value()) |
        order_by("valid_from"_c.desc());

    const auto sql = postgres::to_sql(query);
    BOOST_LOG_SEV(lg(), debug) << "Query: " << sql;

    return execute_read_query<business_centre_entity, domain::business_centre>(ctx, query,
        [](const auto& entities) { return business_centre_mapper::map(entities); },
        lg(), "Reading latest business centres");
}

std::vector<domain::business_centre>
business_centre_repository::read_latest(context ctx, const std::string& code) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest business centres. Code: "
                             << code;

    const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto query = sqlgen::read<std::vector<business_centre_entity>> |
        where("code"_c == code && "valid_to"_c == max.value()) |
        order_by("valid_from"_c.desc());

    return execute_read_query<business_centre_entity, domain::business_centre>(ctx, query,
        [](const auto& entities) { return business_centre_mapper::map(entities); },
        lg(), "Reading latest business centres by code.");
}

std::vector<domain::business_centre>
business_centre_repository::read_latest(context ctx, std::uint32_t offset,
                                 std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest business centres with offset: "
                               << offset << " and limit: " << limit;

    const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto query = sqlgen::read<std::vector<business_centre_entity>> |
        where("valid_to"_c == max.value()) |
        order_by("valid_from"_c.desc()) |
        sqlgen::offset(offset) |
        sqlgen::limit(limit);

    return execute_read_query<business_centre_entity, domain::business_centre>(ctx, query,
        [](const auto& entities) { return business_centre_mapper::map(entities); },
        lg(), "Reading latest business centres with pagination.");
}

std::uint32_t business_centre_repository::get_total_business_centre_count(context ctx) {
    BOOST_LOG_SEV(lg(), debug) << "Retrieving total active business centre count";

    const auto max(make_timestamp(MAX_TIMESTAMP, lg()));

    struct count_result {
        long long count;
    };

    const auto query = sqlgen::select_from<business_centre_entity>(
        sqlgen::count().as<"count">()) |
        where("valid_to"_c == max.value()) |
        sqlgen::to<count_result>;

    const auto r = sqlgen::session(ctx.connection_pool()).and_then(query);
    ensure_success(r, lg());

    const auto count = static_cast<std::uint32_t>(r->count);
    BOOST_LOG_SEV(lg(), debug) << "Total active business centre count: " << count;
    return count;
}

std::vector<domain::business_centre>
business_centre_repository::read_at_timepoint(context ctx, const std::string& as_of) {
    BOOST_LOG_SEV(lg(), debug) << "Reading business centres at timepoint: " << as_of;

    const auto ts = make_timestamp(as_of, lg());
    const auto query = sqlgen::read<std::vector<business_centre_entity>> |
        where("valid_from"_c <= ts.value() && "valid_to"_c >= ts.value());

    return execute_read_query<business_centre_entity, domain::business_centre>(ctx, query,
        [](const auto& entities) { return business_centre_mapper::map(entities); },
        lg(), "Reading business centres at timepoint.");
}

std::vector<domain::business_centre>
business_centre_repository::read_at_timepoint(context ctx, const std::string& as_of,
    const std::string& code) {

    const auto ts = make_timestamp(as_of, lg());
    const auto query = sqlgen::read<std::vector<business_centre_entity>> |
        where("code"_c == code &&
            "valid_from"_c <= ts.value() && "valid_to"_c >= ts.value());

    return execute_read_query<business_centre_entity, domain::business_centre>(ctx, query,
        [](const auto& entities) { return business_centre_mapper::map(entities); },
        lg(), "Reading business centres at timepoint by code.");
}

std::vector<domain::business_centre> business_centre_repository::read_all(context ctx) {
    const auto query = sqlgen::read<std::vector<business_centre_entity>> |
        order_by("valid_from"_c.desc());

    return execute_read_query<business_centre_entity, domain::business_centre>(ctx, query,
        [](const auto& entities) { return business_centre_mapper::map(entities); },
        lg(), "Reading all business centres.");
}

std::vector<domain::business_centre>
business_centre_repository::read_all(context ctx, const std::string& code) {
    const auto query = sqlgen::read<std::vector<business_centre_entity>> |
        where("code"_c == code) |
        order_by("valid_from"_c.desc());

    return execute_read_query<business_centre_entity, domain::business_centre>(ctx, query,
        [](const auto& entities) { return business_centre_mapper::map(entities); },
        lg(), "Reading all business centres by code");
}

void business_centre_repository::remove(context ctx, const std::string& code) {
    BOOST_LOG_SEV(lg(), debug) << "Removing business centre from database: " << code;

    const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto query = sqlgen::delete_from<business_centre_entity> |
        where("code"_c == code && "valid_to"_c == max.value());

    execute_delete_query(ctx, query, lg(), "Removing business centre from database.");
}

void business_centre_repository::remove(context ctx,
    const std::vector<std::string>& codes) {
    const auto query = sqlgen::delete_from<business_centre_entity> |
        where("code"_c.in(codes));
    execute_delete_query(ctx, query, lg(), "batch removing business_centres");
}

}
