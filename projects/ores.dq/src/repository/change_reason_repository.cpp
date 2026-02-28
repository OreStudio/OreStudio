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
#include "ores.dq/repository/change_reason_repository.hpp"

#include <sqlgen/postgres.hpp>
#include "ores.database/repository/helpers.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.dq/domain/change_reason_json_io.hpp" // IWYU pragma: keep.
#include "ores.dq/repository/change_reason_entity.hpp"
#include "ores.dq/repository/change_reason_mapper.hpp"

namespace ores::dq::repository {

using namespace sqlgen;
using namespace sqlgen::literals;
using namespace ores::logging;
using namespace ores::database::repository;

std::string change_reason_repository::sql() {
    return generate_create_table_sql<change_reason_entity>(lg());
}

change_reason_repository::change_reason_repository(context ctx)
    : ctx_(std::move(ctx)) {}

void change_reason_repository::write(const domain::change_reason& reason) {
    BOOST_LOG_SEV(lg(), debug) << "Writing change_reason to database: "
                               << reason.code;

    execute_write_query(ctx_, change_reason_mapper::map(reason),
        lg(), "writing change_reason to database");
}

void change_reason_repository::write(
    const std::vector<domain::change_reason>& reasons) {
    BOOST_LOG_SEV(lg(), debug) << "Writing change_reasons to database. Count: "
                               << reasons.size();

    execute_write_query(ctx_, change_reason_mapper::map(reasons),
        lg(), "writing change_reasons to database");
}

std::vector<domain::change_reason>
change_reason_repository::read_latest() {
    const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto query = sqlgen::read<std::vector<change_reason_entity>> |
        where("valid_to"_c == max.value()) |
        order_by("display_order"_c, "code"_c);

    return execute_read_query<change_reason_entity, domain::change_reason>(
        ctx_, query,
        [](const auto& entities) { return change_reason_mapper::map(entities); },
        lg(), "Reading latest change_reasons");
}

std::vector<domain::change_reason>
change_reason_repository::read_latest(const std::string& code) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest change_reason. Code: " << code;

    const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto query = sqlgen::read<std::vector<change_reason_entity>> |
        where("code"_c == code && "valid_to"_c == max.value());

    return execute_read_query<change_reason_entity, domain::change_reason>(
        ctx_, query,
        [](const auto& entities) { return change_reason_mapper::map(entities); },
        lg(), "Reading latest change_reason by code.");
}

std::vector<domain::change_reason>
change_reason_repository::read_latest(std::uint32_t offset, std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest change_reasons with offset: "
                               << offset << " and limit: " << limit;

    const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto query = sqlgen::read<std::vector<change_reason_entity>> |
        where("valid_to"_c == max.value()) |
        order_by("display_order"_c, "code"_c) |
        sqlgen::offset(offset) |
        sqlgen::limit(limit);

    return execute_read_query<change_reason_entity, domain::change_reason>(
        ctx_, query,
        [](const auto& entities) { return change_reason_mapper::map(entities); },
        lg(), "Reading latest change_reasons with pagination.");
}

std::vector<domain::change_reason>
change_reason_repository::read_latest_by_category(const std::string& category_code) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest change_reasons by category: "
                               << category_code;

    const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto query = sqlgen::read<std::vector<change_reason_entity>> |
        where("category_code"_c == category_code && "valid_to"_c == max.value()) |
        order_by("display_order"_c, "code"_c);

    return execute_read_query<change_reason_entity, domain::change_reason>(
        ctx_, query,
        [](const auto& entities) { return change_reason_mapper::map(entities); },
        lg(), "Reading latest change_reasons by category.");
}

std::uint32_t change_reason_repository::get_total_count() {
    BOOST_LOG_SEV(lg(), debug) << "Retrieving total active change_reason count";

    const auto max(make_timestamp(MAX_TIMESTAMP, lg()));

    struct count_result {
        long long count;
    };

    const auto query = sqlgen::select_from<change_reason_entity>(
        sqlgen::count().as<"count">()) |
        where("valid_to"_c == max.value()) |
        sqlgen::to<count_result>;

    const auto r = sqlgen::session(ctx_.connection_pool()).and_then(query);
    ensure_success(r, lg());

    const auto count = static_cast<std::uint32_t>(r->count);
    BOOST_LOG_SEV(lg(), debug) << "Total active change_reason count: " << count;
    return count;
}

std::vector<domain::change_reason>
change_reason_repository::read_all(const std::string& code) {
    BOOST_LOG_SEV(lg(), debug) << "Reading all change_reason versions. Code: "
                               << code;

    const auto query = sqlgen::read<std::vector<change_reason_entity>> |
        where("code"_c == code) |
        order_by("version"_c.desc());

    return execute_read_query<change_reason_entity, domain::change_reason>(
        ctx_, query,
        [](const auto& entities) { return change_reason_mapper::map(entities); },
        lg(), "Reading all change_reason versions by code.");
}

void change_reason_repository::remove(const std::string& code) {
    BOOST_LOG_SEV(lg(), debug) << "Removing change_reason from database: " << code;

    // Delete the reason - the database rule will close the temporal record
    // instead of actually deleting it (sets valid_to = current_timestamp)
    const auto query = sqlgen::delete_from<change_reason_entity> |
        where("code"_c == code);

    execute_delete_query(ctx_, query, lg(), "removing change_reason from database");
}

void change_reason_repository::remove(const std::vector<std::string>& codes) {
    const auto query = sqlgen::delete_from<change_reason_entity> |
        where("code"_c.in(codes));
    execute_delete_query(ctx_, query, lg(), "batch removing change_reasons");
}

}
