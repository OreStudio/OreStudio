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
#include "ores.iam/repository/change_reason_category_repository.hpp"

#include <sqlgen/postgres.hpp>
#include "ores.database/repository/helpers.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.iam/domain/change_reason_category_json_io.hpp" // IWYU pragma: keep.
#include "ores.iam/repository/change_reason_category_entity.hpp"
#include "ores.iam/repository/change_reason_category_mapper.hpp"

namespace ores::iam::repository {

using namespace sqlgen;
using namespace sqlgen::literals;
using namespace ores::logging;
using namespace ores::database::repository;

std::string change_reason_category_repository::sql() {
    return generate_create_table_sql<change_reason_category_entity>(lg());
}

change_reason_category_repository::change_reason_category_repository(context ctx)
    : ctx_(std::move(ctx)) {}

void change_reason_category_repository::write(
    const domain::change_reason_category& category) {
    BOOST_LOG_SEV(lg(), debug) << "Writing change_reason_category to database: "
                               << category.code;

    execute_write_query(ctx_, change_reason_category_mapper::map(category),
        lg(), "writing change_reason_category to database");
}

void change_reason_category_repository::write(
    const std::vector<domain::change_reason_category>& categories) {
    BOOST_LOG_SEV(lg(), debug) << "Writing change_reason_categories to database. Count: "
                               << categories.size();

    execute_write_query(ctx_, change_reason_category_mapper::map(categories),
        lg(), "writing change_reason_categories to database");
}

std::vector<domain::change_reason_category>
change_reason_category_repository::read_latest() {
    static auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto query = sqlgen::read<std::vector<change_reason_category_entity>> |
        where("valid_to"_c == max.value()) |
        order_by("code"_c);

    return execute_read_query<change_reason_category_entity,
                              domain::change_reason_category>(ctx_, query,
        [](const auto& entities) {
            return change_reason_category_mapper::map(entities);
        },
        lg(), "Reading latest change_reason_categories");
}

std::vector<domain::change_reason_category>
change_reason_category_repository::read_latest(const std::string& code) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest change_reason_category. Code: "
                               << code;

    static auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto query = sqlgen::read<std::vector<change_reason_category_entity>> |
        where("code"_c == code && "valid_to"_c == max.value());

    return execute_read_query<change_reason_category_entity,
                              domain::change_reason_category>(ctx_, query,
        [](const auto& entities) {
            return change_reason_category_mapper::map(entities);
        },
        lg(), "Reading latest change_reason_category by code.");
}

std::vector<domain::change_reason_category>
change_reason_category_repository::read_latest(std::uint32_t offset,
                                                std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest change_reason_categories with offset: "
                               << offset << " and limit: " << limit;

    static auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto query = sqlgen::read<std::vector<change_reason_category_entity>> |
        where("valid_to"_c == max.value()) |
        order_by("code"_c) |
        sqlgen::offset(offset) |
        sqlgen::limit(limit);

    return execute_read_query<change_reason_category_entity,
                              domain::change_reason_category>(ctx_, query,
        [](const auto& entities) {
            return change_reason_category_mapper::map(entities);
        },
        lg(), "Reading latest change_reason_categories with pagination.");
}

std::uint32_t change_reason_category_repository::get_total_count() {
    BOOST_LOG_SEV(lg(), debug) << "Retrieving total active change_reason_category count";

    static auto max(make_timestamp(MAX_TIMESTAMP, lg()));

    struct count_result {
        long long count;
    };

    const auto query = sqlgen::select_from<change_reason_category_entity>(
        sqlgen::count().as<"count">()) |
        where("valid_to"_c == max.value()) |
        sqlgen::to<count_result>;

    const auto r = sqlgen::session(ctx_.connection_pool()).and_then(query);
    ensure_success(r, lg());

    const auto count = static_cast<std::uint32_t>(r->count);
    BOOST_LOG_SEV(lg(), debug) << "Total active change_reason_category count: " << count;
    return count;
}

std::vector<domain::change_reason_category>
change_reason_category_repository::read_all(const std::string& code) {
    BOOST_LOG_SEV(lg(), debug) << "Reading all change_reason_category versions. Code: "
                               << code;

    const auto query = sqlgen::read<std::vector<change_reason_category_entity>> |
        where("code"_c == code) |
        order_by("version"_c.desc());

    return execute_read_query<change_reason_category_entity,
                              domain::change_reason_category>(ctx_, query,
        [](const auto& entities) {
            return change_reason_category_mapper::map(entities);
        },
        lg(), "Reading all change_reason_category versions by code.");
}

void change_reason_category_repository::remove(const std::string& code) {
    BOOST_LOG_SEV(lg(), debug) << "Removing change_reason_category from database: "
                               << code;

    // Delete the category - the database rule will close the temporal record
    // instead of actually deleting it (sets valid_to = current_timestamp)
    const auto query = sqlgen::delete_from<change_reason_category_entity> |
        where("code"_c == code);

    execute_delete_query(ctx_, query, lg(),
        "removing change_reason_category from database");
}

}
