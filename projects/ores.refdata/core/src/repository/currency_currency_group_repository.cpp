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
#include "ores.refdata.core/repository/currency_currency_group_repository.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.database/repository/helpers.hpp"
#include "ores.refdata.api/domain/currency_currency_group_json_io.hpp" // IWYU pragma: keep.
#include "ores.refdata.core/repository/currency_currency_group_entity.hpp"
#include "ores.refdata.core/repository/currency_currency_group_mapper.hpp"
#include <sqlgen/postgres.hpp>

namespace ores::refdata::repository {

using namespace sqlgen;
using namespace sqlgen::literals;
using namespace ores::logging;
using namespace ores::database::repository;

std::string currency_currency_group_repository::sql() {
    return generate_create_table_sql<currency_currency_group_entity>(lg());
}

currency_currency_group_repository::currency_currency_group_repository(context ctx)
    : ctx_(std::move(ctx)) {}

void currency_currency_group_repository::write(
    const domain::currency_currency_group& currency_group) {
    BOOST_LOG_SEV(lg(), debug) << "Writing currency group to database: "
                               << currency_group.currency_iso_code << "/"
                               << currency_group.currency_group_code;
    execute_write_query(ctx_,
                        currency_currency_group_mapper::map(currency_group),
                        lg(),
                        "writing currency group to database");
}

void currency_currency_group_repository::write(
    const std::vector<domain::currency_currency_group>& currency_groups) {
    BOOST_LOG_SEV(lg(), debug) << "Writing currency groups to database. Count: "
                               << currency_groups.size();
    execute_write_query(ctx_,
                        currency_currency_group_mapper::map(currency_groups),
                        lg(),
                        "writing currency groups to database");
}

std::vector<domain::currency_currency_group> currency_currency_group_repository::read_latest() {
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx_.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<currency_currency_group_entity>> |
                       where("tenant_id"_c == tid && "valid_to"_c == max.value()) |
                       order_by("currency_iso_code"_c, "currency_group_code"_c);

    return execute_read_query<currency_currency_group_entity, domain::currency_currency_group>(
        ctx_,
        query,
        [](const auto& entities) { return currency_currency_group_mapper::map(entities); },
        lg(),
        "Reading latest currency groups");
}

std::vector<domain::currency_currency_group>
currency_currency_group_repository::read_latest_by_currency(const std::string& currency_iso_code) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest currency groups. Currency: " << currency_iso_code;

    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx_.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<currency_currency_group_entity>> |
                       where("tenant_id"_c == tid && "currency_iso_code"_c == currency_iso_code &&
                             "valid_to"_c == max.value()) |
                       order_by("currency_group_code"_c);

    return execute_read_query<currency_currency_group_entity, domain::currency_currency_group>(
        ctx_,
        query,
        [](const auto& entities) { return currency_currency_group_mapper::map(entities); },
        lg(),
        "Reading latest currency groups by currency.");
}

std::vector<domain::currency_currency_group>
currency_currency_group_repository::read_latest_by_group(const std::string& currency_group_code) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest currency groups. Group: " << currency_group_code;

    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx_.tenant_id().to_string();
    const auto query =
        sqlgen::read<std::vector<currency_currency_group_entity>> |
        where("tenant_id"_c == tid && "currency_group_code"_c == currency_group_code &&
              "valid_to"_c == max.value()) |
        order_by("currency_iso_code"_c);

    return execute_read_query<currency_currency_group_entity, domain::currency_currency_group>(
        ctx_,
        query,
        [](const auto& entities) { return currency_currency_group_mapper::map(entities); },
        lg(),
        "Reading latest currency groups by group.");
}

void currency_currency_group_repository::remove(const std::string& currency_iso_code,
                                                const std::string& currency_group_code) {
    BOOST_LOG_SEV(lg(), debug) << "Removing currency group from database: " << currency_iso_code
                               << "/" << currency_group_code;

    const auto tid = ctx_.tenant_id().to_string();
    const auto query = sqlgen::delete_from<currency_currency_group_entity> |
                       where("tenant_id"_c == tid && "currency_iso_code"_c == currency_iso_code &&
                             "currency_group_code"_c == currency_group_code);

    execute_delete_query(ctx_, query, lg(), "removing currency group from database");
}

void currency_currency_group_repository::remove_by_currency(const std::string& currency_iso_code) {
    BOOST_LOG_SEV(lg(), debug) << "Removing all currency groups from database: "
                               << currency_iso_code;

    const auto tid = ctx_.tenant_id().to_string();
    const auto query = sqlgen::delete_from<currency_currency_group_entity> |
                       where("tenant_id"_c == tid && "currency_iso_code"_c == currency_iso_code);

    execute_delete_query(ctx_, query, lg(), "removing all currency groups from database");
}

}
