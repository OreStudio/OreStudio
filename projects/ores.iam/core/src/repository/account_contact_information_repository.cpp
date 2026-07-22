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
#include "ores.iam.core/repository/account_contact_information_repository.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.database/repository/helpers.hpp"
#include "ores.iam.api/domain/account_contact_information_json_io.hpp" // IWYU pragma: keep.
#include "ores.iam.core/repository/account_contact_information_entity.hpp"
#include "ores.iam.core/repository/account_contact_information_mapper.hpp"
#include "ores.platform/time/datetime.hpp"
#include <sqlgen/postgres.hpp>

namespace ores::iam::repository {

using namespace sqlgen;
using namespace sqlgen::literals;
using namespace ores::logging;
using namespace ores::database::repository;

std::string account_contact_information_repository::sql() {
    return generate_create_table_sql<account_contact_information_entity>(lg());
}

void account_contact_information_repository::write(context ctx,
                                                   const domain::account_contact_information& v) {
    BOOST_LOG_SEV(lg(), debug) << "Writing account contact information: " << v.id;
    execute_write_query(ctx,
                        account_contact_information_mapper::map(v),
                        lg(),
                        "Writing account contact information to database.");
}

void account_contact_information_repository::write(
    context ctx, const std::vector<domain::account_contact_information>& v) {
    BOOST_LOG_SEV(lg(), debug) << "Writing account contact informations. Count: " << v.size();
    execute_write_query(ctx,
                        account_contact_information_mapper::map(v),
                        lg(),
                        "Writing account contact informations to database.");
}

std::vector<domain::account_contact_information>
account_contact_information_repository::read_latest(context ctx) {
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<account_contact_information_entity>> |
                       where("tenant_id"_c == tid && "valid_to"_c == max.value()) |
                       order_by("id"_c);

    return execute_read_query<account_contact_information_entity,
                              domain::account_contact_information>(
        ctx,
        query,
        [](const auto& entities) { return account_contact_information_mapper::map(entities); },
        lg(),
        "Reading latest account contact informations");
}

std::vector<domain::account_contact_information>
account_contact_information_repository::read_latest(context ctx, const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest account contact information. id: " << id;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<account_contact_information_entity>> |
                       where("tenant_id"_c == tid && "id"_c == id && "valid_to"_c == max.value());

    return execute_read_query<account_contact_information_entity,
                              domain::account_contact_information>(
        ctx,
        query,
        [](const auto& entities) { return account_contact_information_mapper::map(entities); },
        lg(),
        "Reading latest account contact information by id.");
}

std::vector<domain::account_contact_information>
account_contact_information_repository::read_all(context ctx, const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading all account contact information versions. id: " << id;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<account_contact_information_entity>> |
                       where("tenant_id"_c == tid && "id"_c == id) |
                       order_by("version"_c.desc(), "valid_from"_c.desc());

    return execute_read_query<account_contact_information_entity,
                              domain::account_contact_information>(
        ctx,
        query,
        [](const auto& entities) { return account_contact_information_mapper::map(entities); },
        lg(),
        "Reading all account contact information versions by id.");
}

std::optional<domain::account_contact_information>
account_contact_information_repository::read_at_version(context ctx,
                                                        const std::string& id,
                                                        std::uint32_t version) {
    BOOST_LOG_SEV(lg(), debug) << "Reading account contact information at version. id: " << id
                               << " version: " << version;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<account_contact_information_entity>> |
                       where("tenant_id"_c == tid && "id"_c == id && "version"_c == version) |
                       sqlgen::limit(1);

    const auto entities =
        execute_read_query<account_contact_information_entity, domain::account_contact_information>(
            ctx,
            query,
            [](const auto& entities) { return account_contact_information_mapper::map(entities); },
            lg(),
            "Reading account contact information at version.");

    if (entities.empty())
        return std::nullopt;
    return entities.front();
}

std::vector<domain::account_contact_information>
account_contact_information_repository::read_latest_by_account_id(context ctx,
                                                                  const std::string& account_id,
                                                                  std::uint32_t offset,
                                                                  std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest account contact informations. account_id: "
                               << account_id << " offset: " << offset << " limit: " << limit;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query =
        sqlgen::read<std::vector<account_contact_information_entity>> |
        where("tenant_id"_c == tid && "account_id"_c == account_id && "valid_to"_c == max.value()) |
        order_by("id"_c) | sqlgen::offset(offset) | sqlgen::limit(limit);

    return execute_read_query<account_contact_information_entity,
                              domain::account_contact_information>(
        ctx,
        query,
        [](const auto& entities) { return account_contact_information_mapper::map(entities); },
        lg(),
        "Reading latest account contact informations by account_id.");
}

std::uint32_t
account_contact_information_repository::get_total_account_contact_information_count_by_account_id(
    context ctx, const std::string& account_id) {
    BOOST_LOG_SEV(lg(), debug)
        << "Retrieving total active account contact informations count. account_id: " << account_id;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));

    struct count_result {
        long long count;
    };

    const auto tid = ctx.tenant_id().to_string();
    const auto query =
        sqlgen::select_from<account_contact_information_entity>(sqlgen::count().as<"count">()) |
        where("tenant_id"_c == tid && "account_id"_c == account_id && "valid_to"_c == max.value()) |
        sqlgen::to<count_result>;

    const auto r = sqlgen::session(ctx.connection_pool()).and_then(query);
    ensure_success(r, lg());

    const auto count = static_cast<std::uint32_t>(r->count);
    BOOST_LOG_SEV(lg(), debug) << "Total active account contact informations count by account_id: "
                               << count;
    return count;
}

std::vector<domain::account_contact_information>
account_contact_information_repository::read_by_account_id_as_of(
    context ctx,
    const std::string& account_id,
    std::chrono::system_clock::time_point valid_from_bound,
    std::chrono::system_clock::time_point valid_to_bound) {
    BOOST_LOG_SEV(lg(), debug) << "Reading account contact informations as of window. account_id: "
                               << account_id;

    const auto vf(
        make_timestamp(ores::platform::time::datetime::to_db_string(valid_from_bound), lg()));
    const auto vt(
        make_timestamp(ores::platform::time::datetime::to_db_string(valid_to_bound), lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<account_contact_information_entity>> |
                       where("tenant_id"_c == tid && "account_id"_c == account_id &&
                             "valid_from"_c < vt.value() && "valid_to"_c > vf.value()) |
                       order_by("id"_c);

    return execute_read_query<account_contact_information_entity,
                              domain::account_contact_information>(
        ctx,
        query,
        [](const auto& entities) { return account_contact_information_mapper::map(entities); },
        lg(),
        "Reading account contact informations as of window by account_id.");
}
void account_contact_information_repository::remove(context ctx, const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Removing account contact information: " << id;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::delete_from<account_contact_information_entity> |
                       where("tenant_id"_c == tid && "id"_c == id && "valid_to"_c == max.value());

    execute_delete_query(ctx, query, lg(), "Removing account contact information from database.");
}

std::vector<domain::account_contact_information>
account_contact_information_repository::read_latest(context ctx,
                                                    std::uint32_t offset,
                                                    std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest account contact informations with offset: "
                               << offset << " and limit: " << limit;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<account_contact_information_entity>> |
                       where("tenant_id"_c == tid && "valid_to"_c == max.value()) |
                       order_by("id"_c) | sqlgen::offset(offset) | sqlgen::limit(limit);

    return execute_read_query<account_contact_information_entity,
                              domain::account_contact_information>(
        ctx,
        query,
        [](const auto& entities) { return account_contact_information_mapper::map(entities); },
        lg(),
        "Reading latest account contact informations with pagination.");
}

std::uint32_t
account_contact_information_repository::get_total_account_contact_information_count(context ctx) {
    BOOST_LOG_SEV(lg(), debug) << "Retrieving total active account contact information count";
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));

    struct count_result {
        long long count;
    };

    const auto tid = ctx.tenant_id().to_string();
    const auto query =
        sqlgen::select_from<account_contact_information_entity>(sqlgen::count().as<"count">()) |
        where("tenant_id"_c == tid && "valid_to"_c == max.value()) | sqlgen::to<count_result>;

    const auto r = sqlgen::session(ctx.connection_pool()).and_then(query);
    ensure_success(r, lg());

    const auto count = static_cast<std::uint32_t>(r->count);
    BOOST_LOG_SEV(lg(), debug) << "Total active account contact information count: " << count;
    return count;
}

void account_contact_information_repository::remove(context ctx,
                                                    const std::vector<std::string>& ids) {
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::delete_from<account_contact_information_entity> |
                       where("tenant_id"_c == tid && "id"_c.in(ids) && "valid_to"_c == max.value());
    execute_delete_query(ctx, query, lg(), "Batch removing account contact informations.");
}


}
