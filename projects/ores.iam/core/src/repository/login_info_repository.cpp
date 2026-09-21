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
 * Template: cpp_domain_type_repository.cpp.mustache
 * To modify, update the template and regenerate.
 */
#include "ores.iam.core/repository/login_info_repository.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.database/repository/helpers.hpp"
#include "ores.iam.api/domain/login_info_json_io.hpp" // IWYU pragma: keep.
#include "ores.iam.core/repository/login_info_entity.hpp"
#include "ores.iam.core/repository/login_info_mapper.hpp"
#include <sqlgen/postgres.hpp>

namespace ores::iam::repository {

using namespace sqlgen;
using namespace sqlgen::literals;
using namespace ores::logging;
using namespace ores::database::repository;

std::string login_info_repository::sql() {
    return generate_create_table_sql<login_info_entity>(lg());
}

void login_info_repository::write(context ctx, const domain::login_info& v) {
    BOOST_LOG_SEV(lg(), debug) << "Writing login info. " << "account_id: " << v.account_id;
    const auto query = sqlgen::insert_or_replace(login_info_mapper::map(v));
    const auto r = sqlgen::session(ctx.connection_pool())
                       .and_then(sqlgen::begin_transaction)
                       .and_then(query)
                       .and_then(sqlgen::commit);
    ensure_success(r, lg());
}

void login_info_repository::write(context ctx, const std::vector<domain::login_info>& v) {
    BOOST_LOG_SEV(lg(), debug) << "Writing login info. Count: " << v.size();
    const auto query = sqlgen::insert_or_replace(login_info_mapper::map(v));
    const auto r = sqlgen::session(ctx.connection_pool())
                       .and_then(sqlgen::begin_transaction)
                       .and_then(query)
                       .and_then(sqlgen::commit);
    ensure_success(r, lg());
}

std::vector<domain::login_info> login_info_repository::read_latest(context ctx) {
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<login_info_entity>> | where("tenant_id"_c == tid) |
                       order_by("account_id"_c);

    return execute_read_query<login_info_entity, domain::login_info>(
        ctx,
        query,
        [](const auto& entities) { return login_info_mapper::map(entities); },
        lg(),
        "Reading latest login info");
}

std::vector<domain::login_info> login_info_repository::read_latest(context ctx,
                                                                   const std::string& account_id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest login info. " << "account_id: " << account_id;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<login_info_entity>> |
                       where("tenant_id"_c == tid && "account_id"_c == account_id);

    return execute_read_query<login_info_entity, domain::login_info>(
        ctx,
        query,
        [](const auto& entities) { return login_info_mapper::map(entities); },
        lg(),
        "Reading latest login info by account_id.");
}


std::vector<domain::login_info> login_info_repository::read_all(context ctx,
                                                                const std::string& account_id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading all login info versions. "
                               << "account_id: " << account_id;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<login_info_entity>> |
                       where("tenant_id"_c == tid && "account_id"_c == account_id) |
                       order_by("account_id"_c);

    return execute_read_query<login_info_entity, domain::login_info>(
        ctx,
        query,
        [](const auto& entities) { return login_info_mapper::map(entities); },
        lg(),
        "Reading all login info versions by account_id.");
}


login_info_repository::remove_status login_info_repository::remove(
    context ctx, const std::string& account_id, std::optional<std::uint32_t> version) {
    BOOST_LOG_SEV(lg(), debug) << "Removing login info. " << "account_id: " << account_id;
    // The store keeps no version column, so a caller that stated a version
    // asked a question this table cannot answer.
    if (version)
        return remove_status::unsupported;
    const auto current = read_latest(ctx, account_id);
    if (current.empty())
        return remove_status::missing;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::delete_from<login_info_entity> |
                       where("tenant_id"_c == tid && "account_id"_c == account_id);

    execute_delete_query(ctx, query, lg(), "Removing login info from database.");
    return remove_status::removed;
}

void login_info_repository::remove(context ctx, const std::string& account_id) {
    static_cast<void>(remove(ctx, account_id, std::nullopt));
}

std::vector<domain::login_info>
login_info_repository::read_latest(context ctx, std::uint32_t offset, std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest login info with offset: " << offset
                               << " and limit: " << limit;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<login_info_entity>> | where("tenant_id"_c == tid) |
                       order_by("account_id"_c) | sqlgen::offset(offset) | sqlgen::limit(limit);

    return execute_read_query<login_info_entity, domain::login_info>(
        ctx,
        query,
        [](const auto& entities) { return login_info_mapper::map(entities); },
        lg(),
        "Reading latest login info with pagination.");
}

std::uint32_t login_info_repository::get_total_login_info_count(context ctx) {
    BOOST_LOG_SEV(lg(), debug) << "Retrieving total active login info count";

    struct count_result {
        long long count;
    };

    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::select_from<login_info_entity>(sqlgen::count().as<"count">()) |
                       where("tenant_id"_c == tid) | sqlgen::to<count_result>;

    const auto r = sqlgen::session(ctx.connection_pool()).and_then(query);
    ensure_success(r, lg());

    const auto count = static_cast<std::uint32_t>(r->count);
    BOOST_LOG_SEV(lg(), debug) << "Total active login info count: " << count;
    return count;
}

std::vector<domain::login_info>
login_info_repository::read_latest(context ctx, const std::vector<std::string>& account_ids) {
    if (account_ids.empty())
        return {};
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<login_info_entity>> |
                       where("tenant_id"_c == tid && "account_id"_c.in(account_ids));
    auto result = execute_read_query<login_info_entity, domain::login_info>(
        ctx,
        query,
        [](const auto& entities) { return login_info_mapper::map(entities); },
        lg(),
        "Reading latest login info by ids.");
    return result;
}

void login_info_repository::remove(context ctx, const std::vector<std::string>& account_ids) {
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::delete_from<login_info_entity> |
                       where("tenant_id"_c == tid && "account_id"_c.in(account_ids));
    execute_delete_query(ctx, query, lg(), "Batch removing login info.");
}


}
