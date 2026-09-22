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
#include "ores.iam.core/repository/account_type_repository.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.database/repository/helpers.hpp"
#include "ores.iam.api/domain/account_type_json_io.hpp" // IWYU pragma: keep.
#include "ores.iam.core/repository/account_type_entity.hpp"
#include "ores.iam.core/repository/account_type_mapper.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <sqlgen/postgres.hpp>

namespace ores::iam::repository {

using namespace sqlgen;
using namespace sqlgen::literals;
using namespace ores::logging;
using namespace ores::database::repository;

std::string account_type_repository::sql() {
    return generate_create_table_sql<account_type_entity>(lg());
}

ores::utility::domain::precondition
account_type_repository::replace_claim(context ctx, const domain::account_type& v) {
    const auto current = read_latest(ctx, v.type);
    if (current.empty())
        return {ores::utility::domain::precondition_kind::must_not_exist, std::nullopt};
    return {ores::utility::domain::precondition_kind::must_match_version,
            static_cast<std::uint32_t>(current.front().version)};
}

domain::account_type account_type_repository::apply_claim(
    context ctx, const domain::account_type& v, const ores::utility::domain::precondition& claim) {
    using ores::utility::domain::precondition_kind;
    auto t = v;
    switch (claim.kind) {
        case precondition_kind::must_not_exist:
            // Zero states that no current row exists, which is the one meaning the
            // store gives a zero version.
            t.version = 0;
            break;
        case precondition_kind::must_match_version:
            t.version = claim.version ? static_cast<int>(*claim.version) : 0;
            break;
        case precondition_kind::any: {
            // A caller that claims nothing still has to say what it replaces, so
            // the row is read and its version stated. A row that moved on between
            // this read and the write is a conflict the trigger raises, never a
            // silent overwrite.
            const auto current = read_latest(ctx, v.type);
            t.version = current.empty() ? 0 : current.front().version;
            break;
        }
    }
    return t;
}

void account_type_repository::write(context ctx, const domain::account_type& v) {
    write(ctx, v, replace_claim(ctx, v));
}

void account_type_repository::write(context ctx, const std::vector<domain::account_type>& v) {
    std::vector<ores::utility::domain::precondition> claims;
    claims.reserve(v.size());
    for (const auto& item : v)
        claims.push_back(replace_claim(ctx, item));
    write(ctx, v, claims);
}

void account_type_repository::write(context ctx,
                                    const domain::account_type& v,
                                    const ores::utility::domain::precondition& claim) {
    BOOST_LOG_SEV(lg(), debug) << "Writing account type. " << "type: " << v.type;
    const auto t = apply_claim(ctx, v, claim);
    execute_write_query(
        ctx, account_type_mapper::map(t), lg(), "Writing account type to database.");
}

void account_type_repository::write(
    context ctx,
    const std::vector<domain::account_type>& v,
    const std::vector<ores::utility::domain::precondition>& claims) {
    BOOST_LOG_SEV(lg(), debug) << "Writing account types. Count: " << v.size();
    std::vector<domain::account_type> batch;
    batch.reserve(v.size());
    for (std::size_t i = 0; i < v.size(); ++i)
        batch.push_back(apply_claim(ctx, v[i], claims[i]));
    execute_write_query(
        ctx, account_type_mapper::map(batch), lg(), "Writing account types to database.");
}

std::vector<domain::account_type> account_type_repository::read_latest(context ctx) {
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<account_type_entity>> |
                       where("tenant_id"_c == tid && "valid_to"_c == max.value()) |
                       order_by("type"_c);

    return execute_read_query<account_type_entity, domain::account_type>(
        ctx,
        query,
        [](const auto& entities) { return account_type_mapper::map(entities); },
        lg(),
        "Reading latest account types");
}

std::vector<domain::account_type> account_type_repository::read_latest(context ctx,
                                                                       const std::string& type) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest account type. " << "type: " << type;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query =
        sqlgen::read<std::vector<account_type_entity>> |
        where("tenant_id"_c == tid && "type"_c == type && "valid_to"_c == max.value());

    return execute_read_query<account_type_entity, domain::account_type>(
        ctx,
        query,
        [](const auto& entities) { return account_type_mapper::map(entities); },
        lg(),
        "Reading latest account type by type.");
}


std::vector<domain::account_type> account_type_repository::read_all(context ctx,
                                                                    const std::string& type) {
    BOOST_LOG_SEV(lg(), debug) << "Reading all account type versions. " << "type: " << type;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<account_type_entity>> |
                       where("tenant_id"_c == tid && "type"_c == type) |
                       order_by("version"_c.desc(), "valid_from"_c.desc());

    return execute_read_query<account_type_entity, domain::account_type>(
        ctx,
        query,
        [](const auto& entities) { return account_type_mapper::map(entities); },
        lg(),
        "Reading all account type versions by type.");
}

std::optional<domain::account_type> account_type_repository::read_at_version(
    context ctx, const std::string& type, std::uint32_t version) {
    BOOST_LOG_SEV(lg(), debug) << "Reading account type at version. " << "type: " << type
                               << " version: " << version;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<account_type_entity>> |
                       where("tenant_id"_c == tid && "type"_c == type && "version"_c == version) |
                       sqlgen::limit(1);

    const auto entities = execute_read_query<account_type_entity, domain::account_type>(
        ctx,
        query,
        [](const auto& entities) { return account_type_mapper::map(entities); },
        lg(),
        "Reading account type at version.");

    if (entities.empty())
        return std::nullopt;
    return entities.front();
}


account_type_repository::remove_status account_type_repository::remove(
    context ctx, const std::string& type, std::optional<std::uint32_t> version) {
    BOOST_LOG_SEV(lg(), debug) << "Removing account type. " << "type: " << type;
    const auto current = read_latest(ctx, type);
    if (current.empty())
        return remove_status::missing;
    // The protocol states the version as a uint32 and the row carries it as an
    // int, so the comparison states the conversion rather than relying on one.
    if (version && static_cast<std::uint32_t>(current.front().version) != *version)
        return remove_status::conflicting;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    // The row is named by its version as well as by its key, so the removal
    // cannot close a row that replaced the one the caller read between the
    // read above and this statement.
    const auto expected = version ? static_cast<int>(*version) : current.front().version;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::delete_from<account_type_entity> |
                       where("tenant_id"_c == tid && "type"_c == type &&
                             "valid_to"_c == max.value() && "version"_c == expected);

    execute_delete_query(ctx, query, lg(), "Removing account type from database.");
    // The delete reports no affected-row count, so the row is read back: a row
    // still open after the statement means the store refused the removal, and
    // the caller hears "conflicting" rather than "removed".
    if (!read_latest(ctx, type).empty())
        return remove_status::conflicting;
    return remove_status::removed;
}

void account_type_repository::remove(context ctx, const std::string& type) {
    static_cast<void>(remove(ctx, type, std::nullopt));
}

std::vector<domain::account_type>
account_type_repository::read_latest(context ctx, std::uint32_t offset, std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest account types with offset: " << offset
                               << " and limit: " << limit;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<account_type_entity>> |
                       where("tenant_id"_c == tid && "valid_to"_c == max.value()) |
                       order_by("type"_c) | sqlgen::offset(offset) | sqlgen::limit(limit);

    return execute_read_query<account_type_entity, domain::account_type>(
        ctx,
        query,
        [](const auto& entities) { return account_type_mapper::map(entities); },
        lg(),
        "Reading latest account types with pagination.");
}

std::uint32_t account_type_repository::get_total_type_count(context ctx) {
    BOOST_LOG_SEV(lg(), debug) << "Retrieving total active account type count";
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));

    struct count_result {
        long long count;
    };

    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::select_from<account_type_entity>(sqlgen::count().as<"count">()) |
                       where("tenant_id"_c == tid && "valid_to"_c == max.value()) |
                       sqlgen::to<count_result>;

    const auto r = sqlgen::session(ctx.connection_pool()).and_then(query);
    ensure_success(r, lg());

    const auto count = static_cast<std::uint32_t>(r->count);
    BOOST_LOG_SEV(lg(), debug) << "Total active account type count: " << count;
    return count;
}

std::vector<domain::account_type>
account_type_repository::read_latest(context ctx, const std::vector<std::string>& types) {
    if (types.empty())
        return {};
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query =
        sqlgen::read<std::vector<account_type_entity>> |
        where("tenant_id"_c == tid && "type"_c.in(types) && "valid_to"_c == max.value());
    auto result = execute_read_query<account_type_entity, domain::account_type>(
        ctx,
        query,
        [](const auto& entities) { return account_type_mapper::map(entities); },
        lg(),
        "Reading latest account types by ids.");
    return result;
}

void account_type_repository::remove(context ctx, const std::vector<std::string>& types) {
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query =
        sqlgen::delete_from<account_type_entity> |
        where("tenant_id"_c == tid && "type"_c.in(types) && "valid_to"_c == max.value());
    execute_delete_query(ctx, query, lg(), "Batch removing account types.");
}


}
