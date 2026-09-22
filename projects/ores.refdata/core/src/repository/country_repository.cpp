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
#include "ores.refdata.core/repository/country_repository.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.database/repository/helpers.hpp"
#include "ores.refdata.api/domain/country_json_io.hpp" // IWYU pragma: keep.
#include "ores.refdata.core/repository/country_entity.hpp"
#include "ores.refdata.core/repository/country_mapper.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <sqlgen/postgres.hpp>


namespace ores::refdata::repository {

using namespace sqlgen;
using namespace sqlgen::literals;
using namespace ores::logging;
using namespace ores::database::repository;

std::string country_repository::sql() {
    return generate_create_table_sql<country_entity>(lg());
}

ores::utility::domain::precondition country_repository::replace_claim(context ctx,
                                                                      const domain::country& v) {
    const auto current = read_latest(ctx, v.alpha2_code);
    if (current.empty())
        return {ores::utility::domain::precondition_kind::must_not_exist, std::nullopt};
    return {ores::utility::domain::precondition_kind::must_match_version,
            static_cast<std::uint32_t>(current.front().version)};
}

domain::country country_repository::apply_claim(context ctx,
                                                const domain::country& v,
                                                const ores::utility::domain::precondition& claim) {
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
            const auto current = read_latest(ctx, v.alpha2_code);
            t.version = current.empty() ? 0 : current.front().version;
            break;
        }
    }
    return t;
}

void country_repository::write(context ctx, const domain::country& v) {
    write(ctx, v, replace_claim(ctx, v));
}

void country_repository::write(context ctx, const std::vector<domain::country>& v) {
    std::vector<ores::utility::domain::precondition> claims;
    claims.reserve(v.size());
    for (const auto& item : v)
        claims.push_back(replace_claim(ctx, item));
    write(ctx, v, claims);
}

void country_repository::write(context ctx,
                               const domain::country& v,
                               const ores::utility::domain::precondition& claim) {
    BOOST_LOG_SEV(lg(), debug) << "Writing country. " << "alpha2_code: " << v.alpha2_code;
    const auto t = apply_claim(ctx, v, claim);
    execute_write_query(ctx, country_mapper::map(t), lg(), "Writing country to database.");
}

void country_repository::write(context ctx,
                               const std::vector<domain::country>& v,
                               const std::vector<ores::utility::domain::precondition>& claims) {
    BOOST_LOG_SEV(lg(), debug) << "Writing countries. Count: " << v.size();
    std::vector<domain::country> batch;
    batch.reserve(v.size());
    for (std::size_t i = 0; i < v.size(); ++i)
        batch.push_back(apply_claim(ctx, v[i], claims[i]));
    execute_write_query(ctx, country_mapper::map(batch), lg(), "Writing countries to database.");
}

std::vector<domain::country> country_repository::read_latest(context ctx) {
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<country_entity>> |
                       where("tenant_id"_c == tid && "valid_to"_c == max.value()) |
                       order_by("alpha2_code"_c);

    return execute_read_query<country_entity, domain::country>(
        ctx,
        query,
        [](const auto& entities) { return country_mapper::map(entities); },
        lg(),
        "Reading latest countries");
}

std::vector<domain::country> country_repository::read_latest(context ctx,
                                                             const std::string& alpha2_code) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest country. " << "alpha2_code: " << alpha2_code;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<country_entity>> |
                       where("tenant_id"_c == tid && "alpha2_code"_c == alpha2_code &&
                             "valid_to"_c == max.value());

    return execute_read_query<country_entity, domain::country>(
        ctx,
        query,
        [](const auto& entities) { return country_mapper::map(entities); },
        lg(),
        "Reading latest country by alpha2_code.");
}


std::vector<domain::country> country_repository::read_all(context ctx,
                                                          const std::string& alpha2_code) {
    BOOST_LOG_SEV(lg(), debug) << "Reading all country versions. "
                               << "alpha2_code: " << alpha2_code;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<country_entity>> |
                       where("tenant_id"_c == tid && "alpha2_code"_c == alpha2_code) |
                       order_by("version"_c.desc(), "valid_from"_c.desc());

    return execute_read_query<country_entity, domain::country>(
        ctx,
        query,
        [](const auto& entities) { return country_mapper::map(entities); },
        lg(),
        "Reading all country versions by alpha2_code.");
}

std::optional<domain::country> country_repository::read_at_version(context ctx,
                                                                   const std::string& alpha2_code,
                                                                   std::uint32_t version) {
    BOOST_LOG_SEV(lg(), debug) << "Reading country at version. " << "alpha2_code: " << alpha2_code
                               << " version: " << version;
    const auto tid = ctx.tenant_id().to_string();
    const auto query =
        sqlgen::read<std::vector<country_entity>> |
        where("tenant_id"_c == tid && "alpha2_code"_c == alpha2_code && "version"_c == version) |
        sqlgen::limit(1);

    const auto entities = execute_read_query<country_entity, domain::country>(
        ctx,
        query,
        [](const auto& entities) { return country_mapper::map(entities); },
        lg(),
        "Reading country at version.");

    if (entities.empty())
        return std::nullopt;
    return entities.front();
}

country_repository::remove_status country_repository::remove(context ctx,
                                                             const std::string& alpha2_code,
                                                             std::optional<std::uint32_t> version) {
    BOOST_LOG_SEV(lg(), debug) << "Removing country. " << "alpha2_code: " << alpha2_code;
    const auto current = read_latest(ctx, alpha2_code);
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
    const auto query = sqlgen::delete_from<country_entity> |
                       where("tenant_id"_c == tid && "alpha2_code"_c == alpha2_code &&
                             "valid_to"_c == max.value() && "version"_c == expected);

    execute_delete_query(ctx, query, lg(), "Removing country from database.");
    return remove_status::removed;
}

void country_repository::remove(context ctx, const std::string& alpha2_code) {
    static_cast<void>(remove(ctx, alpha2_code, std::nullopt));
}

std::vector<domain::country>
country_repository::read_latest(context ctx, std::uint32_t offset, std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest countries with offset: " << offset
                               << " and limit: " << limit;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<country_entity>> |
                       where("tenant_id"_c == tid && "valid_to"_c == max.value()) |
                       order_by("alpha2_code"_c) | sqlgen::offset(offset) | sqlgen::limit(limit);

    return execute_read_query<country_entity, domain::country>(
        ctx,
        query,
        [](const auto& entities) { return country_mapper::map(entities); },
        lg(),
        "Reading latest countries with pagination.");
}

std::uint32_t country_repository::get_total_country_count(context ctx) {
    BOOST_LOG_SEV(lg(), debug) << "Retrieving total active country count";
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));

    struct count_result {
        long long count;
    };

    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::select_from<country_entity>(sqlgen::count().as<"count">()) |
                       where("tenant_id"_c == tid && "valid_to"_c == max.value()) |
                       sqlgen::to<count_result>;

    const auto r = sqlgen::session(ctx.connection_pool()).and_then(query);
    ensure_success(r, lg());

    const auto count = static_cast<std::uint32_t>(r->count);
    BOOST_LOG_SEV(lg(), debug) << "Total active country count: " << count;
    return count;
}

std::vector<domain::country>
country_repository::read_latest(context ctx, const std::vector<std::string>& alpha2_codes) {
    if (alpha2_codes.empty())
        return {};
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<country_entity>> |
                       where("tenant_id"_c == tid && "alpha2_code"_c.in(alpha2_codes) &&
                             "valid_to"_c == max.value());
    auto result = execute_read_query<country_entity, domain::country>(
        ctx,
        query,
        [](const auto& entities) { return country_mapper::map(entities); },
        lg(),
        "Reading latest countries by ids.");
    return result;
}

void country_repository::remove(context ctx, const std::vector<std::string>& alpha2_codes) {
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::delete_from<country_entity> |
                       where("tenant_id"_c == tid && "alpha2_code"_c.in(alpha2_codes) &&
                             "valid_to"_c == max.value());
    execute_delete_query(ctx, query, lg(), "Batch removing countries.");
}


std::vector<domain::country> country_repository::read_all(context ctx) {
    BOOST_LOG_SEV(lg(), debug) << "Reading all country versions.";
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<country_entity>> | where("tenant_id"_c == tid) |
                       order_by("valid_from"_c.desc());

    return execute_read_query<country_entity, domain::country>(
        ctx,
        query,
        [](const auto& entities) { return country_mapper::map(entities); },
        lg(),
        "Reading all countries.");
}

}
