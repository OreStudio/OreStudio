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
#include "ores.reporting.core/repository/report_configuration_repository.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.database/repository/helpers.hpp"
#include "ores.reporting.api/domain/report_configuration_json_io.hpp" // IWYU pragma: keep.
#include "ores.reporting.core/repository/report_configuration_entity.hpp"
#include "ores.reporting.core/repository/report_configuration_mapper.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <sqlgen/postgres.hpp>

namespace ores::reporting::repository {

using namespace sqlgen;
using namespace sqlgen::literals;
using namespace ores::logging;
using namespace ores::database::repository;

std::string report_configuration_repository::sql() {
    return generate_create_table_sql<report_configuration_entity>(lg());
}

ores::utility::domain::precondition
report_configuration_repository::replace_claim(context ctx, const domain::report_configuration& v) {
    const auto current = read_latest(ctx, boost::uuids::to_string(v.id));
    if (current.empty())
        return {ores::utility::domain::precondition_kind::must_not_exist, std::nullopt};
    return {ores::utility::domain::precondition_kind::must_match_version,
            static_cast<std::uint32_t>(current.front().version)};
}

domain::report_configuration
report_configuration_repository::apply_claim(context ctx,
                                             const domain::report_configuration& v,
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
            const auto current = read_latest(ctx, boost::uuids::to_string(v.id));
            t.version = current.empty() ? 0 : current.front().version;
            break;
        }
    }
    return t;
}

void report_configuration_repository::write(context ctx, const domain::report_configuration& v) {
    write(ctx, v, replace_claim(ctx, v));
}

void report_configuration_repository::write(context ctx,
                                            const std::vector<domain::report_configuration>& v) {
    std::vector<ores::utility::domain::precondition> claims;
    claims.reserve(v.size());
    for (const auto& item : v)
        claims.push_back(replace_claim(ctx, item));
    write(ctx, v, claims);
}

void report_configuration_repository::write(context ctx,
                                            const domain::report_configuration& v,
                                            const ores::utility::domain::precondition& claim) {
    BOOST_LOG_SEV(lg(), debug) << "Writing report configuration. " << "id: " << v.id;
    const auto t = apply_claim(ctx, v, claim);
    execute_write_query(ctx,
                        report_configuration_mapper::map(t),
                        lg(),
                        "Writing report configuration to database.");
}

void report_configuration_repository::write(
    context ctx,
    const std::vector<domain::report_configuration>& v,
    const std::vector<ores::utility::domain::precondition>& claims) {
    BOOST_LOG_SEV(lg(), debug) << "Writing report configurations. Count: " << v.size();
    std::vector<domain::report_configuration> batch;
    batch.reserve(v.size());
    for (std::size_t i = 0; i < v.size(); ++i)
        batch.push_back(apply_claim(ctx, v[i], claims[i]));
    execute_write_query(ctx,
                        report_configuration_mapper::map(batch),
                        lg(),
                        "Writing report configurations to database.");
}

std::vector<domain::report_configuration>
report_configuration_repository::read_latest(context ctx) {
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto& chain = ctx.workspace_resolution();
    if (!chain.empty()) {
        const auto query = sqlgen::read<std::vector<report_configuration_entity>> |
                           where("tenant_id"_c == tid && "workspace_id"_c.in(chain) &&
                                 "valid_to"_c == max.value()) |
                           order_by("id"_c);
        return execute_read_query<report_configuration_entity, domain::report_configuration>(
            ctx,
            query,
            [](const auto& entities) { return report_configuration_mapper::map(entities); },
            lg(),
            "Reading latest report configurations (workspace resolution chain).");
    }
    const auto wid = ctx.workspace_id();
    const auto query =
        sqlgen::read<std::vector<report_configuration_entity>> |
        where("tenant_id"_c == tid && "workspace_id"_c == wid && "valid_to"_c == max.value()) |
        order_by("id"_c);

    return execute_read_query<report_configuration_entity, domain::report_configuration>(
        ctx,
        query,
        [](const auto& entities) { return report_configuration_mapper::map(entities); },
        lg(),
        "Reading latest report configurations");
}

std::vector<domain::report_configuration>
report_configuration_repository::read_latest(context ctx, const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest report configuration. " << "id: " << id;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto wid = ctx.workspace_id();
    const auto query = sqlgen::read<std::vector<report_configuration_entity>> |
                       where("tenant_id"_c == tid && "workspace_id"_c == wid && "id"_c == id &&
                             "valid_to"_c == max.value());

    return execute_read_query<report_configuration_entity, domain::report_configuration>(
        ctx,
        query,
        [](const auto& entities) { return report_configuration_mapper::map(entities); },
        lg(),
        "Reading latest report configuration by id.");
}

std::vector<domain::report_configuration>
report_configuration_repository::read_latest_by_configuration_type_code(
    context ctx, const std::string& configuration_type_code) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest report configuration by configuration_type_code: "
                               << configuration_type_code;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto wid = ctx.workspace_id();
    const auto query = sqlgen::read<std::vector<report_configuration_entity>> |
                       where("tenant_id"_c == tid && "workspace_id"_c == wid &&
                             "configuration_type_code"_c == configuration_type_code &&
                             "valid_to"_c == max.value());

    return execute_read_query<report_configuration_entity, domain::report_configuration>(
        ctx,
        query,
        [](const auto& entities) { return report_configuration_mapper::map(entities); },
        lg(),
        "Reading latest report configuration by configuration_type_code.");
}

std::vector<domain::report_configuration>
report_configuration_repository::read_any_by_configuration_type_code(
    context ctx, const std::string& configuration_type_code) {
    BOOST_LOG_SEV(lg(), debug) << "Reading any report configuration by configuration_type_code: "
                               << configuration_type_code;
    const auto tid = ctx.tenant_id().to_string();
    const auto wid = ctx.workspace_id();
    const auto query = sqlgen::read<std::vector<report_configuration_entity>> |
                       where("tenant_id"_c == tid && "workspace_id"_c == wid &&
                             "configuration_type_code"_c == configuration_type_code) |
                       order_by("valid_from"_c.desc()) | sqlgen::limit(1);

    return execute_read_query<report_configuration_entity, domain::report_configuration>(
        ctx,
        query,
        [](const auto& entities) { return report_configuration_mapper::map(entities); },
        lg(),
        "Reading any report configuration by configuration_type_code.");
}


std::vector<domain::report_configuration>
report_configuration_repository::read_all(context ctx, const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading all report configuration versions. " << "id: " << id;
    const auto tid = ctx.tenant_id().to_string();
    const auto wid = ctx.workspace_id();
    const auto query = sqlgen::read<std::vector<report_configuration_entity>> |
                       where("tenant_id"_c == tid && "workspace_id"_c == wid && "id"_c == id) |
                       order_by("version"_c.desc(), "valid_from"_c.desc());

    return execute_read_query<report_configuration_entity, domain::report_configuration>(
        ctx,
        query,
        [](const auto& entities) { return report_configuration_mapper::map(entities); },
        lg(),
        "Reading all report configuration versions by id.");
}

std::optional<domain::report_configuration> report_configuration_repository::read_at_version(
    context ctx, const std::string& id, std::uint32_t version) {
    BOOST_LOG_SEV(lg(), debug) << "Reading report configuration at version. " << "id: " << id
                               << " version: " << version;
    const auto tid = ctx.tenant_id().to_string();
    const auto wid = ctx.workspace_id();
    const auto query = sqlgen::read<std::vector<report_configuration_entity>> |
                       where("tenant_id"_c == tid && "workspace_id"_c == wid && "id"_c == id &&
                             "version"_c == version) |
                       sqlgen::limit(1);

    const auto entities =
        execute_read_query<report_configuration_entity, domain::report_configuration>(
            ctx,
            query,
            [](const auto& entities) { return report_configuration_mapper::map(entities); },
            lg(),
            "Reading report configuration at version.");

    if (entities.empty())
        return std::nullopt;
    return entities.front();
}

std::vector<domain::report_configuration>
report_configuration_repository::read_latest_by_report_definition_id(
    context ctx,
    const std::string& report_definition_id,
    std::uint32_t offset,
    std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest report configurations. report_definition_id: "
                               << report_definition_id << " offset: " << offset
                               << " limit: " << limit;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto wid = ctx.workspace_id();
    const auto query =
        sqlgen::read<std::vector<report_configuration_entity>> |
        where("tenant_id"_c == tid && "workspace_id"_c == wid &&
              "report_definition_id"_c == report_definition_id && "valid_to"_c == max.value()) |
        order_by("id"_c) | sqlgen::offset(offset) | sqlgen::limit(limit);

    return execute_read_query<report_configuration_entity, domain::report_configuration>(
        ctx,
        query,
        [](const auto& entities) { return report_configuration_mapper::map(entities); },
        lg(),
        "Reading latest report configurations by report_definition_id.");
}

std::uint32_t
report_configuration_repository::get_total_report_configuration_count_by_report_definition_id(
    context ctx, const std::string& report_definition_id) {
    BOOST_LOG_SEV(lg(), debug)
        << "Retrieving total active report configurations count. report_definition_id: "
        << report_definition_id;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));

    struct count_result {
        long long count;
    };

    const auto tid = ctx.tenant_id().to_string();
    const auto wid = ctx.workspace_id();
    const auto query =
        sqlgen::select_from<report_configuration_entity>(sqlgen::count().as<"count">()) |
        where("tenant_id"_c == tid && "workspace_id"_c == wid &&
              "report_definition_id"_c == report_definition_id && "valid_to"_c == max.value()) |
        sqlgen::to<count_result>;

    const auto r = sqlgen::session(ctx.connection_pool()).and_then(query);
    ensure_success(r, lg());

    const auto count = static_cast<std::uint32_t>(r->count);
    BOOST_LOG_SEV(lg(), debug)
        << "Total active report configurations count by report_definition_id: " << count;
    return count;
}


std::vector<domain::report_configuration>
report_configuration_repository::read_latest_by_configuration_type_code(
    context ctx,
    const std::string& configuration_type_code,
    std::uint32_t offset,
    std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest report configurations. configuration_type_code: "
                               << configuration_type_code << " offset: " << offset
                               << " limit: " << limit;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto wid = ctx.workspace_id();
    const auto query = sqlgen::read<std::vector<report_configuration_entity>> |
                       where("tenant_id"_c == tid && "workspace_id"_c == wid &&
                             "configuration_type_code"_c == configuration_type_code &&
                             "valid_to"_c == max.value()) |
                       order_by("id"_c) | sqlgen::offset(offset) | sqlgen::limit(limit);

    return execute_read_query<report_configuration_entity, domain::report_configuration>(
        ctx,
        query,
        [](const auto& entities) { return report_configuration_mapper::map(entities); },
        lg(),
        "Reading latest report configurations by configuration_type_code.");
}

std::uint32_t
report_configuration_repository::get_total_report_configuration_count_by_configuration_type_code(
    context ctx, const std::string& configuration_type_code) {
    BOOST_LOG_SEV(lg(), debug)
        << "Retrieving total active report configurations count. configuration_type_code: "
        << configuration_type_code;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));

    struct count_result {
        long long count;
    };

    const auto tid = ctx.tenant_id().to_string();
    const auto wid = ctx.workspace_id();
    const auto query =
        sqlgen::select_from<report_configuration_entity>(sqlgen::count().as<"count">()) |
        where("tenant_id"_c == tid && "workspace_id"_c == wid &&
              "configuration_type_code"_c == configuration_type_code &&
              "valid_to"_c == max.value()) |
        sqlgen::to<count_result>;

    const auto r = sqlgen::session(ctx.connection_pool()).and_then(query);
    ensure_success(r, lg());

    const auto count = static_cast<std::uint32_t>(r->count);
    BOOST_LOG_SEV(lg(), debug)
        << "Total active report configurations count by configuration_type_code: " << count;
    return count;
}


std::vector<domain::report_configuration>
report_configuration_repository::read_latest_by_configuration_id(
    context ctx, const std::string& configuration_id, std::uint32_t offset, std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest report configurations. configuration_id: "
                               << configuration_id << " offset: " << offset << " limit: " << limit;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto wid = ctx.workspace_id();
    const auto query =
        sqlgen::read<std::vector<report_configuration_entity>> |
        where("tenant_id"_c == tid && "workspace_id"_c == wid &&
              "configuration_id"_c == configuration_id && "valid_to"_c == max.value()) |
        order_by("id"_c) | sqlgen::offset(offset) | sqlgen::limit(limit);

    return execute_read_query<report_configuration_entity, domain::report_configuration>(
        ctx,
        query,
        [](const auto& entities) { return report_configuration_mapper::map(entities); },
        lg(),
        "Reading latest report configurations by configuration_id.");
}

std::uint32_t
report_configuration_repository::get_total_report_configuration_count_by_configuration_id(
    context ctx, const std::string& configuration_id) {
    BOOST_LOG_SEV(lg(), debug)
        << "Retrieving total active report configurations count. configuration_id: "
        << configuration_id;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));

    struct count_result {
        long long count;
    };

    const auto tid = ctx.tenant_id().to_string();
    const auto wid = ctx.workspace_id();
    const auto query =
        sqlgen::select_from<report_configuration_entity>(sqlgen::count().as<"count">()) |
        where("tenant_id"_c == tid && "workspace_id"_c == wid &&
              "configuration_id"_c == configuration_id && "valid_to"_c == max.value()) |
        sqlgen::to<count_result>;

    const auto r = sqlgen::session(ctx.connection_pool()).and_then(query);
    ensure_success(r, lg());

    const auto count = static_cast<std::uint32_t>(r->count);
    BOOST_LOG_SEV(lg(), debug) << "Total active report configurations count by configuration_id: "
                               << count;
    return count;
}


report_configuration_repository::remove_status report_configuration_repository::remove(
    context ctx, const std::string& id, std::optional<std::uint32_t> version) {
    BOOST_LOG_SEV(lg(), debug) << "Removing report configuration. " << "id: " << id;
    const auto current = read_latest(ctx, id);
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
    const auto wid = ctx.workspace_id();
    const auto query = sqlgen::delete_from<report_configuration_entity> |
                       where("tenant_id"_c == tid && "workspace_id"_c == wid && "id"_c == id &&
                             "valid_to"_c == max.value() && "version"_c == expected);

    execute_delete_query(ctx, query, lg(), "Removing report configuration from database.");
    // The delete reports no affected-row count, so the row is read back: a row
    // still open after the statement means the store refused the removal, and
    // the caller hears "conflicting" rather than "removed".
    if (!read_latest(ctx, id).empty())
        return remove_status::conflicting;
    return remove_status::removed;
}

void report_configuration_repository::remove(context ctx, const std::string& id) {
    static_cast<void>(remove(ctx, id, std::nullopt));
}

std::vector<domain::report_configuration> report_configuration_repository::read_latest(
    context ctx, std::uint32_t offset, std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest report configurations with offset: " << offset
                               << " and limit: " << limit;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto wid = ctx.workspace_id();
    const auto query =
        sqlgen::read<std::vector<report_configuration_entity>> |
        where("tenant_id"_c == tid && "workspace_id"_c == wid && "valid_to"_c == max.value()) |
        order_by("id"_c) | sqlgen::offset(offset) | sqlgen::limit(limit);

    return execute_read_query<report_configuration_entity, domain::report_configuration>(
        ctx,
        query,
        [](const auto& entities) { return report_configuration_mapper::map(entities); },
        lg(),
        "Reading latest report configurations with pagination.");
}

std::uint32_t report_configuration_repository::get_total_report_configuration_count(context ctx) {
    BOOST_LOG_SEV(lg(), debug) << "Retrieving total active report configuration count";
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));

    struct count_result {
        long long count;
    };

    const auto tid = ctx.tenant_id().to_string();
    const auto wid = ctx.workspace_id();
    const auto query =
        sqlgen::select_from<report_configuration_entity>(sqlgen::count().as<"count">()) |
        where("tenant_id"_c == tid && "workspace_id"_c == wid && "valid_to"_c == max.value()) |
        sqlgen::to<count_result>;

    const auto r = sqlgen::session(ctx.connection_pool()).and_then(query);
    ensure_success(r, lg());

    const auto count = static_cast<std::uint32_t>(r->count);
    BOOST_LOG_SEV(lg(), debug) << "Total active report configuration count: " << count;
    return count;
}

std::vector<domain::report_configuration>
report_configuration_repository::read_latest(context ctx, const std::vector<std::string>& ids) {
    if (ids.empty())
        return {};
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto wid = ctx.workspace_id();
    const auto query = sqlgen::read<std::vector<report_configuration_entity>> |
                       where("tenant_id"_c == tid && "workspace_id"_c == wid && "id"_c.in(ids) &&
                             "valid_to"_c == max.value());
    auto result = execute_read_query<report_configuration_entity, domain::report_configuration>(
        ctx,
        query,
        [](const auto& entities) { return report_configuration_mapper::map(entities); },
        lg(),
        "Reading latest report configurations by ids.");
    return result;
}

void report_configuration_repository::remove(context ctx, const std::vector<std::string>& ids) {
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto wid = ctx.workspace_id();
    const auto query = sqlgen::delete_from<report_configuration_entity> |
                       where("tenant_id"_c == tid && "workspace_id"_c == wid && "id"_c.in(ids) &&
                             "valid_to"_c == max.value());
    execute_delete_query(ctx, query, lg(), "Batch removing report configurations.");
}


}
