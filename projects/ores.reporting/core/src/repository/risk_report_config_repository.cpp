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
#include "ores.reporting.core/repository/risk_report_config_repository.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.database/repository/helpers.hpp"
#include "ores.reporting.api/domain/risk_report_config_json_io.hpp" // IWYU pragma: keep.
#include "ores.reporting.core/repository/risk_report_config_entity.hpp"
#include "ores.reporting.core/repository/risk_report_config_mapper.hpp"
#include <sqlgen/postgres.hpp>

namespace ores::reporting::repository {

using namespace sqlgen;
using namespace sqlgen::literals;
using namespace ores::logging;
using namespace ores::database::repository;

std::string risk_report_config_repository::sql() {
    return generate_create_table_sql<risk_report_config_entity>(lg());
}

void risk_report_config_repository::write(context ctx, const domain::risk_report_config& v) {
    BOOST_LOG_SEV(lg(), debug) << "Writing risk report config. " << "id: " << v.id;
    execute_write_query(
        ctx, risk_report_config_mapper::map(v), lg(), "Writing risk report config to database.");
}

void risk_report_config_repository::write(context ctx,
                                          const std::vector<domain::risk_report_config>& v) {
    BOOST_LOG_SEV(lg(), debug) << "Writing risk report configs. Count: " << v.size();
    execute_write_query(
        ctx, risk_report_config_mapper::map(v), lg(), "Writing risk report configs to database.");
}

std::vector<domain::risk_report_config> risk_report_config_repository::read_latest(context ctx) {
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<risk_report_config_entity>> |
                       where("tenant_id"_c == tid && "valid_to"_c == max.value()) |
                       order_by("id"_c);

    return execute_read_query<risk_report_config_entity, domain::risk_report_config>(
        ctx,
        query,
        [](const auto& entities) { return risk_report_config_mapper::map(entities); },
        lg(),
        "Reading latest risk report configs");
}

std::vector<domain::risk_report_config>
risk_report_config_repository::read_latest(context ctx, const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest risk report config. " << "id: " << id;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<risk_report_config_entity>> |
                       where("tenant_id"_c == tid && "id"_c == id && "valid_to"_c == max.value());

    return execute_read_query<risk_report_config_entity, domain::risk_report_config>(
        ctx,
        query,
        [](const auto& entities) { return risk_report_config_mapper::map(entities); },
        lg(),
        "Reading latest risk report config by id.");
}


std::vector<domain::risk_report_config>
risk_report_config_repository::read_all(context ctx, const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading all risk report config versions. " << "id: " << id;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<risk_report_config_entity>> |
                       where("tenant_id"_c == tid && "id"_c == id) |
                       order_by("version"_c.desc(), "valid_from"_c.desc());

    return execute_read_query<risk_report_config_entity, domain::risk_report_config>(
        ctx,
        query,
        [](const auto& entities) { return risk_report_config_mapper::map(entities); },
        lg(),
        "Reading all risk report config versions by id.");
}

std::optional<domain::risk_report_config> risk_report_config_repository::read_at_version(
    context ctx, const std::string& id, std::uint32_t version) {
    BOOST_LOG_SEV(lg(), debug) << "Reading risk report config at version. " << "id: " << id
                               << " version: " << version;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<risk_report_config_entity>> |
                       where("tenant_id"_c == tid && "id"_c == id && "version"_c == version) |
                       sqlgen::limit(1);

    const auto entities = execute_read_query<risk_report_config_entity, domain::risk_report_config>(
        ctx,
        query,
        [](const auto& entities) { return risk_report_config_mapper::map(entities); },
        lg(),
        "Reading risk report config at version.");

    if (entities.empty())
        return std::nullopt;
    return entities.front();
}

std::vector<domain::risk_report_config>
risk_report_config_repository::read_latest_by_report_definition_id(
    context ctx,
    const std::string& report_definition_id,
    std::uint32_t offset,
    std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest risk report configs. report_definition_id: "
                               << report_definition_id << " offset: " << offset
                               << " limit: " << limit;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query =
        sqlgen::read<std::vector<risk_report_config_entity>> |
        where("tenant_id"_c == tid && "report_definition_id"_c == report_definition_id &&
              "valid_to"_c == max.value()) |
        order_by("id"_c) | sqlgen::offset(offset) | sqlgen::limit(limit);

    return execute_read_query<risk_report_config_entity, domain::risk_report_config>(
        ctx,
        query,
        [](const auto& entities) { return risk_report_config_mapper::map(entities); },
        lg(),
        "Reading latest risk report configs by report_definition_id.");
}

std::uint32_t risk_report_config_repository::get_total_config_count_by_report_definition_id(
    context ctx, const std::string& report_definition_id) {
    BOOST_LOG_SEV(lg(), debug)
        << "Retrieving total active risk report configs count. report_definition_id: "
        << report_definition_id;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));

    struct count_result {
        long long count;
    };

    const auto tid = ctx.tenant_id().to_string();
    const auto query =
        sqlgen::select_from<risk_report_config_entity>(sqlgen::count().as<"count">()) |
        where("tenant_id"_c == tid && "report_definition_id"_c == report_definition_id &&
              "valid_to"_c == max.value()) |
        sqlgen::to<count_result>;

    const auto r = sqlgen::session(ctx.connection_pool()).and_then(query);
    ensure_success(r, lg());

    const auto count = static_cast<std::uint32_t>(r->count);
    BOOST_LOG_SEV(lg(), debug) << "Total active risk report configs count by report_definition_id: "
                               << count;
    return count;
}

void risk_report_config_repository::remove(context ctx, const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Removing risk report config. " << "id: " << id;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::delete_from<risk_report_config_entity> |
                       where("tenant_id"_c == tid && "id"_c == id && "valid_to"_c == max.value());

    execute_delete_query(ctx, query, lg(), "Removing risk report config from database.");
}

std::vector<domain::risk_report_config>
risk_report_config_repository::read_latest(context ctx, std::uint32_t offset, std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest risk report configs with offset: " << offset
                               << " and limit: " << limit;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<risk_report_config_entity>> |
                       where("tenant_id"_c == tid && "valid_to"_c == max.value()) |
                       order_by("id"_c) | sqlgen::offset(offset) | sqlgen::limit(limit);

    return execute_read_query<risk_report_config_entity, domain::risk_report_config>(
        ctx,
        query,
        [](const auto& entities) { return risk_report_config_mapper::map(entities); },
        lg(),
        "Reading latest risk report configs with pagination.");
}

std::uint32_t risk_report_config_repository::get_total_config_count(context ctx) {
    BOOST_LOG_SEV(lg(), debug) << "Retrieving total active risk report config count";
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));

    struct count_result {
        long long count;
    };

    const auto tid = ctx.tenant_id().to_string();
    const auto query =
        sqlgen::select_from<risk_report_config_entity>(sqlgen::count().as<"count">()) |
        where("tenant_id"_c == tid && "valid_to"_c == max.value()) | sqlgen::to<count_result>;

    const auto r = sqlgen::session(ctx.connection_pool()).and_then(query);
    ensure_success(r, lg());

    const auto count = static_cast<std::uint32_t>(r->count);
    BOOST_LOG_SEV(lg(), debug) << "Total active risk report config count: " << count;
    return count;
}

void risk_report_config_repository::remove(context ctx, const std::vector<std::string>& ids) {
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::delete_from<risk_report_config_entity> |
                       where("tenant_id"_c == tid && "id"_c.in(ids) && "valid_to"_c == max.value());
    execute_delete_query(ctx, query, lg(), "Batch removing risk report configs.");
}


namespace {

struct book_scope_entity {
    constexpr static const char* schema = "public";
    constexpr static const char* tablename = "ores_reporting_risk_report_config_books_tbl";

    std::string tenant_id;
    std::string risk_report_config_id;
    std::string book_id;
    std::optional<db_timestamp> valid_from = "9999-12-31 23:59:59";
    std::optional<db_timestamp> valid_to = "9999-12-31 23:59:59";
};

struct portfolio_scope_entity {
    constexpr static const char* schema = "public";
    constexpr static const char* tablename = "ores_reporting_risk_report_config_portfolios_tbl";

    std::string tenant_id;
    std::string risk_report_config_id;
    std::string portfolio_id;
    std::optional<db_timestamp> valid_from = "9999-12-31 23:59:59";
    std::optional<db_timestamp> valid_to = "9999-12-31 23:59:59";
};

}

std::optional<domain::risk_report_config>
risk_report_config_repository::find_by_definition_id(context ctx,
                                                     const std::string& definition_id) {

    BOOST_LOG_SEV(lg(), debug) << "Finding risk_report_config by definition_id: " << definition_id;

    static auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<risk_report_config_entity>> |
                       where("tenant_id"_c == tid && "report_definition_id"_c == definition_id &&
                             "valid_to"_c == max.value());

    auto results = execute_read_query<risk_report_config_entity, domain::risk_report_config>(
        ctx,
        query,
        [](const auto& entities) { return risk_report_config_mapper::map(entities); },
        lg(),
        "Finding risk_report_config by definition_id");

    if (results.empty())
        return std::nullopt;
    return results.front();
}

std::vector<std::string>
risk_report_config_repository::resolve_book_ids(context ctx, const std::string& config_id) {

    BOOST_LOG_SEV(lg(), debug) << "Resolving book IDs for config: " << config_id;

    const auto tid = ctx.tenant_id().to_string();
    const std::string sql =
        "SELECT id::text FROM "
        "ores_reporting_resolve_book_ids_for_config_fn($1::uuid, $2::uuid) AS t(id)";

    return execute_parameterized_string_query(
        ctx, sql, {tid, config_id}, lg(), "Resolving book IDs for risk_report_config");
}

std::vector<std::string>
risk_report_config_repository::get_book_scope(context ctx, const std::string& config_id) {

    BOOST_LOG_SEV(lg(), debug) << "Reading book scope for config: " << config_id;

    static auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<book_scope_entity>> |
                       where("tenant_id"_c == tid && "risk_report_config_id"_c == config_id &&
                             "valid_to"_c == max.value());

    auto rows = execute_read_query<book_scope_entity, book_scope_entity>(
        ctx, query, [](const auto& entities) { return entities; }, lg(), "Reading book scope");

    std::vector<std::string> book_ids;
    book_ids.reserve(rows.size());
    for (const auto& row : rows)
        book_ids.push_back(row.book_id);

    BOOST_LOG_SEV(lg(), debug) << "Found " << book_ids.size() << " book(s) in scope";
    return book_ids;
}

std::vector<std::string>
risk_report_config_repository::get_portfolio_scope(context ctx, const std::string& config_id) {

    BOOST_LOG_SEV(lg(), debug) << "Reading portfolio scope for config: " << config_id;

    static auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<portfolio_scope_entity>> |
                       where("tenant_id"_c == tid && "risk_report_config_id"_c == config_id &&
                             "valid_to"_c == max.value());

    auto rows = execute_read_query<portfolio_scope_entity, portfolio_scope_entity>(
        ctx, query, [](const auto& entities) { return entities; }, lg(), "Reading portfolio scope");

    std::vector<std::string> portfolio_ids;
    portfolio_ids.reserve(rows.size());
    for (const auto& row : rows)
        portfolio_ids.push_back(row.portfolio_id);

    BOOST_LOG_SEV(lg(), debug) << "Found " << portfolio_ids.size() << " portfolio(s) in scope";
    return portfolio_ids;
}

}
