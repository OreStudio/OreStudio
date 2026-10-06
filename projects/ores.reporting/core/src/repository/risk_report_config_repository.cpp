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
#include "ores.database/repository/stated_order.hpp"
#include "ores.database/repository/valid_at.hpp"
#include "ores.logging/boost_severity.hpp"
#include "ores.reporting.api/domain/risk_report_config.hpp"
#include "ores.reporting.api/domain/risk_report_config_json_io.hpp" // IWYU pragma: keep.
#include "ores.reporting.core/repository/risk_report_config_entity.hpp"
#include "ores.reporting.core/repository/risk_report_config_mapper.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <boost/log/sources/severity_feature.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <algorithm>
#include <cstddef>
#include <cstdint>
#include <initializer_list>
#include <optional>
#include <sqlgen/delete_from.hpp>
#include <sqlgen/dynamic/OrderBy.hpp>
#include <sqlgen/limit.hpp>
#include <sqlgen/literals.hpp>
#include <sqlgen/offset.hpp>
#include <sqlgen/order_by.hpp>
#include <sqlgen/read.hpp>
#include <sqlgen/where.hpp>
#include <stdexcept>
#include <string>
#include <string_view>
#include <vector>

namespace ores::reporting::repository {

using namespace sqlgen;
using namespace sqlgen::literals;
using namespace ores::logging;
using namespace ores::database::repository;

std::string risk_report_config_repository::sql() {
    return generate_create_table_sql<risk_report_config_entity>(lg());
}

bool risk_report_config_repository::is_sortable(std::string_view field) {
    const std::initializer_list<std::string_view> sortable = {};
    return std::ranges::find(sortable, field) != sortable.end();
}

namespace {

/*
 * The order a page is read in. An empty field is the default order, which
 * the stated direction reverses; any other field must be sortable, because
 * the service refuses the rest before it reaches the store.
 */
sqlgen::dynamic::OrderBy list_order(const ores::utility::domain::order& order,
                                    std::initializer_list<std::string> default_columns,
                                    bool default_descending) {
    if (order.field.empty())
        return make_order(default_columns, default_descending != order.descending, {"id"});
    if (!risk_report_config_repository::is_sortable(order.field))
        throw std::invalid_argument("A list of risk report configs cannot be ordered by " +
                                    order.field + ".");
    return make_order({order.field}, order.descending, {"id"});
}

}

ores::utility::domain::precondition
risk_report_config_repository::replace_claim(context ctx, const domain::risk_report_config& v) {
    const auto current = read_latest(ctx, boost::uuids::to_string(v.id));
    if (current.empty())
        return {ores::utility::domain::precondition_kind::must_not_exist, std::nullopt};
    return {ores::utility::domain::precondition_kind::must_match_version,
            static_cast<std::uint32_t>(current.front().version)};
}

domain::risk_report_config
risk_report_config_repository::apply_claim(context ctx,
                                           const domain::risk_report_config& v,
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

void risk_report_config_repository::write(context ctx, const domain::risk_report_config& v) {
    write(ctx, v, replace_claim(ctx, v));
}

void risk_report_config_repository::write(context ctx,
                                          const std::vector<domain::risk_report_config>& v) {
    std::vector<ores::utility::domain::precondition> claims;
    claims.reserve(v.size());
    for (const auto& item : v)
        claims.push_back(replace_claim(ctx, item));
    write(ctx, v, claims);
}

void risk_report_config_repository::write(context ctx,
                                          const domain::risk_report_config& v,
                                          const ores::utility::domain::precondition& claim) {
    BOOST_LOG_SEV(lg(), debug) << "Writing risk report config. " << "id: " << v.id;
    const auto t = apply_claim(ctx, v, claim);
    execute_write_query(
        ctx, risk_report_config_mapper::map(t), lg(), "Writing risk report config to database.");
}

void risk_report_config_repository::write(
    context ctx,
    const std::vector<domain::risk_report_config>& v,
    const std::vector<ores::utility::domain::precondition>& claims) {
    BOOST_LOG_SEV(lg(), debug) << "Writing risk report configs. Count: " << v.size();
    std::vector<domain::risk_report_config> batch;
    batch.reserve(v.size());
    for (std::size_t i = 0; i < v.size(); ++i)
        batch.push_back(apply_claim(ctx, v[i], claims[i]));
    execute_write_query(ctx,
                        risk_report_config_mapper::map(batch),
                        lg(),
                        "Writing risk report configs to database.");
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
    std::uint32_t limit,
    const ores::utility::domain::order& order) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest risk report configs. report_definition_id: "
                               << report_definition_id << " offset: " << offset
                               << " limit: " << limit;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query =
        sqlgen::read<std::vector<risk_report_config_entity>> |
        where("tenant_id"_c == tid && "report_definition_id"_c == report_definition_id &&
              "valid_to"_c == max.value()) |
        sqlgen::offset(offset) | sqlgen::limit(limit);

    return execute_ordered_read_query<risk_report_config_entity, domain::risk_report_config>(
        ctx,
        query,
        list_order(order, {"id"}, false),
        std::nullopt,
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

    const auto tid = ctx.tenant_id().to_string();
    const auto query =
        sqlgen::read<std::vector<risk_report_config_entity>> |
        where("tenant_id"_c == tid && "report_definition_id"_c == report_definition_id &&
              "valid_to"_c == max.value());

    return execute_count_query<risk_report_config_entity>(
        ctx, query, std::nullopt, lg(), "Counting risk report configs by report_definition_id");
}


risk_report_config_repository::remove_status risk_report_config_repository::remove(
    context ctx, const std::string& id, std::optional<std::uint32_t> version) {
    BOOST_LOG_SEV(lg(), debug) << "Removing risk report config. " << "id: " << id;
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
    const auto query = sqlgen::delete_from<risk_report_config_entity> |
                       where("tenant_id"_c == tid && "id"_c == id && "valid_to"_c == max.value() &&
                             "version"_c == expected);

    execute_delete_query(ctx, query, lg(), "Removing risk report config from database.");
    // The delete reports no affected-row count, so the row is read back: a row
    // still open after the statement means the store refused the removal, and
    // the caller hears "conflicting" rather than "removed".
    if (!read_latest(ctx, id).empty())
        return remove_status::conflicting;
    return remove_status::removed;
}

void risk_report_config_repository::remove(context ctx, const std::string& id) {
    static_cast<void>(remove(ctx, id, std::nullopt));
}

std::vector<domain::risk_report_config>
risk_report_config_repository::read_latest(context ctx,
                                           std::uint32_t offset,
                                           std::uint32_t limit,
                                           const ores::utility::domain::order& order,
                                           const std::optional<std::string>& as_of) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest risk report configs with offset: " << offset
                               << " and limit: " << limit;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<risk_report_config_entity>> |
                       where("tenant_id"_c == tid) | sqlgen::offset(offset) | sqlgen::limit(limit);

    return execute_ordered_read_query<risk_report_config_entity, domain::risk_report_config>(
        ctx,
        query,
        list_order(order, {"id"}, false),
        narrowed(valid_at(as_of), std::nullopt),
        [](const auto& entities) { return risk_report_config_mapper::map(entities); },
        lg(),
        "Reading latest risk report configs with pagination.");
}

std::uint32_t
risk_report_config_repository::get_total_config_count(context ctx,
                                                      const std::optional<std::string>& as_of) {
    BOOST_LOG_SEV(lg(), debug) << "Retrieving total active risk report config count";

    const auto tid = ctx.tenant_id().to_string();
    const auto query =
        sqlgen::read<std::vector<risk_report_config_entity>> | where("tenant_id"_c == tid);

    return execute_count_query<risk_report_config_entity>(
        ctx, query, narrowed(valid_at(as_of), std::nullopt), lg(), "Counting risk report configs");
}

std::vector<domain::risk_report_config>
risk_report_config_repository::read_latest(context ctx, const std::vector<std::string>& ids) {
    if (ids.empty())
        return {};
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<risk_report_config_entity>> |
                       where("tenant_id"_c == tid && "id"_c.in(ids) && "valid_to"_c == max.value());
    auto result = execute_read_query<risk_report_config_entity, domain::risk_report_config>(
        ctx,
        query,
        [](const auto& entities) { return risk_report_config_mapper::map(entities); },
        lg(),
        "Reading latest risk report configs by ids.");
    return result;
}

void risk_report_config_repository::remove(context ctx, const std::vector<std::string>& ids) {
    // A batch of nothing addresses no row, so there is nothing to delete. The
    // query builder renders an empty key list as an empty IN (), which the
    // server refuses as a syntax error; the read overloads answer the empty
    // case the same way. The compound branch above is left alone: it loops, so
    // it already removes nothing, and its length check still refuses an
    // asymmetric pair.
    if (ids.empty())
        return;
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
