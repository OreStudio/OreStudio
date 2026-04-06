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
#include "ores.reporting.core/repository/risk_report_config_repository.hpp"

#include <sqlgen/postgres.hpp>
#include "ores.database/repository/helpers.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.reporting.core/repository/risk_report_config_entity.hpp"
#include "ores.reporting.core/repository/risk_report_config_mapper.hpp"

namespace ores::reporting::repository {

using namespace sqlgen;
using namespace sqlgen::literals;
using namespace ores::logging;
using namespace ores::database::repository;

namespace {

// Lightweight entities for the scope junction tables. Only the columns
// we need for scope resolution are included.
struct book_scope_entity {
    constexpr static const char* schema = "public";
    constexpr static const char* tablename =
        "ores_reporting_risk_report_config_books_tbl";

    std::string tenant_id;
    std::string risk_report_config_id;
    std::string book_id;
    std::optional<sqlgen::Timestamp<"%Y-%m-%d %H:%M:%S">> valid_from =
        "9999-12-31 23:59:59";
    std::optional<sqlgen::Timestamp<"%Y-%m-%d %H:%M:%S">> valid_to =
        "9999-12-31 23:59:59";
};

struct portfolio_scope_entity {
    constexpr static const char* schema = "public";
    constexpr static const char* tablename =
        "ores_reporting_risk_report_config_portfolios_tbl";

    std::string tenant_id;
    std::string risk_report_config_id;
    std::string portfolio_id;
    std::optional<sqlgen::Timestamp<"%Y-%m-%d %H:%M:%S">> valid_from =
        "9999-12-31 23:59:59";
    std::optional<sqlgen::Timestamp<"%Y-%m-%d %H:%M:%S">> valid_to =
        "9999-12-31 23:59:59";
};

} // namespace

std::optional<domain::risk_report_config>
risk_report_config_repository::find_by_definition_id(
    context ctx, const std::string& definition_id) {

    BOOST_LOG_SEV(lg(), debug)
        << "Finding risk_report_config by definition_id: " << definition_id;

    static auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<risk_report_config_entity>> |
        where("tenant_id"_c == tid &&
              "report_definition_id"_c == definition_id &&
              "valid_to"_c == max.value());

    auto results = execute_read_query<
        risk_report_config_entity, domain::risk_report_config>(
        ctx, query,
        [](const auto& entities) {
            return risk_report_config_mapper::map(entities);
        },
        lg(), "Finding risk_report_config by definition_id");

    if (results.empty()) return std::nullopt;
    return results.front();
}

std::vector<std::string>
risk_report_config_repository::get_book_scope(
    context ctx, const std::string& config_id) {

    BOOST_LOG_SEV(lg(), debug)
        << "Reading book scope for config: " << config_id;

    static auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<book_scope_entity>> |
        where("tenant_id"_c == tid &&
              "risk_report_config_id"_c == config_id &&
              "valid_to"_c == max.value());

    auto rows = execute_read_query<book_scope_entity, book_scope_entity>(
        ctx, query,
        [](const auto& entities) { return entities; },
        lg(), "Reading book scope");

    std::vector<std::string> book_ids;
    book_ids.reserve(rows.size());
    for (const auto& row : rows)
        book_ids.push_back(row.book_id);

    BOOST_LOG_SEV(lg(), debug)
        << "Found " << book_ids.size() << " book(s) in scope";
    return book_ids;
}

std::vector<std::string>
risk_report_config_repository::get_portfolio_scope(
    context ctx, const std::string& config_id) {

    BOOST_LOG_SEV(lg(), debug)
        << "Reading portfolio scope for config: " << config_id;

    static auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<portfolio_scope_entity>> |
        where("tenant_id"_c == tid &&
              "risk_report_config_id"_c == config_id &&
              "valid_to"_c == max.value());

    auto rows = execute_read_query<
        portfolio_scope_entity, portfolio_scope_entity>(
        ctx, query,
        [](const auto& entities) { return entities; },
        lg(), "Reading portfolio scope");

    std::vector<std::string> portfolio_ids;
    portfolio_ids.reserve(rows.size());
    for (const auto& row : rows)
        portfolio_ids.push_back(row.portfolio_id);

    BOOST_LOG_SEV(lg(), debug)
        << "Found " << portfolio_ids.size() << " portfolio(s) in scope";
    return portfolio_ids;
}

}
