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
#include "ores.trading/repository/trade_repository.hpp"

#include <sqlgen/postgres.hpp>
#include <boost/lexical_cast.hpp>
#include <boost/uuid/uuid_io.hpp>
#include "ores.database/repository/helpers.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.database/repository/mapper_helpers.hpp"
#include "ores.trading/domain/trade_json_io.hpp" // IWYU pragma: keep.
#include "ores.trading/repository/trade_entity.hpp"
#include "ores.trading/repository/trade_mapper.hpp"

namespace ores::trading::repository {

using namespace sqlgen;
using namespace sqlgen::literals;
using namespace ores::logging;
using namespace ores::database::repository;

namespace {

domain::trade raw_row_to_trade(
    const std::vector<std::optional<std::string>>& row) {
    // Column order matches explicit SELECT in read_latest_filtered CTE:
    // 0:id, 1:tenant_id, 2:version, 3:party_id, 4:external_id, 5:book_id,
    // 6:portfolio_id, 7:successor_trade_id, 8:counterparty_id, 9:trade_type,
    // 10:netting_set_id, 11:lifecycle_event, 12:trade_date, 13:execution_timestamp,
    // 14:effective_date, 15:termination_date, 16:modified_by, 17:performed_by,
    // 18:change_reason_code, 19:change_commentary, 20:valid_from
    domain::trade r;
    r.id = boost::lexical_cast<boost::uuids::uuid>(row[0].value());
    r.tenant_id = utility::uuid::tenant_id::from_string(row[1].value()).value();
    r.version = std::stoi(row[2].value());
    r.party_id = boost::lexical_cast<boost::uuids::uuid>(row[3].value());
    r.external_id = row[4].value_or("");
    r.book_id = boost::lexical_cast<boost::uuids::uuid>(row[5].value());
    r.portfolio_id = boost::lexical_cast<boost::uuids::uuid>(row[6].value());
    if (row[7].has_value())
        r.successor_trade_id = boost::lexical_cast<boost::uuids::uuid>(*row[7]);
    if (row[8].has_value())
        r.counterparty_id = boost::lexical_cast<boost::uuids::uuid>(*row[8]);
    r.trade_type = row[9].value();
    r.netting_set_id = row[10].value();
    r.lifecycle_event = row[11].value();
    r.trade_date = row[12].value();
    r.execution_timestamp = row[13].value();
    r.effective_date = row[14].value();
    r.termination_date = row[15].value();
    r.modified_by = row[16].value();
    r.performed_by = row[17].value();
    r.change_reason_code = row[18].value();
    r.change_commentary = row[19].value();
    r.recorded_at = timestamp_to_timepoint(std::string_view{row[20].value()});
    return r;
}

} // anonymous namespace

std::string trade_repository::sql() {
    return generate_create_table_sql<trade_entity>(lg());
}

void trade_repository::write(context ctx, const domain::trade& v) {
    BOOST_LOG_SEV(lg(), debug) << "Writing trade: " << v.id;
    execute_write_query(ctx, trade_mapper::map(v),
        lg(), "Writing trade to database.");
}

void trade_repository::write(
    context ctx, const std::vector<domain::trade>& v) {
    BOOST_LOG_SEV(lg(), debug) << "Writing trades. Count: " << v.size();
    execute_write_query(ctx, trade_mapper::map(v),
        lg(), "Writing trades to database.");
}

std::vector<domain::trade>
trade_repository::read_latest(context ctx) {
    static auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<trade_entity>> |
        where("tenant_id"_c == tid && "valid_to"_c == max.value()) |
        order_by("id"_c);

    return execute_read_query<trade_entity, domain::trade>(
        ctx, query,
        [](const auto& entities) { return trade_mapper::map(entities); },
        lg(), "Reading latest trades");
}

std::vector<domain::trade>
trade_repository::read_latest(context ctx, std::uint32_t offset,
    std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest trades with offset: "
                               << offset << " and limit: " << limit;
    static auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<trade_entity>> |
        where("tenant_id"_c == tid && "valid_to"_c == max.value()) |
        order_by("id"_c) |
        sqlgen::offset(offset) |
        sqlgen::limit(limit);

    return execute_read_query<trade_entity, domain::trade>(
        ctx, query,
        [](const auto& entities) { return trade_mapper::map(entities); },
        lg(), "Reading latest trades with pagination");
}

std::uint32_t trade_repository::count_latest(context ctx) {
    BOOST_LOG_SEV(lg(), debug) << "Counting latest trades";
    static auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();

    struct count_result {
        long long count;
    };

    const auto query = sqlgen::select_from<trade_entity>(
        sqlgen::count().as<"count">()) |
        where("tenant_id"_c == tid && "valid_to"_c == max.value()) |
        sqlgen::to<count_result>;

    const auto r = sqlgen::session(ctx.connection_pool()).and_then(query);
    ensure_success(r, lg());

    const auto count = static_cast<std::uint32_t>(r->count);
    BOOST_LOG_SEV(lg(), debug) << "Total active trade count: " << count;
    return count;
}

std::vector<domain::trade>
trade_repository::read_latest(context ctx, const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest trade. id: " << id;
    static auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<trade_entity>> |
        where("tenant_id"_c == tid && "id"_c == id && "valid_to"_c == max.value());

    return execute_read_query<trade_entity, domain::trade>(
        ctx, query,
        [](const auto& entities) { return trade_mapper::map(entities); },
        lg(), "Reading latest trade by id.");
}

std::vector<domain::trade>
trade_repository::read_all(context ctx, const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading all trade versions. id: " << id;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<trade_entity>> |
        where("tenant_id"_c == tid && "id"_c == id) |
        order_by("version"_c.desc());

    return execute_read_query<trade_entity, domain::trade>(
        ctx, query,
        [](const auto& entities) { return trade_mapper::map(entities); },
        lg(), "Reading all trade versions by id.");
}

std::vector<domain::trade>
trade_repository::read_latest_filtered(context ctx,
    std::uint32_t offset, std::uint32_t limit,
    std::optional<boost::uuids::uuid> book_id,
    std::optional<boost::uuids::uuid> portfolio_id) {

    static auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();

    if (book_id.has_value()) {
        BOOST_LOG_SEV(lg(), debug)
            << "Reading trades filtered by book: " << *book_id;
        const auto bid = boost::uuids::to_string(*book_id);
        const auto query = sqlgen::read<std::vector<trade_entity>> |
            where("tenant_id"_c == tid && "valid_to"_c == max.value()
                  && "book_id"_c == bid) |
            order_by("id"_c) |
            sqlgen::offset(offset) |
            sqlgen::limit(limit);

        return execute_read_query<trade_entity, domain::trade>(
            ctx, query,
            [](const auto& entities) { return trade_mapper::map(entities); },
            lg(), "Reading trades filtered by book_id");
    }

    // portfolio_id case: delegate to SQL function with recursive CTE
    const auto pid = boost::uuids::to_string(*portfolio_id);
    BOOST_LOG_SEV(lg(), debug)
        << "Reading trades filtered by portfolio subtree: " << pid;

    const std::string sql =
        "SELECT * FROM ores_trading_read_trades_by_portfolio_fn('"
        + tid + "'::uuid, '" + pid + "'::uuid, "
        + std::to_string(offset) + ", " + std::to_string(limit) + ")";

    const auto rows = execute_raw_multi_column_query(
        ctx, sql, lg(), "Reading trades for portfolio subtree");

    std::vector<domain::trade> result;
    result.reserve(rows.size());
    for (const auto& row : rows) {
        if (row.size() >= 21)
            result.push_back(raw_row_to_trade(row));
    }
    return result;
}

std::uint32_t
trade_repository::count_latest_filtered(context ctx,
    std::optional<boost::uuids::uuid> book_id,
    std::optional<boost::uuids::uuid> portfolio_id) {

    static auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();

    if (book_id.has_value()) {
        BOOST_LOG_SEV(lg(), debug)
            << "Counting trades filtered by book: " << *book_id;
        const auto bid = boost::uuids::to_string(*book_id);

        struct count_result { long long count; };
        const auto query = sqlgen::select_from<trade_entity>(
            sqlgen::count().as<"count">()) |
            where("tenant_id"_c == tid && "valid_to"_c == max.value()
                  && "book_id"_c == bid) |
            sqlgen::to<count_result>;

        const auto r = sqlgen::session(ctx.connection_pool()).and_then(query);
        ensure_success(r, lg());
        return static_cast<std::uint32_t>(r->count);
    }

    // portfolio_id case: delegate to SQL function with recursive CTE
    const auto pid = boost::uuids::to_string(*portfolio_id);
    BOOST_LOG_SEV(lg(), debug)
        << "Counting trades for portfolio subtree: " << pid;

    const std::string sql =
        "SELECT ores_trading_count_trades_by_portfolio_fn('"
        + tid + "'::uuid, '" + pid + "'::uuid)";

    const auto rows = execute_raw_multi_column_query(
        ctx, sql, lg(), "Counting trades for portfolio subtree");

    if (rows.empty() || rows[0].empty() || !rows[0][0].has_value())
        return 0;
    return static_cast<std::uint32_t>(std::stoll(*rows[0][0]));
}

void trade_repository::remove(context ctx, const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Removing trade: " << id;
    static auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::delete_from<trade_entity> |
        where("tenant_id"_c == tid && "id"_c == id && "valid_to"_c == max.value());

    execute_delete_query(ctx, query, lg(), "Removing trade from database.");
}

}
