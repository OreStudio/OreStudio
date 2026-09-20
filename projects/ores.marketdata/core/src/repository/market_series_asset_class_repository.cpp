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
#include "ores.marketdata.core/repository/market_series_asset_class_repository.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.database/repository/helpers.hpp"
#include "ores.marketdata.api/domain/market_series_asset_class_json_io.hpp" // IWYU pragma: keep.
#include "ores.marketdata.core/repository/market_series_asset_class_entity.hpp"
#include "ores.marketdata.core/repository/market_series_asset_class_mapper.hpp"
#include <boost/uuid/uuid_io.hpp>
#include <sqlgen/postgres.hpp>

namespace ores::marketdata::repository {

using namespace sqlgen;
using namespace sqlgen::literals;
using namespace ores::logging;
using namespace ores::database::repository;

std::string market_series_asset_class_repository::sql() {
    return generate_create_table_sql<market_series_asset_class_entity>(lg());
}

market_series_asset_class_repository::market_series_asset_class_repository(context ctx)
    : ctx_(std::move(ctx)) {}

void market_series_asset_class_repository::write(
    const domain::market_series_asset_class& asset_class) {
    BOOST_LOG_SEV(lg(), debug) << "Writing asset class to database: "
                               << asset_class.market_series_id << "/"
                               << asset_class.asset_class_code;
    execute_write_query(ctx_,
                        market_series_asset_class_mapper::map(asset_class),
                        lg(),
                        "writing asset class to database");
}

void market_series_asset_class_repository::write(
    const std::vector<domain::market_series_asset_class>& asset_classes) {
    BOOST_LOG_SEV(lg(), debug) << "Writing asset classes to database. Count: "
                               << asset_classes.size();
    execute_write_query(ctx_,
                        market_series_asset_class_mapper::map(asset_classes),
                        lg(),
                        "writing asset classes to database");
}

std::vector<domain::market_series_asset_class> market_series_asset_class_repository::read_latest() {
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx_.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<market_series_asset_class_entity>> |
                       where("tenant_id"_c == tid && "valid_to"_c == max.value()) |
                       order_by("market_series_id"_c, "asset_class_code"_c);

    return execute_read_query<market_series_asset_class_entity, domain::market_series_asset_class>(
        ctx_,
        query,
        [](const auto& entities) { return market_series_asset_class_mapper::map(entities); },
        lg(),
        "Reading latest asset classes");
}

std::vector<domain::market_series_asset_class>
market_series_asset_class_repository::read_latest(std::uint32_t offset, std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest asset classes with offset: " << offset
                               << " and limit: " << limit;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx_.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<market_series_asset_class_entity>> |
                       where("tenant_id"_c == tid && "valid_to"_c == max.value()) |
                       order_by("market_series_id"_c, "asset_class_code"_c) |
                       sqlgen::offset(offset) | sqlgen::limit(limit);

    return execute_read_query<market_series_asset_class_entity, domain::market_series_asset_class>(
        ctx_,
        query,
        [](const auto& entities) { return market_series_asset_class_mapper::map(entities); },
        lg(),
        "Reading latest asset classes (paginated).");
}

std::uint32_t market_series_asset_class_repository::get_total_asset_class_count() {
    BOOST_LOG_SEV(lg(), debug) << "Retrieving total active asset classes count";
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));

    struct count_result {
        long long count;
    };

    const auto tid = ctx_.tenant_id().to_string();
    const auto query =
        sqlgen::select_from<market_series_asset_class_entity>(sqlgen::count().as<"count">()) |
        where("tenant_id"_c == tid && "valid_to"_c == max.value()) | sqlgen::to<count_result>;

    const auto r = sqlgen::session(ctx_.connection_pool()).and_then(query);
    ensure_success(r, lg());

    const auto count = static_cast<std::uint32_t>(r->count);
    BOOST_LOG_SEV(lg(), debug) << "Total active asset classes count: " << count;
    return count;
}

std::vector<domain::market_series_asset_class>
market_series_asset_class_repository::read_latest_by_series(
    const boost::uuids::uuid& market_series_id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest asset classes. Series: " << market_series_id;

    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto market_series_id_str = boost::uuids::to_string(market_series_id);
    const auto tid = ctx_.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<market_series_asset_class_entity>> |
                       where("tenant_id"_c == tid && "market_series_id"_c == market_series_id_str &&
                             "valid_to"_c == max.value()) |
                       order_by("asset_class_code"_c);

    auto rows =
        execute_read_query<market_series_asset_class_entity, domain::market_series_asset_class>(
            ctx_,
            query,
            [](const auto& entities) { return market_series_asset_class_mapper::map(entities); },
            lg(),
            "Reading latest asset classes by series.");

    return rows;
}

std::vector<domain::market_series_asset_class>
market_series_asset_class_repository::read_latest_by_asset_class(
    const std::string& asset_class_code) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest asset classes. Asset Class: " << asset_class_code;

    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx_.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<market_series_asset_class_entity>> |
                       where("tenant_id"_c == tid && "asset_class_code"_c == asset_class_code &&
                             "valid_to"_c == max.value()) |
                       order_by("market_series_id"_c);

    auto rows =
        execute_read_query<market_series_asset_class_entity, domain::market_series_asset_class>(
            ctx_,
            query,
            [](const auto& entities) { return market_series_asset_class_mapper::map(entities); },
            lg(),
            "Reading latest asset classes by asset_class.");

    return rows;
}

std::uint32_t market_series_asset_class_repository::get_total_asset_class_count_by_series(
    const boost::uuids::uuid& market_series_id) {
    const auto market_series_id_str = boost::uuids::to_string(market_series_id);
    BOOST_LOG_SEV(lg(), debug) << "Retrieving total active asset classes count. Series: "
                               << market_series_id;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));

    struct count_result {
        long long count;
    };

    const auto tid = ctx_.tenant_id().to_string();
    const auto query =
        sqlgen::select_from<market_series_asset_class_entity>(sqlgen::count().as<"count">()) |
        where("tenant_id"_c == tid && "market_series_id"_c == market_series_id_str &&
              "valid_to"_c == max.value()) |
        sqlgen::to<count_result>;

    const auto r = sqlgen::session(ctx_.connection_pool()).and_then(query);
    ensure_success(r, lg());

    const auto count = static_cast<std::uint32_t>(r->count);
    BOOST_LOG_SEV(lg(), debug) << "Total active asset classes count by series: " << count;
    return count;
}

std::uint32_t market_series_asset_class_repository::get_total_asset_class_count_by_asset_class(
    const std::string& asset_class_code) {
    BOOST_LOG_SEV(lg(), debug) << "Retrieving total active asset classes count. Asset Class: "
                               << asset_class_code;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));

    struct count_result {
        long long count;
    };

    const auto tid = ctx_.tenant_id().to_string();
    const auto query =
        sqlgen::select_from<market_series_asset_class_entity>(sqlgen::count().as<"count">()) |
        where("tenant_id"_c == tid && "asset_class_code"_c == asset_class_code &&
              "valid_to"_c == max.value()) |
        sqlgen::to<count_result>;

    const auto r = sqlgen::session(ctx_.connection_pool()).and_then(query);
    ensure_success(r, lg());

    const auto count = static_cast<std::uint32_t>(r->count);
    BOOST_LOG_SEV(lg(), debug) << "Total active asset classes count by asset_class: " << count;
    return count;
}

void market_series_asset_class_repository::remove(const boost::uuids::uuid& market_series_id,
                                                  const std::string& asset_class_code) {
    BOOST_LOG_SEV(lg(), debug) << "Removing asset class from database: " << market_series_id << "/"
                               << asset_class_code;

    const auto market_series_id_str = boost::uuids::to_string(market_series_id);
    const auto tid = ctx_.tenant_id().to_string();
    const auto query = sqlgen::delete_from<market_series_asset_class_entity> |
                       where("tenant_id"_c == tid && "market_series_id"_c == market_series_id_str &&
                             "asset_class_code"_c == asset_class_code);

    execute_delete_query(ctx_, query, lg(), "removing asset class from database");
}

void market_series_asset_class_repository::remove_by_series(
    const boost::uuids::uuid& market_series_id) {
    BOOST_LOG_SEV(lg(), debug) << "Removing all asset classes from database: " << market_series_id;

    const auto market_series_id_str = boost::uuids::to_string(market_series_id);
    const auto tid = ctx_.tenant_id().to_string();
    const auto query = sqlgen::delete_from<market_series_asset_class_entity> |
                       where("tenant_id"_c == tid && "market_series_id"_c == market_series_id_str);

    execute_delete_query(ctx_, query, lg(), "removing all asset classes from database");
}

}
