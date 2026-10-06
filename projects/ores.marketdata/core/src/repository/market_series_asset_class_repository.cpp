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
#include "ores.database/domain/tenant_aware_pool.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.database/repository/helpers.hpp"
#include "ores.logging/boost_severity.hpp"
#include "ores.marketdata.api/domain/market_series_asset_class.hpp"
#include "ores.marketdata.api/domain/market_series_asset_class_json_io.hpp" // IWYU pragma: keep.
#include "ores.marketdata.core/repository/market_series_asset_class_entity.hpp"
#include "ores.marketdata.core/repository/market_series_asset_class_mapper.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <boost/log/sources/severity_feature.hpp>
#include <boost/uuid/uuid.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <cstddef>
#include <cstdint>
#include <optional>
#include <sqlgen/aggregations.hpp>
#include <sqlgen/delete_from.hpp>
#include <sqlgen/limit.hpp>
#include <sqlgen/literals.hpp>
#include <sqlgen/offset.hpp>
#include <sqlgen/order_by.hpp>
#include <sqlgen/read.hpp>
#include <sqlgen/select_from.hpp>
#include <sqlgen/to.hpp>
#include <sqlgen/where.hpp>
#include <stdexcept>
#include <string>
#include <utility>
#include <vector>

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

ores::utility::domain::precondition
market_series_asset_class_repository::replace_claim(const domain::market_series_asset_class& v) {
    const auto current = read_latest(v.market_series_id, v.asset_class_code);
    if (current.empty())
        return {ores::utility::domain::precondition_kind::must_not_exist, std::nullopt};
    return {ores::utility::domain::precondition_kind::must_match_version,
            static_cast<std::uint32_t>(current.front().version)};
}

domain::market_series_asset_class market_series_asset_class_repository::apply_claim(
    const domain::market_series_asset_class& v, const ores::utility::domain::precondition& claim) {
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
            const auto current = read_latest(v.market_series_id, v.asset_class_code);
            t.version = current.empty() ? 0 : current.front().version;
            break;
        }
    }
    return t;
}

void market_series_asset_class_repository::write(
    const domain::market_series_asset_class& asset_class) {
    write(asset_class, replace_claim(asset_class));
}

void market_series_asset_class_repository::write(
    const std::vector<domain::market_series_asset_class>& asset_classes) {
    std::vector<ores::utility::domain::precondition> claims;
    claims.reserve(asset_classes.size());
    for (const auto& item : asset_classes)
        claims.push_back(replace_claim(item));
    write(asset_classes, claims);
}

void market_series_asset_class_repository::write(
    const domain::market_series_asset_class& asset_class,
    const ores::utility::domain::precondition& claim) {
    BOOST_LOG_SEV(lg(), debug) << "Writing asset class to database: "
                               << asset_class.market_series_id << "/"
                               << asset_class.asset_class_code;
    const auto t = apply_claim(asset_class, claim);
    execute_write_query(
        ctx_, market_series_asset_class_mapper::map(t), lg(), "writing asset class to database");
}

void market_series_asset_class_repository::write(
    const std::vector<domain::market_series_asset_class>& asset_classes,
    const std::vector<ores::utility::domain::precondition>& claims) {
    BOOST_LOG_SEV(lg(), debug) << "Writing asset classes to database. Count: "
                               << asset_classes.size();
    std::vector<domain::market_series_asset_class> batch;
    batch.reserve(asset_classes.size());
    for (std::size_t i = 0; i < asset_classes.size(); ++i)
        batch.push_back(apply_claim(asset_classes[i], claims[i]));
    execute_write_query(ctx_,
                        market_series_asset_class_mapper::map(batch),
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

std::vector<domain::market_series_asset_class>
market_series_asset_class_repository::read_latest(const boost::uuids::uuid& market_series_id,
                                                  const std::string& asset_class_code) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest asset class. " << market_series_id << "/"
                               << asset_class_code;

    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto market_series_id_str = boost::uuids::to_string(market_series_id);
    const auto asset_class_code_str = asset_class_code;
    const auto tid = ctx_.tenant_id().to_string();
    const auto query =
        sqlgen::read<std::vector<market_series_asset_class_entity>> |
        where("tenant_id"_c == tid && "market_series_id"_c == market_series_id_str &&
              "asset_class_code"_c == asset_class_code && "valid_to"_c == max.value());

    return execute_read_query<market_series_asset_class_entity, domain::market_series_asset_class>(
        ctx_,
        query,
        [](const auto& entities) { return market_series_asset_class_mapper::map(entities); },
        lg(),
        "Reading latest asset class by key.");
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

std::vector<domain::market_series_asset_class>
market_series_asset_class_repository::read_latest_by_series(
    const boost::uuids::uuid& market_series_id, std::uint32_t offset, std::uint32_t limit) {
    const auto market_series_id_str = boost::uuids::to_string(market_series_id);
    BOOST_LOG_SEV(lg(), debug) << "Reading latest asset classes. Series: " << market_series_id
                               << " offset: " << offset << " limit: " << limit;

    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx_.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<market_series_asset_class_entity>> |
                       where("tenant_id"_c == tid && "market_series_id"_c == market_series_id_str &&
                             "valid_to"_c == max.value()) |
                       order_by("asset_class_code"_c) | sqlgen::offset(offset) |
                       sqlgen::limit(limit);

    auto rows =
        execute_read_query<market_series_asset_class_entity, domain::market_series_asset_class>(
            ctx_,
            query,
            [](const auto& entities) { return market_series_asset_class_mapper::map(entities); },
            lg(),
            "Reading latest asset classes by series (paginated).");

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
    static_cast<void>(remove(market_series_id, asset_class_code, std::nullopt));
}

market_series_asset_class_repository::remove_status
market_series_asset_class_repository::remove(const boost::uuids::uuid& market_series_id,
                                             const std::string& asset_class_code,
                                             std::optional<std::uint32_t> version) {
    BOOST_LOG_SEV(lg(), debug) << "Removing asset class from database: " << market_series_id << "/"
                               << asset_class_code;

    const auto current = read_latest(market_series_id, asset_class_code);
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
    const auto market_series_id_str = boost::uuids::to_string(market_series_id);
    const auto asset_class_code_str = asset_class_code;
    const auto tid = ctx_.tenant_id().to_string();
    const auto query = sqlgen::delete_from<market_series_asset_class_entity> |
                       where("tenant_id"_c == tid && "market_series_id"_c == market_series_id_str &&
                             "asset_class_code"_c == asset_class_code &&
                             "valid_to"_c == max.value() && "version"_c == expected);

    execute_delete_query(ctx_, query, lg(), "removing asset class from database");
    // The delete reports no affected-row count, so the row is read back: a row
    // still open after the statement means the store refused the removal, and
    // the caller hears "conflicting" rather than "removed".
    if (!read_latest(market_series_id, asset_class_code).empty())
        return remove_status::conflicting;
    return remove_status::removed;
}

void market_series_asset_class_repository::remove(
    const std::vector<boost::uuids::uuid>& market_series_ids,
    const std::vector<std::string>& asset_class_codes) {
    // A junction's key is the pair of columns, and a per-column .in() DELETE
    // would be a cross-product over-delete (rows outside the requested pairs),
    // so each pair is removed on its own.
    if (market_series_ids.size() != asset_class_codes.size())
        throw std::invalid_argument("market_series_asset_class_repository::remove: key column "
                                    "vectors must be the same length");
    for (std::size_t i = 0; i < market_series_ids.size(); ++i)
        static_cast<void>(remove(market_series_ids[i], asset_class_codes[i], std::nullopt));
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
