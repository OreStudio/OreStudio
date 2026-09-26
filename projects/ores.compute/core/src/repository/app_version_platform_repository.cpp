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
#include "ores.compute.core/repository/app_version_platform_repository.hpp"
#include "ores.compute.api/domain/app_version_platform_json_io.hpp" // IWYU pragma: keep.
#include "ores.compute.core/repository/app_version_platform_entity.hpp"
#include "ores.compute.core/repository/app_version_platform_mapper.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.database/repository/helpers.hpp"
#include <boost/uuid/uuid_io.hpp>
#include <cstddef>
#include <optional>
#include <sqlgen/postgres.hpp>
#include <stdexcept>
#include <unordered_map>

namespace ores::compute::repository {

using namespace sqlgen;
using namespace sqlgen::literals;
using namespace ores::logging;
using namespace ores::database::repository;

std::string app_version_platform_repository::sql() {
    return generate_create_table_sql<app_version_platform_entity>(lg());
}

app_version_platform_repository::app_version_platform_repository(context ctx)
    : ctx_(std::move(ctx)) {}

ores::utility::domain::precondition
app_version_platform_repository::replace_claim(const domain::app_version_platform& v) {
    const auto current = read_latest(v.app_version_id, v.platform_id);
    if (current.empty())
        return {ores::utility::domain::precondition_kind::must_not_exist, std::nullopt};
    return {ores::utility::domain::precondition_kind::must_match_version,
            static_cast<std::uint32_t>(current.front().version)};
}

domain::app_version_platform
app_version_platform_repository::apply_claim(const domain::app_version_platform& v,
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
            const auto current = read_latest(v.app_version_id, v.platform_id);
            t.version = current.empty() ? 0 : current.front().version;
            break;
        }
    }
    return t;
}

void app_version_platform_repository::write(
    const domain::app_version_platform& app_version_platform) {
    write(app_version_platform, replace_claim(app_version_platform));
}

void app_version_platform_repository::write(
    const std::vector<domain::app_version_platform>& app_version_platforms) {
    std::vector<ores::utility::domain::precondition> claims;
    claims.reserve(app_version_platforms.size());
    for (const auto& item : app_version_platforms)
        claims.push_back(replace_claim(item));
    write(app_version_platforms, claims);
}

void app_version_platform_repository::write(
    const domain::app_version_platform& app_version_platform,
    const ores::utility::domain::precondition& claim) {
    BOOST_LOG_SEV(lg(), debug) << "Writing app version platform to database: "
                               << app_version_platform.app_version_id << "/"
                               << app_version_platform.platform_id;
    const auto t = apply_claim(app_version_platform, claim);
    execute_write_query(ctx_,
                        app_version_platform_mapper::map(t),
                        lg(),
                        "writing app version platform to database");
}

void app_version_platform_repository::write(
    const std::vector<domain::app_version_platform>& app_version_platforms,
    const std::vector<ores::utility::domain::precondition>& claims) {
    BOOST_LOG_SEV(lg(), debug) << "Writing app version platforms to database. Count: "
                               << app_version_platforms.size();
    std::vector<domain::app_version_platform> batch;
    batch.reserve(app_version_platforms.size());
    for (std::size_t i = 0; i < app_version_platforms.size(); ++i)
        batch.push_back(apply_claim(app_version_platforms[i], claims[i]));
    execute_write_query(ctx_,
                        app_version_platform_mapper::map(batch),
                        lg(),
                        "writing app version platforms to database");
}

std::vector<domain::app_version_platform> app_version_platform_repository::read_latest() {
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx_.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<app_version_platform_entity>> |
                       where("tenant_id"_c == tid && "valid_to"_c == max.value()) |
                       order_by("app_version_id"_c, "platform_id"_c);

    return execute_read_query<app_version_platform_entity, domain::app_version_platform>(
        ctx_,
        query,
        [](const auto& entities) { return app_version_platform_mapper::map(entities); },
        lg(),
        "Reading latest app version platforms");
}

std::vector<domain::app_version_platform>
app_version_platform_repository::read_latest(std::uint32_t offset, std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest app version platforms with offset: " << offset
                               << " and limit: " << limit;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx_.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<app_version_platform_entity>> |
                       where("tenant_id"_c == tid && "valid_to"_c == max.value()) |
                       order_by("app_version_id"_c, "platform_id"_c) | sqlgen::offset(offset) |
                       sqlgen::limit(limit);

    return execute_read_query<app_version_platform_entity, domain::app_version_platform>(
        ctx_,
        query,
        [](const auto& entities) { return app_version_platform_mapper::map(entities); },
        lg(),
        "Reading latest app version platforms (paginated).");
}

std::vector<domain::app_version_platform>
app_version_platform_repository::read_latest(const boost::uuids::uuid& app_version_id,
                                             const boost::uuids::uuid& platform_id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest app version platform. " << app_version_id << "/"
                               << platform_id;

    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto app_version_id_str = boost::uuids::to_string(app_version_id);
    const auto platform_id_str = boost::uuids::to_string(platform_id);
    const auto tid = ctx_.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<app_version_platform_entity>> |
                       where("tenant_id"_c == tid && "app_version_id"_c == app_version_id_str &&
                             "platform_id"_c == platform_id_str && "valid_to"_c == max.value());

    return execute_read_query<app_version_platform_entity, domain::app_version_platform>(
        ctx_,
        query,
        [](const auto& entities) { return app_version_platform_mapper::map(entities); },
        lg(),
        "Reading latest app version platform by key.");
}

std::uint32_t app_version_platform_repository::get_total_app_version_platform_count() {
    BOOST_LOG_SEV(lg(), debug) << "Retrieving total active app version platforms count";
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));

    struct count_result {
        long long count;
    };

    const auto tid = ctx_.tenant_id().to_string();
    const auto query =
        sqlgen::select_from<app_version_platform_entity>(sqlgen::count().as<"count">()) |
        where("tenant_id"_c == tid && "valid_to"_c == max.value()) | sqlgen::to<count_result>;

    const auto r = sqlgen::session(ctx_.connection_pool()).and_then(query);
    ensure_success(r, lg());

    const auto count = static_cast<std::uint32_t>(r->count);
    BOOST_LOG_SEV(lg(), debug) << "Total active app version platforms count: " << count;
    return count;
}

std::vector<domain::app_version_platform>
app_version_platform_repository::read_latest_by_app_version(
    const boost::uuids::uuid& app_version_id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest app version platforms. App Version: "
                               << app_version_id;

    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto app_version_id_str = boost::uuids::to_string(app_version_id);
    const auto tid = ctx_.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<app_version_platform_entity>> |
                       where("tenant_id"_c == tid && "app_version_id"_c == app_version_id_str &&
                             "valid_to"_c == max.value()) |
                       order_by("platform_id"_c);

    auto rows = execute_read_query<app_version_platform_entity, domain::app_version_platform>(
        ctx_,
        query,
        [](const auto& entities) { return app_version_platform_mapper::map(entities); },
        lg(),
        "Reading latest app version platforms by app_version.");

    if (!rows.empty()) {
        std::unordered_map<std::string, std::string> code_by_id;
        std::string ids = "{";
        for (std::size_t i = 0; i < rows.size(); ++i) {
            if (i > 0)
                ids += ",";
            ids += boost::uuids::to_string(rows[i].platform_id);
        }
        ids += "}";
        const auto codes = execute_parameterized_multi_column_query(
            ctx_,
            "SELECT \"id\"::text, \"code\"::text"
            "  FROM ores_compute_platforms_tbl"
            " WHERE \"valid_to\" = ores_utility_infinity_timestamp_fn()"
            "   AND \"id\" = ANY($1::uuid[])",
            {ids},
            lg(),
            "Reading platform codes for app version platforms");
        code_by_id.reserve(codes.size());
        for (const auto& code_row : codes) {
            if (code_row[0] && code_row[1])
                code_by_id.emplace(*code_row[0], *code_row[1]);
        }
        for (auto& row : rows) {
            if (const auto it = code_by_id.find(boost::uuids::to_string(row.platform_id));
                it != code_by_id.end())
                row.platform_code = it->second;
        }
    }
    return rows;
}

std::vector<domain::app_version_platform>
app_version_platform_repository::read_latest_by_platform(const boost::uuids::uuid& platform_id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest app version platforms. Platform: " << platform_id;

    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto platform_id_str = boost::uuids::to_string(platform_id);
    const auto tid = ctx_.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<app_version_platform_entity>> |
                       where("tenant_id"_c == tid && "platform_id"_c == platform_id_str &&
                             "valid_to"_c == max.value()) |
                       order_by("app_version_id"_c);

    auto rows = execute_read_query<app_version_platform_entity, domain::app_version_platform>(
        ctx_,
        query,
        [](const auto& entities) { return app_version_platform_mapper::map(entities); },
        lg(),
        "Reading latest app version platforms by platform.");

    return rows;
}

std::vector<domain::app_version_platform>
app_version_platform_repository::read_latest_by_app_version(
    const boost::uuids::uuid& app_version_id, std::uint32_t offset, std::uint32_t limit) {
    const auto app_version_id_str = boost::uuids::to_string(app_version_id);
    BOOST_LOG_SEV(lg(), debug) << "Reading latest app version platforms. App Version: "
                               << app_version_id << " offset: " << offset << " limit: " << limit;

    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx_.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<app_version_platform_entity>> |
                       where("tenant_id"_c == tid && "app_version_id"_c == app_version_id_str &&
                             "valid_to"_c == max.value()) |
                       order_by("platform_id"_c) | sqlgen::offset(offset) | sqlgen::limit(limit);

    auto rows = execute_read_query<app_version_platform_entity, domain::app_version_platform>(
        ctx_,
        query,
        [](const auto& entities) { return app_version_platform_mapper::map(entities); },
        lg(),
        "Reading latest app version platforms by app_version (paginated).");

    if (!rows.empty()) {
        std::unordered_map<std::string, std::string> code_by_id;
        std::string ids = "{";
        for (std::size_t i = 0; i < rows.size(); ++i) {
            if (i > 0)
                ids += ",";
            ids += boost::uuids::to_string(rows[i].platform_id);
        }
        ids += "}";
        const auto codes = execute_parameterized_multi_column_query(
            ctx_,
            "SELECT \"id\"::text, \"code\"::text"
            "  FROM ores_compute_platforms_tbl"
            " WHERE \"valid_to\" = ores_utility_infinity_timestamp_fn()"
            "   AND \"id\" = ANY($1::uuid[])",
            {ids},
            lg(),
            "Reading platform codes for app version platforms");
        code_by_id.reserve(codes.size());
        for (const auto& code_row : codes) {
            if (code_row[0] && code_row[1])
                code_by_id.emplace(*code_row[0], *code_row[1]);
        }
        for (auto& row : rows) {
            if (const auto it = code_by_id.find(boost::uuids::to_string(row.platform_id));
                it != code_by_id.end())
                row.platform_code = it->second;
        }
    }
    return rows;
}

std::uint32_t app_version_platform_repository::get_total_app_version_platform_count_by_app_version(
    const boost::uuids::uuid& app_version_id) {
    const auto app_version_id_str = boost::uuids::to_string(app_version_id);
    BOOST_LOG_SEV(lg(), debug)
        << "Retrieving total active app version platforms count. App Version: " << app_version_id;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));

    struct count_result {
        long long count;
    };

    const auto tid = ctx_.tenant_id().to_string();
    const auto query =
        sqlgen::select_from<app_version_platform_entity>(sqlgen::count().as<"count">()) |
        where("tenant_id"_c == tid && "app_version_id"_c == app_version_id_str &&
              "valid_to"_c == max.value()) |
        sqlgen::to<count_result>;

    const auto r = sqlgen::session(ctx_.connection_pool()).and_then(query);
    ensure_success(r, lg());

    const auto count = static_cast<std::uint32_t>(r->count);
    BOOST_LOG_SEV(lg(), debug) << "Total active app version platforms count by app_version: "
                               << count;
    return count;
}

std::uint32_t app_version_platform_repository::get_total_app_version_platform_count_by_platform(
    const boost::uuids::uuid& platform_id) {
    const auto platform_id_str = boost::uuids::to_string(platform_id);
    BOOST_LOG_SEV(lg(), debug) << "Retrieving total active app version platforms count. Platform: "
                               << platform_id;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));

    struct count_result {
        long long count;
    };

    const auto tid = ctx_.tenant_id().to_string();
    const auto query =
        sqlgen::select_from<app_version_platform_entity>(sqlgen::count().as<"count">()) |
        where("tenant_id"_c == tid && "platform_id"_c == platform_id_str &&
              "valid_to"_c == max.value()) |
        sqlgen::to<count_result>;

    const auto r = sqlgen::session(ctx_.connection_pool()).and_then(query);
    ensure_success(r, lg());

    const auto count = static_cast<std::uint32_t>(r->count);
    BOOST_LOG_SEV(lg(), debug) << "Total active app version platforms count by platform: " << count;
    return count;
}

void app_version_platform_repository::remove(const boost::uuids::uuid& app_version_id,
                                             const boost::uuids::uuid& platform_id) {
    static_cast<void>(remove(app_version_id, platform_id, std::nullopt));
}

app_version_platform_repository::remove_status
app_version_platform_repository::remove(const boost::uuids::uuid& app_version_id,
                                        const boost::uuids::uuid& platform_id,
                                        std::optional<std::uint32_t> version) {
    BOOST_LOG_SEV(lg(), debug) << "Removing app version platform from database: " << app_version_id
                               << "/" << platform_id;

    const auto current = read_latest(app_version_id, platform_id);
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
    const auto app_version_id_str = boost::uuids::to_string(app_version_id);
    const auto platform_id_str = boost::uuids::to_string(platform_id);
    const auto tid = ctx_.tenant_id().to_string();
    const auto query = sqlgen::delete_from<app_version_platform_entity> |
                       where("tenant_id"_c == tid && "app_version_id"_c == app_version_id_str &&
                             "platform_id"_c == platform_id_str && "valid_to"_c == max.value() &&
                             "version"_c == expected);

    execute_delete_query(ctx_, query, lg(), "removing app version platform from database");
    // The delete reports no affected-row count, so the row is read back: a row
    // still open after the statement means the store refused the removal, and
    // the caller hears "conflicting" rather than "removed".
    if (!read_latest(app_version_id, platform_id).empty())
        return remove_status::conflicting;
    return remove_status::removed;
}

void app_version_platform_repository::remove(const std::vector<boost::uuids::uuid>& app_version_ids,
                                             const std::vector<boost::uuids::uuid>& platform_ids) {
    // A junction's key is the pair of columns, and a per-column .in() DELETE
    // would be a cross-product over-delete (rows outside the requested pairs),
    // so each pair is removed on its own.
    if (app_version_ids.size() != platform_ids.size())
        throw std::invalid_argument(
            "app_version_platform_repository::remove: key column vectors must be the same length");
    for (std::size_t i = 0; i < app_version_ids.size(); ++i)
        static_cast<void>(remove(app_version_ids[i], platform_ids[i], std::nullopt));
}

void app_version_platform_repository::remove_by_app_version(
    const boost::uuids::uuid& app_version_id) {
    BOOST_LOG_SEV(lg(), debug) << "Removing all app version platforms from database: "
                               << app_version_id;

    const auto app_version_id_str = boost::uuids::to_string(app_version_id);
    const auto tid = ctx_.tenant_id().to_string();
    const auto query = sqlgen::delete_from<app_version_platform_entity> |
                       where("tenant_id"_c == tid && "app_version_id"_c == app_version_id_str);

    execute_delete_query(ctx_, query, lg(), "removing all app version platforms from database");
}


}
