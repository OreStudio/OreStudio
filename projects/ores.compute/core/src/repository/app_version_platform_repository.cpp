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
#include "ores.compute.api/domain/app_version_platform.hpp"
#include "ores.compute.api/domain/app_version_platform_json_io.hpp" // IWYU pragma: keep.
#include "ores.compute.core/repository/app_version_platform_entity.hpp"
#include "ores.compute.core/repository/app_version_platform_mapper.hpp"
#include "ores.database/query_builder/execute.hpp"
#include "ores.database/repository/helpers.hpp"
#include "ores.database/service/tenant_context.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <boost/uuid/uuid.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <algorithm>
#include <cstddef>
#include <cstdint>
#include <map>
#include <optional>
#include <set>
#include <stdexcept>
#include <string>
#include <unordered_map>
#include <utility>
#include <vector>

namespace ores::compute::repository {

namespace query_builder = ores::database::query_builder;
using namespace ores::database::query_builder::literals;
using namespace ores::logging;
using namespace ores::database::repository;

std::string app_version_platform_repository::sql() {
    return query_builder::insert_statement_sql<app_version_platform_entity>(lg());
}

app_version_platform_repository::app_version_platform_repository(const context& ctx)
    : ctx_(ctx) {}

namespace {

/**
 * @brief The key of a link in the text form the store compares it in.
 */
std::pair<std::string, std::string>
app_version_platform_pair_of(const domain::app_version_platform& v) {
    return {boost::uuids::to_string(v.app_version_id), boost::uuids::to_string(v.platform_id)};
}

/**
 * @brief The claim a replace makes, from the rows the key names now: the
 * version the row carries, or that no row exists yet.
 */
ores::utility::domain::precondition
app_version_platform_replace_claim_of(const std::vector<domain::app_version_platform>& current) {
    if (current.empty())
        return {ores::utility::domain::precondition_kind::must_not_exist, std::nullopt};
    return {ores::utility::domain::precondition_kind::must_match_version,
            static_cast<std::uint32_t>(current.front().version)};
}

/**
 * @brief The object with the claim's version stamped onto it.
 *
 * The caller passes the rows the key names now whenever the claim is any.
 */
domain::app_version_platform
app_version_platform_apply_claim(const domain::app_version_platform& v,
                                 const ores::utility::domain::precondition& claim,
                                 const std::vector<domain::app_version_platform>& current) {
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
        case precondition_kind::any:
            // A caller that claims nothing still has to say what it replaces, so
            // the row's current version is stated. A row that moved on between
            // the read and the write is a conflict the trigger raises, never a
            // silent overwrite.
            t.version = current.empty() ? 0 : current.front().version;
            break;
    }
    return t;
}

/**
 * @brief The current rows the key of an object names, or none.
 */
const std::vector<domain::app_version_platform>&
app_version_platform_rows_of(const app_version_platform_repository::rows_by_pair& current,
                             const domain::app_version_platform& v) {
    static const std::vector<domain::app_version_platform> none;
    const auto found = current.find(app_version_platform_pair_of(v));
    return found == current.end() ? none : found->second;
}

}

app_version_platform_repository::rows_by_pair
app_version_platform_repository::read_current(const std::vector<domain::app_version_platform>& of) {
    rows_by_pair current;
    static constexpr auto max = db_timestamp::max();
    // Every key is a bind and the store's bind count is finite, so a larger
    // set is read one part at a time.
    constexpr std::size_t part_size = 1000;
    for (std::size_t begin = 0; begin < of.size(); begin += part_size) {
        const auto end = std::min(begin + part_size, of.size());
        std::set<std::pair<std::string, std::string>> requested;
        std::set<std::string> left_set;
        std::set<std::string> right_set;
        for (std::size_t i = begin; i < end; ++i) {
            auto pair = app_version_platform_pair_of(of[i]);
            left_set.insert(pair.first);
            right_set.insert(pair.second);
            requested.insert(std::move(pair));
        }
        const std::vector<std::string> lefts(left_set.begin(), left_set.end());
        const std::vector<std::string> rights(right_set.begin(), right_set.end());
        const auto tid = ctx_.tenant_id().to_string();
        static const std::string sys(ores::database::service::tenant_context::system_tenant_id);
        const auto query = query_builder::read<std::vector<app_version_platform_entity>> |
                           query_builder::where(("tenant_id"_c == tid || "tenant_id"_c == sys) &&
                                                "app_version_id"_c.in(lefts) &&
                                                "platform_id"_c.in(rights) && "valid_to"_c == max);
        auto rows = query_builder::execute_read_query<app_version_platform_entity,
                                                      domain::app_version_platform>(
            ctx_,
            query,
            [](const auto& entities) { return app_version_platform_mapper::map(entities); },
            lg(),
            "Reading current app version platforms by key.");
        // The query matches every left with every right, so a row outside the
        // requested pairs is dropped here.
        for (auto& row : rows) {
            auto pair = app_version_platform_pair_of(row);
            if (requested.contains(pair))
                current[std::move(pair)].push_back(std::move(row));
        }
    }
    return current;
}

ores::utility::domain::precondition
app_version_platform_repository::replace_claim(const domain::app_version_platform& v) {
    return app_version_platform_replace_claim_of(read_latest(v.app_version_id, v.platform_id));
}

void app_version_platform_repository::write(
    const domain::app_version_platform& app_version_platform) {
    write(app_version_platform, replace_claim(app_version_platform));
}

void app_version_platform_repository::write(
    const std::vector<domain::app_version_platform>& app_version_platforms) {
    // One read for the set, so a batch states its claims from one read rather
    // than one read per row.
    const auto current = read_current(app_version_platforms);
    std::vector<ores::utility::domain::precondition> claims;
    claims.reserve(app_version_platforms.size());
    for (const auto& item : app_version_platforms)
        claims.push_back(
            app_version_platform_replace_claim_of(app_version_platform_rows_of(current, item)));
    write(app_version_platforms, claims);
}

void app_version_platform_repository::write(
    const domain::app_version_platform& app_version_platform,
    const ores::utility::domain::precondition& claim) {
    ORES_LOG_SEV(lg(), debug) << "Writing app version platform to database: "
                              << app_version_platform.app_version_id << "/"
                              << app_version_platform.platform_id;
    const auto current =
        claim.kind == ores::utility::domain::precondition_kind::any ?
            read_latest(app_version_platform.app_version_id, app_version_platform.platform_id) :
            std::vector<domain::app_version_platform>{};
    const auto t = app_version_platform_apply_claim(app_version_platform, claim, current);
    query_builder::execute_write_query(ctx_,
                                       app_version_platform_mapper::map(t),
                                       lg(),
                                       "writing app version platform to database");
}

void app_version_platform_repository::write(
    const std::vector<domain::app_version_platform>& app_version_platforms,
    const std::vector<ores::utility::domain::precondition>& claims) {
    ORES_LOG_SEV(lg(), debug) << "Writing app version platforms to database. Count: "
                              << app_version_platforms.size();
    if (claims.size() != app_version_platforms.size())
        throw std::invalid_argument(
            "app_version_platform_repository::write: one claim is needed for each row");
    std::vector<domain::app_version_platform> to_read;
    for (std::size_t i = 0; i < app_version_platforms.size(); ++i) {
        if (claims[i].kind == ores::utility::domain::precondition_kind::any)
            to_read.push_back(app_version_platforms[i]);
    }
    const auto current = read_current(to_read);
    std::vector<domain::app_version_platform> batch;
    batch.reserve(app_version_platforms.size());
    for (std::size_t i = 0; i < app_version_platforms.size(); ++i)
        batch.push_back(app_version_platform_apply_claim(
            app_version_platforms[i],
            claims[i],
            app_version_platform_rows_of(current, app_version_platforms[i])));
    query_builder::execute_write_query(ctx_,
                                       app_version_platform_mapper::map(batch),
                                       lg(),
                                       "writing app version platforms to database");
}

std::vector<domain::app_version_platform> app_version_platform_repository::read_latest() {
    static constexpr auto max = db_timestamp::max();
    const auto tid = ctx_.tenant_id().to_string();
    static const std::string sys(ores::database::service::tenant_context::system_tenant_id);
    const auto query = query_builder::read<std::vector<app_version_platform_entity>> |
                       query_builder::where(("tenant_id"_c == tid || "tenant_id"_c == sys) &&
                                            "valid_to"_c == max) |
                       query_builder::order_by("app_version_id"_c, "platform_id"_c);

    return query_builder::execute_read_query<app_version_platform_entity,
                                             domain::app_version_platform>(
        ctx_,
        query,
        [](const auto& entities) { return app_version_platform_mapper::map(entities); },
        lg(),
        "Reading latest app version platforms");
}

std::vector<domain::app_version_platform>
app_version_platform_repository::read_latest(std::uint32_t offset, std::uint32_t limit) {
    ORES_LOG_SEV(lg(), debug) << "Reading latest app version platforms with offset: " << offset
                              << " and limit: " << limit;
    static constexpr auto max = db_timestamp::max();
    const auto tid = ctx_.tenant_id().to_string();
    static const std::string sys(ores::database::service::tenant_context::system_tenant_id);
    const auto query = query_builder::read<std::vector<app_version_platform_entity>> |
                       query_builder::where(("tenant_id"_c == tid || "tenant_id"_c == sys) &&
                                            "valid_to"_c == max) |
                       query_builder::order_by("app_version_id"_c, "platform_id"_c) |
                       query_builder::offset(offset) | query_builder::limit(limit);

    return query_builder::execute_read_query<app_version_platform_entity,
                                             domain::app_version_platform>(
        ctx_,
        query,
        [](const auto& entities) { return app_version_platform_mapper::map(entities); },
        lg(),
        "Reading latest app version platforms (paginated).");
}

std::vector<domain::app_version_platform>
app_version_platform_repository::read_latest(const boost::uuids::uuid& app_version_id,
                                             const boost::uuids::uuid& platform_id) {
    ORES_LOG_SEV(lg(), debug) << "Reading latest app version platform. " << app_version_id << "/"
                              << platform_id;

    static constexpr auto max = db_timestamp::max();
    const auto app_version_id_str = boost::uuids::to_string(app_version_id);
    const auto platform_id_str = boost::uuids::to_string(platform_id);
    const auto tid = ctx_.tenant_id().to_string();
    static const std::string sys(ores::database::service::tenant_context::system_tenant_id);
    const auto query =
        query_builder::read<std::vector<app_version_platform_entity>> |
        query_builder::where(("tenant_id"_c == tid || "tenant_id"_c == sys) &&
                             "app_version_id"_c == app_version_id_str &&
                             "platform_id"_c == platform_id_str && "valid_to"_c == max);

    return query_builder::execute_read_query<app_version_platform_entity,
                                             domain::app_version_platform>(
        ctx_,
        query,
        [](const auto& entities) { return app_version_platform_mapper::map(entities); },
        lg(),
        "Reading latest app version platform by key.");
}

std::uint32_t app_version_platform_repository::get_total_app_version_platform_count() {
    ORES_LOG_SEV(lg(), debug) << "Retrieving total active app version platforms count";
    static constexpr auto max = db_timestamp::max();

    const auto tid = ctx_.tenant_id().to_string();
    static const std::string sys(ores::database::service::tenant_context::system_tenant_id);
    const auto query =
        query_builder::read<std::vector<app_version_platform_entity>> |
        query_builder::where(("tenant_id"_c == tid || "tenant_id"_c == sys) && "valid_to"_c == max);

    const auto count = query_builder::execute_count_query<app_version_platform_entity>(
        ctx_, query, std::nullopt, lg(), "Counting app version platforms");
    ORES_LOG_SEV(lg(), debug) << "Total active app version platforms count: " << count;
    return count;
}

std::vector<domain::app_version_platform>
app_version_platform_repository::read_latest_by_app_version(
    const boost::uuids::uuid& app_version_id) {
    ORES_LOG_SEV(lg(), debug) << "Reading latest app version platforms. App Version: "
                              << app_version_id;

    static constexpr auto max = db_timestamp::max();
    const auto app_version_id_str = boost::uuids::to_string(app_version_id);
    const auto tid = ctx_.tenant_id().to_string();
    static const std::string sys(ores::database::service::tenant_context::system_tenant_id);
    const auto query =
        query_builder::read<std::vector<app_version_platform_entity>> |
        query_builder::where(("tenant_id"_c == tid || "tenant_id"_c == sys) &&
                             "app_version_id"_c == app_version_id_str && "valid_to"_c == max) |
        query_builder::order_by("platform_id"_c);

    auto rows = query_builder::execute_read_query<app_version_platform_entity,
                                                  domain::app_version_platform>(
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
        const auto codes = query_builder::execute_parameterized_multi_column_query(
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
    ORES_LOG_SEV(lg(), debug) << "Reading latest app version platforms. Platform: " << platform_id;

    static constexpr auto max = db_timestamp::max();
    const auto platform_id_str = boost::uuids::to_string(platform_id);
    const auto tid = ctx_.tenant_id().to_string();
    static const std::string sys(ores::database::service::tenant_context::system_tenant_id);
    const auto query =
        query_builder::read<std::vector<app_version_platform_entity>> |
        query_builder::where(("tenant_id"_c == tid || "tenant_id"_c == sys) &&
                             "platform_id"_c == platform_id_str && "valid_to"_c == max) |
        query_builder::order_by("app_version_id"_c);

    auto rows = query_builder::execute_read_query<app_version_platform_entity,
                                                  domain::app_version_platform>(
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
    ORES_LOG_SEV(lg(), debug) << "Reading latest app version platforms. App Version: "
                              << app_version_id << " offset: " << offset << " limit: " << limit;

    static constexpr auto max = db_timestamp::max();
    const auto tid = ctx_.tenant_id().to_string();
    static const std::string sys(ores::database::service::tenant_context::system_tenant_id);
    const auto query =
        query_builder::read<std::vector<app_version_platform_entity>> |
        query_builder::where(("tenant_id"_c == tid || "tenant_id"_c == sys) &&
                             "app_version_id"_c == app_version_id_str && "valid_to"_c == max) |
        query_builder::order_by("platform_id"_c) | query_builder::offset(offset) |
        query_builder::limit(limit);

    auto rows = query_builder::execute_read_query<app_version_platform_entity,
                                                  domain::app_version_platform>(
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
        const auto codes = query_builder::execute_parameterized_multi_column_query(
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
    ORES_LOG_SEV(lg(), debug)
        << "Retrieving total active app version platforms count. App Version: " << app_version_id;
    static constexpr auto max = db_timestamp::max();

    const auto tid = ctx_.tenant_id().to_string();
    static const std::string sys(ores::database::service::tenant_context::system_tenant_id);
    const auto query =
        query_builder::read<std::vector<app_version_platform_entity>> |
        query_builder::where(("tenant_id"_c == tid || "tenant_id"_c == sys) &&
                             "app_version_id"_c == app_version_id_str && "valid_to"_c == max);

    const auto count = query_builder::execute_count_query<app_version_platform_entity>(
        ctx_, query, std::nullopt, lg(), "Counting app version platforms");
    ORES_LOG_SEV(lg(), debug) << "Total active app version platforms count by app_version: "
                              << count;
    return count;
}

std::uint32_t app_version_platform_repository::get_total_app_version_platform_count_by_platform(
    const boost::uuids::uuid& platform_id) {
    const auto platform_id_str = boost::uuids::to_string(platform_id);
    ORES_LOG_SEV(lg(), debug) << "Retrieving total active app version platforms count. Platform: "
                              << platform_id;
    static constexpr auto max = db_timestamp::max();

    const auto tid = ctx_.tenant_id().to_string();
    static const std::string sys(ores::database::service::tenant_context::system_tenant_id);
    const auto query =
        query_builder::read<std::vector<app_version_platform_entity>> |
        query_builder::where(("tenant_id"_c == tid || "tenant_id"_c == sys) &&
                             "platform_id"_c == platform_id_str && "valid_to"_c == max);

    const auto count = query_builder::execute_count_query<app_version_platform_entity>(
        ctx_, query, std::nullopt, lg(), "Counting app version platforms");
    ORES_LOG_SEV(lg(), debug) << "Total active app version platforms count by platform: " << count;
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
    ORES_LOG_SEV(lg(), debug) << "Removing app version platform from database: " << app_version_id
                              << "/" << platform_id;

    const auto current = read_latest(app_version_id, platform_id);
    if (current.empty())
        return remove_status::missing;
    // The protocol states the version as a uint32 and the row carries it as an
    // int, so the comparison states the conversion rather than relying on one.
    if (version && static_cast<std::uint32_t>(current.front().version) != *version)
        return remove_status::conflicting;

    static constexpr auto max = db_timestamp::max();
    // The row is named by its version as well as by its key, so the removal
    // cannot close a row that replaced the one the caller read between the
    // read above and this statement.
    const auto expected = version ? static_cast<int>(*version) : current.front().version;
    const auto app_version_id_str = boost::uuids::to_string(app_version_id);
    const auto platform_id_str = boost::uuids::to_string(platform_id);
    const auto tid = ctx_.tenant_id().to_string();
    const auto query =
        query_builder::delete_from<app_version_platform_entity> |
        query_builder::where("tenant_id"_c == tid && "app_version_id"_c == app_version_id_str &&
                             "platform_id"_c == platform_id_str && "valid_to"_c == max &&
                             "version"_c == expected);

    query_builder::execute_delete_query(
        ctx_, query, lg(), "removing app version platform from database");
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
    ORES_LOG_SEV(lg(), debug) << "Removing all app version platforms from database: "
                              << app_version_id;

    const auto app_version_id_str = boost::uuids::to_string(app_version_id);
    const auto tid = ctx_.tenant_id().to_string();
    const auto query =
        query_builder::delete_from<app_version_platform_entity> |
        query_builder::where("tenant_id"_c == tid && "app_version_id"_c == app_version_id_str);

    query_builder::execute_delete_query(
        ctx_, query, lg(), "removing all app version platforms from database");
}


}
