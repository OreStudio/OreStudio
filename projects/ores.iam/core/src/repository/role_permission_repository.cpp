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
#include "ores.iam.core/repository/role_permission_repository.hpp"
#include "ores.database/domain/tenant_aware_pool.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.database/repository/helpers.hpp"
#include "ores.database/repository/mapper_helpers.hpp"
#include "ores.iam.api/domain/role_permission.hpp"
#include "ores.iam.api/domain/role_permission_json_io.hpp" // IWYU pragma: keep.
#include "ores.iam.core/repository/role_permission_entity.hpp"
#include "ores.iam.core/repository/role_permission_mapper.hpp"
#include "ores.logging/boost_severity.hpp"
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

namespace ores::iam::repository {

using namespace sqlgen;
using namespace sqlgen::literals;
using namespace ores::logging;
using namespace ores::database::repository;

std::string role_permission_repository::sql() {
    return generate_create_table_sql<role_permission_entity>(lg());
}

role_permission_repository::role_permission_repository(context ctx)
    : ctx_(std::move(ctx)) {}

ores::utility::domain::precondition
role_permission_repository::replace_claim(const domain::role_permission& v) {
    const auto current = read_latest(v.role_id, v.permission_id);
    if (current.empty())
        return {ores::utility::domain::precondition_kind::must_not_exist, std::nullopt};
    return {ores::utility::domain::precondition_kind::must_match_version,
            static_cast<std::uint32_t>(current.front().version)};
}

domain::role_permission
role_permission_repository::apply_claim(const domain::role_permission& v,
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
            const auto current = read_latest(v.role_id, v.permission_id);
            t.version = current.empty() ? 0 : current.front().version;
            break;
        }
    }
    return t;
}

void role_permission_repository::write(const domain::role_permission& role_permission) {
    write(role_permission, replace_claim(role_permission));
}

void role_permission_repository::write(
    const std::vector<domain::role_permission>& role_permissions) {
    std::vector<ores::utility::domain::precondition> claims;
    claims.reserve(role_permissions.size());
    for (const auto& item : role_permissions)
        claims.push_back(replace_claim(item));
    write(role_permissions, claims);
}

void role_permission_repository::write(const domain::role_permission& role_permission,
                                       const ores::utility::domain::precondition& claim) {
    BOOST_LOG_SEV(lg(), debug) << "Writing role permission to database: " << role_permission.role_id
                               << "/" << role_permission.permission_id;
    const auto t = apply_claim(role_permission, claim);
    execute_write_query(
        ctx_, role_permission_mapper::map(t), lg(), "writing role permission to database");
}

void role_permission_repository::write(
    const std::vector<domain::role_permission>& role_permissions,
    const std::vector<ores::utility::domain::precondition>& claims) {
    BOOST_LOG_SEV(lg(), debug) << "Writing role permissions to database. Count: "
                               << role_permissions.size();
    std::vector<domain::role_permission> batch;
    batch.reserve(role_permissions.size());
    for (std::size_t i = 0; i < role_permissions.size(); ++i)
        batch.push_back(apply_claim(role_permissions[i], claims[i]));
    execute_write_query(
        ctx_, role_permission_mapper::map(batch), lg(), "writing role permissions to database");
}

std::vector<domain::role_permission> role_permission_repository::read_latest() {
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx_.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<role_permission_entity>> |
                       where("tenant_id"_c == tid && "valid_to"_c == max.value()) |
                       order_by("role_id"_c, "role_id"_c);

    return execute_read_query<role_permission_entity, domain::role_permission>(
        ctx_,
        query,
        [](const auto& entities) { return role_permission_mapper::map(entities); },
        lg(),
        "Reading latest role permissions");
}

std::vector<domain::role_permission> role_permission_repository::read_latest(std::uint32_t offset,
                                                                             std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest role permissions with offset: " << offset
                               << " and limit: " << limit;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx_.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<role_permission_entity>> |
                       where("tenant_id"_c == tid && "valid_to"_c == max.value()) |
                       order_by("role_id"_c, "role_id"_c) | sqlgen::offset(offset) |
                       sqlgen::limit(limit);

    return execute_read_query<role_permission_entity, domain::role_permission>(
        ctx_,
        query,
        [](const auto& entities) { return role_permission_mapper::map(entities); },
        lg(),
        "Reading latest role permissions (paginated).");
}

std::vector<domain::role_permission>
role_permission_repository::read_latest(const boost::uuids::uuid& role_id,
                                        const boost::uuids::uuid& permission_id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest role permission. " << role_id << "/"
                               << permission_id;

    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto role_id_str = boost::uuids::to_string(role_id);
    const auto permission_id_str = boost::uuids::to_string(permission_id);
    const auto tid = ctx_.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<role_permission_entity>> |
                       where("tenant_id"_c == tid && "role_id"_c == role_id_str &&
                             "permission_id"_c == permission_id_str && "valid_to"_c == max.value());

    return execute_read_query<role_permission_entity, domain::role_permission>(
        ctx_,
        query,
        [](const auto& entities) { return role_permission_mapper::map(entities); },
        lg(),
        "Reading latest role permission by key.");
}

std::uint32_t role_permission_repository::get_total_role_permission_count() {
    BOOST_LOG_SEV(lg(), debug) << "Retrieving total active role permissions count";
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));

    struct count_result {
        long long count;
    };

    const auto tid = ctx_.tenant_id().to_string();
    const auto query = sqlgen::select_from<role_permission_entity>(sqlgen::count().as<"count">()) |
                       where("tenant_id"_c == tid && "valid_to"_c == max.value()) |
                       sqlgen::to<count_result>;

    const auto r = sqlgen::session(ctx_.connection_pool()).and_then(query);
    ensure_success(r, lg());

    const auto count = static_cast<std::uint32_t>(r->count);
    BOOST_LOG_SEV(lg(), debug) << "Total active role permissions count: " << count;
    return count;
}

std::vector<domain::role_permission>
role_permission_repository::read_latest_by_role(const boost::uuids::uuid& role_id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest role permissions. Role: " << role_id;

    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto role_id_str = boost::uuids::to_string(role_id);
    const auto tid = ctx_.tenant_id().to_string();
    const auto query =
        sqlgen::read<std::vector<role_permission_entity>> |
        where("tenant_id"_c == tid && "role_id"_c == role_id_str && "valid_to"_c == max.value()) |
        order_by("role_id"_c);

    auto rows = execute_read_query<role_permission_entity, domain::role_permission>(
        ctx_,
        query,
        [](const auto& entities) { return role_permission_mapper::map(entities); },
        lg(),
        "Reading latest role permissions by role.");

    return rows;
}

std::vector<domain::role_permission>
role_permission_repository::read_latest_by_permission(const boost::uuids::uuid& permission_id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest role permissions. Permission: " << permission_id;

    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto permission_id_str = boost::uuids::to_string(permission_id);
    const auto tid = ctx_.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<role_permission_entity>> |
                       where("tenant_id"_c == tid && "permission_id"_c == permission_id_str &&
                             "valid_to"_c == max.value()) |
                       order_by("role_id"_c);

    auto rows = execute_read_query<role_permission_entity, domain::role_permission>(
        ctx_,
        query,
        [](const auto& entities) { return role_permission_mapper::map(entities); },
        lg(),
        "Reading latest role permissions by permission.");

    return rows;
}

std::vector<domain::role_permission> role_permission_repository::read_latest_by_role(
    const boost::uuids::uuid& role_id, std::uint32_t offset, std::uint32_t limit) {
    const auto role_id_str = boost::uuids::to_string(role_id);
    BOOST_LOG_SEV(lg(), debug) << "Reading latest role permissions. Role: " << role_id
                               << " offset: " << offset << " limit: " << limit;

    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx_.tenant_id().to_string();
    const auto query =
        sqlgen::read<std::vector<role_permission_entity>> |
        where("tenant_id"_c == tid && "role_id"_c == role_id_str && "valid_to"_c == max.value()) |
        order_by("role_id"_c) | sqlgen::offset(offset) | sqlgen::limit(limit);

    auto rows = execute_read_query<role_permission_entity, domain::role_permission>(
        ctx_,
        query,
        [](const auto& entities) { return role_permission_mapper::map(entities); },
        lg(),
        "Reading latest role permissions by role (paginated).");

    return rows;
}

std::uint32_t role_permission_repository::get_total_role_permission_count_by_role(
    const boost::uuids::uuid& role_id) {
    const auto role_id_str = boost::uuids::to_string(role_id);
    BOOST_LOG_SEV(lg(), debug) << "Retrieving total active role permissions count. Role: "
                               << role_id;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));

    struct count_result {
        long long count;
    };

    const auto tid = ctx_.tenant_id().to_string();
    const auto query =
        sqlgen::select_from<role_permission_entity>(sqlgen::count().as<"count">()) |
        where("tenant_id"_c == tid && "role_id"_c == role_id_str && "valid_to"_c == max.value()) |
        sqlgen::to<count_result>;

    const auto r = sqlgen::session(ctx_.connection_pool()).and_then(query);
    ensure_success(r, lg());

    const auto count = static_cast<std::uint32_t>(r->count);
    BOOST_LOG_SEV(lg(), debug) << "Total active role permissions count by role: " << count;
    return count;
}

std::vector<domain::role_permission> role_permission_repository::read_latest_by_permission(
    const boost::uuids::uuid& permission_id, std::uint32_t offset, std::uint32_t limit) {
    const auto permission_id_str = boost::uuids::to_string(permission_id);
    BOOST_LOG_SEV(lg(), debug) << "Reading latest role permissions. Permission: " << permission_id
                               << " offset: " << offset << " limit: " << limit;

    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx_.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<role_permission_entity>> |
                       where("tenant_id"_c == tid && "permission_id"_c == permission_id_str &&
                             "valid_to"_c == max.value()) |
                       order_by("role_id"_c) | sqlgen::offset(offset) | sqlgen::limit(limit);

    auto rows = execute_read_query<role_permission_entity, domain::role_permission>(
        ctx_,
        query,
        [](const auto& entities) { return role_permission_mapper::map(entities); },
        lg(),
        "Reading latest role permissions by permission (paginated).");

    return rows;
}

std::uint32_t role_permission_repository::get_total_role_permission_count_by_permission(
    const boost::uuids::uuid& permission_id) {
    const auto permission_id_str = boost::uuids::to_string(permission_id);
    BOOST_LOG_SEV(lg(), debug) << "Retrieving total active role permissions count. Permission: "
                               << permission_id;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));

    struct count_result {
        long long count;
    };

    const auto tid = ctx_.tenant_id().to_string();
    const auto query = sqlgen::select_from<role_permission_entity>(sqlgen::count().as<"count">()) |
                       where("tenant_id"_c == tid && "permission_id"_c == permission_id_str &&
                             "valid_to"_c == max.value()) |
                       sqlgen::to<count_result>;

    const auto r = sqlgen::session(ctx_.connection_pool()).and_then(query);
    ensure_success(r, lg());

    const auto count = static_cast<std::uint32_t>(r->count);
    BOOST_LOG_SEV(lg(), debug) << "Total active role permissions count by permission: " << count;
    return count;
}

std::vector<role_permission_history_row>
role_permission_repository::read_all_by_role(const boost::uuids::uuid& role_id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading every version of the role permissions. Role: "
                               << role_id;

    const auto role_id_str = boost::uuids::to_string(role_id);
    const auto tid = ctx_.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<role_permission_entity>> |
                       where("tenant_id"_c == tid && "role_id"_c == role_id_str) |
                       order_by("permission_id"_c, "valid_from"_c.desc());

    return execute_read_query<role_permission_entity, role_permission_history_row>(
        ctx_,
        query,
        [](const auto& entities) {
            std::vector<role_permission_history_row> rows;
            rows.reserve(entities.size());
            for (const auto& entity : entities) {
                role_permission_history_row row;
                row.link = role_permission_mapper::map(entity);
                row.modified_by = row.link.modified_by;
                row.recorded_at = row.link.recorded_at;
                // A version ends when a later version replaces it, which is not
                // a closing, or when the link is closed. The rows of one link run
                // newest first, so the row before this one, when it is the same
                // link, is the next newer version, and it replaced this one only
                // if it carries the next version number. A link opened again
                // after a closing starts its numbering over.
                const bool replaced = !rows.empty() &&
                                      rows.back().link.permission_id == row.link.permission_id &&
                                      rows.back().link.version == row.link.version + 1;
                if (!replaced) {
                    row.closed_at = validity_end(entity.valid_to);
                    if (row.closed_at)
                        row.closed_by = entity.closed_by.value_or("");
                }
                rows.push_back(std::move(row));
            }
            return rows;
        },
        lg(),
        "Reading every version of the role permissions by role.");
}

void role_permission_repository::remove(const boost::uuids::uuid& role_id,
                                        const boost::uuids::uuid& permission_id) {
    static_cast<void>(remove(role_id, permission_id, std::nullopt));
}

role_permission_repository::remove_status
role_permission_repository::remove(const boost::uuids::uuid& role_id,
                                   const boost::uuids::uuid& permission_id,
                                   std::optional<std::uint32_t> version) {
    BOOST_LOG_SEV(lg(), debug) << "Removing role permission from database: " << role_id << "/"
                               << permission_id;

    const auto current = read_latest(role_id, permission_id);
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
    const auto role_id_str = boost::uuids::to_string(role_id);
    const auto permission_id_str = boost::uuids::to_string(permission_id);
    const auto tid = ctx_.tenant_id().to_string();
    const auto query = sqlgen::delete_from<role_permission_entity> |
                       where("tenant_id"_c == tid && "role_id"_c == role_id_str &&
                             "permission_id"_c == permission_id_str &&
                             "valid_to"_c == max.value() && "version"_c == expected);

    execute_delete_query(ctx_, query, lg(), "removing role permission from database");
    // The delete reports no affected-row count, so the row is read back: a row
    // still open after the statement means the store refused the removal, and
    // the caller hears "conflicting" rather than "removed".
    if (!read_latest(role_id, permission_id).empty())
        return remove_status::conflicting;
    return remove_status::removed;
}

void role_permission_repository::remove(const std::vector<boost::uuids::uuid>& role_ids,
                                        const std::vector<boost::uuids::uuid>& permission_ids) {
    // A junction's key is the pair of columns, and a per-column .in() DELETE
    // would be a cross-product over-delete (rows outside the requested pairs),
    // so each pair is removed on its own.
    if (role_ids.size() != permission_ids.size())
        throw std::invalid_argument(
            "role_permission_repository::remove: key column vectors must be the same length");
    for (std::size_t i = 0; i < role_ids.size(); ++i)
        static_cast<void>(remove(role_ids[i], permission_ids[i], std::nullopt));
}

void role_permission_repository::remove_by_role(const boost::uuids::uuid& role_id) {
    BOOST_LOG_SEV(lg(), debug) << "Removing all role permissions from database: " << role_id;

    const auto role_id_str = boost::uuids::to_string(role_id);
    const auto tid = ctx_.tenant_id().to_string();
    const auto query = sqlgen::delete_from<role_permission_entity> |
                       where("tenant_id"_c == tid && "role_id"_c == role_id_str);

    execute_delete_query(ctx_, query, lg(), "removing all role permissions from database");
}


}
