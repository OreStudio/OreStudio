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
#include "ores.database/query_builder/execute.hpp"
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
#include <algorithm>
#include <cstddef>
#include <cstdint>
#include <map>
#include <optional>
#include <set>
#include <stdexcept>
#include <string>
#include <utility>
#include <vector>

namespace ores::iam::repository {

namespace query_builder = ores::database::query_builder;
using namespace ores::database::query_builder::literals;
using namespace ores::logging;
using namespace ores::database::repository;

std::string role_permission_repository::sql() {
    return query_builder::insert_statement_sql<role_permission_entity>(lg());
}

role_permission_repository::role_permission_repository(const context& ctx)
    : ctx_(ctx) {}

namespace {

/**
 * @brief The key of a link in the text form the store compares it in.
 */
std::pair<std::string, std::string> role_permission_pair_of(const domain::role_permission& v) {
    return {boost::uuids::to_string(v.role_id), boost::uuids::to_string(v.permission_id)};
}

/**
 * @brief The claim a replace makes, from the rows the key names now: the
 * version the row carries, or that no row exists yet.
 */
ores::utility::domain::precondition
role_permission_replace_claim_of(const std::vector<domain::role_permission>& current) {
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
domain::role_permission
role_permission_apply_claim(const domain::role_permission& v,
                            const ores::utility::domain::precondition& claim,
                            const std::vector<domain::role_permission>& current) {
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
const std::vector<domain::role_permission>&
role_permission_rows_of(const role_permission_repository::rows_by_pair& current,
                        const domain::role_permission& v) {
    static const std::vector<domain::role_permission> none;
    const auto found = current.find(role_permission_pair_of(v));
    return found == current.end() ? none : found->second;
}

}

role_permission_repository::rows_by_pair
role_permission_repository::read_current(const std::vector<domain::role_permission>& of) {
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
            auto pair = role_permission_pair_of(of[i]);
            left_set.insert(pair.first);
            right_set.insert(pair.second);
            requested.insert(std::move(pair));
        }
        const std::vector<std::string> lefts(left_set.begin(), left_set.end());
        const std::vector<std::string> rights(right_set.begin(), right_set.end());
        const auto tid = ctx_.tenant_id().to_string();
        const auto query =
            query_builder::read<std::vector<role_permission_entity>> |
            query_builder::where("tenant_id"_c == tid && "role_id"_c.in(lefts) &&
                                 "permission_id"_c.in(rights) && "valid_to"_c == max);
        auto rows =
            query_builder::execute_read_query<role_permission_entity, domain::role_permission>(
                ctx_,
                query,
                [](const auto& entities) { return role_permission_mapper::map(entities); },
                lg(),
                "Reading current role permissions by key.");
        // The query matches every left with every right, so a row outside the
        // requested pairs is dropped here.
        for (auto& row : rows) {
            auto pair = role_permission_pair_of(row);
            if (requested.contains(pair))
                current[std::move(pair)].push_back(std::move(row));
        }
    }
    return current;
}

ores::utility::domain::precondition
role_permission_repository::replace_claim(const domain::role_permission& v) {
    return role_permission_replace_claim_of(read_latest(v.role_id, v.permission_id));
}

void role_permission_repository::write(const domain::role_permission& role_permission) {
    write(role_permission, replace_claim(role_permission));
}

void role_permission_repository::write(
    const std::vector<domain::role_permission>& role_permissions) {
    // One read for the set, so a batch states its claims from one read rather
    // than one read per row.
    const auto current = read_current(role_permissions);
    std::vector<ores::utility::domain::precondition> claims;
    claims.reserve(role_permissions.size());
    for (const auto& item : role_permissions)
        claims.push_back(role_permission_replace_claim_of(role_permission_rows_of(current, item)));
    write(role_permissions, claims);
}

void role_permission_repository::write(const domain::role_permission& role_permission,
                                       const ores::utility::domain::precondition& claim) {
    BOOST_LOG_SEV(lg(), debug) << "Writing role permission to database: " << role_permission.role_id
                               << "/" << role_permission.permission_id;
    const auto current = claim.kind == ores::utility::domain::precondition_kind::any ?
                             read_latest(role_permission.role_id, role_permission.permission_id) :
                             std::vector<domain::role_permission>{};
    const auto t = role_permission_apply_claim(role_permission, claim, current);
    query_builder::execute_write_query(
        ctx_, role_permission_mapper::map(t), lg(), "writing role permission to database");
}

void role_permission_repository::write(
    const std::vector<domain::role_permission>& role_permissions,
    const std::vector<ores::utility::domain::precondition>& claims) {
    BOOST_LOG_SEV(lg(), debug) << "Writing role permissions to database. Count: "
                               << role_permissions.size();
    if (claims.size() != role_permissions.size())
        throw std::invalid_argument(
            "role_permission_repository::write: one claim is needed for each row");
    std::vector<domain::role_permission> to_read;
    for (std::size_t i = 0; i < role_permissions.size(); ++i) {
        if (claims[i].kind == ores::utility::domain::precondition_kind::any)
            to_read.push_back(role_permissions[i]);
    }
    const auto current = read_current(to_read);
    std::vector<domain::role_permission> batch;
    batch.reserve(role_permissions.size());
    for (std::size_t i = 0; i < role_permissions.size(); ++i)
        batch.push_back(role_permission_apply_claim(
            role_permissions[i], claims[i], role_permission_rows_of(current, role_permissions[i])));
    query_builder::execute_write_query(
        ctx_, role_permission_mapper::map(batch), lg(), "writing role permissions to database");
}

std::vector<domain::role_permission> role_permission_repository::read_latest() {
    static constexpr auto max = db_timestamp::max();
    const auto tid = ctx_.tenant_id().to_string();
    const auto query = query_builder::read<std::vector<role_permission_entity>> |
                       query_builder::where("tenant_id"_c == tid && "valid_to"_c == max) |
                       query_builder::order_by("role_id"_c, "role_id"_c);

    return query_builder::execute_read_query<role_permission_entity, domain::role_permission>(
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
    static constexpr auto max = db_timestamp::max();
    const auto tid = ctx_.tenant_id().to_string();
    const auto query = query_builder::read<std::vector<role_permission_entity>> |
                       query_builder::where("tenant_id"_c == tid && "valid_to"_c == max) |
                       query_builder::order_by("role_id"_c, "role_id"_c) |
                       query_builder::offset(offset) | query_builder::limit(limit);

    return query_builder::execute_read_query<role_permission_entity, domain::role_permission>(
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

    static constexpr auto max = db_timestamp::max();
    const auto role_id_str = boost::uuids::to_string(role_id);
    const auto permission_id_str = boost::uuids::to_string(permission_id);
    const auto tid = ctx_.tenant_id().to_string();
    const auto query =
        query_builder::read<std::vector<role_permission_entity>> |
        query_builder::where("tenant_id"_c == tid && "role_id"_c == role_id_str &&
                             "permission_id"_c == permission_id_str && "valid_to"_c == max);

    return query_builder::execute_read_query<role_permission_entity, domain::role_permission>(
        ctx_,
        query,
        [](const auto& entities) { return role_permission_mapper::map(entities); },
        lg(),
        "Reading latest role permission by key.");
}

std::uint32_t role_permission_repository::get_total_role_permission_count() {
    BOOST_LOG_SEV(lg(), debug) << "Retrieving total active role permissions count";
    static constexpr auto max = db_timestamp::max();

    const auto tid = ctx_.tenant_id().to_string();
    const auto query = query_builder::read<std::vector<role_permission_entity>> |
                       query_builder::where("tenant_id"_c == tid && "valid_to"_c == max);

    const auto count = query_builder::execute_count_query<role_permission_entity>(
        ctx_, query, std::nullopt, lg(), "Counting role permissions");
    BOOST_LOG_SEV(lg(), debug) << "Total active role permissions count: " << count;
    return count;
}

std::vector<domain::role_permission>
role_permission_repository::read_latest_by_role(const boost::uuids::uuid& role_id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest role permissions. Role: " << role_id;

    static constexpr auto max = db_timestamp::max();
    const auto role_id_str = boost::uuids::to_string(role_id);
    const auto tid = ctx_.tenant_id().to_string();
    const auto query = query_builder::read<std::vector<role_permission_entity>> |
                       query_builder::where("tenant_id"_c == tid && "role_id"_c == role_id_str &&
                                            "valid_to"_c == max) |
                       query_builder::order_by("role_id"_c);

    auto rows = query_builder::execute_read_query<role_permission_entity, domain::role_permission>(
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

    static constexpr auto max = db_timestamp::max();
    const auto permission_id_str = boost::uuids::to_string(permission_id);
    const auto tid = ctx_.tenant_id().to_string();
    const auto query =
        query_builder::read<std::vector<role_permission_entity>> |
        query_builder::where("tenant_id"_c == tid && "permission_id"_c == permission_id_str &&
                             "valid_to"_c == max) |
        query_builder::order_by("role_id"_c);

    auto rows = query_builder::execute_read_query<role_permission_entity, domain::role_permission>(
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

    static constexpr auto max = db_timestamp::max();
    const auto tid = ctx_.tenant_id().to_string();
    const auto query = query_builder::read<std::vector<role_permission_entity>> |
                       query_builder::where("tenant_id"_c == tid && "role_id"_c == role_id_str &&
                                            "valid_to"_c == max) |
                       query_builder::order_by("role_id"_c) | query_builder::offset(offset) |
                       query_builder::limit(limit);

    auto rows = query_builder::execute_read_query<role_permission_entity, domain::role_permission>(
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
    static constexpr auto max = db_timestamp::max();

    const auto tid = ctx_.tenant_id().to_string();
    const auto query = query_builder::read<std::vector<role_permission_entity>> |
                       query_builder::where("tenant_id"_c == tid && "role_id"_c == role_id_str &&
                                            "valid_to"_c == max);

    const auto count = query_builder::execute_count_query<role_permission_entity>(
        ctx_, query, std::nullopt, lg(), "Counting role permissions");
    BOOST_LOG_SEV(lg(), debug) << "Total active role permissions count by role: " << count;
    return count;
}

std::vector<domain::role_permission> role_permission_repository::read_latest_by_permission(
    const boost::uuids::uuid& permission_id, std::uint32_t offset, std::uint32_t limit) {
    const auto permission_id_str = boost::uuids::to_string(permission_id);
    BOOST_LOG_SEV(lg(), debug) << "Reading latest role permissions. Permission: " << permission_id
                               << " offset: " << offset << " limit: " << limit;

    static constexpr auto max = db_timestamp::max();
    const auto tid = ctx_.tenant_id().to_string();
    const auto query =
        query_builder::read<std::vector<role_permission_entity>> |
        query_builder::where("tenant_id"_c == tid && "permission_id"_c == permission_id_str &&
                             "valid_to"_c == max) |
        query_builder::order_by("role_id"_c) | query_builder::offset(offset) |
        query_builder::limit(limit);

    auto rows = query_builder::execute_read_query<role_permission_entity, domain::role_permission>(
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
    static constexpr auto max = db_timestamp::max();

    const auto tid = ctx_.tenant_id().to_string();
    const auto query =
        query_builder::read<std::vector<role_permission_entity>> |
        query_builder::where("tenant_id"_c == tid && "permission_id"_c == permission_id_str &&
                             "valid_to"_c == max);

    const auto count = query_builder::execute_count_query<role_permission_entity>(
        ctx_, query, std::nullopt, lg(), "Counting role permissions");
    BOOST_LOG_SEV(lg(), debug) << "Total active role permissions count by permission: " << count;
    return count;
}

std::vector<role_permission_history_row>
role_permission_repository::read_all_by_role(const boost::uuids::uuid& role_id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading every version of the role permissions. Role: "
                               << role_id;

    const auto role_id_str = boost::uuids::to_string(role_id);
    const auto tid = ctx_.tenant_id().to_string();
    const auto query = query_builder::read<std::vector<role_permission_entity>> |
                       query_builder::where("tenant_id"_c == tid && "role_id"_c == role_id_str) |
                       query_builder::order_by("permission_id"_c, "valid_from"_c.desc());

    return query_builder::execute_read_query<role_permission_entity, role_permission_history_row>(
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

    static constexpr auto max = db_timestamp::max();
    // The row is named by its version as well as by its key, so the removal
    // cannot close a row that replaced the one the caller read between the
    // read above and this statement.
    const auto expected = version ? static_cast<int>(*version) : current.front().version;
    const auto role_id_str = boost::uuids::to_string(role_id);
    const auto permission_id_str = boost::uuids::to_string(permission_id);
    const auto tid = ctx_.tenant_id().to_string();
    const auto query = query_builder::delete_from<role_permission_entity> |
                       query_builder::where("tenant_id"_c == tid && "role_id"_c == role_id_str &&
                                            "permission_id"_c == permission_id_str &&
                                            "valid_to"_c == max && "version"_c == expected);

    query_builder::execute_delete_query(
        ctx_, query, lg(), "removing role permission from database");
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
    const auto query = query_builder::delete_from<role_permission_entity> |
                       query_builder::where("tenant_id"_c == tid && "role_id"_c == role_id_str);

    query_builder::execute_delete_query(
        ctx_, query, lg(), "removing all role permissions from database");
}


}
