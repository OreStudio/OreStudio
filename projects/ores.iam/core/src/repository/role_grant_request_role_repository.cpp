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
#include "ores.iam.core/repository/role_grant_request_role_repository.hpp"
#include "ores.database/query_builder/execute.hpp"
#include "ores.database/repository/helpers.hpp"
#include "ores.database/repository/mapper_helpers.hpp"
#include "ores.iam.api/domain/role_grant_request_role.hpp"
#include "ores.iam.api/domain/role_grant_request_role_json_io.hpp" // IWYU pragma: keep.
#include "ores.iam.core/repository/role_grant_request_role_entity.hpp"
#include "ores.iam.core/repository/role_grant_request_role_mapper.hpp"
#include "ores.logging/boost_severity.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <boost/log/sources/severity_feature.hpp>
#include <boost/uuid/uuid.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <cstddef>
#include <cstdint>
#include <optional>
#include <stdexcept>
#include <string>
#include <utility>
#include <vector>

namespace ores::iam::repository {

namespace query_builder = ores::database::query_builder;
using namespace ores::database::query_builder::literals;
using namespace ores::logging;
using namespace ores::database::repository;

std::string role_grant_request_role_repository::sql() {
    return query_builder::insert_statement_sql<role_grant_request_role_entity>(lg());
}

role_grant_request_role_repository::role_grant_request_role_repository(context ctx)
    : ctx_(std::move(ctx)) {}

ores::utility::domain::precondition
role_grant_request_role_repository::replace_claim(const domain::role_grant_request_role& v) {
    const auto current = read_latest(v.request_id, v.role_id);
    if (current.empty())
        return {ores::utility::domain::precondition_kind::must_not_exist, std::nullopt};
    return {ores::utility::domain::precondition_kind::must_match_version,
            static_cast<std::uint32_t>(current.front().version)};
}

domain::role_grant_request_role
role_grant_request_role_repository::apply_claim(const domain::role_grant_request_role& v,
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
            const auto current = read_latest(v.request_id, v.role_id);
            t.version = current.empty() ? 0 : current.front().version;
            break;
        }
    }
    return t;
}

void role_grant_request_role_repository::write(
    const domain::role_grant_request_role& role_grant_request_role) {
    write(role_grant_request_role, replace_claim(role_grant_request_role));
}

void role_grant_request_role_repository::write(
    const std::vector<domain::role_grant_request_role>& role_grant_request_roles) {
    std::vector<ores::utility::domain::precondition> claims;
    claims.reserve(role_grant_request_roles.size());
    for (const auto& item : role_grant_request_roles)
        claims.push_back(replace_claim(item));
    write(role_grant_request_roles, claims);
}

void role_grant_request_role_repository::write(
    const domain::role_grant_request_role& role_grant_request_role,
    const ores::utility::domain::precondition& claim) {
    BOOST_LOG_SEV(lg(), debug) << "Writing role grant request role to database: "
                               << role_grant_request_role.request_id << "/"
                               << role_grant_request_role.role_id;
    const auto t = apply_claim(role_grant_request_role, claim);
    query_builder::execute_write_query(ctx_,
                                       role_grant_request_role_mapper::map(t),
                                       lg(),
                                       "writing role grant request role to database");
}

void role_grant_request_role_repository::write(
    const std::vector<domain::role_grant_request_role>& role_grant_request_roles,
    const std::vector<ores::utility::domain::precondition>& claims) {
    BOOST_LOG_SEV(lg(), debug) << "Writing role grant request roles to database. Count: "
                               << role_grant_request_roles.size();
    std::vector<domain::role_grant_request_role> batch;
    batch.reserve(role_grant_request_roles.size());
    for (std::size_t i = 0; i < role_grant_request_roles.size(); ++i)
        batch.push_back(apply_claim(role_grant_request_roles[i], claims[i]));
    query_builder::execute_write_query(ctx_,
                                       role_grant_request_role_mapper::map(batch),
                                       lg(),
                                       "writing role grant request roles to database");
}

std::vector<domain::role_grant_request_role> role_grant_request_role_repository::read_latest() {
    static constexpr auto max = db_timestamp::max();
    const auto tid = ctx_.tenant_id().to_string();
    const auto query = query_builder::read<std::vector<role_grant_request_role_entity>> |
                       query_builder::where("tenant_id"_c == tid && "valid_to"_c == max) |
                       query_builder::order_by("request_id"_c, "role_id"_c);

    return query_builder::execute_read_query<role_grant_request_role_entity,
                                             domain::role_grant_request_role>(
        ctx_,
        query,
        [](const auto& entities) { return role_grant_request_role_mapper::map(entities); },
        lg(),
        "Reading latest role grant request roles");
}

std::vector<domain::role_grant_request_role>
role_grant_request_role_repository::read_latest(std::uint32_t offset, std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest role grant request roles with offset: " << offset
                               << " and limit: " << limit;
    static constexpr auto max = db_timestamp::max();
    const auto tid = ctx_.tenant_id().to_string();
    const auto query = query_builder::read<std::vector<role_grant_request_role_entity>> |
                       query_builder::where("tenant_id"_c == tid && "valid_to"_c == max) |
                       query_builder::order_by("request_id"_c, "role_id"_c) |
                       query_builder::offset(offset) | query_builder::limit(limit);

    return query_builder::execute_read_query<role_grant_request_role_entity,
                                             domain::role_grant_request_role>(
        ctx_,
        query,
        [](const auto& entities) { return role_grant_request_role_mapper::map(entities); },
        lg(),
        "Reading latest role grant request roles (paginated).");
}

std::vector<domain::role_grant_request_role>
role_grant_request_role_repository::read_latest(const boost::uuids::uuid& request_id,
                                                const boost::uuids::uuid& role_id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest role grant request role. " << request_id << "/"
                               << role_id;

    static constexpr auto max = db_timestamp::max();
    const auto request_id_str = boost::uuids::to_string(request_id);
    const auto role_id_str = boost::uuids::to_string(role_id);
    const auto tid = ctx_.tenant_id().to_string();
    const auto query =
        query_builder::read<std::vector<role_grant_request_role_entity>> |
        query_builder::where("tenant_id"_c == tid && "request_id"_c == request_id_str &&
                             "role_id"_c == role_id_str && "valid_to"_c == max);

    return query_builder::execute_read_query<role_grant_request_role_entity,
                                             domain::role_grant_request_role>(
        ctx_,
        query,
        [](const auto& entities) { return role_grant_request_role_mapper::map(entities); },
        lg(),
        "Reading latest role grant request role by key.");
}

std::uint32_t role_grant_request_role_repository::get_total_role_grant_request_role_count() {
    BOOST_LOG_SEV(lg(), debug) << "Retrieving total active role grant request roles count";
    static constexpr auto max = db_timestamp::max();

    const auto tid = ctx_.tenant_id().to_string();
    const auto query = query_builder::read<std::vector<role_grant_request_role_entity>> |
                       query_builder::where("tenant_id"_c == tid && "valid_to"_c == max);

    const auto count = query_builder::execute_count_query<role_grant_request_role_entity>(
        ctx_, query, std::nullopt, lg(), "Counting role grant request roles");
    BOOST_LOG_SEV(lg(), debug) << "Total active role grant request roles count: " << count;
    return count;
}

std::vector<domain::role_grant_request_role>
role_grant_request_role_repository::read_latest_by_request(const boost::uuids::uuid& request_id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest role grant request roles. Request: "
                               << request_id;

    static constexpr auto max = db_timestamp::max();
    const auto request_id_str = boost::uuids::to_string(request_id);
    const auto tid = ctx_.tenant_id().to_string();
    const auto query =
        query_builder::read<std::vector<role_grant_request_role_entity>> |
        query_builder::where("tenant_id"_c == tid && "request_id"_c == request_id_str &&
                             "valid_to"_c == max) |
        query_builder::order_by("role_id"_c);

    auto rows = query_builder::execute_read_query<role_grant_request_role_entity,
                                                  domain::role_grant_request_role>(
        ctx_,
        query,
        [](const auto& entities) { return role_grant_request_role_mapper::map(entities); },
        lg(),
        "Reading latest role grant request roles by request.");

    return rows;
}

std::vector<domain::role_grant_request_role>
role_grant_request_role_repository::read_latest_by_role(const boost::uuids::uuid& role_id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest role grant request roles. Role: " << role_id;

    static constexpr auto max = db_timestamp::max();
    const auto role_id_str = boost::uuids::to_string(role_id);
    const auto tid = ctx_.tenant_id().to_string();
    const auto query = query_builder::read<std::vector<role_grant_request_role_entity>> |
                       query_builder::where("tenant_id"_c == tid && "role_id"_c == role_id_str &&
                                            "valid_to"_c == max) |
                       query_builder::order_by("request_id"_c);

    auto rows = query_builder::execute_read_query<role_grant_request_role_entity,
                                                  domain::role_grant_request_role>(
        ctx_,
        query,
        [](const auto& entities) { return role_grant_request_role_mapper::map(entities); },
        lg(),
        "Reading latest role grant request roles by role.");

    return rows;
}

std::vector<domain::role_grant_request_role>
role_grant_request_role_repository::read_latest_by_request(const boost::uuids::uuid& request_id,
                                                           std::uint32_t offset,
                                                           std::uint32_t limit) {
    const auto request_id_str = boost::uuids::to_string(request_id);
    BOOST_LOG_SEV(lg(), debug) << "Reading latest role grant request roles. Request: " << request_id
                               << " offset: " << offset << " limit: " << limit;

    static constexpr auto max = db_timestamp::max();
    const auto tid = ctx_.tenant_id().to_string();
    const auto query =
        query_builder::read<std::vector<role_grant_request_role_entity>> |
        query_builder::where("tenant_id"_c == tid && "request_id"_c == request_id_str &&
                             "valid_to"_c == max) |
        query_builder::order_by("role_id"_c) | query_builder::offset(offset) |
        query_builder::limit(limit);

    auto rows = query_builder::execute_read_query<role_grant_request_role_entity,
                                                  domain::role_grant_request_role>(
        ctx_,
        query,
        [](const auto& entities) { return role_grant_request_role_mapper::map(entities); },
        lg(),
        "Reading latest role grant request roles by request (paginated).");

    return rows;
}

std::uint32_t
role_grant_request_role_repository::get_total_role_grant_request_role_count_by_request(
    const boost::uuids::uuid& request_id) {
    const auto request_id_str = boost::uuids::to_string(request_id);
    BOOST_LOG_SEV(lg(), debug)
        << "Retrieving total active role grant request roles count. Request: " << request_id;
    static constexpr auto max = db_timestamp::max();

    const auto tid = ctx_.tenant_id().to_string();
    const auto query =
        query_builder::read<std::vector<role_grant_request_role_entity>> |
        query_builder::where("tenant_id"_c == tid && "request_id"_c == request_id_str &&
                             "valid_to"_c == max);

    const auto count = query_builder::execute_count_query<role_grant_request_role_entity>(
        ctx_, query, std::nullopt, lg(), "Counting role grant request roles");
    BOOST_LOG_SEV(lg(), debug) << "Total active role grant request roles count by request: "
                               << count;
    return count;
}

std::uint32_t role_grant_request_role_repository::get_total_role_grant_request_role_count_by_role(
    const boost::uuids::uuid& role_id) {
    const auto role_id_str = boost::uuids::to_string(role_id);
    BOOST_LOG_SEV(lg(), debug) << "Retrieving total active role grant request roles count. Role: "
                               << role_id;
    static constexpr auto max = db_timestamp::max();

    const auto tid = ctx_.tenant_id().to_string();
    const auto query = query_builder::read<std::vector<role_grant_request_role_entity>> |
                       query_builder::where("tenant_id"_c == tid && "role_id"_c == role_id_str &&
                                            "valid_to"_c == max);

    const auto count = query_builder::execute_count_query<role_grant_request_role_entity>(
        ctx_, query, std::nullopt, lg(), "Counting role grant request roles");
    BOOST_LOG_SEV(lg(), debug) << "Total active role grant request roles count by role: " << count;
    return count;
}

std::vector<role_grant_request_role_history_row>
role_grant_request_role_repository::read_all_by_request(const boost::uuids::uuid& request_id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading every version of the role grant request roles. Request: "
                               << request_id;

    const auto request_id_str = boost::uuids::to_string(request_id);
    const auto tid = ctx_.tenant_id().to_string();
    const auto query =
        query_builder::read<std::vector<role_grant_request_role_entity>> |
        query_builder::where("tenant_id"_c == tid && "request_id"_c == request_id_str) |
        query_builder::order_by("role_id"_c, "valid_from"_c.desc());

    return query_builder::execute_read_query<role_grant_request_role_entity,
                                             role_grant_request_role_history_row>(
        ctx_,
        query,
        [](const auto& entities) {
            std::vector<role_grant_request_role_history_row> rows;
            rows.reserve(entities.size());
            for (const auto& entity : entities) {
                role_grant_request_role_history_row row;
                row.link = role_grant_request_role_mapper::map(entity);
                row.modified_by = row.link.modified_by;
                row.recorded_at = row.link.recorded_at;
                // A version ends when a later version replaces it, which is not
                // a closing, or when the link is closed. The rows of one link run
                // newest first, so the row before this one, when it is the same
                // link, is the next newer version, and it replaced this one only
                // if it carries the next version number. A link opened again
                // after a closing starts its numbering over.
                const bool replaced = !rows.empty() &&
                                      rows.back().link.role_id == row.link.role_id &&
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
        "Reading every version of the role grant request roles by request.");
}

void role_grant_request_role_repository::remove(const boost::uuids::uuid& request_id,
                                                const boost::uuids::uuid& role_id) {
    static_cast<void>(remove(request_id, role_id, std::nullopt));
}

role_grant_request_role_repository::remove_status
role_grant_request_role_repository::remove(const boost::uuids::uuid& request_id,
                                           const boost::uuids::uuid& role_id,
                                           std::optional<std::uint32_t> version) {
    BOOST_LOG_SEV(lg(), debug) << "Removing role grant request role from database: " << request_id
                               << "/" << role_id;

    const auto current = read_latest(request_id, role_id);
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
    const auto request_id_str = boost::uuids::to_string(request_id);
    const auto role_id_str = boost::uuids::to_string(role_id);
    const auto tid = ctx_.tenant_id().to_string();
    const auto query =
        query_builder::delete_from<role_grant_request_role_entity> |
        query_builder::where("tenant_id"_c == tid && "request_id"_c == request_id_str &&
                             "role_id"_c == role_id_str && "valid_to"_c == max &&
                             "version"_c == expected);

    query_builder::execute_delete_query(
        ctx_, query, lg(), "removing role grant request role from database");
    // The delete reports no affected-row count, so the row is read back: a row
    // still open after the statement means the store refused the removal, and
    // the caller hears "conflicting" rather than "removed".
    if (!read_latest(request_id, role_id).empty())
        return remove_status::conflicting;
    return remove_status::removed;
}

void role_grant_request_role_repository::remove(const std::vector<boost::uuids::uuid>& request_ids,
                                                const std::vector<boost::uuids::uuid>& role_ids) {
    // A junction's key is the pair of columns, and a per-column .in() DELETE
    // would be a cross-product over-delete (rows outside the requested pairs),
    // so each pair is removed on its own.
    if (request_ids.size() != role_ids.size())
        throw std::invalid_argument("role_grant_request_role_repository::remove: key column "
                                    "vectors must be the same length");
    for (std::size_t i = 0; i < request_ids.size(); ++i)
        static_cast<void>(remove(request_ids[i], role_ids[i], std::nullopt));
}

void role_grant_request_role_repository::remove_by_request(const boost::uuids::uuid& request_id) {
    BOOST_LOG_SEV(lg(), debug) << "Removing all role grant request roles from database: "
                               << request_id;

    const auto request_id_str = boost::uuids::to_string(request_id);
    const auto tid = ctx_.tenant_id().to_string();
    const auto query =
        query_builder::delete_from<role_grant_request_role_entity> |
        query_builder::where("tenant_id"_c == tid && "request_id"_c == request_id_str);

    query_builder::execute_delete_query(
        ctx_, query, lg(), "removing all role grant request roles from database");
}


}
