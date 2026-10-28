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
#include "ores.iam.core/repository/account_role_repository.hpp"
#include "ores.database/query_builder/execute.hpp"
#include "ores.database/repository/helpers.hpp"
#include "ores.database/repository/mapper_helpers.hpp"
#include "ores.iam.api/domain/account_role.hpp"
#include "ores.iam.api/domain/account_role_json_io.hpp" // IWYU pragma: keep.
#include "ores.iam.core/repository/account_role_entity.hpp"
#include "ores.iam.core/repository/account_role_mapper.hpp"
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

std::string account_role_repository::sql() {
    return query_builder::insert_statement_sql<account_role_entity>(lg());
}

account_role_repository::account_role_repository(context ctx)
    : ctx_(std::move(ctx)) {}

ores::utility::domain::precondition
account_role_repository::replace_claim(const domain::account_role& v) {
    const auto current = read_latest(v.account_id, v.role_id);
    if (current.empty())
        return {ores::utility::domain::precondition_kind::must_not_exist, std::nullopt};
    return {ores::utility::domain::precondition_kind::must_match_version,
            static_cast<std::uint32_t>(current.front().version)};
}

domain::account_role
account_role_repository::apply_claim(const domain::account_role& v,
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
            const auto current = read_latest(v.account_id, v.role_id);
            t.version = current.empty() ? 0 : current.front().version;
            break;
        }
    }
    return t;
}

void account_role_repository::write(const domain::account_role& account_role) {
    write(account_role, replace_claim(account_role));
}

void account_role_repository::write(const std::vector<domain::account_role>& account_roles) {
    std::vector<ores::utility::domain::precondition> claims;
    claims.reserve(account_roles.size());
    for (const auto& item : account_roles)
        claims.push_back(replace_claim(item));
    write(account_roles, claims);
}

void account_role_repository::write(const domain::account_role& account_role,
                                    const ores::utility::domain::precondition& claim) {
    BOOST_LOG_SEV(lg(), debug) << "Writing account role to database: " << account_role.account_id
                               << "/" << account_role.role_id;
    const auto t = apply_claim(account_role, claim);
    query_builder::execute_write_query(
        ctx_, account_role_mapper::map(t), lg(), "writing account role to database");
}

void account_role_repository::write(
    const std::vector<domain::account_role>& account_roles,
    const std::vector<ores::utility::domain::precondition>& claims) {
    BOOST_LOG_SEV(lg(), debug) << "Writing account roles to database. Count: "
                               << account_roles.size();
    std::vector<domain::account_role> batch;
    batch.reserve(account_roles.size());
    for (std::size_t i = 0; i < account_roles.size(); ++i)
        batch.push_back(apply_claim(account_roles[i], claims[i]));
    query_builder::execute_write_query(
        ctx_, account_role_mapper::map(batch), lg(), "writing account roles to database");
}

std::vector<domain::account_role> account_role_repository::read_latest() {
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx_.tenant_id().to_string();
    const auto query = query_builder::read<std::vector<account_role_entity>> |
                       query_builder::where("tenant_id"_c == tid && "valid_to"_c == max.value()) |
                       query_builder::order_by("account_id"_c, "role_id"_c);

    return query_builder::execute_read_query<account_role_entity, domain::account_role>(
        ctx_,
        query,
        [](const auto& entities) { return account_role_mapper::map(entities); },
        lg(),
        "Reading latest account roles");
}

std::vector<domain::account_role> account_role_repository::read_latest(std::uint32_t offset,
                                                                       std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest account roles with offset: " << offset
                               << " and limit: " << limit;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx_.tenant_id().to_string();
    const auto query = query_builder::read<std::vector<account_role_entity>> |
                       query_builder::where("tenant_id"_c == tid && "valid_to"_c == max.value()) |
                       query_builder::order_by("account_id"_c, "role_id"_c) |
                       query_builder::offset(offset) | query_builder::limit(limit);

    return query_builder::execute_read_query<account_role_entity, domain::account_role>(
        ctx_,
        query,
        [](const auto& entities) { return account_role_mapper::map(entities); },
        lg(),
        "Reading latest account roles (paginated).");
}

std::vector<domain::account_role>
account_role_repository::read_latest(const boost::uuids::uuid& account_id,
                                     const boost::uuids::uuid& role_id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest account role. " << account_id << "/" << role_id;

    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto account_id_str = boost::uuids::to_string(account_id);
    const auto role_id_str = boost::uuids::to_string(role_id);
    const auto tid = ctx_.tenant_id().to_string();
    const auto query =
        query_builder::read<std::vector<account_role_entity>> |
        query_builder::where("tenant_id"_c == tid && "account_id"_c == account_id_str &&
                             "role_id"_c == role_id_str && "valid_to"_c == max.value());

    return query_builder::execute_read_query<account_role_entity, domain::account_role>(
        ctx_,
        query,
        [](const auto& entities) { return account_role_mapper::map(entities); },
        lg(),
        "Reading latest account role by key.");
}

std::uint32_t account_role_repository::get_total_account_role_count() {
    BOOST_LOG_SEV(lg(), debug) << "Retrieving total active account roles count";
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));

    const auto tid = ctx_.tenant_id().to_string();
    const auto query = query_builder::read<std::vector<account_role_entity>> |
                       query_builder::where("tenant_id"_c == tid && "valid_to"_c == max.value());

    const auto count = query_builder::execute_count_query<account_role_entity>(
        ctx_, query, std::nullopt, lg(), "Counting account roles");
    BOOST_LOG_SEV(lg(), debug) << "Total active account roles count: " << count;
    return count;
}

std::vector<domain::account_role>
account_role_repository::read_latest_by_account(const boost::uuids::uuid& account_id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest account roles. Account: " << account_id;

    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto account_id_str = boost::uuids::to_string(account_id);
    const auto tid = ctx_.tenant_id().to_string();
    const auto query =
        query_builder::read<std::vector<account_role_entity>> |
        query_builder::where("tenant_id"_c == tid && "account_id"_c == account_id_str &&
                             "valid_to"_c == max.value()) |
        query_builder::order_by("role_id"_c);

    auto rows = query_builder::execute_read_query<account_role_entity, domain::account_role>(
        ctx_,
        query,
        [](const auto& entities) { return account_role_mapper::map(entities); },
        lg(),
        "Reading latest account roles by account.");

    return rows;
}

std::vector<domain::account_role>
account_role_repository::read_latest_by_role(const boost::uuids::uuid& role_id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest account roles. Role: " << role_id;

    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto role_id_str = boost::uuids::to_string(role_id);
    const auto tid = ctx_.tenant_id().to_string();
    const auto query = query_builder::read<std::vector<account_role_entity>> |
                       query_builder::where("tenant_id"_c == tid && "role_id"_c == role_id_str &&
                                            "valid_to"_c == max.value()) |
                       query_builder::order_by("account_id"_c);

    auto rows = query_builder::execute_read_query<account_role_entity, domain::account_role>(
        ctx_,
        query,
        [](const auto& entities) { return account_role_mapper::map(entities); },
        lg(),
        "Reading latest account roles by role.");

    return rows;
}

std::vector<domain::account_role> account_role_repository::read_latest_by_account(
    const boost::uuids::uuid& account_id, std::uint32_t offset, std::uint32_t limit) {
    const auto account_id_str = boost::uuids::to_string(account_id);
    BOOST_LOG_SEV(lg(), debug) << "Reading latest account roles. Account: " << account_id
                               << " offset: " << offset << " limit: " << limit;

    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx_.tenant_id().to_string();
    const auto query =
        query_builder::read<std::vector<account_role_entity>> |
        query_builder::where("tenant_id"_c == tid && "account_id"_c == account_id_str &&
                             "valid_to"_c == max.value()) |
        query_builder::order_by("role_id"_c) | query_builder::offset(offset) |
        query_builder::limit(limit);

    auto rows = query_builder::execute_read_query<account_role_entity, domain::account_role>(
        ctx_,
        query,
        [](const auto& entities) { return account_role_mapper::map(entities); },
        lg(),
        "Reading latest account roles by account (paginated).");

    return rows;
}

std::uint32_t account_role_repository::get_total_account_role_count_by_account(
    const boost::uuids::uuid& account_id) {
    const auto account_id_str = boost::uuids::to_string(account_id);
    BOOST_LOG_SEV(lg(), debug) << "Retrieving total active account roles count. Account: "
                               << account_id;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));

    const auto tid = ctx_.tenant_id().to_string();
    const auto query =
        query_builder::read<std::vector<account_role_entity>> |
        query_builder::where("tenant_id"_c == tid && "account_id"_c == account_id_str &&
                             "valid_to"_c == max.value());

    const auto count = query_builder::execute_count_query<account_role_entity>(
        ctx_, query, std::nullopt, lg(), "Counting account roles");
    BOOST_LOG_SEV(lg(), debug) << "Total active account roles count by account: " << count;
    return count;
}

std::vector<domain::account_role> account_role_repository::read_latest_by_role(
    const boost::uuids::uuid& role_id, std::uint32_t offset, std::uint32_t limit) {
    const auto role_id_str = boost::uuids::to_string(role_id);
    BOOST_LOG_SEV(lg(), debug) << "Reading latest account roles. Role: " << role_id
                               << " offset: " << offset << " limit: " << limit;

    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx_.tenant_id().to_string();
    const auto query = query_builder::read<std::vector<account_role_entity>> |
                       query_builder::where("tenant_id"_c == tid && "role_id"_c == role_id_str &&
                                            "valid_to"_c == max.value()) |
                       query_builder::order_by("account_id"_c) | query_builder::offset(offset) |
                       query_builder::limit(limit);

    auto rows = query_builder::execute_read_query<account_role_entity, domain::account_role>(
        ctx_,
        query,
        [](const auto& entities) { return account_role_mapper::map(entities); },
        lg(),
        "Reading latest account roles by role (paginated).");

    return rows;
}

std::uint32_t
account_role_repository::get_total_account_role_count_by_role(const boost::uuids::uuid& role_id) {
    const auto role_id_str = boost::uuids::to_string(role_id);
    BOOST_LOG_SEV(lg(), debug) << "Retrieving total active account roles count. Role: " << role_id;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));

    const auto tid = ctx_.tenant_id().to_string();
    const auto query = query_builder::read<std::vector<account_role_entity>> |
                       query_builder::where("tenant_id"_c == tid && "role_id"_c == role_id_str &&
                                            "valid_to"_c == max.value());

    const auto count = query_builder::execute_count_query<account_role_entity>(
        ctx_, query, std::nullopt, lg(), "Counting account roles");
    BOOST_LOG_SEV(lg(), debug) << "Total active account roles count by role: " << count;
    return count;
}

std::vector<account_role_history_row>
account_role_repository::read_all_by_account(const boost::uuids::uuid& account_id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading every version of the account roles. Account: "
                               << account_id;

    const auto account_id_str = boost::uuids::to_string(account_id);
    const auto tid = ctx_.tenant_id().to_string();
    const auto query =
        query_builder::read<std::vector<account_role_entity>> |
        query_builder::where("tenant_id"_c == tid && "account_id"_c == account_id_str) |
        query_builder::order_by("role_id"_c, "valid_from"_c.desc());

    return query_builder::execute_read_query<account_role_entity, account_role_history_row>(
        ctx_,
        query,
        [](const auto& entities) {
            std::vector<account_role_history_row> rows;
            rows.reserve(entities.size());
            for (const auto& entity : entities) {
                account_role_history_row row;
                row.link = account_role_mapper::map(entity);
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
        "Reading every version of the account roles by account.");
}

void account_role_repository::remove(const boost::uuids::uuid& account_id,
                                     const boost::uuids::uuid& role_id) {
    static_cast<void>(remove(account_id, role_id, std::nullopt));
}

account_role_repository::remove_status
account_role_repository::remove(const boost::uuids::uuid& account_id,
                                const boost::uuids::uuid& role_id,
                                std::optional<std::uint32_t> version) {
    BOOST_LOG_SEV(lg(), debug) << "Removing account role from database: " << account_id << "/"
                               << role_id;

    const auto current = read_latest(account_id, role_id);
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
    const auto account_id_str = boost::uuids::to_string(account_id);
    const auto role_id_str = boost::uuids::to_string(role_id);
    const auto tid = ctx_.tenant_id().to_string();
    const auto query =
        query_builder::delete_from<account_role_entity> |
        query_builder::where("tenant_id"_c == tid && "account_id"_c == account_id_str &&
                             "role_id"_c == role_id_str && "valid_to"_c == max.value() &&
                             "version"_c == expected);

    query_builder::execute_delete_query(ctx_, query, lg(), "removing account role from database");
    // The delete reports no affected-row count, so the row is read back: a row
    // still open after the statement means the store refused the removal, and
    // the caller hears "conflicting" rather than "removed".
    if (!read_latest(account_id, role_id).empty())
        return remove_status::conflicting;
    return remove_status::removed;
}

void account_role_repository::remove(const std::vector<boost::uuids::uuid>& account_ids,
                                     const std::vector<boost::uuids::uuid>& role_ids) {
    // A junction's key is the pair of columns, and a per-column .in() DELETE
    // would be a cross-product over-delete (rows outside the requested pairs),
    // so each pair is removed on its own.
    if (account_ids.size() != role_ids.size())
        throw std::invalid_argument(
            "account_role_repository::remove: key column vectors must be the same length");
    for (std::size_t i = 0; i < account_ids.size(); ++i)
        static_cast<void>(remove(account_ids[i], role_ids[i], std::nullopt));
}

void account_role_repository::remove_by_account(const boost::uuids::uuid& account_id) {
    BOOST_LOG_SEV(lg(), debug) << "Removing all account roles from database: " << account_id;

    const auto account_id_str = boost::uuids::to_string(account_id);
    const auto tid = ctx_.tenant_id().to_string();
    const auto query =
        query_builder::delete_from<account_role_entity> |
        query_builder::where("tenant_id"_c == tid && "account_id"_c == account_id_str);

    query_builder::execute_delete_query(
        ctx_, query, lg(), "removing all account roles from database");
}


}
