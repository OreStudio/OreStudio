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

role_grant_request_role_repository::role_grant_request_role_repository(const context& ctx)
    : ctx_(ctx) {}

namespace {

/**
 * @brief The key of a link in the text form the store compares it in.
 */
std::pair<std::string, std::string>
role_grant_request_role_pair_of(const domain::role_grant_request_role& v) {
    return {boost::uuids::to_string(v.request_id), boost::uuids::to_string(v.role_id)};
}

/**
 * @brief The claim a replace makes, from the rows the key names now: the
 * version the row carries, or that no row exists yet.
 */
ores::utility::domain::precondition role_grant_request_role_replace_claim_of(
    const std::vector<domain::role_grant_request_role>& current) {
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
domain::role_grant_request_role
role_grant_request_role_apply_claim(const domain::role_grant_request_role& v,
                                    const ores::utility::domain::precondition& claim,
                                    const std::vector<domain::role_grant_request_role>& current) {
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
const std::vector<domain::role_grant_request_role>&
role_grant_request_role_rows_of(const role_grant_request_role_repository::rows_by_pair& current,
                                const domain::role_grant_request_role& v) {
    static const std::vector<domain::role_grant_request_role> none;
    const auto found = current.find(role_grant_request_role_pair_of(v));
    return found == current.end() ? none : found->second;
}

}

role_grant_request_role_repository::rows_by_pair role_grant_request_role_repository::read_current(
    const std::vector<domain::role_grant_request_role>& of) {
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
            auto pair = role_grant_request_role_pair_of(of[i]);
            left_set.insert(pair.first);
            right_set.insert(pair.second);
            requested.insert(std::move(pair));
        }
        const std::vector<std::string> lefts(left_set.begin(), left_set.end());
        const std::vector<std::string> rights(right_set.begin(), right_set.end());
        const auto tid = ctx_.tenant_id().to_string();
        const auto query = query_builder::read<std::vector<role_grant_request_role_entity>> |
                           query_builder::where("tenant_id"_c == tid && "request_id"_c.in(lefts) &&
                                                "role_id"_c.in(rights) && "valid_to"_c == max);
        auto rows = query_builder::execute_read_query<role_grant_request_role_entity,
                                                      domain::role_grant_request_role>(
            ctx_,
            query,
            [](const auto& entities) { return role_grant_request_role_mapper::map(entities); },
            lg(),
            "Reading current role grant request roles by key.");
        // The query matches every left with every right, so a row outside the
        // requested pairs is dropped here.
        for (auto& row : rows) {
            auto pair = role_grant_request_role_pair_of(row);
            if (requested.contains(pair))
                current[std::move(pair)].push_back(std::move(row));
        }
    }
    return current;
}

ores::utility::domain::precondition
role_grant_request_role_repository::replace_claim(const domain::role_grant_request_role& v) {
    return role_grant_request_role_replace_claim_of(read_latest(v.request_id, v.role_id));
}

void role_grant_request_role_repository::write(
    const domain::role_grant_request_role& role_grant_request_role) {
    write(role_grant_request_role, replace_claim(role_grant_request_role));
}

void role_grant_request_role_repository::write(
    const std::vector<domain::role_grant_request_role>& role_grant_request_roles) {
    // One read for the set, so a batch states its claims from one read rather
    // than one read per row.
    const auto current = read_current(role_grant_request_roles);
    std::vector<ores::utility::domain::precondition> claims;
    claims.reserve(role_grant_request_roles.size());
    for (const auto& item : role_grant_request_roles)
        claims.push_back(role_grant_request_role_replace_claim_of(
            role_grant_request_role_rows_of(current, item)));
    write(role_grant_request_roles, claims);
}

void role_grant_request_role_repository::write(
    const domain::role_grant_request_role& role_grant_request_role,
    const ores::utility::domain::precondition& claim) {
    ORES_LOG_SEV(lg(), debug) << "Writing role grant request role to database: "
                              << role_grant_request_role.request_id << "/"
                              << role_grant_request_role.role_id;
    const auto current =
        claim.kind == ores::utility::domain::precondition_kind::any ?
            read_latest(role_grant_request_role.request_id, role_grant_request_role.role_id) :
            std::vector<domain::role_grant_request_role>{};
    const auto t = role_grant_request_role_apply_claim(role_grant_request_role, claim, current);
    query_builder::execute_write_query(ctx_,
                                       role_grant_request_role_mapper::map(t),
                                       lg(),
                                       "writing role grant request role to database");
}

void role_grant_request_role_repository::write(
    const std::vector<domain::role_grant_request_role>& role_grant_request_roles,
    const std::vector<ores::utility::domain::precondition>& claims) {
    ORES_LOG_SEV(lg(), debug) << "Writing role grant request roles to database. Count: "
                              << role_grant_request_roles.size();
    if (claims.size() != role_grant_request_roles.size())
        throw std::invalid_argument(
            "role_grant_request_role_repository::write: one claim is needed for each row");
    std::vector<domain::role_grant_request_role> to_read;
    for (std::size_t i = 0; i < role_grant_request_roles.size(); ++i) {
        if (claims[i].kind == ores::utility::domain::precondition_kind::any)
            to_read.push_back(role_grant_request_roles[i]);
    }
    const auto current = read_current(to_read);
    std::vector<domain::role_grant_request_role> batch;
    batch.reserve(role_grant_request_roles.size());
    for (std::size_t i = 0; i < role_grant_request_roles.size(); ++i)
        batch.push_back(role_grant_request_role_apply_claim(
            role_grant_request_roles[i],
            claims[i],
            role_grant_request_role_rows_of(current, role_grant_request_roles[i])));
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
    ORES_LOG_SEV(lg(), debug) << "Reading latest role grant request roles with offset: " << offset
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
    ORES_LOG_SEV(lg(), debug) << "Reading latest role grant request role. " << request_id << "/"
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
    ORES_LOG_SEV(lg(), debug) << "Retrieving total active role grant request roles count";
    static constexpr auto max = db_timestamp::max();

    const auto tid = ctx_.tenant_id().to_string();
    const auto query = query_builder::read<std::vector<role_grant_request_role_entity>> |
                       query_builder::where("tenant_id"_c == tid && "valid_to"_c == max);

    const auto count = query_builder::execute_count_query<role_grant_request_role_entity>(
        ctx_, query, std::nullopt, lg(), "Counting role grant request roles");
    ORES_LOG_SEV(lg(), debug) << "Total active role grant request roles count: " << count;
    return count;
}

std::vector<domain::role_grant_request_role>
role_grant_request_role_repository::read_latest_by_request(const boost::uuids::uuid& request_id) {
    ORES_LOG_SEV(lg(), debug) << "Reading latest role grant request roles. Request: " << request_id;

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
    ORES_LOG_SEV(lg(), debug) << "Reading latest role grant request roles. Role: " << role_id;

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
    ORES_LOG_SEV(lg(), debug) << "Reading latest role grant request roles. Request: " << request_id
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
    ORES_LOG_SEV(lg(), debug) << "Retrieving total active role grant request roles count. Request: "
                              << request_id;
    static constexpr auto max = db_timestamp::max();

    const auto tid = ctx_.tenant_id().to_string();
    const auto query =
        query_builder::read<std::vector<role_grant_request_role_entity>> |
        query_builder::where("tenant_id"_c == tid && "request_id"_c == request_id_str &&
                             "valid_to"_c == max);

    const auto count = query_builder::execute_count_query<role_grant_request_role_entity>(
        ctx_, query, std::nullopt, lg(), "Counting role grant request roles");
    ORES_LOG_SEV(lg(), debug) << "Total active role grant request roles count by request: "
                              << count;
    return count;
}

std::uint32_t role_grant_request_role_repository::get_total_role_grant_request_role_count_by_role(
    const boost::uuids::uuid& role_id) {
    const auto role_id_str = boost::uuids::to_string(role_id);
    ORES_LOG_SEV(lg(), debug) << "Retrieving total active role grant request roles count. Role: "
                              << role_id;
    static constexpr auto max = db_timestamp::max();

    const auto tid = ctx_.tenant_id().to_string();
    const auto query = query_builder::read<std::vector<role_grant_request_role_entity>> |
                       query_builder::where("tenant_id"_c == tid && "role_id"_c == role_id_str &&
                                            "valid_to"_c == max);

    const auto count = query_builder::execute_count_query<role_grant_request_role_entity>(
        ctx_, query, std::nullopt, lg(), "Counting role grant request roles");
    ORES_LOG_SEV(lg(), debug) << "Total active role grant request roles count by role: " << count;
    return count;
}

std::vector<role_grant_request_role_history_row>
role_grant_request_role_repository::read_all_by_request(const boost::uuids::uuid& request_id) {
    ORES_LOG_SEV(lg(), debug) << "Reading every version of the role grant request roles. Request: "
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
    ORES_LOG_SEV(lg(), debug) << "Removing role grant request role from database: " << request_id
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
    ORES_LOG_SEV(lg(), debug) << "Removing all role grant request roles from database: "
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
