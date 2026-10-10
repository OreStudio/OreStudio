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
#include "ores.inbox.core/repository/approval_request_part_repository.hpp"
#include "ores.database/domain/tenant_aware_pool.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.database/repository/helpers.hpp"
#include "ores.inbox.api/domain/approval_request_part.hpp"
#include "ores.inbox.api/domain/approval_request_part_json_io.hpp" // IWYU pragma: keep.
#include "ores.inbox.core/repository/approval_request_part_entity.hpp"
#include "ores.inbox.core/repository/approval_request_part_mapper.hpp"
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

namespace ores::inbox::repository {

using namespace sqlgen;
using namespace sqlgen::literals;
using namespace ores::logging;
using namespace ores::database::repository;

std::string approval_request_part_repository::sql() {
    return generate_create_table_sql<approval_request_part_entity>(lg());
}

approval_request_part_repository::approval_request_part_repository(context ctx)
    : ctx_(std::move(ctx)) {}

ores::utility::domain::precondition
approval_request_part_repository::replace_claim(const domain::approval_request_part& v) {
    const auto current = read_latest(v.request_id, v.part_code);
    if (current.empty())
        return {ores::utility::domain::precondition_kind::must_not_exist, std::nullopt};
    return {ores::utility::domain::precondition_kind::must_match_version,
            static_cast<std::uint32_t>(current.front().version)};
}

domain::approval_request_part
approval_request_part_repository::apply_claim(const domain::approval_request_part& v,
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
            const auto current = read_latest(v.request_id, v.part_code);
            t.version = current.empty() ? 0 : current.front().version;
            break;
        }
    }
    return t;
}

void approval_request_part_repository::write(
    const domain::approval_request_part& approval_request_part) {
    write(approval_request_part, replace_claim(approval_request_part));
}

void approval_request_part_repository::write(
    const std::vector<domain::approval_request_part>& approval_request_parts) {
    std::vector<ores::utility::domain::precondition> claims;
    claims.reserve(approval_request_parts.size());
    for (const auto& item : approval_request_parts)
        claims.push_back(replace_claim(item));
    write(approval_request_parts, claims);
}

void approval_request_part_repository::write(
    const domain::approval_request_part& approval_request_part,
    const ores::utility::domain::precondition& claim) {
    BOOST_LOG_SEV(lg(), debug) << "Writing approval request part to database: "
                               << approval_request_part.request_id << "/"
                               << approval_request_part.part_code;
    const auto t = apply_claim(approval_request_part, claim);
    execute_write_query(ctx_,
                        approval_request_part_mapper::map(t),
                        lg(),
                        "writing approval request part to database");
}

void approval_request_part_repository::write(
    const std::vector<domain::approval_request_part>& approval_request_parts,
    const std::vector<ores::utility::domain::precondition>& claims) {
    BOOST_LOG_SEV(lg(), debug) << "Writing approval request parts to database. Count: "
                               << approval_request_parts.size();
    std::vector<domain::approval_request_part> batch;
    batch.reserve(approval_request_parts.size());
    for (std::size_t i = 0; i < approval_request_parts.size(); ++i)
        batch.push_back(apply_claim(approval_request_parts[i], claims[i]));
    execute_write_query(ctx_,
                        approval_request_part_mapper::map(batch),
                        lg(),
                        "writing approval request parts to database");
}

std::vector<domain::approval_request_part> approval_request_part_repository::read_latest() {
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx_.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<approval_request_part_entity>> |
                       where("tenant_id"_c == tid && "valid_to"_c == max.value()) |
                       order_by("request_id"_c, "part_code"_c);

    return execute_read_query<approval_request_part_entity, domain::approval_request_part>(
        ctx_,
        query,
        [](const auto& entities) { return approval_request_part_mapper::map(entities); },
        lg(),
        "Reading latest approval request parts");
}

std::vector<domain::approval_request_part>
approval_request_part_repository::read_latest(std::uint32_t offset, std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest approval request parts with offset: " << offset
                               << " and limit: " << limit;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx_.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<approval_request_part_entity>> |
                       where("tenant_id"_c == tid && "valid_to"_c == max.value()) |
                       order_by("request_id"_c, "part_code"_c) | sqlgen::offset(offset) |
                       sqlgen::limit(limit);

    return execute_read_query<approval_request_part_entity, domain::approval_request_part>(
        ctx_,
        query,
        [](const auto& entities) { return approval_request_part_mapper::map(entities); },
        lg(),
        "Reading latest approval request parts (paginated).");
}

std::vector<domain::approval_request_part>
approval_request_part_repository::read_latest(const boost::uuids::uuid& request_id,
                                              const std::string& part_code) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest approval request part. " << request_id << "/"
                               << part_code;

    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto request_id_str = boost::uuids::to_string(request_id);
    const auto part_code_str = part_code;
    const auto tid = ctx_.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<approval_request_part_entity>> |
                       where("tenant_id"_c == tid && "request_id"_c == request_id_str &&
                             "part_code"_c == part_code && "valid_to"_c == max.value());

    return execute_read_query<approval_request_part_entity, domain::approval_request_part>(
        ctx_,
        query,
        [](const auto& entities) { return approval_request_part_mapper::map(entities); },
        lg(),
        "Reading latest approval request part by key.");
}

std::uint32_t approval_request_part_repository::get_total_approval_request_part_count() {
    BOOST_LOG_SEV(lg(), debug) << "Retrieving total active approval request parts count";
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));

    struct count_result {
        long long count;
    };

    const auto tid = ctx_.tenant_id().to_string();
    const auto query =
        sqlgen::select_from<approval_request_part_entity>(sqlgen::count().as<"count">()) |
        where("tenant_id"_c == tid && "valid_to"_c == max.value()) | sqlgen::to<count_result>;

    const auto r = sqlgen::session(ctx_.connection_pool()).and_then(query);
    ensure_success(r, lg());

    const auto count = static_cast<std::uint32_t>(r->count);
    BOOST_LOG_SEV(lg(), debug) << "Total active approval request parts count: " << count;
    return count;
}

std::vector<domain::approval_request_part>
approval_request_part_repository::read_latest_by_request(const boost::uuids::uuid& request_id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest approval request parts. Request: " << request_id;

    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto request_id_str = boost::uuids::to_string(request_id);
    const auto tid = ctx_.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<approval_request_part_entity>> |
                       where("tenant_id"_c == tid && "request_id"_c == request_id_str &&
                             "valid_to"_c == max.value()) |
                       order_by("part_code"_c);

    auto rows = execute_read_query<approval_request_part_entity, domain::approval_request_part>(
        ctx_,
        query,
        [](const auto& entities) { return approval_request_part_mapper::map(entities); },
        lg(),
        "Reading latest approval request parts by request.");

    return rows;
}

std::vector<domain::approval_request_part>
approval_request_part_repository::read_latest_by_part(const std::string& part_code) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest approval request parts. Part: " << part_code;

    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx_.tenant_id().to_string();
    const auto query =
        sqlgen::read<std::vector<approval_request_part_entity>> |
        where("tenant_id"_c == tid && "part_code"_c == part_code && "valid_to"_c == max.value()) |
        order_by("request_id"_c);

    auto rows = execute_read_query<approval_request_part_entity, domain::approval_request_part>(
        ctx_,
        query,
        [](const auto& entities) { return approval_request_part_mapper::map(entities); },
        lg(),
        "Reading latest approval request parts by part.");

    return rows;
}

std::vector<domain::approval_request_part> approval_request_part_repository::read_latest_by_request(
    const boost::uuids::uuid& request_id, std::uint32_t offset, std::uint32_t limit) {
    const auto request_id_str = boost::uuids::to_string(request_id);
    BOOST_LOG_SEV(lg(), debug) << "Reading latest approval request parts. Request: " << request_id
                               << " offset: " << offset << " limit: " << limit;

    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx_.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<approval_request_part_entity>> |
                       where("tenant_id"_c == tid && "request_id"_c == request_id_str &&
                             "valid_to"_c == max.value()) |
                       order_by("part_code"_c) | sqlgen::offset(offset) | sqlgen::limit(limit);

    auto rows = execute_read_query<approval_request_part_entity, domain::approval_request_part>(
        ctx_,
        query,
        [](const auto& entities) { return approval_request_part_mapper::map(entities); },
        lg(),
        "Reading latest approval request parts by request (paginated).");

    return rows;
}

std::uint32_t approval_request_part_repository::get_total_approval_request_part_count_by_request(
    const boost::uuids::uuid& request_id) {
    const auto request_id_str = boost::uuids::to_string(request_id);
    BOOST_LOG_SEV(lg(), debug) << "Retrieving total active approval request parts count. Request: "
                               << request_id;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));

    struct count_result {
        long long count;
    };

    const auto tid = ctx_.tenant_id().to_string();
    const auto query =
        sqlgen::select_from<approval_request_part_entity>(sqlgen::count().as<"count">()) |
        where("tenant_id"_c == tid && "request_id"_c == request_id_str &&
              "valid_to"_c == max.value()) |
        sqlgen::to<count_result>;

    const auto r = sqlgen::session(ctx_.connection_pool()).and_then(query);
    ensure_success(r, lg());

    const auto count = static_cast<std::uint32_t>(r->count);
    BOOST_LOG_SEV(lg(), debug) << "Total active approval request parts count by request: " << count;
    return count;
}

std::uint32_t approval_request_part_repository::get_total_approval_request_part_count_by_part(
    const std::string& part_code) {
    BOOST_LOG_SEV(lg(), debug) << "Retrieving total active approval request parts count. Part: "
                               << part_code;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));

    struct count_result {
        long long count;
    };

    const auto tid = ctx_.tenant_id().to_string();
    const auto query =
        sqlgen::select_from<approval_request_part_entity>(sqlgen::count().as<"count">()) |
        where("tenant_id"_c == tid && "part_code"_c == part_code && "valid_to"_c == max.value()) |
        sqlgen::to<count_result>;

    const auto r = sqlgen::session(ctx_.connection_pool()).and_then(query);
    ensure_success(r, lg());

    const auto count = static_cast<std::uint32_t>(r->count);
    BOOST_LOG_SEV(lg(), debug) << "Total active approval request parts count by part: " << count;
    return count;
}

void approval_request_part_repository::remove(const boost::uuids::uuid& request_id,
                                              const std::string& part_code) {
    static_cast<void>(remove(request_id, part_code, std::nullopt));
}

approval_request_part_repository::remove_status
approval_request_part_repository::remove(const boost::uuids::uuid& request_id,
                                         const std::string& part_code,
                                         std::optional<std::uint32_t> version) {
    BOOST_LOG_SEV(lg(), debug) << "Removing approval request part from database: " << request_id
                               << "/" << part_code;

    const auto current = read_latest(request_id, part_code);
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
    const auto request_id_str = boost::uuids::to_string(request_id);
    const auto part_code_str = part_code;
    const auto tid = ctx_.tenant_id().to_string();
    const auto query =
        sqlgen::delete_from<approval_request_part_entity> |
        where("tenant_id"_c == tid && "request_id"_c == request_id_str &&
              "part_code"_c == part_code && "valid_to"_c == max.value() && "version"_c == expected);

    execute_delete_query(ctx_, query, lg(), "removing approval request part from database");
    // The delete reports no affected-row count, so the row is read back: a row
    // still open after the statement means the store refused the removal, and
    // the caller hears "conflicting" rather than "removed".
    if (!read_latest(request_id, part_code).empty())
        return remove_status::conflicting;
    return remove_status::removed;
}

void approval_request_part_repository::remove(const std::vector<boost::uuids::uuid>& request_ids,
                                              const std::vector<std::string>& part_codes) {
    // A junction's key is the pair of columns, and a per-column .in() DELETE
    // would be a cross-product over-delete (rows outside the requested pairs),
    // so each pair is removed on its own.
    if (request_ids.size() != part_codes.size())
        throw std::invalid_argument(
            "approval_request_part_repository::remove: key column vectors must be the same length");
    for (std::size_t i = 0; i < request_ids.size(); ++i)
        static_cast<void>(remove(request_ids[i], part_codes[i], std::nullopt));
}

void approval_request_part_repository::remove_by_request(const boost::uuids::uuid& request_id) {
    BOOST_LOG_SEV(lg(), debug) << "Removing all approval request parts from database: "
                               << request_id;

    const auto request_id_str = boost::uuids::to_string(request_id);
    const auto tid = ctx_.tenant_id().to_string();
    const auto query = sqlgen::delete_from<approval_request_part_entity> |
                       where("tenant_id"_c == tid && "request_id"_c == request_id_str);

    execute_delete_query(ctx_, query, lg(), "removing all approval request parts from database");
}


}
