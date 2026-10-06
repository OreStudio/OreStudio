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
#include "ores.dq.core/repository/dataset_bundle_member_repository.hpp"
#include "ores.database/domain/tenant_aware_pool.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.database/repository/helpers.hpp"
#include "ores.dq.api/domain/dataset_bundle_member.hpp"
#include "ores.dq.api/domain/dataset_bundle_member_json_io.hpp" // IWYU pragma: keep.
#include "ores.dq.core/repository/dataset_bundle_member_entity.hpp"
#include "ores.dq.core/repository/dataset_bundle_member_mapper.hpp"
#include "ores.logging/boost_severity.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <boost/log/sources/severity_feature.hpp>
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

namespace ores::dq::repository {

using namespace sqlgen;
using namespace sqlgen::literals;
using namespace ores::logging;
using namespace ores::database::repository;

std::string dataset_bundle_member_repository::sql() {
    return generate_create_table_sql<dataset_bundle_member_entity>(lg());
}

dataset_bundle_member_repository::dataset_bundle_member_repository(context ctx)
    : ctx_(std::move(ctx)) {}

ores::utility::domain::precondition
dataset_bundle_member_repository::replace_claim(const domain::dataset_bundle_member& v) {
    const auto current = read_latest(v.bundle_code, v.dataset_code);
    if (current.empty())
        return {ores::utility::domain::precondition_kind::must_not_exist, std::nullopt};
    return {ores::utility::domain::precondition_kind::must_match_version,
            static_cast<std::uint32_t>(current.front().version)};
}

domain::dataset_bundle_member
dataset_bundle_member_repository::apply_claim(const domain::dataset_bundle_member& v,
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
            const auto current = read_latest(v.bundle_code, v.dataset_code);
            t.version = current.empty() ? 0 : current.front().version;
            break;
        }
    }
    return t;
}

void dataset_bundle_member_repository::write(const domain::dataset_bundle_member& member) {
    write(member, replace_claim(member));
}

void dataset_bundle_member_repository::write(
    const std::vector<domain::dataset_bundle_member>& members) {
    std::vector<ores::utility::domain::precondition> claims;
    claims.reserve(members.size());
    for (const auto& item : members)
        claims.push_back(replace_claim(item));
    write(members, claims);
}

void dataset_bundle_member_repository::write(const domain::dataset_bundle_member& member,
                                             const ores::utility::domain::precondition& claim) {
    BOOST_LOG_SEV(lg(), debug) << "Writing dataset bundle member to database: "
                               << member.bundle_code << "/" << member.dataset_code;
    const auto t = apply_claim(member, claim);
    execute_write_query(ctx_,
                        dataset_bundle_member_mapper::map(t),
                        lg(),
                        "writing dataset bundle member to database");
}

void dataset_bundle_member_repository::write(
    const std::vector<domain::dataset_bundle_member>& members,
    const std::vector<ores::utility::domain::precondition>& claims) {
    BOOST_LOG_SEV(lg(), debug) << "Writing dataset bundle members to database. Count: "
                               << members.size();
    std::vector<domain::dataset_bundle_member> batch;
    batch.reserve(members.size());
    for (std::size_t i = 0; i < members.size(); ++i)
        batch.push_back(apply_claim(members[i], claims[i]));
    execute_write_query(ctx_,
                        dataset_bundle_member_mapper::map(batch),
                        lg(),
                        "writing dataset bundle members to database");
}

std::vector<domain::dataset_bundle_member> dataset_bundle_member_repository::read_latest() {
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx_.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<dataset_bundle_member_entity>> |
                       where("tenant_id"_c == tid && "valid_to"_c == max.value()) |
                       order_by("bundle_code"_c, "display_order"_c);

    return execute_read_query<dataset_bundle_member_entity, domain::dataset_bundle_member>(
        ctx_,
        query,
        [](const auto& entities) { return dataset_bundle_member_mapper::map(entities); },
        lg(),
        "Reading latest dataset bundle members");
}

std::vector<domain::dataset_bundle_member>
dataset_bundle_member_repository::read_latest(std::uint32_t offset, std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest dataset bundle members with offset: " << offset
                               << " and limit: " << limit;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx_.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<dataset_bundle_member_entity>> |
                       where("tenant_id"_c == tid && "valid_to"_c == max.value()) |
                       order_by("bundle_code"_c, "display_order"_c) | sqlgen::offset(offset) |
                       sqlgen::limit(limit);

    return execute_read_query<dataset_bundle_member_entity, domain::dataset_bundle_member>(
        ctx_,
        query,
        [](const auto& entities) { return dataset_bundle_member_mapper::map(entities); },
        lg(),
        "Reading latest dataset bundle members (paginated).");
}

std::vector<domain::dataset_bundle_member>
dataset_bundle_member_repository::read_latest(const std::string& bundle_code,
                                              const std::string& dataset_code) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest dataset bundle member. " << bundle_code << "/"
                               << dataset_code;

    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto bundle_code_str = bundle_code;
    const auto dataset_code_str = dataset_code;
    const auto tid = ctx_.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<dataset_bundle_member_entity>> |
                       where("tenant_id"_c == tid && "bundle_code"_c == bundle_code &&
                             "dataset_code"_c == dataset_code && "valid_to"_c == max.value());

    return execute_read_query<dataset_bundle_member_entity, domain::dataset_bundle_member>(
        ctx_,
        query,
        [](const auto& entities) { return dataset_bundle_member_mapper::map(entities); },
        lg(),
        "Reading latest dataset bundle member by key.");
}

std::uint32_t dataset_bundle_member_repository::get_total_member_count() {
    BOOST_LOG_SEV(lg(), debug) << "Retrieving total active dataset bundle members count";
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));

    struct count_result {
        long long count;
    };

    const auto tid = ctx_.tenant_id().to_string();
    const auto query =
        sqlgen::select_from<dataset_bundle_member_entity>(sqlgen::count().as<"count">()) |
        where("tenant_id"_c == tid && "valid_to"_c == max.value()) | sqlgen::to<count_result>;

    const auto r = sqlgen::session(ctx_.connection_pool()).and_then(query);
    ensure_success(r, lg());

    const auto count = static_cast<std::uint32_t>(r->count);
    BOOST_LOG_SEV(lg(), debug) << "Total active dataset bundle members count: " << count;
    return count;
}

std::vector<domain::dataset_bundle_member>
dataset_bundle_member_repository::read_latest_by_bundle(const std::string& bundle_code) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest dataset bundle members. Bundle: " << bundle_code;

    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx_.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<dataset_bundle_member_entity>> |
                       where("tenant_id"_c == tid && "bundle_code"_c == bundle_code &&
                             "valid_to"_c == max.value()) |
                       order_by("display_order"_c);

    auto rows = execute_read_query<dataset_bundle_member_entity, domain::dataset_bundle_member>(
        ctx_,
        query,
        [](const auto& entities) { return dataset_bundle_member_mapper::map(entities); },
        lg(),
        "Reading latest dataset bundle members by bundle.");

    return rows;
}

std::vector<domain::dataset_bundle_member>
dataset_bundle_member_repository::read_latest_by_dataset(const std::string& dataset_code) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest dataset bundle members. Dataset: "
                               << dataset_code;

    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx_.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<dataset_bundle_member_entity>> |
                       where("tenant_id"_c == tid && "dataset_code"_c == dataset_code &&
                             "valid_to"_c == max.value()) |
                       order_by("bundle_code"_c);

    auto rows = execute_read_query<dataset_bundle_member_entity, domain::dataset_bundle_member>(
        ctx_,
        query,
        [](const auto& entities) { return dataset_bundle_member_mapper::map(entities); },
        lg(),
        "Reading latest dataset bundle members by dataset.");

    return rows;
}

std::vector<domain::dataset_bundle_member> dataset_bundle_member_repository::read_latest_by_bundle(
    const std::string& bundle_code, std::uint32_t offset, std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest dataset bundle members. Bundle: " << bundle_code
                               << " offset: " << offset << " limit: " << limit;

    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx_.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<dataset_bundle_member_entity>> |
                       where("tenant_id"_c == tid && "bundle_code"_c == bundle_code &&
                             "valid_to"_c == max.value()) |
                       order_by("display_order"_c) | sqlgen::offset(offset) | sqlgen::limit(limit);

    auto rows = execute_read_query<dataset_bundle_member_entity, domain::dataset_bundle_member>(
        ctx_,
        query,
        [](const auto& entities) { return dataset_bundle_member_mapper::map(entities); },
        lg(),
        "Reading latest dataset bundle members by bundle (paginated).");

    return rows;
}

std::uint32_t
dataset_bundle_member_repository::get_total_member_count_by_bundle(const std::string& bundle_code) {
    BOOST_LOG_SEV(lg(), debug) << "Retrieving total active dataset bundle members count. Bundle: "
                               << bundle_code;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));

    struct count_result {
        long long count;
    };

    const auto tid = ctx_.tenant_id().to_string();
    const auto query =
        sqlgen::select_from<dataset_bundle_member_entity>(sqlgen::count().as<"count">()) |
        where("tenant_id"_c == tid && "bundle_code"_c == bundle_code &&
              "valid_to"_c == max.value()) |
        sqlgen::to<count_result>;

    const auto r = sqlgen::session(ctx_.connection_pool()).and_then(query);
    ensure_success(r, lg());

    const auto count = static_cast<std::uint32_t>(r->count);
    BOOST_LOG_SEV(lg(), debug) << "Total active dataset bundle members count by bundle: " << count;
    return count;
}

std::uint32_t dataset_bundle_member_repository::get_total_member_count_by_dataset(
    const std::string& dataset_code) {
    BOOST_LOG_SEV(lg(), debug) << "Retrieving total active dataset bundle members count. Dataset: "
                               << dataset_code;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));

    struct count_result {
        long long count;
    };

    const auto tid = ctx_.tenant_id().to_string();
    const auto query =
        sqlgen::select_from<dataset_bundle_member_entity>(sqlgen::count().as<"count">()) |
        where("tenant_id"_c == tid && "dataset_code"_c == dataset_code &&
              "valid_to"_c == max.value()) |
        sqlgen::to<count_result>;

    const auto r = sqlgen::session(ctx_.connection_pool()).and_then(query);
    ensure_success(r, lg());

    const auto count = static_cast<std::uint32_t>(r->count);
    BOOST_LOG_SEV(lg(), debug) << "Total active dataset bundle members count by dataset: " << count;
    return count;
}

void dataset_bundle_member_repository::remove(const std::string& bundle_code,
                                              const std::string& dataset_code) {
    static_cast<void>(remove(bundle_code, dataset_code, std::nullopt));
}

dataset_bundle_member_repository::remove_status
dataset_bundle_member_repository::remove(const std::string& bundle_code,
                                         const std::string& dataset_code,
                                         std::optional<std::uint32_t> version) {
    BOOST_LOG_SEV(lg(), debug) << "Removing dataset bundle member from database: " << bundle_code
                               << "/" << dataset_code;

    const auto current = read_latest(bundle_code, dataset_code);
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
    const auto bundle_code_str = bundle_code;
    const auto dataset_code_str = dataset_code;
    const auto tid = ctx_.tenant_id().to_string();
    const auto query = sqlgen::delete_from<dataset_bundle_member_entity> |
                       where("tenant_id"_c == tid && "bundle_code"_c == bundle_code &&
                             "dataset_code"_c == dataset_code && "valid_to"_c == max.value() &&
                             "version"_c == expected);

    execute_delete_query(ctx_, query, lg(), "removing dataset bundle member from database");
    // The delete reports no affected-row count, so the row is read back: a row
    // still open after the statement means the store refused the removal, and
    // the caller hears "conflicting" rather than "removed".
    if (!read_latest(bundle_code, dataset_code).empty())
        return remove_status::conflicting;
    return remove_status::removed;
}

void dataset_bundle_member_repository::remove(const std::vector<std::string>& bundle_codes,
                                              const std::vector<std::string>& dataset_codes) {
    // A junction's key is the pair of columns, and a per-column .in() DELETE
    // would be a cross-product over-delete (rows outside the requested pairs),
    // so each pair is removed on its own.
    if (bundle_codes.size() != dataset_codes.size())
        throw std::invalid_argument(
            "dataset_bundle_member_repository::remove: key column vectors must be the same length");
    for (std::size_t i = 0; i < bundle_codes.size(); ++i)
        static_cast<void>(remove(bundle_codes[i], dataset_codes[i], std::nullopt));
}

void dataset_bundle_member_repository::remove_by_bundle(const std::string& bundle_code) {
    BOOST_LOG_SEV(lg(), debug) << "Removing all dataset bundle members from database: "
                               << bundle_code;

    const auto tid = ctx_.tenant_id().to_string();
    const auto query = sqlgen::delete_from<dataset_bundle_member_entity> |
                       where("tenant_id"_c == tid && "bundle_code"_c == bundle_code);

    execute_delete_query(ctx_, query, lg(), "removing all dataset bundle members from database");
}


}
