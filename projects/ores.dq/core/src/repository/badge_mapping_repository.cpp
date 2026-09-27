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
#include "ores.dq.core/repository/badge_mapping_repository.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.database/repository/helpers.hpp"
#include "ores.dq.api/domain/badge_mapping_json_io.hpp" // IWYU pragma: keep.
#include "ores.dq.core/repository/badge_mapping_entity.hpp"
#include "ores.dq.core/repository/badge_mapping_mapper.hpp"
#include <cstddef>
#include <optional>
#include <sqlgen/postgres.hpp>
#include <stdexcept>

namespace ores::dq::repository {

using namespace sqlgen;
using namespace sqlgen::literals;
using namespace ores::logging;
using namespace ores::database::repository;

std::string badge_mapping_repository::sql() {
    return generate_create_table_sql<badge_mapping_entity>(lg());
}

badge_mapping_repository::badge_mapping_repository(context ctx)
    : ctx_(std::move(ctx)) {}

ores::utility::domain::precondition
badge_mapping_repository::replace_claim(const domain::badge_mapping& v) {
    const auto current = read_latest(v.code_domain_code, v.entity_code);
    if (current.empty())
        return {ores::utility::domain::precondition_kind::must_not_exist, std::nullopt};
    return {ores::utility::domain::precondition_kind::must_match_version,
            static_cast<std::uint32_t>(current.front().version)};
}

domain::badge_mapping
badge_mapping_repository::apply_claim(const domain::badge_mapping& v,
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
            const auto current = read_latest(v.code_domain_code, v.entity_code);
            t.version = current.empty() ? 0 : current.front().version;
            break;
        }
    }
    return t;
}

void badge_mapping_repository::write(const domain::badge_mapping& mapping) {
    write(mapping, replace_claim(mapping));
}

void badge_mapping_repository::write(const std::vector<domain::badge_mapping>& mappings) {
    std::vector<ores::utility::domain::precondition> claims;
    claims.reserve(mappings.size());
    for (const auto& item : mappings)
        claims.push_back(replace_claim(item));
    write(mappings, claims);
}

void badge_mapping_repository::write(const domain::badge_mapping& mapping,
                                     const ores::utility::domain::precondition& claim) {
    BOOST_LOG_SEV(lg(), debug) << "Writing badge mapping to database: " << mapping.code_domain_code
                               << "/" << mapping.entity_code;
    const auto t = apply_claim(mapping, claim);
    execute_write_query(
        ctx_, badge_mapping_mapper::map(t), lg(), "writing badge mapping to database");
}

void badge_mapping_repository::write(
    const std::vector<domain::badge_mapping>& mappings,
    const std::vector<ores::utility::domain::precondition>& claims) {
    BOOST_LOG_SEV(lg(), debug) << "Writing badge mappings to database. Count: " << mappings.size();
    std::vector<domain::badge_mapping> batch;
    batch.reserve(mappings.size());
    for (std::size_t i = 0; i < mappings.size(); ++i)
        batch.push_back(apply_claim(mappings[i], claims[i]));
    execute_write_query(
        ctx_, badge_mapping_mapper::map(batch), lg(), "writing badge mappings to database");
}

std::vector<domain::badge_mapping> badge_mapping_repository::read_latest() {
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx_.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<badge_mapping_entity>> |
                       where("tenant_id"_c == tid && "valid_to"_c == max.value()) |
                       order_by("code_domain_code"_c, "entity_code"_c);

    return execute_read_query<badge_mapping_entity, domain::badge_mapping>(
        ctx_,
        query,
        [](const auto& entities) { return badge_mapping_mapper::map(entities); },
        lg(),
        "Reading latest badge mappings");
}

std::vector<domain::badge_mapping> badge_mapping_repository::read_latest(std::uint32_t offset,
                                                                         std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest badge mappings with offset: " << offset
                               << " and limit: " << limit;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx_.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<badge_mapping_entity>> |
                       where("tenant_id"_c == tid && "valid_to"_c == max.value()) |
                       order_by("code_domain_code"_c, "entity_code"_c) | sqlgen::offset(offset) |
                       sqlgen::limit(limit);

    return execute_read_query<badge_mapping_entity, domain::badge_mapping>(
        ctx_,
        query,
        [](const auto& entities) { return badge_mapping_mapper::map(entities); },
        lg(),
        "Reading latest badge mappings (paginated).");
}

std::vector<domain::badge_mapping>
badge_mapping_repository::read_latest(const std::string& code_domain_code,
                                      const std::string& entity_code) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest badge mapping. " << code_domain_code << "/"
                               << entity_code;

    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx_.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<badge_mapping_entity>> |
                       where("tenant_id"_c == tid && "code_domain_code"_c == code_domain_code &&
                             "entity_code"_c == entity_code && "valid_to"_c == max.value());

    return execute_read_query<badge_mapping_entity, domain::badge_mapping>(
        ctx_,
        query,
        [](const auto& entities) { return badge_mapping_mapper::map(entities); },
        lg(),
        "Reading latest badge mapping by key.");
}

std::uint32_t badge_mapping_repository::get_total_mapping_count() {
    BOOST_LOG_SEV(lg(), debug) << "Retrieving total active badge mappings count";
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));

    struct count_result {
        long long count;
    };

    const auto tid = ctx_.tenant_id().to_string();
    const auto query = sqlgen::select_from<badge_mapping_entity>(sqlgen::count().as<"count">()) |
                       where("tenant_id"_c == tid && "valid_to"_c == max.value()) |
                       sqlgen::to<count_result>;

    const auto r = sqlgen::session(ctx_.connection_pool()).and_then(query);
    ensure_success(r, lg());

    const auto count = static_cast<std::uint32_t>(r->count);
    BOOST_LOG_SEV(lg(), debug) << "Total active badge mappings count: " << count;
    return count;
}

std::vector<domain::badge_mapping>
badge_mapping_repository::read_latest_by_code_domain(const std::string& code_domain_code) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest badge mappings. Code Domain: "
                               << code_domain_code;

    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx_.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<badge_mapping_entity>> |
                       where("tenant_id"_c == tid && "code_domain_code"_c == code_domain_code &&
                             "valid_to"_c == max.value()) |
                       order_by("entity_code"_c);

    auto rows = execute_read_query<badge_mapping_entity, domain::badge_mapping>(
        ctx_,
        query,
        [](const auto& entities) { return badge_mapping_mapper::map(entities); },
        lg(),
        "Reading latest badge mappings by code_domain.");

    return rows;
}

std::vector<domain::badge_mapping>
badge_mapping_repository::read_latest_by_entity(const std::string& entity_code) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest badge mappings. Entity Code: " << entity_code;

    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx_.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<badge_mapping_entity>> |
                       where("tenant_id"_c == tid && "entity_code"_c == entity_code &&
                             "valid_to"_c == max.value()) |
                       order_by("code_domain_code"_c);

    auto rows = execute_read_query<badge_mapping_entity, domain::badge_mapping>(
        ctx_,
        query,
        [](const auto& entities) { return badge_mapping_mapper::map(entities); },
        lg(),
        "Reading latest badge mappings by entity.");

    return rows;
}

std::vector<domain::badge_mapping> badge_mapping_repository::read_latest_by_code_domain(
    const std::string& code_domain_code, std::uint32_t offset, std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest badge mappings. Code Domain: " << code_domain_code
                               << " offset: " << offset << " limit: " << limit;

    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx_.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<badge_mapping_entity>> |
                       where("tenant_id"_c == tid && "code_domain_code"_c == code_domain_code &&
                             "valid_to"_c == max.value()) |
                       order_by("entity_code"_c) | sqlgen::offset(offset) | sqlgen::limit(limit);

    auto rows = execute_read_query<badge_mapping_entity, domain::badge_mapping>(
        ctx_,
        query,
        [](const auto& entities) { return badge_mapping_mapper::map(entities); },
        lg(),
        "Reading latest badge mappings by code_domain (paginated).");

    return rows;
}

std::uint32_t badge_mapping_repository::get_total_mapping_count_by_code_domain(
    const std::string& code_domain_code) {
    BOOST_LOG_SEV(lg(), debug) << "Retrieving total active badge mappings count. Code Domain: "
                               << code_domain_code;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));

    struct count_result {
        long long count;
    };

    const auto tid = ctx_.tenant_id().to_string();
    const auto query = sqlgen::select_from<badge_mapping_entity>(sqlgen::count().as<"count">()) |
                       where("tenant_id"_c == tid && "code_domain_code"_c == code_domain_code &&
                             "valid_to"_c == max.value()) |
                       sqlgen::to<count_result>;

    const auto r = sqlgen::session(ctx_.connection_pool()).and_then(query);
    ensure_success(r, lg());

    const auto count = static_cast<std::uint32_t>(r->count);
    BOOST_LOG_SEV(lg(), debug) << "Total active badge mappings count by code_domain: " << count;
    return count;
}

std::uint32_t
badge_mapping_repository::get_total_mapping_count_by_entity(const std::string& entity_code) {
    BOOST_LOG_SEV(lg(), debug) << "Retrieving total active badge mappings count. Entity Code: "
                               << entity_code;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));

    struct count_result {
        long long count;
    };

    const auto tid = ctx_.tenant_id().to_string();
    const auto query = sqlgen::select_from<badge_mapping_entity>(sqlgen::count().as<"count">()) |
                       where("tenant_id"_c == tid && "entity_code"_c == entity_code &&
                             "valid_to"_c == max.value()) |
                       sqlgen::to<count_result>;

    const auto r = sqlgen::session(ctx_.connection_pool()).and_then(query);
    ensure_success(r, lg());

    const auto count = static_cast<std::uint32_t>(r->count);
    BOOST_LOG_SEV(lg(), debug) << "Total active badge mappings count by entity: " << count;
    return count;
}

void badge_mapping_repository::remove(const std::string& code_domain_code,
                                      const std::string& entity_code) {
    static_cast<void>(remove(code_domain_code, entity_code, std::nullopt));
}

badge_mapping_repository::remove_status
badge_mapping_repository::remove(const std::string& code_domain_code,
                                 const std::string& entity_code,
                                 std::optional<std::uint32_t> version) {
    BOOST_LOG_SEV(lg(), debug) << "Removing badge mapping from database: " << code_domain_code
                               << "/" << entity_code;

    const auto current = read_latest(code_domain_code, entity_code);
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
    const auto tid = ctx_.tenant_id().to_string();
    const auto query = sqlgen::delete_from<badge_mapping_entity> |
                       where("tenant_id"_c == tid && "code_domain_code"_c == code_domain_code &&
                             "entity_code"_c == entity_code && "valid_to"_c == max.value() &&
                             "version"_c == expected);

    execute_delete_query(ctx_, query, lg(), "removing badge mapping from database");
    // The delete reports no affected-row count, so the row is read back: a row
    // still open after the statement means the store refused the removal, and
    // the caller hears "conflicting" rather than "removed".
    if (!read_latest(code_domain_code, entity_code).empty())
        return remove_status::conflicting;
    return remove_status::removed;
}

void badge_mapping_repository::remove(const std::vector<std::string>& code_domain_codes,
                                      const std::vector<std::string>& entity_codes) {
    // A junction's key is the pair of columns, and a per-column .in() DELETE
    // would be a cross-product over-delete (rows outside the requested pairs),
    // so each pair is removed on its own.
    if (code_domain_codes.size() != entity_codes.size())
        throw std::invalid_argument(
            "badge_mapping_repository::remove: key column vectors must be the same length");
    for (std::size_t i = 0; i < code_domain_codes.size(); ++i)
        static_cast<void>(remove(code_domain_codes[i], entity_codes[i], std::nullopt));
}

void badge_mapping_repository::remove_by_code_domain(const std::string& code_domain_code) {
    BOOST_LOG_SEV(lg(), debug) << "Removing all badge mappings from database: " << code_domain_code;

    const auto tid = ctx_.tenant_id().to_string();
    const auto query = sqlgen::delete_from<badge_mapping_entity> |
                       where("tenant_id"_c == tid && "code_domain_code"_c == code_domain_code);

    execute_delete_query(ctx_, query, lg(), "removing all badge mappings from database");
}


}
