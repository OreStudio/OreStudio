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
#include "ores.inbox.core/repository/approval_kind_part_repository.hpp"
#include "ores.database/query_builder/execute.hpp"
#include "ores.database/repository/helpers.hpp"
#include "ores.database/repository/mapper_helpers.hpp"
#include "ores.inbox.api/domain/approval_kind_part.hpp"
#include "ores.inbox.api/domain/approval_kind_part_json_io.hpp" // IWYU pragma: keep.
#include "ores.inbox.core/repository/approval_kind_part_entity.hpp"
#include "ores.inbox.core/repository/approval_kind_part_mapper.hpp"
#include "ores.logging/boost_severity.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <boost/log/sources/severity_feature.hpp>
#include <cstddef>
#include <cstdint>
#include <optional>
#include <stdexcept>
#include <string>
#include <utility>
#include <vector>

namespace ores::inbox::repository {

namespace query_builder = ores::database::query_builder;
using namespace ores::database::query_builder::literals;
using namespace ores::logging;
using namespace ores::database::repository;

std::string approval_kind_part_repository::sql() {
    return query_builder::insert_statement_sql<approval_kind_part_entity>(lg());
}

approval_kind_part_repository::approval_kind_part_repository(context ctx)
    : ctx_(std::move(ctx)) {}

ores::utility::domain::precondition
approval_kind_part_repository::replace_claim(const domain::approval_kind_part& v) {
    const auto current = read_latest(v.kind_code, v.part_code);
    if (current.empty())
        return {ores::utility::domain::precondition_kind::must_not_exist, std::nullopt};
    return {ores::utility::domain::precondition_kind::must_match_version,
            static_cast<std::uint32_t>(current.front().version)};
}

domain::approval_kind_part
approval_kind_part_repository::apply_claim(const domain::approval_kind_part& v,
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
            const auto current = read_latest(v.kind_code, v.part_code);
            t.version = current.empty() ? 0 : current.front().version;
            break;
        }
    }
    return t;
}

void approval_kind_part_repository::write(const domain::approval_kind_part& approval_kind_part) {
    write(approval_kind_part, replace_claim(approval_kind_part));
}

void approval_kind_part_repository::write(
    const std::vector<domain::approval_kind_part>& approval_kind_parts) {
    std::vector<ores::utility::domain::precondition> claims;
    claims.reserve(approval_kind_parts.size());
    for (const auto& item : approval_kind_parts)
        claims.push_back(replace_claim(item));
    write(approval_kind_parts, claims);
}

void approval_kind_part_repository::write(const domain::approval_kind_part& approval_kind_part,
                                          const ores::utility::domain::precondition& claim) {
    BOOST_LOG_SEV(lg(), debug) << "Writing approval kind part to database: "
                               << approval_kind_part.kind_code << "/"
                               << approval_kind_part.part_code;
    const auto t = apply_claim(approval_kind_part, claim);
    query_builder::execute_write_query(
        ctx_, approval_kind_part_mapper::map(t), lg(), "writing approval kind part to database");
}

void approval_kind_part_repository::write(
    const std::vector<domain::approval_kind_part>& approval_kind_parts,
    const std::vector<ores::utility::domain::precondition>& claims) {
    BOOST_LOG_SEV(lg(), debug) << "Writing approval kind parts to database. Count: "
                               << approval_kind_parts.size();
    std::vector<domain::approval_kind_part> batch;
    batch.reserve(approval_kind_parts.size());
    for (std::size_t i = 0; i < approval_kind_parts.size(); ++i)
        batch.push_back(apply_claim(approval_kind_parts[i], claims[i]));
    query_builder::execute_write_query(ctx_,
                                       approval_kind_part_mapper::map(batch),
                                       lg(),
                                       "writing approval kind parts to database");
}

std::vector<domain::approval_kind_part> approval_kind_part_repository::read_latest() {
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx_.tenant_id().to_string();
    const auto query = query_builder::read<std::vector<approval_kind_part_entity>> |
                       query_builder::where("tenant_id"_c == tid && "valid_to"_c == max.value()) |
                       query_builder::order_by("kind_code"_c, "part_code"_c);

    return query_builder::execute_read_query<approval_kind_part_entity, domain::approval_kind_part>(
        ctx_,
        query,
        [](const auto& entities) { return approval_kind_part_mapper::map(entities); },
        lg(),
        "Reading latest approval kind parts");
}

std::vector<domain::approval_kind_part>
approval_kind_part_repository::read_latest(std::uint32_t offset, std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest approval kind parts with offset: " << offset
                               << " and limit: " << limit;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx_.tenant_id().to_string();
    const auto query = query_builder::read<std::vector<approval_kind_part_entity>> |
                       query_builder::where("tenant_id"_c == tid && "valid_to"_c == max.value()) |
                       query_builder::order_by("kind_code"_c, "part_code"_c) |
                       query_builder::offset(offset) | query_builder::limit(limit);

    return query_builder::execute_read_query<approval_kind_part_entity, domain::approval_kind_part>(
        ctx_,
        query,
        [](const auto& entities) { return approval_kind_part_mapper::map(entities); },
        lg(),
        "Reading latest approval kind parts (paginated).");
}

std::vector<domain::approval_kind_part>
approval_kind_part_repository::read_latest(const std::string& kind_code,
                                           const std::string& part_code) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest approval kind part. " << kind_code << "/"
                               << part_code;

    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto kind_code_str = kind_code;
    const auto part_code_str = part_code;
    const auto tid = ctx_.tenant_id().to_string();
    const auto query =
        query_builder::read<std::vector<approval_kind_part_entity>> |
        query_builder::where("tenant_id"_c == tid && "kind_code"_c == kind_code &&
                             "part_code"_c == part_code && "valid_to"_c == max.value());

    return query_builder::execute_read_query<approval_kind_part_entity, domain::approval_kind_part>(
        ctx_,
        query,
        [](const auto& entities) { return approval_kind_part_mapper::map(entities); },
        lg(),
        "Reading latest approval kind part by key.");
}

std::uint32_t approval_kind_part_repository::get_total_approval_kind_part_count() {
    BOOST_LOG_SEV(lg(), debug) << "Retrieving total active approval kind parts count";
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));

    const auto tid = ctx_.tenant_id().to_string();
    const auto query = query_builder::read<std::vector<approval_kind_part_entity>> |
                       query_builder::where("tenant_id"_c == tid && "valid_to"_c == max.value());

    const auto count = query_builder::execute_count_query<approval_kind_part_entity>(
        ctx_, query, std::nullopt, lg(), "Counting approval kind parts");
    BOOST_LOG_SEV(lg(), debug) << "Total active approval kind parts count: " << count;
    return count;
}

std::vector<domain::approval_kind_part>
approval_kind_part_repository::read_latest_by_kind(const std::string& kind_code) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest approval kind parts. Kind: " << kind_code;

    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx_.tenant_id().to_string();
    const auto query = query_builder::read<std::vector<approval_kind_part_entity>> |
                       query_builder::where("tenant_id"_c == tid && "kind_code"_c == kind_code &&
                                            "valid_to"_c == max.value()) |
                       query_builder::order_by("part_code"_c);

    auto rows =
        query_builder::execute_read_query<approval_kind_part_entity, domain::approval_kind_part>(
            ctx_,
            query,
            [](const auto& entities) { return approval_kind_part_mapper::map(entities); },
            lg(),
            "Reading latest approval kind parts by kind.");

    return rows;
}

std::vector<domain::approval_kind_part>
approval_kind_part_repository::read_latest_by_part(const std::string& part_code) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest approval kind parts. Part: " << part_code;

    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx_.tenant_id().to_string();
    const auto query = query_builder::read<std::vector<approval_kind_part_entity>> |
                       query_builder::where("tenant_id"_c == tid && "part_code"_c == part_code &&
                                            "valid_to"_c == max.value()) |
                       query_builder::order_by("kind_code"_c);

    auto rows =
        query_builder::execute_read_query<approval_kind_part_entity, domain::approval_kind_part>(
            ctx_,
            query,
            [](const auto& entities) { return approval_kind_part_mapper::map(entities); },
            lg(),
            "Reading latest approval kind parts by part.");

    return rows;
}

std::vector<domain::approval_kind_part> approval_kind_part_repository::read_latest_by_kind(
    const std::string& kind_code, std::uint32_t offset, std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest approval kind parts. Kind: " << kind_code
                               << " offset: " << offset << " limit: " << limit;

    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx_.tenant_id().to_string();
    const auto query = query_builder::read<std::vector<approval_kind_part_entity>> |
                       query_builder::where("tenant_id"_c == tid && "kind_code"_c == kind_code &&
                                            "valid_to"_c == max.value()) |
                       query_builder::order_by("part_code"_c) | query_builder::offset(offset) |
                       query_builder::limit(limit);

    auto rows =
        query_builder::execute_read_query<approval_kind_part_entity, domain::approval_kind_part>(
            ctx_,
            query,
            [](const auto& entities) { return approval_kind_part_mapper::map(entities); },
            lg(),
            "Reading latest approval kind parts by kind (paginated).");

    return rows;
}

std::uint32_t approval_kind_part_repository::get_total_approval_kind_part_count_by_kind(
    const std::string& kind_code) {
    BOOST_LOG_SEV(lg(), debug) << "Retrieving total active approval kind parts count. Kind: "
                               << kind_code;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));

    const auto tid = ctx_.tenant_id().to_string();
    const auto query = query_builder::read<std::vector<approval_kind_part_entity>> |
                       query_builder::where("tenant_id"_c == tid && "kind_code"_c == kind_code &&
                                            "valid_to"_c == max.value());

    const auto count = query_builder::execute_count_query<approval_kind_part_entity>(
        ctx_, query, std::nullopt, lg(), "Counting approval kind parts");
    BOOST_LOG_SEV(lg(), debug) << "Total active approval kind parts count by kind: " << count;
    return count;
}

std::uint32_t approval_kind_part_repository::get_total_approval_kind_part_count_by_part(
    const std::string& part_code) {
    BOOST_LOG_SEV(lg(), debug) << "Retrieving total active approval kind parts count. Part: "
                               << part_code;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));

    const auto tid = ctx_.tenant_id().to_string();
    const auto query = query_builder::read<std::vector<approval_kind_part_entity>> |
                       query_builder::where("tenant_id"_c == tid && "part_code"_c == part_code &&
                                            "valid_to"_c == max.value());

    const auto count = query_builder::execute_count_query<approval_kind_part_entity>(
        ctx_, query, std::nullopt, lg(), "Counting approval kind parts");
    BOOST_LOG_SEV(lg(), debug) << "Total active approval kind parts count by part: " << count;
    return count;
}

std::vector<approval_kind_part_history_row>
approval_kind_part_repository::read_all_by_kind(const std::string& kind_code) {
    BOOST_LOG_SEV(lg(), debug) << "Reading every version of the approval kind parts. Kind: "
                               << kind_code;

    const auto tid = ctx_.tenant_id().to_string();
    const auto query = query_builder::read<std::vector<approval_kind_part_entity>> |
                       query_builder::where("tenant_id"_c == tid && "kind_code"_c == kind_code) |
                       query_builder::order_by("part_code"_c, "valid_from"_c.desc());

    return query_builder::execute_read_query<approval_kind_part_entity,
                                             approval_kind_part_history_row>(
        ctx_,
        query,
        [](const auto& entities) {
            std::vector<approval_kind_part_history_row> rows;
            rows.reserve(entities.size());
            for (const auto& entity : entities) {
                approval_kind_part_history_row row;
                row.link = approval_kind_part_mapper::map(entity);
                row.modified_by = row.link.modified_by;
                row.recorded_at = row.link.recorded_at;
                // A version ends when a later version replaces it, which is not
                // a closing, or when the link is closed. The rows of one link run
                // newest first, so the row before this one, when it is the same
                // link, is the next newer version, and it replaced this one only
                // if it carries the next version number. A link opened again
                // after a closing starts its numbering over.
                const bool replaced = !rows.empty() &&
                                      rows.back().link.part_code == row.link.part_code &&
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
        "Reading every version of the approval kind parts by kind.");
}

void approval_kind_part_repository::remove(const std::string& kind_code,
                                           const std::string& part_code) {
    static_cast<void>(remove(kind_code, part_code, std::nullopt));
}

approval_kind_part_repository::remove_status
approval_kind_part_repository::remove(const std::string& kind_code,
                                      const std::string& part_code,
                                      std::optional<std::uint32_t> version) {
    BOOST_LOG_SEV(lg(), debug) << "Removing approval kind part from database: " << kind_code << "/"
                               << part_code;

    const auto current = read_latest(kind_code, part_code);
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
    const auto kind_code_str = kind_code;
    const auto part_code_str = part_code;
    const auto tid = ctx_.tenant_id().to_string();
    const auto query = query_builder::delete_from<approval_kind_part_entity> |
                       query_builder::where("tenant_id"_c == tid && "kind_code"_c == kind_code &&
                                            "part_code"_c == part_code &&
                                            "valid_to"_c == max.value() && "version"_c == expected);

    query_builder::execute_delete_query(
        ctx_, query, lg(), "removing approval kind part from database");
    // The delete reports no affected-row count, so the row is read back: a row
    // still open after the statement means the store refused the removal, and
    // the caller hears "conflicting" rather than "removed".
    if (!read_latest(kind_code, part_code).empty())
        return remove_status::conflicting;
    return remove_status::removed;
}

void approval_kind_part_repository::remove(const std::vector<std::string>& kind_codes,
                                           const std::vector<std::string>& part_codes) {
    // A junction's key is the pair of columns, and a per-column .in() DELETE
    // would be a cross-product over-delete (rows outside the requested pairs),
    // so each pair is removed on its own.
    if (kind_codes.size() != part_codes.size())
        throw std::invalid_argument(
            "approval_kind_part_repository::remove: key column vectors must be the same length");
    for (std::size_t i = 0; i < kind_codes.size(); ++i)
        static_cast<void>(remove(kind_codes[i], part_codes[i], std::nullopt));
}

void approval_kind_part_repository::remove_by_kind(const std::string& kind_code) {
    BOOST_LOG_SEV(lg(), debug) << "Removing all approval kind parts from database: " << kind_code;

    const auto tid = ctx_.tenant_id().to_string();
    const auto query = query_builder::delete_from<approval_kind_part_entity> |
                       query_builder::where("tenant_id"_c == tid && "kind_code"_c == kind_code);

    query_builder::execute_delete_query(
        ctx_, query, lg(), "removing all approval kind parts from database");
}


}
