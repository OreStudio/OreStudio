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
#include "ores.refdata.core/repository/tenor_convention_resolution_repository.hpp"
#include "ores.database/query_builder/execute.hpp"
#include "ores.database/repository/helpers.hpp"
#include "ores.database/repository/mapper_helpers.hpp"
#include "ores.logging/boost_severity.hpp"
#include "ores.refdata.api/domain/tenor_convention_resolution.hpp"
#include "ores.refdata.api/domain/tenor_convention_resolution_json_io.hpp" // IWYU pragma: keep.
#include "ores.refdata.core/repository/tenor_convention_resolution_entity.hpp"
#include "ores.refdata.core/repository/tenor_convention_resolution_mapper.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <boost/log/sources/severity_feature.hpp>
#include <cstddef>
#include <cstdint>
#include <optional>
#include <stdexcept>
#include <string>
#include <utility>
#include <vector>

namespace ores::refdata::repository {

namespace query_builder = ores::database::query_builder;
using namespace ores::database::query_builder::literals;
using namespace ores::logging;
using namespace ores::database::repository;

std::string tenor_convention_resolution_repository::sql() {
    return query_builder::insert_statement_sql<tenor_convention_resolution_entity>(lg());
}

tenor_convention_resolution_repository::tenor_convention_resolution_repository(context ctx)
    : ctx_(std::move(ctx)) {}

ores::utility::domain::precondition tenor_convention_resolution_repository::replace_claim(
    const domain::tenor_convention_resolution& v) {
    const auto current = read_latest(v.convention_code, v.tenor_code);
    if (current.empty())
        return {ores::utility::domain::precondition_kind::must_not_exist, std::nullopt};
    return {ores::utility::domain::precondition_kind::must_match_version,
            static_cast<std::uint32_t>(current.front().version)};
}

domain::tenor_convention_resolution tenor_convention_resolution_repository::apply_claim(
    const domain::tenor_convention_resolution& v,
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
            const auto current = read_latest(v.convention_code, v.tenor_code);
            t.version = current.empty() ? 0 : current.front().version;
            break;
        }
    }
    return t;
}

void tenor_convention_resolution_repository::write(
    const domain::tenor_convention_resolution& resolution) {
    write(resolution, replace_claim(resolution));
}

void tenor_convention_resolution_repository::write(
    const std::vector<domain::tenor_convention_resolution>& resolutions) {
    std::vector<ores::utility::domain::precondition> claims;
    claims.reserve(resolutions.size());
    for (const auto& item : resolutions)
        claims.push_back(replace_claim(item));
    write(resolutions, claims);
}

void tenor_convention_resolution_repository::write(
    const domain::tenor_convention_resolution& resolution,
    const ores::utility::domain::precondition& claim) {
    BOOST_LOG_SEV(lg(), debug) << "Writing tenor convention resolution to database: "
                               << resolution.convention_code << "/" << resolution.tenor_code;
    const auto t = apply_claim(resolution, claim);
    query_builder::execute_write_query(ctx_,
                                       tenor_convention_resolution_mapper::map(t),
                                       lg(),
                                       "writing tenor convention resolution to database");
}

void tenor_convention_resolution_repository::write(
    const std::vector<domain::tenor_convention_resolution>& resolutions,
    const std::vector<ores::utility::domain::precondition>& claims) {
    BOOST_LOG_SEV(lg(), debug) << "Writing tenor convention resolutions to database. Count: "
                               << resolutions.size();
    std::vector<domain::tenor_convention_resolution> batch;
    batch.reserve(resolutions.size());
    for (std::size_t i = 0; i < resolutions.size(); ++i)
        batch.push_back(apply_claim(resolutions[i], claims[i]));
    query_builder::execute_write_query(ctx_,
                                       tenor_convention_resolution_mapper::map(batch),
                                       lg(),
                                       "writing tenor convention resolutions to database");
}

std::vector<domain::tenor_convention_resolution>
tenor_convention_resolution_repository::read_latest() {
    static constexpr auto max = db_timestamp::max();
    const auto tid = ctx_.tenant_id().to_string();
    const auto query = query_builder::read<std::vector<tenor_convention_resolution_entity>> |
                       query_builder::where("tenant_id"_c == tid && "valid_to"_c == max) |
                       query_builder::order_by("convention_code"_c, "tenor_code"_c);

    return query_builder::execute_read_query<tenor_convention_resolution_entity,
                                             domain::tenor_convention_resolution>(
        ctx_,
        query,
        [](const auto& entities) { return tenor_convention_resolution_mapper::map(entities); },
        lg(),
        "Reading latest tenor convention resolutions");
}

std::vector<domain::tenor_convention_resolution>
tenor_convention_resolution_repository::read_latest(std::uint32_t offset, std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest tenor convention resolutions with offset: "
                               << offset << " and limit: " << limit;
    static constexpr auto max = db_timestamp::max();
    const auto tid = ctx_.tenant_id().to_string();
    const auto query = query_builder::read<std::vector<tenor_convention_resolution_entity>> |
                       query_builder::where("tenant_id"_c == tid && "valid_to"_c == max) |
                       query_builder::order_by("convention_code"_c, "tenor_code"_c) |
                       query_builder::offset(offset) | query_builder::limit(limit);

    return query_builder::execute_read_query<tenor_convention_resolution_entity,
                                             domain::tenor_convention_resolution>(
        ctx_,
        query,
        [](const auto& entities) { return tenor_convention_resolution_mapper::map(entities); },
        lg(),
        "Reading latest tenor convention resolutions (paginated).");
}

std::vector<domain::tenor_convention_resolution>
tenor_convention_resolution_repository::read_latest(const std::string& convention_code,
                                                    const std::string& tenor_code) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest tenor convention resolution. " << convention_code
                               << "/" << tenor_code;

    static constexpr auto max = db_timestamp::max();
    const auto convention_code_str = convention_code;
    const auto tenor_code_str = tenor_code;
    const auto tid = ctx_.tenant_id().to_string();
    const auto query =
        query_builder::read<std::vector<tenor_convention_resolution_entity>> |
        query_builder::where("tenant_id"_c == tid && "convention_code"_c == convention_code &&
                             "tenor_code"_c == tenor_code && "valid_to"_c == max);

    return query_builder::execute_read_query<tenor_convention_resolution_entity,
                                             domain::tenor_convention_resolution>(
        ctx_,
        query,
        [](const auto& entities) { return tenor_convention_resolution_mapper::map(entities); },
        lg(),
        "Reading latest tenor convention resolution by key.");
}

std::uint32_t tenor_convention_resolution_repository::get_total_resolution_count() {
    BOOST_LOG_SEV(lg(), debug) << "Retrieving total active tenor convention resolutions count";
    static constexpr auto max = db_timestamp::max();

    const auto tid = ctx_.tenant_id().to_string();
    const auto query = query_builder::read<std::vector<tenor_convention_resolution_entity>> |
                       query_builder::where("tenant_id"_c == tid && "valid_to"_c == max);

    const auto count = query_builder::execute_count_query<tenor_convention_resolution_entity>(
        ctx_, query, std::nullopt, lg(), "Counting tenor convention resolutions");
    BOOST_LOG_SEV(lg(), debug) << "Total active tenor convention resolutions count: " << count;
    return count;
}

std::vector<domain::tenor_convention_resolution>
tenor_convention_resolution_repository::read_latest_by_convention(
    const std::string& convention_code) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest tenor convention resolutions. Convention: "
                               << convention_code;

    static constexpr auto max = db_timestamp::max();
    const auto tid = ctx_.tenant_id().to_string();
    const auto query =
        query_builder::read<std::vector<tenor_convention_resolution_entity>> |
        query_builder::where("tenant_id"_c == tid && "convention_code"_c == convention_code &&
                             "valid_to"_c == max) |
        query_builder::order_by("tenor_code"_c);

    auto rows = query_builder::execute_read_query<tenor_convention_resolution_entity,
                                                  domain::tenor_convention_resolution>(
        ctx_,
        query,
        [](const auto& entities) { return tenor_convention_resolution_mapper::map(entities); },
        lg(),
        "Reading latest tenor convention resolutions by convention.");

    return rows;
}

std::vector<domain::tenor_convention_resolution>
tenor_convention_resolution_repository::read_latest_by_tenor(const std::string& tenor_code) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest tenor convention resolutions. Tenor: "
                               << tenor_code;

    static constexpr auto max = db_timestamp::max();
    const auto tid = ctx_.tenant_id().to_string();
    const auto query = query_builder::read<std::vector<tenor_convention_resolution_entity>> |
                       query_builder::where("tenant_id"_c == tid && "tenor_code"_c == tenor_code &&
                                            "valid_to"_c == max) |
                       query_builder::order_by("convention_code"_c);

    auto rows = query_builder::execute_read_query<tenor_convention_resolution_entity,
                                                  domain::tenor_convention_resolution>(
        ctx_,
        query,
        [](const auto& entities) { return tenor_convention_resolution_mapper::map(entities); },
        lg(),
        "Reading latest tenor convention resolutions by tenor.");

    return rows;
}

std::vector<domain::tenor_convention_resolution>
tenor_convention_resolution_repository::read_latest_by_convention(
    const std::string& convention_code, std::uint32_t offset, std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest tenor convention resolutions. Convention: "
                               << convention_code << " offset: " << offset << " limit: " << limit;

    static constexpr auto max = db_timestamp::max();
    const auto tid = ctx_.tenant_id().to_string();
    const auto query =
        query_builder::read<std::vector<tenor_convention_resolution_entity>> |
        query_builder::where("tenant_id"_c == tid && "convention_code"_c == convention_code &&
                             "valid_to"_c == max) |
        query_builder::order_by("tenor_code"_c) | query_builder::offset(offset) |
        query_builder::limit(limit);

    auto rows = query_builder::execute_read_query<tenor_convention_resolution_entity,
                                                  domain::tenor_convention_resolution>(
        ctx_,
        query,
        [](const auto& entities) { return tenor_convention_resolution_mapper::map(entities); },
        lg(),
        "Reading latest tenor convention resolutions by convention (paginated).");

    return rows;
}

std::uint32_t tenor_convention_resolution_repository::get_total_resolution_count_by_convention(
    const std::string& convention_code) {
    BOOST_LOG_SEV(lg(), debug)
        << "Retrieving total active tenor convention resolutions count. Convention: "
        << convention_code;
    static constexpr auto max = db_timestamp::max();

    const auto tid = ctx_.tenant_id().to_string();
    const auto query =
        query_builder::read<std::vector<tenor_convention_resolution_entity>> |
        query_builder::where("tenant_id"_c == tid && "convention_code"_c == convention_code &&
                             "valid_to"_c == max);

    const auto count = query_builder::execute_count_query<tenor_convention_resolution_entity>(
        ctx_, query, std::nullopt, lg(), "Counting tenor convention resolutions");
    BOOST_LOG_SEV(lg(), debug) << "Total active tenor convention resolutions count by convention: "
                               << count;
    return count;
}

std::uint32_t tenor_convention_resolution_repository::get_total_resolution_count_by_tenor(
    const std::string& tenor_code) {
    BOOST_LOG_SEV(lg(), debug)
        << "Retrieving total active tenor convention resolutions count. Tenor: " << tenor_code;
    static constexpr auto max = db_timestamp::max();

    const auto tid = ctx_.tenant_id().to_string();
    const auto query = query_builder::read<std::vector<tenor_convention_resolution_entity>> |
                       query_builder::where("tenant_id"_c == tid && "tenor_code"_c == tenor_code &&
                                            "valid_to"_c == max);

    const auto count = query_builder::execute_count_query<tenor_convention_resolution_entity>(
        ctx_, query, std::nullopt, lg(), "Counting tenor convention resolutions");
    BOOST_LOG_SEV(lg(), debug) << "Total active tenor convention resolutions count by tenor: "
                               << count;
    return count;
}

std::vector<tenor_convention_resolution_history_row>
tenor_convention_resolution_repository::read_all_by_convention(const std::string& convention_code) {
    BOOST_LOG_SEV(lg(), debug)
        << "Reading every version of the tenor convention resolutions. Convention: "
        << convention_code;

    const auto tid = ctx_.tenant_id().to_string();
    const auto query =
        query_builder::read<std::vector<tenor_convention_resolution_entity>> |
        query_builder::where("tenant_id"_c == tid && "convention_code"_c == convention_code) |
        query_builder::order_by("tenor_code"_c, "valid_from"_c.desc());

    return query_builder::execute_read_query<tenor_convention_resolution_entity,
                                             tenor_convention_resolution_history_row>(
        ctx_,
        query,
        [](const auto& entities) {
            std::vector<tenor_convention_resolution_history_row> rows;
            rows.reserve(entities.size());
            for (const auto& entity : entities) {
                tenor_convention_resolution_history_row row;
                row.link = tenor_convention_resolution_mapper::map(entity);
                row.modified_by = row.link.modified_by;
                row.recorded_at = row.link.recorded_at;
                // A version ends when a later version replaces it, which is not
                // a closing, or when the link is closed. The rows of one link run
                // newest first, so the row before this one, when it is the same
                // link, is the next newer version, and it replaced this one only
                // if it carries the next version number. A link opened again
                // after a closing starts its numbering over.
                const bool replaced = !rows.empty() &&
                                      rows.back().link.tenor_code == row.link.tenor_code &&
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
        "Reading every version of the tenor convention resolutions by convention.");
}

void tenor_convention_resolution_repository::remove(const std::string& convention_code,
                                                    const std::string& tenor_code) {
    static_cast<void>(remove(convention_code, tenor_code, std::nullopt));
}

tenor_convention_resolution_repository::remove_status
tenor_convention_resolution_repository::remove(const std::string& convention_code,
                                               const std::string& tenor_code,
                                               std::optional<std::uint32_t> version) {
    BOOST_LOG_SEV(lg(), debug) << "Removing tenor convention resolution from database: "
                               << convention_code << "/" << tenor_code;

    const auto current = read_latest(convention_code, tenor_code);
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
    const auto convention_code_str = convention_code;
    const auto tenor_code_str = tenor_code;
    const auto tid = ctx_.tenant_id().to_string();
    const auto query =
        query_builder::delete_from<tenor_convention_resolution_entity> |
        query_builder::where("tenant_id"_c == tid && "convention_code"_c == convention_code &&
                             "tenor_code"_c == tenor_code && "valid_to"_c == max &&
                             "version"_c == expected);

    query_builder::execute_delete_query(
        ctx_, query, lg(), "removing tenor convention resolution from database");
    // The delete reports no affected-row count, so the row is read back: a row
    // still open after the statement means the store refused the removal, and
    // the caller hears "conflicting" rather than "removed".
    if (!read_latest(convention_code, tenor_code).empty())
        return remove_status::conflicting;
    return remove_status::removed;
}

void tenor_convention_resolution_repository::remove(
    const std::vector<std::string>& convention_codes, const std::vector<std::string>& tenor_codes) {
    // A junction's key is the pair of columns, and a per-column .in() DELETE
    // would be a cross-product over-delete (rows outside the requested pairs),
    // so each pair is removed on its own.
    if (convention_codes.size() != tenor_codes.size())
        throw std::invalid_argument("tenor_convention_resolution_repository::remove: key column "
                                    "vectors must be the same length");
    for (std::size_t i = 0; i < convention_codes.size(); ++i)
        static_cast<void>(remove(convention_codes[i], tenor_codes[i], std::nullopt));
}

void tenor_convention_resolution_repository::remove_by_convention(
    const std::string& convention_code) {
    BOOST_LOG_SEV(lg(), debug) << "Removing all tenor convention resolutions from database: "
                               << convention_code;

    const auto tid = ctx_.tenant_id().to_string();
    const auto query =
        query_builder::delete_from<tenor_convention_resolution_entity> |
        query_builder::where("tenant_id"_c == tid && "convention_code"_c == convention_code);

    query_builder::execute_delete_query(
        ctx_, query, lg(), "removing all tenor convention resolutions from database");
}


}
