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
#include "ores.reporting.core/repository/report_type_configuration_type_repository.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.database/repository/helpers.hpp"
#include "ores.reporting.api/domain/report_type_configuration_type_json_io.hpp" // IWYU pragma: keep.
#include "ores.reporting.core/repository/report_type_configuration_type_entity.hpp"
#include "ores.reporting.core/repository/report_type_configuration_type_mapper.hpp"
#include <cstddef>
#include <optional>
#include <sqlgen/postgres.hpp>
#include <stdexcept>

namespace ores::reporting::repository {

using namespace sqlgen;
using namespace sqlgen::literals;
using namespace ores::logging;
using namespace ores::database::repository;

std::string report_type_configuration_type_repository::sql() {
    return generate_create_table_sql<report_type_configuration_type_entity>(lg());
}

report_type_configuration_type_repository::report_type_configuration_type_repository(context ctx)
    : ctx_(std::move(ctx)) {}

ores::utility::domain::precondition report_type_configuration_type_repository::replace_claim(
    const domain::report_type_configuration_type& v) {
    const auto current = read_latest(v.report_type_code, v.configuration_type_code);
    if (current.empty())
        return {ores::utility::domain::precondition_kind::must_not_exist, std::nullopt};
    return {ores::utility::domain::precondition_kind::must_match_version,
            static_cast<std::uint32_t>(current.front().version)};
}

domain::report_type_configuration_type report_type_configuration_type_repository::apply_claim(
    const domain::report_type_configuration_type& v,
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
            const auto current = read_latest(v.report_type_code, v.configuration_type_code);
            t.version = current.empty() ? 0 : current.front().version;
            break;
        }
    }
    return t;
}

void report_type_configuration_type_repository::write(
    const domain::report_type_configuration_type& report_type_configuration_type) {
    write(report_type_configuration_type, replace_claim(report_type_configuration_type));
}

void report_type_configuration_type_repository::write(
    const std::vector<domain::report_type_configuration_type>& report_type_configuration_types) {
    std::vector<ores::utility::domain::precondition> claims;
    claims.reserve(report_type_configuration_types.size());
    for (const auto& item : report_type_configuration_types)
        claims.push_back(replace_claim(item));
    write(report_type_configuration_types, claims);
}

void report_type_configuration_type_repository::write(
    const domain::report_type_configuration_type& report_type_configuration_type,
    const ores::utility::domain::precondition& claim) {
    BOOST_LOG_SEV(lg(), debug) << "Writing report type configuration type to database: "
                               << report_type_configuration_type.report_type_code << "/"
                               << report_type_configuration_type.configuration_type_code;
    const auto t = apply_claim(report_type_configuration_type, claim);
    execute_write_query(ctx_,
                        report_type_configuration_type_mapper::map(t),
                        lg(),
                        "writing report type configuration type to database");
}

void report_type_configuration_type_repository::write(
    const std::vector<domain::report_type_configuration_type>& report_type_configuration_types,
    const std::vector<ores::utility::domain::precondition>& claims) {
    BOOST_LOG_SEV(lg(), debug) << "Writing report type configuration types to database. Count: "
                               << report_type_configuration_types.size();
    std::vector<domain::report_type_configuration_type> batch;
    batch.reserve(report_type_configuration_types.size());
    for (std::size_t i = 0; i < report_type_configuration_types.size(); ++i)
        batch.push_back(apply_claim(report_type_configuration_types[i], claims[i]));
    execute_write_query(ctx_,
                        report_type_configuration_type_mapper::map(batch),
                        lg(),
                        "writing report type configuration types to database");
}

std::vector<domain::report_type_configuration_type>
report_type_configuration_type_repository::read_latest() {
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx_.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<report_type_configuration_type_entity>> |
                       where("tenant_id"_c == tid && "valid_to"_c == max.value()) |
                       order_by("report_type_code"_c, "configuration_type_code"_c);

    return execute_read_query<report_type_configuration_type_entity,
                              domain::report_type_configuration_type>(
        ctx_,
        query,
        [](const auto& entities) { return report_type_configuration_type_mapper::map(entities); },
        lg(),
        "Reading latest report type configuration types");
}

std::vector<domain::report_type_configuration_type>
report_type_configuration_type_repository::read_latest(std::uint32_t offset, std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest report type configuration types with offset: "
                               << offset << " and limit: " << limit;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx_.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<report_type_configuration_type_entity>> |
                       where("tenant_id"_c == tid && "valid_to"_c == max.value()) |
                       order_by("report_type_code"_c, "configuration_type_code"_c) |
                       sqlgen::offset(offset) | sqlgen::limit(limit);

    return execute_read_query<report_type_configuration_type_entity,
                              domain::report_type_configuration_type>(
        ctx_,
        query,
        [](const auto& entities) { return report_type_configuration_type_mapper::map(entities); },
        lg(),
        "Reading latest report type configuration types (paginated).");
}

std::vector<domain::report_type_configuration_type>
report_type_configuration_type_repository::read_latest(const std::string& report_type_code,
                                                       const std::string& configuration_type_code) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest report type configuration type. "
                               << report_type_code << "/" << configuration_type_code;

    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto report_type_code_str = report_type_code;
    const auto configuration_type_code_str = configuration_type_code;
    const auto tid = ctx_.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<report_type_configuration_type_entity>> |
                       where("tenant_id"_c == tid && "report_type_code"_c == report_type_code &&
                             "configuration_type_code"_c == configuration_type_code &&
                             "valid_to"_c == max.value());

    return execute_read_query<report_type_configuration_type_entity,
                              domain::report_type_configuration_type>(
        ctx_,
        query,
        [](const auto& entities) { return report_type_configuration_type_mapper::map(entities); },
        lg(),
        "Reading latest report type configuration type by key.");
}

std::uint32_t
report_type_configuration_type_repository::get_total_report_type_configuration_type_count() {
    BOOST_LOG_SEV(lg(), debug) << "Retrieving total active report type configuration types count";
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));

    struct count_result {
        long long count;
    };

    const auto tid = ctx_.tenant_id().to_string();
    const auto query =
        sqlgen::select_from<report_type_configuration_type_entity>(sqlgen::count().as<"count">()) |
        where("tenant_id"_c == tid && "valid_to"_c == max.value()) | sqlgen::to<count_result>;

    const auto r = sqlgen::session(ctx_.connection_pool()).and_then(query);
    ensure_success(r, lg());

    const auto count = static_cast<std::uint32_t>(r->count);
    BOOST_LOG_SEV(lg(), debug) << "Total active report type configuration types count: " << count;
    return count;
}

std::vector<domain::report_type_configuration_type>
report_type_configuration_type_repository::read_latest_by_report_type(
    const std::string& report_type_code) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest report type configuration types. Report Type: "
                               << report_type_code;

    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx_.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<report_type_configuration_type_entity>> |
                       where("tenant_id"_c == tid && "report_type_code"_c == report_type_code &&
                             "valid_to"_c == max.value()) |
                       order_by("configuration_type_code"_c);

    auto rows = execute_read_query<report_type_configuration_type_entity,
                                   domain::report_type_configuration_type>(
        ctx_,
        query,
        [](const auto& entities) { return report_type_configuration_type_mapper::map(entities); },
        lg(),
        "Reading latest report type configuration types by report_type.");

    return rows;
}

std::vector<domain::report_type_configuration_type>
report_type_configuration_type_repository::read_latest_by_configuration_type(
    const std::string& configuration_type_code) {
    BOOST_LOG_SEV(lg(), debug)
        << "Reading latest report type configuration types. Configuration Type: "
        << configuration_type_code;

    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx_.tenant_id().to_string();
    const auto query =
        sqlgen::read<std::vector<report_type_configuration_type_entity>> |
        where("tenant_id"_c == tid && "configuration_type_code"_c == configuration_type_code &&
              "valid_to"_c == max.value()) |
        order_by("report_type_code"_c);

    auto rows = execute_read_query<report_type_configuration_type_entity,
                                   domain::report_type_configuration_type>(
        ctx_,
        query,
        [](const auto& entities) { return report_type_configuration_type_mapper::map(entities); },
        lg(),
        "Reading latest report type configuration types by configuration_type.");

    return rows;
}

std::vector<domain::report_type_configuration_type>
report_type_configuration_type_repository::read_latest_by_report_type(
    const std::string& report_type_code, std::uint32_t offset, std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest report type configuration types. Report Type: "
                               << report_type_code << " offset: " << offset << " limit: " << limit;

    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx_.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<report_type_configuration_type_entity>> |
                       where("tenant_id"_c == tid && "report_type_code"_c == report_type_code &&
                             "valid_to"_c == max.value()) |
                       order_by("configuration_type_code"_c) | sqlgen::offset(offset) |
                       sqlgen::limit(limit);

    auto rows = execute_read_query<report_type_configuration_type_entity,
                                   domain::report_type_configuration_type>(
        ctx_,
        query,
        [](const auto& entities) { return report_type_configuration_type_mapper::map(entities); },
        lg(),
        "Reading latest report type configuration types by report_type (paginated).");

    return rows;
}

std::uint32_t report_type_configuration_type_repository::
    get_total_report_type_configuration_type_count_by_report_type(
        const std::string& report_type_code) {
    BOOST_LOG_SEV(lg(), debug)
        << "Retrieving total active report type configuration types count. Report Type: "
        << report_type_code;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));

    struct count_result {
        long long count;
    };

    const auto tid = ctx_.tenant_id().to_string();
    const auto query =
        sqlgen::select_from<report_type_configuration_type_entity>(sqlgen::count().as<"count">()) |
        where("tenant_id"_c == tid && "report_type_code"_c == report_type_code &&
              "valid_to"_c == max.value()) |
        sqlgen::to<count_result>;

    const auto r = sqlgen::session(ctx_.connection_pool()).and_then(query);
    ensure_success(r, lg());

    const auto count = static_cast<std::uint32_t>(r->count);
    BOOST_LOG_SEV(lg(), debug)
        << "Total active report type configuration types count by report_type: " << count;
    return count;
}

std::uint32_t report_type_configuration_type_repository::
    get_total_report_type_configuration_type_count_by_configuration_type(
        const std::string& configuration_type_code) {
    BOOST_LOG_SEV(lg(), debug)
        << "Retrieving total active report type configuration types count. Configuration Type: "
        << configuration_type_code;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));

    struct count_result {
        long long count;
    };

    const auto tid = ctx_.tenant_id().to_string();
    const auto query =
        sqlgen::select_from<report_type_configuration_type_entity>(sqlgen::count().as<"count">()) |
        where("tenant_id"_c == tid && "configuration_type_code"_c == configuration_type_code &&
              "valid_to"_c == max.value()) |
        sqlgen::to<count_result>;

    const auto r = sqlgen::session(ctx_.connection_pool()).and_then(query);
    ensure_success(r, lg());

    const auto count = static_cast<std::uint32_t>(r->count);
    BOOST_LOG_SEV(lg(), debug)
        << "Total active report type configuration types count by configuration_type: " << count;
    return count;
}

void report_type_configuration_type_repository::remove(const std::string& report_type_code,
                                                       const std::string& configuration_type_code) {
    static_cast<void>(remove(report_type_code, configuration_type_code, std::nullopt));
}

report_type_configuration_type_repository::remove_status
report_type_configuration_type_repository::remove(const std::string& report_type_code,
                                                  const std::string& configuration_type_code,
                                                  std::optional<std::uint32_t> version) {
    BOOST_LOG_SEV(lg(), debug) << "Removing report type configuration type from database: "
                               << report_type_code << "/" << configuration_type_code;

    const auto current = read_latest(report_type_code, configuration_type_code);
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
    const auto report_type_code_str = report_type_code;
    const auto configuration_type_code_str = configuration_type_code;
    const auto tid = ctx_.tenant_id().to_string();
    const auto query = sqlgen::delete_from<report_type_configuration_type_entity> |
                       where("tenant_id"_c == tid && "report_type_code"_c == report_type_code &&
                             "configuration_type_code"_c == configuration_type_code &&
                             "valid_to"_c == max.value() && "version"_c == expected);

    execute_delete_query(
        ctx_, query, lg(), "removing report type configuration type from database");
    // The delete reports no affected-row count, so the row is read back: a row
    // still open after the statement means the store refused the removal, and
    // the caller hears "conflicting" rather than "removed".
    if (!read_latest(report_type_code, configuration_type_code).empty())
        return remove_status::conflicting;
    return remove_status::removed;
}

void report_type_configuration_type_repository::remove(
    const std::vector<std::string>& report_type_codes,
    const std::vector<std::string>& configuration_type_codes) {
    // A junction's key is the pair of columns, and a per-column .in() DELETE
    // would be a cross-product over-delete (rows outside the requested pairs),
    // so each pair is removed on its own.
    if (report_type_codes.size() != configuration_type_codes.size())
        throw std::invalid_argument("report_type_configuration_type_repository::remove: key column "
                                    "vectors must be the same length");
    for (std::size_t i = 0; i < report_type_codes.size(); ++i)
        static_cast<void>(remove(report_type_codes[i], configuration_type_codes[i], std::nullopt));
}

void report_type_configuration_type_repository::remove_by_report_type(
    const std::string& report_type_code) {
    BOOST_LOG_SEV(lg(), debug) << "Removing all report type configuration types from database: "
                               << report_type_code;

    const auto tid = ctx_.tenant_id().to_string();
    const auto query = sqlgen::delete_from<report_type_configuration_type_entity> |
                       where("tenant_id"_c == tid && "report_type_code"_c == report_type_code);

    execute_delete_query(
        ctx_, query, lg(), "removing all report type configuration types from database");
}


}
