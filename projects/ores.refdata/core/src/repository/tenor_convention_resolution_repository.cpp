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
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.database/repository/helpers.hpp"
#include "ores.refdata.api/domain/tenor_convention_resolution_json_io.hpp" // IWYU pragma: keep.
#include "ores.refdata.core/repository/tenor_convention_resolution_entity.hpp"
#include "ores.refdata.core/repository/tenor_convention_resolution_mapper.hpp"
#include <sqlgen/postgres.hpp>

namespace ores::refdata::repository {

using namespace sqlgen;
using namespace sqlgen::literals;
using namespace ores::logging;
using namespace ores::database::repository;

std::string tenor_convention_resolution_repository::sql() {
    return generate_create_table_sql<tenor_convention_resolution_entity>(lg());
}

tenor_convention_resolution_repository::tenor_convention_resolution_repository(context ctx)
    : ctx_(std::move(ctx)) {}

std::vector<domain::tenor_convention_resolution>
tenor_convention_resolution_repository::read_latest() {
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx_.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<tenor_convention_resolution_entity>> |
                       where("tenant_id"_c == tid && "valid_to"_c == max.value()) |
                       order_by("convention_code"_c, "tenor_code"_c);

    return execute_read_query<tenor_convention_resolution_entity,
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
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx_.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<tenor_convention_resolution_entity>> |
                       where("tenant_id"_c == tid && "valid_to"_c == max.value()) |
                       order_by("convention_code"_c, "tenor_code"_c) | sqlgen::offset(offset) |
                       sqlgen::limit(limit);

    return execute_read_query<tenor_convention_resolution_entity,
                              domain::tenor_convention_resolution>(
        ctx_,
        query,
        [](const auto& entities) { return tenor_convention_resolution_mapper::map(entities); },
        lg(),
        "Reading latest tenor convention resolutions (paginated).");
}

std::uint32_t tenor_convention_resolution_repository::get_total_resolution_count() {
    BOOST_LOG_SEV(lg(), debug) << "Retrieving total active tenor convention resolutions count";
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));

    struct count_result {
        long long count;
    };

    const auto tid = ctx_.tenant_id().to_string();
    const auto query =
        sqlgen::select_from<tenor_convention_resolution_entity>(sqlgen::count().as<"count">()) |
        where("tenant_id"_c == tid && "valid_to"_c == max.value()) | sqlgen::to<count_result>;

    const auto r = sqlgen::session(ctx_.connection_pool()).and_then(query);
    ensure_success(r, lg());

    const auto count = static_cast<std::uint32_t>(r->count);
    BOOST_LOG_SEV(lg(), debug) << "Total active tenor convention resolutions count: " << count;
    return count;
}

std::vector<domain::tenor_convention_resolution>
tenor_convention_resolution_repository::read_latest_by_convention(
    const std::string& convention_code) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest tenor convention resolutions. Convention: "
                               << convention_code;

    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx_.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<tenor_convention_resolution_entity>> |
                       where("tenant_id"_c == tid && "convention_code"_c == convention_code &&
                             "valid_to"_c == max.value()) |
                       order_by("tenor_code"_c);

    auto rows =
        execute_read_query<tenor_convention_resolution_entity, domain::tenor_convention_resolution>(
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

    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx_.tenant_id().to_string();
    const auto query =
        sqlgen::read<std::vector<tenor_convention_resolution_entity>> |
        where("tenant_id"_c == tid && "tenor_code"_c == tenor_code && "valid_to"_c == max.value()) |
        order_by("convention_code"_c);

    auto rows =
        execute_read_query<tenor_convention_resolution_entity, domain::tenor_convention_resolution>(
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

    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx_.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<tenor_convention_resolution_entity>> |
                       where("tenant_id"_c == tid && "convention_code"_c == convention_code &&
                             "valid_to"_c == max.value()) |
                       order_by("tenor_code"_c) | sqlgen::offset(offset) | sqlgen::limit(limit);

    auto rows =
        execute_read_query<tenor_convention_resolution_entity, domain::tenor_convention_resolution>(
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
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));

    struct count_result {
        long long count;
    };

    const auto tid = ctx_.tenant_id().to_string();
    const auto query =
        sqlgen::select_from<tenor_convention_resolution_entity>(sqlgen::count().as<"count">()) |
        where("tenant_id"_c == tid && "convention_code"_c == convention_code &&
              "valid_to"_c == max.value()) |
        sqlgen::to<count_result>;

    const auto r = sqlgen::session(ctx_.connection_pool()).and_then(query);
    ensure_success(r, lg());

    const auto count = static_cast<std::uint32_t>(r->count);
    BOOST_LOG_SEV(lg(), debug) << "Total active tenor convention resolutions count by convention: "
                               << count;
    return count;
}

std::uint32_t tenor_convention_resolution_repository::get_total_resolution_count_by_tenor(
    const std::string& tenor_code) {
    BOOST_LOG_SEV(lg(), debug)
        << "Retrieving total active tenor convention resolutions count. Tenor: " << tenor_code;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));

    struct count_result {
        long long count;
    };

    const auto tid = ctx_.tenant_id().to_string();
    const auto query =
        sqlgen::select_from<tenor_convention_resolution_entity>(sqlgen::count().as<"count">()) |
        where("tenant_id"_c == tid && "tenor_code"_c == tenor_code && "valid_to"_c == max.value()) |
        sqlgen::to<count_result>;

    const auto r = sqlgen::session(ctx_.connection_pool()).and_then(query);
    ensure_success(r, lg());

    const auto count = static_cast<std::uint32_t>(r->count);
    BOOST_LOG_SEV(lg(), debug) << "Total active tenor convention resolutions count by tenor: "
                               << count;
    return count;
}

}
