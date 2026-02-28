/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2025 Marco Craveiro <marco.craveiro@gmail.com>
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
#include "ores.dq/repository/coding_scheme_repository.hpp"

#include <sqlgen/postgres.hpp>
#include "ores.database/repository/helpers.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.dq/domain/coding_scheme_json_io.hpp" // IWYU pragma: keep.
#include "ores.dq/repository/coding_scheme_entity.hpp"
#include "ores.dq/repository/coding_scheme_mapper.hpp"

namespace ores::dq::repository {

using namespace sqlgen;
using namespace sqlgen::literals;
using namespace ores::logging;
using namespace ores::database::repository;

std::string coding_scheme_repository::sql() {
    return generate_create_table_sql<coding_scheme_entity>(lg());
}

coding_scheme_repository::coding_scheme_repository(context ctx)
    : ctx_(std::move(ctx)) {}

void coding_scheme_repository::write(const domain::coding_scheme& scheme) {
    BOOST_LOG_SEV(lg(), debug) << "Writing coding_scheme to database: "
                               << scheme.code;
    execute_write_query(ctx_, coding_scheme_mapper::map(scheme),
        lg(), "writing coding_scheme to database");
}

void coding_scheme_repository::write(
    const std::vector<domain::coding_scheme>& schemes) {
    BOOST_LOG_SEV(lg(), debug) << "Writing coding_schemes to database. Count: "
                               << schemes.size();
    execute_write_query(ctx_, coding_scheme_mapper::map(schemes),
        lg(), "writing coding_schemes to database");
}

std::vector<domain::coding_scheme>
coding_scheme_repository::read_latest() {
    const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto query = sqlgen::read<std::vector<coding_scheme_entity>> |
        where("valid_to"_c == max.value()) |
        order_by("code"_c);

    return execute_read_query<coding_scheme_entity, domain::coding_scheme>(
        ctx_, query,
        [](const auto& entities) { return coding_scheme_mapper::map(entities); },
        lg(), "Reading latest coding_schemes");
}

std::vector<domain::coding_scheme>
coding_scheme_repository::read_latest(const std::string& code) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest coding_scheme. Code: " << code;

    const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto query = sqlgen::read<std::vector<coding_scheme_entity>> |
        where("code"_c == code && "valid_to"_c == max.value());

    return execute_read_query<coding_scheme_entity, domain::coding_scheme>(
        ctx_, query,
        [](const auto& entities) { return coding_scheme_mapper::map(entities); },
        lg(), "Reading latest coding_scheme by code.");
}

std::vector<domain::coding_scheme>
coding_scheme_repository::read_latest(std::uint32_t offset, std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest coding_schemes with offset: "
                               << offset << " and limit: " << limit;

    const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto query = sqlgen::read<std::vector<coding_scheme_entity>> |
        where("valid_to"_c == max.value()) |
        order_by("code"_c) |
        sqlgen::offset(offset) |
        sqlgen::limit(limit);

    return execute_read_query<coding_scheme_entity, domain::coding_scheme>(
        ctx_, query,
        [](const auto& entities) { return coding_scheme_mapper::map(entities); },
        lg(), "Reading latest coding_schemes with pagination.");
}

std::vector<domain::coding_scheme>
coding_scheme_repository::read_latest_by_authority_type(const std::string& authority_type) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest coding_schemes by authority_type: "
                               << authority_type;

    const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto query = sqlgen::read<std::vector<coding_scheme_entity>> |
        where("authority_type"_c == authority_type && "valid_to"_c == max.value()) |
        order_by("code"_c);

    return execute_read_query<coding_scheme_entity, domain::coding_scheme>(
        ctx_, query,
        [](const auto& entities) { return coding_scheme_mapper::map(entities); },
        lg(), "Reading latest coding_schemes by authority_type.");
}

std::vector<domain::coding_scheme>
coding_scheme_repository::read_latest_by_subject_area(
    const std::string& subject_area_name,
    const std::string& domain_name) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest coding_schemes by subject_area: "
                               << subject_area_name << "/" << domain_name;

    const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto query = sqlgen::read<std::vector<coding_scheme_entity>> |
        where("subject_area_name"_c == subject_area_name &&
              "domain_name"_c == domain_name &&
              "valid_to"_c == max.value()) |
        order_by("code"_c);

    return execute_read_query<coding_scheme_entity, domain::coding_scheme>(
        ctx_, query,
        [](const auto& entities) { return coding_scheme_mapper::map(entities); },
        lg(), "Reading latest coding_schemes by subject_area.");
}

std::uint32_t coding_scheme_repository::get_total_count() {
    BOOST_LOG_SEV(lg(), debug) << "Retrieving total active coding_scheme count";

    const auto max(make_timestamp(MAX_TIMESTAMP, lg()));

    struct count_result {
        long long count;
    };

    const auto query = sqlgen::select_from<coding_scheme_entity>(
        sqlgen::count().as<"count">()) |
        where("valid_to"_c == max.value()) |
        sqlgen::to<count_result>;

    const auto r = sqlgen::session(ctx_.connection_pool()).and_then(query);
    ensure_success(r, lg());

    const auto count = static_cast<std::uint32_t>(r->count);
    BOOST_LOG_SEV(lg(), debug) << "Total active coding_scheme count: " << count;
    return count;
}

std::vector<domain::coding_scheme>
coding_scheme_repository::read_all(const std::string& code) {
    BOOST_LOG_SEV(lg(), debug) << "Reading all coding_scheme versions. Code: " << code;

    const auto query = sqlgen::read<std::vector<coding_scheme_entity>> |
        where("code"_c == code) |
        order_by("version"_c.desc());

    return execute_read_query<coding_scheme_entity, domain::coding_scheme>(
        ctx_, query,
        [](const auto& entities) { return coding_scheme_mapper::map(entities); },
        lg(), "Reading all coding_scheme versions by code.");
}

void coding_scheme_repository::remove(const std::string& code) {
    BOOST_LOG_SEV(lg(), debug) << "Removing coding_scheme from database: " << code;

    const auto query = sqlgen::delete_from<coding_scheme_entity> |
        where("code"_c == code);

    execute_delete_query(ctx_, query, lg(), "removing coding_scheme from database");
}

void coding_scheme_repository::remove(const std::vector<std::string>& codes) {
    const auto query = sqlgen::delete_from<coding_scheme_entity> |
        where("code"_c.in(codes));
    execute_delete_query(ctx_, query, lg(), "batch removing coding_schemes");
}

}
