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
#include "ores.dq/repository/subject_area_repository.hpp"

#include <sqlgen/postgres.hpp>
#include "ores.database/repository/helpers.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.dq/domain/subject_area_json_io.hpp" // IWYU pragma: keep.
#include "ores.dq/repository/subject_area_entity.hpp"
#include "ores.dq/repository/subject_area_mapper.hpp"

namespace ores::dq::repository {

using namespace sqlgen;
using namespace sqlgen::literals;
using namespace ores::logging;
using namespace ores::database::repository;

std::string subject_area_repository::sql() {
    return generate_create_table_sql<subject_area_entity>(lg());
}

subject_area_repository::subject_area_repository(context ctx)
    : ctx_(std::move(ctx)) {}

void subject_area_repository::write(const domain::subject_area& subject_area) {
    BOOST_LOG_SEV(lg(), debug) << "Writing subject_area to database: "
                               << subject_area.name << "/" << subject_area.domain_name;

    execute_write_query(ctx_, subject_area_mapper::map(subject_area),
        lg(), "writing subject_area to database");
}

void subject_area_repository::write(
    const std::vector<domain::subject_area>& subject_areas) {
    BOOST_LOG_SEV(lg(), debug) << "Writing subject_areas to database. Count: "
                               << subject_areas.size();

    execute_write_query(ctx_, subject_area_mapper::map(subject_areas),
        lg(), "writing subject_areas to database");
}

std::vector<domain::subject_area>
subject_area_repository::read_latest() {
    const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto query = sqlgen::read<std::vector<subject_area_entity>> |
        where("valid_to"_c == max.value()) |
        order_by("domain_name"_c, "name"_c);

    return execute_read_query<subject_area_entity, domain::subject_area>(
        ctx_, query,
        [](const auto& entities) { return subject_area_mapper::map(entities); },
        lg(), "Reading latest subject_areas");
}

std::vector<domain::subject_area>
subject_area_repository::read_latest(const std::string& name,
                                      const std::string& domain_name) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest subject_area. Name: " << name
                               << ", Domain: " << domain_name;

    const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto query = sqlgen::read<std::vector<subject_area_entity>> |
        where("name"_c == name && "domain_name"_c == domain_name &&
              "valid_to"_c == max.value());

    return execute_read_query<subject_area_entity, domain::subject_area>(
        ctx_, query,
        [](const auto& entities) { return subject_area_mapper::map(entities); },
        lg(), "Reading latest subject_area by composite key.");
}

std::vector<domain::subject_area>
subject_area_repository::read_latest_by_domain(const std::string& domain_name) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest subject_areas by domain: "
                               << domain_name;

    const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto query = sqlgen::read<std::vector<subject_area_entity>> |
        where("domain_name"_c == domain_name && "valid_to"_c == max.value()) |
        order_by("name"_c);

    return execute_read_query<subject_area_entity, domain::subject_area>(
        ctx_, query,
        [](const auto& entities) { return subject_area_mapper::map(entities); },
        lg(), "Reading latest subject_areas by domain.");
}

std::vector<domain::subject_area>
subject_area_repository::read_latest(std::uint32_t offset, std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest subject_areas with offset: "
                               << offset << " and limit: " << limit;

    const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto query = sqlgen::read<std::vector<subject_area_entity>> |
        where("valid_to"_c == max.value()) |
        order_by("domain_name"_c, "name"_c) |
        sqlgen::offset(offset) |
        sqlgen::limit(limit);

    return execute_read_query<subject_area_entity, domain::subject_area>(
        ctx_, query,
        [](const auto& entities) { return subject_area_mapper::map(entities); },
        lg(), "Reading latest subject_areas with pagination.");
}

std::uint32_t subject_area_repository::get_total_count() {
    BOOST_LOG_SEV(lg(), debug) << "Retrieving total active subject_area count";

    const auto max(make_timestamp(MAX_TIMESTAMP, lg()));

    struct count_result {
        long long count;
    };

    const auto query = sqlgen::select_from<subject_area_entity>(
        sqlgen::count().as<"count">()) |
        where("valid_to"_c == max.value()) |
        sqlgen::to<count_result>;

    const auto r = sqlgen::session(ctx_.connection_pool()).and_then(query);
    ensure_success(r, lg());

    const auto count = static_cast<std::uint32_t>(r->count);
    BOOST_LOG_SEV(lg(), debug) << "Total active subject_area count: " << count;
    return count;
}

std::vector<domain::subject_area>
subject_area_repository::read_all(const std::string& name,
                                   const std::string& domain_name) {
    BOOST_LOG_SEV(lg(), debug) << "Reading all subject_area versions. Name: "
                               << name << ", Domain: " << domain_name;

    const auto query = sqlgen::read<std::vector<subject_area_entity>> |
        where("name"_c == name && "domain_name"_c == domain_name) |
        order_by("version"_c.desc());

    return execute_read_query<subject_area_entity, domain::subject_area>(
        ctx_, query,
        [](const auto& entities) { return subject_area_mapper::map(entities); },
        lg(), "Reading all subject_area versions by composite key.");
}

void subject_area_repository::remove(const std::string& name,
                                      const std::string& domain_name) {
    BOOST_LOG_SEV(lg(), debug) << "Removing subject_area from database: "
                               << name << "/" << domain_name;

    const auto query = sqlgen::delete_from<subject_area_entity> |
        where("name"_c == name && "domain_name"_c == domain_name);

    execute_delete_query(ctx_, query, lg(), "removing subject_area from database");
}

}
