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
#include "ores.variability/repository/feature_flags_repository.hpp"

#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.variability/domain/feature_flags_json_io.hpp" // IWYU pragma: keep.
#include "ores.variability/repository/feature_flags_entity.hpp"
#include "ores.variability/repository/feature_flags_mapper.hpp"

namespace ores::variability::repository {

using namespace sqlgen;
using namespace sqlgen::literals;
using namespace ores::logging;
using namespace ores::database::repository;

std::string feature_flags_repository::sql() {
    return generate_create_table_sql<feature_flags_entity>(lg());
}

feature_flags_repository::feature_flags_repository(context ctx)
    : ctx_(std::move(ctx)) {}

void feature_flags_repository::
write(const domain::feature_flags& flag) {
    BOOST_LOG_SEV(lg(), debug) << "Writing feature flag to database: " << flag;

    execute_write_query(ctx_, feature_flags_mapper::map(flag),
        lg(), "writing feature flag to database");
}

void feature_flags_repository::
write(const std::vector<domain::feature_flags>& flags) {
    BOOST_LOG_SEV(lg(), debug) << "Writing feature flags to database. Count: "
                             << flags.size();

    execute_write_query(ctx_, feature_flags_mapper::map(flags),
        lg(), "Writing feature flags to database.");
}

std::vector<domain::feature_flags> feature_flags_repository::read_latest() {
    static auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto query = sqlgen::read<std::vector<feature_flags_entity>> |
        where("valid_to"_c == max.value()) |
        order_by("valid_from"_c.desc());

    return execute_read_query<feature_flags_entity, domain::feature_flags>(ctx_, query,
        [](const auto& entities) { return feature_flags_mapper::map(entities); },
        lg(), "Reading latest feature flags");
}

std::vector<domain::feature_flags>
feature_flags_repository::read_latest(const std::string& name) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest feature flag by name: " << name;

    static auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto query = sqlgen::read<std::vector<feature_flags_entity>> |
        where("name"_c == name && "valid_to"_c == max.value()) |
        order_by("valid_from"_c.desc());

    return execute_read_query<feature_flags_entity, domain::feature_flags>(ctx_, query,
        [](const auto& entities) { return feature_flags_mapper::map(entities); },
        lg(), "Reading latest feature flag by name");
}

std::vector<domain::feature_flags>
feature_flags_repository::read_latest(std::uint32_t offset, std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest feature flags with offset: "
                               << offset << " and limit: " << limit;

    static auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto query = sqlgen::read<std::vector<feature_flags_entity>> |
        where("valid_to"_c == max.value()) |
        order_by("valid_from"_c.desc()) |
        sqlgen::offset(offset) |
        sqlgen::limit(limit);

    return execute_read_query<feature_flags_entity, domain::feature_flags>(ctx_, query,
        [](const auto& entities) { return feature_flags_mapper::map(entities); },
        lg(), "Reading latest feature flags with pagination.");
}

std::uint32_t feature_flags_repository::get_total_feature_flags_count() {
    BOOST_LOG_SEV(lg(), debug) << "Retrieving total active feature flags count";

    static auto max(make_timestamp(MAX_TIMESTAMP, lg()));

    struct count_result {
        long long count;
    };

    const auto query = sqlgen::select_from<feature_flags_entity>(
        sqlgen::count().as<"count">()) |
        where("valid_to"_c == max.value()) |
        sqlgen::to<count_result>;

    const auto r = sqlgen::session(ctx_.connection_pool()).and_then(query);
    ensure_success(r, lg());

    const auto count = static_cast<std::uint32_t>(r->count);
    BOOST_LOG_SEV(lg(), debug) << "Total active feature flags count: " << count;
    return count;
}

std::vector<domain::feature_flags> feature_flags_repository::read_all() {
    const auto query = sqlgen::read<std::vector<feature_flags_entity>> |
        order_by("valid_from"_c.desc());

    return execute_read_query<feature_flags_entity, domain::feature_flags>(ctx_, query,
        [](const auto& entities) { return feature_flags_mapper::map(entities); },
        lg(), "Reading all feature flags");
}

std::vector<domain::feature_flags>
feature_flags_repository::read_all(const std::string& name) {
    BOOST_LOG_SEV(lg(), debug) << "Reading all versions of feature flag by name: "
                               << name;

    const auto query = sqlgen::read<std::vector<feature_flags_entity>> |
        where("name"_c == name) |
        order_by("valid_from"_c.desc());

    return execute_read_query<feature_flags_entity, domain::feature_flags>(ctx_, query,
        [](const auto& entities) { return feature_flags_mapper::map(entities); },
        lg(), "Reading all feature flags by name.");
}

void feature_flags_repository::remove(const std::string& name) {
    BOOST_LOG_SEV(lg(), debug) << "Removing feature flag from database: " << name;

    // Delete the feature flag - the database trigger will close the temporal record
    // instead of actually deleting it (sets valid_to = current_timestamp)
    const auto query = sqlgen::delete_from<feature_flags_entity> |
        where("name"_c == name);

    execute_delete_query(ctx_, query, lg(),
        "Removing feature flag from database.");
}

}
