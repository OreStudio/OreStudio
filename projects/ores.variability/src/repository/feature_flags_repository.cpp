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

#include <format>
#include "ores.utility/repository/repository_exception.hpp"
#include "ores.variability/domain/feature_flags_json_io.hpp" // IWYU pragma: keep.
#include "ores.variability/repository/feature_flags_entity.hpp"
#include "ores.variability/repository/feature_flags_mapper.hpp"

namespace ores::variability::repository {

using namespace sqlgen;
using namespace sqlgen::literals;
using namespace ores::utility::log;
using namespace ores::utility::repository;

std::string feature_flags_repository::sql() {
    return generate_create_table_sql<feature_flags_entity>(
        "ores.variability.repository.feature_flags_repository");
}

feature_flags_repository::feature_flags_repository(context ctx)
    : ctx_(std::move(ctx)) {}

void feature_flags_repository::
write(const domain::feature_flags& flag) {
    BOOST_LOG_SEV(lg(), debug) << "Writing feature flag to database: " << flag;

    execute_write_query(ctx_,
        feature_flags_mapper::map(flag),
        "ores.variability.repository.feature_flags_repository",
        "writing feature flag to database");
}

void feature_flags_repository::
write(const std::vector<domain::feature_flags>& flags) {
    BOOST_LOG_SEV(lg(), debug) << "Writing feature flags to database. Count: "
                             << flags.size();

    execute_write_query(ctx_,
        feature_flags_mapper::map(flags),
        "ores.variability.repository.feature_flags_repository",
        "writing feature flags to database");
}

std::vector<domain::feature_flags> feature_flags_repository::read_latest() {
    static auto max(make_timestamp(MAX_TIMESTAMP));
    const auto query = sqlgen::read<std::vector<feature_flags_entity>> |
        where("valid_to"_c == max.value()) |
        order_by("valid_from"_c.desc());

    return execute_read_query<feature_flags_entity, domain::feature_flags>(ctx_, query,
        [](const auto& entities) { return feature_flags_mapper::map(entities); },
        "ores.variability.repository.feature_flags_repository",
        "Reading latest feature flags");
}

std::vector<domain::feature_flags>
feature_flags_repository::read_latest(const std::string& name) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest feature flag by name: " << name;

    static auto max(make_timestamp(MAX_TIMESTAMP));
    const auto query = sqlgen::read<std::vector<feature_flags_entity>> |
        where("name"_c == name && "valid_to"_c == max.value()) |
        order_by("valid_from"_c.desc());

    return execute_read_query<feature_flags_entity, domain::feature_flags>(ctx_, query,
        [](const auto& entities) { return feature_flags_mapper::map(entities); },
        "ores.variability.repository.feature_flags_repository",
        "Reading latest feature flag by name");
}

std::vector<domain::feature_flags> feature_flags_repository::read_all() {
    const auto query = sqlgen::read<std::vector<feature_flags_entity>> |
        order_by("valid_from"_c.desc());

    return execute_read_query<feature_flags_entity, domain::feature_flags>(ctx_, query,
        [](const auto& entities) { return feature_flags_mapper::map(entities); },
        "ores.variability.repository.feature_flags_repository",
        "Reading all feature flags");
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
        "ores.variability.repository.feature_flags_repository",
        "Reading all feature flags by name");
}

void feature_flags_repository::remove(const std::string& name) {
    BOOST_LOG_SEV(lg(), debug) << "Removing feature flag from database: " << name;

    // Delete the feature flag - the database trigger will close the temporal record
    // instead of actually deleting it (sets valid_to = current_timestamp)
    const auto query = sqlgen::delete_from<feature_flags_entity> |
        where("name"_c == name);

    execute_delete_query(ctx_, query,
        "ores.variability.repository.feature_flags_repository",
        "removing feature flag from database");
}

}
