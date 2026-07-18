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
#include "ores.dq.core/repository/badge_mapping_repository.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.database/repository/helpers.hpp"
#include "ores.dq.api/domain/badge_mapping_json_io.hpp" // IWYU pragma: keep.
#include "ores.dq.core/repository/badge_mapping_entity.hpp"
#include "ores.dq.core/repository/badge_mapping_mapper.hpp"
#include <sqlgen/postgres.hpp>

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

void badge_mapping_repository::write(const domain::badge_mapping& mapping) {
    BOOST_LOG_SEV(lg(), debug) << "Writing badge mapping to database: " << mapping.code_domain_code
                               << "/" << mapping.entity_code;
    execute_write_query(
        ctx_, badge_mapping_mapper::map(mapping), lg(), "writing badge mapping to database");
}

void badge_mapping_repository::write(const std::vector<domain::badge_mapping>& mappings) {
    BOOST_LOG_SEV(lg(), debug) << "Writing badge mappings to database. Count: " << mappings.size();
    execute_write_query(
        ctx_, badge_mapping_mapper::map(mappings), lg(), "writing badge mappings to database");
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

    return execute_read_query<badge_mapping_entity, domain::badge_mapping>(
        ctx_,
        query,
        [](const auto& entities) { return badge_mapping_mapper::map(entities); },
        lg(),
        "Reading latest badge mappings by code_domain.");
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

    return execute_read_query<badge_mapping_entity, domain::badge_mapping>(
        ctx_,
        query,
        [](const auto& entities) { return badge_mapping_mapper::map(entities); },
        lg(),
        "Reading latest badge mappings by entity.");
}

void badge_mapping_repository::remove(const std::string& code_domain_code,
                                      const std::string& entity_code) {
    BOOST_LOG_SEV(lg(), debug) << "Removing badge mapping from database: " << code_domain_code
                               << "/" << entity_code;

    const auto tid = ctx_.tenant_id().to_string();
    const auto query = sqlgen::delete_from<badge_mapping_entity> |
                       where("tenant_id"_c == tid && "code_domain_code"_c == code_domain_code &&
                             "entity_code"_c == entity_code);

    execute_delete_query(ctx_, query, lg(), "removing badge mapping from database");
}

void badge_mapping_repository::remove_by_code_domain(const std::string& code_domain_code) {
    BOOST_LOG_SEV(lg(), debug) << "Removing all badge mappings from database: " << code_domain_code;

    const auto tid = ctx_.tenant_id().to_string();
    const auto query = sqlgen::delete_from<badge_mapping_entity> |
                       where("tenant_id"_c == tid && "code_domain_code"_c == code_domain_code);

    execute_delete_query(ctx_, query, lg(), "removing all badge mappings from database");
}

}
