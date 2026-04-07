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
#include "ores.dq.core/repository/code_domain_repository.hpp"

#include <sqlgen/postgres.hpp>
#include "ores.database/repository/helpers.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.dq.api/domain/code_domain_json_io.hpp" // IWYU pragma: keep.
#include "ores.dq.core/repository/code_domain_entity.hpp"
#include "ores.dq.core/repository/code_domain_mapper.hpp"

namespace ores::dq::repository {

using namespace sqlgen;
using namespace sqlgen::literals;
using namespace ores::logging;
using namespace ores::database::repository;

std::string code_domain_repository::sql() {
    return generate_create_table_sql<code_domain_entity>(lg());
}

void code_domain_repository::write(context ctx, const domain::code_domain& v) {
    BOOST_LOG_SEV(lg(), debug) << "Writing code domain: " << v.code;
    execute_write_query(ctx, code_domain_mapper::map(v),
        lg(), "Writing code domain to database.");
}

void code_domain_repository::write(
    context ctx, const std::vector<domain::code_domain>& v) {
    BOOST_LOG_SEV(lg(), debug) << "Writing code domains. Count: " << v.size();
    execute_write_query(ctx, code_domain_mapper::map(v),
        lg(), "Writing code domains to database.");
}

std::vector<domain::code_domain>
code_domain_repository::read_latest(context ctx) {
    static auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<code_domain_entity>> |
        where("tenant_id"_c == tid && "valid_to"_c == max.value()) |
        order_by("code"_c);

    return execute_read_query<code_domain_entity, domain::code_domain>(
        ctx, query,
        [](const auto& entities) { return code_domain_mapper::map(entities); },
        lg(), "Reading latest code domains");
}

std::vector<domain::code_domain>
code_domain_repository::read_latest(context ctx, const std::string& code) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest code domain. code: " << code;
    static auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<code_domain_entity>> |
        where("tenant_id"_c == tid && "code"_c == code && "valid_to"_c == max.value());

    return execute_read_query<code_domain_entity, domain::code_domain>(
        ctx, query,
        [](const auto& entities) { return code_domain_mapper::map(entities); },
        lg(), "Reading latest code domain by code.");
}

std::vector<domain::code_domain>
code_domain_repository::read_all(context ctx, const std::string& code) {
    BOOST_LOG_SEV(lg(), debug) << "Reading all code domain versions. code: " << code;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<code_domain_entity>> |
        where("tenant_id"_c == tid && "code"_c == code) |
        order_by("version"_c.desc());

    return execute_read_query<code_domain_entity, domain::code_domain>(
        ctx, query,
        [](const auto& entities) { return code_domain_mapper::map(entities); },
        lg(), "Reading all code domain versions by code.");
}

void code_domain_repository::remove(context ctx, const std::string& code) {
    BOOST_LOG_SEV(lg(), debug) << "Removing code domain: " << code;
    static auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::delete_from<code_domain_entity> |
        where("tenant_id"_c == tid && "code"_c == code && "valid_to"_c == max.value());

    execute_delete_query(ctx, query, lg(), "Removing code domain from database.");
}

}
