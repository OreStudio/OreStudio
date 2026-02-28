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
#include "ores.refdata/repository/contact_type_repository.hpp"

#include <sqlgen/postgres.hpp>
#include "ores.database/repository/helpers.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.refdata/domain/contact_type_json_io.hpp" // IWYU pragma: keep.
#include "ores.refdata/repository/contact_type_entity.hpp"
#include "ores.refdata/repository/contact_type_mapper.hpp"

namespace ores::refdata::repository {

using namespace sqlgen;
using namespace sqlgen::literals;
using namespace ores::logging;
using namespace ores::database::repository;

std::string contact_type_repository::sql() {
    return generate_create_table_sql<contact_type_entity>(lg());
}

void contact_type_repository::
write(context ctx, const domain::contact_type& type) {
    BOOST_LOG_SEV(lg(), debug) << "Writing contact type to database: "
                               << type.code;
    execute_write_query(ctx, contact_type_mapper::map(type),
        lg(), "Writing contact type to database.");
}

void contact_type_repository::
write(context ctx, const std::vector<domain::contact_type>& types) {
    BOOST_LOG_SEV(lg(), debug) << "Writing contact types to database. Count: "
                               << types.size();
    execute_write_query(ctx, contact_type_mapper::map(types),
        lg(), "Writing contact types to database.");
}

std::vector<domain::contact_type>
contact_type_repository::read_latest(context ctx) {
    const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<contact_type_entity>> |
        where("tenant_id"_c == tid && "valid_to"_c == max.value()) |
        order_by("name"_c);

    return execute_read_query<contact_type_entity, domain::contact_type>(
        ctx, query,
        [](const auto& entities) {
            return contact_type_mapper::map(entities);
        },
        lg(), "Reading latest contact types");
}

std::vector<domain::contact_type>
contact_type_repository::read_latest(context ctx, const std::string& code) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest contact type. Code: " << code;

    const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<contact_type_entity>> |
        where("tenant_id"_c == tid && "code"_c == code && "valid_to"_c == max.value());

    return execute_read_query<contact_type_entity, domain::contact_type>(
        ctx, query,
        [](const auto& entities) {
            return contact_type_mapper::map(entities);
        },
        lg(), "Reading latest contact type by code.");
}

std::vector<domain::contact_type>
contact_type_repository::read_all(context ctx, const std::string& code) {
    BOOST_LOG_SEV(lg(), debug) << "Reading all contact type versions. Code: "
                               << code;

    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<contact_type_entity>> |
        where("tenant_id"_c == tid && "code"_c == code) |
        order_by("version"_c.desc());

    return execute_read_query<contact_type_entity, domain::contact_type>(
        ctx, query,
        [](const auto& entities) {
            return contact_type_mapper::map(entities);
        },
        lg(), "Reading all contact type versions by code.");
}

void contact_type_repository::remove(context ctx, const std::string& code) {
    BOOST_LOG_SEV(lg(), debug) << "Removing contact type from database: "
                               << code;

    const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::delete_from<contact_type_entity> |
        where("tenant_id"_c == tid && "code"_c == code && "valid_to"_c == max.value());

    execute_delete_query(ctx, query, lg(),
        "Removing contact type from database.");
}

void contact_type_repository::remove(context ctx,
    const std::vector<std::string>& codes) {
    const auto query = sqlgen::delete_from<contact_type_entity> |
        where("code"_c.in(codes));
    execute_delete_query(ctx, query, lg(), "batch removing contact_types");
}

}
