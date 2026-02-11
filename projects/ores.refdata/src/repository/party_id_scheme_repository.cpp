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
#include "ores.refdata/repository/party_id_scheme_repository.hpp"

#include <sqlgen/postgres.hpp>
#include "ores.database/repository/helpers.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.refdata/domain/party_id_scheme_json_io.hpp" // IWYU pragma: keep.
#include "ores.refdata/repository/party_id_scheme_entity.hpp"
#include "ores.refdata/repository/party_id_scheme_mapper.hpp"

namespace ores::refdata::repository {

using namespace sqlgen;
using namespace sqlgen::literals;
using namespace ores::logging;
using namespace ores::database::repository;

std::string party_id_scheme_repository::sql() {
    return generate_create_table_sql<party_id_scheme_entity>(lg());
}

void party_id_scheme_repository::
write(context ctx, const domain::party_id_scheme& scheme) {
    BOOST_LOG_SEV(lg(), debug) << "Writing party ID scheme to database: "
                               << scheme.code;
    execute_write_query(ctx, party_id_scheme_mapper::map(scheme),
        lg(), "Writing party ID scheme to database.");
}

void party_id_scheme_repository::
write(context ctx, const std::vector<domain::party_id_scheme>& schemes) {
    BOOST_LOG_SEV(lg(), debug) << "Writing party ID schemes to database. Count: "
                               << schemes.size();
    execute_write_query(ctx, party_id_scheme_mapper::map(schemes),
        lg(), "Writing party ID schemes to database.");
}

std::vector<domain::party_id_scheme>
party_id_scheme_repository::read_latest(context ctx) {
    static auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<party_id_scheme_entity>> |
        where("tenant_id"_c == tid && "valid_to"_c == max.value()) |
        order_by("name"_c);

    return execute_read_query<party_id_scheme_entity, domain::party_id_scheme>(
        ctx, query,
        [](const auto& entities) {
            return party_id_scheme_mapper::map(entities);
        },
        lg(), "Reading latest party ID schemes");
}

std::vector<domain::party_id_scheme>
party_id_scheme_repository::read_latest(context ctx, const std::string& code) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest party ID scheme. Code: "
                               << code;

    static auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<party_id_scheme_entity>> |
        where("tenant_id"_c == tid && "code"_c == code && "valid_to"_c == max.value());

    return execute_read_query<party_id_scheme_entity, domain::party_id_scheme>(
        ctx, query,
        [](const auto& entities) {
            return party_id_scheme_mapper::map(entities);
        },
        lg(), "Reading latest party ID scheme by code.");
}

std::vector<domain::party_id_scheme>
party_id_scheme_repository::read_all(context ctx, const std::string& code) {
    BOOST_LOG_SEV(lg(), debug) << "Reading all party ID scheme versions. Code: "
                               << code;

    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<party_id_scheme_entity>> |
        where("tenant_id"_c == tid && "code"_c == code) |
        order_by("version"_c.desc());

    return execute_read_query<party_id_scheme_entity, domain::party_id_scheme>(
        ctx, query,
        [](const auto& entities) {
            return party_id_scheme_mapper::map(entities);
        },
        lg(), "Reading all party ID scheme versions by code.");
}

void party_id_scheme_repository::remove(context ctx, const std::string& code) {
    BOOST_LOG_SEV(lg(), debug) << "Removing party ID scheme from database: "
                               << code;

    static auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::delete_from<party_id_scheme_entity> |
        where("tenant_id"_c == tid && "code"_c == code && "valid_to"_c == max.value());

    execute_delete_query(ctx, query, lg(),
        "Removing party ID scheme from database.");
}

}
