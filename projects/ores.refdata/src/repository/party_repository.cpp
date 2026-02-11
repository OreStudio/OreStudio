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
#include "ores.refdata/repository/party_repository.hpp"

#include <sqlgen/postgres.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <boost/lexical_cast.hpp>
#include "ores.database/repository/helpers.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.database/repository/mapper_helpers.hpp"
#include "ores.refdata/domain/party_json_io.hpp" // IWYU pragma: keep.
#include "ores.refdata/repository/party_entity.hpp"
#include "ores.refdata/repository/party_mapper.hpp"

namespace ores::refdata::repository {

using namespace sqlgen;
using namespace sqlgen::literals;
using namespace ores::logging;
using namespace ores::database::repository;

std::string party_repository::sql() {
    return generate_create_table_sql<party_entity>(lg());
}

party_repository::party_repository(context ctx)
    : ctx_(std::move(ctx)) {}

void party_repository::write(const domain::party& party) {
    BOOST_LOG_SEV(lg(), debug) << "Writing party to database: "
                               << party.id;
    execute_write_query(ctx_, party_mapper::map(party),
        lg(), "writing party to database");
}

void party_repository::write(
    const std::vector<domain::party>& parties) {
    BOOST_LOG_SEV(lg(), debug) << "Writing parties to database. Count: "
                               << parties.size();
    execute_write_query(ctx_, party_mapper::map(parties),
        lg(), "writing parties to database");
}

std::vector<domain::party>
party_repository::read_latest() {
    static auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto query = sqlgen::read<std::vector<party_entity>> |
        where("valid_to"_c == max.value()) |
        order_by("full_name"_c);

    return execute_read_query<party_entity, domain::party>(
        ctx_, query,
        [](const auto& entities) { return party_mapper::map(entities); },
        lg(), "Reading latest parties");
}

std::vector<domain::party>
party_repository::read_latest(const boost::uuids::uuid& id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest party. Id: " << id;

    static auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto id_str = boost::uuids::to_string(id);
    const auto query = sqlgen::read<std::vector<party_entity>> |
        where("id"_c == id_str && "valid_to"_c == max.value());

    return execute_read_query<party_entity, domain::party>(
        ctx_, query,
        [](const auto& entities) { return party_mapper::map(entities); },
        lg(), "Reading latest party by id.");
}

std::vector<domain::party>
party_repository::read_latest_by_code(const std::string& code) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest party. Code: " << code;

    static auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto query = sqlgen::read<std::vector<party_entity>> |
        where("short_code"_c == code && "valid_to"_c == max.value());

    return execute_read_query<party_entity, domain::party>(
        ctx_, query,
        [](const auto& entities) { return party_mapper::map(entities); },
        lg(), "Reading latest party by code.");
}

std::vector<domain::party>
party_repository::read_system_party(const std::string& tenant_id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading system party for tenant: "
                               << tenant_id;

    const std::string sql =
        "SELECT * FROM ores_refdata_read_system_party_fn('" +
        tenant_id + "'::uuid)";

    const auto rows = execute_raw_multi_column_query(ctx_, sql, lg(),
        "Reading system party by tenant");

    std::vector<domain::party> result;
    result.reserve(rows.size());

    for (const auto& row : rows) {
        if (row.size() >= 16 && row[0] && row[1] && row[2] && row[3] &&
            row[4] && row[5] && row[6] && row[9] && row[10] && row[11] &&
            row[12] && row[13] && row[14]) {
            domain::party p;
            p.id = boost::lexical_cast<boost::uuids::uuid>(*row[0]);
            p.tenant_id = *row[1];
            p.version = std::stoi(*row[2]);
            p.full_name = *row[3];
            p.short_code = *row[4];
            p.party_category = *row[5];
            p.party_type = *row[6];
            if (row[7])
                p.parent_party_id = boost::lexical_cast<boost::uuids::uuid>(*row[7]);
            if (row[8])
                p.business_center_code = *row[8];
            p.status = *row[9];
            p.recorded_by = *row[10];
            p.performed_by = *row[11];
            p.change_reason_code = *row[12];
            p.change_commentary = *row[13];
            p.recorded_at = timestamp_to_timepoint(std::string_view{*row[14]});
            result.push_back(std::move(p));
        }
    }

    BOOST_LOG_SEV(lg(), debug) << "Read system party. Total: " << result.size();
    return result;
}

std::vector<domain::party>
party_repository::read_latest(std::uint32_t offset, std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest parties with offset: "
                               << offset << " and limit: " << limit;

    static auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto query = sqlgen::read<std::vector<party_entity>> |
        where("valid_to"_c == max.value()) |
        order_by("full_name"_c) |
        sqlgen::offset(offset) |
        sqlgen::limit(limit);

    return execute_read_query<party_entity, domain::party>(
        ctx_, query,
        [](const auto& entities) { return party_mapper::map(entities); },
        lg(), "Reading latest parties with pagination.");
}

std::uint32_t party_repository::get_total_party_count() {
    BOOST_LOG_SEV(lg(), debug) << "Retrieving total active party count";

    static auto max(make_timestamp(MAX_TIMESTAMP, lg()));

    struct count_result {
        long long count;
    };

    const auto query = sqlgen::select_from<party_entity>(
        sqlgen::count().as<"count">()) |
        where("valid_to"_c == max.value()) |
        sqlgen::to<count_result>;

    const auto r = sqlgen::session(ctx_.connection_pool()).and_then(query);
    ensure_success(r, lg());

    const auto count = static_cast<std::uint32_t>(r->count);
    BOOST_LOG_SEV(lg(), debug) << "Total active party count: " << count;
    return count;
}

std::vector<domain::party>
party_repository::read_all(const boost::uuids::uuid& id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading all party versions. Id: " << id;

    const auto id_str = boost::uuids::to_string(id);
    const auto query = sqlgen::read<std::vector<party_entity>> |
        where("id"_c == id_str) |
        order_by("version"_c.desc());

    return execute_read_query<party_entity, domain::party>(
        ctx_, query,
        [](const auto& entities) { return party_mapper::map(entities); },
        lg(), "Reading all party versions by id.");
}

void party_repository::remove(const boost::uuids::uuid& id) {
    BOOST_LOG_SEV(lg(), debug) << "Removing party from database: " << id;

    const auto id_str = boost::uuids::to_string(id);
    const auto query = sqlgen::delete_from<party_entity> |
        where("id"_c == id_str);

    execute_delete_query(ctx_, query, lg(), "removing party from database");
}

}
