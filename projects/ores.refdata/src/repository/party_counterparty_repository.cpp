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
#include "ores.refdata/repository/party_counterparty_repository.hpp"

#include <sqlgen/postgres.hpp>
#include <boost/uuid/uuid_io.hpp>
#include "ores.database/repository/helpers.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.refdata/domain/party_counterparty_json_io.hpp" // IWYU pragma: keep.
#include "ores.refdata/repository/party_counterparty_entity.hpp"
#include "ores.refdata/repository/party_counterparty_mapper.hpp"

namespace ores::refdata::repository {

using namespace sqlgen;
using namespace sqlgen::literals;
using namespace ores::logging;
using namespace ores::database::repository;

std::string party_counterparty_repository::sql() {
    return generate_create_table_sql<party_counterparty_entity>(lg());
}

party_counterparty_repository::party_counterparty_repository(context ctx)
    : ctx_(std::move(ctx)) {}

void party_counterparty_repository::write(
    const domain::party_counterparty& party_counterparty) {
    BOOST_LOG_SEV(lg(), debug) << "Writing party counterparty to database: "
                               << party_counterparty.party_id << "/"
                               << party_counterparty.counterparty_id;
    execute_write_query(ctx_, party_counterparty_mapper::map(party_counterparty),
        lg(), "writing party counterparty to database");
}

void party_counterparty_repository::write(
    const std::vector<domain::party_counterparty>& party_counterparties) {
    BOOST_LOG_SEV(lg(), debug) << "Writing party counterparties to database. Count: "
                               << party_counterparties.size();
    execute_write_query(ctx_, party_counterparty_mapper::map(party_counterparties),
        lg(), "writing party counterparties to database");
}

std::vector<domain::party_counterparty>
party_counterparty_repository::read_latest() {
    const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto query = sqlgen::read<std::vector<party_counterparty_entity>> |
        where("valid_to"_c == max.value()) |
        order_by("party_id"_c, "counterparty_id"_c);

    return execute_read_query<party_counterparty_entity, domain::party_counterparty>(
        ctx_, query,
        [](const auto& entities) { return party_counterparty_mapper::map(entities); },
        lg(), "Reading latest party counterparties");
}

std::vector<domain::party_counterparty>
party_counterparty_repository::read_latest_by_party(
    const boost::uuids::uuid& party_id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest party counterparties. Party: "
                               << party_id;

    const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto party_id_str = boost::uuids::to_string(party_id);
    const auto query = sqlgen::read<std::vector<party_counterparty_entity>> |
        where("party_id"_c == party_id_str && "valid_to"_c == max.value()) |
        order_by("counterparty_id"_c);

    return execute_read_query<party_counterparty_entity, domain::party_counterparty>(
        ctx_, query,
        [](const auto& entities) { return party_counterparty_mapper::map(entities); },
        lg(), "Reading latest party counterparties by party.");
}

std::vector<domain::party_counterparty>
party_counterparty_repository::read_latest_by_counterparty(
    const boost::uuids::uuid& counterparty_id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest party counterparties. Counterparty: "
                               << counterparty_id;

    const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto counterparty_id_str = boost::uuids::to_string(counterparty_id);
    const auto query = sqlgen::read<std::vector<party_counterparty_entity>> |
        where("counterparty_id"_c == counterparty_id_str && "valid_to"_c == max.value()) |
        order_by("party_id"_c);

    return execute_read_query<party_counterparty_entity, domain::party_counterparty>(
        ctx_, query,
        [](const auto& entities) { return party_counterparty_mapper::map(entities); },
        lg(), "Reading latest party counterparties by counterparty.");
}

void party_counterparty_repository::remove(
    const boost::uuids::uuid& party_id,
    const boost::uuids::uuid& counterparty_id) {
    BOOST_LOG_SEV(lg(), debug) << "Removing party counterparty from database: "
                               << party_id << "/" << counterparty_id;

    const auto party_id_str = boost::uuids::to_string(party_id);
    const auto counterparty_id_str = boost::uuids::to_string(counterparty_id);
    const auto query = sqlgen::delete_from<party_counterparty_entity> |
        where("party_id"_c == party_id_str &&
              "counterparty_id"_c == counterparty_id_str);

    execute_delete_query(ctx_, query, lg(),
        "removing party counterparty from database");
}

void party_counterparty_repository::remove_by_party(
    const boost::uuids::uuid& party_id) {
    BOOST_LOG_SEV(lg(), debug) << "Removing all party counterparties from database: "
                               << party_id;

    const auto party_id_str = boost::uuids::to_string(party_id);
    const auto query = sqlgen::delete_from<party_counterparty_entity> |
        where("party_id"_c == party_id_str);

    execute_delete_query(ctx_, query, lg(),
        "removing all party counterparties from database");
}

}
