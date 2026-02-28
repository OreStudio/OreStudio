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
#include "ores.refdata/repository/portfolio_repository.hpp"

#include <sqlgen/postgres.hpp>
#include <boost/uuid/uuid_io.hpp>
#include "ores.database/repository/helpers.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.refdata/domain/portfolio_json_io.hpp" // IWYU pragma: keep.
#include "ores.refdata/repository/portfolio_entity.hpp"
#include "ores.refdata/repository/portfolio_mapper.hpp"

namespace ores::refdata::repository {

using namespace sqlgen;
using namespace sqlgen::literals;
using namespace ores::logging;
using namespace ores::database::repository;

std::string portfolio_repository::sql() {
    return generate_create_table_sql<portfolio_entity>(lg());
}

portfolio_repository::portfolio_repository(context ctx)
    : ctx_(std::move(ctx)) {}

void portfolio_repository::write(const domain::portfolio& portfolio) {
    BOOST_LOG_SEV(lg(), debug) << "Writing portfolio to database: "
                               << portfolio.id;
    execute_write_query(ctx_, portfolio_mapper::map(portfolio),
        lg(), "writing portfolio to database");
}

void portfolio_repository::write(
    const std::vector<domain::portfolio>& portfolios) {
    BOOST_LOG_SEV(lg(), debug) << "Writing portfolios to database. Count: "
                               << portfolios.size();
    execute_write_query(ctx_, portfolio_mapper::map(portfolios),
        lg(), "writing portfolios to database");
}

std::vector<domain::portfolio>
portfolio_repository::read_latest() {
    const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto query = sqlgen::read<std::vector<portfolio_entity>> |
        where("valid_to"_c == max.value()) |
        order_by("name"_c);

    return execute_read_query<portfolio_entity, domain::portfolio>(
        ctx_, query,
        [](const auto& entities) { return portfolio_mapper::map(entities); },
        lg(), "Reading latest portfolios");
}

std::vector<domain::portfolio>
portfolio_repository::read_latest(const boost::uuids::uuid& id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest portfolio. Id: " << id;

    const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto id_str = boost::uuids::to_string(id);
    const auto query = sqlgen::read<std::vector<portfolio_entity>> |
        where("id"_c == id_str && "valid_to"_c == max.value());

    return execute_read_query<portfolio_entity, domain::portfolio>(
        ctx_, query,
        [](const auto& entities) { return portfolio_mapper::map(entities); },
        lg(), "Reading latest portfolio by id.");
}

std::vector<domain::portfolio>
portfolio_repository::read_latest_by_code(const std::string& code) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest portfolio. Code: " << code;

    const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto query = sqlgen::read<std::vector<portfolio_entity>> |
        where("name"_c == code && "valid_to"_c == max.value());

    return execute_read_query<portfolio_entity, domain::portfolio>(
        ctx_, query,
        [](const auto& entities) { return portfolio_mapper::map(entities); },
        lg(), "Reading latest portfolio by code.");
}

std::vector<domain::portfolio>
portfolio_repository::read_all(const boost::uuids::uuid& id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading all portfolio versions. Id: " << id;

    const auto id_str = boost::uuids::to_string(id);
    const auto query = sqlgen::read<std::vector<portfolio_entity>> |
        where("id"_c == id_str) |
        order_by("version"_c.desc());

    return execute_read_query<portfolio_entity, domain::portfolio>(
        ctx_, query,
        [](const auto& entities) { return portfolio_mapper::map(entities); },
        lg(), "Reading all portfolio versions by id.");
}

void portfolio_repository::remove(const boost::uuids::uuid& id) {
    BOOST_LOG_SEV(lg(), debug) << "Removing portfolio from database: " << id;

    const auto id_str = boost::uuids::to_string(id);
    const auto query = sqlgen::delete_from<portfolio_entity> |
        where("id"_c == id_str);

    execute_delete_query(ctx_, query, lg(), "removing portfolio from database");
}

}
