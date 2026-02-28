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
#include "ores.refdata/repository/counterparty_repository.hpp"

#include <sqlgen/postgres.hpp>
#include <boost/uuid/uuid_io.hpp>
#include "ores.database/repository/helpers.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.refdata/domain/counterparty_json_io.hpp" // IWYU pragma: keep.
#include "ores.refdata/repository/counterparty_entity.hpp"
#include "ores.refdata/repository/counterparty_mapper.hpp"

namespace ores::refdata::repository {

using namespace sqlgen;
using namespace sqlgen::literals;
using namespace ores::logging;
using namespace ores::database::repository;

std::string counterparty_repository::sql() {
    return generate_create_table_sql<counterparty_entity>(lg());
}

counterparty_repository::counterparty_repository(context ctx)
    : ctx_(std::move(ctx)) {}

void counterparty_repository::write(const domain::counterparty& counterparty) {
    BOOST_LOG_SEV(lg(), debug) << "Writing counterparty to database: "
                               << counterparty.id;
    execute_write_query(ctx_, counterparty_mapper::map(counterparty),
        lg(), "writing counterparty to database");
}

void counterparty_repository::write(
    const std::vector<domain::counterparty>& counterparties) {
    BOOST_LOG_SEV(lg(), debug) << "Writing counterparties to database. Count: "
                               << counterparties.size();
    execute_write_query(ctx_, counterparty_mapper::map(counterparties),
        lg(), "writing counterparties to database");
}

std::vector<domain::counterparty>
counterparty_repository::read_latest() {
    const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto query = sqlgen::read<std::vector<counterparty_entity>> |
        where("valid_to"_c == max.value()) |
        order_by("full_name"_c);

    return execute_read_query<counterparty_entity, domain::counterparty>(
        ctx_, query,
        [](const auto& entities) { return counterparty_mapper::map(entities); },
        lg(), "Reading latest counterparties");
}

std::vector<domain::counterparty>
counterparty_repository::read_latest(std::uint32_t offset,
                                      std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest counterparties with offset: "
                               << offset << " and limit: " << limit;

    const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto query = sqlgen::read<std::vector<counterparty_entity>> |
        where("valid_to"_c == max.value()) |
        order_by("full_name"_c) |
        sqlgen::offset(offset) |
        sqlgen::limit(limit);

    return execute_read_query<counterparty_entity, domain::counterparty>(
        ctx_, query,
        [](const auto& entities) { return counterparty_mapper::map(entities); },
        lg(), "Reading latest counterparties with pagination.");
}

std::uint32_t counterparty_repository::get_total_count() {
    BOOST_LOG_SEV(lg(), debug) << "Retrieving total active counterparty count";

    const auto max(make_timestamp(MAX_TIMESTAMP, lg()));

    struct count_result {
        long long count;
    };

    const auto query = sqlgen::select_from<counterparty_entity>(
        sqlgen::count().as<"count">()) |
        where("valid_to"_c == max.value()) |
        sqlgen::to<count_result>;

    const auto r = sqlgen::session(ctx_.connection_pool()).and_then(query);
    ensure_success(r, lg());

    const auto count = static_cast<std::uint32_t>(r->count);
    BOOST_LOG_SEV(lg(), debug) << "Total active counterparty count: " << count;
    return count;
}

std::vector<domain::counterparty>
counterparty_repository::read_latest(const boost::uuids::uuid& id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest counterparty. Id: " << id;

    const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto id_str = boost::uuids::to_string(id);
    const auto query = sqlgen::read<std::vector<counterparty_entity>> |
        where("id"_c == id_str && "valid_to"_c == max.value());

    return execute_read_query<counterparty_entity, domain::counterparty>(
        ctx_, query,
        [](const auto& entities) { return counterparty_mapper::map(entities); },
        lg(), "Reading latest counterparty by id.");
}

std::vector<domain::counterparty>
counterparty_repository::read_latest_by_code(const std::string& code) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest counterparty. Code: " << code;

    const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto query = sqlgen::read<std::vector<counterparty_entity>> |
        where("short_code"_c == code && "valid_to"_c == max.value());

    return execute_read_query<counterparty_entity, domain::counterparty>(
        ctx_, query,
        [](const auto& entities) { return counterparty_mapper::map(entities); },
        lg(), "Reading latest counterparty by code.");
}

std::vector<domain::counterparty>
counterparty_repository::read_all(const boost::uuids::uuid& id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading all counterparty versions. Id: " << id;

    const auto id_str = boost::uuids::to_string(id);
    const auto query = sqlgen::read<std::vector<counterparty_entity>> |
        where("id"_c == id_str) |
        order_by("version"_c.desc());

    return execute_read_query<counterparty_entity, domain::counterparty>(
        ctx_, query,
        [](const auto& entities) { return counterparty_mapper::map(entities); },
        lg(), "Reading all counterparty versions by id.");
}

void counterparty_repository::remove(const boost::uuids::uuid& id) {
    BOOST_LOG_SEV(lg(), debug) << "Removing counterparty from database: " << id;

    const auto id_str = boost::uuids::to_string(id);
    const auto query = sqlgen::delete_from<counterparty_entity> |
        where("id"_c == id_str);

    execute_delete_query(ctx_, query, lg(), "removing counterparty from database");
}

}
