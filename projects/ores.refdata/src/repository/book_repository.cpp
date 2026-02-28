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
#include "ores.refdata/repository/book_repository.hpp"

#include <sqlgen/postgres.hpp>
#include <boost/uuid/uuid_io.hpp>
#include "ores.database/repository/helpers.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.refdata/domain/book_json_io.hpp" // IWYU pragma: keep.
#include "ores.refdata/repository/book_entity.hpp"
#include "ores.refdata/repository/book_mapper.hpp"

namespace ores::refdata::repository {

using namespace sqlgen;
using namespace sqlgen::literals;
using namespace ores::logging;
using namespace ores::database::repository;

std::string book_repository::sql() {
    return generate_create_table_sql<book_entity>(lg());
}

book_repository::book_repository(context ctx)
    : ctx_(std::move(ctx)) {}

void book_repository::write(const domain::book& book) {
    BOOST_LOG_SEV(lg(), debug) << "Writing book to database: "
                               << book.id;
    execute_write_query(ctx_, book_mapper::map(book),
        lg(), "writing book to database");
}

void book_repository::write(
    const std::vector<domain::book>& books) {
    BOOST_LOG_SEV(lg(), debug) << "Writing books to database. Count: "
                               << books.size();
    execute_write_query(ctx_, book_mapper::map(books),
        lg(), "writing books to database");
}

std::vector<domain::book>
book_repository::read_latest() {
    const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto query = sqlgen::read<std::vector<book_entity>> |
        where("valid_to"_c == max.value()) |
        order_by("name"_c);

    return execute_read_query<book_entity, domain::book>(
        ctx_, query,
        [](const auto& entities) { return book_mapper::map(entities); },
        lg(), "Reading latest books");
}

std::vector<domain::book>
book_repository::read_latest(const boost::uuids::uuid& id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest book. Id: " << id;

    const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto id_str = boost::uuids::to_string(id);
    const auto query = sqlgen::read<std::vector<book_entity>> |
        where("id"_c == id_str && "valid_to"_c == max.value());

    return execute_read_query<book_entity, domain::book>(
        ctx_, query,
        [](const auto& entities) { return book_mapper::map(entities); },
        lg(), "Reading latest book by id.");
}

std::vector<domain::book>
book_repository::read_latest_by_code(const std::string& code) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest book. Code: " << code;

    const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto query = sqlgen::read<std::vector<book_entity>> |
        where("name"_c == code && "valid_to"_c == max.value());

    return execute_read_query<book_entity, domain::book>(
        ctx_, query,
        [](const auto& entities) { return book_mapper::map(entities); },
        lg(), "Reading latest book by code.");
}

std::vector<domain::book>
book_repository::read_all(const boost::uuids::uuid& id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading all book versions. Id: " << id;

    const auto id_str = boost::uuids::to_string(id);
    const auto query = sqlgen::read<std::vector<book_entity>> |
        where("id"_c == id_str) |
        order_by("version"_c.desc());

    return execute_read_query<book_entity, domain::book>(
        ctx_, query,
        [](const auto& entities) { return book_mapper::map(entities); },
        lg(), "Reading all book versions by id.");
}

void book_repository::remove(const boost::uuids::uuid& id) {
    BOOST_LOG_SEV(lg(), debug) << "Removing book from database: " << id;

    const auto id_str = boost::uuids::to_string(id);
    const auto query = sqlgen::delete_from<book_entity> |
        where("id"_c == id_str);

    execute_delete_query(ctx_, query, lg(), "removing book from database");
}

}
