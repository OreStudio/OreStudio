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
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */
#include "ores.database/query_builder/execute.hpp"
#include "test_suite.hpp"
#include <catch2/catch_test_macros.hpp>

namespace query_builder = ores::database::query_builder;
using namespace ores::database::query_builder::literals;

namespace {

struct probe_text_entity {
    constexpr static const char* schema = "public";
    constexpr static const char* tablename = "probe_text_tbl";

    std::string code;
    ores::database::repository::db_timestamp valid_to =
        ores::database::repository::db_timestamp::max();

    static constexpr auto columns() {
        using e = probe_text_entity;
        return query_builder::columns<e>() | query_builder::key("code", &e::code) |
               query_builder::bind_text("valid_to", &e::valid_to);
    }
};

struct probe_entity {
    constexpr static const char* schema = "public";
    constexpr static const char* tablename = "probe_tbl";

    std::string code;
    std::string tenant_id;
    int version = 0;
    ores::database::repository::db_timestamp valid_to =
        ores::database::repository::db_timestamp::max();

    static constexpr auto columns() {
        using e = probe_entity;
        return query_builder::columns<e>() | query_builder::key("code", &e::code) |
               query_builder::bind("tenant_id", &e::tenant_id) |
               query_builder::bind("version", &e::version) |
               query_builder::bind("valid_to", &e::valid_to);
    }
};

}

TEST_CASE("each placeholder is numbered in the order it appears", "[q]") {
    CHECK(query_builder::detail::numbered("a = ? and (b = ? or c in (?, ?))") ==
          "a = $1 and (b = $2 or c in ($3, $4))");
}

TEST_CASE("an insert names every column and an upsert replaces all but the key", "[q]") {
    CHECK(query_builder::detail::insert_sql<probe_entity>(false) ==
          "insert into public.probe_tbl (code, tenant_id, version, valid_to)"
          " values ($1, $2, $3, $4)");
    CHECK(query_builder::detail::insert_sql<probe_entity>(true) ==
          "insert into public.probe_tbl (code, tenant_id, version, valid_to)"
          " values ($1, $2, $3, $4) on conflict (code) do update set"
          " tenant_id = excluded.tenant_id, version = excluded.version,"
          " valid_to = excluded.valid_to");
}

TEST_CASE("a column the row holds as text is asked for cast to text", "[q]") {
    const auto query = query_builder::read<std::vector<probe_text_entity>>;
    const auto s = query_builder::detail::select_sql<probe_text_entity>(
        query.filter, query.sorting, query.max_rows, query.skip);
    CHECK(s.sql == "select code, valid_to::text from public.probe_text_tbl");
    const probe_text_entity row{.code = "x"};
    const auto params = query_builder::detail::row_params(row);
    REQUIRE(params.size() == 2);
    CHECK(params[1].text);
}

TEST_CASE("a read states its condition, order, limit and offset with bound values", "[q]") {
    const auto query = query_builder::read<std::vector<probe_entity>> |
                       query_builder::where("code"_c == std::string("a'b") && "version"_c > 2) |
                       query_builder::order_by("version"_c.desc(), "code"_c) |
                       query_builder::limit(10) | query_builder::offset(20);
    const auto s = query_builder::detail::select_sql<probe_entity>(
        query.filter, query.sorting, query.max_rows, query.skip);
    CHECK(query_builder::detail::numbered(s.sql) ==
          "select code, tenant_id, version, valid_to from public.probe_tbl"
          " where (code = $1) and (version > $2) order by version desc, code limit $3 offset $4");
    REQUIRE(s.params.size() == 4);
    CHECK(s.params[0].bytes == "a'b");
    CHECK(s.params[0].text);
    CHECK_FALSE(s.variadic);
}

TEST_CASE("a list in a condition makes the statement unnamed, and an empty one holds for no row",
          "[q]") {
    const auto some = "code"_c.in(std::vector<std::string>{"a", "b"});
    CHECK(some.sql == "code in (?, ?)");
    CHECK(some.variadic);
    CHECK(query_builder::one_of("code", {}).sql == "1 = 0");
}

TEST_CASE("a page reads the current row, or the row valid at an instant", "[q]") {
    CHECK(query_builder::valid_at(std::nullopt).sql == "valid_to = ?");
    const auto at = query_builder::valid_at(std::string("2026-10-27T12:00:00Z"));
    CHECK(at.sql == "valid_from <= ? and valid_to > ?");
    CHECK(at.params.size() == 2);
}
