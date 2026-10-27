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
#include "ores.database/repository/list_filter.hpp"
#include "ores.database/repository/range_bound.hpp"
#include "ores.testing/database_helper.hpp"
#include <boost/uuid/string_generator.hpp>
#include <catch2/catch_test_macros.hpp>
#include <chrono>
#include <optional>
#include <string>
#include <vector>

/*
 * The conditions are checked in the SQL they render to, because that is what
 * the database is asked; the tenant repository tests run them against rows.
 */

namespace {

struct widget {
    std::string code;
};

namespace shapes {

enum class edge { sharp, round };

std::string_view to_string(edge e) {
    return e == edge::sharp ? "sharp" : "round";
}

}

std::string where_clause(std::optional<sqlgen::dynamic::Condition> condition) {
    ores::testing::database_helper h;
    auto select = sqlgen::transpilation::read_to_select_from<widget>();
    select.where = std::move(condition);
    const auto s = sqlgen::session(h.context().connection_pool());
    REQUIRE(s);
    const auto sql = (*s)->to_sql(sqlgen::dynamic::Statement{select});
    const auto at = sql.find(" WHERE ");
    return at == std::string::npos ? std::string() : sql.substr(at + 7);
}

}

using namespace ores::database::repository;

TEST_CASE("an equals member renders an equality on its column", "[list_filter]") {
    const auto sql = where_clause(equals("code", filter_value(std::string("acme"))));
    CHECK(sql.find("\"code\" = 'acme'") != std::string::npos);
}

TEST_CASE("a value with a quote stays a value", "[list_filter]") {
    const auto sql = where_clause(equals("code", filter_value(std::string("o'brien"))));
    CHECK(sql.find("'o''brien'") != std::string::npos);
}

TEST_CASE("a uuid value renders as its text", "[list_filter]") {
    const auto id = boost::uuids::string_generator()("11111111-1111-1111-1111-111111111111");
    const auto sql = where_clause(equals("code", filter_value(id)));
    CHECK(sql.find("'11111111-1111-1111-1111-111111111111'") != std::string::npos);
}

TEST_CASE("a null member renders a null test", "[list_filter]") {
    const auto sql = where_clause(is_null("code"));
    CHECK(sql.find("\"code\" IS NULL") != std::string::npos);
}

TEST_CASE("a one-of member renders an IN list", "[list_filter]") {
    const auto sql = where_clause(
        one_of("code", {filter_value(std::string("a")), filter_value(std::string("b"))}));
    CHECK(sql.find("\"code\" IN ('a', 'b')") != std::string::npos);
}

TEST_CASE("an empty one-of list matches no row", "[list_filter]") {
    const auto sql = where_clause(one_of("code", {}));
    CHECK(sql.find("IN") == std::string::npos);
    CHECK(sql.find("1 = 0") != std::string::npos);
}

TEST_CASE("a search folds both sides and reads every searchable column", "[list_filter]") {
    const auto sql = where_clause(contains_any({"code", "name"}, "Ac%me"));
    CHECK(sql ==
          "(length(replace(lower(\"code\"), lower('Ac%me'), '')) < length(lower(\"code\"))) OR "
          "(length(replace(lower(\"name\"), lower('Ac%me'), '')) < length(lower(\"name\")))");
}

TEST_CASE("members that are set must all hold", "[list_filter]") {
    CHECK_FALSE(all_of({}).has_value());
    const auto sql =
        where_clause(all_of({equals("code", filter_value(std::string("a"))), is_null("name")}));
    CHECK(sql.find(" AND ") != std::string::npos);
}

TEST_CASE("a filter narrows a query's own condition", "[list_filter]") {
    CHECK_FALSE(narrowed(std::nullopt, std::nullopt).has_value());
    const auto own = equals("code", filter_value(std::string("a")));
    const auto filter = is_null("name");
    CHECK(where_clause(narrowed(own, std::nullopt)) == where_clause(own));
    CHECK(where_clause(narrowed(std::nullopt, filter)) == where_clause(filter));
    CHECK(where_clause(narrowed(own, filter)).find(" AND ") != std::string::npos);
}

TEST_CASE("an enum value renders as the text its to_string gives", "[list_filter]") {
    const auto sql = where_clause(equals("code", filter_value(shapes::edge::round)));
    CHECK(sql.find("\"code\" = 'round'") != std::string::npos);
}

TEST_CASE("a date renders as its ISO text", "[list_filter]") {
    using namespace std::chrono;
    const auto sql =
        where_clause(equals("code", filter_value(year_month_day{year{2026} / March / 5})));
    CHECK(sql.find("'2026-03-05'") != std::string::npos);
}

TEST_CASE("a timestamp renders as UTC text", "[list_filter]") {
    using namespace std::chrono;
    const auto tp = sys_days{year{2026} / March / 5} + hours{10};
    const auto sql = where_clause(equals("code", filter_value(system_clock::time_point{tp})));
    CHECK(sql.find("'2026-03-05 10:00:00") != std::string::npos);
}

TEST_CASE("at_least and at_most render inclusive comparisons", "[list_filter]") {
    const auto low = where_clause(at_least("code", filter_value(std::string("2026-03-01"))));
    CHECK(low.find("\"code\" >= '2026-03-01'") != std::string::npos);
    const auto high = where_clause(at_most("code", filter_value(std::string("2026-03-31"))));
    CHECK(high.find("\"code\" <= '2026-03-31'") != std::string::npos);
}

TEST_CASE("a date column takes a date as its bound and nothing else", "[list_filter]") {
    CHECK(range_bound("2026-03-05", range_end::from, false) == "2026-03-05");
    CHECK(range_bound("2026-03-05", range_end::to, false) == "2026-03-05");
    CHECK(!range_bound("2026-03-05T10:00:00Z", range_end::from, false));
    CHECK(!range_bound("yesterday", range_end::from, false));
    CHECK(!range_bound("", range_end::to, false));
}

TEST_CASE("a date that does not exist is not a bound", "[list_filter]") {
    CHECK(!range_bound("2026-02-30", range_end::from, false));
    CHECK(!range_bound("2026-13-01", range_end::to, true));
    CHECK(range_bound("2028-02-29", range_end::from, false));
    CHECK(!range_bound("2027-02-29", range_end::from, false));
}

TEST_CASE("year 0 is not a date, because the database has none", "[list_filter]") {
    CHECK(!range_bound("0000-01-01", range_end::from, false));
    CHECK(!range_bound("0000-06-15", range_end::to, true));
    CHECK(!range_bound("0000-01-01T00:00:00Z", range_end::from, true));
    CHECK(range_bound("0001-01-01", range_end::from, false) == "0001-01-01");
}

TEST_CASE("a date on a timestamp column spans the whole of that day in UTC", "[list_filter]") {
    CHECK(range_bound("2026-03-05", range_end::from, true) == "2026-03-05 00:00:00+00");
    CHECK(range_bound("2026-03-05", range_end::to, true) == "2026-03-05 23:59:59.999999+00");
}

TEST_CASE("a UTC timestamp on a timestamp column is the bound itself", "[list_filter]") {
    CHECK(range_bound("2026-03-05T10:00:00Z", range_end::from, true) == "2026-03-05T10:00:00Z");
    CHECK(range_bound("2026-03-05 10:00:00.5+00", range_end::to, true) ==
          "2026-03-05 10:00:00.5+00");
    CHECK(!range_bound("2026-03-05T10:00:00+02:00", range_end::to, true));
    CHECK(!range_bound("2026-03-05 25:00:00Z", range_end::to, true));
}
