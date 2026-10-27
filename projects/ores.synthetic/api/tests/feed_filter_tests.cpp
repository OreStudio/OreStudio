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
#include "ores.synthetic.api/feeds/feed_filter.hpp"
#include <catch2/catch_test_macros.hpp>
#include <catch2/matchers/catch_matchers_string.hpp>
#include <string>
#include <vector>

namespace {

using Catch::Matchers::ContainsSubstring;
using ores::synthetic::feed::feed_filter;
using ores::synthetic::feed::filter_problem;
using ores::synthetic::feed::selection_problem;
using ores::synthetic::feed::sql_literal;
using ores::synthetic::feed::where_clause;

const std::string folder = "0b7f6f0e-8c5e-4c3b-9d3e-6a0f5a1c2d3e";
const std::string other = "1c8a7a1f-9d6f-4d4c-8e4f-7b1a6b2d3e4f";

}

TEST_CASE("feed_filter accepts an empty filter and every word it knows", "[feed_filter]") {
    CHECK(filter_problem({}).empty());
    feed_filter f;
    f.folder_id = folder;
    f.provider_code = "bloomberg";
    f.asset_class = "interest_rates";
    f.state = "stopped";
    f.search = "usd sofr";
    CHECK(filter_problem(f).empty());
}

TEST_CASE("feed_filter refuses what a query must not be given", "[feed_filter]") {
    feed_filter f;
    f.folder_id = "x'; drop table t; --";
    CHECK_FALSE(filter_problem(f).empty());
    f = {};
    f.provider_code = "Bloomberg";
    CHECK_FALSE(filter_problem(f).empty());
    f = {};
    f.asset_class = "fx' or '1'='1";
    CHECK_FALSE(filter_problem(f).empty());
    f = {};
    f.state = "running";
    CHECK_FALSE(filter_problem(f).empty());
    f = {};
    f.feed_ids = {"not-a-uuid"};
    CHECK_FALSE(filter_problem(f).empty());
    f = {};
    f.excluded_ids = {"not-a-uuid"};
    CHECK_FALSE(filter_problem(f).empty());
}

TEST_CASE("feed_filter limits how many feeds a request names", "[feed_filter]") {
    feed_filter f;
    f.feed_ids.assign(ores::synthetic::feed::most_named_feeds, folder);
    CHECK(filter_problem(f).empty());
    f.feed_ids.push_back(folder);
    CHECK_FALSE(filter_problem(f).empty());
}

TEST_CASE("feed_filter doubles a quote and a backslash in a literal", "[feed_filter]") {
    CHECK(sql_literal("it's") == "E'it''s'");
    CHECK(sql_literal("a\\b") == "E'a\\\\b'");
}

TEST_CASE("feed_filter matches every feed when nothing narrows it", "[feed_filter]") {
    CHECK(where_clause({}, {}, true) == "true");
}

TEST_CASE("feed_filter reads a folder with the folders under it", "[feed_filter]") {
    feed_filter f;
    f.folder_id = folder;
    const auto where = where_clause(f, {}, true);
    CHECK_THAT(where, ContainsSubstring("with recursive below"));
    CHECK_THAT(where, ContainsSubstring("E'" + folder + "'::uuid"));
}

TEST_CASE("feed_filter narrows by provider and asset class", "[feed_filter]") {
    feed_filter f;
    f.provider_code = "bloomberg";
    f.asset_class = "fx";
    const auto where = where_clause(f, {}, true);
    CHECK_THAT(where, ContainsSubstring("provider_code = E'bloomberg'"));
    CHECK_THAT(where, ContainsSubstring("asset_class_code = E'fx'"));
}

TEST_CASE("feed_filter decides started and stopped by the running names", "[feed_filter]") {
    feed_filter f;
    f.state = "started";
    CHECK_THAT(where_clause(f, {"synthetic.a", "synthetic.b"}, true),
               ContainsSubstring("source_name in (E'synthetic.a', E'synthetic.b')"));
    CHECK_THAT(where_clause(f, {}, true), ContainsSubstring("and false"));
    f.state = "stopped";
    CHECK_THAT(where_clause(f, {"synthetic.a"}, true),
               ContainsSubstring("source_name not in (E'synthetic.a')"));
    CHECK(where_clause(f, {}, true) == "true");
    f.state = "all";
    CHECK(where_clause(f, {"synthetic.a"}, true) == "true");
}

TEST_CASE("feed_filter searches each word and escapes the pattern", "[feed_filter]") {
    feed_filter f;
    f.search = "usd 100%_x";
    const auto where = where_clause(f, {}, true);
    CHECK_THAT(where, ContainsSubstring("ilike E'%usd%'"));
    CHECK_THAT(where, ContainsSubstring("ilike E'%100\\\\%\\\\_x%'"));
}

TEST_CASE("feed_filter leaves out the excluded feeds", "[feed_filter]") {
    feed_filter f;
    f.excluded_ids = {folder, other};
    CHECK_THAT(
        where_clause(f, {}, true),
        ContainsSubstring("id::uuid not in (E'" + folder + "'::uuid, E'" + other + "'::uuid)"));
}

TEST_CASE("feed_filter names only the picked feeds and ignores the filter", "[feed_filter]") {
    feed_filter f;
    f.feed_ids = {folder};
    f.provider_code = "bloomberg";
    f.state = "started";
    f.excluded_ids = {other};
    CHECK(where_clause(f, {"synthetic.a"}, true) == "id::uuid in (E'" + folder + "'::uuid)");
}

TEST_CASE("feed_filter keeps the feeds that should run: enabled, automatic and stopped", "[feed_filter]") {
    feed_filter f;
    f.should_run = true;
    CHECK_THAT(where_clause(f, {"synthetic.a"}, true),
               ContainsSubstring("and enabled and auto_start"));
    CHECK_THAT(where_clause(f, {"synthetic.a"}, true),
               ContainsSubstring("source_name not in (E'synthetic.a')"));
    f.state = "started";
    CHECK_THAT(where_clause(f, {"synthetic.a"}, true),
               ContainsSubstring("source_name not in (E'synthetic.a')"));
}

TEST_CASE("feed_filter refuses a selection that names nothing and narrows nothing", "[feed_filter]") {
    CHECK_FALSE(selection_problem({}).empty());
    feed_filter f;
    f.state = "all";
    CHECK(selection_problem(f).empty());
    f = {};
    f.provider_code = "ore";
    CHECK(selection_problem(f).empty());
    f = {};
    f.state = "started";
    CHECK(selection_problem(f).empty());
    f.state = "stopped";
    CHECK(selection_problem(f).empty());
    f = {};
    f.should_run = true;
    CHECK(selection_problem(f).empty());
    f = {};
    f.feed_ids = {folder};
    CHECK(selection_problem(f).empty());
    f = {};
    f.excluded_ids = {folder};
    CHECK_FALSE(selection_problem(f).empty());
    f = {};
    f.state = "running";
    CHECK_FALSE(selection_problem(f).empty());
}

TEST_CASE("feed_filter names the cap in its message", "[feed_filter]") {
    feed_filter f;
    f.feed_ids.assign(ores::synthetic::feed::most_named_feeds + 1, folder);
    CHECK_THAT(filter_problem(f), ContainsSubstring("2000"));
}

TEST_CASE("feed_filter leaves the system feeds out of those another tenant should run", "[feed_filter]") {
    feed_filter f;
    f.should_run = true;
    CHECK_THAT(where_clause(f, {}, false),
               ContainsSubstring("tenant_id <> ores_utility_system_tenant_id_fn()"));
    CHECK_THAT(where_clause(f, {}, true),
               !ContainsSubstring("tenant_id <> ores_utility_system_tenant_id_fn()"));
    f.should_run = false;
    CHECK(where_clause(f, {}, false) == "true");
}
