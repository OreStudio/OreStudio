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
#include "ores.synthetic.api/feeds/feed_overview.hpp"
#include <catch2/catch_test_macros.hpp>
#include <string>
#include <vector>

namespace {

const std::string tags("[feed_overview]");

using ores::synthetic::feed::children_with_counts;
using ores::synthetic::feed::folder_feed_count;
using ores::synthetic::messaging::feed_folder;

feed_folder folder(std::string id, std::string parent, std::string name, std::string kind) {
    feed_folder f;
    f.id = std::move(id);
    f.parent_id = std::move(parent);
    f.name = std::move(name);
    f.kind = std::move(kind);
    return f;
}

// Synthetic > { Bloomberg > { FX > (2 fx, 1 running), IR > (3 ir) }, Generic > FX > (1 fx) }
const std::vector<feed_folder> hierarchy{folder("root", "", "Synthetic", "root"),
                                         folder("g", "root", "Generic", "collection"),
                                         folder("b", "root", "Bloomberg", "collection"),
                                         folder("b-fx", "b", "FX", "asset_class"),
                                         folder("b-ir", "b", "IR", "asset_class"),
                                         folder("g-fx", "g", "FX", "asset_class")};

const std::vector<folder_feed_count> counts{{"b-fx", 2, 0, 1}, {"b-ir", 0, 3, 0}, {"g-fx", 1, 0, 0}};

}

TEST_CASE("feed_overview wants the roots when no folder is named", tags) {
    const auto roots = children_with_counts(hierarchy, counts, "");
    REQUIRE(roots.size() == 1);
    CHECK(roots[0].name == "Synthetic");
    CHECK(roots[0].fx_feeds == 3);
    CHECK(roots[0].ir_feeds == 3);
    CHECK(roots[0].running_feeds == 1);
    CHECK(roots[0].has_children);
}

TEST_CASE("feed_overview adds the feeds of every folder below a child", tags) {
    const auto kids = children_with_counts(hierarchy, counts, "root");
    REQUIRE(kids.size() == 2);
    CHECK(kids[0].name == "Bloomberg");
    CHECK(kids[0].fx_feeds == 2);
    CHECK(kids[0].ir_feeds == 3);
    CHECK(kids[0].running_feeds == 1);
    CHECK(kids[1].name == "Generic");
    CHECK(kids[1].fx_feeds == 1);
    CHECK(kids[1].ir_feeds == 0);
}

TEST_CASE("feed_overview says which children have folders of their own", tags) {
    const auto kids = children_with_counts(hierarchy, counts, "b");
    REQUIRE(kids.size() == 2);
    CHECK_FALSE(kids[0].has_children);
    CHECK_FALSE(kids[1].has_children);
    CHECK(children_with_counts(hierarchy, counts, "root")[0].has_children);
}

TEST_CASE("feed_overview puts the children in name order, then by id", tags) {
    const std::vector<feed_folder> same{folder("2", "", "Same", "root"), folder("1", "", "Same", "root")};
    const auto roots = children_with_counts(same, {}, "");
    REQUIRE(roots.size() == 2);
    CHECK(roots[0].id == "1");
    CHECK(roots[1].id == "2");
}

TEST_CASE("feed_overview counts nothing for a folder with no feeds", tags) {
    const auto kids = children_with_counts(hierarchy, {}, "root");
    for (const auto& kid : kids) {
        CHECK(kid.fx_feeds == 0);
        CHECK(kid.ir_feeds == 0);
        CHECK(kid.running_feeds == 0);
    }
}

TEST_CASE("feed_overview has no children for a folder that is not there", tags) {
    CHECK(children_with_counts(hierarchy, counts, "nowhere").empty());
}

TEST_CASE("feed_overview orders the names without regard to case", tags) {
    const std::vector<feed_folder> mixed{folder("1", "", "beta", "root"),
                                         folder("2", "", "Alpha", "root"),
                                         folder("3", "", "Gamma", "root")};
    const auto roots = children_with_counts(mixed, {}, "");
    REQUIRE(roots.size() == 3);
    CHECK(roots[0].name == "Alpha");
    CHECK(roots[1].name == "beta");
    CHECK(roots[2].name == "Gamma");
}

TEST_CASE("feed_overview splits a search into lower case words", tags) {
    using ores::synthetic::feed::search_terms;
    CHECK(search_terms("  USD   SOFR ") == std::vector<std::string>{"usd", "sofr"});
    CHECK(search_terms("").empty());
    CHECK(search_terms("   \t ").empty());
}

TEST_CASE("feed_overview bounds a search", tags) {
    using ores::synthetic::feed::search_terms;
    const auto many = search_terms("a b c d e f g h i j k l");
    CHECK(many.size() == 8);
    const auto long_word = search_terms(std::string(200, 'x'));
    REQUIRE(long_word.size() == 1);
    CHECK(long_word[0].size() == 64);
}

TEST_CASE("feed_overview accepts only a UUID as a folder id", tags) {
    using ores::synthetic::feed::is_folder_id;
    CHECK(is_folder_id("0a32284a-5f8c-4a99-9539-34923631ded3"));
    CHECK(is_folder_id("0A32284A-5F8C-4A99-9539-34923631DED3"));
    CHECK_FALSE(is_folder_id(""));
    CHECK_FALSE(is_folder_id("not-a-uuid"));
    CHECK_FALSE(is_folder_id("0a32284a-5f8c-4a99-9539-34923631ded3'; drop table x; --"));
    CHECK_FALSE(is_folder_id("0a32284a5f8c4a9995393492363ded3xxxxx"));
}
