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
#include "ores.orgmode.core/service/knowledge_index.hpp"
#include "ores.utility/codec/json_codec.hpp"
#include "test_suite.hpp"
#include <catch2/catch_test_macros.hpp>
#include <string>
#include <vector>

namespace {

using namespace ores::orgmode::service;
using ores::orgmode::domain::knowledge_page;

const std::string tags("[knowledge_index]");

knowledge_page page(std::string id, std::string path, std::string org) {
    knowledge_page p;
    p.id = std::move(id);
    p.path = std::move(path);
    p.org = std::move(org);
    return p;
}

const knowledge_page volatility = page("V-1",
                                       "doc/knowledge/domain/volatility.org",
                                       R"(:PROPERTIES:
:ID: V-1
:END:
#+title: Volatility
#+description: How much a price moves.
#+filetags: :domain:market:

* Definition

Volatility measures how widely returns spread, see [[id:O-1][options]].

* Kinds

** Implied
:PROPERTIES:
:ID: IMPLIED-1
:END:

Backed out of [[id:O-1][option]] prices.

** Kinds

| Kind | Source |
|------+--------|
| Historical | returns |
)");

const knowledge_page glossary = page("G-1", "doc/glossary.org", R"(:PROPERTIES:
:ID: G-1
:END:
#+title: Glossary
)");

}

TEST_CASE("a_page_gives_one_page_row_with_its_keywords", tags) {
    const auto index = build_knowledge_index({volatility});

    REQUIRE(index.pages.size() == 1);
    const auto& p = index.pages[0];
    CHECK(p.page_id == "V-1");
    CHECK(p.path == "doc/knowledge/domain/volatility.org");
    CHECK(p.title == "Volatility");
    CHECK(p.description == "How much a price moves.");
    CHECK(p.tags == std::vector<std::string>{"domain", "market"});
}

TEST_CASE("each_heading_gives_one_section_after_the_page_itself", tags) {
    const auto index = build_knowledge_index({volatility});

    REQUIRE(index.sections.size() == 5);
    CHECK(index.sections[0].ord == 0);
    CHECK(index.sections[0].level == 0);
    CHECK(index.sections[0].text == "How much a price moves.");

    CHECK(index.sections[1].heading_path == "Definition");
    CHECK(index.sections[1].anchor == "definition");
    CHECK(index.sections[2].heading_path == "Kinds");
    CHECK(index.sections[3].heading_path == "Kinds / Implied");
    CHECK(index.sections[3].level == 2);
    CHECK(index.sections[3].anchor == "IMPLIED-1");
    CHECK(index.sections[3].text == "Backed out of option prices.");
    CHECK(index.sections[4].heading_path == "Kinds / Kinds");
    CHECK(index.sections[4].text.find("Historical returns") != std::string::npos);

    for (std::size_t i = 0; i < index.sections.size(); ++i)
        CHECK(index.sections[i].ord == static_cast<std::int32_t>(i));
}

TEST_CASE("section_text_shows_a_link_as_its_description", tags) {
    const auto index = build_knowledge_index({volatility});
    CHECK(index.sections[1].text == "Volatility measures how widely returns spread, see options.");
}

TEST_CASE("anchors_are_unique_within_a_page", tags) {
    const auto index = build_knowledge_index({page("D-1", "d.org", R"(#+title: D
* Notes
* Notes
* Notes
)")});

    REQUIRE(index.sections.size() == 4);
    CHECK(index.sections[1].anchor == "notes");
    CHECK(index.sections[2].anchor == "notes-2");
    CHECK(index.sections[3].anchor == "notes-3");
}

TEST_CASE("each_id_link_gives_one_link_row_in_its_section", tags) {
    const auto index = build_knowledge_index({volatility});

    REQUIRE(index.links.size() == 2);
    CHECK(index.links[0].section_ord == 1);
    CHECK(index.links[0].target_id == "O-1");
    CHECK(index.links[0].text == "options");
    CHECK(index.links[1].section_ord == 3);
    CHECK(index.links[1].text == "option");
}

TEST_CASE("a_page_without_headings_is_still_one_section", tags) {
    const auto index = build_knowledge_index({glossary});

    REQUIRE(index.sections.size() == 1);
    CHECK(index.sections[0].page_id == "G-1");
    CHECK(index.pages[0].description.empty());
}

TEST_CASE("the_same_pages_give_the_same_index", tags) {
    const auto a = build_knowledge_index({volatility, glossary});
    const auto b = build_knowledge_index({volatility, glossary});
    CHECK(a == b);
}

TEST_CASE("the_index_writes_the_keys_the_replace_function_reads", tags) {
    const auto json = ores::utility::codec::json::write(build_knowledge_index({glossary}));
    for (const auto* key : {"\"pages\"",
                            "\"sections\"",
                            "\"links\"",
                            "\"page_id\"",
                            "\"heading_path\"",
                            "\"anchor\"",
                            "\"tags\""}) {
        INFO(key);
        CHECK(json.find(key) != std::string::npos);
    }
}
