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
#include "ores.orgmode/parser/parser.hpp"
#include <catch2/catch_test_macros.hpp>

namespace {

using ores::orgmode::parser::parse;

}

TEST_CASE("frontmatter and file-level properties are parsed", "[ores.orgmode][parser]") {
    const std::string text = ":PROPERTIES:\n"
                             ":ID: 0345DCE3-4B85-4132-9A25-E58285632F76\n"
                             ":END:\n"
                             "#+title: Story: Example\n"
                             "#+type: story\n";

    const auto doc = parse(text);
    REQUIRE(doc.id() == "0345DCE3-4B85-4132-9A25-E58285632F76");
    REQUIRE(doc.find_keyword("title") == "Story: Example");
    REQUIRE(doc.find_keyword("type") == "story");
    REQUIRE(doc.headings.empty());
}

TEST_CASE("headings nest by level, tolerating skipped levels", "[ores.orgmode][parser]") {
    const std::string text = "* Goal\n"
                             "Some prose.\n"
                             "*** Deeply nested\n"
                             "Nested prose.\n"
                             "* Status\n"
                             "More prose.\n";

    const auto doc = parse(text);
    REQUIRE(doc.headings.size() == 2);

    const auto& goal = doc.headings[0];
    REQUIRE(goal.level == 1);
    REQUIRE(goal.title == "Goal");
    REQUIRE(goal.body_lines == std::vector<std::string>{"Some prose."});
    REQUIRE(goal.children.size() == 1);
    REQUIRE(goal.children[0].level == 3);
    REQUIRE(goal.children[0].title == "Deeply nested");

    const auto& status = doc.headings[1];
    REQUIRE(status.title == "Status");
    REQUIRE(status.children.empty());
}

TEST_CASE("heading PROPERTIES drawer sets id and other properties", "[ores.orgmode][parser]") {
    const std::string text = "* Task\n"
                             ":PROPERTIES:\n"
                             ":ID: F7C39C66-DFA0-4A10-B473-F3827B8A7187\n"
                             ":owner: marco\n"
                             ":END:\n"
                             "Body text.\n";

    const auto doc = parse(text);
    REQUIRE(doc.headings.size() == 1);
    const auto& h = doc.headings[0];
    REQUIRE(h.id.has_value());
    REQUIRE(*h.id == "F7C39C66-DFA0-4A10-B473-F3827B8A7187");
    REQUIRE(h.properties.size() == 2);
    REQUIRE(h.body_lines == std::vector<std::string>{"Body text."});
}

TEST_CASE("bullet lists are captured as a run and kept in body_lines", "[ores.orgmode][parser]") {
    const std::string text = "* List\n"
                             "- first\n"
                             "- second\n"
                             "Not a bullet.\n";

    const auto doc = parse(text);
    const auto& h = doc.headings[0];
    REQUIRE(h.bullet_lists.size() == 1);
    REQUIRE(h.bullet_lists[0] == std::vector<std::string>{"first", "second"});
    REQUIRE(h.body_lines.size() == 3);
}

TEST_CASE("pipe tables split the header row from data rows", "[ores.orgmode][parser]") {
    const std::string text = "* Table\n"
                             "| Task | State |\n"
                             "|------+-------|\n"
                             "| Foo  | DONE  |\n"
                             "| Bar  | BACKLOG |\n";

    const auto doc = parse(text);
    REQUIRE(doc.headings[0].tables.size() == 1);
    const auto& t = doc.headings[0].tables[0];
    REQUIRE(t.headers == std::vector<std::string>{"Task", "State"});
    REQUIRE(t.rows.size() == 2);
    REQUIRE(t.rows[0] == std::vector<std::string>{"Foo", "DONE"});
    REQUIRE(t.rows[1] == std::vector<std::string>{"Bar", "BACKLOG"});
}

TEST_CASE("id links are extracted from body text and table cells", "[ores.orgmode][parser]") {
    const std::string text =
        "* Goal\n"
        "See [[id:0345DCE3-4B85-4132-9A25-E58285632F76][the story]] for context.\n"
        "| Task | Description |\n"
        "|------+-------------|\n"
        "| [[id:F7C39C66-DFA0-4A10-B473-F3827B8A7187][Do it]] | thing |\n";

    const auto doc = parse(text);
    const auto& h = doc.headings[0];
    REQUIRE(h.links.size() == 2);
    REQUIRE(h.links[0].target_id == "0345DCE3-4B85-4132-9A25-E58285632F76");
    REQUIRE(h.links[0].text == "the story");
    REQUIRE(h.links[1].target_id == "F7C39C66-DFA0-4A10-B473-F3827B8A7187");
    REQUIRE(h.links[1].text == "Do it");
}

TEST_CASE("malformed input degrades gracefully instead of crashing", "[ores.orgmode][parser]") {
    const std::string text = ":PROPERTIES:\n"
                             "#+title: no END drawer above, frontmatter never reached\n"
                             "* Heading with no closing anything\n"
                             "| unterminated | table\n";

    REQUIRE_NOTHROW(parse(text));
}

TEST_CASE("parse_file throws on a missing file", "[ores.orgmode][parser]") {
    REQUIRE_THROWS_AS(ores::orgmode::parser::parse_file("/no/such/file.org"),
                      std::filesystem::filesystem_error);
}
