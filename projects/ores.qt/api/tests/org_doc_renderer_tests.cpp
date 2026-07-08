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
#include "ores.qt/OrgDocRenderer.hpp"
#include <catch2/catch_test_macros.hpp>

namespace {

const std::string tags("[org_doc_renderer]");

}

TEST_CASE("headings render with the right HTML tag and title text", tags) {
    const auto doc = ores::orgmode::parser::parse("* Goal\nSome prose.\n");
    const auto html = ores::qt::render_org_doc_to_html(doc);
    REQUIRE(html.contains("<h1>Goal</h1>"));
    REQUIRE(html.contains("<p>Some prose.</p>"));
}

TEST_CASE("bullet lists render as ul/li", tags) {
    const auto doc = ores::orgmode::parser::parse("* List\n- first\n- second\n");
    const auto html = ores::qt::render_org_doc_to_html(doc);
    REQUIRE(html.contains("<ul><li>first</li><li>second</li></ul>"));
}

TEST_CASE("tables render with headers and rows", tags) {
    const auto doc = ores::orgmode::parser::parse(
        "* Table\n| Task | State |\n|------+-------|\n| Foo | DONE |\n");
    const auto html = ores::qt::render_org_doc_to_html(doc);
    REQUIRE(html.contains("<th>Task</th>"));
    REQUIRE(html.contains("<th>State</th>"));
    REQUIRE(html.contains("<td>Foo</td>"));
    REQUIRE(html.contains("<td>DONE</td>"));
}

TEST_CASE("inline markers render as light HTML tags", tags) {
    const auto doc =
        ores::orgmode::parser::parse("* Goal\nThis is *bold* and /italic/ and =code=.\n");
    const auto html = ores::qt::render_org_doc_to_html(doc);
    REQUIRE(html.contains("<b>bold</b>"));
    REQUIRE(html.contains("<i>italic</i>"));
    REQUIRE(html.contains("<code>code</code>"));
}

TEST_CASE("bold followed by italic on the same line doesn't cross-contaminate", tags) {
    // Regression: a naive sequential-regex-pass implementation matches
    // the '/' inside a previously-emitted </b> closing tag against the
    // italic pattern, corrupting the output.
    const auto doc = ores::orgmode::parser::parse("* Goal\n*bold* then /italic/ text.\n");
    const auto html = ores::qt::render_org_doc_to_html(doc);
    REQUIRE(html.contains("<b>bold</b>"));
    REQUIRE(html.contains("<i>italic</i>"));
    REQUIRE_FALSE(html.contains("<<i>"));
}

TEST_CASE("HTML special characters in body text are escaped, not misinterpreted", tags) {
    const auto doc = ores::orgmode::parser::parse("* Goal\nA <script>alert(1)</script> tag.\n");
    const auto html = ores::qt::render_org_doc_to_html(doc);
    REQUIRE_FALSE(html.contains("<script>"));
    REQUIRE(html.contains("&lt;script&gt;"));
}

TEST_CASE("nested headings render recursively", tags) {
    const auto doc = ores::orgmode::parser::parse("* Goal\n** Sub goal\nNested prose.\n");
    const auto html = ores::qt::render_org_doc_to_html(doc);
    REQUIRE(html.contains("<h1>Goal</h1>"));
    REQUIRE(html.contains("<h2>Sub goal</h2>"));
    REQUIRE(html.contains("Nested prose."));
}
