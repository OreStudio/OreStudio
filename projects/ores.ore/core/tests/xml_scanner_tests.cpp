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
#include "ores.logging/make_logger.hpp"
#include "ores.ore.core/xml/scanner.hpp"
#include "test_suite.hpp"
#include <catch2/catch_test_macros.hpp>
#include <string>
#include <vector>

namespace {

using namespace ores::logging;
using ores::ore::xml::event_type;
using ores::ore::xml::reader;

struct seen {
    event_type kind;
    std::string name;
    std::string value;
};

auto collect(const std::string& document, bool& failed) -> std::vector<seen> {
    reader input(document);
    std::vector<seen> events;
    for (;;) {
        const auto ev = input.next();
        if (ev.kind == event_type::end_of_document)
            break;
        if (ev.kind == event_type::error) {
            failed = true;
            break;
        }
        events.push_back({ev.kind, std::string(ev.name), std::string(ev.value)});
    }
    return events;
}

}

TEST_CASE("a_document_without_markup_yields_one_text_event", "[ore][xml][scanner]") {
    bool failed = false;
    const auto events = collect("hello", failed);

    REQUIRE_FALSE(failed);
    REQUIRE(events.size() == 1);
    CHECK(events[0].kind == event_type::text);
    CHECK(events[0].value == "hello");
}

TEST_CASE("an_element_yields_the_begin_and_the_end", "[ore][xml][scanner]") {
    bool failed = false;
    const auto events = collect("<a/>", failed);

    REQUIRE_FALSE(failed);
    REQUIRE(events.size() == 2);
    CHECK(events[0].kind == event_type::begin_element);
    CHECK(events[0].name == "a");
    CHECK(events[1].kind == event_type::end_element);
    CHECK(events[1].name == "a");
}

TEST_CASE("an_attribute_arrives_between_the_begin_and_the_first_child", "[ore][xml][scanner]") {
    bool failed = false;
    const auto events = collect("<a x=\"1\" y='2'>t</a>", failed);

    REQUIRE_FALSE(failed);
    REQUIRE(events.size() == 5);
    CHECK(events[0].kind == event_type::begin_element);
    CHECK(events[1].kind == event_type::attribute);
    CHECK(events[1].name == "x");
    CHECK(events[1].value == "1");
    CHECK(events[2].name == "y");
    CHECK(events[2].value == "2");
    CHECK(events[3].kind == event_type::text);
    CHECK(events[3].value == "t");
}

TEST_CASE("leading_whitespace_belongs_to_the_text", "[ore][xml][scanner]") {
    bool failed = false;
    const auto events = collect("<a>\n  indented</a>", failed);

    REQUIRE_FALSE(failed);
    REQUIRE(events.size() == 3);
    CHECK(events[1].kind == event_type::text);
    CHECK(events[1].value == "\n  indented");
}

TEST_CASE("a_cdata_section_keeps_its_markup_characters", "[ore][xml][scanner]") {
    auto lg(make_logger(test_suite));

    bool failed = false;
    const auto events = collect("<a><![CDATA[if (x < 1 && y > 2) {}]]></a>", failed);

    REQUIRE_FALSE(failed);
    REQUIRE(events.size() == 3);
    CHECK(events[1].kind == event_type::text);
    CHECK(events[1].value == "if (x < 1 && y > 2) {}");
}

TEST_CASE("the_five_predefined_entities_decode", "[ore][xml][scanner]") {
    bool failed = false;
    const auto events = collect("<a>&lt;&gt;&amp;&quot;&apos;</a>", failed);

    REQUIRE_FALSE(failed);
    REQUIRE(events.size() == 3);
    CHECK(events[1].value == "<>&\"'");
}

TEST_CASE("an_unknown_entity_is_kept_verbatim", "[ore][xml][scanner]") {
    auto lg(make_logger(test_suite));

    bool failed = false;
    const auto events = collect("<a>&nbsp;</a>", failed);

    REQUIRE_FALSE(failed);
    REQUIRE(events.size() == 3);
    CHECK(events[1].value == "&nbsp;");
}

TEST_CASE("comments_and_processing_instructions_are_skipped", "[ore][xml][scanner]") {
    bool failed = false;
    const auto events = collect("<?xml version=\"1.0\"?><!-- a --><a><!-- b -->x</a>", failed);

    REQUIRE_FALSE(failed);
    REQUIRE(events.size() == 3);
    CHECK(events[0].name == "a");
    CHECK(events[1].value == "x");
}

TEST_CASE("an_abruptly_closed_comment_runs_to_the_next_terminator", "[ore][xml][scanner]") {
    // ORE ships ore.xml files that open a comment as "<!-->", which XML
    // forbids and every parser in use repairs. The scanner must agree with
    // them or it reads the rest of the line as markup.
    bool failed = false;
    const auto events = collect("<!--> 1.D Other <--><a/>", failed);

    REQUIRE_FALSE(failed);
    REQUIRE(events.size() == 2);
    CHECK(events[0].name == "a");
}

TEST_CASE("an_end_tag_that_does_not_match_is_rejected_with_a_position", "[ore][xml][scanner]") {
    reader input("<a>\n  <b></a>");
    for (;;) {
        const auto ev = input.next();
        if (ev.kind == event_type::error) {
            CHECK(input.failed());
            CHECK(input.line() == 2);
            return;
        }
        if (ev.kind == event_type::end_of_document)
            break;
    }
    FAIL("a mismatched end tag was accepted");
}

TEST_CASE("a_declaration_is_rejected", "[ore][xml][scanner]") {
    reader input("<!DOCTYPE a><a/>");
    CHECK(input.next().kind == event_type::error);
}
