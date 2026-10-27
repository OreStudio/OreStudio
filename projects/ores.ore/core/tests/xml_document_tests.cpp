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
#include "ores.ore.core/xml/document.hpp"
#include "test_suite.hpp"
#include <catch2/catch_test_macros.hpp>
#include <string>

namespace {

using ores::ore::xml::document;
using namespace ores::logging;

auto read_ok(const std::string& text) -> document {
    const auto parsed = document::read(text);
    REQUIRE(parsed.has_value());
    return *parsed;
}

auto round_trip(const std::string& text) -> std::string {
    std::string out;
    read_ok(text).write(out);
    return out;
}

}

TEST_CASE("an_element_with_text_survives_a_round_trip", "[ore][xml][document]") {
    auto lg(make_logger(test_suite));

    const std::string original = "<CurrencyConfig><Currency><ISOCode>USD</ISOCode></Currency>"
                                 "</CurrencyConfig>";
    const auto written = round_trip(original);

    CHECK(written == "<CurrencyConfig>\n  <Currency>\n    <ISOCode>USD</ISOCode>\n"
                     "  </Currency>\n</CurrencyConfig>\n");
    CHECK(read_ok(original) == read_ok(written));
}

TEST_CASE("an_empty_element_is_written_self_closing_and_still_reads_back", "[ore][xml][document]") {
    auto lg(make_logger(test_suite));

    const auto written = round_trip("<a><b></b></a>");

    CHECK(written == "<a>\n  <b/>\n</a>\n");
    CHECK(read_ok("<a><b></b></a>") == read_ok(written));
}

TEST_CASE("whitespace_between_elements_is_formatting_and_is_dropped", "[ore][xml][document]") {
    auto lg(make_logger(test_suite));

    const auto written = round_trip("<a>\n\n   <b/>\t</a>");

    CHECK(written == "<a>\n  <b/>\n</a>\n");
    CHECK(read_ok("<a><b/></a>") == read_ok(written));
}

TEST_CASE("whitespace_that_is_the_content_of_an_element_is_kept", "[ore][xml][document]") {
    auto lg(make_logger(test_suite));

    const auto written = round_trip("<a><b> </b></a>");

    CHECK(written.contains("<b> </b>"));
    CHECK(read_ok("<a><b> </b></a>") == read_ok(written));
}

TEST_CASE("markup_characters_in_text_are_escaped_and_read_back", "[ore][xml][document]") {
    auto lg(make_logger(test_suite));

    const auto written = round_trip("<a>x &lt; y &amp; z</a>");

    CHECK(written.contains("x &lt; y &amp; z"));
    CHECK(read_ok("<a>x &lt; y &amp; z</a>") == read_ok(written));
}

TEST_CASE("an_attribute_keeps_a_quote_that_was_a_character_reference", "[ore][xml][document]") {
    auto lg(make_logger(test_suite));

    const auto written = round_trip("<a x=\"1&quot;2\"/>");

    CHECK(written.contains("x=\"1&quot;2\""));
    CHECK(read_ok("<a x=\"1&quot;2\"/>") == read_ok(written));
}

TEST_CASE("a_tab_in_an_attribute_is_normalised_to_a_space", "[ore][xml][document]") {
    auto lg(make_logger(test_suite));

    // A literal tab is a space once the document is normalised, so a tab can
    // only reach the tape through a character reference. Writing the space
    // back raw is what keeps the two sides agreeing.
    const auto written = round_trip("<a x=\"p\tq\"/>");

    CHECK(written == "<a x=\"p q\"/>\n");
    CHECK(read_ok(written) == read_ok("<a x=\"p q\"/>"));
}

TEST_CASE("a_tab_written_as_a_character_reference_survives", "[ore][xml][document]") {
    auto lg(make_logger(test_suite));

    const auto written = round_trip("<a x=\"p&#9;q\"/>");

    CHECK(written.contains("&#9;"));
    CHECK(read_ok(written) == read_ok("<a x=\"p&#9;q\"/>"));
}

TEST_CASE("a_carriage_return_is_normalised_and_can_be_written_back", "[ore][xml][document]") {
    auto lg(make_logger(test_suite));

    // A literal carriage return is a line feed after normalisation, so only a
    // reference can carry one, and the writer must write it as a reference.
    CHECK(round_trip("<a>p\rq</a>") == "<a>p\nq</a>\n");
    const auto written = round_trip("<a>p&#13;q</a>");
    CHECK(written.contains("&#13;"));
    CHECK(read_ok(written) == read_ok("<a>p&#13;q</a>"));
}

TEST_CASE("a_numeric_character_reference_is_read_as_its_character", "[ore][xml][document]") {
    auto lg(make_logger(test_suite));

    const auto parsed = read_ok("<a>&#65;&#x42;</a>");
    CHECK(parsed.value(parsed.tokens()[1]) == "AB");
}

TEST_CASE("mixed_content_is_not_indented", "[ore][xml][document]") {
    auto lg(make_logger(test_suite));

    const auto written = round_trip("<a>before<b>inner</b>after</a>");

    CHECK(written == "<a>before<b>inner</b>after</a>\n");
    CHECK(read_ok("<a>before<b>inner</b>after</a>") == read_ok(written));
}

TEST_CASE("a_cdata_section_survives_the_trip_as_escaped_text", "[ore][xml][document]") {
    auto lg(make_logger(test_suite));

    const std::string original = "<a><![CDATA[if (x < 1 && y > 2) {}]]></a>";
    const auto written = round_trip(original);

    CHECK(written.contains("if (x &lt; 1 &amp;&amp; y &gt; 2) {}"));
    CHECK(read_ok(original) == read_ok(written));
}

TEST_CASE("a_number_keeps_every_digit_it_arrived_with", "[ore][xml][document]") {
    auto lg(make_logger(test_suite));

    for (const std::string value : {"1000000.00",
                                    "0.0000000000010000",
                                    "-12345.678901234567",
                                    "7342.8000000000002",
                                    "0.000054"}) {
        const auto written = round_trip("<a>" + value + "</a>");
        CHECK(written.contains(">" + value + "<"));
        CHECK(read_ok("<a>" + value + "</a>") == read_ok(written));
    }
}

TEST_CASE("documents_that_differ_in_a_value_are_not_equal", "[ore][xml][document]") {
    auto lg(make_logger(test_suite));

    CHECK_FALSE(read_ok("<a>1</a>") == read_ok("<a>2</a>"));
    CHECK_FALSE(read_ok("<a b=\"1\"/>") == read_ok("<a b=\"2\"/>"));
    CHECK_FALSE(read_ok("<a><b/></a>") == read_ok("<a><c/></a>"));
}

TEST_CASE("a_document_needs_exactly_one_root_element", "[ore][xml][document]") {
    auto lg(make_logger(test_suite));

    CHECK_FALSE(document::read("hello").has_value());
    CHECK_FALSE(document::read("").has_value());
    CHECK_FALSE(document::read("<a/><b/>").has_value());
    CHECK(document::read("  <a/>  ").has_value());
    CHECK(read_ok("<Portfolio/>").root() == "Portfolio");
}
