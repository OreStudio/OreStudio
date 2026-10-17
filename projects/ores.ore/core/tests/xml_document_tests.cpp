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
#include "ores.ore.core/xml/document.hpp"
#include <catch2/catch_test_macros.hpp>
#include <string>

namespace {

const std::string tags("[ore][xml][document]");

using ores::ore::xml::document;
using ores::ore::xml::parse_error;

auto read_ok(const std::string& text) -> document {
    parse_error error;
    auto parsed = document::read(text, error);
    REQUIRE(parsed.has_value());
    return *parsed;
}

auto round_trip(const std::string& text) -> std::string {
    std::string out;
    read_ok(text).write(out);
    return out;
}

}

TEST_CASE("an_element_with_text_survives_a_round_trip", tags) {
    const std::string original = "<CurrencyConfig><Currency><ISOCode>USD</ISOCode></Currency>"
                                 "</CurrencyConfig>";
    const auto written = round_trip(original);

    CHECK(written == "<CurrencyConfig>\n  <Currency>\n    <ISOCode>USD</ISOCode>\n"
                     "  </Currency>\n</CurrencyConfig>\n");
    CHECK(read_ok(original) == read_ok(written));
}

TEST_CASE("an_empty_element_is_written_self_closing_and_still_reads_back", tags) {
    const auto written = round_trip("<a><b></b></a>");

    CHECK(written == "<a>\n  <b/>\n</a>\n");
    CHECK(read_ok("<a><b></b></a>") == read_ok(written));
}

TEST_CASE("whitespace_between_elements_is_formatting_and_is_dropped", tags) {
    const auto written = round_trip("<a>\n\n   <b/>\t</a>");

    CHECK(read_ok("<a><b/></a>") == read_ok(written));
}

TEST_CASE("whitespace_that_is_the_content_of_an_element_is_kept", tags) {
    const auto written = round_trip("<a><b> </b></a>");

    CHECK(written.find("<b> </b>") != std::string::npos);
    CHECK(read_ok("<a><b> </b></a>") == read_ok(written));
}

TEST_CASE("markup_characters_in_text_are_escaped_and_read_back", tags) {
    const auto written = round_trip("<a>x &lt; y &amp; z</a>");

    CHECK(written.find("x &lt; y &amp; z") != std::string::npos);
    CHECK(read_ok("<a>x &lt; y &amp; z</a>") == read_ok(written));
}

TEST_CASE("an_attribute_keeps_its_quotes_and_tabs", tags) {
    const auto written = round_trip("<a x=\"1&quot;2\"/>");

    CHECK(written.find("x=\"1&quot;2\"") != std::string::npos);
    CHECK(read_ok("<a x=\"1&quot;2\"/>") == read_ok(written));
}

TEST_CASE("mixed_content_is_not_indented", tags) {
    const auto written = round_trip("<a>before<b>inner</b>after</a>");

    CHECK(written == "<a>before<b>inner</b>after</a>\n");
    CHECK(read_ok("<a>before<b>inner</b>after</a>") == read_ok(written));
}

TEST_CASE("a_cdata_section_survives_the_trip_as_escaped_text", tags) {
    const std::string original = "<a><![CDATA[if (x < 1 && y > 2) {}]]></a>";
    const auto written = round_trip(original);

    CHECK(written.find("if (x &lt; 1 &amp;&amp; y &gt; 2) {}") != std::string::npos);
    CHECK(read_ok(original) == read_ok(written));
}

TEST_CASE("a_number_keeps_every_digit_it_arrived_with", tags) {
    for (const std::string& value : {"1000000.00",
                                     "0.0000000000010000",
                                     "-12345.678901234567",
                                     "7342.8000000000002",
                                     "0.000054"}) {
        const auto written = round_trip("<a>" + value + "</a>");
        CHECK(written.find(">" + value + "<") != std::string::npos);
        CHECK(read_ok("<a>" + value + "</a>") == read_ok(written));
    }
}

TEST_CASE("documents_that_differ_in_a_value_are_not_equal", tags) {
    CHECK_FALSE(read_ok("<a>1</a>") == read_ok("<a>2</a>"));
    CHECK_FALSE(read_ok("<a>b=\"1\"/>") == read_ok("<a>b=\"2\"/>"));
    CHECK_FALSE(read_ok("<a><b/></a>") == read_ok("<a><c/></a>"));
}

TEST_CASE("empty_text_does_not_name_the_root", tags) {
    CHECK(read_ok("<Portfolio/>").root() == "Portfolio");
    CHECK(document().root().empty());
}
