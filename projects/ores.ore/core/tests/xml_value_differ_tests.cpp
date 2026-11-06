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
#include "ores.ore.core/xml/value_differ.hpp"
#include "ores.ore.core/xml/value_fingerprint.hpp"
#include "test_suite.hpp"
#include <catch2/catch_test_macros.hpp>
#include <string>
#include <string_view>
#include <vector>

namespace {

using namespace ores::logging;
using namespace ores::ore::xml;

auto fingerprint_of(std::string_view text) -> std::vector<value_entry> {
    const auto doc = document::read(text);
    REQUIRE(doc);
    return value_fingerprint(*doc);
}

auto differences(std::string_view expected, std::string_view actual) -> std::vector<std::string> {
    std::vector<std::string> out;
    for (const auto& difference : diff_values(fingerprint_of(expected), fingerprint_of(actual)))
        out.push_back(describe(difference));
    return out;
}

constexpr std::string_view option = "<FxOptionData><BoughtAmount>100</BoughtAmount>"
                                    "<OptionData><Style>European</Style>"
                                    "<LongShort>Long</LongShort></OptionData></FxOptionData>";

}

TEST_CASE("documents that state the same values have no differences", "[ore][xml][differ]") {
    auto lg(make_logger(test_suite));
    CHECK(differences(option, option).empty());
}

TEST_CASE("a value the actual document leaves out is lost", "[ore][xml][differ]") {
    auto lg(make_logger(test_suite));
    const auto found = differences(option,
                                   "<FxOptionData><BoughtAmount>100</BoughtAmount>"
                                   "<OptionData><LongShort>Long</LongShort></OptionData>"
                                   "</FxOptionData>");
    CHECK(found == std::vector<std::string>{"lost FxOptionData/OptionData/Style: 'European'"});
}

TEST_CASE("a value only the actual document states is added", "[ore][xml][differ]") {
    auto lg(make_logger(test_suite));
    const auto found = differences("<A><B>1</B></A>", "<A><B>1</B><C>x</C></A>");
    CHECK(found == std::vector<std::string>{"added A/C: 'x'"});
}

TEST_CASE("a value both documents state differently is changed", "[ore][xml][differ]") {
    auto lg(make_logger(test_suite));
    const auto found = differences(option,
                                   "<FxOptionData><BoughtAmount>200</BoughtAmount>"
                                   "<OptionData><Style>European</Style>"
                                   "<LongShort>Long</LongShort></OptionData></FxOptionData>");
    CHECK(found ==
          std::vector<std::string>{"changed FxOptionData/BoughtAmount: '100' became '200'"});
}

TEST_CASE("every difference is reported, not the first", "[ore][xml][differ]") {
    auto lg(make_logger(test_suite));
    const auto found = differences(option,
                                   "<FxOptionData><BoughtAmount>200</BoughtAmount>"
                                   "</FxOptionData>");
    CHECK(found == std::vector<std::string>{
                       "changed FxOptionData/BoughtAmount: '100' became '200'",
                       "lost FxOptionData/OptionData/LongShort: 'Long'",
                       "lost FxOptionData/OptionData/Style: 'European'",
                   });
}

TEST_CASE("a number spelled two ways is one value", "[ore][xml][differ]") {
    auto lg(make_logger(test_suite));
    CHECK(
        differences("<A><B>1.0</B><C>0.0000000001</C></A>", "<A><B>1</B><C>1e-10</C></A>").empty());
}

TEST_CASE("an empty element is the same as an absent one", "[ore][xml][differ]") {
    auto lg(make_logger(test_suite));
    CHECK(differences("<A><B>1</B><C/><D>\n  </D></A>", "<A><B>1</B></A>").empty());
    CHECK(differences("<A><B>1</B></A>", "<A><B>1</B><C></C></A>").empty());
    CHECK(differences("<A><B>1</B><C/></A>", "<A><B>1</B><C>2</C></A>") ==
          std::vector<std::string>{"added A/C: '2'"});
}

TEST_CASE("the order of siblings is not a difference", "[ore][xml][differ]") {
    auto lg(make_logger(test_suite));
    CHECK(differences("<A><B>1</B><C>2</C></A>", "<A><C>2</C><B>1</B></A>").empty());
}

TEST_CASE("a repeated element is compared as many values at one path", "[ore][xml][differ]") {
    auto lg(make_logger(test_suite));
    const auto expected = "<Dates><Date>2026-01-01</Date><Date>2026-02-01</Date>"
                          "<Date>2026-03-01</Date></Dates>";

    CHECK(differences(expected,
                      "<Dates><Date>2026-03-01</Date><Date>2026-01-01</Date>"
                      "<Date>2026-02-01</Date></Dates>")
              .empty());
    CHECK(differences(expected, "<Dates><Date>2026-01-01</Date><Date>2026-03-01</Date></Dates>") ==
          std::vector<std::string>{"lost Dates/Date: '2026-02-01'"});
    CHECK(differences(expected,
                      "<Dates><Date>2026-01-01</Date><Date>2026-02-02</Date>"
                      "<Date>2026-03-01</Date></Dates>") ==
          std::vector<std::string>{"changed Dates/Date: '2026-02-01' became '2026-02-02'"});
}

TEST_CASE("an attribute is a value of its own", "[ore][xml][differ]") {
    auto lg(make_logger(test_suite));
    CHECK(differences("<Trade id=\"a\"><B>1</B></Trade>", "<Trade id=\"b\"><B>1</B></Trade>") ==
          std::vector<std::string>{"changed Trade/@id: 'a' became 'b'"});
}

TEST_CASE("each occurrence of an element is fingerprinted under its key", "[ore][xml][differ]") {
    auto lg(make_logger(test_suite));
    const auto doc = document::read("<Portfolio>"
                                    "<Trade id=\"t1\"><TradeType>FxOption</TradeType></Trade>"
                                    "<Trade id=\"t2\"><TradeType>Swap</TradeType>"
                                    "<SwapData><LegData><Payer>true</Payer></LegData></SwapData>"
                                    "</Trade></Portfolio>");
    REQUIRE(doc);

    const auto trades = value_fingerprints_of(*doc, "Trade", "id");

    REQUIRE(trades.size() == 2);
    CHECK(trades[0].key == "t1");
    CHECK(trades[1].key == "t2");
    REQUIRE(trades[0].values.size() == 2);
    CHECK(trades[0].values[0].path == "Trade/@id");
    CHECK(trades[0].values[1].path == "Trade/TradeType");
    CHECK(trades[0].values[1].value == "FxOption");
    CHECK(trades[1].values.size() == 3);
    CHECK(trades[1].values[1].path == "Trade/SwapData/LegData/Payer");
}

TEST_CASE("two trades compare by their keys whatever their order", "[ore][xml][differ]") {
    auto lg(make_logger(test_suite));
    const auto read = document::read("<Portfolio>"
                                     "<Trade id=\"t1\"><Notional>1</Notional></Trade>"
                                     "<Trade id=\"t2\"><Notional>2</Notional></Trade>"
                                     "</Portfolio>");
    const auto written = document::read("<Portfolio>"
                                        "<Trade id=\"t2\"><Notional>2</Notional></Trade>"
                                        "<Trade id=\"t1\"><Notional>3</Notional></Trade>"
                                        "</Portfolio>");
    REQUIRE(read);
    REQUIRE(written);

    const auto before = value_fingerprints_of(*read, "Trade", "id");
    const auto after = value_fingerprints_of(*written, "Trade", "id");
    REQUIRE(before.size() == 2);
    REQUIRE(after.size() == 2);

    CHECK(diff_values(before[1].values, after[0].values).empty());
    const auto changed = diff_values(before[0].values, after[1].values);
    REQUIRE(changed.size() == 1);
    CHECK(describe(changed[0]) == "changed Trade/Notional: '1' became '3'");
}

TEST_CASE("a repeated element of a trade is labelled with its position", "[ore][xml][differ]") {
    auto lg(make_logger(test_suite));
    const auto doc = document::read("<Portfolio><Trade id=\"t1\"><SwapData>"
                                    "<LegData><Payer>true</Payer></LegData>"
                                    "<LegData><Payer>false</Payer></LegData>"
                                    "<Currency>EUR</Currency>"
                                    "</SwapData></Trade></Portfolio>");
    REQUIRE(doc);

    const auto trades = value_fingerprints_of(*doc, "Trade", "id");

    REQUIRE(trades.size() == 1);
    std::vector<std::string> paths;
    for (const auto& value : trades[0].values)
        paths.push_back(value.path + "=" + value.value);
    CHECK(paths == std::vector<std::string>{
                       "Trade/@id=t1",
                       "Trade/SwapData/Currency=EUR",
                       "Trade/SwapData/LegData[1]/Payer=true",
                       "Trade/SwapData/LegData[2]/Payer=false",
                   });
}

TEST_CASE("a value one leg lost is not read as a value the other changed", "[ore][xml][differ]") {
    auto lg(make_logger(test_suite));
    const auto read = document::read("<Trade id=\"t\"><LegData><Rate>1</Rate></LegData>"
                                     "<LegData><Rate>2</Rate></LegData></Trade>");
    const auto written = document::read("<Trade id=\"t\"><LegData><Rate>1</Rate></LegData>"
                                        "<LegData></LegData></Trade>");
    REQUIRE(read);
    REQUIRE(written);

    const auto found = diff_values(value_fingerprints_of(*read, "Trade", "id")[0].values,
                                   value_fingerprints_of(*written, "Trade", "id")[0].values);

    REQUIRE(found.size() == 1);
    CHECK(describe(found[0]) == "lost Trade/LegData[2]/Rate: '2'");
}
