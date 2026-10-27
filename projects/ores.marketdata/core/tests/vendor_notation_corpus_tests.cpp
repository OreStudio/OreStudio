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
#include "ores.marketdata.core/vendor/bloomberg_notation.hpp"
#include "ores.marketdata.core/vendor/demo_notation.hpp"
#include "ores.marketdata.core/vendor/lseg_notation.hpp"
#include "ores.marketdata.core/vendor/provider_notation.hpp"
#include "ores.testing/project_root.hpp"
#include <algorithm>
#include <catch2/catch_test_macros.hpp>
#include <fstream>
#include <map>
#include <sstream>
#include <string>
#include <vector>

// The notations of the providers are independent classes: they need no database, no NATS and no
// catalogue. This file tests them on their own, against a corpus of ids and descriptions kept
// under corpus/vendor_notation. The corpus is data a person can read and extend, and the
// expectations in it come from the providers' documented examples, not from the code under test.
//
// Three things are proved for every provider:
// - every id in the corpus reads as the corpus says, or is refused for the reason it says;
// - every description in the corpus writes as the corpus says, or is refused for the reason it
//   says;
// - what a notation reads it writes back, and what it writes it reads back, so a notation
//   never turns an id into another id or a description into another description.

namespace {

namespace vendor = ores::marketdata::vendor;
namespace messaging = ores::marketdata::messaging;

const std::vector<std::string> providers{"bloomberg", "lseg", "demo"};

std::vector<std::string> split(const std::string& line, char separator) {
    std::vector<std::string> out;
    std::string cell;
    std::istringstream in(line);
    while (std::getline(in, cell, separator))
        out.push_back(cell);
    // getline drops an empty last cell, which a row with an empty field ends in.
    if (!line.empty() && line.back() == separator)
        out.emplace_back();
    return out;
}

// The rows of a corpus file: a comment line starts with '#', and a blank line is skipped.
std::vector<std::string> rows_of(const std::string& name) {
    const auto path = ores::testing::project_root::resolve(
        "projects/ores.marketdata/core/tests/corpus/vendor_notation/" + name);
    std::ifstream in(path);
    REQUIRE(in.good());
    std::vector<std::string> rows;
    for (std::string line; std::getline(in, line);)
        if (!line.empty() && line.front() != '#')
            rows.push_back(line);
    REQUIRE_FALSE(rows.empty());
    return rows;
}

vendor::price_side side_named(const std::string& text) {
    static const std::map<std::string, vendor::price_side> sides{{"last", vendor::price_side::last},
                                                                 {"bid", vendor::price_side::bid},
                                                                 {"ask", vendor::price_side::ask},
                                                                 {"mid", vendor::price_side::mid}};
    return sides.at(text);
}

vendor::notation_error::kind kind_named(const std::string& text) {
    using kind = vendor::notation_error::kind;
    static const std::map<std::string, kind> kinds{{"wrong_scheme", kind::wrong_scheme},
                                                   {"wrong_field", kind::wrong_field},
                                                   {"malformed", kind::malformed},
                                                   {"unsupported", kind::unsupported},
                                                   {"not_representable", kind::not_representable}};
    return kinds.at(text);
}

const vendor::provider_notation& notation(const std::string& provider) {
    const auto* found = vendor::notation_of(provider);
    REQUIRE(found != nullptr);
    return *found;
}

vendor::quote_description fx(std::string base, std::string quote, vendor::price_side side) {
    return vendor::fx_spot_quote{std::move(base), std::move(quote), side};
}

vendor::quote_description pillar(std::string currency,
                                 std::string family,
                                 std::string instrument,
                                 std::string start,
                                 std::string end) {
    return vendor::ir_curve_pillar{std::move(currency),
                                   std::move(family),
                                   std::move(instrument),
                                   std::move(start),
                                   std::move(end)};
}

} // namespace

TEST_CASE("the_registry_lists_the_providers_that_have_a_notation", "[vendor_notation][corpus]") {
    for (const auto& provider : providers) {
        const auto* found = vendor::notation_of(provider);
        REQUIRE(found != nullptr);
        CHECK(found->provider() == provider);
    }
    CHECK(vendor::notation_of("ore") == nullptr);
    CHECK(vendor::notation_of("ice") == nullptr);
    CHECK(vendor::notation_of("") == nullptr);
}

TEST_CASE("every_id_in_the_corpus_reads_as_the_corpus_says_and_roundtrips",
          "[vendor_notation][corpus]") {
    for (const auto& provider : providers) {
        const auto& sut = notation(provider);
        std::size_t valid = 0;
        std::size_t refused = 0;
        for (const auto& row : rows_of("ids_" + provider + ".csv")) {
            INFO(provider << ": " << row);
            const auto cells = split(row, ',');
            REQUIRE(cells.size() == 4);
            const messaging::vendor_id id{
                .id_scheme = cells[0], .external_id = cells[1], .field = cells[2]};
            const auto outcome = split(cells[3], ' ');

            const auto read = sut.read(id);
            if (outcome[0] == "fx" || outcome[0] == "pillar") {
                REQUIRE(read.has_value());
                if (outcome[0] == "fx") {
                    REQUIRE(outcome.size() == 4);
                    CHECK(*read == fx(outcome[1], outcome[2], side_named(outcome[3])));
                } else {
                    REQUIRE(outcome.size() == 6);
                    CHECK(*read ==
                          pillar(outcome[1], outcome[2], outcome[3], outcome[4], outcome[5]));
                }
                // The id is canonical: writing what was read gives the same id.
                const auto written = sut.write(*read);
                REQUIRE(written.has_value());
                CHECK(written->id_scheme == id.id_scheme);
                CHECK(written->external_id == id.external_id);
                CHECK(written->field == id.field);
                ++valid;
            } else {
                REQUIRE(outcome[0] == "error");
                REQUIRE_FALSE(read.has_value());
                CHECK(read.error().reason == kind_named(outcome[1]));
                CHECK_FALSE(read.error().message.empty());
                ++refused;
            }
        }
        CHECK(valid > 20);
        CHECK(refused > 5);
    }
}

TEST_CASE("every_pillar_in_the_corpus_writes_as_the_corpus_says_and_roundtrips",
          "[vendor_notation][corpus]") {
    std::size_t written_ids = 0;
    std::size_t refusals = 0;
    for (const auto& row : rows_of("ir_curve_pillar_descriptions.csv")) {
        INFO(row);
        const auto cells = split(row, ',');
        REQUIRE(cells.size() == 8);
        const auto description = pillar(cells[0], cells[1], cells[2], cells[3], cells[4]);
        for (std::size_t i = 0; i < providers.size(); ++i) {
            INFO(providers[i] << ": " << cells[5 + i]);
            const auto& sut = notation(providers[i]);
            const auto& expected = cells[5 + i];
            const auto written = sut.write(description);
            if (expected.starts_with("error:")) {
                REQUIRE_FALSE(written.has_value());
                CHECK(written.error().reason == kind_named(expected.substr(6)));
                ++refusals;
                continue;
            }
            const auto parts = split(expected, '|');
            REQUIRE(parts.size() == 2);
            REQUIRE(written.has_value());
            CHECK(written->id_scheme == sut.id_scheme());
            CHECK(written->external_id == parts[0]);
            CHECK(written->field == parts[1]);
            // What a notation writes it reads back as the same description.
            const auto read = sut.read(*written);
            REQUIRE(read.has_value());
            CHECK(*read == description);
            ++written_ids;
        }
    }
    CHECK(written_ids >= 27);
    CHECK(refusals >= 33);
}

TEST_CASE("a_pair_is_not_read_as_a_pillar_and_a_pillar_is_not_read_as_a_pair",
          "[vendor_notation][corpus]") {
    const std::vector<messaging::vendor_id> pairs{{"bbg_ticker", "EURUSD Curncy", "PX_LAST"},
                                                  {"ric", "EUR=", "BID"},
                                                  {"demo_key", "EUR/USD", "value"}};
    const std::vector<messaging::vendor_id> pillars{
        {"bbg_ticker", "USD SOFR DEPO SPOT 3M Curncy", "PX_LAST"},
        {"ric", "USDSOFR.DEPO.SPOT.3M=", "BID"},
        {"demo_key", "USD/sofr/DEPO/SPOT/3M", "value"}};

    for (const auto& provider : providers) {
        const auto& sut = notation(provider);
        for (const auto& pair : pairs) {
            if (pair.id_scheme != sut.id_scheme())
                continue;
            const auto read = sut.read(pair);
            REQUIRE(read.has_value());
            CHECK(std::holds_alternative<vendor::fx_spot_quote>(*read));
        }
        for (const auto& one : pillars) {
            if (one.id_scheme != sut.id_scheme())
                continue;
            const auto read = sut.read(one);
            REQUIRE(read.has_value());
            CHECK(std::holds_alternative<vendor::ir_curve_pillar>(*read));
        }
    }
}

TEST_CASE("every_description_in_the_corpus_writes_as_the_corpus_says_and_roundtrips",
          "[vendor_notation][corpus]") {
    std::size_t written_ids = 0;
    std::size_t refusals = 0;
    for (const auto& row : rows_of("fx_spot_descriptions.csv")) {
        INFO(row);
        const auto cells = split(row, ',');
        REQUIRE(cells.size() == 6);
        const auto description = fx(cells[0], cells[1], side_named(cells[2]));
        for (std::size_t i = 0; i < providers.size(); ++i) {
            INFO(providers[i] << ": " << cells[3 + i]);
            const auto& sut = notation(providers[i]);
            const auto& expected = cells[3 + i];
            const auto written = sut.write(description);
            if (expected.starts_with("error:")) {
                REQUIRE_FALSE(written.has_value());
                CHECK(written.error().reason == kind_named(expected.substr(6)));
                ++refusals;
                continue;
            }
            const auto parts = split(expected, '|');
            REQUIRE(parts.size() == 2);
            REQUIRE(written.has_value());
            CHECK(written->id_scheme == sut.id_scheme());
            CHECK(written->external_id == parts[0]);
            CHECK(written->field == parts[1]);
            // What a notation writes it reads back as the same description.
            const auto read = sut.read(*written);
            REQUIRE(read.has_value());
            CHECK(*read == description);
            ++written_ids;
        }
    }
    CHECK(written_ids > 200);
    CHECK(refusals > 50);
}

TEST_CASE("no_notation_writes_an_id_that_reads_as_another_description",
          "[vendor_notation][corpus]") {
    // The property over a grid the corpus does not list: every ordered pair of a set of
    // currencies, with every side, through every notation.
    const std::vector<std::string> currencies{
        "USD", "EUR", "GBP", "JPY", "CHF", "AUD", "NZD", "CAD", "SEK", "NOK", "MXN", "ZAR"};
    std::size_t checked = 0;
    for (const auto& provider : providers) {
        const auto& sut = notation(provider);
        for (const auto& base : currencies)
            for (const auto& quote : currencies)
                for (const auto side : {vendor::price_side::last,
                                        vendor::price_side::bid,
                                        vendor::price_side::ask,
                                        vendor::price_side::mid}) {
                    const auto description = fx(base, quote, side);
                    const auto written = sut.write(description);
                    if (!written)
                        continue;
                    const auto read = sut.read(*written);
                    INFO(provider << " " << base << "/" << quote);
                    REQUIRE(read.has_value());
                    CHECK(*read == description);
                    ++checked;
                }
    }
    CHECK(checked > 500);
}

TEST_CASE("a_damaged_id_is_refused_or_read_as_exactly_that_id", "[vendor_notation][corpus]") {
    // Take each valid id of the corpus and damage it one character at a time. A notation that
    // reads the damaged id must write the damaged id back, so it never repairs an id quietly or
    // reads a different one.
    std::size_t damaged = 0;
    std::size_t still_read = 0;
    for (const auto& provider : providers) {
        const auto& sut = notation(provider);
        for (const auto& row : rows_of("ids_" + provider + ".csv")) {
            const auto cells = split(row, ',');
            const auto outcome = split(cells[3], ' ')[0];
            if (outcome != "fx" && outcome != "pillar")
                continue;
            const messaging::vendor_id id{
                .id_scheme = cells[0], .external_id = cells[1], .field = cells[2]};
            for (std::size_t i = 0; i < id.external_id.size(); ++i) {
                for (const char replacement : {'!', 'x', '0', ' ', '='}) {
                    auto bad = id;
                    bad.external_id[i] = replacement;
                    ++damaged;
                    const auto read = sut.read(bad);
                    if (!read)
                        continue;
                    ++still_read;
                    const auto written = sut.write(*read);
                    INFO(provider << ": '" << bad.external_id << "'");
                    REQUIRE(written.has_value());
                    CHECK(written->external_id == bad.external_id);
                    CHECK(written->field == bad.field);
                }
            }
            auto cut = id;
            while (!cut.external_id.empty()) {
                cut.external_id.pop_back();
                ++damaged;
                const auto read = sut.read(cut);
                if (!read)
                    continue;
                ++still_read;
                const auto written = sut.write(*read);
                INFO(provider << ": '" << cut.external_id << "'");
                REQUIRE(written.has_value());
                CHECK(written->external_id == cut.external_id);
            }
        }
    }
    CHECK(damaged > 3000);
    CHECK(still_read < damaged);
}

TEST_CASE("the_primary_side_of_each_notation_is_a_field_it_writes", "[vendor_notation][corpus]") {
    for (const auto& provider : providers) {
        const auto& sut = notation(provider);
        const auto written = sut.write(fx("EUR", "GBP", sut.primary_side()));
        INFO(provider);
        CHECK(written.has_value());
        CHECK(written->id_scheme == sut.id_scheme());
    }
}

TEST_CASE("a_notation_names_the_scheme_it_refuses_a_foreign_id_with", "[vendor_notation][corpus]") {
    const messaging::vendor_id bloomberg{"bbg_ticker", "EURUSD Curncy", "PX_LAST"};
    const messaging::vendor_id lseg{"ric", "EUR=", "BID"};
    const messaging::vendor_id demo{"demo_key", "EUR/USD", "value"};

    for (const auto& provider : providers) {
        const auto& sut = notation(provider);
        for (const auto* foreign : {&bloomberg, &lseg, &demo}) {
            if (foreign->id_scheme == sut.id_scheme())
                continue;
            const auto read = sut.read(*foreign);
            REQUIRE_FALSE(read.has_value());
            CHECK(read.error().reason == vendor::notation_error::kind::wrong_scheme);
        }
    }
}
