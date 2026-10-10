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
#include "datum_catalogue.hpp"
#include "ores.marketdata.core/datum/ore_index_codec.hpp"
#include "ores.marketdata.core/datum/oresmd_uri_codec.hpp"
#include "ores.platform/filesystem/file.hpp"
#include "ores.testing/project_root.hpp"
#include <catch2/catch_test_macros.hpp>
#include <map>
#include <set>
#include <sstream>
#include <string>
#include <vector>

/**
 * @file datum_ore_index_codec_tests.cpp
 * @brief The ORE index codec against ORE's own parseIndex.
 *
 * The index catalogue records, for every index form parseIndex documents and
 * every index name the corpus's fixing files carry, the index class ORE builds
 * and its family's inspectors, or ORE's error. The codec must accept what ORE
 * accepts and refuse what it refuses, but for the names listed below with why,
 * write each name back as it read it, and hold each part where ORE does.
 */

namespace {

const std::string tags("[marketdata][datum][ore_index_codec]");

using namespace ores::marketdata::datum;
using ores::marketdata::test::accepted_by_ore;
using ores::marketdata::test::catalogue_index_corpus;
using ores::marketdata::test::catalogue_index_forms;
using ores::marketdata::test::catalogue_line;

/// The names in both maps are negative probes from index_forms.txt, not ORE
/// data: the codec and ORE agree on every corpus name.
///
/// Names ORE accepts and the codec refuses: ORE completes them from outside the
/// name, so no index could write them back.
const std::map<std::string, std::string> refused_on_purpose{
    {"EQ-", "ORE builds an equity index with no name"},
    {"BOND-", "ORE builds a bond index with no security"},
    {"COMM-", "ORE builds a commodity index with no name"},
    {"POWER-ICE:PDQ", "ORE takes the delivery date from the evaluation date"},
};

/// Names ORE refuses and the codec accepts: the codec reads by structure, as
/// agreed, so a name ORE refuses for want of data outside the grammar reads.
const std::map<std::string, std::string> accepted_by_structure{
    {"FX-ECB-EUR-XXX", "ORE checks its own currency list; the codec takes three letters"},
    {"CMB-US-TREASURY-10Y", "ORE needs the bond's reference data"},
    {"CMB-BUND-5Y", "ORE needs the bond's reference data"},
    {"EUR", "a hyphen-free name reads as an inflation index a convention may define"},
    {"NOSUCHCPI", "a hyphen-free name reads as an inflation index a convention may define"},
};

/// The spelling the codec writes for a name ORE reads as an alias.
const std::map<std::string, std::string> canonical_spellings{
    {"UK RPI", "UKRPI"},
    {"EU HICPXT", "EUHICPXT"},
};

/// Whether two tenors are one period, as QuantLib reads them: 7D is 1W.
bool same_period(const std::string& a, const std::string& b) {
    const auto days = [](const std::string& t) -> long {
        const long n = std::stol(t.substr(0, t.size() - 1));
        switch (t.back()) {
            case 'D':
                return n;
            case 'W':
                return 7 * n;
            case 'M':
                return 10000 * n;
            default:
                return 120000 * n;
        }
    };
    return days(a) == days(b);
}

/// The month or day an expiry names, as ORE prints the date it builds.
std::string as_date(const std::string& expiry) {
    return expiry.size() == 7 ? expiry + "-01" : expiry;
}

std::string at(const catalogue_line& line, const std::string& key) {
    const auto it = line.find(key);
    return it == line.end() ? std::string("(absent)") : it->second;
}

/// Every difference between the index and ORE's reading of the same name.
std::vector<std::string> differences(const market_index& index, const catalogue_line& ore) {
    std::vector<std::string> out;
    const auto& cls = at(ore, "class");
    const auto expect = [&](bool ok, const std::string& what) {
        if (!ok)
            out.push_back(what + " (ORE class " + cls + ")");
    };
    const auto* tenor = index.get("tenor");
    const auto* expiry = index.get("expiry");
    switch (index.family()) {
        case index_family::equity:
            expect(cls == "QuantExt::EquityIndex2", "class");
            expect(at(ore, "familyName") == index.subject(), "name");
            break;
        case index_family::bond:
            if (expiry) {
                expect(cls == "QuantExt::BondFuturesIndex", "class");
                expect(at(ore, "futureContract") == index.subject(), "security");
                expect(at(ore, "futureExpiryDate") == as_date(*expiry), "expiry");
            } else {
                expect(cls == "QuantExt::BondIndex", "class");
                expect(at(ore, "securityName") == index.subject(), "security");
            }
            break;
        case index_family::bond_future:
            expect(cls == "QuantExt::BondFuturesIndex", "class");
            expect(at(ore, "futureContract") == index.subject(), "contract");
            break;
        case index_family::commodity:
            expect(at(ore, "underlyingName") == index.subject(), "name");
            expect(at(ore, "isFuturesIndex") == (expiry ? "true" : "false"), "futures");
            expect(at(ore, "expiryDate") == (expiry ? as_date(*expiry) : "null"), "expiry");
            break;
        case index_family::power:
            expect(cls == "QuantExt::IntradayPowerIndex", "class");
            expect(at(ore, "deliveryDate") == *index.get("delivery"), "delivery");
            break;
        case index_family::fx:
            expect(cls == "QuantExt::FxIndex", "class");
            expect(at(ore, "familyName") == *index.get("source"), "source");
            expect(at(ore, "sourceCurrency") == index.subject(), "unit");
            expect(at(ore, "targetCurrency") == *index.get("ccy"), "ccy");
            break;
        case index_family::generic:
            expect(cls == "QuantExt::GenericIndex", "class");
            break;
        case index_family::ibor:
            expect(cls != "QuantLib::SwapIndex" && ore.contains("tenor"), "class");
            expect(at(ore, "currency") == index.subject(), "ccy");
            if (tenor)
                expect(same_period(at(ore, "tenor"), *tenor), "tenor");
            break;
        case index_family::swap:
            expect(cls == "QuantLib::SwapIndex", "class");
            expect(at(ore, "currency") == index.subject(), "ccy");
            expect(same_period(at(ore, "tenor"), *tenor), "tenor");
            break;
        case index_family::cmb:
            expect(false, "a constant maturity bond index ORE built");
            break;
        case index_family::inflation:
            expect(ore.contains("region"), "class");
            break;
    }
    return out;
}

struct tally {
    std::size_t lines = 0;
    std::size_t read = 0;
    std::vector<std::string> failures;

    void fail(const std::string& name, const std::string& why) {
        if (failures.size() < 40)
            failures.push_back(name + " -- " + why);
    }
};

tally check(const std::vector<catalogue_line>& lines) {
    tally t;
    for (const auto& line : lines) {
        ++t.lines;
        const auto& name = line.at("key");
        const auto index = ore_index_codec::read(name);
        if (const auto it = refused_on_purpose.find(name); it != refused_on_purpose.end()) {
            if (index)
                t.fail(name, "the codec should refuse it: " + it->second);
            continue;
        }
        if (const auto it = accepted_by_structure.find(name); it != accepted_by_structure.end()) {
            if (!index)
                t.fail(name, "the codec should read it: " + it->second);
            continue;
        }
        if (!accepted_by_ore(line)) {
            if (index)
                t.fail(name, "ORE refuses it (" + at(line, "error") + ") and the codec reads it");
            continue;
        }
        if (!index) {
            t.fail(name, "ORE reads it and the codec refuses it: " + index.error());
            continue;
        }
        ++t.read;
        const auto spelling =
            canonical_spellings.contains(name) ? canonical_spellings.at(name) : name;
        if (const auto written = ore_index_codec::write(*index); written != spelling)
            t.fail(name, "writes back as " + written);
        for (const auto& d : differences(*index, line))
            t.fail(name, d);
    }
    return t;
}

void report(const tally& t) {
    for (const auto& f : t.failures)
        UNSCOPED_INFO(f);
    CHECK(t.failures.empty());
}

std::vector<market_index> corpus_indices() {
    std::vector<market_index> out;
    for (const auto* lines : {&catalogue_index_forms(), &catalogue_index_corpus()}) {
        for (const auto& line : *lines) {
            if (auto index = ore_index_codec::read(line.at("key")))
                out.push_back(std::move(*index));
        }
    }
    return out;
}

}

TEST_CASE("the_index_codec_reads_every_documented_index_form_as_ore_does", tags) {
    const auto t = check(catalogue_index_forms());
    CHECK(t.lines == catalogue_index_forms().size());
    CHECK(t.read > 40);
    report(t);
}

TEST_CASE("the_index_codec_reads_every_corpus_index_name_as_ore_does", tags) {
    const auto t = check(catalogue_index_corpus());
    CHECK(t.read == catalogue_index_corpus().size());
    report(t);
}

TEST_CASE("every_index_round_trips_through_its_fixing_uri", tags) {
    std::vector<std::string> failures;
    std::set<std::string> uris;
    std::set<std::string> names;
    for (const auto& index : corpus_indices()) {
        const auto uri = oresmd_uri_codec::write_index(index);
        if (!uri) {
            failures.push_back(ore_index_codec::write(index) + ": " + uri.error());
            continue;
        }
        const auto back = oresmd_uri_codec::read_index(*uri);
        if (!back || *back != index)
            failures.push_back(*uri +
                               (back ? " reads back as another index" : ": " + back.error()));
        uris.insert(*uri);
        names.insert(ore_index_codec::write(index));
    }
    for (const auto& f : failures)
        UNSCOPED_INFO(f);
    CHECK(failures.empty());
    CHECK(uris.size() == names.size());
}

TEST_CASE("a_fixing_uri_has_the_agreed_shape", tags) {
    const std::map<std::string, std::string> expected{
        {"EUR-EURIBOR-6M", "oresmd://ir/EUR?type=fixing&index=ibor&name=EURIBOR&tenor=6M"},
        {"USD-SOFR", "oresmd://ir/USD?type=fixing&index=ibor&name=SOFR"},
        {"EUR-CMS-10Y", "oresmd://ir/EUR?type=fixing&index=swap&tenor=10Y"},
        {"UKRPI", "oresmd://inflation/UKRPI?type=fixing&index=inflation"},
        {"FX-ECB-EUR-USD", "oresmd://fx/EUR?type=fixing&index=fx&source=ECB&ccy=USD"},
        {"EQ-RIC:.SPX", "oresmd://equity/RIC:.SPX?type=fixing&index=equity"},
        {"COMM-NYMEX:CL-2025-01",
         "oresmd://commodity/NYMEX:CL?type=fixing&index=commodity&expiry=2025-01"},
        {"POWER-ICE:PDQ-2021-01-01-3600-7200",
         "oresmd://commodity/ICE:PDQ?type=fixing&index=power&delivery=2021-01-01&start=3600"
         "&end=7200"},
        {"GENERIC-JuniorNote", "oresmd://generic/JuniorNote?type=fixing&index=generic"},
    };
    for (const auto& [name, uri] : expected) {
        INFO("name: " << name);
        const auto index = ore_index_codec::read(name);
        REQUIRE(index);
        CHECK(oresmd_uri_codec::write_index(*index) == uri);
    }
}

TEST_CASE("the_fixing_uri_reader_refuses_a_uri_that_breaks_the_contract", tags) {
    for (const auto* uri :
         {"oresmd://ir/EUR?type=fixing&index=ibor&tenor=6M",
          "oresmd://ir/EUR?type=fixing&index=ibor&name=EURIBOR&tenor=6M&colour=blue",
          "oresmd://fx/EUR?type=fixing&index=ibor&name=EURIBOR&tenor=6M",
          "oresmd://ir/EUR?type=quote&index=ibor&name=EURIBOR&tenor=6M",
          "oresmd://ir/EUR?type=fixing&index=nosuch&name=EURIBOR",
          "oresmd://ir/EUR?type=fixing&index=ibor&tenor=6M&name=EURIBOR",
          "oresmd://commodity/ICE:PDQ?type=fixing&index=power&start=3600&end=7200"}) {
        INFO("uri: " << uri);
        CHECK_FALSE(oresmd_uri_codec::read_index(uri));
    }
}

TEST_CASE("every_committed_index_map_row_round_trips_through_the_codecs", tags) {
    // tools/ore_conventions/oresmd_index_map.tsv is generated once from the
    // codecs and committed, so the seed generator needs no C++ binding. That
    // makes it data that can rot under a codec change: this is the guard. Every
    // committed row must be a name the index codec reads and the URI it writes
    // back from it, or the fixed table has drifted from the codec.
    const auto path = ores::testing::project_root::resolve(
        "tools/ore_conventions/oresmd_index_map.tsv");
    const auto content = ores::platform::filesystem::file::read_content(path);
    std::istringstream stream(content);
    std::string line;
    std::string previous;
    std::size_t checked = 0;
    while (std::getline(stream, line)) {
        if (!line.empty() && line.back() == '\r')
            line.pop_back();
        if (line.empty())
            continue;
        const auto tab = line.find('\t');
        REQUIRE(tab != std::string::npos);
        const auto name = line.substr(0, tab);
        const auto uri = line.substr(tab + 1);
        INFO("name: " << name << ", uri: " << uri);
        // Sorted, one row per name: a hand edit that reordered or duplicated a
        // row would still round-trip, so the file's shape is checked too.
        CHECK(name > previous);
        previous = name;
        const auto index = ore_index_codec::read(name);
        REQUIRE(index);
        const auto written = oresmd_uri_codec::write_index(*index);
        REQUIRE(written);
        CHECK(*written == uri);
        ++checked;
    }
    CHECK(checked > 0);
}
