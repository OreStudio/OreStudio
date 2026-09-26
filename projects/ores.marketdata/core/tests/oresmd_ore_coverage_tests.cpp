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
#include "ores.marketdata.core/oresmd/oresmd_projections.hpp"
#include "ores.platform/filesystem/file.hpp"
#include "ores.testing/project_root.hpp"
#include <algorithm>
#include <catch2/catch_test_macros.hpp>
#include <cstddef>
#include <filesystem>
#include <format>
#include <map>
#include <optional>
#include <set>
#include <sstream>
#include <string>
#include <string_view>
#include <vector>

/**
 * @file oresmd_ore_coverage_tests.cpp
 * @brief Measures, against the whole ORE example corpus, how much of it oresmd
 * can name -- and fails when a series type that could be named stops being.
 *
 * The coverage record in
 * =projects/ores.codegen/scripts/check_oresmd_ore_coverage.py= compares the
 * registry's series types against the quote types the models declare. That is a
 * comparison of two lists, and it cannot see whether a declared type actually
 * reads the keys ORE writes for it: a quote type can be modelled in a shape the
 * corpus never uses, and the record would call it covered.
 *
 * This test is the other half. It walks the corpus, asks the real projection
 * library to name every key, and round-trips the ones it can. The full table is
 * reported on every run, so the current figure is visible rather than asserted
 * in prose, and the completely unnameable types are pinned against a list so
 * that closing one fails until the list is updated.
 */

namespace {

const std::string tags("[marketdata][oresmd][corpus]");

using ores::marketdata::core::oresmd_projections;

/// Per series type: the corpus keys seen, and how many oresmd could name.
struct type_coverage {
    std::size_t keys = 0;
    std::size_t named = 0;
    std::size_t round_tripped = 0;
    /// One key that projected and did not read back, with what it became. A
    /// count says a gap exists; the example says what it is.
    std::string mismatch;
};

/// A corpus file that carries market data. The corpus names its own files, and
/// the name is the only thing that says which reader a file feeds: the fixings
/// reader and this one take different formats.
bool is_market_payload(const std::string& path) {
    const auto name = std::filesystem::path(path).filename().string();
    if (name.find("fixing") != std::string::npos)
        return false;
    if (name.find("market") == std::string::npos)
        return false;
    if (!name.ends_with(".txt") && !name.ends_with(".csv"))
        return false;
    // ORE's own output files carry "market" in their names too --
    // todaysmarketcalibration.csv is a calibration dump -- and reading one as
    // market data invents a pseudo-type per curve name it happens to list.
    if (path.find("ExpectedOutput") != std::string::npos)
        return false;
    // The dated MD_*.csv dumps are the same thing under another name.
    return name.rfind("MD_", 0) != 0;
}

/// The key on a corpus line, or nullopt for a comment, a blank, or a line in
/// neither of the corpus's two formats. ORE text separates with whitespace,
/// the derived CSVs with commas.
std::optional<std::string> key_of(std::string_view line) {
    const auto first = line.find_first_not_of(" \t\r");
    if (first == std::string_view::npos || line[first] == '#')
        return std::nullopt;
    line.remove_prefix(first);

    const auto comma = line.find(',');
    if (comma != std::string_view::npos) {
        const auto second = line.find(',', comma + 1);
        if (second == std::string_view::npos)
            return std::nullopt;
        return std::string(line.substr(comma + 1, second - comma - 1));
    }

    const auto first_space = line.find_first_of(" \t");
    if (first_space == std::string_view::npos)
        return std::nullopt;
    auto rest = line.substr(first_space);
    const auto start = rest.find_first_not_of(" \t");
    if (start == std::string_view::npos)
        return std::nullopt;
    rest.remove_prefix(start);
    const auto end = rest.find_first_of(" \t");
    return std::string(rest.substr(0, end));
}

/// Walks the corpus once, naming every key it carries.
std::map<std::string, type_coverage> survey() {
    std::map<std::string, type_coverage> coverage;
    const auto root = ores::testing::project_root::resolve("external/ore/examples");

    for (const auto& entry : std::filesystem::recursive_directory_iterator(root)) {
        if (!entry.is_regular_file())
            continue;
        const auto path = entry.path().string();
        if (!is_market_payload(path))
            continue;

        const auto content = ores::platform::filesystem::file::read_content(entry.path());
        std::istringstream stream(content);
        std::string line;
        while (std::getline(stream, line)) {
            const auto key = key_of(line);
            if (!key)
                continue;
            const auto type = key->substr(0, key->find('/'));
            auto& c = coverage[type];
            ++c.keys;

            const auto id = oresmd_projections::from_ore_key(*key);
            if (!id)
                continue;
            ++c.named;
            const auto back = oresmd_projections::to_quote_key(*id);
            if (back && *back == *key) {
                ++c.round_tripped;
            } else if (c.mismatch.empty()) {
                c.mismatch = *key + " -> " + (back ? *back : std::string("<none>"));
            }
        }
    }
    return coverage;
}

const std::map<std::string, type_coverage>& corpus_coverage() {
    static const auto coverage = survey();
    return coverage;
}

std::set<std::string> types_oresmd_cannot_name() {
    std::set<std::string> result;
    for (const auto& [type, c] : corpus_coverage()) {
        if (c.named == 0)
            result.insert(type);
    }
    return result;
}

/// Types the corpus carries that oresmd names every key of. A type that stops
/// being fully named is a regression whatever the coverage record says.
std::set<std::string> fully_named_types() {
    std::set<std::string> result;
    for (const auto& [type, c] : corpus_coverage()) {
        if (c.named == c.keys && c.keys > 0)
            result.insert(type);
    }
    return result;
}

/// Types oresmd names keys of and then fails to read back. This is a different
/// defect from an unnameable type, and a worse one: the projection reports
/// success, so nothing downstream sees the loss.
std::set<std::string> types_with_round_trip_gaps() {
    std::set<std::string> result;
    for (const auto& [type, c] : corpus_coverage()) {
        if (c.named > c.round_tripped)
            result.insert(type);
    }
    return result;
}

}

TEST_CASE("no_series_type_oresmd_cannot_name_has_gone_unrecorded", tags) {
    const auto& coverage = corpus_coverage();
    REQUIRE_FALSE(coverage.empty());

    std::size_t keys = 0;
    std::size_t named = 0;
    std::size_t round_tripped = 0;
    for (const auto& [type, c] : coverage) {
        keys += c.keys;
        named += c.named;
        round_tripped += c.round_tripped;
        WARN(std::format("{:<26} {:>7} keys, {:>7} named, {:>7} round-tripped",
                         type, c.keys, c.named, c.round_tripped));
        // On its own line: the log wraps long messages, and a truncated key is
        // worse than none.
        if (!c.mismatch.empty())
            WARN(std::format("  {} first loss: {}", type, c.mismatch));
    }
    WARN(std::format("{:<26} {:>7} keys, {:>7} named, {:>7} round-tripped",
                     "TOTAL", keys, named, round_tripped));

    // The series types the corpus carries that oresmd cannot name at all. Each
    // is recorded with its reason in check_oresmd_ore_coverage.py; this list is
    // the measured half of the same claim. It fails when one is closed without
    // the record being updated, and when a change breaks a type that worked.
    //
    // CPR appears here even though the model declares a quote type for it: it is
    // modelled in a shape the corpus does not use, so against real data it names
    // nothing. That is the shape-mismatch list in the codegen record, seen from
    // the data rather than from the models.
    const std::set<std::string> recorded{
        "BOND", "COMMODITY_OPTION", "CPR", "INDEX_CDS_OPTION",
        "RATING", "SHAPE_PROFILE"};

    REQUIRE(types_oresmd_cannot_name() == recorded);
}

TEST_CASE("only_the_recorded_types_lose_keys_on_the_way_back", tags) {
    // A key that projects to a URI and does not read back as itself is the
    // silent kind of loss: the projection reports success and the caller has no
    // reason to look. There are none left -- the list held eight types when it
    // was written and they were fixed one at a time -- so this asserts that no
    // type loses a key, and fails the moment one starts to.
    const std::set<std::string> recorded{};

    REQUIRE(types_with_round_trip_gaps() == recorded);
}

TEST_CASE("the_families_this_work_brought_in_name_and_round_trip_every_key", tags) {
    // Pinned at every corpus key rather than at a sample, so a shape that
    // regresses in one variant of a family fails here. The two lists differ
    // because four types name every key and still lose some on the way back;
    // those are in the round-trip gap list above, not here.
    const std::set<std::string> expected_fully_named{
        "BMA_SWAP", "BOND_OPTION", "CAPFLOOR", "CC_BASIS_SWAP",
        "CC_FIX_FLOAT_SWAP", "CDS_INDEX", "COMMODITY", "COMMODITY_FWD",
        "CORRELATION", "EQUITY", "EQUITY_DIVIDEND", "EQUITY_FWD",
        "EQUITY_OPTION", "FRA",
        "FX", "FXFWD", "FX_OPTION", "HAZARD_RATE", "IMM_FRA",
        "INDEX_CDS_TRANCHE", "MM_FUTURE", "OI_FUTURE", "SEASONALITY",
        "YY_INFLATIONCAPFLOOR", "YY_INFLATIONSWAP", "ZC_INFLATIONCAPFLOOR",
        "ZC_INFLATIONSWAP", "ZERO"};

    const std::set<std::string> expected_fully_round_tripped{
        "BMA_SWAP", "BOND_OPTION", "CAPFLOOR", "CC_BASIS_SWAP",
        "CC_FIX_FLOAT_SWAP", "CDS_INDEX", "COMMODITY", "EQUITY_DIVIDEND",
        "FRA", "FX", "FXFWD", "FX_OPTION", "HAZARD_RATE", "IMM_FRA",
        "INDEX_CDS_TRANCHE", "IR_SWAP", "MM_FUTURE", "OI_FUTURE", "SEASONALITY",
        "YY_INFLATIONCAPFLOOR", "YY_INFLATIONSWAP", "ZC_INFLATIONCAPFLOOR",
        "ZC_INFLATIONSWAP", "ZERO"};

    REQUIRE(fully_named_types() == expected_fully_named);
    for (const auto& type : expected_fully_round_tripped) {
        const auto& c = corpus_coverage().at(type);
        CHECK(c.named == c.round_tripped);
    }
}
