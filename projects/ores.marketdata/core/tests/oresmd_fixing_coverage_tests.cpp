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
#include "corpus_files.hpp"
#include "ores.marketdata.core/oresmd/oresmd_projections.hpp"
#include "ores.ore.core/market/market_data_parser.hpp"
#include "ores.platform/filesystem/file.hpp"
#include "ores.testing/project_root.hpp"
#include <algorithm>
#include <catch2/catch_test_macros.hpp>
#include <cctype>
#include <cstddef>
#include <filesystem>
#include <format>
#include <map>
#include <set>
#include <sstream>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

/**
 * @file oresmd_fixing_coverage_tests.cpp
 * @brief Measures, over the fixing half of the ORE example corpus, how much of
 * its index-name key space reaches an oresmd identifier -- and fails when a
 * class that reached one stops reaching it.
 *
 * A fixing row's second field is an index name, not an instrument key: the whole
 * name is the series identifier, and it is the only key the fixing boundary has.
 * The sibling coverage test measures the instrument keys, and it steps around
 * these files because they are the other reader's payload.
 *
 * The corpus carries 158 distinct index names across eight shapes. This test
 * walks them with the real fixing reader, so a name it counts is a name the
 * import sees, and asks the real projection library to name each one -- one way
 * for the interest-rate names, which are the ones index_family declares, and
 * nothing for the other classes, whose grammar is a decision still open. The full
 * table is reported on every run, so the current figure is visible rather than
 * asserted in prose, and the classes that name nothing are pinned against a list
 * so that closing one fails until the list is updated.
 */

namespace {

const std::string tags("[marketdata][oresmd][corpus][fixing]");

using ores::marketdata::core::oresmd_projections;
using ores::marketdata::test::fixing_payloads;

/// Per index-name class: the names seen, and how many oresmd could name.
struct class_coverage {
    std::size_t names = 0;
    std::size_t named = 0;
    std::size_t round_tripped = 0;
    /// One name that did not project at all. The shape of the failing name is
    /// the difference between a class this library was never asked to name and a
    /// family missing from one it was, and a count cannot tell them apart.
    std::string unprojected;
    /// One name that projected and did not read back, with what it became. A
    /// count says a gap exists; the example says what it is.
    std::string mismatch;
};

/// A fixing payload the reader refused, with why. A file that cannot be read is
/// worse than one whose names go unnamed: its rows never reach the census, so
/// the measurement shrinks and no count says so.
struct refused_file {
    std::string path;
    std::string reason;
};

/// The class an index name belongs to, from the name alone. The corpus does not
/// label its names, so the shape is the only thing that says which asset class a
/// family belongs to -- and the shape is what this library discriminates on, so
/// the classification is stated in the same terms.
std::string class_of(std::string_view name) {
    const std::pair<std::string_view, std::string_view> prefixed[] = {
        {"FX-", "fx"},
        {"EQ-", "equity"},
        {"COMM-", "commodity"},
        {"POWER-", "power"},
        {"BOND-ISIN:", "security"},
        {"GENERIC-", "unclassified"}};
    for (const auto& [prefix, cls] : prefixed) {
        if (name.starts_with(prefix))
            return std::string(cls);
    }

    // CCY-FAMILY[-TENOR]: three letters, then a dash. The inflation codes the
    // corpus writes (UKRPI, ZACPI) carry no dash at all, so this is what
    // separates the two classes that arrive without a prefix.
    const auto is_currency_segment =
        name.size() > 4 && name[3] == '-' &&
        std::ranges::all_of(name.substr(0, 3),
                            [](unsigned char c) { return std::isalpha(c) != 0; });
    return is_currency_segment ? "ir" : "inflation";
}

struct corpus_survey {
    std::map<std::string, class_coverage> classes;
    std::vector<refused_file> refused;
};

/// Walks the fixing corpus once, naming every distinct index name it carries.
corpus_survey survey() {
    corpus_survey result;
    std::set<std::string> seen;
    const auto root = ores::testing::project_root::resolve("external/ore/examples");

    for (const auto& path : fixing_payloads(root)) {
        std::istringstream in(ores::platform::filesystem::file::read_content(path));
        std::vector<ores::ore::market::fixing> rows;
        try {
            rows = ores::ore::market::parse_fixings(in);
        } catch (const std::invalid_argument& ex) {
            // Relative to the corpus root: a recorded refusal has to read the same
            // on every machine that checks out the corpus.
            result.refused.push_back(
                {std::filesystem::relative(path, root).string(), ex.what()});
            continue;
        }

        for (const auto& row : rows) {
            if (!seen.insert(row.index_name).second)
                continue;

            auto& c = result.classes[class_of(row.index_name)];
            ++c.names;

            const auto id = oresmd_projections::from_index_name(row.index_name);
            if (!id) {
                if (c.unprojected.empty())
                    c.unprojected = row.index_name;
                continue;
            }
            ++c.named;

            const auto back = oresmd_projections::to_index_name(*id);
            if (back && *back == row.index_name) {
                ++c.round_tripped;
            } else if (c.mismatch.empty()) {
                c.mismatch =
                    row.index_name + " -> " + (back ? *back : std::string("<none>"));
            }
        }
    }
    return result;
}

const corpus_survey& corpus_coverage() {
    static const auto coverage = survey();
    return coverage;
}

/// Classes whose names reach no identifier at all.
std::set<std::string> classes_with_unnamed_names() {
    std::set<std::string> result;
    for (const auto& [cls, c] : corpus_coverage().classes) {
        if (c.named < c.names)
            result.insert(cls);
    }
    return result;
}

/// Classes the library names names of and then fails to read back. A different
/// defect from an unnameable class, and a worse one: the projection reports
/// success, so nothing downstream sees the loss.
std::set<std::string> classes_with_round_trip_gaps() {
    std::set<std::string> result;
    for (const auto& [cls, c] : corpus_coverage().classes) {
        if (c.round_tripped < c.named)
            result.insert(cls);
    }
    return result;
}

/// The fixing payloads the reader refused, as corpus-relative paths.
std::set<std::string> refused_payloads() {
    std::set<std::string> result;
    for (const auto& file : corpus_coverage().refused)
        result.insert(file.path);
    return result;
}

}

TEST_CASE("no_fixing_payload_the_reader_refuses_has_gone_unrecorded", tags) {
    // The census is only as complete as the reader. A payload the reader refuses
    // contributes no names, and the classes below would report a clean sheet over
    // a file nobody looked at. The one entry is a fixing file ORE reads and this
    // library does not: CurveBuilding writes it with semicolons, where the reader
    // separates on commas and whitespace.
    for (const auto& file : corpus_coverage().refused)
        WARN(std::format("{}: {}", file.path, file.reason));

    const std::set<std::string> recorded{"CurveBuilding/Input/fixings_bondyieldshifted.csv"};

    REQUIRE(refused_payloads() == recorded);
}

TEST_CASE("no_fixing_index_class_has_gone_unrecorded", tags) {
    const auto& coverage = corpus_coverage().classes;
    REQUIRE_FALSE(coverage.empty());

    std::size_t names = 0;
    std::size_t named = 0;
    std::size_t round_tripped = 0;
    for (const auto& [cls, c] : coverage) {
        names += c.names;
        named += c.named;
        round_tripped += c.round_tripped;
        WARN(std::format(
            "{:<14} {:>5} names, {:>5} named, {:>5} round-tripped", cls, c.names, c.named,
            c.round_tripped));
        // On its own line: the log wraps long messages, and a truncated name is
        // worse than none.
        if (!c.unprojected.empty())
            WARN(std::format("  {} first unnamed: {}", cls, c.unprojected));
        if (!c.mismatch.empty())
            WARN(std::format("  {} first loss: {}", cls, c.mismatch));
    }
    WARN(std::format("{:<14} {:>5} names, {:>5} named, {:>5} round-tripped", "TOTAL", names,
                     named, round_tripped));

    // The classes whose index-name grammar is a decision this task has not taken
    // yet. Each is recorded with its reason on the task; this list is the
    // measured half of the same claim. It fails when one is closed without the
    // record being updated, and when a change breaks a class that worked.
    //
    // Interest rates are absent because they are the class this work closes, and
    // the assertion below fails if they come back.
    const std::set<std::string> recorded{"commodity", "equity",     "fx",
                                         "inflation", "power",      "security",
                                         "unclassified"};

    REQUIRE(classes_with_unnamed_names() == recorded);
}

TEST_CASE("no_fixing_index_class_loses_names_on_the_way_back", tags) {
    // A name that projects to an identifier and does not read back as itself is
    // the silent kind of loss: the projection reports success and the caller has
    // no reason to look. An alias is where this bites -- the family resolves, and
    // the projection has to prove it also emits the spelling it was given.
    const std::set<std::string> recorded{};

    REQUIRE(classes_with_round_trip_gaps() == recorded);
}

TEST_CASE("the_interest_rate_fixing_names_all_reach_an_identifier", tags) {
    // The class this work closes, pinned at every name the corpus carries rather
    // than at a sample, so a family that regresses in one variant fails here.
    const auto& ir = corpus_coverage().classes.at("ir");

    CHECK(ir.named == ir.names);
    CHECK(ir.round_tripped == ir.names);
}
