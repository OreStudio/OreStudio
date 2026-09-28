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
 * The corpus carries 158 distinct names in a fixing column. This test walks them
 * with the real fixing reader, so a name it counts is a name the import sees, and
 * asks the real projection library to name each one. Every class the corpus puts
 * in the fixing space resolves, so the list of classes that name nothing is empty
 * and stays asserted: a class that stops resolving fails here. The two names that
 * are market-data keys rather than index names are counted apart and pinned by
 * name, so the fixing space and the keys inside it still add up to the whole
 * corpus. The full table is reported on every run, so the current figure is
 * visible rather than asserted in prose.
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

/// The bucket for a name the corpus puts in a fixing column that is not an index
/// name at all.
constexpr std::string_view market_data_key{"market-data-key"};

/// The class an index name belongs to, from the name alone. The corpus does not
/// label its names, so the shape is the only thing that says which asset class a
/// family belongs to -- and the shape is what this library discriminates on, so
/// the classification is stated in the same terms.
std::string class_of(std::string_view name) {
    // Two prefixes, not one. GENERIC-<name> is an index name ORE resolves, while
    // GENERIC-MD/<TYPE>/<METRIC>/... is the market-data key form an oresmd URI
    // replaces. The corpus puts two of the latter in a fixing payload, and they
    // are errors rather than fixings.
    const std::pair<std::string_view, std::string_view> prefixed[] = {{"FX-", "fx"},
                                                                      {"EQ-", "equity"},
                                                                      {"COMM-", "commodity"},
                                                                      {"POWER-", "power"},
                                                                      {"BOND-ISIN:", "security"},
                                                                      {"GENERIC-MD/", market_data_key},
                                                                      {"GENERIC-", "generic"}};
    for (const auto& [prefix, cls] : prefixed) {
        if (name.starts_with(prefix))
            return std::string(cls);
    }

    // CCY-FAMILY[-TENOR]: three letters, then a dash. The inflation codes the
    // corpus writes (UKRPI, ZACPI) carry no dash at all, so this is what
    // separates the two classes that arrive without a prefix.
    const auto is_currency_segment = name.size() > 4 && name[3] == '-' &&
                                     std::ranges::all_of(name.substr(0, 3), [](unsigned char c) {
                                         return std::isalpha(c) != 0;
                                     });
    return is_currency_segment ? "ir" : "inflation";
}

struct corpus_survey {
    std::map<std::string, class_coverage> classes;
    /// The names the corpus puts in a fixing column that are not index names at
    /// all. They are counted apart from the classes rather than dropped, and a
    /// case pins them and their refusal by name, so the population stays whole
    /// while the classes carry an empty allowlist.
    std::vector<std::string> not_index_names;
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
            result.refused.push_back({std::filesystem::relative(path, root).string(), ex.what()});
            continue;
        }

        for (const auto& row : rows) {
            if (!seen.insert(row.index_name).second)
                continue;

            if (class_of(row.index_name) == market_data_key) {
                result.not_index_names.push_back(row.index_name);
                continue;
            }

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
                c.mismatch = row.index_name + " -> " + (back ? *back : std::string("<none>"));
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

}

TEST_CASE("every_fixing_payload_in_the_corpus_is_readable_as_fixings", tags) {
    // The census is only as complete as the reader. A payload the reader refuses
    // contributes no names, and the classes below would report a clean sheet over
    // a file nobody looked at. Empty is the goal: a payload the corpus names as
    // fixings has to be one the fixing reader reads.
    for (const auto& file : corpus_coverage().refused)
        WARN(std::format("{}: {}", file.path, file.reason));

    REQUIRE(corpus_coverage().refused.empty());
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
        WARN(std::format("{:<14} {:>5} names, {:>5} named, {:>5} round-tripped",
                         cls,
                         c.names,
                         c.named,
                         c.round_tripped));
        // On its own line: the log wraps long messages, and a truncated name is
        // worse than none.
        if (!c.unprojected.empty())
            WARN(std::format("  {} first unnamed: {}", cls, c.unprojected));
        if (!c.mismatch.empty())
            WARN(std::format("  {} first loss: {}", cls, c.mismatch));
    }
    WARN(std::format("{:<14} {:>5} names, {:>5} named, {:>5} round-tripped",
                     "TOTAL",
                     names,
                     named,
                     round_tripped));
    for (const auto& key : corpus_coverage().not_index_names)
        WARN(std::format("  not an index name: {}", key));

    // The classes whose names reach no identifier, and the list is empty: every
    // class the corpus puts in the fixing space resolves. A class that stops
    // resolving fails here, and so does one whose names are not this corpus's.
    const std::set<std::string> recorded{};

    REQUIRE(classes_with_unnamed_names() == recorded);
}

TEST_CASE("the_market_data_keys_the_corpus_misfiles_as_fixings_stay_refused", tags) {
    // Two rows in Products/Input/fixings.csv carry market-data keys where the
    // header promises a fixing id: they are SPX call prices, 2862.29 at the 3300
    // strike and 2767.41 at 3400, and no class owes them an index name. They are
    // pinned by name rather than dropped from the census, so the fixing space and
    // the keys inside it still add up to every distinct name the corpus carries.
    const std::vector<std::string> recorded{
        "GENERIC-MD/EQUITY_OPTION/PRICE/RIC:.SPX/USD/2025-10-03/3300/C",
        "GENERIC-MD/EQUITY_OPTION/PRICE/RIC:.SPX/USD/2025-10-03/3400/C"};

    auto keys = corpus_coverage().not_index_names;
    std::ranges::sort(keys);
    REQUIRE(keys == recorded);
    for (const auto& key : keys)
        REQUIRE_FALSE(oresmd_projections::from_index_name(key).has_value());
}

TEST_CASE("no_fixing_index_class_loses_names_on_the_way_back", tags) {
    // A name that projects to an identifier and does not read back as itself is
    // the silent kind of loss: the projection reports success and the caller has
    // no reason to look. An alias is where this bites -- the family resolves, and
    // the projection has to prove it also emits the spelling it was given.
    const std::set<std::string> recorded{};

    REQUIRE(classes_with_round_trip_gaps() == recorded);
}

TEST_CASE("the_closed_classes_name_every_fixing_name_the_corpus_carries", tags) {
    // The classes whose grammar is decided, pinned at every name the corpus
    // carries rather than at a sample, so a family or a source that regresses in
    // one variant fails here.
    for (const auto& cls :
         {"ir", "fx", "commodity", "power", "generic", "inflation", "equity", "security"}) {
        const auto& c = corpus_coverage().classes.at(cls);

        CHECK(c.named == c.names);
        CHECK(c.round_tripped == c.names);
    }
}
