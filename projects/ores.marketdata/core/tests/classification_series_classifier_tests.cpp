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
#include "ores.marketdata.core/classification/series_classifier.hpp"
#include "ores.ore.core/market/market_data_parser.hpp"
#include "ores.platform/filesystem/file.hpp"
#include "ores.testing/project_root.hpp"
#include "ores.testing/series_key_shape_seed.hpp"
#include <catch2/catch_test_macros.hpp>
#include <filesystem>
#include <map>
#include <set>
#include <sstream>
#include <stdexcept>
#include <string>
#include <vector>

/**
 * @file classification_series_classifier_tests.cpp
 * @brief Walks the whole ORE example corpus and asserts that every market data
 * series it carries classifies, then sweeps the classifier's own vocabulary.
 *
 * The corpus is walked rather than listed: the sibling roundtrip test names 52
 * files by hand and covers under two thirds of what is there. A corpus whose
 * walk silently finds nothing must fail rather than pass, so the walk asserts a
 * floor on what it found.
 *
 * The walk cannot reach every type. A fixing is not a market datum, so FIXING
 * is covered by the vocabulary sweep and by the correlation cases, which assert
 * the rule directly rather than through the corpus.
 *
 * Classification only. The ORE-to-oresmd round trip is a separate concern.
 */

namespace {

using ores::marketdata::core::series_classification;
using ores::marketdata::core::series_classifier;

const std::string tags("[marketdata][classification][corpus]");

/**
 * Anti-vacuity floors, set below the census measured over the corpus when this
 * test was written: 107,951 distinct keys, 42 series types, 95 files carrying
 * market data. A walk that silently stops finding files must fail rather than
 * pass; a walk that finds a little less than the last run should not. Raise
 * these only to the values a run actually measured.
 */
constexpr std::size_t min_distinct_keys = 90000;
constexpr std::size_t min_distinct_types = 40;
constexpr std::size_t min_parsed_files = 85;

/// One distinct market data key, split as the parser split it.
struct corpus_entry {
    std::string key;
    std::string series_type;
    std::string metric;
    std::string qualifier;
};

struct corpus_survey {
    /// Keyed by the verbatim key, so a key repeated across files collapses.
    std::map<std::string, corpus_entry> entries;
    std::set<std::string> parsed_files;
    /// Files the walk reached, could not read, and the first such error.
    /// Without these a walk that reads nothing reports the same empty census
    /// as a corpus that carries nothing.
    std::size_t files_seen = 0;
    std::size_t unreadable_files = 0;
    std::string first_read_error;
};

/// One walk of the corpus, shared by every case in this file.
const corpus_survey& survey() {
    static const auto result = [] {
        corpus_survey s;
        const auto root = ores::testing::project_root::resolve("external/ore/examples");

        for (const auto& dir_entry : std::filesystem::recursive_directory_iterator(root)) {
            if (!dir_entry.is_regular_file())
                continue;
            ++s.files_seen;

            const auto path = dir_entry.path();
            std::string content;
            try {
                content = ores::platform::filesystem::file::read_content(path);
            } catch (const std::exception& ex) {
                ++s.unreadable_files;
                if (s.first_read_error.empty())
                    s.first_read_error = path.string() + ": " + ex.what();
                continue;
            }

            // Outside the try: a seed that cannot be read is a broken test,
            // not a file that fails to parse, and inside the catch below it
            // would be swallowed once per corpus file.
            const auto& registry = ores::testing::seed_registry();
            std::istringstream in{content};
            try {
                for (const auto& d : ores::ore::market::parse_market_data(in, registry)) {
                    s.entries[d.key] = corpus_entry{d.key, d.series_type, d.metric, d.qualifier};
                    s.parsed_files.insert(path.string());
                }
            } catch (const std::invalid_argument&) {
                // Most of the corpus is XML trades, and a fixing file is not a
                // market data file. Neither parses, and neither is a finding.
            }
        }
        return s;
    }();
    return result;
}

std::set<std::string> distinct_types() {
    std::set<std::string> types;
    for (const auto& [key, entry] : survey().entries)
        types.insert(entry.series_type);
    return types;
}

/// Asserts a code the classifier emits is one the catalogue carries, since the
/// marketdata tables validate against it and a missing code fails the insert.
void check_code_is_in_catalogue(const std::string& code, const std::string& rel_path) {
    INFO("code: " << code);
    const auto path = ores::testing::project_root::resolve(rel_path);
    REQUIRE(std::filesystem::exists(path));
    const auto content = ores::platform::filesystem::file::read_content(path);
    CHECK(content.find("'" + code + "'") != std::string::npos);
}

}

TEST_CASE("every_distinct_series_key_in_the_ore_corpus_classifies", tags) {
    const auto& entries = survey().entries;

    // Reported on a passing run too, so the census is visible to whoever
    // tightens the floors above.
    std::ostringstream census;
    census << "corpus census: " << entries.size() << " distinct keys, " << distinct_types().size()
           << " series types, " << survey().parsed_files.size() << " parsed files of "
           << survey().files_seen << " seen, " << survey().unreadable_files << " unreadable";
    // Named only when the walk found nothing. A healthy walk parses the XML
    // majority into nothing by design, and those messages would drown the
    // census; an empty walk is the case where one of them is the answer.
    if (entries.empty() && !survey().first_read_error.empty())
        census << "; first read error: " << survey().first_read_error;
    WARN(census.str());
    REQUIRE(entries.size() >= min_distinct_keys);
    REQUIRE(survey().parsed_files.size() >= min_parsed_files);

    std::vector<std::string> unclassified;
    for (const auto& [key, entry] : entries) {
        if (!series_classifier::try_classify(entry.series_type, entry.metric, entry.qualifier))
            unclassified.push_back(key);
    }

    for (const auto& key : unclassified)
        INFO("unclassified key: " << key);
    CHECK(unclassified.empty());
}

TEST_CASE("every_series_type_the_ore_corpus_carries_is_known_to_the_classifier", tags) {
    const auto types = distinct_types();
    const auto known = series_classifier::known_series_types();
    const std::set<std::string> known_set(known.begin(), known.end());

    INFO("distinct types: " << types.size());
    REQUIRE(types.size() >= min_distinct_types);

    std::vector<std::string> unknown;
    for (const auto& type : types) {
        if (!known_set.count(type))
            unknown.push_back(type);
    }

    for (const auto& type : unknown)
        INFO("type the corpus carries but the vocabulary lacks: " << type);
    CHECK(unknown.empty());
}

TEST_CASE("every_classification_code_the_classifier_can_emit_exists_in_its_catalogue", tags) {
    std::set<std::string> asset_classes;
    std::set<std::string> subclasses;

    const auto record = [&](const series_classification& c) {
        asset_classes.insert(c.asset_classes.begin(), c.asset_classes.end());
        subclasses.insert(c.series_subclass);
    };

    // Swept over the vocabulary rather than over the corpus, because FIXING is
    // a fixing and not a market datum: the walk never reaches it, and an
    // index_fixing row missing from the catalogue would otherwise go unnoticed
    // until an import wrote one and the insert failed. The metric is ignored
    // for every type but the two handled below.
    for (const auto& type : series_classifier::known_series_types()) {
        if (type == "CORRELATION" || type == "GENERIC-MD")
            continue;
        record(series_classifier::classify(type, "PROBE", "PROBE"));
    }

    record(series_classifier::classify(
        "CORRELATION", "RATE", "EQ-RIC:.SPX/FX-GENERIC-USD-EUR/1Y/ATM"));
    record(series_classifier::classify(
        "GENERIC-MD", "EQUITY_OPTION", "PRICE/RIC:.SPX/USD/2025-10-03/3300/C"));

    REQUIRE(!asset_classes.empty());
    REQUIRE(!subclasses.empty());

    for (const auto& code : asset_classes)
        check_code_is_in_catalogue(code,
                                   "projects/ores.sql/populate/refdata/"
                                   "refdata_asset_class_codes_populate.sql");
    for (const auto& code : subclasses)
        check_code_is_in_catalogue(code,
                                   "projects/ores.sql/populate/refdata/"
                                   "refdata_series_subclass_codes_populate.sql");
}

TEST_CASE("a_correlation_takes_its_classes_from_the_two_operands_in_its_key", tags) {
    std::map<std::pair<std::string, std::string>, series_classification> by_operands;
    for (const auto& [key, entry] : survey().entries) {
        if (entry.series_type != "CORRELATION")
            continue;
        const auto c =
            series_classifier::try_classify(entry.series_type, entry.metric, entry.qualifier);
        REQUIRE(c);
        const auto slash = entry.qualifier.find('/');
        REQUIRE(slash != std::string::npos);
        const auto first = entry.qualifier.substr(0, slash);
        const auto rest = entry.qualifier.substr(slash + 1);
        const auto second = rest.substr(0, rest.find('/'));
        by_operands[{first, second}] = *c;
    }

    REQUIRE(!by_operands.empty());

    // Two classes where the operands name different ones.
    const auto cross = by_operands.find({"EQ-RIC:.SPX", "FX-GENERIC-USD-EUR"});
    REQUIRE(cross != by_operands.end());
    CHECK(cross->second.asset_classes.size() == 2);

    // One class where both operands name the same one.
    const auto same = by_operands.find({"EQ-RIC:.SPX", "EQ-RIC:.NDX"});
    REQUIRE(same != by_operands.end());
    REQUIRE(same->second.asset_classes.size() == 1);
    CHECK(same->second.asset_classes.front() == "equity");

    // A rates operand carries no prefix; its class comes from the CMS marker.
    const auto rates = by_operands.find({"EUR-CMS-10Y", "EUR-CMS-2Y"});
    REQUIRE(rates != by_operands.end());
    REQUIRE(rates->second.asset_classes.size() == 1);
    CHECK(rates->second.asset_classes.front() == "interest_rates");
}
