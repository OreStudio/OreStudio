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
#include "ores.marketdata.api/domain/series_classification_rule.hpp"
#include "ores.marketdata.core/classification/series_classifier.hpp"
#include "ores.ore.core/market/market_data_parser.hpp"
#include "ores.platform/filesystem/file.hpp"
#include "ores.testing/project_root.hpp"
#include "ores.testing/series_classification_rule_seed.hpp"
#include "ores.testing/series_key_shape_seed.hpp"
#include <algorithm>
#include <catch2/catch_test_macros.hpp>
#include <filesystem>
#include <iterator>
#include <map>
#include <set>
#include <sstream>
#include <stdexcept>
#include <string>
#include <tuple>
#include <utility>
#include <vector>

/**
 * @file classification_series_classifier_tests.cpp
 * @brief Walks the whole ORE example corpus and asserts that every series it
 * carries classifies, then sweeps the classifier's own vocabulary.
 *
 * The corpus is walked rather than listed: the sibling roundtrip test names 52
 * files by hand and covers under two thirds of what is there. A corpus whose
 * walk silently finds nothing must fail rather than pass, so the walk asserts a
 * floor on what it found.
 *
 * The census counts the union of the two readers, because both feed an import.
 * The file name decides which reader a file feeds, as it does for an import:
 * import_service takes market data and fixings as two separately named
 * payloads, and this corpus names its fixing payloads `fixings*`. A fixing is
 * not a market datum, and import_service composes its series as
 * FIXING/RATE/<index name>, which is the key this walk gives it too. A file
 * that is neither, which is most of the corpus, parses into nothing.
 *
 * Classification only. The ORE-to-oresmd round trip is a separate concern.
 */

namespace {

using ores::marketdata::core::series_classification;
using ores::marketdata::core::series_classifier;
using ores::marketdata::domain::series_classification_rule;

const std::string tags("[marketdata][classification][corpus]");
const std::string unit_tags("[marketdata][classification]");

/**
 * Anti-vacuity floors, set under the census a run measured over the corpus:
 * 108,105 distinct keys across 42 series types, from 107,947 market data keys
 * in 91 files and 158 fixing keys in 54 files, out of 3,528 walked, none
 * unreadable. A walk that silently stops finding files must fail rather than
 * pass, and a walk that finds a little less than the last run must not.
 */
constexpr std::size_t min_distinct_keys = 100000;
constexpr std::size_t min_distinct_types = 40;
constexpr std::size_t min_parsed_files = 130;

/// The eight types this task added to the shape table. A registry without
/// them is the grammar the compiled table carried, which is what the census
/// of distinct series is compared against.
const std::set<std::string> k_added_types{"BOND_OPTION",
                                          "CPR",
                                          "FIXING",
                                          "GENERIC-MD",
                                          "INDEX_CDS_TRANCHE",
                                          "OI_FUTURE",
                                          "RATING",
                                          "SHAPE_PROFILE"};

/// One distinct market data key, split as the parser split it.
struct corpus_entry {
    std::string key;
    std::string series_type;
    std::string metric;
    std::string qualifier;
};

/// One file under the corpus, read once. The read error is kept rather than
/// thrown, because a file that cannot be read is a finding the census reports
/// instead of a failure that stops the walk.
struct corpus_file {
    std::string path;
    std::string content;
    std::string read_error;
};

struct corpus_survey {
    /// Every distinct series the two readers produce, keyed by the verbatim
    /// market data key or, for a fixing, the key the import composes for it.
    /// A key both readers reach collapses into one entry.
    std::map<std::string, corpus_entry> entries;
    /// The files each reader took, and the distinct keys each supplied, so the
    /// census can say which of the two it counted.
    std::set<std::string> market_data_files;
    std::set<std::string> fixing_files;
    std::set<std::string> market_data_keys;
    std::set<std::string> fixing_keys;
    /// Files the walk reached, could not read, and the first such error.
    /// Without these a walk that reads nothing reports the same empty census
    /// as a corpus that carries nothing.
    std::size_t files_seen = 0;
    std::size_t unreadable_files = 0;
    std::string first_read_error;

    /// Distinct series, which is the market_series natural key and so the
    /// number of rows an import of this corpus would write.
    std::size_t distinct_series() const {
        std::set<std::tuple<std::string, std::string, std::string>> series;
        for (const auto& [key, entry] : entries)
            series.insert({entry.series_type, entry.metric, entry.qualifier});
        return series.size();
    }
};

/// Every regular file under the corpus. Read once per process: the walk runs
/// once per registry, and reading 3,500 files twice buys no new information.
const std::vector<corpus_file>& corpus_files() {
    static const auto files = [] {
        std::vector<corpus_file> result;
        const auto root = ores::testing::project_root::resolve("external/ore/examples");

        for (const auto& dir_entry : std::filesystem::recursive_directory_iterator(root)) {
            if (!dir_entry.is_regular_file())
                continue;

            const auto path = dir_entry.path();
            try {
                result.push_back(
                    {path.string(), ores::platform::filesystem::file::read_content(path), {}});
            } catch (const std::exception& ex) {
                result.push_back({path.string(), {}, ex.what()});
            }
        }
        return result;
    }();
    return files;
}

/// A fixing payload is one the corpus names as fixings, because the name is
/// the only thing that says which reader a file feeds. import_service never
/// asks either reader to guess: market data and fixings arrive as two
/// separately named payloads. Handing every file the market data reader
/// rejects to the fixing reader instead would take the corpus's exposure
/// reports for fixings, since parse_fixings checks the date and stores the
/// two fields after it without reading them.
bool is_fixing_payload(const std::string& path) {
    return std::filesystem::path(path).filename().string().rfind("fixings", 0) == 0;
}

/// One pass of the corpus under a given key grammar.
corpus_survey walk_with(const ores::ore::market::series_key_registry& registry) {
    corpus_survey s;
    s.files_seen = corpus_files().size();

    for (const auto& file : corpus_files()) {
        if (!file.read_error.empty()) {
            ++s.unreadable_files;
            if (s.first_read_error.empty())
                s.first_read_error = file.path + ": " + file.read_error;
            continue;
        }

        std::set<std::string> from_this_file;
        if (is_fixing_payload(file.path)) {
            // A fixing series is the one import_service builds for it, so the
            // census counts series and not file lines.
            std::istringstream fixings{file.content};
            try {
                for (const auto& f : ores::ore::market::parse_fixings(fixings)) {
                    const auto key = "FIXING/RATE/" + f.qualifier;
                    s.entries[key] = corpus_entry{key, "FIXING", "RATE", f.qualifier};
                    from_this_file.insert(key);
                }
            } catch (const std::invalid_argument&) {
                // Named as fixings, but not readable as them.
            }
            s.fixing_keys.insert(from_this_file.begin(), from_this_file.end());
            if (!from_this_file.empty())
                s.fixing_files.insert(file.path);
            continue;
        }

        std::istringstream market_data{file.content};
        try {
            for (const auto& d : ores::ore::market::parse_market_data(market_data, registry)) {
                s.entries[d.key] = corpus_entry{d.key, d.series_type, d.metric, d.qualifier};
                from_this_file.insert(d.key);
            }
        } catch (const std::invalid_argument&) {
            // An XML trade, a script, a manifest: most of the corpus.
        }
        s.market_data_keys.insert(from_this_file.begin(), from_this_file.end());
        if (!from_this_file.empty())
            s.market_data_files.insert(file.path);
    }
    return s;
}

/// The walk every case in this file shares, under the grammar the shape table
/// carries.
const corpus_survey& survey() {
    static const auto result = walk_with(ores::testing::seed_registry());
    return result;
}

/// The classifier every case in this file shares, built from the classification
/// rule seed the way import_service builds it from the table.
const series_classifier& seed_classifier() {
    static const auto result = series_classifier(ores::testing::seed_classification_rules());
    return result;
}

std::set<std::string> distinct_types() {
    std::set<std::string> types;
    for (const auto& [key, entry] : survey().entries)
        types.insert(entry.series_type);
    return types;
}

series_classification_rule make_literal_rule(const std::string& series_type,
                                             const std::string& metric,
                                             const std::string& asset_class_code,
                                             const std::string& series_subclass_code) {
    series_classification_rule rule;
    rule.series_type = series_type;
    rule.metric = metric;
    rule.asset_class_source = "literal";
    rule.asset_class_code = asset_class_code;
    rule.series_subclass_code = series_subclass_code;
    return rule;
}

series_classification_rule make_correlation_rule(const std::string& series_type,
                                                 const std::string& series_subclass_code) {
    series_classification_rule rule;
    rule.series_type = series_type;
    rule.asset_class_source = "correlation_operands";
    rule.series_subclass_code = series_subclass_code;
    return rule;
}

/// Covers each branch the classifier takes: a row keyed by its type alone, a
/// pair of rows where the metric decides, and the row whose classes come from
/// the operands in the key.
std::vector<series_classification_rule> fixture() {
    return {
        make_literal_rule("DISCOUNT", "", "interest_rates", "yield"),
        make_literal_rule("GENERIC-MD", "EQUITY_OPTION", "equity", "volatility"),
        make_literal_rule("GENERIC-MD", "", "commodity", "forward"),
        make_correlation_rule("CORRELATION", "correlation"),
    };
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
    const auto parsed_files = survey().market_data_files.size() + survey().fixing_files.size();

    // Reported on a passing run too, so the census is visible to whoever
    // tightens the floors above.
    std::ostringstream census;
    census << "corpus census: " << entries.size() << " distinct series keys ("
           << distinct_types().size() << " types) from " << survey().market_data_keys.size()
           << " market data keys in " << survey().market_data_files.size() << " files and "
           << survey().fixing_keys.size() << " fixing keys in " << survey().fixing_files.size()
           << " files, of " << survey().files_seen << " seen, " << survey().unreadable_files
           << " unreadable";
    // Named only when the walk found nothing. A healthy walk parses the XML
    // majority into nothing by design, and those messages would drown the
    // census; an empty walk is the case where one of them is the answer.
    if (entries.empty() && !survey().first_read_error.empty())
        census << "; first read error: " << survey().first_read_error;
    WARN(census.str());
    REQUIRE(entries.size() >= min_distinct_keys);
    REQUIRE(parsed_files >= min_parsed_files);

    std::vector<std::string> unclassified;
    for (const auto& [key, entry] : entries) {
        if (!seed_classifier().try_classify(entry.series_type, entry.metric, entry.qualifier))
            unclassified.push_back(key);
    }

    for (const auto& key : unclassified)
        INFO("unclassified key: " << key);
    CHECK(unclassified.empty());
}

TEST_CASE("every_series_type_the_ore_corpus_carries_is_known_to_the_classifier", tags) {
    const auto types = distinct_types();
    const auto known = seed_classifier().known_series_types();
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

TEST_CASE("the_classifier_and_the_key_registry_name_the_same_series_types", tags) {
    const auto from_classifier = seed_classifier().known_series_types();
    const auto from_registry = ores::testing::seed_registry().known_series_types();

    // Reported separately in each direction, because which table gained a type
    // is what says where the row is missing. The two must agree: a type with a
    // shape row and no classification row aborts an import, and a type with a
    // classification row and no shape row folds every key into its qualifier.
    std::vector<std::string> classifier_only;
    std::vector<std::string> registry_only;
    std::set_difference(from_classifier.begin(),
                        from_classifier.end(),
                        from_registry.begin(),
                        from_registry.end(),
                        std::back_inserter(classifier_only));
    std::set_difference(from_registry.begin(),
                        from_registry.end(),
                        from_classifier.begin(),
                        from_classifier.end(),
                        std::back_inserter(registry_only));

    for (const auto& type : classifier_only)
        INFO("classifiable but with no shape row: " << type);
    for (const auto& type : registry_only)
        INFO("shape row but not classifiable: " << type);

    CHECK(classifier_only.empty());
    CHECK(registry_only.empty());
}

TEST_CASE("every_classification_code_the_classifier_can_emit_exists_in_its_catalogue", tags) {
    std::set<std::string> asset_classes;
    std::set<std::string> subclasses;

    const auto record = [&](const series_classification& c) {
        asset_classes.insert(c.asset_classes.begin(), c.asset_classes.end());
        subclasses.insert(c.series_subclass);
    };

    // Swept over the vocabulary rather than over the corpus, because a code
    // only an unused rule emits would otherwise go unnoticed until an import
    // wrote a row and the insert failed. The metric is ignored for every type
    // but the two handled below.
    for (const auto& type : seed_classifier().known_series_types()) {
        if (type == "CORRELATION" || type == "GENERIC-MD")
            continue;
        record(seed_classifier().classify(type, "PROBE", "PROBE"));
    }

    record(
        seed_classifier().classify("CORRELATION", "RATE", "EQ-RIC:.SPX/FX-GENERIC-USD-EUR/1Y/ATM"));
    record(seed_classifier().classify(
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
            seed_classifier().try_classify(entry.series_type, entry.metric, entry.qualifier);
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

TEST_CASE("the_classification_rule_seed_is_well_formed", tags) {
    const auto& rules = ores::testing::seed_classification_rules();

    // The reader of the table aborts on an empty one and names the table, so a
    // seed that parses into nothing is the failure this catches first.
    REQUIRE(!rules.empty());

    std::set<std::pair<std::string, std::string>> seen;
    for (const auto& rule : rules) {
        INFO("rule: " << rule.series_type << "/" << rule.metric);
        CHECK((rule.asset_class_source == "literal" ||
               rule.asset_class_source == "correlation_operands"));
        CHECK(seen.insert({rule.series_type, rule.metric}).second);
        CHECK(!rule.series_subclass_code.empty());
        CHECK(!rule.description.empty());

        // The two columns gate each other: a literal rule names its class and
        // no other rule does, which is what the table's checks enforce at
        // insert time.
        if (rule.asset_class_source == "literal")
            CHECK(rule.asset_class_code.has_value());
        else
            CHECK(!rule.asset_class_code.has_value());
    }

    // The one rule whose classes come from its key, and the one keyed by its
    // metric rather than by its type alone.
    const auto correlation = std::find_if(rules.begin(), rules.end(), [](const auto& rule) {
        return rule.series_type == "CORRELATION";
    });
    REQUIRE(correlation != rules.end());
    CHECK(correlation->asset_class_source == "correlation_operands");
    CHECK(correlation->series_subclass_code == "correlation");

    const auto generic = std::find_if(rules.begin(), rules.end(), [](const auto& rule) {
        return rule.series_type == "GENERIC-MD";
    });
    REQUIRE(generic != rules.end());
    CHECK(generic->metric == "EQUITY_OPTION");
    CHECK(generic->asset_class_code == "equity");
}

TEST_CASE("the_eight_added_rows_collapse_the_census_of_distinct_series", tags) {
    // The grammar the compiled table carried: every row the shape table holds
    // today, less the eight this task added. A key of one of those types folded
    // its whole remainder into the qualifier, so each distinct key became a
    // series of its own.
    auto before = ores::testing::seed_shapes();
    std::erase_if(before,
                  [](const auto& shape) { return k_added_types.count(shape.series_type) > 0; });
    REQUIRE(before.size() + k_added_types.size() == ores::testing::seed_shapes().size());

    const ores::ore::market::series_key_registry compiled{before};
    const auto after = survey().distinct_series();
    const auto prior = walk_with(compiled).distinct_series();

    WARN("distinct series: " << prior << " before the eight rows, " << after << " after");
    CHECK(after < prior);
}

// =============================================================================
// series_classifier — a value built from rows, with no database
// =============================================================================

TEST_CASE("classifier_answers_from_its_rows_and_not_a_compiled_table", unit_tags) {
    const series_classifier classifier(fixture());

    const auto discount = classifier.classify("DISCOUNT", "RATE", "EUR/EUR1D");
    CHECK(discount.asset_classes == std::vector<std::string>{"interest_rates"});
    CHECK(discount.series_subclass == "yield");

    // IR_SWAP has no row in this fixture, so a classifier built from rows has
    // nothing to answer with: there is no compiled fallback behind it.
    CHECK_FALSE(classifier.try_classify("IR_SWAP", "RATE", "EUR/1Y"));
    CHECK_THROWS_AS(classifier.classify("IR_SWAP", "RATE", "EUR/1Y"), std::invalid_argument);
}

TEST_CASE("classifier_prefers_the_metric_row_and_falls_back_to_the_empty_one", unit_tags) {
    const series_classifier classifier(fixture());

    const auto exact = classifier.try_classify("GENERIC-MD", "EQUITY_OPTION", "PRICE/RIC:.SPX");
    REQUIRE(exact);
    CHECK(exact->asset_classes == std::vector<std::string>{"equity"});
    CHECK(exact->series_subclass == "volatility");

    // A metric with no row of its own takes the type's row for the empty
    // metric, which is what the column's documented lookup rule says it does.
    const auto fallback = classifier.try_classify("GENERIC-MD", "SHAPE_FACTOR", "PRICE");
    REQUIRE(fallback);
    CHECK(fallback->asset_classes == std::vector<std::string>{"commodity"});
    CHECK(fallback->series_subclass == "forward");
}

TEST_CASE("classifier_takes_a_correlations_classes_from_its_operands", unit_tags) {
    const series_classifier classifier(fixture());

    const auto cross =
        classifier.classify("CORRELATION", "RATE", "EQ-RIC:.SPX/FX-GENERIC-USD-EUR/1Y/ATM");
    CHECK(cross.asset_classes == std::vector<std::string>{"equity", "fx"});
    CHECK(cross.series_subclass == "correlation");

    // The same class on both sides is one class, not two.
    const auto same = classifier.classify("CORRELATION", "RATE", "EQ-RIC:.SPX/EQ-RIC:.NDX/1Y/ATM");
    CHECK(same.asset_classes == std::vector<std::string>{"equity"});

    // An operand the convention does not name contributes no class, which the
    // junction allows and a not-null column would not.
    const auto unknown = classifier.classify("CORRELATION", "RATE", "SOMETHING/A-NOTHER");
    CHECK(unknown.asset_classes.empty());
}

TEST_CASE("classifier_known_series_types_is_sorted", unit_tags) {
    const series_classifier classifier(fixture());
    const std::vector<std::string> expected{"CORRELATION", "DISCOUNT", "GENERIC-MD"};
    CHECK(classifier.known_series_types() == expected);
}

TEST_CASE("classifier_takes_the_last_row_when_a_type_and_metric_repeat", unit_tags) {
    auto rules = fixture();
    rules.push_back(make_literal_rule("DISCOUNT", "", "inflation", "capfloor"));

    const series_classifier classifier(rules);
    const auto discount = classifier.classify("DISCOUNT", "RATE", "EUR/EUR1D");
    CHECK(discount.asset_classes == std::vector<std::string>{"inflation"});
    CHECK(discount.series_subclass == "capfloor");
    CHECK(classifier.known_series_types().size() == 3);
}

TEST_CASE("classifier_rejects_an_unseeded_table", unit_tags) {
    try {
        const series_classifier classifier(std::vector<series_classification_rule>{});
        FAIL("accepted an empty table, known types: " << classifier.known_series_types().size());
    } catch (const std::invalid_argument& ex) {
        const std::string msg{ex.what()};
        CHECK(msg.find("ores_marketdata_series_classification_rules_tbl") != std::string::npos);
        CHECK(msg.find("unseeded") != std::string::npos);
    }
}

TEST_CASE("classifier_rejects_a_literal_row_with_no_asset_class_code", unit_tags) {
    // The helper always sets the column, and an empty string engages the
    // optional, so the null column a reader must reject is stated by clearing
    // it — the way the mapper carries a null through.
    auto unclassified = make_literal_rule("OI_FUTURE", "", "commodity", "forward");
    unclassified.asset_class_code.reset();

    auto rules = fixture();
    rules.push_back(std::move(unclassified));

    try {
        const series_classifier classifier(rules);
        FAIL("accepted a literal row with no class, known types: "
             << classifier.known_series_types().size());
    } catch (const std::invalid_argument& ex) {
        const std::string msg{ex.what()};
        CHECK(msg.find("OI_FUTURE") != std::string::npos);
        CHECK(msg.find("no asset class code") != std::string::npos);
    }
}

TEST_CASE("classifier_rejects_an_asset_class_source_it_does_not_know", unit_tags) {
    auto rules = fixture();
    auto rule = make_literal_rule("OI_FUTURE", "", "commodity", "forward");
    rule.asset_class_source = "magic";
    rules.push_back(rule);

    try {
        const series_classifier classifier(rules);
        FAIL("accepted an unknown source, known types: " << classifier.known_series_types().size());
    } catch (const std::invalid_argument& ex) {
        const std::string msg{ex.what()};
        CHECK(msg.find("magic") != std::string::npos);
        CHECK(msg.find("literal") != std::string::npos);
    }
}

TEST_CASE("classifier_rejects_a_correlation_row_that_also_names_a_class", unit_tags) {
    auto rules = fixture();
    auto rule = make_correlation_rule("INDEX_CDS_TRANCHE", "correlation");
    rule.asset_class_code = "credit";
    rules.push_back(rule);

    try {
        const series_classifier classifier(rules);
        FAIL("accepted a contradictory row, known types: "
             << classifier.known_series_types().size());
    } catch (const std::invalid_argument& ex) {
        const std::string msg{ex.what()};
        CHECK(msg.find("INDEX_CDS_TRANCHE") != std::string::npos);
        CHECK(msg.find("credit") != std::string::npos);
    }
}
