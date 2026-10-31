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
#include "ores.marketdata.api/domain/asset_class_authorities.hpp"
#include "ores.marketdata.api/domain/series_classification_rule.hpp"
#include "ores.marketdata.core/classification/series_classifier.hpp"
#include "ores.marketdata.core/datum/ore_index_codec.hpp"
#include "ores.marketdata.core/datum/ore_key_codec.hpp"
#include "ores.marketdata.core/datum/oresmd_uri_codec.hpp"
#include "ores.ore.core/market/market_data_parser.hpp"
#include "ores.platform/filesystem/file.hpp"
#include "ores.testing/project_root.hpp"
#include "ores.testing/series_classification_rule_seed.hpp"
#include <algorithm>
#include <catch2/catch_test_macros.hpp>
#include <filesystem>
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
 * payloads, and this corpus names its fixing payloads `fixings*`. A fixing is an
 * index and not a market datum, so the import names its series by the index and
 * this walk gives it the same identity. A file that is neither, which is most of
 * the corpus, parses into nothing.
 *
 * Classification only. The ORE-to-oresmd round trip is a separate concern.
 */

namespace {

using ores::marketdata::core::classification_key;
using ores::marketdata::core::classification_key_of;
using ores::marketdata::core::key_kind;
using ores::marketdata::core::series_classification;
using ores::marketdata::core::series_classifier;
using ores::marketdata::domain::series_classification_rule;

const std::string unit_tags("[marketdata][classification]");

/**
 * Anti-vacuity floors, set under the census a run measured over the corpus:
 * 108,215 distinct series identities from 108,057 market data keys in 100 files
 * and 158 fixing keys in 55 files, out of 3,528 walked, none unreadable. A walk
 * that silently stops finding files must fail rather than pass, and a walk that
 * finds a little less than the last run must not.
 */
constexpr std::size_t min_distinct_keys = 100000;
constexpr std::size_t min_distinct_instruments = 35;
constexpr std::size_t min_parsed_files = 130;

/// One distinct series, with the classification key the ingest gives it.
struct corpus_entry {
    std::string key;
    classification_key identity;
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
    /// market data key or, for a fixing, the index URI the import composes.
    /// A key both readers reach collapses into one entry.
    std::map<std::string, corpus_entry> entries;
    /// The files each reader took, and the distinct keys each supplied, so the
    /// census can say which of the two it counted.
    std::set<std::string> market_data_files;
    std::set<std::string> fixing_files;
    std::set<std::string> market_data_keys;
    std::set<std::string> fixing_keys;
    /// Files the walk reached, could not read, and the first such error.
    std::size_t files_seen = 0;
    std::size_t unreadable_files = 0;
    std::string first_read_error;
};

/// Every regular file under the corpus, read once per process.
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

/// A file the corpus names as fixings, because the name is the only thing that
/// says which reader a file feeds.
bool named_as_fixings(const std::string& path) {
    return std::filesystem::path(path).filename().string().rfind("fixings", 0) == 0;
}

/// One pass of the corpus, naming each market datum as the import does: the ORE
/// key codec reads the key, and a key the codec refuses is skipped, because the
/// import skips it too. A fixing is named by the index its name projects to.
corpus_survey walk() {
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
        if (named_as_fixings(file.path)) {
            std::istringstream fixings{file.content};
            try {
                for (const auto& f : ores::ore::market::parse_fixings(fixings)) {
                    const auto index = ores::marketdata::datum::ore_index_codec::read(f.qualifier);
                    if (!index)
                        continue;
                    const auto uri = ores::marketdata::datum::oresmd_uri_codec::write_index(*index);
                    if (!uri)
                        continue;
                    s.entries[*uri] = corpus_entry{*uri, classification_key_of(*index)};
                    from_this_file.insert(*uri);
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
            for (const auto& d : ores::ore::market::parse_market_data(market_data)) {
                const auto datum = ores::marketdata::datum::ore_key_codec::read(d.key);
                if (!datum)
                    continue;
                s.entries[d.key] = corpus_entry{d.key, classification_key_of(*datum)};
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

/// The walk every case in this file shares.
const corpus_survey& survey() {
    static const auto result = walk();
    return result;
}

/// The classifier every case in this file shares, built from the classification
/// rule seed the way the ingest builds it from the table.
const series_classifier& seed_classifier() {
    static const auto result = series_classifier(ores::testing::seed_classification_rules());
    return result;
}

std::set<std::string> distinct_instruments() {
    std::set<std::string> instruments;
    for (const auto& [key, entry] : survey().entries)
        instruments.insert(entry.identity.instrument);
    return instruments;
}

series_classification_rule make_literal_rule(const std::string& instrument,
                                             const std::string& quote,
                                             const std::string& asset_class_code,
                                             const std::string& series_subclass_code) {
    series_classification_rule rule;
    rule.key_kind = "datum";
    rule.instrument = instrument;
    rule.quote = quote;
    rule.asset_class_source = "literal";
    rule.asset_class_code = asset_class_code;
    rule.series_subclass_code = series_subclass_code;
    return rule;
}

series_classification_rule make_correlation_rule(const std::string& series_subclass_code) {
    series_classification_rule rule;
    rule.key_kind = "datum";
    rule.instrument = "correlation";
    rule.asset_class_source = "correlation_operands";
    rule.series_subclass_code = series_subclass_code;
    return rule;
}

series_classification_rule make_index_rule(const std::string& instrument,
                                           const std::string& series_subclass_code) {
    series_classification_rule rule;
    rule.key_kind = "index";
    rule.instrument = instrument;
    rule.asset_class_source = "index_authority";
    rule.series_subclass_code = series_subclass_code;
    return rule;
}

/// A datum key, with the operands a correlation carries and nothing else.
classification_key datum_key(const std::string& instrument, const std::string& quote) {
    classification_key key;
    key.kind = key_kind::datum;
    key.instrument = instrument;
    key.quote = quote;
    return key;
}

classification_key correlation_key(const std::vector<std::string>& operands) {
    auto key = datum_key("correlation", "rate");
    key.operands = operands;
    return key;
}

classification_key index_key(const std::string& family) {
    classification_key key;
    key.kind = key_kind::index;
    key.instrument = family;
    return key;
}

/// Covers each branch the classifier takes: a row keyed by its instrument alone,
/// a pair of rows where the quote decides, the row whose classes come from a
/// correlation's operands, and the row keyed by an index.
std::vector<series_classification_rule> fixture() {
    return {
        make_literal_rule("discount", "", "interest_rates", "yield"),
        make_literal_rule("equity_option", "price", "equity", "volatility"),
        make_literal_rule("equity_option", "", "equity", "forward"),
        make_correlation_rule("correlation"),
        make_index_rule("", "index_fixing"),
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

TEST_CASE("every_distinct_series_identity_in_the_ore_corpus_classifies",
          "[marketdata][classification][corpus]") {
    const auto& entries = survey().entries;
    const auto parsed_files = survey().market_data_files.size() + survey().fixing_files.size();

    // Reported on a passing run too, so the census is visible to whoever
    // tightens the floors above.
    std::ostringstream census;
    census << "corpus census: " << entries.size() << " distinct series identities ("
           << distinct_instruments().size() << " instruments) from "
           << survey().market_data_keys.size() << " market data keys in "
           << survey().market_data_files.size() << " files and " << survey().fixing_keys.size()
           << " fixing keys in " << survey().fixing_files.size() << " files, of "
           << survey().files_seen << " seen, " << survey().unreadable_files << " unreadable";
    if (entries.empty() && !survey().first_read_error.empty())
        census << "; first read error: " << survey().first_read_error;
    WARN(census.str());
    REQUIRE(entries.size() >= min_distinct_keys);
    REQUIRE(parsed_files >= min_parsed_files);

    std::vector<std::string> unclassified;
    for (const auto& [key, entry] : entries) {
        if (!seed_classifier().try_classify(entry.identity))
            unclassified.push_back(key);
    }

    for (const auto& key : unclassified)
        INFO("unclassified identity: " << key);
    CHECK(unclassified.empty());
}

TEST_CASE("every_instrument_the_ore_corpus_carries_is_known_to_the_classifier",
          "[marketdata][classification][corpus]") {
    const auto instruments = distinct_instruments();
    REQUIRE(instruments.size() >= min_distinct_instruments);

    std::set<std::string> known;
    for (const auto kind : {key_kind::datum, key_kind::index})
        for (const auto& instrument : seed_classifier().known_instruments(kind))
            known.insert(instrument);

    std::vector<std::string> unknown;
    for (const auto& instrument : instruments) {
        // An index rule keyed by the empty instrument speaks for every family.
        if (!known.count(instrument) && !known.count(std::string{}))
            unknown.push_back(instrument);
    }

    for (const auto& instrument : unknown)
        INFO("instrument the corpus carries but the vocabulary lacks: " << instrument);
    CHECK(unknown.empty());
}

TEST_CASE("every_classification_code_the_classifier_can_emit_exists_in_its_catalogue",
          "[marketdata][classification][corpus]") {
    std::set<std::string> asset_classes;
    std::set<std::string> subclasses;

    const auto record = [&](const series_classification& c) {
        asset_classes.insert(c.asset_classes.begin(), c.asset_classes.end());
        subclasses.insert(c.series_subclass);
    };

    // Swept over the vocabulary rather than over the corpus, because a code
    // only an unused rule emits would otherwise go unnoticed until an ingest
    // wrote a row and the insert failed.
    for (const auto kind : {key_kind::datum, key_kind::index}) {
        for (const auto& instrument : seed_classifier().known_instruments(kind)) {
            if (instrument == "correlation") {
                record(seed_classifier().classify(
                    correlation_key({"EQ-RIC:.SPX", "FX-GENERIC-USD-EUR"})));
                continue;
            }
            classification_key key;
            key.kind = kind;
            key.instrument = instrument;
            key.quote = "probe";
            record(seed_classifier().classify(key));
        }
    }

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

TEST_CASE("a_correlation_takes_its_classes_from_its_two_operands",
          "[marketdata][classification][corpus]") {
    std::map<std::pair<std::string, std::string>, series_classification> by_operands;
    for (const auto& [key, entry] : survey().entries) {
        if (entry.identity.instrument != "correlation")
            continue;
        REQUIRE(entry.identity.operands.size() == 2);
        const auto c = seed_classifier().try_classify(entry.identity);
        REQUIRE(c);
        by_operands[{entry.identity.operands[0], entry.identity.operands[1]}] = *c;
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

TEST_CASE("the_classification_rule_seed_is_well_formed", "[marketdata][classification][corpus]") {
    const auto& rules = ores::testing::seed_classification_rules();

    // The reader of the table aborts on an empty one and names the table, so a
    // seed that parses into nothing is the failure this catches first.
    REQUIRE(!rules.empty());

    std::set<std::tuple<std::string, std::string, std::string>> seen;
    for (const auto& rule : rules) {
        INFO("rule: " << rule.key_kind << "/" << rule.instrument << "/" << rule.quote);
        CHECK((rule.key_kind == "datum" || rule.key_kind == "index"));
        CHECK((rule.asset_class_source == "literal" ||
               rule.asset_class_source == "correlation_operands" ||
               rule.asset_class_source == "index_authority"));
        CHECK(seen.insert({rule.key_kind, rule.instrument, rule.quote}).second);
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

    // The rule whose classes come from its operands.
    const auto correlation = std::find_if(rules.begin(), rules.end(), [](const auto& rule) {
        return rule.asset_class_source == "correlation_operands";
    });
    REQUIRE(correlation != rules.end());
    CHECK(correlation->instrument == "correlation");
    CHECK(correlation->series_subclass_code == "correlation");

    // One index rule serves every family, so the taxonomy has a single row that
    // is not a datum, and no row for ORE's wrapper that nothing can reach.
    const auto indexes = std::count_if(
        rules.begin(), rules.end(), [](const auto& rule) { return rule.key_kind == "index"; });
    CHECK(indexes == 1);

    // Every instrument is keyed by the spelling a URI writes: lower case, with
    // underscores, never an ORE key token.
    for (const auto& rule : rules)
        CHECK(rule.instrument.find('-') == std::string::npos);
}

// =============================================================================
// series_classifier -- a value built from rows, with no database
// =============================================================================

TEST_CASE("classifier_answers_from_its_rows_and_not_a_compiled_table", unit_tags) {
    const series_classifier classifier(fixture());

    const auto discount = classifier.classify(datum_key("discount", "rate"));
    CHECK(discount.asset_classes == std::vector<std::string>{"interest_rates"});
    CHECK(discount.series_subclass == "yield");

    // ir_swap has no row in this fixture, so a classifier built from rows has
    // nothing to answer with: there is no compiled fallback behind it.
    CHECK_FALSE(classifier.try_classify(datum_key("ir_swap", "rate")));
    CHECK_THROWS_AS(classifier.classify(datum_key("ir_swap", "rate")), std::invalid_argument);
}

TEST_CASE("classifier_prefers_the_quote_row_and_falls_back_to_the_empty_one", unit_tags) {
    const series_classifier classifier(fixture());

    const auto exact = classifier.try_classify(datum_key("equity_option", "price"));
    REQUIRE(exact);
    CHECK(exact->asset_classes == std::vector<std::string>{"equity"});
    CHECK(exact->series_subclass == "volatility");

    // A quote with no row of its own takes the instrument's row for the empty
    // quote, which is what the column's documented lookup rule says it does.
    const auto fallback = classifier.try_classify(datum_key("equity_option", "shape_factor"));
    REQUIRE(fallback);
    CHECK(fallback->asset_classes == std::vector<std::string>{"equity"});
    CHECK(fallback->series_subclass == "forward");
}

TEST_CASE("classifier_keeps_the_two_kinds_of_identity_apart", unit_tags) {
    const series_classifier classifier(fixture());

    // An instrument name is a datum; the same text as a family is an index.
    CHECK_FALSE(classifier.try_classify(datum_key("", "rate")).has_value());
    const auto index = classifier.try_classify(index_key("equity"));
    REQUIRE(index);
    CHECK(index->series_subclass == "index_fixing");
}

TEST_CASE("the_catalogue_mapping_answers_for_every_authority", unit_tags) {
    using ores::marketdata::domain::asset_class_for_authority;

    // The mapping the index_authority branch reads, exercised directly so the
    // generated table cannot drift from the catalogue unnoticed.
    CHECK(asset_class_for_authority("ir") == std::optional<std::string_view>{"interest_rates"});
    CHECK(asset_class_for_authority("security") == std::optional<std::string_view>{"bond"});
    CHECK(asset_class_for_authority("power") == std::optional<std::string_view>{"commodity"});
    CHECK(asset_class_for_authority("rating") == std::optional<std::string_view>{"credit"});

    // An authority the catalogue maps to no class, and a name it does not
    // hold, both answer nothing.
    CHECK_FALSE(asset_class_for_authority("correlation").has_value());
    CHECK_FALSE(asset_class_for_authority("generic").has_value());
    CHECK_FALSE(asset_class_for_authority("ghost").has_value());
}

TEST_CASE("classifier_reads_a_fixings_class_from_its_index_family", unit_tags) {
    const series_classifier classifier(fixture());

    // One index rule serves every family, and the class follows the family
    // rather than one literal all fixings would share.
    const auto rates = classifier.classify(index_key("ibor"));
    CHECK(rates.asset_classes == std::vector<std::string>{"interest_rates"});
    CHECK(rates.series_subclass == "index_fixing");

    const auto power = classifier.classify(index_key("power"));
    CHECK(power.asset_classes == std::vector<std::string>{"commodity"});

    // A family the catalogue maps to no class leaves the class list empty
    // rather than guessing a class or blocking the ingest.
    const auto unknown = classifier.classify(index_key("generic"));
    CHECK(unknown.asset_classes.empty());
    CHECK(unknown.series_subclass == "index_fixing");
}

TEST_CASE("classifier_rejects_an_index_rule_that_names_a_datum_source", unit_tags) {
    auto rules = fixture();
    auto rule = make_literal_rule("ibor", "", "interest_rates", "index_fixing");
    rule.key_kind = "index";
    rules.push_back(rule);

    try {
        const series_classifier classifier(rules);
        FAIL("accepted a contradictory row, index instruments: "
             << classifier.known_instruments(key_kind::index).size());
    } catch (const std::invalid_argument& ex) {
        const std::string msg{ex.what()};
        CHECK(msg.find("index") != std::string::npos);
    }
}

TEST_CASE("classifier_takes_a_correlations_classes_from_its_operands", unit_tags) {
    const series_classifier classifier(fixture());

    const auto cross = classifier.classify(correlation_key({"EQ-RIC:.SPX", "FX-GENERIC-USD-EUR"}));
    CHECK(cross.asset_classes == std::vector<std::string>{"equity", "fx"});
    CHECK(cross.series_subclass == "correlation");

    // The same class on both sides is one class, not two.
    const auto same = classifier.classify(correlation_key({"EQ-RIC:.SPX", "EQ-RIC:.NDX"}));
    CHECK(same.asset_classes == std::vector<std::string>{"equity"});

    // An operand the convention does not name contributes no class, which the
    // junction allows and a not-null column would not.
    const auto unknown = classifier.classify(correlation_key({"SOMETHING", "A-NOTHER"}));
    CHECK(unknown.asset_classes.empty());
}

TEST_CASE("classifier_known_instruments_is_sorted", unit_tags) {
    const series_classifier classifier(fixture());
    const std::vector<std::string> datum_expected{"correlation", "discount", "equity_option"};
    CHECK(classifier.known_instruments(key_kind::datum) == datum_expected);
    // The index row is keyed by the empty instrument, which speaks for every
    // family, so the index vocabulary is that one entry.
    CHECK(classifier.known_instruments(key_kind::index) == std::vector<std::string>{""});
}

TEST_CASE("classifier_takes_the_last_row_when_a_key_repeats", unit_tags) {
    auto rules = fixture();
    rules.push_back(make_literal_rule("discount", "", "inflation", "capfloor"));

    const series_classifier classifier(rules);
    const auto discount = classifier.classify(datum_key("discount", "rate"));
    CHECK(discount.asset_classes == std::vector<std::string>{"inflation"});
    CHECK(discount.series_subclass == "capfloor");
    CHECK(classifier.known_instruments(key_kind::datum).size() == 3);
}

TEST_CASE("classifier_rejects_an_unseeded_table", unit_tags) {
    try {
        const series_classifier classifier(std::vector<series_classification_rule>{});
        FAIL("accepted an empty table, instruments: "
             << classifier.known_instruments(key_kind::datum).size());
    } catch (const std::invalid_argument& ex) {
        const std::string msg{ex.what()};
        CHECK(msg.find("ores_marketdata_series_classification_rules_tbl") != std::string::npos);
        CHECK(msg.find("unseeded") != std::string::npos);
    }
}

TEST_CASE("classifier_rejects_a_literal_row_with_no_asset_class_code", unit_tags) {
    // The helper always sets the column, and an empty string engages the
    // optional, so the null column a reader must reject is stated by clearing
    // it -- the way the mapper carries a null through.
    auto unclassified = make_literal_rule("oi_future", "", "commodity", "forward");
    unclassified.asset_class_code.reset();

    auto rules = fixture();
    rules.push_back(std::move(unclassified));

    try {
        const series_classifier classifier(rules);
        FAIL("accepted a literal row with no class, instruments: "
             << classifier.known_instruments(key_kind::datum).size());
    } catch (const std::invalid_argument& ex) {
        const std::string msg{ex.what()};
        CHECK(msg.find("oi_future") != std::string::npos);
        CHECK(msg.find("no asset class code") != std::string::npos);
    }
}

TEST_CASE("classifier_rejects_an_asset_class_source_it_does_not_know", unit_tags) {
    auto rules = fixture();
    auto rule = make_literal_rule("oi_future", "", "commodity", "forward");
    rule.asset_class_source = "magic";
    rules.push_back(rule);

    try {
        const series_classifier classifier(rules);
        FAIL("accepted an unknown source, instruments: "
             << classifier.known_instruments(key_kind::datum).size());
    } catch (const std::invalid_argument& ex) {
        const std::string msg{ex.what()};
        CHECK(msg.find("magic") != std::string::npos);
        CHECK(msg.find("literal") != std::string::npos);
    }
}

TEST_CASE("classifier_rejects_a_key_kind_it_does_not_know", unit_tags) {
    auto rules = fixture();
    auto rule = make_literal_rule("oi_future", "", "commodity", "forward");
    rule.key_kind = "ore_key";
    rules.push_back(rule);

    try {
        const series_classifier classifier(rules);
        FAIL("accepted an unknown key kind, instruments: "
             << classifier.known_instruments(key_kind::datum).size());
    } catch (const std::invalid_argument& ex) {
        const std::string msg{ex.what()};
        CHECK(msg.find("ore_key") != std::string::npos);
        CHECK(msg.find("datum") != std::string::npos);
    }
}

TEST_CASE("classifier_rejects_a_correlation_row_that_also_names_a_class", unit_tags) {
    auto rules = fixture();
    auto rule = make_correlation_rule("correlation");
    rule.instrument = "index_cds_tranche";
    rule.asset_class_code = "credit";
    rules.push_back(rule);

    try {
        const series_classifier classifier(rules);
        FAIL("accepted a contradictory row, instruments: "
             << classifier.known_instruments(key_kind::datum).size());
    } catch (const std::invalid_argument& ex) {
        const std::string msg{ex.what()};
        CHECK(msg.find("index_cds_tranche") != std::string::npos);
        CHECK(msg.find("credit") != std::string::npos);
    }
}
