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
#include "ores.http.core/routes/ore_value_sets.hpp"
#include "ores.logging/make_logger.hpp"
#include "test_suite.hpp"
#include <catch2/catch_test_macros.hpp>
#include <string>
#include <vector>

using ores::http::routes::setting_value_set_row;
using ores::http::routes::source_kinds;
using ores::http::routes::value_choice;
using ores::http::routes::value_fields;
using ores::http::routes::value_set_catalogue_row;
using ores::http::routes::value_set_error;
using ores::http::routes::value_set_reader;
using ores::http::routes::value_set_row;
using ores::http::routes::value_set_tables;
using ores::http::routes::value_set_value_row;
using namespace ores::logging;

namespace {

/// The tables one test states: a set, the setting that links to it, and the values it lists.
value_set_tables tables_of(value_set_row set, setting_value_set_row link) {
    value_set_tables tables;
    tables.sets.push_back(std::move(set));
    tables.links.push_back(std::move(link));
    return tables;
}

value_set_row set_of(std::string code, std::string kind, std::string catalogue = {}) {
    value_set_row row;
    row.code = std::move(code);
    row.source_kind = std::move(kind);
    row.catalogue_code = std::move(catalogue);
    return row;
}

setting_value_set_row link_of(std::string scope, std::string name, std::string set) {
    setting_value_set_row row;
    row.scope = std::move(scope);
    row.name = std::move(name);
    row.value_set_code = std::move(set);
    return row;
}

value_set_value_row
value_of(std::string set, std::string value, int order, std::string canonical = {}) {
    value_set_value_row row;
    row.value_set_code = std::move(set);
    row.value = std::move(value);
    row.label = row.value;
    row.display_order = order;
    row.canonical_value = std::move(canonical);
    return row;
}

/// The sentence a refusal states.
std::string
sentence_of(const value_set_reader& sut, std::string_view scope, std::string_view name) {
    try {
        static_cast<void>(sut.fields_of(scope, name));
    } catch (const value_set_error& e) {
        return e.what();
    }
    return {};
}

}

TEST_CASE("a_boolean_value_set_offers_the_built_in_pair", "[ore_value_sets]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing a boolean value set";

    value_set_reader sut(
        tables_of(set_of("setup.flag", "boolean"), link_of("Setup", "nThreads", "setup.flag")), {});
    const auto fields = sut.fields_of("Setup", "nThreads");

    CHECK(fields.value_set == "setup.flag");
    CHECK(fields.source_kind == "boolean");
    CHECK(fields.choices.size() == 2);
    CHECK(fields.choices[0].value == "true");
    CHECK(fields.choices[1].value == "false");
    CHECK(!fields.minimum.has_value());
    CHECK(fields.pattern.empty());
}

TEST_CASE("an_enumerated_value_set_offers_its_rows_in_order", "[ore_value_sets]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing an enumerated value set";

    auto tables = tables_of(set_of("setup.observation_model", "enumerated"),
                            link_of("Setup", "observationModel", "setup.observation_model"));
    tables.values.push_back(value_of("setup.observation_model", "Explicit", 2));
    tables.values.push_back(value_of("setup.observation_model", "None", 1));
    // A row that names a canonical form is another rendering of one, not a choice.
    tables.values.push_back(value_of("setup.observation_model", "none", 0, "None"));

    value_set_reader sut(std::move(tables), {});
    const auto fields = sut.fields_of("Setup", "observationModel");

    REQUIRE(fields.choices.size() == 2);
    CHECK(fields.choices[0].value == "None");
    CHECK(fields.choices[1].value == "Explicit");
}

TEST_CASE("a_catalogue_relation_is_read_once_however_many_sets_name_it", "[ore_value_sets]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing a catalogue relation read once";

    std::vector<std::string> asked;
    value_set_tables tables;
    tables.catalogues.push_back(value_set_catalogue_row{"refdata.tenor", "tenors_v"});
    tables.sets.push_back(set_of("setup.tenor", "catalogue", "refdata.tenor"));
    tables.sets.push_back(set_of("curve.tenor", "catalogue", "refdata.tenor"));
    tables.links.push_back(link_of("Setup", "Tenor", "setup.tenor"));
    tables.links.push_back(link_of("Curve", "Tenor", "curve.tenor"));

    value_set_reader sut(std::move(tables), [&asked](std::string_view relation) {
        asked.emplace_back(relation);
        return std::vector<value_choice>{{"1D", "1 day"}, {"", "no code"}};
    });

    CHECK(sut.fields_of("Setup", "Tenor").choices.size() == 1);
    CHECK(sut.fields_of("Curve", "Tenor").choices.size() == 1);
    CHECK(asked.size() == 1);
    CHECK(asked.front() == "tenors_v");

    CHECK(sut.fields_of("Setup", "Tenor").catalogue == "refdata.tenor");
    CHECK(sut.fields_of("Setup", "Tenor").choices.front().label == "1 day");
}

TEST_CASE("a_scope_shorthand_finds_the_word_the_link_table_states", "[ore_value_sets]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing a scope shorthand";

    value_set_reader sut(tables_of(set_of("convention.period", "regex"),
                                   link_of("ModelParameters", "Tenor", "convention.period")),
                         {});

    CHECK(sut.fields_of("model", "Tenor").value_set == "convention.period");
    CHECK(sut.fields_of("ModelParameters", "Tenor").value_set == "convention.period");
}

TEST_CASE("a_setting_the_model_holds_no_set_for_states_no_kind", "[ore_value_sets]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing a setting with no value set";

    value_set_reader sut(
        tables_of(set_of("setup.flag", "boolean"), link_of("Setup", "nThreads", "setup.flag")), {});
    const auto fields = sut.fields_of("Setup", "buildFailedTrades");

    CHECK(fields.value_set.empty());
    CHECK(fields.source_kind.empty());
    CHECK(fields.choices.empty());
    CHECK(!fields.minimum.has_value());
}

TEST_CASE("a_source_kind_this_build_does_not_know_is_refused_by_name", "[ore_value_sets]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing a kind this build does not know";

    value_set_reader sut(
        tables_of(set_of("setup.mask", "magnitude"), link_of("Setup", "logMask", "setup.mask")),
        {});
    const auto sentence = sentence_of(sut, "Setup", "logMask");

    CHECK(sentence.find("setup.mask") != std::string::npos);
    CHECK(sentence.find("magnitude") != std::string::npos);
    CHECK(sentence.find("does not know") != std::string::npos);
}

TEST_CASE("a_catalogue_set_whose_row_is_missing_is_refused", "[ore_value_sets]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing a catalogue that no row holds";

    value_set_reader sut(tables_of(set_of("setup.tenor", "catalogue", "refdata.tenor"),
                                   link_of("Setup", "Tenor", "setup.tenor")),
                         [](std::string_view) { return std::vector<value_choice>{}; });
    const auto sentence = sentence_of(sut, "Setup", "Tenor");

    CHECK(sentence.find("setup.tenor") != std::string::npos);
    CHECK(sentence.find("catalogue") != std::string::npos);
}

TEST_CASE("the_kinds_a_value_set_states_are_the_eight_the_table_uses", "[ore_value_sets]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing the kinds";

    CHECK(source_kinds().size() == 8);
    CHECK(source_kinds().front() == "text");
    CHECK(source_kinds().back() == "boolean");
}
