/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2025 Marco Craveiro <marco.craveiro@gmail.com>
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
#include "ores.logging/make_logger.hpp"
#include "ores.ore.core/domain/domain.hpp"
#include "ores.testing/project_root.hpp"
#include <catch2/catch_test_macros.hpp>
#include <filesystem>

namespace {

const std::string_view test_suite("ores.ore.tests");
const std::string tags("[ore][xml][file_io]");

std::filesystem::path ore_path(const std::string& relative) {
    return ores::testing::project_root::resolve("external/ore/" + relative);
}

using namespace ores::logging;

}

// =============================================================================
// load_file / save_data / load_data roundtrip tests
// =============================================================================

TEST_CASE("currency_config_load_file_save_file_roundtrip", tags) {
    auto lg(make_logger(test_suite));

    using ores::ore::domain::currencyConfig;

    const auto input = ore_path("examples/Input/currencies.xml");
    BOOST_LOG_SEV(lg, debug) << "Loading from file: " << input;

    currencyConfig original;
    ores::ore::domain::load_file(input.string(), original);
    REQUIRE(original.Currency.size() == 179);

    const std::string serialised = ores::ore::domain::save_data(original);

    currencyConfig reloaded;
    ores::ore::domain::load_data(serialised, reloaded);
    CHECK(reloaded.Currency.size() == original.Currency.size());

    BOOST_LOG_SEV(lg, info) << "File I/O roundtrip passed for currencyConfig";
}

TEST_CASE("simulation_load_file_save_file_roundtrip", tags) {
    auto lg(make_logger(test_suite));

    using ores::ore::domain::simulation;

    const auto input = ore_path("examples/ORE-API/Input/simulation.xml");
    BOOST_LOG_SEV(lg, debug) << "Loading from file: " << input;

    simulation original;
    ores::ore::domain::load_file(input.string(), original);

    const std::string serialised = ores::ore::domain::save_data(original);

    simulation reloaded;
    ores::ore::domain::load_data(serialised, reloaded);

    CHECK(static_cast<bool>(reloaded.Parameters) == static_cast<bool>(original.Parameters));
    CHECK(static_cast<bool>(reloaded.CrossAssetModel) ==
          static_cast<bool>(original.CrossAssetModel));

    BOOST_LOG_SEV(lg, info) << "File I/O roundtrip passed for simulation";
}

TEST_CASE("todaysmarket_load_file_save_file_roundtrip", tags) {
    auto lg(make_logger(test_suite));

    using ores::ore::domain::todaysmarket;

    const auto input = ore_path("examples/ORE-API/Input/todaysmarket.xml");
    BOOST_LOG_SEV(lg, debug) << "Loading from file: " << input;

    todaysmarket original;
    ores::ore::domain::load_file(input.string(), original);
    REQUIRE(!original.Configuration.empty());

    const std::string serialised = ores::ore::domain::save_data(original);

    todaysmarket reloaded;
    ores::ore::domain::load_data(serialised, reloaded);
    CHECK(reloaded.Configuration.size() == original.Configuration.size());

    BOOST_LOG_SEV(lg, info) << "File I/O roundtrip passed for todaysmarket";
}

// =============================================================================
// Entity and character references in element text
// =============================================================================

namespace {

ores::ore::domain::correlationsType_Correlation_t only_correlation(const std::string& xml) {
    ores::ore::domain::todaysmarket doc;
    ores::ore::domain::load_data(xml, doc);
    REQUIRE(doc.Correlations.size() == 1);
    REQUIRE(doc.Correlations.front().Correlation.size() == 1);
    return doc.Correlations.front().Correlation.front();
}

std::string correlation_document(const std::string& name, const std::string& text) {
    return "<TodaysMarket><Correlations id=\"default\"><Correlation name=\"" + name + "\">" + text +
           "</Correlation></Correlations></TodaysMarket>";
}

std::string text_of(const ores::ore::domain::correlationsType_Correlation_t& c) {
    return static_cast<const xsd::string&>(c);
}

}

TEST_CASE("element_text_decodes_entity_references_as_attributes_do", tags) {
    auto lg(make_logger(test_suite));
    const auto c = only_correlation(correlation_document("A&amp;B", "Correlation/A&amp;B"));
    CHECK(std::string(c.name) == "A&B");
    CHECK(text_of(c) == "Correlation/A&B");
    BOOST_LOG_SEV(lg, info) << "Element text decoded: " << text_of(c);
}

TEST_CASE("element_text_decodes_every_predefined_and_numeric_reference", tags) {
    auto lg(make_logger(test_suite));
    const auto c =
        only_correlation(correlation_document("x", "&lt;&gt;&amp;&quot;&apos;&#38;&#x26;"));
    CHECK(text_of(c) == "<>&\"'&&");
    BOOST_LOG_SEV(lg, info) << "Every reference decoded: " << text_of(c);
}

TEST_CASE("element_text_survives_any_number_of_save_and_load_cycles", tags) {
    auto lg(make_logger(test_suite));
    using ores::ore::domain::todaysmarket;
    todaysmarket first;
    ores::ore::domain::load_data(correlation_document("A&amp;B", "Correlation/A&amp;B"), first);

    const std::string once = ores::ore::domain::save_data(first);
    todaysmarket second;
    ores::ore::domain::load_data(once, second);
    const std::string twice = ores::ore::domain::save_data(second);

    CHECK(once == twice);
    CHECK(text_of(second.Correlations.front().Correlation.front()) == "Correlation/A&B");
    BOOST_LOG_SEV(lg, info) << "Stable after two cycles: " << twice;
}

TEST_CASE("element_text_leaves_cdata_content_undecoded", tags) {
    auto lg(make_logger(test_suite));
    const auto c = only_correlation(correlation_document("x", "a&amp;<![CDATA[&amp;]]>b"));
    CHECK(text_of(c) == "a&&amp;b");
    BOOST_LOG_SEV(lg, info) << "CDATA kept as written: " << text_of(c);
}

TEST_CASE("an_entity_name_is_matched_exactly_not_by_prefix", tags) {
    auto lg(make_logger(test_suite));
    const auto c = only_correlation(correlation_document("&a;", "&a;&;&ampx;"));
    CHECK(std::string(c.name) == "&a;");
    CHECK(text_of(c) == "&a;&;&ampx;");
    BOOST_LOG_SEV(lg, info) << "Unknown names kept as written: " << text_of(c);
}
