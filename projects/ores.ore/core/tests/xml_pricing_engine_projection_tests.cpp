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
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */
#include "ores.logging/make_logger.hpp"
#include "ores.ore.core/domain/pricing_engine_projection.hpp"
#include "ores.ore.core/xml/binding.hpp"
#include "ores.ore.core/xml/document_kind.hpp"
#include "ores.ore.core/xml/generated/pricing_engines.hpp"
#include "ores.ore.core/xml/record_diff.hpp"
#include "ores.testing/project_root.hpp"
#include "test_suite.hpp"
#include <boost/uuid/random_generator.hpp>
#include <catch2/catch_test_macros.hpp>
#include <filesystem>
#include <fstream>
#include <stdexcept>
#include <string>

/**
 * @file xml_pricing_engine_projection_tests.cpp
 * @brief The pricing engine document, projected and projected back.
 *
 * The corpus walk proves the whole family. The cases below it prove the three
 * things the projection carries in a column rather than in the document's own
 * shape: a repeated product type, a repeated parameter name, and a parameter
 * that belongs to no product. Each is a shortcut the projection is forbidden
 * from taking, so each gets a case that fails if it is taken.
 */

namespace {

namespace xg = ores::ore::xml::generated::pricing_engines;
using namespace ores::logging;

/**
 * @brief The record tables the projection walks.
 */
struct tables {
    const ores::ore::xml::record_type& root;
    const ores::ore::xml::record_type& product;
    const ores::ore::xml::record_type& parameter;
};

/**
 * @brief States a row the record fills, or refuses to write silently.
 */
void state(const ores::ore::xml::record_type& type, void* record, const std::string_view name) {
    REQUIRE(ores::ore::xml::mark_present(type, record, name));
}

const tables& table() {
    using namespace ores::ore::xml;
    static const tables found = [] {
        const auto& root = *xg::pricing_engines_document().root;
        const auto& product = *find_child_type(root, "Product");
        const auto& parameter =
            *find_child_type(*find_child_type(product, "ModelParameters"), "Parameter");
        return tables{root, product, parameter};
    }();
    return found;
}

xg::parameter make_parameter(const std::string& name, const std::string& value) {
    xg::parameter p;
    p.name = name;
    p.value = value;
    state(table().parameter, &p, "name");
    ores::ore::xml::mark_text_present(table().parameter, &p);
    return p;
}

xg::product
make_product(const std::string& type, const std::string& model, const std::string& engine) {
    xg::product p;
    p.type = type;
    p.model = model;
    p.engine = engine;
    state(table().product, &p, "type");
    state(table().product, &p, "Model");
    state(table().product, &p, "Engine");
    return p;
}

}

TEST_CASE("a_repeated_product_type_keeps_the_order_the_document_wrote", "[ore][xml][roundtrip][pricingengines]") {
    auto lg(make_logger(test_suite));

    xg::pricingengines document;
    document.product.push_back(make_product("CommodityForward", "First", "One"));
    document.product.push_back(make_product("CommodityForward", "Second", "Two"));
    state(table().root, &document, "Product");

    const auto mapped = ores::ore::domain::pricing_engine_projection::map(document);
    REQUIRE(mapped.products.size() == 2);
    CHECK(mapped.products.at(0).position < mapped.products.at(1).position);

    const auto rebuilt = ores::ore::domain::pricing_engine_projection::reverse(mapped);
    REQUIRE(rebuilt.product.size() == 2);
    CHECK(rebuilt.product.at(0).model == "First");
    CHECK(rebuilt.product.at(1).model == "Second");
}

TEST_CASE("a_repeated_parameter_name_in_one_scope_keeps_its_order", "[ore][xml][roundtrip][pricingengines]") {
    auto lg(make_logger(test_suite));

    auto product = make_product("ScriptedTrade", "Scripted", "Scripted");
    product.engine_parameters.parameter.push_back(make_parameter("Interactive", "false"));
    product.engine_parameters.parameter.push_back(make_parameter("Interactive", "true"));
    state(table().product, &product, "EngineParameters");

    xg::pricingengines document;
    document.product.push_back(product);
    state(table().root, &document, "Product");

    const auto mapped = ores::ore::domain::pricing_engine_projection::map(document);
    REQUIRE(mapped.parameters.size() == 2);
    CHECK(mapped.parameters.at(0).parameter_value == "false");
    CHECK(mapped.parameters.at(1).parameter_value == "true");

    const auto rebuilt = ores::ore::domain::pricing_engine_projection::reverse(mapped);
    REQUIRE(rebuilt.product.size() == 1);
    const auto& parameters = rebuilt.product.at(0).engine_parameters.parameter;
    REQUIRE(parameters.size() == 2);
    CHECK(parameters.at(0).value == "false");
    CHECK(parameters.at(1).value == "true");
}

TEST_CASE("a_global_parameter_belongs_to_no_product_and_comes_back", "[ore][xml][roundtrip][pricingengines]") {
    auto lg(make_logger(test_suite));

    xg::pricingengines document;
    document.product.push_back(make_product("Swap", "Discounted", "Discounted"));
    state(table().root, &document, "Product");

    document.global_parameters.parameter.push_back(make_parameter("Calendar", "TARGET"));
    state(*ores::ore::xml::find_child_type(table().root, "GlobalParameters"),
          &document.global_parameters,
          "Parameter");
    state(table().root, &document, "GlobalParameters");

    const auto mapped = ores::ore::domain::pricing_engine_projection::map(document);
    REQUIRE(mapped.parameters.size() == 1);
    CHECK(mapped.parameters.at(0).parameter_scope == "global");
    CHECK_FALSE(mapped.parameters.at(0).pricing_model_product_id);

    const auto rebuilt = ores::ore::domain::pricing_engine_projection::reverse(mapped);
    REQUIRE(rebuilt.global_parameters.parameter.size() == 1);
    CHECK(rebuilt.global_parameters.parameter.at(0).name == "Calendar");
}

TEST_CASE("a_row_the_document_has_no_place_for_is_refused", "[ore][xml][roundtrip][pricingengines]") {
    auto lg(make_logger(test_suite));

    ores::analytics::messaging::pricing_engines_document mapped;
    ores::analytics::domain::pricing_model_product_parameter row;
    row.parameter_name = "Calendar";
    row.parameter_value = "TARGET";
    row.parameter_scope = "nonsense";
    mapped.parameters.push_back(row);
    CHECK_THROWS_AS(ores::ore::domain::pricing_engine_projection::reverse(mapped),
                    std::runtime_error);

    mapped.parameters.front().parameter_scope = "global";
    mapped.parameters.front().pricing_model_product_id = boost::uuids::random_generator()();
    CHECK_THROWS_AS(ores::ore::domain::pricing_engine_projection::reverse(mapped),
                    std::runtime_error);

    mapped.parameters.front().parameter_scope = "model";
    mapped.parameters.front().pricing_model_product_id = boost::uuids::random_generator()();
    CHECK_THROWS_AS(ores::ore::domain::pricing_engine_projection::reverse(mapped),
                    std::runtime_error);
}

TEST_CASE("rows_that_share_a_position_are_ordered_by_id", "[ore][xml][roundtrip][pricingengines]") {
    auto lg(make_logger(test_suite));

    ores::analytics::messaging::pricing_engines_document mapped;
    ores::analytics::domain::pricing_model_product first;
    first.id = boost::uuids::random_generator()();
    first.pricing_engine_type_code = "Swap";
    first.position = 0;
    ores::analytics::domain::pricing_model_product second = first;
    second.id = boost::uuids::random_generator()();
    mapped.products.push_back(first);
    mapped.products.push_back(second);

    const auto rebuilt = ores::ore::domain::pricing_engine_projection::reverse(mapped);
    REQUIRE(rebuilt.product.size() == 2);
    const auto& expected = first.id < second.id ? first : second;
    CHECK(rebuilt.product.at(0).type == expected.pricing_engine_type_code);
}

TEST_CASE("a_document_without_a_global_list_writes_none", "[ore][xml][roundtrip][pricingengines]") {
    auto lg(make_logger(test_suite));

    xg::pricingengines document;
    document.product.push_back(make_product("Swap", "Discounted", "Discounted"));
    state(table().root, &document, "Product");

    const auto mapped = ores::ore::domain::pricing_engine_projection::map(document);
    CHECK(mapped.parameters.empty());

    const auto rebuilt = ores::ore::domain::pricing_engine_projection::reverse(mapped);
    CHECK_FALSE(ores::ore::xml::is_present(table().root, &rebuilt, "GlobalParameters"));
    CHECK_FALSE(rebuilt.product.empty());
}

TEST_CASE("every_pricing_engine_document_in_the_corpus_round_trips", "[ore][xml][roundtrip][pricingengines]") {
    auto lg(make_logger(test_suite));

    const auto root = ores::testing::project_root::resolve("external/ore/examples");
    REQUIRE(std::filesystem::is_directory(root));

    std::size_t files = 0;
    // The writer states every section of the document, so a file that names no
    // GlobalParameters block comes back with an empty one. That is the only
    // difference the round trip may carry, and the counts below pin it: a
    // document that loses anything, or gains anything else, fails.
    std::size_t gained_globals = 0;
    for (const auto& entry : std::filesystem::recursive_directory_iterator(root)) {
        if (!entry.is_regular_file() || entry.path().extension() != ".xml")
            continue;
        if (ores::ore::xml::detect_document_kind(entry.path()) !=
            ores::ore::xml::document_kind::pricing_engines)
            continue;
        ++files;
        INFO(entry.path().string());

        std::string content;
        {
            std::ifstream file(entry.path(), std::ios::binary);
            content.assign(std::istreambuf_iterator<char>(file), std::istreambuf_iterator<char>());
        }

        const auto original = ores::ore::domain::pricing_engine_projection::read(content);
        const auto mapped = ores::ore::domain::pricing_engine_projection::map(original);
        const auto rebuilt = ores::ore::domain::pricing_engine_projection::reverse(mapped);
        const auto exported = ores::ore::domain::pricing_engine_projection::read(
            ores::ore::domain::pricing_engine_projection::write(rebuilt));

        const auto difference = ores::ore::xml::compare_records(table().root, &original, &exported);
        if (!difference)
            continue;
        INFO(difference->path << ": " << difference->detail);
        ++gained_globals;
        // The difference is stated at the record the block sits under, which
        // is the document itself.
        CHECK(difference->path == table().root.name);
        CHECK(difference->detail == "GlobalParameters was added");
        // The block is empty: the parameters it would hold are the ones the
        // rows carry, and the file held none.
        CHECK(exported.global_parameters.parameter.empty());
    }
    BOOST_LOG_SEV(lg, info) << "pricing engine documents: " << files
                            << ", of which gained an empty GlobalParameters: " << gained_globals;
    CHECK(files == 125);
    CHECK(gained_globals == 70);
}
