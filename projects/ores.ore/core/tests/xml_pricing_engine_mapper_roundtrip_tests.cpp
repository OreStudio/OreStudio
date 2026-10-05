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
#include "ores.ore.core/domain/domain.hpp"
#include "ores.ore.core/domain/pricing_engine_mapper.hpp"
#include "ores.ore.core/xml/roundtrip_harness.hpp"
#include "ores.testing/project_root.hpp"
#include <catch2/catch_test_macros.hpp>
#include <filesystem>
#include <stdexcept>
#include <string>
#include <utility>

/**
 * @file xml_pricing_engine_mapper_roundtrip_tests.cpp
 * @brief The pricing engine document, mapped and mapped back.
 *
 * The walk proves the whole corpus, and the cases below it prove the three
 * things the mapper carries in a column rather than in the document's own
 * shape: a repeated product type, a repeated parameter name, and a parameter
 * that belongs to no product. Each of those is a shortcut the mapper is
 * forbidden from taking, so each gets a case that fails if it is taken. The
 * last two cover rows that did not come from a document: rows that share a
 * position, and rows the document has no place for.
 */

namespace {

const std::string tags("[ore][xml][roundtrip][pricingengines]");

std::filesystem::path corpus_root() {
    return ores::testing::project_root::resolve("external/ore/examples");
}

ores::ore::xml::roundtrip_kind pricing_engines_kind() {
    return ores::ore::xml::make_roundtrip_kind<
        ores::ore::domain::pricingengines,
        ores::analytics::messaging::pricing_engines_document>(
        "pricing engines",
        "pricingengine",
        &ores::ore::domain::pricing_engine_mapper::map,
        &ores::ore::domain::pricing_engine_mapper::reverse,
        ores::ore::xml::parsed_text_difference<ores::ore::domain::pricingengines>);
}

// The generated text elements derive from xsd::string without inheriting its
// constructors, so the base subobject is the only assignment target a
// std::string converts to.
ores::ore::domain::product
make_product(const std::string& type, const std::string& model, const std::string& engine) {
    using namespace ores::ore::domain;
    product built;
    built.type = type;
    static_cast<xsd::string&>(built.Model) = model;
    static_cast<xsd::string&>(built.Engine) = engine;
    return built;
}

ores::ore::domain::parameter make_parameter(const std::string& name, const std::string& value) {
    using namespace ores::ore::domain;
    parameter built;
    built.name = name;
    static_cast<xsd::string&>(built) = value;
    return built;
}

}

TEST_CASE("every pricing engine document round trips through the entities", tags) {
    const auto walk = ores::ore::xml::walk_kind(pricing_engines_kind(), corpus_root());

    INFO("files: " << walk.files << ", passed: " << walk.passed);
    for (const auto& failure : walk.failures)
        INFO(failure);

    CHECK(walk.files > 0);
    CHECK(walk.passed == walk.files);
}

TEST_CASE("a repeated product type keeps the order the document wrote", tags) {
    using namespace ores::ore::domain;

    pricingengines document;
    document.Product.push_back(make_product("CommodityForward", "First", "One"));
    document.Product.push_back(make_product("CommodityForward", "Second", "Two"));

    const auto mapped = pricing_engine_mapper::map(document);
    REQUIRE(mapped.products.size() == 2);
    CHECK(mapped.products.at(0).position < mapped.products.at(1).position);

    const auto rebuilt = pricing_engine_mapper::reverse(mapped);
    REQUIRE(rebuilt.Product.size() == 2);
    CHECK(std::string(rebuilt.Product.at(0).Model) == "First");
    CHECK(std::string(rebuilt.Product.at(1).Model) == "Second");
}

TEST_CASE("a repeated parameter name in one scope keeps its order", tags) {
    using namespace ores::ore::domain;

    auto product = make_product("ScriptedTrade", "Scripted", "Scripted");
    product.EngineParameters.Parameter.push_back(make_parameter("Interactive", "false"));
    product.EngineParameters.Parameter.push_back(make_parameter("Interactive", "true"));

    pricingengines document;
    document.Product.push_back(product);

    const auto mapped = pricing_engine_mapper::map(document);
    REQUIRE(mapped.parameters.size() == 2);
    CHECK(mapped.parameters.at(0).parameter_value == "false");
    CHECK(mapped.parameters.at(1).parameter_value == "true");

    const auto rebuilt = pricing_engine_mapper::reverse(mapped);
    REQUIRE(rebuilt.Product.size() == 1);
    const auto& parameters = rebuilt.Product.at(0).EngineParameters.Parameter;
    REQUIRE(parameters.size() == 2);
    CHECK(std::string(parameters.at(0)) == "false");
    CHECK(std::string(parameters.at(1)) == "true");
}

TEST_CASE("a global parameter belongs to no product and comes back", tags) {
    using namespace ores::ore::domain;

    pricingengines document;
    document.Product.push_back(
        make_product("Swap", "DiscountedCashflows", "DiscountingSwapEngine"));

    globalParameters global;
    global.Parameter.push_back(make_parameter("ContinueOnError", "Y"));
    document.GlobalParameters = global;

    const auto mapped = pricing_engine_mapper::map(document);
    REQUIRE(mapped.parameters.size() == 1);
    CHECK(mapped.parameters.at(0).parameter_scope == "global");
    CHECK(!mapped.parameters.at(0).pricing_model_product_id);

    const auto rebuilt = pricing_engine_mapper::reverse(mapped);
    REQUIRE(static_cast<bool>(rebuilt.GlobalParameters));
    REQUIRE(rebuilt.GlobalParameters->Parameter.size() == 1);
    CHECK(std::string(rebuilt.GlobalParameters->Parameter.at(0).name) == "ContinueOnError");
    CHECK(std::string(rebuilt.GlobalParameters->Parameter.at(0)) == "Y");
}

TEST_CASE("a document without global parameters comes back without them", tags) {
    using namespace ores::ore::domain;

    pricingengines document;
    document.Product.push_back(
        make_product("Swap", "DiscountedCashflows", "DiscountingSwapEngine"));

    const auto rebuilt = pricing_engine_mapper::reverse(pricing_engine_mapper::map(document));
    CHECK(!static_cast<bool>(rebuilt.GlobalParameters));
}

TEST_CASE("rows that share a position come back in id order", tags) {
    using namespace ores::ore::domain;

    pricingengines document;
    document.Product.push_back(make_product("Swap", "First", "One"));
    document.Product.push_back(make_product("Swap", "Second", "Two"));

    // A row written through the shell carries position 0 whatever its place,
    // so the export falls back to the id. Whichever input order arrives, the
    // product with the smaller id comes first.
    auto mapped = pricing_engine_mapper::map(document);
    REQUIRE(mapped.products.size() == 2);
    for (auto& product : mapped.products)
        product.position = 0;
    const auto& lower = mapped.products.at(0).id < mapped.products.at(1).id ?
                            mapped.products.at(0) :
                            mapped.products.at(1);
    const std::string expected_first = lower.model;

    auto swapped = mapped;
    std::swap(swapped.products.at(0), swapped.products.at(1));

    for (const auto& input : {mapped, swapped}) {
        const auto rebuilt = pricing_engine_mapper::reverse(input);
        REQUIRE(rebuilt.Product.size() == 2);
        CHECK(std::string(rebuilt.Product.at(0).Model) == expected_first);
    }
}

TEST_CASE("a parameter row the document has no place for is an error", tags) {
    using namespace ores::ore::domain;

    auto product = make_product("Swap", "DiscountedCashflows", "DiscountingSwapEngine");
    product.EngineParameters.Parameter.push_back(make_parameter("Interactive", "false"));
    pricingengines document;
    document.Product.push_back(product);
    const auto mapped = pricing_engine_mapper::map(document);
    REQUIRE(mapped.parameters.size() == 1);

    auto unknown_scope = mapped;
    unknown_scope.parameters.at(0).parameter_scope = "trade";
    CHECK_THROWS_AS(pricing_engine_mapper::reverse(unknown_scope), std::runtime_error);

    auto global_with_product = mapped;
    global_with_product.parameters.at(0).parameter_scope = "global";
    CHECK_THROWS_AS(pricing_engine_mapper::reverse(global_with_product), std::runtime_error);

    auto orphan = mapped;
    orphan.products.clear();
    CHECK_THROWS_AS(pricing_engine_mapper::reverse(orphan), std::runtime_error);
}
