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
#include "ores.logging/make_logger.hpp"
#include "ores.ore.core/domain/pricing_engines_import.hpp"
#include "ores.ore.core/xml/exporter.hpp"
#include "test_suite.hpp"
#include <boost/uuid/random_generator.hpp>
#include <algorithm>
#include <catch2/catch_test_macros.hpp>
#include <optional>
#include <set>
#include <string>
#include <vector>

namespace {

using ores::analytics::messaging::pricing_engines_document;
using ores::ore::domain::pricing_engines_import;
using ores::ore::domain::pricing_engines_import_file;
using namespace ores::logging;

/** The document ORE reads, as the export writes it for one configuration. */
std::string exported_swap_document() {
    pricing_engines_document document;
    ores::analytics::domain::pricing_model_product swap;
    swap.id = boost::uuids::random_generator()();
    swap.pricing_engine_type_code = "Swap";
    swap.model = "DiscountedCashflows";
    swap.engine = "DiscountingSwapEngine";
    swap.position = 0;
    document.products.push_back(swap);

    // The settings of one scope, in an order no sort would produce, so a
    // document that comes back sorted fails the round trip.
    for (const auto& [position, name] : std::vector<std::pair<int, std::string>>{
             {0, "Training.Sequence"}, {1, "Training.Seed"}, {2, "Pricing.Sequence"}}) {
        ores::analytics::domain::pricing_model_product_parameter setting;
        setting.id = boost::uuids::random_generator()();
        setting.pricing_model_product_id = swap.id;
        setting.parameter_scope = "engine";
        setting.parameter_name = name;
        setting.parameter_value = name + "-value";
        setting.position = position;
        document.parameters.push_back(setting);
    }

    ores::analytics::domain::pricing_model_product_parameter calibration;
    calibration.id = boost::uuids::random_generator()();
    calibration.pricing_model_product_id = swap.id;
    calibration.parameter_scope = "model";
    calibration.parameter_name = "Calibration";
    calibration.parameter_value = "Bootstrap";
    calibration.position = 0;
    document.parameters.push_back(calibration);

    ores::analytics::domain::pricing_model_product_parameter global;
    global.id = boost::uuids::random_generator()();
    global.parameter_scope = "global";
    global.parameter_name = "ContinueOnCalibrationError";
    global.parameter_value = "true";
    global.position = 0;
    document.parameters.push_back(global);

    return ores::ore::xml::exporter::export_pricing_engines(document);
}

/** The product types every seeded vocabulary holds, as the service reads them. */
const std::set<std::string>& vocabulary() {
    static const std::set<std::string> types{"Swap", "FxForward", "Bond", "CrossCurrencySwap"};
    return types;
}

std::size_t issues_of_kind(const pricing_engines_import_file& file, const std::string& kind) {
    return static_cast<std::size_t>(std::ranges::count_if(
        file.issues, [&kind](const auto& issue) { return issue.kind == kind; }));
}

}

TEST_CASE("a_pricing_engine_document_exports_and_imports_back_as_the_same_document",
          "[ore][import][pricing_engines]") {
    auto lg(make_logger(test_suite));

    const auto text = exported_swap_document();
    BOOST_LOG_SEV(lg, debug) << "Exported document:\n" << text;

    const auto file = pricing_engines_import::parse(text);
    BOOST_LOG_SEV(lg, debug) << "Refusal: " << file.refusal;
    REQUIRE(file.ok());
    CHECK(file.root_element == "PricingEngines");
    REQUIRE(file.products.size() == 1);
    CHECK(file.products.front().type_code == "Swap");
    CHECK(file.products.front().model == "DiscountedCashflows");
    CHECK(file.products.front().engine == "DiscountingSwapEngine");
    CHECK(file.products.front().parameters == 4);
    REQUIRE(file.global_parameters.size() == 1);
    CHECK(file.global_parameters.front() == "ContinueOnCalibrationError");

    // Imported into a copy of the configuration it came from, the document
    // comes back as itself.
    const auto target = pricing_engines_import::parse(text).document;
    const auto merged = pricing_engines_import::merge(file, target, {}, target.config.id);

    CHECK(ores::ore::xml::exporter::export_pricing_engines(merged) == text);
}

TEST_CASE("importing_a_pricing_engine_document_refuses_another_documents_root",
          "[ore][import][pricing_engines]") {
    auto lg(make_logger(test_suite));

    const auto file = pricing_engines_import::parse(
        R"(<?xml version="1.0"?><CurrencyConfig><Currency><ISOCode>EUR</ISOCode></Currency>)"
        R"(</CurrencyConfig>)");
    BOOST_LOG_SEV(lg, debug) << "Refusal: " << file.refusal;

    CHECK(!file.ok());
    CHECK(file.root_element == "CurrencyConfig");
    CHECK(file.refusal ==
          "This file is not a pricing engine configuration: its root element is CurrencyConfig. "
          "Choose a PricingEngines file.");
    CHECK(file.document.products.empty());
}

TEST_CASE("importing_a_pricing_engine_document_refuses_text_that_is_not_xml",
          "[ore][import][pricing_engines]") {
    auto lg(make_logger(test_suite));

    const auto file = pricing_engines_import::parse("<PricingEngines><Product>");
    BOOST_LOG_SEV(lg, debug) << "Refusal: " << file.refusal;

    CHECK(!file.ok());
    CHECK(file.root_element.empty());
    CHECK(file.refusal.starts_with("The file could not be read as XML:"));
}

TEST_CASE("a_product_type_the_vocabulary_does_not_hold_is_listed_and_still_written",
          "[ore][import][pricing_engines]") {
    auto lg(make_logger(test_suite));

    const auto file = pricing_engines_import::parse(R"(<?xml version="1.0"?>
<PricingEngines>
  <Product type="CommodityForward">
    <Model>DiscountedCashflows</Model>
    <ModelParameters />
    <Engine>DiscountingCommodityForwardEngine</Engine>
    <EngineParameters />
  </Product>
</PricingEngines>
)");
    REQUIRE(file.ok());

    const auto difference =
        pricing_engines_import::difference(file, pricing_engines_document{}, vocabulary());
    const auto unknown = std::ranges::find_if(
        difference.issues, [](const auto& issue) { return issue.kind == "unknown_type"; });
    REQUIRE(unknown != difference.issues.end());
    CHECK(unknown->subject == "CommodityForward");

    // Reported, not dropped: the product is written as the file states it.
    CHECK(difference.added == 1);
    CHECK(file.document.products.size() == 1);
    CHECK(file.document.products.front().pricing_engine_type_code == "CommodityForward");
}

TEST_CASE("the_elements_the_tables_cannot_hold_are_named", "[ore][import][pricing_engines]") {
    auto lg(make_logger(test_suite));

    const auto file = pricing_engines_import::parse(R"(<?xml version="1.0"?>
<PricingEngines>
  <Product type="Swap" id="swap-1">
    <Model>DiscountedCashflows</Model>
    <ModelParameters />
    <Engine>DiscountingSwapEngine</Engine>
    <EngineParameters />
    <Description>Not a column</Description>
  </Product>
</PricingEngines>
)");
    REQUIRE(file.ok());

    const auto subjects = [&file] {
        std::vector<std::string> found;
        for (const auto& issue : file.issues)
            if (issue.kind == "dropped")
                found.push_back(issue.subject);
        return found;
    }();
    CHECK(subjects == std::vector<std::string>{"Product@id", "Swap#0/Description"});
}

TEST_CASE("a_setting_the_file_states_twice_is_named_and_the_first_stands",
          "[ore][import][pricing_engines]") {
    auto lg(make_logger(test_suite));

    const auto file = pricing_engines_import::parse(R"(<?xml version="1.0"?>
<PricingEngines>
  <Product type="Swap">
    <Model>DiscountedCashflows</Model>
    <ModelParameters />
    <Engine>DiscountingSwapEngine</Engine>
    <EngineParameters>
      <Parameter name="DiscountFactor">First</Parameter>
      <Parameter name="DiscountFactor">Second</Parameter>
    </EngineParameters>
  </Product>
</PricingEngines>
)");
    REQUIRE(file.ok());

    REQUIRE(issues_of_kind(file, "duplicate") == 1);
    REQUIRE(file.document.parameters.size() == 1);
    CHECK(file.document.parameters.front().parameter_value == "First");
}

TEST_CASE("a_duplicate_setting_is_resolved_by_the_person_who_reviews_the_row",
          "[ore][import][pricing_engines]") {
    auto lg(make_logger(test_suite));

    // The target holds the setting; the file states it twice, so the file's
    // reading of it is named as a duplicate and the person decides what happens
    // to the row they are shown.
    pricing_engines_document target;
    target.config.name = "Existing";
    ores::analytics::domain::pricing_model_product swap;
    swap.id = boost::uuids::random_generator()();
    swap.pricing_engine_type_code = "Swap";
    swap.model = "DiscountedCashflows";
    swap.engine = "DiscountingSwapEngine";
    swap.position = 0;
    target.products.push_back(swap);

    ores::analytics::domain::pricing_model_product_parameter discount;
    discount.id = boost::uuids::random_generator()();
    discount.pricing_model_product_id = swap.id;
    discount.parameter_scope = "engine";
    discount.parameter_name = "DiscountFactor";
    discount.parameter_value = "Ours";
    discount.position = 0;
    target.parameters.push_back(discount);

    const auto file = pricing_engines_import::parse(R"(<?xml version="1.0"?>
<PricingEngines>
  <Product type="Swap">
    <Model>DiscountedCashflows</Model>
    <ModelParameters />
    <Engine>DiscountingSwapEngine</Engine>
    <EngineParameters>
      <Parameter name="DiscountFactor">First</Parameter>
      <Parameter name="DiscountFactor">Second</Parameter>
    </EngineParameters>
  </Product>
</PricingEngines>
)");
    REQUIRE(file.ok());
    REQUIRE(issues_of_kind(file, "duplicate") == 1);

    const auto difference = pricing_engines_import::difference(file, target, vocabulary());
    REQUIRE(difference.rows.size() == 1);
    CHECK(difference.rows.front().key == "Swap#0/engine/DiscountFactor");
    CHECK(difference.rows.front().change == "changed");

    // The person takes the file's row: the first statement of the setting is
    // written, and the review is what says so.
    const auto merged = pricing_engines_import::merge(file, target, {}, target.config.id);
    REQUIRE(merged.parameters.size() == 1);
    CHECK(merged.parameters.front().parameter_value == "First");
    CHECK(merged.parameters.front().id == discount.id);
}

TEST_CASE("a_row_the_file_omits_is_kept_unless_the_person_says_otherwise",
          "[ore][import][pricing_engines]") {
    auto lg(make_logger(test_suite));

    pricing_engines_document target;
    ores::analytics::domain::pricing_model_product swap;
    swap.id = boost::uuids::random_generator()();
    swap.pricing_engine_type_code = "Swap";
    swap.model = "DiscountedCashflows";
    swap.engine = "DiscountingSwapEngine";
    swap.position = 0;
    target.products.push_back(swap);

    ores::analytics::domain::pricing_model_product bond;
    bond.id = boost::uuids::random_generator()();
    bond.pricing_engine_type_code = "Bond";
    bond.model = "DiscountedCashflows";
    bond.engine = "DiscountingBondEngine";
    bond.position = 1;
    target.products.push_back(bond);

    ores::analytics::domain::pricing_model_product_parameter yield;
    yield.id = boost::uuids::random_generator()();
    yield.pricing_model_product_id = bond.id;
    yield.parameter_scope = "engine";
    yield.parameter_name = "YieldCurve";
    yield.parameter_value = "EUR";
    yield.position = 0;
    target.parameters.push_back(yield);

    const auto file = pricing_engines_import::parse(R"(<?xml version="1.0"?>
<PricingEngines>
  <Product type="Swap">
    <Model>DiscountedCashflows</Model>
    <ModelParameters />
    <Engine>DiscountingSwapEngine</Engine>
    <EngineParameters />
  </Product>
</PricingEngines>
)");
    REQUIRE(file.ok());

    const auto difference = pricing_engines_import::difference(file, target, vocabulary());
    REQUIRE(difference.rows.size() == 1);
    CHECK(difference.rows.front().key == "Bond#0");
    CHECK(difference.rows.front().change == "removed");
    CHECK(difference.rows.front().what == "Not in the file. It carries 1 setting.");

    // A review the person changes nothing in keeps every removal, and the Bond
    // and the setting it owns stay.
    const auto kept = pricing_engines_import::kept_by_default(difference);
    CHECK(kept == std::vector<std::string>{"Bond#0"});

    const auto defaulted = pricing_engines_import::merge(file, target, kept, target.config.id);
    REQUIRE(defaulted.products.size() == 2);
    CHECK(defaulted.products.back().id == bond.id);
    REQUIRE(defaulted.parameters.size() == 1);
    CHECK(defaulted.parameters.front().id == yield.id);

    // A person who says the Bond goes leaves the document without it, and
    // without the setting it owned.
    const auto dropped = pricing_engines_import::merge(file, target, {}, target.config.id);
    REQUIRE(dropped.products.size() == 1);
    CHECK(dropped.products.front().id == swap.id);
    CHECK(dropped.parameters.empty());
}

TEST_CASE("a_product_the_file_omits_is_one_removal_and_not_one_per_setting",
          "[ore][import][pricing_engines]") {
    auto lg(make_logger(test_suite));

    pricing_engines_document target;
    ores::analytics::domain::pricing_model_product bond;
    bond.id = boost::uuids::random_generator()();
    bond.pricing_engine_type_code = "Bond";
    bond.model = "DiscountedCashflows";
    bond.engine = "DiscountingBondEngine";
    bond.position = 0;
    target.products.push_back(bond);
    for (const auto* name : {"YieldCurve", "DayCounter"}) {
        ores::analytics::domain::pricing_model_product_parameter setting;
        setting.id = boost::uuids::random_generator()();
        setting.pricing_model_product_id = bond.id;
        setting.parameter_scope = "engine";
        setting.parameter_name = name;
        setting.parameter_value = "value";
        setting.position = 0;
        target.parameters.push_back(setting);
    }

    const auto file = pricing_engines_import::parse(R"(<?xml version="1.0"?>
<PricingEngines>
  <Product type="Swap">
    <Model>DiscountedCashflows</Model>
    <ModelParameters />
    <Engine>DiscountingSwapEngine</Engine>
    <EngineParameters />
  </Product>
</PricingEngines>
)");
    REQUIRE(file.ok());

    const auto difference = pricing_engines_import::difference(file, target, vocabulary());
    const auto removals = [&difference] {
        std::vector<std::string> keys;
        for (const auto& row : difference.rows)
            if (row.change == "removed")
                keys.push_back(row.key);
        return keys;
    }();
    CHECK(removals == std::vector<std::string>{"Bond#0"});

    // Kept, the product brings its two settings back with it; dropped, it takes
    // them with it, because the save writes the document whole.
    CHECK(pricing_engines_import::merge(file, target, {"Bond#0"}, target.config.id)
              .parameters.size() == 2);
    CHECK(pricing_engines_import::merge(file, target, {}, target.config.id).parameters.empty());
}

TEST_CASE("a_setting_the_file_omits_is_a_removal_of_its_own", "[ore][import][pricing_engines]") {
    auto lg(make_logger(test_suite));

    pricing_engines_document target;
    ores::analytics::domain::pricing_model_product swap;
    swap.id = boost::uuids::random_generator()();
    swap.pricing_engine_type_code = "Swap";
    swap.model = "DiscountedCashflows";
    swap.engine = "DiscountingSwapEngine";
    swap.position = 0;
    target.products.push_back(swap);

    ores::analytics::domain::pricing_model_product_parameter curve;
    curve.id = boost::uuids::random_generator()();
    curve.pricing_model_product_id = swap.id;
    curve.parameter_scope = "engine";
    curve.parameter_name = "DiscountingCurve";
    curve.parameter_value = "EUR-OIS";
    curve.position = 0;
    target.parameters.push_back(curve);

    const auto file = pricing_engines_import::parse(R"(<?xml version="1.0"?>
<PricingEngines>
  <Product type="Swap">
    <Model>DiscountedCashflows</Model>
    <ModelParameters />
    <Engine>DiscountingSwapEngine</Engine>
    <EngineParameters />
  </Product>
</PricingEngines>
)");
    REQUIRE(file.ok());

    const auto difference = pricing_engines_import::difference(file, target, vocabulary());
    REQUIRE(difference.rows.size() == 1);
    CHECK(difference.rows.front().key == "Swap#0/engine/DiscountingCurve");
    CHECK(difference.rows.front().change == "removed");

    CHECK(pricing_engines_import::merge(
              file, target, {"Swap#0/engine/DiscountingCurve"}, target.config.id)
              .parameters.size() == 1);
    CHECK(pricing_engines_import::merge(file, target, {}, target.config.id).parameters.empty());
}

TEST_CASE("an_imported_row_belongs_to_the_configuration_it_was_written_into",
          "[ore][import][pricing_engines]") {
    auto lg(make_logger(test_suite));

    // The parser stamps its rows with the id it minted for the configuration it
    // read. A row written under that id would belong to a configuration that
    // does not exist, and the analytics save writes the rows it is given as they
    // stand.
    const auto file = pricing_engines_import::parse(exported_swap_document());
    REQUIRE(file.ok());
    const auto file_configuration = file.document.config.id;
    CHECK(file.document.products.front().pricing_model_config_id == file_configuration);
    CHECK(file.document.parameters.front().pricing_model_config_id == file_configuration);

    pricing_engines_document target;
    target.config.id = boost::uuids::random_generator()();
    target.config.name = "Existing";
    ores::analytics::domain::pricing_model_product swap;
    swap.id = boost::uuids::random_generator()();
    swap.pricing_model_config_id = target.config.id;
    swap.pricing_engine_type_code = "Swap";
    swap.model = "DiscountedCashflows";
    swap.engine = "DiscountingSwapEngine";
    swap.position = 0;
    target.products.push_back(swap);

    // Into a configuration that exists, every row belongs to it.
    const auto into_target = pricing_engines_import::merge(file, target, {}, target.config.id);
    CHECK(into_target.config.id == target.config.id);
    REQUIRE(into_target.products.size() == 1);
    CHECK(into_target.products.front().pricing_model_config_id == target.config.id);
    REQUIRE(into_target.parameters.size() == 5);
    for (const auto& parameter : into_target.parameters)
        CHECK(parameter.pricing_model_config_id == target.config.id);

    // Into a new one, every row belongs to the configuration the caller is about
    // to create, which is the id the parser minted and the caller states.
    const auto new_configuration = boost::uuids::random_generator()();
    const auto into_new = pricing_engines_import::merge(file, {}, {}, new_configuration);
    CHECK(into_new.config.id == new_configuration);
    CHECK(into_new.products.front().pricing_model_config_id == new_configuration);
    for (const auto& parameter : into_new.parameters)
        CHECK(parameter.pricing_model_config_id == new_configuration);
}

TEST_CASE("a_document_larger_than_the_import_reads_is_refused_by_its_size",
          "[ore][import][pricing_engines]") {
    auto lg(make_logger(test_suite));

    const auto padding = std::string(pricing_engines_import::maximum_content + 1 - 64, ' ');
    const auto file = pricing_engines_import::parse("<PricingEngines>" + std::string(64, ' ') +
                                                    padding + "</PricingEngines>");
    BOOST_LOG_SEV(lg, debug) << "Refusal: " << file.refusal;

    CHECK(!file.ok());
    CHECK(file.refusal.starts_with("The file is "));
    CHECK(file.refusal.find("bytes") != std::string::npos);
    CHECK(file.document.products.empty());
}

TEST_CASE("a_product_key_is_the_type_and_the_place_of_that_type",
          "[ore][import][pricing_engines]") {
    auto lg(make_logger(test_suite));

    // The review names every row by these keys, and a refused row is named by the
    // same builder, so the two cannot drift.
    const auto file = pricing_engines_import::parse(R"(<?xml version="1.0"?>
<PricingEngines>
  <Product type="Swap">
    <Model>M</Model>
    <ModelParameters />
    <Engine>E</Engine>
    <EngineParameters />
  </Product>
  <Product type="Bond">
    <Model>M</Model>
    <ModelParameters />
    <Engine>E</Engine>
    <EngineParameters />
  </Product>
  <Product type="Swap">
    <Model>M</Model>
    <ModelParameters />
    <Engine>E</Engine>
    <EngineParameters />
  </Product>
</PricingEngines>
)");
    REQUIRE(file.ok());

    const auto keys = pricing_engines_import::product_keys(file.document);
    REQUIRE(file.document.products.size() == 3);
    CHECK(keys.at(file.document.products[0].id) == "Swap#0");
    CHECK(keys.at(file.document.products[1].id) == "Bond#0");
    CHECK(keys.at(file.document.products[2].id) == "Swap#1");
}
