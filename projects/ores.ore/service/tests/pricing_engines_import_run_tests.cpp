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
#include "ores.ore.service/messaging/pricing_engines_import_run.hpp"
#include "test_suite.hpp"
#include <boost/uuid/random_generator.hpp>
#include <boost/uuid/uuid.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <catch2/catch_test_macros.hpp>
#include <set>
#include <stdexcept>
#include <string>
#include <vector>

namespace {

using namespace ores::logging;
using ores::analytics::messaging::pricing_engines_document;
using ores::analytics::messaging::save_pricing_engines_document_response;
using ores::ore::service::messaging::pricing_engines_import_operations;
using ores::ore::service::messaging::run_pricing_engines_import;
using ores::utility::domain::change_intent;

/** A document ORE reads, as a file states it. */
std::string document_text() {
    return R"(<?xml version="1.0"?>
<PricingEngines>
  <Product type="Swap">
    <Model>DiscountedCashflows</Model>
    <ModelParameters />
    <Engine>DiscountingSwapEngine</Engine>
    <EngineParameters>
      <Parameter name="DiscountFactor">MersenneTwister</Parameter>
    </EngineParameters>
  </Product>
  <GlobalParameters>
    <Parameter name="ContinueOnCalibrationError">true</Parameter>
  </GlobalParameters>
</PricingEngines>
)";
}

/** The vocabulary every case reads, unless the case makes the read fail. */
const std::set<std::string>& vocabulary() {
    static const std::set<std::string> types{"Swap", "Bond"};
    return types;
}

/**
 * The world an import runs in, with each step of it a fake: the configurations
 * it registers, and the ones it removed again.
 */
struct world {
    /** The configurations reporting holds, by name. */
    std::set<std::string> live;
    std::vector<std::string> registered;
    std::vector<std::string> removed;

    /** The document an existing target answers with. */
    pricing_engines_document target;
    bool target_unreadable = false;
    bool vocabulary_unreadable = false;
    bool save_refused = false;
    bool save_threw = false;
    bool saved = false;

    /** The rows the last save was given, for the cases that read them. */
    pricing_engines_document last_save;

    world() {
        target.config.id = boost::uuids::random_generator()();
        target.config.name = "Existing";
        target.config.configuration_id = boost::uuids::random_generator()();
    }

    pricing_engines_import_operations operations() {
        return {
            .known_types =
                [this] {
                    if (vocabulary_unreadable)
                        throw std::runtime_error("the pricing engine types could not be read");
                    return vocabulary();
                },
            .read_target =
                [this](const std::string& id, std::string& error) {
                    if (id.empty())
                        return pricing_engines_document{};
                    if (target_unreadable) {
                        error = "No pricing engines document fills configuration " + id + ".";
                        return pricing_engines_document{};
                    }
                    return target;
                },
            .register_configuration =
                [this](const std::string& name, const change_intent&) {
                    live.insert(name);
                    registered.push_back(name);
                    return boost::uuids::random_generator()();
                },
            .remove_configuration =
                [this](const std::string& name, const change_intent&) {
                    removed.push_back(name);
                    live.erase(name);
                },
            .save =
                [this](pricing_engines_document document) {
                    saved = true;
                    last_save = std::move(document);
                    if (save_threw)
                        throw std::runtime_error("the pricing engines document could not be saved");
                    if (save_refused)
                        return save_pricing_engines_document_response{
                            .message = "The document has 1 problem.",
                            .problems = {{.code = "parameter_value_missing",
                                          .message = "The setting DiscountFactor has no value."}}};
                    return save_pricing_engines_document_response{
                        .success = true, .id = boost::uuids::to_string(document.config.id)};
                },
        };
    }
};

ores::ore::messaging::import_pricing_engines_request
new_configuration(const std::string& name = "From a test") {
    return {.content = document_text(),
            .new_configuration_name = name,
            .new_configuration_variant = "test",
            .reason_code = "system.test",
            .commentary = "A test drives the import"};
}

ores::ore::messaging::import_pricing_engines_request
into(const boost::uuids::uuid& configuration_id) {
    auto request = new_configuration();
    request.target_configuration_id = boost::uuids::to_string(configuration_id);
    request.new_configuration_name.clear();
    return request;
}

}

TEST_CASE("a_vocabulary_that_cannot_be_read_registers_no_configuration", "[ore][import][run]") {
    auto lg(make_logger(test_suite));

    world w;
    w.vocabulary_unreadable = true;
    const auto response = run_pricing_engines_import(w.operations(), new_configuration());
    BOOST_LOG_SEV(lg, debug) << "Refusal: " << response.message;

    CHECK_FALSE(response.success);
    CHECK(response.message.find("could not be read") != std::string::npos);
    // The read that failed comes before anything is registered, so there is
    // nothing to undo and nothing left behind.
    CHECK(w.registered.empty());
    CHECK(w.removed.empty());
    CHECK(w.live.empty());
    CHECK_FALSE(w.saved);
}

TEST_CASE("a_target_that_cannot_be_read_registers_no_configuration", "[ore][import][run]") {
    auto lg(make_logger(test_suite));

    world w;
    w.target_unreadable = true;
    const auto response =
        run_pricing_engines_import(w.operations(), into(boost::uuids::random_generator()()));
    BOOST_LOG_SEV(lg, debug) << "Refusal: " << response.message;

    CHECK_FALSE(response.success);
    CHECK(response.message.find("No pricing engines document fills configuration") !=
          std::string::npos);
    CHECK(w.registered.empty());
    CHECK(w.live.empty());
}

TEST_CASE("a_refused_save_removes_the_configuration_the_import_registered", "[ore][import][run]") {
    auto lg(make_logger(test_suite));

    world w;
    w.save_refused = true;
    const auto response = run_pricing_engines_import(w.operations(), new_configuration());
    BOOST_LOG_SEV(lg, debug) << "Refusal: " << response.message;

    CHECK_FALSE(response.success);
    CHECK(response.message == "The document has 1 problem.");
    // The row the save refused is named by the key the review uses.
    REQUIRE(response.errors.size() == 1);
    CHECK(response.errors.front().key == "");
    CHECK(response.errors.front().message == "The setting DiscountFactor has no value.");

    // The configuration the run registered is gone again.
    REQUIRE(w.registered.size() == 1);
    CHECK(w.removed == w.registered);
    CHECK(w.live.empty());
}

TEST_CASE("a_save_that_throws_removes_the_configuration_the_import_registered",
          "[ore][import][run]") {
    auto lg(make_logger(test_suite));

    world w;
    w.save_threw = true;
    const auto response = run_pricing_engines_import(w.operations(), new_configuration());
    BOOST_LOG_SEV(lg, debug) << "Refusal: " << response.message;

    CHECK_FALSE(response.success);
    CHECK(response.message == "the pricing engines document could not be saved");
    REQUIRE(w.registered.size() == 1);
    CHECK(w.removed == w.registered);
    CHECK(w.live.empty());
}

TEST_CASE("an_import_into_a_configuration_that_exists_registers_nothing", "[ore][import][run]") {
    auto lg(make_logger(test_suite));

    world w;
    // The target is named by the reporting configuration the record fills, which
    // is what analytics finds a document by.
    const auto response =
        run_pricing_engines_import(w.operations(), into(w.target.config.configuration_id));
    BOOST_LOG_SEV(lg, debug) << "Answer: " << response.message;

    CHECK(response.success);
    CHECK(w.registered.empty());
    CHECK(w.live.empty());
    // Every row the save is given belongs to the configuration it fills, and the
    // configuration answers the reporting configuration the target names.
    REQUIRE(w.saved);
    CHECK(w.last_save.config.id == w.target.config.id);
    CHECK(w.last_save.config.configuration_id == w.target.config.configuration_id);
    REQUIRE(w.last_save.products.size() == 1);
    CHECK(w.last_save.products.front().pricing_model_config_id == w.target.config.id);
    for (const auto& parameter : w.last_save.parameters)
        CHECK(parameter.pricing_model_config_id == w.target.config.id);
    // The reason travels on the rows, which is where the save keeps one.
    CHECK(w.last_save.products.front().change_reason_code == "system.test");
    CHECK(w.last_save.products.front().change_commentary == "A test drives the import");
}

TEST_CASE("a_new_configuration_is_named_and_counted_as_the_file_states_it", "[ore][import][run]") {
    auto lg(make_logger(test_suite));

    world w;
    const auto response = run_pricing_engines_import(w.operations(), new_configuration());
    BOOST_LOG_SEV(lg, debug) << "Answer: " << response.message;

    CHECK(response.success);
    CHECK(response.configuration_name == "From a test");
    CHECK(response.added == 3);
    CHECK(response.changed == 0);
    CHECK(response.deleted == 0);
    CHECK(response.kept == 0);
    CHECK(response.message == "Imported: 3 added, 0 changed, 0 deleted, 0 kept.");
    REQUIRE(w.registered.size() == 1);
    CHECK(w.removed.empty());
    CHECK(w.live.size() == 1);
    // The rows belong to the configuration the run registered for them.
    REQUIRE(w.saved);
    CHECK(w.last_save.config.name == "From a test");
    CHECK(w.last_save.config.config_variant == "test");
    CHECK(w.last_save.products.front().pricing_model_config_id == w.last_save.config.id);
}
