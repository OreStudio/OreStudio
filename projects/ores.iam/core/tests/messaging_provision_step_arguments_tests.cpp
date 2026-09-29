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
#include "ores.iam.api/workflow/provision_tenant_workflow.hpp"
#include "ores.iam.core/messaging/provision_step_arguments.hpp"
#include <catch2/catch_test_macros.hpp>
#include <catch2/matchers/catch_matchers_string.hpp>
#include <stdexcept>
#include <string>
#include <utility>
#include <vector>

namespace {

const std::string tags("[provision_step]");

using ores::iam::messaging::classify_step_kind;
using ores::iam::messaging::parse_lei_hierarchy_arguments;
using ores::iam::messaging::parse_market_feed_arguments;
using ores::iam::messaging::parse_photo_arguments;
using ores::iam::messaging::parse_staff_assignments;
using ores::iam::messaging::parse_step_bundles;
using ores::iam::messaging::provision_step_action;

std::vector<ores::iam::workflow::provision_tenant_parameter> parameters(std::string name,
                                                                        std::string value) {
    return {ores::iam::workflow::provision_tenant_parameter{.name = std::move(name),
                                                            .value = std::move(value)}};
}

}

TEST_CASE("the executor classifies each kind it executes into its own action", tags) {
    CHECK(classify_step_kind("publish_bundle") == provision_step_action::publish_bundle);
    CHECK(classify_step_kind("import_lei_hierarchy") ==
          provision_step_action::import_lei_hierarchy);
    CHECK(classify_step_kind("provision_party") == provision_step_action::provision_party);
    CHECK(classify_step_kind("load_staff") == provision_step_action::load_staff);
    CHECK(classify_step_kind("attach_photos") == provision_step_action::attach_photos);
    CHECK(classify_step_kind("start_market_feeds") == provision_step_action::start_market_feeds);
    CHECK(classify_step_kind("complete_provisioning") ==
          provision_step_action::complete_provisioning);
}

TEST_CASE("the executor refuses every kind it does not know", tags) {
    CHECK(classify_step_kind("publish_everything") == provision_step_action::refuse);
    CHECK(classify_step_kind("") == provision_step_action::refuse);
    CHECK(classify_step_kind("complete_provisioning_x") == provision_step_action::refuse);
}

TEST_CASE("a step's bundles are read in the order the step names them", tags) {
    const auto bundles = parse_step_bundles(R"({"bundles": ["base", "risk_management"]})");

    CHECK(bundles == std::vector<std::string>{"base", "risk_management"});
}

TEST_CASE("a step that names no bundles argument is refused", tags) {
    CHECK_THROWS_WITH(parse_step_bundles("{}"), "The step names no 'bundles' argument.");
    CHECK_THROWS_WITH(parse_step_bundles(""), "The step names no 'bundles' argument.");
}

TEST_CASE("a step that names an empty bundles list is refused", tags) {
    CHECK_THROWS_WITH(parse_step_bundles(R"({"bundles": []})"),
                      "The step names no bundle in its 'bundles' argument.");
}

TEST_CASE("a step whose bundles argument is not a list of strings is refused", tags) {
    CHECK_THROWS_WITH(parse_step_bundles(R"({"bundles": "base"})"),
                      "The step's argument 'bundles' is not a list of strings.");
    CHECK_THROWS_WITH(parse_step_bundles(R"({"bundles": ["base", 7]})"),
                      "The step's argument 'bundles' is not a list of strings.");
}

TEST_CASE("a step whose arguments are not a JSON object is refused", tags) {
    CHECK_THROWS_WITH(parse_step_bundles("not json"), "The step's arguments are not JSON.");
    CHECK_THROWS_WITH(parse_step_bundles("[1, 2]"), "The step's arguments are not a JSON object.");
}

TEST_CASE("an LEI import reads its bundle and its root LEI from the step's arguments", tags) {
    const auto arguments = parse_lei_hierarchy_arguments(
        R"({"bundles": ["acme_lei_import"], "root_lei": "9695ACMEGROUP0000030"})", {});

    CHECK(arguments.bundles == std::vector<std::string>{"acme_lei_import"});
    CHECK(arguments.root_lei == "9695ACMEGROUP0000030");
}

TEST_CASE("an LEI import takes the root LEI from the run's parameters when the step states "
          "none",
          tags) {
    const auto arguments = parse_lei_hierarchy_arguments(
        R"({"bundles": ["lei_hierarchy"]})", parameters("root_lei", "529900T8BM49AURSDO55"));

    CHECK(arguments.root_lei == "529900T8BM49AURSDO55");
}

TEST_CASE("an LEI import prefers the step's own root LEI to the run's parameter", tags) {
    const auto arguments = parse_lei_hierarchy_arguments(
        R"({"bundles": ["lei_hierarchy"], "root_lei": "9695ACMEGROUP0000030"})",
        parameters("root_lei", "529900T8BM49AURSDO55"));

    CHECK(arguments.root_lei == "9695ACMEGROUP0000030");
}

TEST_CASE("an LEI import that names no bundle is refused", tags) {
    CHECK_THROWS_WITH(parse_lei_hierarchy_arguments(R"({"root_lei": "9695ACMEGROUP0000030"})", {}),
                      "The step names no 'bundles' argument.");
}

TEST_CASE("an LEI import whose root LEI comes from neither the step nor the run is refused", tags) {
    CHECK_THROWS_WITH(parse_lei_hierarchy_arguments(R"({"bundles": ["lei_hierarchy"]})", {}),
                      "The step states no 'root_lei' argument and the run supplies no 'root_lei' "
                      "parameter.");
    CHECK_THROWS_WITH(
        parse_lei_hierarchy_arguments(R"({"bundles": ["lei_hierarchy"]})",
                                      parameters("counterparty_size", "small")),
        "The step states no 'root_lei' argument and the run supplies no 'root_lei' parameter.");
}

TEST_CASE("an LEI import whose root LEI parameter is empty is refused", tags) {
    CHECK_THROWS_WITH(
        parse_lei_hierarchy_arguments(R"({"bundles": ["lei_hierarchy"]})",
                                      parameters("root_lei", "")),
        "The step states no 'root_lei' argument and the run supplies no 'root_lei' parameter.");
}

TEST_CASE("an LEI import whose root_lei argument is not a string is refused", tags) {
    CHECK_THROWS_WITH(
        parse_lei_hierarchy_arguments(R"({"bundles": ["lei_hierarchy"], "root_lei": 7})", {}),
        "The step's argument 'root_lei' is not a string.");
}

TEST_CASE("a staff step reads each party's name, its bundles and its default flag", tags) {
    const auto assignments = parse_staff_assignments(
        R"({"parties": [
             {"name": "Acme Corporation Plc", "bundles": ["acme_group"], "default": true},
             {"name": "ACME Corporation UK plc", "bundles": ["acme_uk", "acme_extra"]}
           ]})");

    REQUIRE(assignments.size() == 2);
    CHECK(assignments[0].party_name == "Acme Corporation Plc");
    CHECK(assignments[0].bundles == std::vector<std::string>{"acme_group"});
    CHECK(assignments[0].is_default);
    CHECK(assignments[1].party_name == "ACME Corporation UK plc");
    CHECK(assignments[1].bundles == std::vector<std::string>{"acme_uk", "acme_extra"});
    CHECK_FALSE(assignments[1].is_default);
}

TEST_CASE("a staff step that names no parties is refused", tags) {
    CHECK_THROWS_WITH(parse_staff_assignments("{}"), "The step names no 'parties' argument.");
    CHECK_THROWS_WITH(parse_staff_assignments(R"({"parties": []})"),
                      "The step names no party in its 'parties' argument.");
    CHECK_THROWS_WITH(parse_staff_assignments(R"({"parties": "acme_uk"})"),
                      "The step's argument 'parties' is not a list of objects.");
}

TEST_CASE("a staff step's party entry that omits its name or its bundles is refused", tags) {
    CHECK_THROWS_WITH(parse_staff_assignments(R"({"parties": [{"bundles": ["acme_uk"]}]})"),
                      "The step's party entry names no 'name' argument.");
    CHECK_THROWS_WITH(
        parse_staff_assignments(R"({"parties": [{"name": "ACME Corporation UK plc"}]})"),
        "The step names no 'bundles' argument.");
    CHECK_THROWS_WITH(parse_staff_assignments(
                          R"({"parties": [{"name": "ACME Corporation UK plc", "bundles": []}]})"),
                      "The step names no bundle in its 'bundles' argument.");
}

TEST_CASE("a staff step's party entry whose default flag is not a boolean is refused", tags) {
    CHECK_THROWS_WITH(parse_staff_assignments(R"({"parties": [
             {"name": "ACME Corporation UK plc", "bundles": ["acme_uk"], "default": "yes"}
           ]})"),
                      "The step's argument 'default' is not a boolean.");
}

TEST_CASE("a photo step reads each party's name and dataset and the logo's key", tags) {
    const auto arguments = parse_photo_arguments(
        R"({"party_logo": "acme_party_logo",
            "parties": [
              {"name": "Acme Corporation Plc", "dataset": "acme.acme_group.accounts"},
              {"name": "ACME Corporation UK plc", "dataset": "acme.acme_uk.accounts"}
            ]})");

    CHECK(arguments.party_logo == "acme_party_logo");
    REQUIRE(arguments.parties.size() == 2);
    CHECK(arguments.parties[0].party_name == "Acme Corporation Plc");
    CHECK(arguments.parties[0].dataset == "acme.acme_group.accounts");
    CHECK(arguments.parties[1].dataset == "acme.acme_uk.accounts");
}

TEST_CASE("a photo step that names no party or no dataset is refused", tags) {
    CHECK_THROWS_WITH(parse_photo_arguments("{}"), "The step names no 'parties' argument.");
    CHECK_THROWS_WITH(parse_photo_arguments(R"({"parties": []})"),
                      "The step names no party in its 'parties' argument.");
    CHECK_THROWS_WITH(
        parse_photo_arguments(R"({"parties": [{"name": "ACME Corporation UK plc"}]})"),
        "The step's party entry names no 'dataset' argument.");
}

TEST_CASE("a photo step's party logo key is optional", tags) {
    const auto arguments = parse_photo_arguments(
        R"({"parties": [{"name": "ACME Corporation UK plc", "dataset": "acme.acme_uk.accounts"}]})");

    CHECK(arguments.party_logo.empty());
}

TEST_CASE("a market feed step reads its configuration bundles and its theme", tags) {
    const auto arguments = parse_market_feed_arguments(
        R"({"bundles": ["synthetic_realistic_2026", "marketdata.reference_vintage_2026_05_05"],
            "theme": "synthetic.themes.realistic_2026"})");

    CHECK(arguments.bundles == std::vector<std::string>{"synthetic_realistic_2026",
                                                        "marketdata.reference_vintage_2026_05_05"});
    CHECK(arguments.theme == "synthetic.themes.realistic_2026");
}

TEST_CASE("a market feed step that names no theme is refused", tags) {
    CHECK_THROWS_WITH(parse_market_feed_arguments(R"({"bundles": ["synthetic_realistic_2026"]})"),
                      "The step names no 'theme' argument.");
    CHECK_THROWS_WITH(
        parse_market_feed_arguments(R"({"theme": "synthetic.themes.realistic_2026"})"),
        "The step names no 'bundles' argument.");
    CHECK_THROWS_WITH(parse_market_feed_arguments(R"({"bundles": ["synthetic_realistic_2026"],
                                                       "theme": 7})"),
                      "The step's argument 'theme' is not a string.");
}
