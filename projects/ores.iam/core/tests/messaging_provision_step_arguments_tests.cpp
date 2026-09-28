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
    CHECK(classify_step_kind("complete_provisioning") ==
          provision_step_action::complete_provisioning);
}

TEST_CASE("the executor refuses every kind it does not execute", tags) {
    CHECK(classify_step_kind("load_staff") == provision_step_action::refuse);
    CHECK(classify_step_kind("attach_photos") == provision_step_action::refuse);
    CHECK(classify_step_kind("start_market_feeds") == provision_step_action::refuse);
    CHECK(classify_step_kind("publish_everything") == provision_step_action::refuse);
    CHECK(classify_step_kind("") == provision_step_action::refuse);
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
