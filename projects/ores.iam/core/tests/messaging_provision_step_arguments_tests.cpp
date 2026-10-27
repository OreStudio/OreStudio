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
#include <chrono>
#include <stdexcept>
#include <string>
#include <utility>
#include <vector>

namespace {

using ores::iam::messaging::classify_step_kind;
using ores::iam::messaging::parse_lei_hierarchy_arguments;
using ores::iam::messaging::parse_market_data_arguments;
using ores::iam::messaging::parse_photo_arguments;
using ores::iam::messaging::parse_provision_party_arguments;
using ores::iam::messaging::parse_publish_bundle_arguments;
using ores::iam::messaging::parse_set_staff_passwords_arguments;
using ores::iam::messaging::parse_staff_assignments;
using ores::iam::messaging::parse_step_bundles;
using ores::iam::messaging::parse_trade_generation_assignments;
using ores::iam::messaging::provision_step_action;

std::vector<ores::iam::workflow::provision_tenant_parameter> parameters(std::string name,
                                                                        std::string value) {
    return {ores::iam::workflow::provision_tenant_parameter{.name = std::move(name),
                                                            .value = std::move(value)}};
}

}

TEST_CASE("the executor classifies each kind it executes into its own action", "[provision_step]") {
    CHECK(classify_step_kind("publish_bundle") == provision_step_action::publish_bundle);
    CHECK(classify_step_kind("import_lei_hierarchy") ==
          provision_step_action::import_lei_hierarchy);
    CHECK(classify_step_kind("provision_party") == provision_step_action::provision_party);
    CHECK(classify_step_kind("load_staff") == provision_step_action::load_staff);
    CHECK(classify_step_kind("set_staff_passwords") == provision_step_action::set_staff_passwords);
    CHECK(classify_step_kind("attach_photos") == provision_step_action::attach_photos);
    CHECK(classify_step_kind("subscribe_market_data") ==
          provision_step_action::subscribe_market_data);
    CHECK(classify_step_kind("generate_trades") == provision_step_action::generate_trades);
    CHECK(classify_step_kind("complete_provisioning") ==
          provision_step_action::complete_provisioning);
    CHECK(classify_step_kind("start_tenant_setup") == provision_step_action::start_tenant_setup);
}

TEST_CASE("the executor refuses every kind it does not know", "[provision_step]") {
    CHECK(classify_step_kind("publish_everything") == provision_step_action::refuse);
    CHECK(classify_step_kind("") == provision_step_action::refuse);
    CHECK(classify_step_kind("complete_provisioning_x") == provision_step_action::refuse);
}

TEST_CASE("a step's bundles are read in the order the step names them", "[provision_step]") {
    const auto bundles = parse_step_bundles(R"({"bundles": ["base", "risk_management"]})");

    CHECK(bundles == std::vector<std::string>{"base", "risk_management"});
}

TEST_CASE("a step that names no bundles argument is refused", "[provision_step]") {
    CHECK_THROWS_WITH(parse_step_bundles("{}"), "The step names no 'bundles' argument.");
    CHECK_THROWS_WITH(parse_step_bundles(""), "The step names no 'bundles' argument.");
}

TEST_CASE("a step that names an empty bundles list is refused", "[provision_step]") {
    CHECK_THROWS_WITH(parse_step_bundles(R"({"bundles": []})"),
                      "The step names no bundle in its 'bundles' argument.");
}

TEST_CASE("a step whose bundles argument is not a list of strings is refused", "[provision_step]") {
    CHECK_THROWS_WITH(parse_step_bundles(R"({"bundles": "base"})"),
                      "The step's argument 'bundles' is not a list of strings.");
    CHECK_THROWS_WITH(parse_step_bundles(R"({"bundles": ["base", 7]})"),
                      "The step's argument 'bundles' is not a list of strings.");
}

TEST_CASE("a step whose arguments are not a JSON object is refused", "[provision_step]") {
    CHECK_THROWS_WITH(parse_step_bundles("not json"), "The step's arguments are not JSON.");
    CHECK_THROWS_WITH(parse_step_bundles("[1, 2]"), "The step's arguments are not a JSON object.");
}

TEST_CASE("an LEI import reads its bundle and its root LEI from the step's arguments",
          "[provision_step]") {
    const auto arguments = parse_lei_hierarchy_arguments(
        R"({"bundles": ["acme_lei_import"], "root_lei": "9695ACMEGROUP0000030"})", {});

    CHECK(arguments.bundles == std::vector<std::string>{"acme_lei_import"});
    CHECK(arguments.root_lei == "9695ACMEGROUP0000030");
}

TEST_CASE("an LEI import takes the root LEI from the run's parameters when the step states "
          "none",
          "[provision_step]") {
    const auto arguments = parse_lei_hierarchy_arguments(
        R"({"bundles": ["lei_hierarchy"]})", parameters("root_lei", "529900T8BM49AURSDO55"));

    CHECK(arguments.root_lei == "529900T8BM49AURSDO55");
}

TEST_CASE("an LEI import prefers the step's own root LEI to the run's parameter",
          "[provision_step]") {
    const auto arguments = parse_lei_hierarchy_arguments(
        R"({"bundles": ["lei_hierarchy"], "root_lei": "9695ACMEGROUP0000030"})",
        parameters("root_lei", "529900T8BM49AURSDO55"));

    CHECK(arguments.root_lei == "9695ACMEGROUP0000030");
}

TEST_CASE("an LEI import that names no bundle is refused", "[provision_step]") {
    CHECK_THROWS_WITH(parse_lei_hierarchy_arguments(R"({"root_lei": "9695ACMEGROUP0000030"})", {}),
                      "The step names no 'bundles' argument.");
}

TEST_CASE("an LEI import whose root LEI comes from neither the step nor the run reads as empty",
          "[provision_step]") {
    // A starting point may leave the legal entity out: the step then imports
    // nothing and says so, rather than stopping a run whose tenant is built
    // from the party it creates for itself.
    const auto absent = parse_lei_hierarchy_arguments(R"({"bundles": ["lei_hierarchy"]})", {});
    const auto other = parse_lei_hierarchy_arguments(R"({"bundles": ["lei_hierarchy"]})",
                                                     parameters("counterparty_size", "small"));

    CHECK(absent.root_lei.empty());
    CHECK(absent.bundles == std::vector<std::string>{"lei_hierarchy"});
    CHECK(other.root_lei.empty());
}

TEST_CASE("an LEI import whose root LEI parameter is empty reads as empty", "[provision_step]") {
    const auto arguments = parse_lei_hierarchy_arguments(R"({"bundles": ["lei_hierarchy"]})",
                                                         parameters("root_lei", ""));

    CHECK(arguments.root_lei.empty());
}

TEST_CASE("an LEI import whose root_lei argument is not a string is refused", "[provision_step]") {
    CHECK_THROWS_WITH(
        parse_lei_hierarchy_arguments(R"({"bundles": ["lei_hierarchy"], "root_lei": 7})", {}),
        "The step's argument 'root_lei' is not a string.");
}

TEST_CASE("a staff step reads each party's name, its bundles and its default flag",
          "[provision_step]") {
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

TEST_CASE("a staff step that names no parties is refused", "[provision_step]") {
    CHECK_THROWS_WITH(parse_staff_assignments("{}"), "The step names no 'parties' argument.");
    CHECK_THROWS_WITH(parse_staff_assignments(R"({"parties": []})"),
                      "The step names no party in its 'parties' argument.");
    CHECK_THROWS_WITH(parse_staff_assignments(R"({"parties": "acme_uk"})"),
                      "The step's argument 'parties' is not a list of objects.");
}

TEST_CASE("a staff step's party entry that omits its name or its bundles is refused",
          "[provision_step]") {
    CHECK_THROWS_WITH(parse_staff_assignments(R"({"parties": [{"bundles": ["acme_uk"]}]})"),
                      "The step's party entry names no 'name' argument.");
    CHECK_THROWS_WITH(
        parse_staff_assignments(R"({"parties": [{"name": "ACME Corporation UK plc"}]})"),
        "The step names no 'bundles' argument.");
    CHECK_THROWS_WITH(parse_staff_assignments(
                          R"({"parties": [{"name": "ACME Corporation UK plc", "bundles": []}]})"),
                      "The step names no bundle in its 'bundles' argument.");
}

TEST_CASE("a staff step's party entry whose default flag is not a boolean is refused",
          "[provision_step]") {
    CHECK_THROWS_WITH(parse_staff_assignments(R"({"parties": [
             {"name": "ACME Corporation UK plc", "bundles": ["acme_uk"], "default": "yes"}
           ]})"),
                      "The step's argument 'default' is not a boolean.");
}

TEST_CASE("a photo step reads each party's name and dataset and the logo's key",
          "[provision_step]") {
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

TEST_CASE("a photo step that names no party or no dataset is refused", "[provision_step]") {
    CHECK_THROWS_WITH(parse_photo_arguments("{}"), "The step names no 'parties' argument.");
    CHECK_THROWS_WITH(parse_photo_arguments(R"({"parties": []})"),
                      "The step names no party in its 'parties' argument.");
    CHECK_THROWS_WITH(
        parse_photo_arguments(R"({"parties": [{"name": "ACME Corporation UK plc"}]})"),
        "The step's party entry names no 'dataset' argument.");
}

TEST_CASE("a photo step's party logo key is optional", "[provision_step]") {
    const auto arguments = parse_photo_arguments(
        R"({"parties": [{"name": "ACME Corporation UK plc", "dataset": "acme.acme_uk.accounts"}]})");

    CHECK(arguments.party_logo.empty());
}

TEST_CASE("a market data step reads its provider", "[provision_step]") {
    const auto arguments =
        parse_market_data_arguments(R"({"provider": "bloomberg", "source": "bloomberg-a"})");

    CHECK(arguments.provider == "bloomberg");
    CHECK(arguments.source == "bloomberg-a");
}

TEST_CASE("a market data step reads the provider and source it names for each party", "[provision_step]") {
    // The demonstration's entities take their data from different vendors, so the step states one
    // provider and source for each; a party it names in part takes the step's own for the rest.
    const auto arguments = parse_market_data_arguments(R"({
        "provider": "bloomberg", "source": "bloomberg-a",
        "parties": [
            {"party": "ACME Corporation UK plc", "provider": "demo", "source": "demo-a"},
            {"party": "ACME Corporation US Inc", "provider": "lseg"}
        ]})");

    REQUIRE(arguments.parties.size() == 2);
    CHECK(arguments.parties[0].party == "ACME Corporation UK plc");
    CHECK(arguments.parties[0].provider == "demo");
    CHECK(arguments.parties[0].source == "demo-a");
    CHECK(arguments.parties[1].party == "ACME Corporation US Inc");
    CHECK(arguments.parties[1].provider == "lseg");
    CHECK(arguments.parties[1].source.empty());
}

TEST_CASE("a market data step that names no parties gives every party its own provider", "[provision_step]") {
    const auto arguments =
        parse_market_data_arguments(R"({"provider": "bloomberg", "source": "bloomberg-a"})");

    CHECK(arguments.parties.empty());
}

TEST_CASE("a market data step refuses a party entry that names no party", "[provision_step]") {
    CHECK_THROWS_WITH(parse_market_data_arguments(R"({"provider": "ore", "source": "ore-import",
        "parties": [{"provider": "demo", "source": "demo-a"}]})"),
                      "The step's 'parties' entry names no 'party' argument.");
}

TEST_CASE("a market data step that names no provider is refused", "[provision_step]") {
    CHECK_THROWS_WITH(parse_market_data_arguments(R"({})"),
                      "The step names no 'provider' argument.");
    CHECK_THROWS_WITH(parse_market_data_arguments(R"({"provider": 7})"),
                      "The step's argument 'provider' is not a string.");
    CHECK_THROWS_WITH(parse_market_data_arguments(R"({"provider": "ore"})"),
                      "The step names no 'source' argument.");
}

TEST_CASE("a password step reads the password its profile states", "[provision_step]") {
    const auto arguments =
        parse_set_staff_passwords_arguments(R"({"password": "Secure-Password-123"})");

    CHECK(arguments.password == "Secure-Password-123");
}

TEST_CASE("a password step that names no password is refused", "[provision_step]") {
    CHECK_THROWS_WITH(parse_set_staff_passwords_arguments(R"({})"),
                      "The step names no 'password' argument.");
    CHECK_THROWS_WITH(parse_set_staff_passwords_arguments(R"({"password": ""})"),
                      "The step names no 'password' argument.");
    CHECK_THROWS_WITH(parse_set_staff_passwords_arguments(R"({"password": 7})"),
                      "The step's argument 'password' is not a string.");
}

TEST_CASE("a publish step reads the datasets its profile opts in", "[provision_step]") {
    const auto arguments = parse_publish_bundle_arguments(
        R"({"bundles": ["base"], "opted_in_datasets": ["gleif.lei_counterparties.large"]})", {});

    CHECK(arguments.bundles == std::vector<std::string>{"base"});
    CHECK(arguments.opted_in_datasets ==
          std::vector<std::string>{"gleif.lei_counterparties.large"});
}

TEST_CASE("a publish step that opts in nothing publishes its bundles whole", "[provision_step]") {
    const auto arguments = parse_publish_bundle_arguments(R"({"bundles": ["base"]})", {});

    CHECK(arguments.bundles == std::vector<std::string>{"base"});
    CHECK(arguments.opted_in_datasets.empty());
}

TEST_CASE("a publish step builds an opted-in dataset from the run's parameter",
          "[provision_step]") {
    // The counterparty set a starting point publishes is the value of its own
    // parameter, so the row names the parameter rather than one of its values.
    const auto arguments = parse_publish_bundle_arguments(
        R"({"bundles": ["base"], "opted_in_datasets": ["gleif.lei_counterparties.{counterparty_size}"]})",
        parameters("counterparty_size", "small"));

    CHECK(arguments.opted_in_datasets ==
          std::vector<std::string>{"gleif.lei_counterparties.small"});
}

TEST_CASE("a publish step whose parameter the run supplies no value for is refused",
          "[provision_step]") {
    CHECK_THROWS_WITH(
        parse_publish_bundle_arguments(
            R"({"bundles": ["base"], "opted_in_datasets": ["gleif.lei_counterparties.{counterparty_size}"]})",
            {}),
        "The step's argument 'gleif.lei_counterparties.{counterparty_size}' names the parameter "
        "'counterparty_size', which the run supplies no value for.");
}

TEST_CASE("a publish step whose parameter reference is not closed is refused", "[provision_step]") {
    CHECK_THROWS_WITH(
        parse_publish_bundle_arguments(
            R"({"bundles": ["base"], "opted_in_datasets": ["gleif.lei_counterparties.{size"]})",
            parameters("size", "small")),
        "The step's argument 'gleif.lei_counterparties.{size' names a parameter without closing "
        "its braces.");
}

TEST_CASE("a publish step fills every reference a dataset names", "[provision_step]") {
    const std::vector<ores::iam::workflow::provision_tenant_parameter> given{
        {.name = "counterparty_size", .value = "large"}, {.name = "office", .value = "uk"}};

    const auto arguments = parse_publish_bundle_arguments(
        R"({"bundles": ["base"], "opted_in_datasets": ["gleif.lei_counterparties.{counterparty_size}", "acme.{office}.accounts"]})",
        given);

    CHECK(arguments.opted_in_datasets ==
          std::vector<std::string>{"gleif.lei_counterparties.large", "acme.uk.accounts"});
}

TEST_CASE("a publish step whose braces name no parameter is refused", "[provision_step]") {
    // A brace that is not a reference has nothing to resolve, so it is refused
    // rather than published as written: a dataset code with a stray brace in it
    // is a profile row that will never publish anything.
    CHECK_THROWS_WITH(
        parse_publish_bundle_arguments(
            R"({"bundles": ["base"], "opted_in_datasets": ["gleif.lei_counterparties.{}"]})",
            parameters("counterparty_size", "small")),
        "The step's argument 'gleif.lei_counterparties.{}' names the parameter '', which the run "
        "supplies no value for.");
}

TEST_CASE("a party step that names no party acts on every party the tenant holds",
          "[provision_step]") {
    const auto arguments = parse_provision_party_arguments(R"({"bundles": ["party_essentials"]})");

    CHECK(arguments.bundles == std::vector<std::string>{"party_essentials"});
    CHECK(arguments.party.empty());
}

TEST_CASE("a party step reads the one party it acts on", "[provision_step]") {
    const auto arguments = parse_provision_party_arguments(
        R"({"bundles": ["party_essentials"], "party": "BARCLAYS PLC"})");

    CHECK(arguments.bundles == std::vector<std::string>{"party_essentials"});
    CHECK(arguments.party == "BARCLAYS PLC");
}

TEST_CASE("a party step that names no bundle is refused", "[provision_step]") {
    CHECK_THROWS_WITH(parse_provision_party_arguments(R"({"party": "BARCLAYS PLC"})"),
                      "The step names no 'bundles' argument.");
}

TEST_CASE("a trade generation step reads each config's recipe in the order it names them",
          "[provision_step]") {
    const auto assignments = parse_trade_generation_assignments(
        R"({"configs": [
            {"party": "Acme Corporation Plc", "name": "ACME ACMCOR bond population",
             "asset_class_code": "bond", "book": "Group Intercompany Loans",
             "min_tenor_code": "1Y", "max_tenor_code": "10Y",
             "min_notional": "100000", "max_notional": "10000000", "seed": 42},
            {"party": "ACME Corporation HK Ltd", "name": "ACME ACCOHK bond population",
             "asset_class_code": "bond", "book": "Liquidity Buffer Bonds",
             "min_tenor_code": "1Y", "max_tenor_code": "10Y",
             "min_notional": "100000", "max_notional": "10000000", "seed": 42,
             "as_of_date": "2026-10-15", "trades_per_book": 24}
        ]})");

    REQUIRE(assignments.size() == 2);
    CHECK(assignments[0].party == "Acme Corporation Plc");
    CHECK(assignments[0].asset_class_code == "bond");
    CHECK(assignments[0].book == "Group Intercompany Loans");
    CHECK(assignments[0].min_notional->to_double() == 100000.0);
    CHECK(assignments[0].max_notional->to_double() == 10000000.0);
    CHECK(assignments[0].seed == 42);
    // An entry that states no anchor has none, so the step anchors it on the
    // day it runs rather than on a date the profile cannot know.
    CHECK_FALSE(assignments[0].as_of_date.has_value());
    // A count per book the entry leaves out stays unset, so the step takes the
    // generator's own rather than a number this step invented.
    CHECK_FALSE(assignments[0].trades_per_book.has_value());
    CHECK(assignments[1].trades_per_book == 24);
    CHECK(assignments[1].as_of_date == std::chrono::year_month_day(std::chrono::year{2026},
                                                                   std::chrono::month{10},
                                                                   std::chrono::day{15}));
}

TEST_CASE("a trade generation step refuses a count per book of zero", "[provision_step]") {
    CHECK_THROWS_WITH(
        parse_trade_generation_assignments(
            R"({"configs": [
            {"party": "Acme Corporation Plc", "name": "ACME ACMCOR bond population",
             "asset_class_code": "bond", "book": "Group Intercompany Loans",
             "min_tenor_code": "1Y", "max_tenor_code": "10Y",
             "min_notional": "100000", "max_notional": "10000000", "seed": 42,
             "trades_per_book": 0}
        ]})"),
        "The step's config 'ACME ACMCOR bond population' states a trades_per_book of 0.");
}

TEST_CASE("a trade generation step that states no config is refused", "[provision_step]") {
    CHECK_THROWS_WITH(parse_trade_generation_assignments(R"({"configs": []})"),
                      "The step names no config in its 'configs' argument.");
}
