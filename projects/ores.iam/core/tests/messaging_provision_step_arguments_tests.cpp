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
#include "ores.iam.core/messaging/population_knobs.hpp"
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

TEST_CASE("a market data step reads the provider and source it names for each party",
          "[provision_step]") {
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

TEST_CASE("a market data step that names no parties gives every party its own provider",
          "[provision_step]") {
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

namespace {

using ores::iam::messaging::apply_population_knobs;
using ores::iam::messaging::population_knobs;
using ores::iam::messaging::read_population_knobs;
using ores::iam::workflow::provision_tenant_parameter;

/// The codes each class's planner cycles, as the handler resolves them.
std::size_t codes_of(const std::string& asset_class) {
    if (asset_class == "bond")
        return 10;
    if (asset_class == "commodity")
        return 25;
    if (asset_class == "interest_rates")
        return 9;
    if (asset_class == "fx")
        return 16;
    if (asset_class == "equity")
        return 17;
    if (asset_class == "credit")
        return 5;
    if (asset_class == "inflation")
        return 1;
    return 3;
}

std::vector<ores::iam::messaging::trade_generation_assignment> four_configs(const std::string& asset_class) {
    std::vector<ores::iam::messaging::trade_generation_assignment> assignments;
    for (int i = 0; i < 4; ++i) {
        ores::iam::messaging::trade_generation_assignment entry;
        entry.name = asset_class + std::to_string(i);
        entry.party = "Acme Corporation Plc";
        entry.asset_class_code = asset_class;
        entry.book = "Book";
        entry.trades_per_book = 1;
        assignments.push_back(std::move(entry));
    }
    return assignments;
}

}

namespace {

/// The configs a demonstration run records: one per book the class books into.
std::vector<ores::iam::messaging::trade_generation_assignment> all_configs() {
    const std::vector<std::pair<std::string, int>> classes{
        {"bond", 4},        {"commodity", 1}, {"credit", 1}, {"equity", 1},
        {"fx", 4},          {"inflation", 1}, {"interest_rates", 3}, {"scripted", 1}};

    std::vector<ores::iam::messaging::trade_generation_assignment> assignments;
    for (const auto& [asset_class, configs] : classes)
        for (int i = 0; i < configs; ++i) {
            auto entry = four_configs(asset_class).front();
            entry.name = asset_class + std::to_string(i);
            assignments.push_back(std::move(entry));
        }
    return assignments;
}

/// The counts the configs of one class state, in order.
std::vector<std::uint32_t> counts_of(
    const std::vector<ores::iam::messaging::trade_generation_assignment>& assignments,
    const std::string& asset_class) {
    std::vector<std::uint32_t> counts;
    for (const auto& entry : assignments)
        if (entry.asset_class_code == asset_class)
            counts.push_back(*entry.trades_per_book);
    return counts;
}

/// The page's mix, with one class stated over it.
std::vector<provision_tenant_parameter> page_mix(std::uint32_t total,
                                                 const std::string& name,
                                                 std::uint32_t value) {
    const std::vector<std::pair<std::string, std::uint32_t>> weights{
        {"population_interest_rates_percent", 42},
        {"population_fx_percent", 25},
        {"population_equity_percent", 12},
        {"population_credit_percent", 6},
        {"population_commodity_percent", 5},
        {"population_bond_percent", 4},
        {"population_inflation_percent", 3},
        {"population_scripted_percent", 3}};

    std::vector<provision_tenant_parameter> parameters{{"population_size", std::to_string(total)}};
    for (const auto& [declared, stated] : weights)
        parameters.push_back({declared, std::to_string(declared == name ? value : stated)});
    return parameters;
}

}

TEST_CASE("a run that states no size keeps the counts its recipe states", "[population_knobs]") {
    auto assignments = all_configs();
    apply_population_knobs(assignments, read_population_knobs({}), codes_of);

    for (const auto& entry : assignments)
        CHECK(entry.trades_per_book == 1);
}

TEST_CASE("a mix states each class's share of the total, remainder first", "[population_knobs]") {
    auto assignments = all_configs();
    apply_population_knobs(assignments,
                           read_population_knobs(page_mix(1000, "population_fx_percent", 25)),
                           codes_of);

    // Forty bond trades over four configs is ten each.
    for (const auto count : counts_of(assignments, "bond"))
        CHECK(count == 10);

    // Two hundred and fifty fx trades over four configs is sixty-two with two
    // spare, and the spare goes to the first configs.
    const auto fx = counts_of(assignments, "fx");
    REQUIRE(fx.size() == 4);
    CHECK(fx[0] == 63);
    CHECK(fx[1] == 63);
    CHECK(fx[2] == 62);
    CHECK(fx[3] == 62);
}

TEST_CASE("a mix that does not sum to a hundred is scaled, not refused", "[population_knobs]") {
    /*
     * The shares are shares: a mix of ninety-nine is scaled to the total the
     * run states rather than refused, so the operator is not asked to do the
     * arithmetic the server can do. The classes still state the whole total.
     */
    const auto class_total = [](const auto& assignments, const std::string& asset_class) {
        std::uint32_t total = 0;
        for (const auto& entry : assignments)
            if (entry.asset_class_code == asset_class)
                total += *entry.trades_per_book;
        return total;
    };
    const auto everyone = [](const auto& assignments) {
        std::uint32_t total = 0;
        for (const auto& entry : assignments)
            total += *entry.trades_per_book;
        return total;
    };

    auto exact = all_configs();
    apply_population_knobs(exact,
                           read_population_knobs(page_mix(1000, "population_fx_percent", 25)),
                           codes_of);

    auto scaled = all_configs();
    apply_population_knobs(scaled,
                           read_population_knobs(page_mix(1000, "population_fx_percent", 24)),
                           codes_of);

    CHECK(everyone(exact) == 1000);
    CHECK(everyone(scaled) == 1000);
    for (const auto* asset_class :
         {"bond", "commodity", "credit", "equity", "fx", "inflation", "interest_rates", "scripted"}) {
        const auto exact_total = class_total(exact, asset_class);
        const auto scaled_total = class_total(scaled, asset_class);
        CHECK(scaled_total + 1 >= exact_total);
        CHECK(scaled_total <= exact_total + 1);
    }
}

TEST_CASE("a mix whose shares state nothing at all is refused", "[population_knobs]") {
    auto assignments = all_configs();
    std::vector<provision_tenant_parameter> parameters{{"population_size", "1000"}};
    for (const auto* name : {"population_bond_percent",
                             "population_fx_percent",
                             "population_equity_percent"})
        parameters.push_back({name, "0"});

    CHECK_THROWS_WITH(apply_population_knobs(assignments,
                                             read_population_knobs(parameters),
                                             codes_of),
                      Catch::Matchers::ContainsSubstring("no share at all"));
}

TEST_CASE("a class whose share cannot hold its codes is refused with the total that would",
          "[population_knobs]") {
    auto assignments = all_configs();

    /*
     * Four per cent of a hundred is four trades over four configs, and each
     * config states ten codes, so the smallest total is a thousand.
     */
    CHECK_THROWS_WITH(apply_population_knobs(assignments,
                                             read_population_knobs(
                                                 page_mix(100, "population_bond_percent", 4)),
                                             codes_of),
                      Catch::Matchers::ContainsSubstring("A total of 1000 is the smallest"));
}

TEST_CASE("a class the run books nothing for is refused", "[population_knobs]") {
    /*
     * A mix that names a class the run records no config for would take a share
     * of the population and book none of it, so it is refused by name.
     */
    auto assignments = all_configs();
    std::erase_if(assignments, [](const auto& entry) {
        return entry.asset_class_code == "equity";
    });

    CHECK_THROWS_WITH(apply_population_knobs(assignments,
                                             read_population_knobs(
                                                 page_mix(1000, "population_fx_percent", 25)),
                                             codes_of),
                      Catch::Matchers::ContainsSubstring("states 'equity'"));
}

TEST_CASE("a size stated alone leaves the mix as the recipe states it", "[population_knobs]") {
    auto assignments = all_configs();
    assignments.front().trades_per_book = 7;
    const std::vector<provision_tenant_parameter> parameters{{"population_size", "5000"}};

    apply_population_knobs(assignments, read_population_knobs(parameters), codes_of);

    CHECK(assignments.front().trades_per_book == 7);
}

TEST_CASE("a share of zero books nothing of that class", "[population_knobs]") {
    /*
     * The bonds' four per cent moves to the rates, so the mix still sums to a
     * hundred and the class that gives up its share books nothing.
     */
    auto parameters = page_mix(1000, "population_bond_percent", 0);
    for (auto& parameter : parameters)
        if (parameter.name == "population_interest_rates_percent")
            parameter.value = "46";

    auto assignments = all_configs();
    apply_population_knobs(assignments, read_population_knobs(parameters), codes_of);

    for (const auto count : counts_of(assignments, "bond"))
        CHECK(count == 0);
}
