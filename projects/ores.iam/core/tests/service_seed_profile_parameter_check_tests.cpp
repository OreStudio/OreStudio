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
#include "ores.iam.api/domain/seed_profile_parameter.hpp"
#include "ores.iam.core/service/seed_profile_parameter_check.hpp"
#include <catch2/catch_test_macros.hpp>

namespace {

const std::string tags("[seed_profile]");

using ores::iam::domain::seed_profile_parameter;
using ores::iam::service::check_parameters;

/// The two parameters the seeded profiles declare: the root legal entity of the
/// tenant, named by its LEI, and a counterparty set with a choice and a default.
std::vector<seed_profile_parameter> declared_parameters() {
    seed_profile_parameter root_lei;
    root_lei.name = "root_lei";
    root_lei.label = "Root legal entity";
    root_lei.data_type = "legal_entity";
    root_lei.default_value = "";
    root_lei.is_required = true;

    seed_profile_parameter counterparty_size;
    counterparty_size.name = "counterparty_size";
    counterparty_size.label = "Counterparty set";
    counterparty_size.data_type = "choice";
    counterparty_size.choices_json = R"(["small","large"])";
    counterparty_size.default_value = "small";
    counterparty_size.is_required = true;

    return {root_lei, counterparty_size};
}

}

TEST_CASE("check_parameters_fills_the_default_of_a_parameter_the_request_omits", tags) {
    const auto checked = check_parameters(declared_parameters(), {"root_lei=9695ACMEGROUP0000030"});

    REQUIRE(checked.accepted());
    REQUIRE(checked.values.size() == 2);
    CHECK(checked.values[0].name == "root_lei");
    CHECK(checked.values[0].value == "9695ACMEGROUP0000030");
    CHECK(checked.values[1].name == "counterparty_size");
    CHECK(checked.values[1].value == "small");
}

TEST_CASE("check_parameters_accepts_a_declared_choice", tags) {
    const auto checked = check_parameters(
        declared_parameters(), {"root_lei=9695ACMEGROUP0000030", "counterparty_size=large"});

    REQUIRE(checked.accepted());
    CHECK(checked.values[1].value == "large");
}

TEST_CASE("check_parameters_refuses_a_parameter_the_profile_does_not_declare", tags) {
    const auto checked = check_parameters(
        declared_parameters(), {"root_lei=9695ACMEGROUP0000030", "counterparty_count=100"});

    CHECK_FALSE(checked.accepted());
    CHECK(checked.refusal ==
          "The profile does not declare a parameter named 'counterparty_count'.");
}

TEST_CASE("check_parameters_refuses_a_missing_required_parameter", tags) {
    const auto checked = check_parameters(declared_parameters(), {});

    CHECK_FALSE(checked.accepted());
    CHECK(checked.refusal == "The parameter 'root_lei' is required and has no value.");
}

TEST_CASE("check_parameters_refuses_an_empty_value_for_a_required_parameter", tags) {
    const auto checked = check_parameters(declared_parameters(), {"root_lei="});

    CHECK_FALSE(checked.accepted());
    CHECK(checked.refusal == "The parameter 'root_lei' is required and has no value.");
}

TEST_CASE("check_parameters_refuses_a_required_parameter_stated_as_empty_rather_than_"
          "replacing_it_with_its_default",
          tags) {
    const auto checked = check_parameters(declared_parameters(),
                                          {"root_lei=9695ACMEGROUP0000030", "counterparty_size="});

    CHECK_FALSE(checked.accepted());
    CHECK(checked.refusal == "The parameter 'counterparty_size' is required and has no value.");
}

TEST_CASE("check_parameters_refuses_a_value_outside_a_choice_parameters_choices", tags) {
    const auto checked = check_parameters(
        declared_parameters(), {"root_lei=9695ACMEGROUP0000030", "counterparty_size=huge"});

    CHECK_FALSE(checked.accepted());
    CHECK(checked.refusal ==
          "The value 'huge' for 'counterparty_size' is not one of: small, large.");
}

TEST_CASE("check_parameters_refuses_a_value_that_is_not_the_declared_data_type", tags) {
    auto declared = declared_parameters();
    declared[1].data_type = "integer";
    declared[1].choices_json = "";
    declared[1].default_value = "10";

    const auto checked =
        check_parameters(declared, {"root_lei=9695ACMEGROUP0000030", "counterparty_size=large"});
    CHECK_FALSE(checked.accepted());
    CHECK(checked.refusal == "The value 'large' for 'counterparty_size' is not an integer.");

    const auto numbers =
        check_parameters(declared, {"root_lei=9695ACMEGROUP0000030", "counterparty_size=-10"});
    CHECK(numbers.accepted());
    CHECK(numbers.values[1].value == "-10");
}

TEST_CASE("check_parameters_refuses_a_boolean_that_is_not_true_or_false", tags) {
    auto declared = declared_parameters();
    declared[1].data_type = "boolean";
    declared[1].choices_json = "";
    declared[1].default_value = "false";

    const auto checked =
        check_parameters(declared, {"root_lei=9695ACMEGROUP0000030", "counterparty_size=yes"});
    CHECK_FALSE(checked.accepted());
    CHECK(checked.refusal == "The value 'yes' for 'counterparty_size' is not true or false.");

    const auto written =
        check_parameters(declared, {"root_lei=9695ACMEGROUP0000030", "counterparty_size=TRUE"});
    CHECK(written.accepted());
    CHECK(written.values[1].value == "TRUE");
}

TEST_CASE("check_parameters_refuses_a_legal_entity_that_is_not_an_LEI", tags) {
    // The code one walk mistyped, which the publication used to refuse from
    // inside its own SQL: eighteen characters, so it never named an entity.
    const auto mistyped = check_parameters(declared_parameters(), {"root_lei=9DJT3UXIJZI4WXO774"});

    CHECK_FALSE(mistyped.accepted());
    CHECK(mistyped.refusal == "The value '9DJT3UXIJZI4WXO774' for 'root_lei' is not an LEI.");

    const auto named = check_parameters(declared_parameters(), {"root_lei=9DJT3UXIJIZJI4WXO774"});

    CHECK(named.accepted());
    CHECK(named.values[0].value == "9DJT3UXIJIZJI4WXO774");
}

TEST_CASE("check_parameters_allows_an_optional_legal_entity_to_be_left_out", tags) {
    // The Operational card declares its entity optional, so a tenant nobody
    // holds an entity for is still a tenant: the import step says it had
    // nothing to read and the parties come from the party stage.
    auto declared = declared_parameters();
    declared[0].is_required = false;

    const auto omitted = check_parameters(declared, {});
    CHECK(omitted.accepted());
    CHECK(omitted.values[0].value == "");

    const auto cleared = check_parameters(declared, {"root_lei="});
    CHECK(cleared.accepted());
    CHECK(cleared.values[0].value == "");
}

TEST_CASE("check_parameters_refuses_an_entry_that_is_not_a_name_value_pair", tags) {
    const auto checked = check_parameters(declared_parameters(), {"root_lei"});

    CHECK_FALSE(checked.accepted());
    CHECK(checked.refusal == "The value 'root_lei' is not a name=value pair.");
}

TEST_CASE("check_parameters_refuses_a_parameter_supplied_more_than_once", tags) {
    const auto checked = check_parameters(declared_parameters(),
                                          {"root_lei=9695ACMEGROUP0000030", "root_lei=9695OTHER"});

    CHECK_FALSE(checked.accepted());
    CHECK(checked.refusal == "The parameter 'root_lei' is supplied more than once.");
}

TEST_CASE("check_parameters_refuses_a_parameter_whose_declared_type_it_does_not_know", tags) {
    auto declared = declared_parameters();
    declared[1].data_type = "counterparty";

    const auto checked =
        check_parameters(declared, {"root_lei=9695ACMEGROUP0000030", "counterparty_size=small"});

    CHECK_FALSE(checked.accepted());
    CHECK(checked.refusal ==
          "The parameter 'counterparty_size' declares the unknown data type 'counterparty'.");
}
