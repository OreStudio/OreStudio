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
#include "ores.http/routes/refdata/convention_terms.hpp"
#include "ores.logging/make_logger.hpp"
#include "test_suite.hpp"
#include <catch2/catch_test_macros.hpp>
#include <string>
#include <vector>

using ores::http::routes::refdata::terms::name_term;
using ores::http::routes::refdata::terms::term_view;
using ores::utility::domain::outcome;
using ores::utility::domain::result;
using namespace ores::logging;

namespace {

/// One term of a swap convention, as the store names it in a refusal.
term_view swap_term() {
    term_view term;
    term.entity = "swap_convention";
    term.column = "fixed_frequency";
    term.scope = "Swap";
    term.ore = "FixedFrequency";
    return term;
}

}

TEST_CASE("a_refused_term_takes_the_field_the_service_named", "[convention_terms]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing the field a refused term is put on";

    result refused;
    refused.outcome = outcome::invalid;
    refused.code = "invalid_value";
    refused.message =
        "The row was refused.\nInvalid value WEEKLY for Swap.FixedFrequency: the value is not "
        "in set ores_refdata_payment_frequencies.\ncontext line";

    name_term(refused, {swap_term()});

    REQUIRE(refused.fields.size() == 1);
    CHECK(refused.fields.front().field == "fixed_frequency");
    CHECK(refused.fields.front().code == "invalid_value");
    CHECK(refused.fields.front().message ==
          "Invalid value WEEKLY for Swap.FixedFrequency: the value is not in set "
          "ores_refdata_payment_frequencies.");
}

TEST_CASE("a_refusal_that_names_no_term_keeps_no_field", "[convention_terms]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing a refusal that names no term";

    result refused;
    refused.outcome = outcome::invalid;
    refused.message = "The row was refused for another reason.";
    name_term(refused, {swap_term()});
    CHECK(refused.fields.empty());

    result accepted;
    accepted.outcome = outcome::ok;
    name_term(accepted, {swap_term()});
    CHECK(accepted.fields.empty());

    result already;
    already.outcome = outcome::invalid;
    already.message = "Invalid value WEEKLY for Swap.FixedFrequency: no.";
    already.fields.push_back({"coupon_frequency", "invalid_value", "The store's own words."});
    name_term(already, {swap_term()});
    REQUIRE(already.fields.size() == 1);
    CHECK(already.fields.front().field == "coupon_frequency");
}
