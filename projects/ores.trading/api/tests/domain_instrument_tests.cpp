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
#include "ores.trading.api/domain/credit_default_swap_instrument.hpp"
#include "ores.trading.api/domain/index_credit_default_swap_option_instrument.hpp"
#include "ores.trading.api/domain/instrument.hpp"
#include "ores.trading.api/domain/risk_participation_agreement_instrument.hpp"
#include <catch2/catch_test_macros.hpp>
#include <string>
#include <string_view>

namespace {

using ores::trading::domain::credit_default_swap_instrument;
using ores::trading::domain::credit_instrument_data;
using ores::trading::domain::fact_rows;
using ores::trading::domain::index_credit_default_swap_option_instrument;
using ores::trading::domain::risk_participation_agreement_instrument;
using ores::trading::domain::visit_credit_facts;

const std::string_view test_suite("ores.trading.tests");
const std::string tags("[domain]");

}

using namespace ores::logging;

TEST_CASE("a_carrier_that_states_no_fact_row_reports_no_answer", tags) {
    auto lg(make_logger(test_suite));

    const credit_instrument_data sut;
    const auto answered = visit_credit_facts([](const auto&) { return true; }, sut);

    BOOST_LOG_SEV(lg, info) << "Answered: " << answered.has_value();
    CHECK(!answered.has_value());
}

TEST_CASE("a_visitor_that_answers_false_is_not_reported_as_silence", tags) {
    auto lg(make_logger(test_suite));

    credit_instrument_data sut;
    sut.credit_default_swap_instruments.push_back(credit_default_swap_instrument{});
    const auto answered = visit_credit_facts([](const auto&) { return false; }, sut);

    BOOST_LOG_SEV(lg, info) << "Answered: " << answered.has_value();
    REQUIRE(answered.has_value());
    CHECK(!*answered);
}

TEST_CASE("the_visitor_sees_the_one_fact_row_the_carrier_states", tags) {
    auto lg(make_logger(test_suite));

    credit_instrument_data sut;
    sut.risk_participation_agreement_instruments.push_back(
        risk_participation_agreement_instrument{});
    const auto answered = visit_credit_facts([](const auto&) { return true; }, sut);

    BOOST_LOG_SEV(lg, info) << "Answered: " << answered.has_value();
    REQUIRE(answered.has_value());
    CHECK(*answered);
}

TEST_CASE("fact_rows_reaches_the_array_the_fact_type_lives_in", tags) {
    auto lg(make_logger(test_suite));

    credit_instrument_data sut;
    sut.credit_default_swap_instruments.push_back(credit_default_swap_instrument{});
    sut.index_credit_default_swap_option_instruments.push_back(
        index_credit_default_swap_option_instrument{});
    const auto& rows = fact_rows<index_credit_default_swap_option_instrument>(sut);

    BOOST_LOG_SEV(lg, info) << "Rows: " << rows.size();
    CHECK(rows.size() == 1U);
    CHECK(fact_rows<credit_default_swap_instrument>(sut).size() == 1U);
    CHECK(fact_rows<risk_participation_agreement_instrument>(sut).empty());
}
