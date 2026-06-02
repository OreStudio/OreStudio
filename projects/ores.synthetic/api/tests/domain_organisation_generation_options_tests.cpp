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
#include "ores.synthetic.api/domain/organisation_generation_options.hpp"

#include <catch2/catch_test_macros.hpp>
#include "ores.logging/make_logger.hpp"

namespace {

const std::string_view test_suite("ores.synthetic.tests");
const std::string tags("[domain][organisation_generation_options]");

}

using ores::synthetic::domain::organisation_generation_options;
using namespace ores::logging;

TEST_CASE("default_constructed_organisation_generation_options_has_expected_values", tags) {
    auto lg(make_logger(test_suite));

    const organisation_generation_options sut;
    BOOST_LOG_SEV(lg, info) << "Default organisation_generation_options country: "
                            << sut.country;

    CHECK(!sut.seed.has_value());
    CHECK(sut.country == "GB");
    CHECK(sut.party_count == 5);
    CHECK(sut.party_max_depth == 3);
    CHECK(sut.counterparty_count == 10);
    CHECK(sut.counterparty_max_depth == 3);
    CHECK(sut.portfolio_leaf_count == 8);
    CHECK(sut.portfolio_max_depth == 4);
    CHECK(sut.books_per_leaf_portfolio == 2);
    CHECK(sut.business_unit_count == 10);
    CHECK(sut.business_unit_max_depth == 2);
    CHECK(sut.generate_addresses == true);
    CHECK(sut.contacts_per_party == 2);
    CHECK(sut.contacts_per_counterparty == 1);
    CHECK(sut.generate_identifiers == true);
}

TEST_CASE("organisation_generation_options_seed_can_be_set", tags) {
    auto lg(make_logger(test_suite));

    organisation_generation_options sut;
    sut.seed = 12345ULL;
    BOOST_LOG_SEV(lg, info) << "organisation_generation_options seed: "
                            << *sut.seed;

    CHECK(sut.seed.has_value());
    CHECK(*sut.seed == 12345ULL);
}

TEST_CASE("organisation_generation_options_counts_can_be_overridden", tags) {
    auto lg(make_logger(test_suite));

    organisation_generation_options sut;
    sut.party_count = 20;
    sut.counterparty_count = 50;
    sut.portfolio_leaf_count = 16;
    sut.business_unit_count = 30;
    BOOST_LOG_SEV(lg, info) << "organisation_generation_options party_count: "
                            << sut.party_count;

    CHECK(sut.party_count == 20);
    CHECK(sut.counterparty_count == 50);
    CHECK(sut.portfolio_leaf_count == 16);
    CHECK(sut.business_unit_count == 30);
}

TEST_CASE("organisation_generation_options_country_can_be_set_to_us", tags) {
    auto lg(make_logger(test_suite));

    organisation_generation_options sut;
    sut.country = "US";
    BOOST_LOG_SEV(lg, info) << "organisation_generation_options country: "
                            << sut.country;

    CHECK(sut.country == "US");
}

TEST_CASE("organisation_generation_options_booleans_can_be_toggled", tags) {
    auto lg(make_logger(test_suite));

    organisation_generation_options sut;
    sut.generate_addresses = false;
    sut.generate_identifiers = false;
    BOOST_LOG_SEV(lg, info) << "organisation_generation_options generate_addresses: "
                            << sut.generate_addresses;

    CHECK(sut.generate_addresses == false);
    CHECK(sut.generate_identifiers == false);
}
