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
#include "ores.synthetic.api/domain/generated_organisation.hpp"

#include <catch2/catch_test_macros.hpp>
#include "ores.logging/make_logger.hpp"

namespace {

const std::string_view test_suite("ores.synthetic.tests");
const std::string tags("[domain][generated_organisation]");

}

using ores::synthetic::domain::generated_organisation;
using namespace ores::logging;

TEST_CASE("default_constructed_generated_organisation_has_zero_seed_and_empty_vectors", tags) {
    auto lg(make_logger(test_suite));

    const generated_organisation sut;
    BOOST_LOG_SEV(lg, info) << "Default generated_organisation seed: " << sut.seed;

    CHECK(sut.seed == 0);
    CHECK(sut.parties.empty());
    CHECK(sut.party_contacts.empty());
    CHECK(sut.party_identifiers.empty());
    CHECK(sut.counterparties.empty());
    CHECK(sut.counterparty_contacts.empty());
    CHECK(sut.counterparty_identifiers.empty());
    CHECK(sut.party_counterparties.empty());
    CHECK(sut.business_unit_types.empty());
    CHECK(sut.business_units.empty());
    CHECK(sut.portfolios.empty());
    CHECK(sut.books.empty());
}

TEST_CASE("generated_organisation_seed_can_be_set", tags) {
    auto lg(make_logger(test_suite));

    generated_organisation sut;
    sut.seed = 999ULL;
    BOOST_LOG_SEV(lg, info) << "generated_organisation seed: " << sut.seed;

    CHECK(sut.seed == 999ULL);
}
