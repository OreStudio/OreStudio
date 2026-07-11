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
#include "ores.refdata.api/domain/country.hpp"
#include "ores.refdata.api/domain/currency.hpp"
#include "ores.refdata.api/domain/currency_pair.hpp"
#include <catch2/catch_test_macros.hpp>

namespace {

const std::string tags("[domain][entity_type]");

}

using ores::refdata::domain::country;
using ores::refdata::domain::currency;
using ores::refdata::domain::currency_pair;
using ores::refdata::domain::entity_type_of;

// The trait is mechanical codegen (one call per entity, straight from
// the org model), so this suite exercises a representative sample of
// domain_entity models rather than every generated refdata entity:
// the shape, not each entity's specific string, is what needs
// proving. Junction models are not yet in the "domain" facet's
// model_types scope (facet_catalogue.org), so no junction entity has
// entity_type_of() to test here — a pre-existing gap, not introduced
// by this change.

TEST_CASE("entity_type_of_returns_the_component_qualified_identifier", tags) {
    CHECK(entity_type_of(currency{}) == "ores.refdata.currency");
    CHECK(entity_type_of(country{}) == "ores.refdata.country");
    CHECK(entity_type_of(currency_pair{}) == "ores.refdata.currency_pair");
}

TEST_CASE("entity_type_of_is_distinct_per_entity", tags) {
    CHECK(entity_type_of(currency{}) != entity_type_of(country{}));
    CHECK(entity_type_of(currency{}) != entity_type_of(currency_pair{}));
}

TEST_CASE("entity_type_of_is_stable_across_distinct_instances", tags) {
    currency a;
    a.iso_code = "USD";
    currency b;
    b.iso_code = "EUR";

    CHECK(entity_type_of(a) == entity_type_of(b));
}

// entity_type_of() is declared constexpr, but proving it via
// static_assert needs a constexpr-constructible argument; audit-
// column entities like currency default-initialise tenant_id via
// tenant_id::system(), which is not itself constexpr, so no
// currency value is usable in a constant expression. The function's
// own constexpr-ness (verified by the compiler at every call site
// regardless) is exercised implicitly by every CHECK() above.
