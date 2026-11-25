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
#include "ores.marketdata.service/app/derived_market_object_kind.hpp"
#include "ores.marketdata.service/app/publication_kind.hpp"
#include "ores.marketdata.service/app/publication_lifecycle.hpp"
#include "ores.testing/scoped_database_helper.hpp"
#include <catch2/catch_test_macros.hpp>
#include <stdexcept>
#include <string>
#include <vector>

constexpr const char* publication_lifecycle_tags = "[publication_lifecycle]";

using namespace ores::marketdata::service::app;

TEST_CASE("every declared kind is served by one implementation, and it names its kind",
          publication_lifecycle_tags) {
    for (const auto& declaration : kind_declarations) {
        const auto& act = publication_kind_of(declaration.kind);
        CHECK(act.kind() == declaration.kind);
    }
}

TEST_CASE("a_raise_that_names_no_subject_and_no_reason_is_refused_before_anything_is_written",
          publication_lifecycle_tags) {
    ores::testing::scoped_database_helper h;
    const auto ctx = h.context();

    CHECK_THROWS_AS(raise_publication(ctx, derived_kind::curve, {}, "A reason"),
                    std::invalid_argument);
    CHECK_THROWS_AS(
        raise_publication(ctx, derived_kind::fx_vol_surface, {{"USD-SOFR-FIXTURE", {}}}, ""),
        std::invalid_argument);
}

TEST_CASE("a_set_that_names_one_subject_twice_is_refused_by_every_kind",
          publication_lifecycle_tags) {
    ores::testing::scoped_database_helper h;
    const auto ctx = h.context();

    // The store would refuse the repeat at its natural key, in words a caller
    // cannot read, so the lifecycle refuses it for the whole set, before a kind
    // is asked for anything.
    const std::vector<publication_request> twice{{"USD-SOFR-FIXTURE", {}},
                                                 {"USD-SOFR-FIXTURE", {}}};
    for (const auto& declaration : kind_declarations) {
        try {
            (void)raise_publication(ctx, declaration.kind, twice, "A set that repeats itself");
            FAIL("A set that names one subject twice was raised.");
        } catch (const std::invalid_argument& e) {
            const auto message = std::string(e.what());
            CHECK(message.find("twice") != std::string::npos);
            CHECK(message.find("USD-SOFR-FIXTURE") != std::string::npos);
        }
    }
}

TEST_CASE("a_kind_the_declaration_does_not_carry_names_no_implementation",
          publication_lifecycle_tags) {
    // The declaration is the only place a kind is named, so there is no code path
    // that reaches an implementation beside it. Asked for a kind outside the
    // declaration, the lifecycle answers with the declaration and nothing else.
    CHECK(kind_declarations.size() == 3);
    for (std::size_t i = 0; i < kind_declarations.size(); ++i)
        for (std::size_t j = i + 1; j < kind_declarations.size(); ++j)
            CHECK(kind_declarations[i].kind != kind_declarations[j].kind);
}
