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
#include "ores.marketdata.core/terms/provider_terms.hpp"
#include <catch2/catch_test_macros.hpp>
#include <chrono>

namespace {

using namespace ores::marketdata;

domain::provider public_provider() {
    domain::provider p;
    p.code = "ecb";
    p.kind = "reference";
    p.terms_url = "https://www.ecb.europa.eu/services/using-our-site/disclaimer/html/index.en.html";
    p.terms_credit = "Source: ECB statistics";
    p.allows_storage = true;
    return p;
}

}

TEST_CASE("a_provider_with_no_terms_page_may_always_be_fetched", "[terms][provider_terms]") {
    domain::provider vendor;
    vendor.code = "bloomberg";

    CHECK_FALSE(terms::has_terms(vendor));
    CHECK(terms::may_fetch(vendor));
}

TEST_CASE("a_provider_with_terms_is_fetched_only_once_they_are_accepted",
          "[terms][provider_terms]") {
    const auto p = public_provider();
    CHECK_FALSE(terms::terms_accepted(p));
    CHECK_FALSE(terms::may_fetch(p));

    const auto accepted = terms::accept_terms(p, "super_admin", std::chrono::system_clock::now());

    CHECK(terms::terms_accepted(accepted));
    CHECK(terms::may_fetch(accepted));
    CHECK(accepted.terms_accepted_by == "super_admin");
    CHECK(accepted.terms_accepted_at.has_value());
}

TEST_CASE("a_change_of_the_terms_clears_the_acceptance_and_a_change_elsewhere_does_not",
          "[terms][provider_terms]") {
    const auto accepted =
        terms::accept_terms(public_provider(), "super_admin", std::chrono::system_clock::now());

    auto described = accepted;
    described.description = "Euro reference rates";
    CHECK(terms::may_fetch(described));

    auto new_page = accepted;
    new_page.terms_url += "?v=2";
    CHECK_FALSE(terms::may_fetch(new_page));

    auto new_credit = accepted;
    new_credit.terms_credit = "Source: European Central Bank";
    CHECK_FALSE(terms::may_fetch(new_credit));
}

TEST_CASE("accepted_terms_that_forbid_storage_still_forbid_the_fetch", "[terms][provider_terms]") {
    auto p = public_provider();
    p.allows_storage = false;
    const auto accepted = terms::accept_terms(p, "super_admin", std::chrono::system_clock::now());

    CHECK(terms::terms_accepted(accepted));
    CHECK_FALSE(terms::may_fetch(accepted));
}
