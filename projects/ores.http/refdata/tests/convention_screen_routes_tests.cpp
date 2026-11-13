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
#include "ores.http.api/domain/http_request.hpp"
#include "ores.http.api/domain/http_response.hpp"
#include "ores.http/routes/refdata/convention_screen_routes.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.refdata.api/domain/convention_family_registry.hpp"
#include "route_answer.hpp"
#include "test_suite.hpp"
#include <algorithm>
#include <catch2/catch_test_macros.hpp>
#include <set>
#include <string>
#include <vector>

using ores::http::domain::http_method;
using ores::http::domain::http_request;
using ores::http::domain::http_response;
using ores::http::domain::http_status;
using ores::http::refdata::tests::answer_at;
using ores::http::routes::refdata::convention_screen_routes;
using namespace ores::logging;

namespace {

const convention_screen_routes::family&
family_named(const std::vector<convention_screen_routes::family>& all, std::string_view key) {
    const auto found = std::ranges::find(all, key, &convention_screen_routes::family::key);
    REQUIRE(found != all.end());
    return *found;
}

}

TEST_CASE("convention_families_are_the_registry_less_what_a_composite_stands_for",
          "[convention_screen]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing the families the screen lists against the registry";

    // The registry families each composite stands for, as the BFF stated them.
    const std::set<std::string> absorbed{"convention",
                                         "commodity_future_averaging_terms",
                                         "commodity_future_contract_terms",
                                         "commodity_future_expiry_anchor",
                                         "commodity_future_off_peak_index",
                                         "commodity_future_option_expiry",
                                         "cross_currency_basis_swap_terms",
                                         "cross_currency_basis_flat_leg",
                                         "cross_currency_basis_spread_leg",
                                         "cross_currency_fix_float_swap_terms",
                                         "cross_currency_fix_float_fixed_leg",
                                         "cross_currency_fix_float_float_leg",
                                         "tenor_basis_swap_pay_receive",
                                         "tenor_basis_swap_long_short"};
    std::vector<std::string> expected{"commodity-future",
                                      "cross-currency-basis",
                                      "cross-currency-fix-float",
                                      "tenor-basis-swap",
                                      "tenor",
                                      "currency-pair"};
    for (const auto& entry : ores::refdata::domain::convention_families) {
        if (absorbed.contains(std::string(entry.key)))
            continue;
        std::string key(entry.key);
        std::ranges::replace(key, '_', '-');
        expected.push_back(key);
    }
    std::ranges::sort(expected);

    const auto all = convention_screen_routes::families();
    std::vector<std::string> keys;
    for (const auto& family : all)
        keys.push_back(family.key);
    CHECK(keys == expected);

    for (const auto& entry : ores::refdata::domain::convention_families) {
        if (absorbed.contains(std::string(entry.key)))
            continue;
        std::string key(entry.key);
        std::ranges::replace(key, '_', '-');
        const auto& family = family_named(all, key);
        CHECK(family.entity == entry.entity);
        CHECK(family.shared == !entry.party_scoped);
        CHECK(family.kind_code.empty());
    }

    const auto& composite = family_named(all, "tenor-basis-swap");
    CHECK(composite.kind_code == "TENOR_BASIS_SWAP");
    CHECK_FALSE(composite.shared);
    CHECK(family_named(all, "tenor").shared);
    CHECK(family_named(all, "currency-pair").shared);

    std::vector<std::string> writable;
    for (const auto& family : all)
        if (family.writable)
            writable.push_back(family.key);
    CHECK(writable == std::vector<std::string>{"deposit", "swap"});
}

TEST_CASE("convention_central_row_has_its_groups_flattened", "[convention_screen]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing a central row's groups under their own names";

    const auto flat = convention_screen_routes::flatten_central(
        R"({"identity":{"id":"a","version":3,"party_id":"p"},"payload":{"kind_code":"K"},"note":"n","list":[1]})");
    CHECK(flat == R"({"id":"a","version":3,"party_id":"p","kind_code":"K","note":"n","list":[1]})");
}

TEST_CASE("convention_parts_lay_their_terms_over_the_central_row", "[convention_screen]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing the zip of a composite's parts";

    const std::string central = R"({"id":"a","version":3,"party_id":"p","kind_code":"K"})";
    const std::string item =
        R"({"key":{"id":"a"},"convention":{"id":"a","version":9},)"
        R"("pay_receive":[{"id":"x","version":7,"convention_id":"a","pay_tenor":"3M",)"
        R"("leg":{"party_id":"q","receive_tenor":"6M"}}],"long_short":[]})";
    const auto zipped = convention_screen_routes::zip_parts(central, item);
    CHECK(
        zipped ==
        R"({"id":"a","version":3,"party_id":"p","kind_code":"K","pay_tenor":"3M","receive_tenor":"6M"})");

    CHECK(convention_screen_routes::zip_parts(central, R"({"key":{"id":"a"},"long_short":[]})") ==
          central);
}

TEST_CASE("convention_routes_refuse_a_malformed_request_before_forwarding", "[convention_screen]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing each route's refusals";

    const auto missing = answer_at<convention_screen_routes>(
        http_method::get, "/api/conventions/nope", http_request{}, "/api/conventions/{family}");
    CHECK(missing.status == http_status::not_found);
    CHECK(missing.body.find("no convention family nope") != std::string::npos);

    for (const std::string limit : {"0", "501", "x", "-1"}) {
        http_request page;
        page.query_params["limit"] = limit;
        const auto answer = answer_at<convention_screen_routes>(
            http_method::get, "/api/conventions/deposit", page, "/api/conventions/{family}");
        CHECK(answer.status == http_status::bad_request);
    }

    http_request undrawn;
    undrawn.body = "{}";
    const auto unwritten = answer_at<convention_screen_routes>(
        http_method::put, "/api/conventions/cds", undrawn, "/api/conventions/{family}");
    CHECK(unwritten.status == http_status::bad_request);
    CHECK(unwritten.body.find("not drawn yet") != std::string::npos);

    http_request reasonless;
    reasonless.body =
        R"({"intent":{"reason_code":"  "},"version":null,"write":{"id":"00000000-0000-0000-0000-00000000000a"}})";
    const auto refused = answer_at<convention_screen_routes>(
        http_method::put, "/api/conventions/deposit", reasonless, "/api/conventions/{family}");
    CHECK(refused.status == http_status::bad_request);
    CHECK(refused.body.find("a reason") != std::string::npos);
}

TEST_CASE("convention_routes_forward_a_well_formed_request", "[convention_screen]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing that an accepted request reaches the service";

    // The client is not connected, so an accepted request fails at the forward step.
    CHECK(answer_at<convention_screen_routes>(http_method::get,
                                              "/api/conventions/deposit",
                                              http_request{},
                                              "/api/conventions/{family}")
              .status == http_status::bad_gateway);
    CHECK(answer_at<convention_screen_routes>(http_method::get,
                                              "/api/conventions/tenor-basis-swap",
                                              http_request{},
                                              "/api/conventions/{family}")
              .status == http_status::bad_gateway);

    http_request write;
    write.body = R"({"intent":{"reason_code":"system.new_record"},"version":null,)"
                 R"("write":{"id":"00000000-0000-0000-0000-00000000000a","fixed_frequency":"1Y",)"
                 R"("fixed_day_count_fraction":"A360","index":"EUR-EURIBOR-6M"}})";
    CHECK(answer_at<convention_screen_routes>(
              http_method::put, "/api/conventions/swap", write, "/api/conventions/{family}")
              .status == http_status::bad_gateway);

    // A count the service cannot give is null, not a failed page: the family is still there to
    // open.
    const auto families = answer_at<convention_screen_routes>(
        http_method::get, "/api/conventions/families", http_request{}, "/api/conventions/families");
    CHECK(families.status == http_status::ok);
    CHECK(families.body.starts_with(
        R"({"families":[{"key":"average-ois","entity":"average_ois_convention",)"
        R"("writable":false,"shared":false,"assetClass":null,"count":null})"));
}
