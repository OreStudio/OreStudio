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
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */
#include "ores.logging/make_logger.hpp"
#include "ores.marketdata.api/conventions/convention_lookup.hpp"
#include "test_suite.hpp"
#include <catch2/catch_test_macros.hpp>
#include <string>

namespace {

using namespace ores::logging;
using ores::marketdata::conventions::convention_lookup;
using ores::marketdata::conventions::convention_set;
using ores::marketdata::conventions::lookup_refusal_kind;
using ores::marketdata::datum::market_reference;
namespace rd = ores::refdata::domain;

constexpr auto euribor_family = "oresmd://ir/EUR?type=fixing&index=ibor&name=EURIBOR";
constexpr auto euribor_six_months = "oresmd://ir/EUR?type=fixing&index=ibor&name=EURIBOR&tenor=6M";
constexpr auto euribor_three_months =
    "oresmd://ir/EUR?type=fixing&index=ibor&name=EURIBOR&tenor=3M";
constexpr auto cms_ten_years = "oresmd://ir/EUR?type=fixing&index=swap&tenor=10Y";
constexpr auto sofr_index = "oresmd://ir/USD?type=fixing&index=ibor&name=SOFR";

market_reference identifier(const char* uri) {
    auto parsed = market_reference::parse(uri);
    REQUIRE(parsed);
    return *parsed;
}

template <typename Convention>
Convention convention_at(std::string id, std::string uri) {
    Convention convention;
    convention.id = std::move(id);
    convention.oresmd_uri = std::move(uri);
    return convention;
}

convention_set conventions() {
    convention_set set;
    set.ibor_indices.push_back(
        convention_at<rd::ibor_index_convention>("EUR-EURIBOR", euribor_family));
    set.overnight_indices.push_back(
        convention_at<rd::overnight_index_convention>("USD-SOFR", sofr_index));
    set.swaps.push_back(
        convention_at<rd::swap_convention>("EUR-EURIBOR-6M-SWAP", euribor_six_months));
    set.ois_swaps.push_back(convention_at<rd::ois_convention>("USD-SOFR-OIS", sofr_index));
    return set;
}

}

TEST_CASE("an index tenor is defined by its family's convention", "[marketdata][conventions]") {
    auto lg(make_logger(test_suite));
    const convention_lookup lookup(conventions());

    for (const auto* uri : {euribor_six_months, euribor_three_months, euribor_family}) {
        INFO("index: " << uri);
        const auto definition = lookup.index_definition_of(identifier(uri));
        REQUIRE(definition);
        const auto* ibor = std::get_if<rd::ibor_index_convention>(&*definition);
        REQUIRE(ibor != nullptr);
        CHECK(ibor->id == "EUR-EURIBOR");
    }
}

TEST_CASE("an overnight index is defined by its overnight convention",
          "[marketdata][conventions]") {
    auto lg(make_logger(test_suite));
    const convention_lookup lookup(conventions());

    const auto definition = lookup.index_definition_of(identifier(sofr_index));

    REQUIRE(definition);
    const auto* overnight = std::get_if<rd::overnight_index_convention>(&*definition);
    REQUIRE(overnight != nullptr);
    CHECK(overnight->id == "USD-SOFR");
}

TEST_CASE("a swap convention matches the index tenor it uses, not another",
          "[marketdata][conventions]") {
    auto lg(make_logger(test_suite));
    const convention_lookup lookup(conventions());

    const auto six_months = lookup.swap_convention_of(identifier(euribor_six_months));
    REQUIRE(six_months);
    CHECK(six_months->id == "EUR-EURIBOR-6M-SWAP");

    const auto three_months = lookup.swap_convention_of(identifier(euribor_three_months));
    REQUIRE(!three_months);
    CHECK(three_months.error().kind == lookup_refusal_kind::no_match);
    CHECK(three_months.error().reference == euribor_three_months);
    CHECK(three_months.error().reason == "no swap convention names it");

    const auto ois = lookup.ois_convention_of(identifier(sofr_index));
    REQUIRE(ois);
    CHECK(ois->id == "USD-SOFR-OIS");
}

TEST_CASE("two conventions naming one address are refused, naming both",
          "[marketdata][conventions]") {
    auto lg(make_logger(test_suite));
    auto set = conventions();
    set.swaps.push_back(
        convention_at<rd::swap_convention>("EUR-EURIBOR-6M-SWAP-OLD", euribor_six_months));
    const convention_lookup lookup(std::move(set));

    const auto found = lookup.swap_convention_of(identifier(euribor_six_months));

    REQUIRE(!found);
    CHECK(found.error().kind == lookup_refusal_kind::more_than_one_match);
    CHECK(found.error().reason.find("EUR-EURIBOR-6M-SWAP and EUR-EURIBOR-6M-SWAP-OLD") !=
          std::string::npos);
}

TEST_CASE("one address in both the Ibor and the overnight conventions is refused",
          "[marketdata][conventions]") {
    auto lg(make_logger(test_suite));
    convention_set set;
    set.ibor_indices.push_back(
        convention_at<rd::ibor_index_convention>("USD-SOFR-IBOR", sofr_index));
    set.overnight_indices.push_back(
        convention_at<rd::overnight_index_convention>("USD-SOFR-OVERNIGHT", sofr_index));
    const convention_lookup lookup(std::move(set));

    const auto found = lookup.index_definition_of(identifier(sofr_index));

    REQUIRE(!found);
    CHECK(found.error().kind == lookup_refusal_kind::more_than_one_match);
    CHECK(found.error().reason.find("USD-SOFR-IBOR") != std::string::npos);
    CHECK(found.error().reason.find("USD-SOFR-OVERNIGHT") != std::string::npos);
}

TEST_CASE("an index family with no defining convention is not this lookup's question",
          "[marketdata][conventions]") {
    auto lg(make_logger(test_suite));
    const convention_lookup lookup(conventions());

    const auto found = lookup.index_definition_of(identifier(cms_ten_years));

    REQUIRE(!found);
    CHECK(found.error().kind == lookup_refusal_kind::family_the_lookup_does_not_answer);
    CHECK(found.error().reason.find("swap") != std::string::npos);
}

TEST_CASE("a convention whose address the grammar refuses is named, not read as absent",
          "[marketdata][conventions]") {
    auto lg(make_logger(test_suite));
    auto set = conventions();
    set.swaps.push_back(convention_at<rd::swap_convention>(
        "EUR-EURIBOR-6M-SWAP-REORDERED",
        "oresmd://ir/EUR?index=ibor&name=EURIBOR&type=fixing&tenor=6M"));
    const convention_lookup lookup(std::move(set));

    const auto found = lookup.swap_convention_of(identifier(euribor_six_months));

    REQUIRE(found);
    CHECK(found->id == "EUR-EURIBOR-6M-SWAP");
    const auto missing = lookup.swap_convention_of(identifier(euribor_three_months));
    REQUIRE(!missing);
    CHECK(missing.error().reason.find("EUR-EURIBOR-6M-SWAP-REORDERED") != std::string::npos);
    CHECK(missing.error().reason.find("the grammar refuses") != std::string::npos);
}

TEST_CASE("a reference to a datum has no index convention", "[marketdata][conventions]") {
    auto lg(make_logger(test_suite));
    const convention_lookup lookup(conventions());
    const auto quote =
        identifier("oresmd://fx/EUR?type=quote&instrument=fx_spot&quote=rate&ccy=USD");

    CHECK(!lookup.index_definition_of(quote));
    CHECK(!lookup.swap_convention_of(quote));
    CHECK(lookup.swap_convention_of(quote).error().kind == lookup_refusal_kind::addresses_no_index);
    CHECK(lookup.swap_convention_of(quote).error().reason ==
          "the address names a datum, not an index");
}

TEST_CASE("a convention with no address is never found", "[marketdata][conventions]") {
    auto lg(make_logger(test_suite));
    convention_set set;
    set.ibor_indices.push_back(rd::ibor_index_convention{});
    set.ibor_indices.back().id = "EUR-EURIBOR";
    const convention_lookup lookup(std::move(set));

    CHECK(!lookup.index_definition_of(identifier(euribor_six_months)));
}
