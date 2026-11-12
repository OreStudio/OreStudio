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
#include "ores.marketdata.api/conventions/convention_lookup.hpp"
#include "ores.marketdata.api/datum/market_datum.hpp"
#include "ores.marketdata.api/datum/ore_key_codec.hpp"
#include "ores.trading.api/builders/builder_registry.hpp"
#include "ores.trading.api/builders/credit_builders.hpp"
#include <catch2/catch_test_macros.hpp>
#include <chrono>
#include <string>

using namespace ores::trading::builders;
using namespace std::chrono;
namespace conv = ores::marketdata::conventions;
namespace md = ores::marketdata::datum;
using ores::utility::decimal::decimal;

namespace {

const auto credit_tags("[builders][credit]");
constexpr auto credit_lei = "529900T8BM49AURSDO55";
constexpr auto credit_euribor_6m = "oresmd://ir/EUR?type=fixing&index=ibor&name=EURIBOR&tenor=6M";
constexpr auto credit_euribor = "oresmd://ir/EUR?type=fixing&index=ibor&name=EURIBOR";

conv::convention_set credit_conventions() {
    ores::refdata::domain::cds_convention cds;
    cds.id = "CDS-STANDARD-CONVENTIONS";
    cds.calendar = "WeekendsOnly";
    cds.frequency = "Quarterly";
    cds.payment_convention = "Following";
    cds.rule = "CDS2015";
    cds.day_count_fraction = "ACT/360";
    cds.settles_accrual = true;
    cds.pays_at_default_time = true;
    ores::refdata::domain::ibor_index_convention index;
    index.id = "EUR-EURIBOR";
    index.fixing_calendar = "TARGET";
    index.settlement_days = 2;
    index.day_count_fraction = "A360";
    index.business_day_convention = "MF";
    index.oresmd_uri = credit_euribor;
    ores::refdata::domain::swap_convention swap;
    swap.id = "EUR-6M-SWAP";
    swap.fixed_calendar = "TARGET";
    swap.fixed_frequency = "Annual";
    swap.fixed_convention = "MF";
    swap.fixed_day_count_fraction = "30/360";
    swap.index = "EUR-EURIBOR-6M";
    swap.oresmd_uri = credit_euribor_6m;
    return {.ibor_indices = {index}, .swaps = {swap}, .cds = {cds}};
}

md::market_reference credit_reference(const char* uri) {
    auto parsed = md::market_reference::parse(uri);
    REQUIRE(parsed);
    return *parsed;
}

/// The credit spread series of the entity, as ORE keys its quotes.
md::market_reference credit_curve() {
    const auto quote =
        md::ore_key_codec::read("CDS/CREDIT_SPREAD/529900T8BM49AURSDO55/SNRFOR/EUR/5Y");
    REQUIRE(quote);
    auto series = md::market_reference::of(md::series_of(*quote));
    REQUIRE(series);
    return *series;
}

credit_protection_terms protection_terms(protection side = protection::buy) {
    return {.notional = *decimal::from_string("10000000"),
            .currency = std::string("EUR"),
            .start = 2026y / 1 / 5,
            .maturity = 2031y / 1 / 5,
            .running_spread = *decimal::from_string("0.01"),
            .reference_entity = std::string(credit_lei),
            .credit_curve = credit_curve(),
            .side = side,
            .convention = std::string("CDS-STANDARD-CONVENTIONS")};
}

risk_participation_agreement_terms participation_terms() {
    return {.underlying = {.notional = *decimal::from_string("10000000"),
                           .currency = std::string("EUR"),
                           .start = 2026y / 1 / 5,
                           .maturity = 2036y / 1 / 5,
                           .fixed_rate = *decimal::from_string("0.02"),
                           .index = credit_reference(credit_euribor_6m),
                           .fixed_side = side::pay},
            .participation_rate = 0.5,
            .reference_entity = std::string(credit_lei),
            .credit_curve = credit_curve(),
            .side = protection::buy,
            .fee = *decimal::from_string("25000"),
            .convention = std::string("CDS-STANDARD-CONVENTIONS")};
}

}

TEST_CASE("a_cds_takes_its_premium_leg_from_the_cds_convention", credit_tags) {
    conv::convention_lookup lookup(credit_conventions());
    const auto cds = credit_default_swap_builder(lookup, protection_terms()).build();
    REQUIRE(cds.has_value());

    CHECK(cds->instrument.reference_entity_id_type == "LEI");
    CHECK(cds->instrument.reference_entity_id == credit_lei);
    REQUIRE(cds->credit_default_swap_instruments.size() == 1);
    const auto& fact = cds->credit_default_swap_instruments.front();
    CHECK(fact.credit_curve_id == credit_curve().uri());
    CHECK(fact.settles_accrual == true);
    CHECK(fact.protection_payment_time == "atDefault");

    REQUIRE(cds->legs.size() == 1);
    const auto& leg = cds->legs.front();
    CHECK(leg.leg_group == "premium");
    CHECK(leg.payer == true);
    CHECK(leg.leg_type_code == "Fixed");
    CHECK(leg.day_count_fraction_code == "A360");
    REQUIRE(cds->leg_rates.size() == 1);
    CHECK(cds->leg_rates.front().value == *decimal::from_string("0.01"));

    REQUIRE(cds->schedules.size() == 1);
    const auto& schedule = cds->schedules.front();
    CHECK(schedule.tenor == "3M");
    CHECK(schedule.calendar == "WeekendsOnly");
    CHECK(schedule.rule == "CDS2015");
    CHECK(schedule.convention == "Following");
}

TEST_CASE("selling_protection_receives_the_premium", credit_tags) {
    conv::convention_lookup lookup(credit_conventions());
    const auto cds =
        credit_default_swap_builder(lookup, protection_terms(protection::sell)).build();
    REQUIRE(cds.has_value());
    CHECK(cds->legs.front().payer == false);
}

TEST_CASE("a_convention_the_lookup_lacks_fails_and_names_the_term", credit_tags) {
    conv::convention_lookup lookup(conv::convention_set{});
    const auto cds = credit_default_swap_builder(lookup, protection_terms()).build();
    REQUIRE_FALSE(cds.has_value());
    CHECK(cds.error().term == "convention");
}

TEST_CASE("a_reference_entity_that_is_no_lei_fails_and_names_the_term", credit_tags) {
    conv::convention_lookup lookup(credit_conventions());
    auto terms = protection_terms();
    terms.reference_entity = std::string("ACME");
    const auto cds = credit_default_swap_builder(lookup, terms).build();
    REQUIRE_FALSE(cds.has_value());
    CHECK(cds.error().term == "reference_entity");
}

TEST_CASE("an_index_cds_states_its_fact_on_the_index_row", credit_tags) {
    conv::convention_lookup lookup(credit_conventions());
    const auto index = index_credit_default_swap_builder(lookup, protection_terms()).build();
    REQUIRE(index.has_value());
    REQUIRE(index->index_credit_default_swap_instruments.size() == 1);
    CHECK(index->index_credit_default_swap_instruments.front().credit_curve_id ==
          credit_curve().uri());
    CHECK(index->credit_default_swap_instruments.empty());
    CHECK(index->legs.size() == 1);
}

TEST_CASE("an_index_option_wraps_the_swap_and_exercises_at_its_expiry", credit_tags) {
    conv::convention_lookup lookup(credit_conventions());
    const index_credit_default_swap_option_terms terms{.swap = protection_terms(),
                                                       .expiry = 2026y / 1 / 4,
                                                       .strike = *decimal::from_string("1"),
                                                       .index_term = std::string("5Y"),
                                                       .position = option_position::long_option};
    const auto option = index_credit_default_swap_option_builder(lookup, terms).build();
    REQUIRE(option.has_value());

    REQUIRE(option->wrapped_index_swap.has_value());
    CHECK(option->wrapped_index_swap->credit_curve_id == credit_curve().uri());
    CHECK(option->index_credit_default_swap_instruments.empty());
    REQUIRE(option->index_credit_default_swap_option_instruments.size() == 1);
    CHECK(option->index_credit_default_swap_option_instruments.front().index_term == "5Y");
    REQUIRE(option->options.size() == 1);
    CHECK(option->options.front().long_short == "Long");
    REQUIRE(option->schedule_dates.size() == 1);
    CHECK(option->schedule_dates.front().schedule_date == 2026y / 1 / 4);
    REQUIRE(option->strikes.size() == 1);
    CHECK(option->strikes.front().strike_role == "single");
}

TEST_CASE("an_index_option_that_expires_after_the_swap_starts_fails", credit_tags) {
    conv::convention_lookup lookup(credit_conventions());
    const index_credit_default_swap_option_terms terms{.swap = protection_terms(),
                                                       .expiry = 2026y / 1 / 5,
                                                       .strike = *decimal::from_string("1"),
                                                       .index_term = std::string("5Y"),
                                                       .position = option_position::long_option};
    const auto option = index_credit_default_swap_option_builder(lookup, terms).build();
    REQUIRE_FALSE(option.has_value());
    CHECK(option.error().term == "expiry");
}

TEST_CASE("a_credit_linked_swap_is_funded_at_the_start_and_repaid_while_the_entity_survives",
          credit_tags) {
    conv::convention_lookup lookup(credit_conventions());
    const auto swap =
        credit_linked_swap_builder(lookup, protection_terms(protection::sell)).build();
    REQUIRE(swap.has_value());
    REQUIRE(swap->legs.size() == 3);
    const auto& funding = swap->legs[0];
    CHECK(funding.leg_group == "independent");
    CHECK(funding.payer == true);
    CHECK(funding.currency == "EUR");
    CHECK(swap->legs[1].leg_group == "contingent");
    CHECK(swap->legs[1].leg_type_code == "Fixed");
    CHECK(swap->legs[1].payer == false);
    CHECK(swap->legs[2].leg_group == "contingent");
    CHECK(swap->legs[2].leg_type_code == "Cashflow");
    CHECK(swap->legs[2].payer == false);
    REQUIRE(swap->leg_amounts.size() == 3);
    CHECK(swap->leg_amounts[0].start_date == 2026y / 1 / 5);
    CHECK(swap->leg_amounts[2].start_date == 2031y / 1 / 5);
    REQUIRE(swap->credit_linked_swap_instruments.size() == 1);
    CHECK(swap->credit_linked_swap_instruments.front().default_payment_time == "atDefault");
}

TEST_CASE("an_rpa_wraps_a_vanilla_swap_and_pays_its_fee_at_the_start", credit_tags) {
    conv::convention_lookup lookup(credit_conventions());
    const auto rpa = risk_participation_agreement_builder(lookup, participation_terms()).build();
    REQUIRE(rpa.has_value());
    REQUIRE(rpa->legs.size() == 3);
    CHECK(rpa->legs[0].leg_group == "underlying");
    CHECK(rpa->legs[1].leg_group == "underlying");
    CHECK(rpa->legs[1].floating_index_code == credit_euribor_6m);
    CHECK(rpa->legs[2].leg_group == "protection_fee");
    CHECK(rpa->legs[2].identity.leg_number == 3);
    CHECK(rpa->legs[2].payer == true);
    CHECK(rpa->schedules.size() == 2);
    REQUIRE(rpa->risk_participation_agreement_instruments.size() == 1);
    CHECK(rpa->risk_participation_agreement_instruments.front().participation_rate == 0.5);
}

TEST_CASE("an_rpa_whose_underlying_cannot_be_built_names_the_underlying_term", credit_tags) {
    auto set = credit_conventions();
    set.swaps.clear();
    conv::convention_lookup lookup(set);
    const auto rpa = risk_participation_agreement_builder(lookup, participation_terms()).build();
    REQUIRE_FALSE(rpa.has_value());
    CHECK(rpa.error().term == "underlying.index");
}

TEST_CASE("a_participation_rate_above_one_fails_and_names_the_term", credit_tags) {
    conv::convention_lookup lookup(credit_conventions());
    auto terms = participation_terms();
    terms.participation_rate = 1.5;
    const auto rpa = risk_participation_agreement_builder(lookup, terms).build();
    REQUIRE_FALSE(rpa.has_value());
    CHECK(rpa.error().term == "participation_rate");
}

TEST_CASE("the_registry_names_every_credit_builder", credit_tags) {
    const auto types = built_trade_types();
    for (const auto* type : {"CreditDefaultSwap",
                             "IndexCreditDefaultSwap",
                             "IndexCreditDefaultSwapOption",
                             "CreditLinkedSwap",
                             "RiskParticipationAgreement"})
        CHECK(types.contains(type));
}
