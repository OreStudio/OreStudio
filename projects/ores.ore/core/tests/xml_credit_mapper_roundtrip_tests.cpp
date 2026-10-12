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
#include "ores.ore.core/domain/credit_instrument_mapper.hpp"
#include "ores.ore.core/domain/domain.hpp"
#include "ores.ore.core/domain/trade_mapper.hpp"
#include "ores.platform/filesystem/file.hpp"
#include "ores.platform/time/datetime.hpp"
#include "ores.testing/project_root.hpp"
#include "ores.utility/decimal/decimal.hpp"
#include <catch2/catch_test_macros.hpp>

/**
 * @file xml_credit_mapper_roundtrip_tests.cpp
 * @brief Thing 3: mapper fidelity tests for credit instrument types.
 *
 * For each credit example file:
 *   1. Parse ORE XML into ores.ore domain types.
 *   2. Forward-map to credit_instrument via trade_mapper.
 *   3. Assert key economic fields are populated.
 *   4. Reverse-map back to ORE XSD trade.
 *   5. Assert the round-tripped XSD type is structurally populated.
 */

namespace {

const std::string_view test_suite("ores.ore.credit.mapper.roundtrip.tests");
const std::string tags("[ore][xml][mapper][roundtrip][credit]");

using ores::ore::domain::portfolio;
using ores::ore::domain::credit_instrument_mapper;
using ores::trading::domain::credit_instrument_data;
using namespace ores::logging;

std::filesystem::path example_path(const std::string& filename) {
    return ores::testing::project_root::resolve("external/ore/examples/Products/Example_Trades/" +
                                                filename);
}

credit_instrument_data load_and_map(const std::string& filename) {
    using ores::platform::filesystem::file;
    const std::string content = file::read_content(example_path(filename));
    portfolio p;
    ores::ore::domain::load_data(content, p);
    REQUIRE(!p.Trade.empty());
    auto r = ores::ore::domain::trade_mapper::map_credit_instrument(p.Trade.front());
    REQUIRE(r.has_value());
    return *r;
}

} // namespace


namespace {

// The domain holds a calendar date; the ORE XML holds its ISO-8601 spelling.
[[maybe_unused]] std::string ore_iso(const std::chrono::year_month_day& d) {
    return ores::platform::time::datetime::to_iso8601_date(d);
}

[[maybe_unused]] std::string ore_iso(const std::optional<std::chrono::year_month_day>& d) {
    return d ? ores::platform::time::datetime::to_iso8601_date(*d) : std::string{};
}

} // namespace

namespace {

template <typename Fact>
const Fact& fact_of(const ores::trading::domain::credit_instrument_data& data) {
    const auto* f = std::get_if<Fact>(&data.facts);
    REQUIRE(f != nullptr);
    return *f;
}

} // namespace

TEST_CASE("credit_mapper_roundtrip_cds", tags) {
    auto lg(make_logger(test_suite));
    const auto data = load_and_map("Credit_Default_Swap.xml");
    const auto& r = data.instrument;

    CHECK(r.identity.trade_type_code == "CreditDefaultSwap");
    CHECK(!r.reference_entity.empty());
    CHECK(r.start_date.has_value());
    CHECK(r.maturity_date.has_value());
    const auto& fact = fact_of<ores::trading::domain::credit_default_swap_instrument>(data);
    CHECK((fact.credit_curve_id || fact.issuer_id));

    // The economics are the premium leg's rows
    REQUIRE(data.legs.size() == 1);
    REQUIRE(!data.leg_amounts.empty());
    CHECK(data.leg_amounts.front().amount > ores::utility::decimal::decimal{});

    // Reverse roundtrip
    const auto rt = credit_instrument_mapper::reverse_cds(data);
    REQUIRE(rt.CreditDefaultSwapData);
    CHECK(rt.CreditDefaultSwapData->creditCurveIdType.CreditCurveId);

    BOOST_LOG_SEV(lg, info) << "CDS roundtrip passed. Reference: " << r.reference_entity;
}

TEST_CASE("credit_mapper_roundtrip_index_cds", tags) {
    auto lg(make_logger(test_suite));
    const auto data = load_and_map("Credit_Index_Credit_Default_Swap.xml");
    const auto& r = data.instrument;

    CHECK(r.identity.trade_type_code == "IndexCreditDefaultSwap");
    CHECK(!r.reference_entity.empty());
    const auto& fact = fact_of<ores::trading::domain::index_credit_default_swap_instrument>(data);
    CHECK(fact.credit_curve_id == r.reference_entity);
    REQUIRE(data.legs.size() == 1);

    // Reverse roundtrip
    const auto rt = credit_instrument_mapper::reverse_index_cds(data);
    REQUIRE(rt.IndexCreditDefaultSwapData);

    BOOST_LOG_SEV(lg, info) << "IndexCDS roundtrip passed. Index: " << fact.credit_curve_id;
}

TEST_CASE("credit_mapper_roundtrip_index_cds_option", tags) {
    auto lg(make_logger(test_suite));
    const auto data = load_and_map("Credit_Index_CDS_Option.xml");
    const auto& r = data.instrument;

    CHECK(r.identity.trade_type_code == "IndexCreditDefaultSwapOption");
    CHECK(!r.reference_entity.empty());
    const auto& fact =
        fact_of<ores::trading::domain::index_credit_default_swap_option_instrument>(data);
    REQUIRE(data.wrapped_index_swap);
    CHECK(data.wrapped_index_swap->credit_curve_id == r.reference_entity);
    CHECK(fact.knock_out == std::optional<bool>(false));
    CHECK(fact.index_term == std::optional<std::string>("5Y"));
    CHECK(fact.strike_type == std::optional<std::string>("Price"));
    REQUIRE(data.options.size() == 1);
    REQUIRE(data.strikes.size() == 1);
    CHECK(data.strikes.front().bare_value.has_value());
    CHECK(!data.schedule_dates.empty());

    // Reverse roundtrip
    const auto rt = credit_instrument_mapper::reverse_index_cds_option(data);
    REQUIRE(rt.IndexCreditDefaultSwapOptionData);
    CHECK(rt.IndexCreditDefaultSwapOptionData->Strike);
    REQUIRE(rt.IndexCreditDefaultSwapOptionData->OptionData.exerciseDatesGroup);

    BOOST_LOG_SEV(lg, info) << "IndexCDSOption roundtrip passed. Dates: "
                            << data.schedule_dates.size();
}

TEST_CASE("credit_mapper_roundtrip_credit_linked_swap", tags) {
    auto lg(make_logger(test_suite));
    const auto data = load_and_map("Credit_CreditLinkedSwap.xml");
    const auto& r = data.instrument;

    CHECK(r.identity.trade_type_code == "CreditLinkedSwap");
    CHECK(!r.reference_entity.empty());
    const auto& fact = fact_of<ores::trading::domain::credit_linked_swap_instrument>(data);
    CHECK(fact.credit_curve_id == r.reference_entity);

    // Reverse roundtrip
    const auto rt = credit_instrument_mapper::reverse_credit_linked_swap(data);
    REQUIRE(rt.CreditLinkedSwapData);

    BOOST_LOG_SEV(lg, info) << "CreditLinkedSwap roundtrip passed. Reference: "
                            << r.reference_entity;
}

TEST_CASE("credit_mapper_roundtrip_synthetic_cdo", tags) {
    auto lg(make_logger(test_suite));
    const auto data = load_and_map("Credit_Synthetic_CDO_refdata.xml");
    const auto& r = data.instrument;

    CHECK(r.identity.trade_type_code == "SyntheticCDO");
    const auto& fact = fact_of<ores::trading::domain::synthetic_cdo_instrument>(data);
    CHECK(!fact.qualifier.empty());
    CHECK(fact.protection_start_date.ok());
    CHECK(fact.detachment_point > fact.attachment_point);

    // Reverse roundtrip
    const auto rt = credit_instrument_mapper::reverse_synthetic_cdo(data);
    REQUIRE(rt.CdoData);
    const bool has_tranche =
        (rt.CdoData->AttachmentPoint != 0.0f || rt.CdoData->DetachmentPoint != 0.0f);
    CHECK(has_tranche);

    BOOST_LOG_SEV(lg, info) << "SyntheticCDO roundtrip passed. Attachment: "
                            << fact.attachment_point;
}

TEST_CASE("credit_mapper_roundtrip_rpa", tags) {
    auto lg(make_logger(test_suite));
    const auto data = load_and_map("Credit_RiskParticipationAgreement_on_Vanilla_Swap.xml");
    const auto& r = data.instrument;

    CHECK(r.identity.trade_type_code == "RiskParticipationAgreement");
    CHECK(!r.reference_entity.empty());
    CHECK(r.start_date.has_value());
    CHECK(r.maturity_date.has_value());
    const auto& fact = fact_of<ores::trading::domain::risk_participation_agreement_instrument>(data);
    CHECK(!fact.credit_curve_id.empty());

    // Reverse roundtrip
    const auto rt = credit_instrument_mapper::reverse_rpa(data);
    REQUIRE(rt.RiskParticipationAgreementData);
    CHECK(std::string(rt.RiskParticipationAgreementData->ProtectionStart) ==
          ores::platform::time::datetime::to_iso8601_date(*r.start_date));

    BOOST_LOG_SEV(lg, info) << "RPA roundtrip passed. Reference: " << r.reference_entity;
}

TEST_CASE("credit_mapper_leaves_the_exercise_dates_unset_without_exercise_dates", tags) {
    auto lg(make_logger(test_suite));

    // The XSD makes exerciseDatesGroup optional, so an option with no
    // ExerciseDates is legal. No schedule row is written, and the export
    // writes no spurious empty ExerciseDate element.
    using ores::platform::filesystem::file;
    std::string content = file::read_content(example_path("Credit_Index_CDS_Option.xml"));
    const std::string open_tag = "<ExerciseDates>";
    const std::string close_tag = "</ExerciseDates>";
    const auto begin = content.find(open_tag);
    REQUIRE(begin != std::string::npos);
    const auto end = content.find(close_tag, begin);
    REQUIRE(end != std::string::npos);
    content.erase(begin, end + close_tag.size() - begin);

    portfolio p;
    ores::ore::domain::load_data(content, p);
    REQUIRE(!p.Trade.empty());
    const auto r = ores::ore::domain::trade_mapper::map_credit_instrument(p.Trade.front());
    REQUIRE(r.has_value());
    CHECK(r->schedule_dates.empty());
    CHECK(r->schedules.empty());

    const auto rt = credit_instrument_mapper::reverse_index_cds_option(*r);
    REQUIRE(rt.IndexCreditDefaultSwapOptionData);
    CHECK(!rt.IndexCreditDefaultSwapOptionData->OptionData.exerciseDatesGroup);
}

namespace {

ores::utility::decimal::decimal dec(const std::string& text) {
    return ores::utility::decimal::decimal::from_string(text).value();
}

} // namespace

TEST_CASE("credit_mapper_holds_the_digits_of_the_option_strike", tags) {
    const auto data = load_and_map("Credit_Index_CDS_Option.xml");

    REQUIRE(data.strikes.size() == 1);
    REQUIRE(data.strikes.front().bare_value);
    CHECK(*data.strikes.front().bare_value == dec("1.0"));

    const auto rt = credit_instrument_mapper::reverse_index_cds_option(data);
    REQUIRE(rt.IndexCreditDefaultSwapOptionData);
    REQUIRE(rt.IndexCreditDefaultSwapOptionData->Strike);
    CHECK(dec(rt.IndexCreditDefaultSwapOptionData->Strike->lexical()) == dec("1.0"));
}

TEST_CASE("credit_mapper_writes_the_premium_leg_of_a_cds", tags) {
    const auto data = load_and_map("Credit_Default_Swap.xml");

    REQUIRE(data.legs.size() == 1);
    REQUIRE(data.legs.front().leg_group);
    CHECK(*data.legs.front().leg_group == "premium");
    CHECK(!data.leg_amounts.empty());
    CHECK(!data.leg_rates.empty());
    CHECK(data.constituents.empty());
}

TEST_CASE("credit_mapper_writes_the_basket_names_of_a_bespoke_index_cds", tags) {
    const auto data = load_and_map("Credit_Index_Credit_Default_Swap_Bespoke_Basket.xml");

    REQUIRE(data.constituents.size() == 3);
    CHECK(data.constituents.front().issuer_id == "CPTY_1");
    CHECK(data.constituents.front().sequence_number == 1);
    REQUIRE(data.constituents.front().credit_curve_id);
    CHECK(*data.constituents.front().credit_curve_id == "RED:027A8A|SNRFOR|USD|MR14");
    REQUIRE(data.constituents.front().notional);
    CHECK(*data.constituents.front().notional == dec("100000.0"));
    REQUIRE(data.constituents.front().currency);
    CHECK(*data.constituents.front().currency == "USD");

    const auto rt = credit_instrument_mapper::reverse_index_cds(data);
    REQUIRE(rt.IndexCreditDefaultSwapData);
    REQUIRE(rt.IndexCreditDefaultSwapData->BasketData);
    CHECK(rt.IndexCreditDefaultSwapData->BasketData->Name.size() == 3);
}

TEST_CASE("credit_mapper_groups_the_legs_of_a_credit_linked_swap", tags) {
    const auto data = load_and_map("Credit_CreditLinkedSwap.xml");

    REQUIRE(!data.legs.empty());
    for (const auto& leg : data.legs) {
        REQUIRE(leg.leg_group);
        CHECK((*leg.leg_group == "independent" || *leg.leg_group == "contingent" ||
               *leg.leg_group == "default" || *leg.leg_group == "recovery"));
    }
}

TEST_CASE("credit_mapper_holds_the_dropped_elements_of_a_cds", tags) {
    const auto data = load_and_map("Credit_Default_Swap.xml");
    const auto& fact = fact_of<ores::trading::domain::credit_default_swap_instrument>(data);

    CHECK(fact.protection_payment_time == std::optional<std::string>("atDefault"));
    REQUIRE(fact.upfront_fee);
    CHECK(*fact.upfront_fee == dec("0.0"));
    CHECK(fact.upfront_date.has_value());
    CHECK(fact.protection_start_date.has_value());

    const auto rt = credit_instrument_mapper::reverse_cds(data);
    REQUIRE(rt.CreditDefaultSwapData);
    REQUIRE(rt.CreditDefaultSwapData->UpfrontFee);
    CHECK(dec(rt.CreditDefaultSwapData->UpfrontFee->lexical()) == dec("0.0"));
    REQUIRE(rt.CreditDefaultSwapData->ProtectionPaymentTime);
    CHECK(std::string(*rt.CreditDefaultSwapData->ProtectionPaymentTime) == "atDefault");
    CHECK(static_cast<bool>(rt.CreditDefaultSwapData->UpfrontDate));
}

TEST_CASE("credit_mapper_holds_the_dropped_elements_of_an_index_cds", tags) {
    const auto data = load_and_map("Credit_Index_Credit_Default_Swap.xml");
    const auto& fact = fact_of<ores::trading::domain::index_credit_default_swap_instrument>(data);

    const auto rt = credit_instrument_mapper::reverse_index_cds(data);
    REQUIRE(rt.IndexCreditDefaultSwapData);
    CHECK(static_cast<bool>(rt.IndexCreditDefaultSwapData->ProtectionStart) ==
          fact.protection_start_date.has_value());
    CHECK(static_cast<bool>(rt.IndexCreditDefaultSwapData->UpfrontFee) == fact.upfront_fee.has_value());
    CHECK(static_cast<bool>(rt.IndexCreditDefaultSwapData->SettlesAccrual) ==
          fact.settles_accrual.has_value());
}

TEST_CASE("credit_mapper_holds_the_option_block_of_an_index_option", tags) {
    const auto data = load_and_map("Credit_Index_CDS_Option.xml");

    const auto rt = credit_instrument_mapper::reverse_index_cds_option(data);
    REQUIRE(rt.IndexCreditDefaultSwapOptionData);
    const auto& od = rt.IndexCreditDefaultSwapOptionData->OptionData;
    CHECK(std::string(od.LongShort) == data.options.front().long_short);
    CHECK(static_cast<bool>(rt.IndexCreditDefaultSwapOptionData->KnockOut));
    CHECK(static_cast<bool>(rt.IndexCreditDefaultSwapOptionData->IndexTerm));
    CHECK(static_cast<bool>(rt.IndexCreditDefaultSwapOptionData->StrikeType));
}

TEST_CASE("credit_mapper_holds_the_dropped_elements_of_a_synthetic_cdo", tags) {
    const auto data = load_and_map("Credit_Synthetic_CDO_refdata.xml");
    const auto& fact = fact_of<ores::trading::domain::synthetic_cdo_instrument>(data);

    const auto rt = credit_instrument_mapper::reverse_synthetic_cdo(data);
    REQUIRE(rt.CdoData);
    CHECK(static_cast<bool>(rt.CdoData->SettlesAccrual) == fact.settles_accrual.has_value());
    CHECK(static_cast<bool>(rt.CdoData->UpfrontDate) == fact.upfront_date.has_value());
    CHECK(static_cast<bool>(rt.CdoData->UpfrontFee) == fact.upfront_fee.has_value());
}

TEST_CASE("credit_mapper_holds_the_dropped_elements_of_a_credit_linked_swap", tags) {
    const auto data = load_and_map("Credit_CreditLinkedSwap.xml");
    const auto& fact = fact_of<ores::trading::domain::credit_linked_swap_instrument>(data);
    CHECK(fact.default_payment_time == std::optional<std::string>("atDefault"));

    const auto rt = credit_instrument_mapper::reverse_credit_linked_swap(data);
    REQUIRE(rt.CreditLinkedSwapData);
    REQUIRE(rt.CreditLinkedSwapData->DefaultPaymentTime);
    CHECK(std::string(*rt.CreditLinkedSwapData->DefaultPaymentTime) == "atDefault");
    CHECK(static_cast<bool>(rt.CreditLinkedSwapData->SettlesAccrual) == fact.settles_accrual.has_value());
}
