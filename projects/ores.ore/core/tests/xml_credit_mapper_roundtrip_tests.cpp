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
using ores::trading::domain::credit_instrument;
using namespace ores::logging;

std::filesystem::path example_path(const std::string& filename) {
    return ores::testing::project_root::resolve("external/ore/examples/Products/Example_Trades/" +
                                                filename);
}

credit_instrument load_and_map(const std::string& filename) {
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

TEST_CASE("credit_mapper_roundtrip_cds", tags) {
    auto lg(make_logger(test_suite));
    const auto r = load_and_map("Credit_Default_Swap.xml");

    CHECK(r.identity.trade_type_code == "CreditDefaultSwap");
    CHECK(!r.reference_entity.empty());
    CHECK(!r.currency.empty());
    CHECK(r.notional.to_double() > 0.0);
    CHECK(r.spread > ores::utility::decimal::decimal{});
    CHECK(r.start_date.ok());
    CHECK(r.maturity_date.ok());

    // Reverse roundtrip
    const auto rt = credit_instrument_mapper::reverse_cds(r);
    REQUIRE(rt.CreditDefaultSwapData);
    CHECK(rt.CreditDefaultSwapData->creditCurveIdType.CreditCurveId);

    BOOST_LOG_SEV(lg, info) << "CDS roundtrip passed. Reference: " << r.reference_entity;
}

TEST_CASE("credit_mapper_roundtrip_index_cds", tags) {
    auto lg(make_logger(test_suite));
    const auto r = load_and_map("Credit_Index_Credit_Default_Swap.xml");

    CHECK(r.identity.trade_type_code == "IndexCreditDefaultSwap");
    CHECK(!r.reference_entity.empty());
    CHECK(!r.index_name.empty());
    CHECK(!r.currency.empty());
    CHECK(r.notional.to_double() > 0.0);

    // Reverse roundtrip
    const auto rt = credit_instrument_mapper::reverse_index_cds(r);
    REQUIRE(rt.IndexCreditDefaultSwapData);

    BOOST_LOG_SEV(lg, info) << "IndexCDS roundtrip passed. Index: " << r.index_name;
}

TEST_CASE("credit_mapper_roundtrip_index_cds_option", tags) {
    auto lg(make_logger(test_suite));
    const auto r = load_and_map("Credit_Index_CDS_Option.xml");

    CHECK(r.identity.trade_type_code == "IndexCreditDefaultSwapOption");
    CHECK(!r.reference_entity.empty());
    CHECK(r.option_expiry_date.has_value());
    CHECK(r.option_strike.has_value());

    // Reverse roundtrip
    const auto rt = credit_instrument_mapper::reverse_index_cds_option(r);
    REQUIRE(rt.IndexCreditDefaultSwapOptionData);
    CHECK(rt.IndexCreditDefaultSwapOptionData->Strike);

    BOOST_LOG_SEV(lg, info) << "IndexCDSOption roundtrip passed. Expiry: "
                            << ore_iso(r.option_expiry_date);
}

TEST_CASE("credit_mapper_roundtrip_credit_linked_swap", tags) {
    auto lg(make_logger(test_suite));
    const auto r = load_and_map("Credit_CreditLinkedSwap.xml");

    CHECK(r.identity.trade_type_code == "CreditLinkedSwap");
    CHECK(!r.reference_entity.empty());
    CHECK(!r.linked_asset_code.empty());

    // Reverse roundtrip
    const auto rt = credit_instrument_mapper::reverse_credit_linked_swap(r);
    REQUIRE(rt.CreditLinkedSwapData);

    BOOST_LOG_SEV(lg, info) << "CreditLinkedSwap roundtrip passed. Reference: "
                            << r.reference_entity;
}

TEST_CASE("credit_mapper_roundtrip_synthetic_cdo", tags) {
    auto lg(make_logger(test_suite));
    const auto r = load_and_map("Credit_Synthetic_CDO_refdata.xml");

    CHECK(r.identity.trade_type_code == "SyntheticCDO");
    CHECK(r.tranche_attachment.has_value());
    CHECK(r.tranche_detachment.has_value());

    // Reverse roundtrip
    const auto rt = credit_instrument_mapper::reverse_synthetic_cdo(r);
    REQUIRE(rt.CdoData);
    const bool has_tranche =
        (rt.CdoData->AttachmentPoint != 0.0f || rt.CdoData->DetachmentPoint != 0.0f);
    CHECK(has_tranche);

    BOOST_LOG_SEV(lg, info) << "SyntheticCDO roundtrip passed. Attachment: "
                            << *r.tranche_attachment;
}

TEST_CASE("credit_mapper_roundtrip_rpa", tags) {
    auto lg(make_logger(test_suite));
    const auto r = load_and_map("Credit_RiskParticipationAgreement_on_Vanilla_Swap.xml");

    CHECK(r.identity.trade_type_code == "RiskParticipationAgreement");
    CHECK(!r.reference_entity.empty());
    CHECK(r.start_date.ok());
    CHECK(r.maturity_date.ok());

    // Reverse roundtrip
    const auto rt = credit_instrument_mapper::reverse_rpa(r);
    REQUIRE(rt.RiskParticipationAgreementData);

    BOOST_LOG_SEV(lg, info) << "RPA roundtrip passed. Reference: " << r.reference_entity;
}

TEST_CASE("credit_mapper_leaves_expiry_unset_without_exercise_dates", tags) {
    auto lg(make_logger(test_suite));

    // The XSD makes exerciseDatesGroup optional, so an option with no
    // ExerciseDates is legal. The expiry must stay unset rather than carry an
    // invalid date that exports as a spurious empty ExerciseDate element.
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
    CHECK(!r->option_expiry_date.has_value());

    const auto rt = credit_instrument_mapper::reverse_index_cds_option(*r);
    REQUIRE(rt.IndexCreditDefaultSwapOptionData);
    CHECK(!rt.IndexCreditDefaultSwapOptionData->OptionData.exerciseDatesGroup);
}
