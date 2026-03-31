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
#include "ores.ore/domain/domain.hpp"
#include "ores.ore/domain/trade_mapper.hpp"
#include "ores.ore/domain/credit_instrument_mapper.hpp"

#include <catch2/catch_test_macros.hpp>
#include "ores.logging/make_logger.hpp"
#include "ores.platform/filesystem/file.hpp"
#include "ores.testing/project_root.hpp"

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
using ores::ore::domain::credit_mapping_result;
using namespace ores::logging;

std::filesystem::path example_path(const std::string& filename) {
    return ores::testing::project_root::resolve(
        "external/ore/examples/Products/Example_Trades/" + filename);
}

credit_mapping_result load_and_map(const std::string& filename) {
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

TEST_CASE("credit_mapper_roundtrip_cds", tags) {
    auto lg(make_logger(test_suite));
    const auto r = load_and_map("Credit_Default_Swap.xml");

    CHECK(r.instrument.trade_type_code == "CreditDefaultSwap");
    CHECK(!r.instrument.reference_entity.empty());
    CHECK(!r.instrument.currency.empty());
    CHECK(r.instrument.notional > 0.0);
    CHECK(r.instrument.spread > 0.0);
    CHECK(!r.instrument.start_date.empty());
    CHECK(!r.instrument.maturity_date.empty());

    // Reverse roundtrip
    const auto rt = credit_instrument_mapper::reverse_cds(r.instrument);
    REQUIRE(rt.CreditDefaultSwapData);
    CHECK(rt.CreditDefaultSwapData->creditCurveIdType.CreditCurveId);

    BOOST_LOG_SEV(lg, info) << "CDS roundtrip passed. Reference: "
                            << r.instrument.reference_entity;
}

TEST_CASE("credit_mapper_roundtrip_index_cds", tags) {
    auto lg(make_logger(test_suite));
    const auto r = load_and_map("Credit_Index_Credit_Default_Swap.xml");

    CHECK(r.instrument.trade_type_code == "IndexCreditDefaultSwap");
    CHECK(!r.instrument.reference_entity.empty());
    CHECK(!r.instrument.index_name.empty());
    CHECK(!r.instrument.currency.empty());
    CHECK(r.instrument.notional > 0.0);

    // Reverse roundtrip
    const auto rt = credit_instrument_mapper::reverse_index_cds(r.instrument);
    REQUIRE(rt.IndexCreditDefaultSwapData);

    BOOST_LOG_SEV(lg, info) << "IndexCDS roundtrip passed. Index: "
                            << r.instrument.index_name;
}

TEST_CASE("credit_mapper_roundtrip_index_cds_option", tags) {
    auto lg(make_logger(test_suite));
    const auto r = load_and_map("Credit_Index_CDS_Option.xml");

    CHECK(r.instrument.trade_type_code == "IndexCreditDefaultSwapOption");
    CHECK(!r.instrument.reference_entity.empty());
    CHECK(!r.instrument.option_expiry_date.empty());
    CHECK(r.instrument.option_strike.has_value());

    // Reverse roundtrip
    const auto rt = credit_instrument_mapper::reverse_index_cds_option(r.instrument);
    REQUIRE(rt.IndexCreditDefaultSwapOptionData);
    CHECK(rt.IndexCreditDefaultSwapOptionData->Strike);

    BOOST_LOG_SEV(lg, info) << "IndexCDSOption roundtrip passed. Expiry: "
                            << r.instrument.option_expiry_date;
}

TEST_CASE("credit_mapper_roundtrip_credit_linked_swap", tags) {
    auto lg(make_logger(test_suite));
    const auto r = load_and_map("Credit_CreditLinkedSwap.xml");

    CHECK(r.instrument.trade_type_code == "CreditLinkedSwap");
    CHECK(!r.instrument.reference_entity.empty());
    CHECK(!r.instrument.linked_asset_code.empty());

    // Reverse roundtrip
    const auto rt = credit_instrument_mapper::reverse_credit_linked_swap(r.instrument);
    REQUIRE(rt.CreditLinkedSwapData);

    BOOST_LOG_SEV(lg, info) << "CreditLinkedSwap roundtrip passed. Reference: "
                            << r.instrument.reference_entity;
}

TEST_CASE("credit_mapper_roundtrip_synthetic_cdo", tags) {
    auto lg(make_logger(test_suite));
    const auto r = load_and_map("Credit_Synthetic_CDO_refdata.xml");

    CHECK(r.instrument.trade_type_code == "SyntheticCDO");
    CHECK(r.instrument.tranche_attachment.has_value());
    CHECK(r.instrument.tranche_detachment.has_value());

    // Reverse roundtrip
    const auto rt = credit_instrument_mapper::reverse_synthetic_cdo(r.instrument);
    REQUIRE(rt.CdoData);
    const bool has_tranche = (rt.CdoData->AttachmentPoint != 0.0f ||
                              rt.CdoData->DetachmentPoint != 0.0f);
    CHECK(has_tranche);

    BOOST_LOG_SEV(lg, info) << "SyntheticCDO roundtrip passed. Attachment: "
                            << *r.instrument.tranche_attachment;
}

TEST_CASE("credit_mapper_roundtrip_rpa", tags) {
    auto lg(make_logger(test_suite));
    const auto r = load_and_map(
        "Credit_RiskParticipationAgreement_on_Vanilla_Swap.xml");

    CHECK(r.instrument.trade_type_code == "RiskParticipationAgreement");
    CHECK(!r.instrument.reference_entity.empty());
    CHECK(!r.instrument.start_date.empty());
    CHECK(!r.instrument.maturity_date.empty());

    // Reverse roundtrip
    const auto rt = credit_instrument_mapper::reverse_rpa(r.instrument);
    REQUIRE(rt.RiskParticipationAgreementData);

    BOOST_LOG_SEV(lg, info) << "RPA roundtrip passed. Reference: "
                            << r.instrument.reference_entity;
}
