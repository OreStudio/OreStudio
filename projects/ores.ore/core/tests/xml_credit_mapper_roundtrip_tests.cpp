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

TEST_CASE("credit_mapper_roundtrip_cds", tags) {
    auto lg(make_logger(test_suite));
    const auto r = load_and_map("Credit_Default_Swap.xml");

    CHECK(r.trade_type_code == "CreditDefaultSwap");
    CHECK(!r.reference_entity.empty());
    CHECK(!r.currency.empty());
    CHECK(r.notional > 0.0);
    CHECK(r.spread > 0.0);
    CHECK(!r.start_date.empty());
    CHECK(!r.maturity_date.empty());

    // Reverse roundtrip
    const auto rt = credit_instrument_mapper::reverse_cds(r);
    REQUIRE(rt.CreditDefaultSwapData);
    CHECK(rt.CreditDefaultSwapData->creditCurveIdType.CreditCurveId);

    BOOST_LOG_SEV(lg, info) << "CDS roundtrip passed. Reference: " << r.reference_entity;
}

TEST_CASE("credit_mapper_roundtrip_index_cds", tags) {
    auto lg(make_logger(test_suite));
    const auto r = load_and_map("Credit_Index_Credit_Default_Swap.xml");

    CHECK(r.trade_type_code == "IndexCreditDefaultSwap");
    CHECK(!r.reference_entity.empty());
    CHECK(!r.index_name.empty());
    CHECK(!r.currency.empty());
    CHECK(r.notional > 0.0);

    // Reverse roundtrip
    const auto rt = credit_instrument_mapper::reverse_index_cds(r);
    REQUIRE(rt.IndexCreditDefaultSwapData);

    BOOST_LOG_SEV(lg, info) << "IndexCDS roundtrip passed. Index: " << r.index_name;
}

TEST_CASE("credit_mapper_roundtrip_index_cds_option", tags) {
    auto lg(make_logger(test_suite));
    const auto r = load_and_map("Credit_Index_CDS_Option.xml");

    CHECK(r.trade_type_code == "IndexCreditDefaultSwapOption");
    CHECK(!r.reference_entity.empty());
    CHECK(!r.option_expiry_date.empty());
    CHECK(r.option_strike.has_value());

    // Reverse roundtrip
    const auto rt = credit_instrument_mapper::reverse_index_cds_option(r);
    REQUIRE(rt.IndexCreditDefaultSwapOptionData);
    CHECK(rt.IndexCreditDefaultSwapOptionData->Strike);

    BOOST_LOG_SEV(lg, info) << "IndexCDSOption roundtrip passed. Expiry: " << r.option_expiry_date;
}

TEST_CASE("credit_mapper_roundtrip_credit_linked_swap", tags) {
    auto lg(make_logger(test_suite));
    const auto r = load_and_map("Credit_CreditLinkedSwap.xml");

    CHECK(r.trade_type_code == "CreditLinkedSwap");
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

    CHECK(r.trade_type_code == "SyntheticCDO");
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

    CHECK(r.trade_type_code == "RiskParticipationAgreement");
    CHECK(!r.reference_entity.empty());
    CHECK(!r.start_date.empty());
    CHECK(!r.maturity_date.empty());

    // Reverse roundtrip
    const auto rt = credit_instrument_mapper::reverse_rpa(r);
    REQUIRE(rt.RiskParticipationAgreementData);

    BOOST_LOG_SEV(lg, info) << "RPA roundtrip passed. Reference: " << r.reference_entity;
}
