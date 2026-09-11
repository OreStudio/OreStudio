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
#include "ores.ore.core/domain/domain.hpp"
#include "ores.ore.core/domain/trade_mapper.hpp"
#include "ores.platform/filesystem/file.hpp"
#include "ores.testing/project_root.hpp"
#include <catch2/catch_test_macros.hpp>

/**
 * @file xml_trade_envelope_mapper_roundtrip_tests.cpp
 * @brief Mapper fidelity tests for the ORE trade envelope.
 *
 * The envelope is keyed by the trade and shared by every product family. It is
 * the only part of a trade the container carries whole, because the schema
 * types AdditionalFields as xs:any. These tests read a document, drive the
 * envelope through the mapper pair, and compare the result with what the
 * generated reader saw.
 */

namespace {

const std::string_view test_suite("ores.ore.trade.envelope.mapper.roundtrip.tests");
const std::string tags("[ore][xml][mapper][roundtrip][envelope]");

using ores::ore::domain::portfolio;
using ores::ore::domain::trade;
using ores::ore::domain::trade_mapper;
using namespace ores::logging;

std::filesystem::path example_path(const std::string& filename) {
    return ores::testing::project_root::resolve("external/ore/examples/Products/Example_Trades/" +
                                                filename);
}

trade load_trade(const std::string& filename, std::size_t index = 0) {
    using ores::platform::filesystem::file;
    const std::string content = file::read_content(example_path(filename));
    portfolio p;
    ores::ore::domain::load_data(content, p);
    REQUIRE(p.Trade.size() > index);
    return p.Trade[index];
}

}

// =============================================================================
// A document that states all four members.
// =============================================================================

TEST_CASE("envelope_forward_maps_every_member", tags) {
    auto lg(make_logger(test_suite));
    const auto t = load_trade("BondOption_StrikePrice_StrikeYield.xml");

    const auto e = trade_mapper::map_envelope(t);

    REQUIRE(e);
    REQUIRE(e->counter_party);
    CHECK(*e->counter_party == "CPTY");

    REQUIRE(e->netting_set_id);
    CHECK(*e->netting_set_id == "NS");

    REQUIRE(e->additional_fields);
    REQUIRE(e->additional_fields->size() == 2);
    CHECK((*e->additional_fields)[0].name == "valuation_date");
    CHECK((*e->additional_fields)[0].value == "2025-02-10");
    CHECK((*e->additional_fields)[1].name == "party_id");
    CHECK((*e->additional_fields)[1].value == "party");

    BOOST_LOG_SEV(lg, info) << "Envelope forward-mapper test passed";
}

TEST_CASE("envelope_roundtrip_reproduces_the_generated_envelope", tags) {
    auto lg(make_logger(test_suite));
    const auto t = load_trade("BondOption_StrikePrice_StrikeYield.xml");
    REQUIRE(t.Envelope);

    const auto e = trade_mapper::map_envelope(t);
    REQUIRE(e);

    const auto r = trade_mapper::reverse_envelope(*e);

    REQUIRE(r.CounterParty);
    CHECK(std::string(*r.CounterParty) == std::string(*t.Envelope->CounterParty));

    REQUIRE(r.nettingSetGroup);
    REQUIRE(r.nettingSetGroup->NettingSetId);
    REQUIRE(t.Envelope->nettingSetGroup->NettingSetId);
    CHECK(std::string(*r.nettingSetGroup->NettingSetId) ==
          std::string(*t.Envelope->nettingSetGroup->NettingSetId));
    CHECK(!r.nettingSetGroup->NettingSetDetails);

    REQUIRE(r.AdditionalFields);
    REQUIRE(t.Envelope->AdditionalFields);
    const auto& want = t.Envelope->AdditionalFields->other_elements;
    const auto& got = r.AdditionalFields->other_elements;
    REQUIRE(got.size() == want.size());
    for (std::size_t i = 0; i < want.size(); ++i) {
        CHECK(got[i].name == want[i].name);
        CHECK(std::string(got[i].value) == std::string(want[i].value));
    }

    BOOST_LOG_SEV(lg, info) << "Envelope round-trip test passed";
}

// =============================================================================
// A document that states elements it leaves empty.
// =============================================================================

TEST_CASE("envelope_keeps_an_element_the_document_states_empty", tags) {
    auto lg(make_logger(test_suite));
    const auto t = load_trade("Cash_Bonds.xml");
    REQUIRE(t.Envelope);

    const auto e = trade_mapper::map_envelope(t);
    REQUIRE(e);

    // The document says <NettingSetId/> and <AdditionalFields/>. An empty
    // string and an empty list are the values; the elements are still there.
    REQUIRE(e->netting_set_id);
    CHECK(e->netting_set_id->empty());
    REQUIRE(e->additional_fields);
    CHECK(e->additional_fields->empty());

    const auto r = trade_mapper::reverse_envelope(*e);
    REQUIRE(r.nettingSetGroup);
    CHECK(r.nettingSetGroup->NettingSetId);
    CHECK(r.AdditionalFields);

    BOOST_LOG_SEV(lg, info) << "Envelope empty-element test passed";
}

TEST_CASE("envelope_keeps_a_stated_empty_envelope", tags) {
    auto lg(make_logger(test_suite));
    const auto t = load_trade("BondOption_StrikePrice_StrikeYield.xml");
    REQUIRE(t.Envelope);

    // A trade states an Envelope; the carrier must be engaged even when every
    // member inside it is absent, so the writer emits the element again.
    ores::ore::domain::trade bare;
    bare.id = t.id;
    bare.TradeType = t.TradeType;
    bare.Envelope = ores::ore::domain::envelope();

    const auto e = trade_mapper::map_envelope(bare);
    REQUIRE(e);
    CHECK(!e->counter_party);
    CHECK(!e->netting_set_id);

    BOOST_LOG_SEV(lg, info) << "Envelope stated-empty test passed";
}

// =============================================================================
// A trade that states no envelope at all.
// =============================================================================

TEST_CASE("envelope_absent_maps_to_nothing", tags) {
    auto lg(make_logger(test_suite));

    ores::ore::domain::trade t;
    t.id = "no_envelope";

    CHECK(!trade_mapper::map_envelope(t));

    BOOST_LOG_SEV(lg, info) << "Envelope absent test passed";
}
