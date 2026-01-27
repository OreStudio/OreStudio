/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2025 Marco Craveiro <marco.craveiro@gmail.com>
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

#include <catch2/catch_test_macros.hpp>
#include "ores.logging/make_logger.hpp"

namespace {

const std::string_view test_suite("ores.ore.portfolio.roundtrip.tests");
const std::string tags("[ore][xml][roundtrip][portfolio]");

using ores::ore::domain::portfolio;
using namespace ores::logging;

/**
 * @brief Compare two portfolio objects by checking trade count and IDs.
 */
void require_portfolio_equal(const portfolio& original,
                              const portfolio& roundtripped) {
    CHECK(roundtripped.Trade.size() == original.Trade.size());

    // Spot check: verify first trade ID if present
    if (!original.Trade.empty()) {
        REQUIRE(!roundtripped.Trade.empty());
        CHECK(std::string(roundtripped.Trade[0].id) ==
              std::string(original.Trade[0].id));

        // Check first trade type matches (TradeType is required)
        CHECK(roundtripped.Trade[0].TradeType == original.Trade[0].TradeType);
    }

    // Spot check: verify all trade IDs match
    for (std::size_t i = 0; i < original.Trade.size(); ++i) {
        INFO("Checking trade at index " << i);
        CHECK(std::string(roundtripped.Trade[i].id) ==
              std::string(original.Trade[i].id));
    }
}

/**
 * @brief Perform a structural roundtrip test on portfolio.
 */
void test_portfolio_roundtrip(const std::string& xml_content,
                               const std::string& source_name) {
    auto lg(make_logger(test_suite));

    // Step 1: Parse original XML
    BOOST_LOG_SEV(lg, debug) << "Parsing original XML from: " << source_name;
    portfolio original;
    ores::ore::domain::load_data(xml_content, original);

    BOOST_LOG_SEV(lg, debug) << "Parsed " << original.Trade.size() << " trades";

    // Step 2: Serialize back to XML
    BOOST_LOG_SEV(lg, debug) << "Serializing to XML";
    const std::string serialized = ores::ore::domain::save_data(original);

    // Step 3: Parse the serialized XML
    BOOST_LOG_SEV(lg, debug) << "Parsing serialized XML";
    portfolio roundtripped;
    ores::ore::domain::load_data(serialized, roundtripped);

    // Step 4: Compare domain objects
    BOOST_LOG_SEV(lg, debug) << "Comparing original and roundtripped objects";
    require_portfolio_equal(original, roundtripped);

    BOOST_LOG_SEV(lg, info) << "Roundtrip test passed for: " << source_name;
}

}

// =============================================================================
// Roundtrip Tests
// =============================================================================

// Note: ORE portfolio XML files use NettingSetId which requires XSD substitution
// group support. Tests use inline XML without NettingSetId until that's implemented.

TEST_CASE("portfolio_roundtrip_simple_trades", tags) {
    const std::string xml = R"(
<Portfolio>
  <Trade id="Swap_EUR_1">
    <TradeType>Swap</TradeType>
    <Envelope>
      <CounterParty>CPTY_A</CounterParty>
    </Envelope>
  </Trade>
  <Trade id="Swap_USD_1">
    <TradeType>Swap</TradeType>
    <Envelope>
      <CounterParty>CPTY_B</CounterParty>
    </Envelope>
  </Trade>
  <Trade id="FxForward_1">
    <TradeType>FxForward</TradeType>
    <Envelope>
      <CounterParty>CPTY_A</CounterParty>
    </Envelope>
  </Trade>
</Portfolio>
)";

    test_portfolio_roundtrip(xml, "simple_trades_inline");
}

TEST_CASE("portfolio_roundtrip_with_portfolio_ids", tags) {
    const std::string xml = R"(
<Portfolio>
  <Trade id="Trade_1">
    <TradeType>Swap</TradeType>
    <Envelope>
      <CounterParty>CPTY_A</CounterParty>
      <PortfolioIds>
        <PortfolioId>Portfolio1</PortfolioId>
        <PortfolioId>Portfolio2</PortfolioId>
      </PortfolioIds>
    </Envelope>
  </Trade>
</Portfolio>
)";

    test_portfolio_roundtrip(xml, "with_portfolio_ids_inline");
}

