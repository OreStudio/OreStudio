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
#include "ores.ore/xml/exporter.hpp"

#include <catch2/catch_test_macros.hpp>
#include "ores.logging/make_logger.hpp"
#include "ores.ore/xml/importer.hpp"
#include "ores.ore/domain/trade_mapper.hpp"
#include "ores.testing/project_root.hpp"
#include "ores.trading.api/messaging/trade_protocol.hpp"

namespace {

const std::string_view test_suite("ores.ore.tests");
const std::string tags("[ore][xml][exporter]");

std::filesystem::path ore_path(const std::string& relative) {
    return ores::testing::project_root::resolve("external/ore/" + relative);
}

std::filesystem::path example_path(const std::string& filename) {
    return ores::testing::project_root::resolve(
        "external/ore/examples/Products/Example_Trades/" + filename);
}

// Convert an import-side instrument_mapping_result (from ores.ore) to the
// export-side instrument_export_result (from ores.trading.api) for testing.
ores::trading::messaging::instrument_export_result to_export_result(
    const ores::ore::domain::instrument_mapping_result& r) {
    using namespace ores::ore::domain;
    using namespace ores::trading::messaging;
    return std::visit([](const auto& v)
            -> ores::trading::messaging::instrument_export_result {
        using T = std::decay_t<decltype(v)>;
        if constexpr (std::is_same_v<T, std::monostate>)
            return std::monostate{};
        else if constexpr (std::is_same_v<T, swap_mapping_result>)
            return swap_export_result{v.instrument, v.legs};
        else if constexpr (std::is_same_v<T, fx_mapping_result>) {
            // fx_mapping_result::instrument (import-side variant) has the same
            // alternatives as fx_export_result::instrument; copy through.
            fx_export_result ex;
            std::visit([&](const auto& instr) {
                ex.instrument = instr;
            }, v.instrument);
            return ex;
        }
        else if constexpr (std::is_same_v<T, bond_mapping_result>)
            return bond_export_result{v.instrument};
        else if constexpr (std::is_same_v<T, credit_mapping_result>)
            return credit_export_result{v.instrument};
        else if constexpr (std::is_same_v<T, equity_mapping_result>) {
            // equity_mapping_result::instrument (import-side variant) has the
            // same alternatives as equity_export_result::instrument; copy
            // through.
            equity_export_result ex;
            std::visit([&](const auto& instr) {
                ex.instrument = instr;
            }, v.instrument);
            return ex;
        }
        else if constexpr (std::is_same_v<T, commodity_mapping_result>)
            return commodity_export_result{v.instrument};
        else if constexpr (std::is_same_v<T, scripted_mapping_result>)
            return scripted_export_result{v.instrument};
        else if constexpr (std::is_same_v<T, composite_mapping_result>)
            return composite_export_result{v.instrument, {}};
        return std::monostate{};
    }, r);
}

}

using ores::ore::xml::exporter;
using ores::ore::xml::importer;
using ores::refdata::domain::currency;
using ores::trading::messaging::trade_export_item;
using namespace ores::logging;

// =============================================================================
// export_currency_config tests
// =============================================================================

TEST_CASE("export_single_currency_to_xml", tags) {
    auto lg(make_logger(test_suite));

    currency c;
    c.iso_code = "USD";
    c.name = "US Dollar";
    c.numeric_code = "840";
    c.symbol = "$";
    c.fraction_symbol = "c";
    c.fractions_per_unit = 100;
    c.rounding_type = "Closest";
    c.rounding_precision = 2;
    c.monetary_nature = "Major";

    const auto xml = exporter::export_currency_config({c});
    BOOST_LOG_SEV(lg, debug) << "Exported XML:\n" << xml;

    CHECK(!xml.empty());
    CHECK(xml.contains("<CurrencyConfig>"));
    CHECK(xml.contains("<ISOCode>USD</ISOCode>"));
    CHECK(xml.contains("<Name>US Dollar</Name>"));
    CHECK(xml.contains("<NumericCode>840</NumericCode>"));
    CHECK(xml.contains("<Symbol>$</Symbol>"));
    CHECK(xml.contains("<FractionSymbol>c</FractionSymbol>"));
    CHECK(xml.contains("<FractionsPerUnit>100</FractionsPerUnit>"));
    CHECK(xml.contains("<RoundingType>Closest</RoundingType>"));
    CHECK(xml.contains("<RoundingPrecision>2</RoundingPrecision>"));
    CHECK(xml.contains("<CurrencyType>Major</CurrencyType>"));
}

TEST_CASE("export_multiple_currencies_to_xml", tags) {
    auto lg(make_logger(test_suite));

    currency usd;
    usd.iso_code = "USD";
    usd.name = "US Dollar";
    usd.fractions_per_unit = 100;
    usd.rounding_type = "Closest";
    usd.rounding_precision = 2;

    currency eur;
    eur.iso_code = "EUR";
    eur.name = "Euro";
    eur.fractions_per_unit = 100;
    eur.rounding_type = "Closest";
    eur.rounding_precision = 2;

    const auto xml = exporter::export_currency_config({usd, eur});
    BOOST_LOG_SEV(lg, debug) << "Exported XML:\n" << xml;

    CHECK(xml.contains("<ISOCode>USD</ISOCode>"));
    CHECK(xml.contains("<ISOCode>EUR</ISOCode>"));
}

TEST_CASE("export_empty_currency_list_to_xml", tags) {
    auto lg(make_logger(test_suite));

    const auto xml = exporter::export_currency_config({});
    BOOST_LOG_SEV(lg, debug) << "Exported XML:\n" << xml;

    CHECK(!xml.empty());
    CHECK(xml.contains("CurrencyConfig"));
}

TEST_CASE("export_currency_without_optional_fields", tags) {
    auto lg(make_logger(test_suite));

    currency c;
    c.iso_code = "TST";
    c.name = "Test Currency";
    c.fractions_per_unit = 100;
    c.rounding_type = "Up";
    c.rounding_precision = 0;
    // numeric_code and asset_class left empty

    const auto xml = exporter::export_currency_config({c});
    BOOST_LOG_SEV(lg, debug) << "Exported XML:\n" << xml;

    CHECK(xml.contains("<ISOCode>TST</ISOCode>"));
    CHECK(!xml.contains("<NumericCode>"));
    CHECK(!xml.contains("<CurrencyType>"));
}

// =============================================================================
// import/export roundtrip tests
// =============================================================================

TEST_CASE("import_export_roundtrip_preserves_currency_fields", tags) {
    auto lg(make_logger(test_suite));

    const auto f = ore_path("examples/Input/currencies.xml");
    BOOST_LOG_SEV(lg, debug) << "Importing from: " << f;

    // Import
    const auto imported = importer::import_currency_config(f);
    REQUIRE(!imported.empty());
    BOOST_LOG_SEV(lg, debug) << "Imported " << imported.size() << " currencies";

    // Export
    const auto xml = exporter::export_currency_config(imported);
    BOOST_LOG_SEV(lg, trace) << "Exported XML:\n" << xml;

    // Re-import by parsing the exported XML through domain layer
    // Since import_currency_config takes a file path, we verify via field checks
    // on a known currency from the original import.
    const auto& usd = *std::ranges::find_if(imported,
        [](const auto& c) { return c.iso_code == "USD"; });

    CHECK(xml.contains("<ISOCode>USD</ISOCode>"));
    CHECK(xml.contains("<Name>" + usd.name + "</Name>"));
    CHECK(xml.contains("<RoundingType>" + usd.rounding_type + "</RoundingType>"));
}

TEST_CASE("export_all_rounding_types", tags) {
    auto lg(make_logger(test_suite));

    const std::vector<std::string> rounding_types =
        {"Up", "Down", "Closest", "Floor", "Ceiling"};

    std::vector<currency> currencies;
    for (const auto& rt : rounding_types) {
        currency c;
        c.iso_code = rt.substr(0, 3);
        c.name = "Rounding " + rt;
        c.fractions_per_unit = 100;
        c.rounding_type = rt;
        c.rounding_precision = 2;
        currencies.push_back(c);
    }

    const auto xml = exporter::export_currency_config(currencies);
    BOOST_LOG_SEV(lg, debug) << "Exported XML:\n" << xml;

    for (const auto& rt : rounding_types) {
        CHECK(xml.contains("<RoundingType>" + rt + "</RoundingType>"));
    }
}

// =============================================================================
// export_portfolio roundtrip tests
// =============================================================================

TEST_CASE("export_portfolio_empty_items_produces_valid_xml", tags) {
    auto lg(make_logger(test_suite));

    const auto xml = exporter::export_portfolio({});
    BOOST_LOG_SEV(lg, debug) << "Exported XML:\n" << xml;

    CHECK(!xml.empty());
    CHECK(xml.contains("Portfolio"));
}

TEST_CASE("export_portfolio_swap_roundtrip", tags) {
    auto lg(make_logger(test_suite));

    const auto f = example_path("IR_Swap_Vanilla.xml");
    const auto imported = importer::import_portfolio_with_context(f);
    REQUIRE(imported.size() == 1);

    std::vector<trade_export_item> items;
    for (const auto& src : imported) {
        trade_export_item item;
        item.trade = src.trade;
        item.instrument = to_export_result(src.instrument);
        items.push_back(std::move(item));
    }

    const auto xml = exporter::export_portfolio(items);
    BOOST_LOG_SEV(lg, debug) << "Exported XML:\n" << xml;

    CHECK(!xml.empty());
    CHECK(xml.contains("<Portfolio>"));
    CHECK(xml.contains("<SwapData>"));
    CHECK(xml.contains(imported.front().trade.external_id));
}

TEST_CASE("export_portfolio_fx_forward_roundtrip", tags) {
    auto lg(make_logger(test_suite));

    const auto f = example_path("FX_Forward.xml");
    const auto imported = importer::import_portfolio_with_context(f);
    REQUIRE(imported.size() == 1);

    std::vector<trade_export_item> items;
    for (const auto& src : imported) {
        trade_export_item item;
        item.trade = src.trade;
        item.instrument = to_export_result(src.instrument);
        items.push_back(std::move(item));
    }

    const auto xml = exporter::export_portfolio(items);
    BOOST_LOG_SEV(lg, debug) << "Exported XML:\n" << xml;

    CHECK(!xml.empty());
    CHECK(xml.contains("<FxForwardData>"));
    CHECK(xml.contains(imported.front().trade.external_id));
}

TEST_CASE("export_portfolio_monostate_items_are_skipped", tags) {
    auto lg(make_logger(test_suite));

    trade_export_item item;
    item.trade.external_id = "UnmappedTrade001";
    item.trade.trade_type = "UnknownType";
    // instrument left as monostate

    const auto xml = exporter::export_portfolio({item});
    BOOST_LOG_SEV(lg, debug) << "Exported XML:\n" << xml;

    CHECK(!xml.empty());
    CHECK(!xml.contains("UnmappedTrade001"));
}
