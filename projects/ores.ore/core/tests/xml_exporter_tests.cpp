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
#include "ores.logging/make_logger.hpp"
#include "ores.ore.core/domain/portfolio_projection.hpp"
#include "ores.ore.core/domain/pricing_engine_projection.hpp"
#include "ores.ore.core/xml/exporter.hpp"
#include "ores.ore.core/xml/importer.hpp"
#include "ores.testing/project_root.hpp"
#include "ores.trading.api/domain/fra_instrument.hpp"
#include "ores.trading.api/domain/fx_accumulator_instrument.hpp"
#include "ores.trading.api/domain/fx_asian_forward_instrument.hpp"
#include "ores.trading.api/domain/fx_barrier_option_instrument.hpp"
#include "ores.trading.api/domain/fx_digital_option_instrument.hpp"
#include "ores.trading.api/domain/fx_forward_instrument.hpp"
#include "ores.trading.api/domain/fx_vanilla_option_instrument.hpp"
#include "ores.trading.api/domain/fx_variance_swap_instrument.hpp"
#include "ores.trading.api/domain/instrument_batch.hpp"
#include "ores.trading.api/domain/instrument_batch_mapper.hpp"
#include "ores.trading.api/domain/range_bound.hpp"
#include "ores.trading.api/domain/rate_instrument.hpp"
#include "ores.trading.api/domain/swap_leg.hpp"
#include "ores.trading.api/domain/swap_leg_amount.hpp"
#include "ores.trading.api/domain/swap_leg_rate.hpp"
#include "ores.trading.api/domain/vanilla_swap_instrument.hpp"
#include "ores.trading.api/messaging/trade_operations_protocol.hpp"
#include "test_helpers.hpp"
#include "test_suite.hpp"
#include <boost/uuid/random_generator.hpp>
#include <catch2/catch_test_macros.hpp>
#include <optional>
#include <string>
#include <string_view>
#include <utility>
#include <variant>
#include <vector>

namespace {

/**
 * @brief Turns imported trades into the payload the exporter consumes.
 *
 * The importer leaves the trade id unset; the planner mints it on the import
 * path. The export joins each item to its instrument in the batch by that id,
 * so this mints a distinct id per trade and stamps it onto the instrument,
 * exactly as the planner does. Without that, every trade in a document shares
 * the nil id and joins to the batch's first instrument.
 */
std::pair<std::vector<ores::trading::messaging::trade_export_item>,
          ores::trading::domain::instrument_batch>
make_export_payload(const std::vector<ores::ore::xml::trade_import_item>& imported) {
    std::vector<ores::trading::messaging::trade_export_item> items;
    ores::trading::domain::instrument_batch instruments;
    boost::uuids::random_generator uuid_gen;

    for (const auto& src : imported) {
        const auto trade_id = uuid_gen();
        auto instrument = src.instrument;
        if (!std::holds_alternative<std::monostate>(instrument))
            ores::trading::domain::stamp_ids(instrument, trade_id);

        ores::trading::messaging::trade_export_item item;
        item.anchor = src.anchor;
        item.anchor.id = trade_id;
        item.ore_id = src.ore_id;
        ores::trading::domain::append_instrument(instruments, instrument);
        items.push_back(std::move(item));
    }

    return {std::move(items), std::move(instruments)};
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

TEST_CASE("export_single_currency_to_xml", "[ore][xml][exporter]") {
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
    c.monetary_nature = "fiat";
    c.ore_currency_type = "Major";

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

TEST_CASE("export_multiple_currencies_to_xml", "[ore][xml][exporter]") {
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

TEST_CASE("export_empty_currency_list_to_xml", "[ore][xml][exporter]") {
    auto lg(make_logger(test_suite));

    const auto xml = exporter::export_currency_config({});
    BOOST_LOG_SEV(lg, debug) << "Exported XML:\n" << xml;

    CHECK(!xml.empty());
    CHECK(xml.contains("CurrencyConfig"));
}

TEST_CASE("export_currency_without_optional_fields", "[ore][xml][exporter]") {
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

TEST_CASE("import_export_roundtrip_preserves_currency_fields", "[ore][xml][exporter]") {
    auto lg(make_logger(test_suite));

    const auto f = ores::testing::ore_path("examples/Input/currencies.xml");
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
    const auto& usd =
        *std::ranges::find_if(imported, [](const auto& c) { return c.iso_code == "USD"; });

    CHECK(xml.contains("<ISOCode>USD</ISOCode>"));
    CHECK(xml.contains("<Name>" + usd.name + "</Name>"));
    CHECK(xml.contains("<RoundingType>" + usd.rounding_type + "</RoundingType>"));
}

TEST_CASE("export_all_rounding_types", "[ore][xml][exporter]") {
    auto lg(make_logger(test_suite));

    const std::vector<std::string> rounding_types = {"Up", "Down", "Closest", "Floor", "Ceiling"};

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

TEST_CASE("export_portfolio_empty_items_produces_valid_xml", "[ore][xml][exporter]") {
    auto lg(make_logger(test_suite));

    const auto xml = exporter::export_portfolio({}, {});
    BOOST_LOG_SEV(lg, debug) << "Exported XML:\n" << xml;

    CHECK(!xml.empty());
    CHECK(xml.contains("Portfolio"));
}

TEST_CASE("export_portfolio_swap_roundtrip", "[ore][xml][exporter]") {
    auto lg(make_logger(test_suite));

    const auto f = example_path("IR_Swap_Vanilla.xml");
    const auto imported = importer::import_portfolio_with_context(f);
    REQUIRE(imported.size() == 1);

    auto [items, instruments] = make_export_payload(imported);

    const auto xml = exporter::export_portfolio(items, instruments);
    BOOST_LOG_SEV(lg, debug) << "Exported XML:\n" << xml;

    CHECK(!xml.empty());
    CHECK(xml.contains("<Portfolio>"));
    CHECK(xml.contains("<SwapData>"));
    CHECK(xml.contains(imported.front().ore_id));
}

TEST_CASE("a_rate_trade_without_a_fact_row_exports_without_its_legs", "[ore][xml][exporter]") {
    auto lg(make_logger(test_suite));

    /*
     * The population books a rate trade as a header with two legs and no
     * specialised leaf, because every routed type reduces to that shape. This
     * asks whether a portfolio the engine reads comes back out of it: the
     * engine prices a swap from its legs, so a document that dropped them
     * would export a trade nobody can price.
     */
    const auto f = example_path("IR_Swap_Vanilla.xml");
    const auto imported = importer::import_portfolio_with_context(f);
    REQUIRE(!imported.empty());

    const auto trade_id = boost::uuids::random_generator{}();
    const auto activity_id = boost::uuids::random_generator{}();

    trade_export_item item;
    item.anchor = imported.front().anchor;
    item.anchor.id = trade_id;
    item.anchor.trade_type = "Swap";
    item.ore_id = imported.front().ore_id;

    ores::trading::domain::instrument_batch instruments;
    ores::trading::domain::rate_instrument header;
    header.identity.trade_id = trade_id;
    header.identity.trade_activity_id = activity_id;
    header.identity.trade_type_code = "Swap";
    header.start_date = std::chrono::year{2026} / std::chrono::month{1} / std::chrono::day{15};
    header.maturity_date = std::chrono::year{2031} / std::chrono::month{1} / std::chrono::day{15};
    instruments.rate_instruments.push_back(header);

    const auto leg = [&](int number,
                         std::string_view type_code,
                         std::string_view index,
                         std::string_view role,
                         double value) {
        ores::trading::domain::swap_leg row;
        row.identity.trade_id = trade_id;
        row.identity.trade_activity_id = activity_id;
        row.identity.leg_number = number;
        row.leg_type_code = std::string(type_code);
        row.day_count_fraction_code = "ActAct(ISDA)";
        row.business_day_convention_code = "ModifiedFollowing";
        row.payment_frequency_code = "Semiannual";
        row.floating_index_code = std::string(index);
        row.currency = "GBP";
        instruments.swap_legs.push_back(row);

        ores::trading::domain::swap_leg_amount amount;
        amount.trade_id = trade_id;
        amount.trade_activity_id = activity_id;
        amount.leg_number = number;
        amount.sequence_number = 1;
        amount.start_date = header.start_date;
        amount.amount = ores::utility::decimal::decimal::from_double(10000000.0).value();
        instruments.swap_leg_amounts.push_back(amount);

        ores::trading::domain::swap_leg_rate rate;
        rate.trade_id = trade_id;
        rate.trade_activity_id = activity_id;
        rate.leg_number = number;
        rate.rate_role = std::string(role);
        rate.sequence_number = 1;
        rate.start_date = header.start_date;
        rate.value = ores::utility::decimal::decimal::from_double(value).value();
        instruments.swap_leg_rates.push_back(rate);
    };

    leg(1, "Fixed", "", "fixed", 0.0425);
    leg(2, "Floating", "GBP-SONIA", "spread", 0.001);

    const auto xml = exporter::export_portfolio({item}, instruments);
    BOOST_LOG_SEV(lg, debug) << "Exported XML:\n" << xml;

    /*
     * The store rebuilds a rates instrument from a header and one fact row, so
     * a batch that states a header and legs and no fact row rebuilds nothing.
     * This case states that limit, because the exporter's silence is what makes
     * a fact row mandatory rather than tidy.
     */
    CHECK_FALSE(xml.contains("<SwapData>"));
    CHECK_FALSE(xml.contains("GBP-SONIA"));
}

TEST_CASE("a_rate_trade_that_states_its_fact_row_exports_its_legs", "[ore][xml][exporter]") {
    auto lg(make_logger(test_suite));

    const auto f = example_path("IR_Swap_Vanilla.xml");
    const auto imported = importer::import_portfolio_with_context(f);
    REQUIRE(!imported.empty());

    const auto trade_id = boost::uuids::random_generator{}();
    const auto activity_id = boost::uuids::random_generator{}();

    trade_export_item item;
    item.anchor = imported.front().anchor;
    item.anchor.id = trade_id;
    item.anchor.trade_type = "Swap";
    item.ore_id = imported.front().ore_id;

    ores::trading::domain::instrument_batch instruments;
    ores::trading::domain::rate_instrument header;
    header.identity.trade_id = trade_id;
    header.identity.trade_activity_id = activity_id;
    header.identity.trade_type_code = "Swap";
    header.start_date = std::chrono::year{2026} / std::chrono::month{1} / std::chrono::day{15};
    header.maturity_date = std::chrono::year{2031} / std::chrono::month{1} / std::chrono::day{15};
    instruments.rate_instruments.push_back(header);

    ores::trading::domain::vanilla_swap_instrument facts;
    facts.trade_id = trade_id;
    facts.trade_activity_id = activity_id;
    facts.settlement_lag = 2;
    instruments.vanilla_swap_instruments.push_back(facts);

    const auto leg = [&](int number,
                         std::string_view type_code,
                         std::string_view index,
                         std::string_view role,
                         double value) {
        ores::trading::domain::swap_leg row;
        row.identity.trade_id = trade_id;
        row.identity.trade_activity_id = activity_id;
        row.identity.leg_number = number;
        row.leg_type_code = std::string(type_code);
        row.day_count_fraction_code = "ActAct(ISDA)";
        row.business_day_convention_code = "ModifiedFollowing";
        row.payment_frequency_code = "Semiannual";
        row.floating_index_code = std::string(index);
        row.currency = "GBP";
        instruments.swap_legs.push_back(row);

        ores::trading::domain::swap_leg_amount amount;
        amount.trade_id = trade_id;
        amount.trade_activity_id = activity_id;
        amount.leg_number = number;
        amount.sequence_number = 1;
        amount.start_date = header.start_date;
        amount.amount = ores::utility::decimal::decimal::from_double(10000000.0).value();
        instruments.swap_leg_amounts.push_back(amount);

        ores::trading::domain::swap_leg_rate rate;
        rate.trade_id = trade_id;
        rate.trade_activity_id = activity_id;
        rate.leg_number = number;
        rate.rate_role = std::string(role);
        rate.sequence_number = 1;
        rate.start_date = header.start_date;
        rate.value = ores::utility::decimal::decimal::from_double(value).value();
        instruments.swap_leg_rates.push_back(rate);
    };

    leg(1, "Fixed", "", "fixed", 0.0425);
    leg(2, "Floating", "GBP-SONIA", "spread", 0.001);

    const auto xml = exporter::export_portfolio({item}, instruments);
    BOOST_LOG_SEV(lg, debug) << "Exported XML:\n" << xml;

    CHECK(xml.contains("<SwapData>"));
    CHECK(xml.contains("GBP-SONIA"));
    CHECK(xml.find("<LegData>") != std::string::npos);
}

TEST_CASE("a_forward_rate_agreement_exports_the_index_its_fact_row_states",
          "[ore][xml][exporter]") {
    auto lg(make_logger(test_suite));

    /*
     * A forward states its index on the fact row, and the population orders the
     * fixed leg first, where the index is empty. Reading the first leg wrote no
     * index at all, so the document named a forward that resets on nothing.
     */
    const auto f = example_path("IR_Swap_Vanilla.xml");
    const auto imported = importer::import_portfolio_with_context(f);
    REQUIRE(!imported.empty());

    const auto trade_id = boost::uuids::random_generator{}();
    const auto activity_id = boost::uuids::random_generator{}();

    trade_export_item item;
    item.anchor = imported.front().anchor;
    item.anchor.id = trade_id;
    item.anchor.trade_type = "ForwardRateAgreement";
    item.ore_id = imported.front().ore_id;

    ores::trading::domain::instrument_batch instruments;
    ores::trading::domain::rate_instrument header;
    header.identity.trade_id = trade_id;
    header.identity.trade_activity_id = activity_id;
    header.identity.trade_type_code = "ForwardRateAgreement";
    header.start_date = std::chrono::year{2026} / std::chrono::month{1} / std::chrono::day{15};
    header.maturity_date = std::chrono::year{2026} / std::chrono::month{7} / std::chrono::day{15};
    instruments.rate_instruments.push_back(header);

    ores::trading::domain::fra_instrument facts;
    facts.trade_id = trade_id;
    facts.trade_activity_id = activity_id;
    facts.currency = "GBP";
    facts.rate_index = "GBP-SONIA";
    facts.long_short = "Long";
    facts.notional = ores::utility::decimal::decimal::from_double(5000000.0).value();
    instruments.fra_instruments.push_back(facts);

    ores::trading::domain::swap_leg fixed;
    fixed.identity.trade_id = trade_id;
    fixed.identity.trade_activity_id = activity_id;
    fixed.identity.leg_number = 1;
    fixed.leg_type_code = "Fixed";
    fixed.day_count_fraction_code = "ActAct(ISDA)";
    fixed.business_day_convention_code = "ModifiedFollowing";
    fixed.payment_frequency_code = "Semiannual";
    fixed.currency = "GBP";
    instruments.swap_legs.push_back(fixed);

    ores::trading::domain::swap_leg floating = fixed;
    floating.identity.leg_number = 2;
    floating.leg_type_code = "Floating";
    floating.floating_index_code = "GBP-SONIA";
    instruments.swap_legs.push_back(floating);

    // A forward states the rate it fixes on the fixed side; the document
    // refuses a forward that states none.
    ores::trading::domain::swap_leg_rate strike;
    strike.trade_id = trade_id;
    strike.trade_activity_id = activity_id;
    strike.leg_number = 1;
    strike.rate_role = "fixed";
    strike.sequence_number = 1;
    strike.start_date = header.start_date;
    strike.value = ores::utility::decimal::decimal::from_double(0.0425).value();
    instruments.swap_leg_rates.push_back(strike);

    const auto xml = exporter::export_portfolio({item}, instruments);
    BOOST_LOG_SEV(lg, debug) << "Exported XML:\n" << xml;

    CHECK(xml.contains("<ForwardRateAgreementData>"));
    CHECK(xml.contains("GBP-SONIA"));
}

/*
 * An FX trade and the batch that holds it, so a case states only the shape it
 * is about. The anchor comes from a document the engine reads, because an
 * export writes the trade around the instrument it is given.
 */
struct fx_export final {
    trade_export_item item;
    ores::trading::domain::instrument_batch instruments;
    boost::uuids::uuid trade_id;
    boost::uuids::uuid activity_id;

    explicit fx_export(const std::string& trade_type) {
        const auto imported =
            importer::import_portfolio_with_context(example_path("IR_Swap_Vanilla.xml"));
        REQUIRE(!imported.empty());
        trade_id = boost::uuids::random_generator{}();
        activity_id = boost::uuids::random_generator{}();
        item.anchor = imported.front().anchor;
        item.anchor.id = trade_id;
        item.anchor.trade_type = trade_type;
        item.ore_id = imported.front().ore_id;
    }
};

TEST_CASE("a_forward_exports_its_two_currencies", "[ore][xml][exporter]") {
    auto lg(make_logger(test_suite));
    fx_export f("FxForward");
    ores::trading::domain::fx_forward_instrument row;
    row.identity.trade_id = f.trade_id;
    row.identity.trade_activity_id = f.activity_id;
    row.bought_currency = "EUR";
    row.sold_currency = "GBP";
    row.bought_amount = ores::utility::decimal::decimal::from_double(1000000.0).value();
    row.sold_amount = ores::utility::decimal::decimal::from_double(850000.0).value();
    row.value_date = std::chrono::year{2026} / std::chrono::month{7} / std::chrono::day{15};
    row.settlement = "Cash";
    f.instruments.fx_forward_instruments.push_back(row);

    const auto xml = exporter::export_portfolio({f.item}, f.instruments);
    BOOST_LOG_SEV(lg, debug) << "Exported XML:\n" << xml;
    CHECK(xml.contains("<FxForwardData>"));
    CHECK(xml.contains("EUR"));
    CHECK(xml.contains("GBP"));
}

TEST_CASE("a_vanilla_option_exports_its_expiry_and_style", "[ore][xml][exporter]") {
    auto lg(make_logger(test_suite));
    fx_export f("FxOption");
    ores::trading::domain::fx_vanilla_option_instrument row;
    row.identity.trade_id = f.trade_id;
    row.identity.trade_activity_id = f.activity_id;
    row.bought_currency = "EUR";
    row.sold_currency = "GBP";
    row.bought_amount = ores::utility::decimal::decimal::from_double(1000000.0).value();
    row.sold_amount = ores::utility::decimal::decimal::from_double(850000.0).value();
    row.option_type = "Call";
    row.expiry_date = std::chrono::year{2026} / std::chrono::month{7} / std::chrono::day{15};
    row.exercise_style = "European";
    row.settlement = "Cash";
    f.instruments.fx_vanilla_option_instruments.push_back(row);

    const auto xml = exporter::export_portfolio({f.item}, f.instruments);
    BOOST_LOG_SEV(lg, debug) << "Exported XML:\n" << xml;
    CHECK(xml.contains("<FxOptionData>"));
    // ORE implies European exercise on an FxOption, so the document states no
    // style and the expiry is what says when the option ends.
    CHECK(xml.contains("2026-07-15"));
}

TEST_CASE("a_barrier_option_exports_its_barrier", "[ore][xml][exporter]") {
    auto lg(make_logger(test_suite));
    fx_export f("FxBarrierOption");
    ores::trading::domain::fx_barrier_option_instrument row;
    row.identity.trade_id = f.trade_id;
    row.identity.trade_activity_id = f.activity_id;
    row.bought_currency = "EUR";
    row.sold_currency = "GBP";
    row.bought_amount = ores::utility::decimal::decimal::from_double(1000000.0).value();
    row.sold_amount = ores::utility::decimal::decimal::from_double(850000.0).value();
    row.option_type = "Call";
    row.expiry_date = std::chrono::year{2026} / std::chrono::month{7} / std::chrono::day{15};
    row.settlement = "Cash";
    row.barrier_type = "KnockOut";
    row.upper_barrier = ores::utility::decimal::decimal::from_double(0.95).value();
    row.underlying_code = "EUR/GBP";
    f.instruments.fx_barrier_option_instruments.push_back(row);

    const auto xml = exporter::export_portfolio({f.item}, f.instruments);
    BOOST_LOG_SEV(lg, debug) << "Exported XML:\n" << xml;
    CHECK(xml.contains("<FxBarrierOptionData>"));
    // The pair is implicit in the two currencies, so a plain barrier states no
    // underlying; the barrier itself is what the element adds.
    CHECK(xml.contains("KnockOut"));
}

TEST_CASE("a_digital_exports_its_payoff", "[ore][xml][exporter]") {
    auto lg(make_logger(test_suite));
    fx_export f("FxDigitalOption");
    ores::trading::domain::fx_digital_option_instrument row;
    row.identity.trade_id = f.trade_id;
    row.identity.trade_activity_id = f.activity_id;
    row.foreign_currency = "EUR";
    row.domestic_currency = "GBP";
    row.payoff_currency = "GBP";
    row.payoff_amount = ores::utility::decimal::decimal::from_double(100000.0).value();
    row.option_type = "Call";
    row.expiry_date = std::chrono::year{2026} / std::chrono::month{7} / std::chrono::day{15};
    row.long_short = "Long";
    row.strike = ores::utility::decimal::decimal::from_double(0.9).value();
    f.instruments.fx_digital_option_instruments.push_back(row);

    const auto xml = exporter::export_portfolio({f.item}, f.instruments);
    BOOST_LOG_SEV(lg, debug) << "Exported XML:\n" << xml;
    CHECK(xml.contains("<FxDigitalOptionData>"));
    CHECK(xml.contains("GBP"));
}

TEST_CASE("an_average_forward_exports_its_index", "[ore][xml][exporter]") {
    auto lg(make_logger(test_suite));
    fx_export f("FxAverageForward");
    ores::trading::domain::fx_asian_forward_instrument row;
    row.identity.trade_id = f.trade_id;
    row.identity.trade_activity_id = f.activity_id;
    row.fx_index = "EUR/GBP";
    row.reference_currency = "EUR";
    row.reference_notional = ores::utility::decimal::decimal::from_double(1000000.0).value();
    row.settlement_currency = "GBP";
    row.settlement_notional = ores::utility::decimal::decimal::from_double(850000.0).value();
    row.long_short = "Long";
    row.currency = "GBP";
    row.fixing_amount = ores::utility::decimal::decimal::from_double(1000000.0).value();
    row.target_amount = ores::utility::decimal::decimal::from_double(2000000.0).value();
    row.strike = ores::utility::decimal::decimal::from_double(0.85).value();
    f.instruments.fx_asian_forward_instruments.push_back(row);

    const auto xml = exporter::export_portfolio({f.item}, f.instruments);
    BOOST_LOG_SEV(lg, debug) << "Exported XML:\n" << xml;
    CHECK(xml.contains("<FxAverageForwardData>"));
    CHECK(xml.contains("EUR/GBP"));
}

TEST_CASE("a_variance_swap_exports_its_strike", "[ore][xml][exporter]") {
    auto lg(make_logger(test_suite));
    fx_export f("FxVarianceSwap");
    ores::trading::domain::fx_variance_swap_instrument row;
    row.identity.trade_id = f.trade_id;
    row.identity.trade_activity_id = f.activity_id;
    row.start_date = std::chrono::year{2026} / std::chrono::month{1} / std::chrono::day{15};
    row.end_date = std::chrono::year{2027} / std::chrono::month{1} / std::chrono::day{15};
    row.currency = "GBP";
    row.underlying_code = "EUR/GBP";
    row.long_short = "Long";
    row.notional = ores::utility::decimal::decimal::from_double(1000000.0).value();
    row.moment_type = "Variance";
    row.strike = 0.09;
    f.instruments.fx_variance_swap_instruments.push_back(row);

    const auto xml = exporter::export_portfolio({f.item}, f.instruments);
    BOOST_LOG_SEV(lg, debug) << "Exported XML:\n" << xml;
    CHECK(xml.contains("<FxVarianceSwapData>"));
    CHECK(xml.contains("0.09"));
}

TEST_CASE("an_accumulator_exports_its_knock_out", "[ore][xml][exporter]") {
    auto lg(make_logger(test_suite));
    fx_export f("FxAccumulator");
    ores::trading::domain::fx_accumulator_instrument row;
    row.identity.trade_id = f.trade_id;
    row.identity.trade_activity_id = f.activity_id;
    row.currency = "GBP";
    row.fixing_amount = ores::utility::decimal::decimal::from_double(1000000.0).value();
    row.underlying_code = "EUR/GBP";
    row.long_short = "Long";
    row.start_date = std::chrono::year{2026} / std::chrono::month{1} / std::chrono::day{15};
    row.strike = ores::utility::decimal::decimal::from_double(0.85).value();
    row.knock_out_barrier = ores::utility::decimal::decimal::from_double(0.95).value();
    f.instruments.fx_accumulator_instruments.push_back(row);

    ores::trading::domain::range_bound bound;
    bound.trade_id = f.trade_id;
    bound.trade_activity_id = f.activity_id;
    bound.sequence_number = 1;
    bound.start_date = row.start_date;
    bound.range_from = row.strike;
    bound.range_to = row.knock_out_barrier;
    bound.strike = row.strike;
    f.instruments.range_bounds.push_back(bound);

    const auto xml = exporter::export_portfolio({f.item}, f.instruments);
    BOOST_LOG_SEV(lg, debug) << "Exported XML:\n" << xml;
    CHECK(xml.contains("<FxAccumulatorData>"));
    CHECK(xml.contains("EUR/GBP"));
}

TEST_CASE("export_portfolio_keeps_a_cross_currency_swap_apart_from_a_swap",
          "[ore][xml][exporter]") {
    auto lg(make_logger(test_suite));

    // The schema states a plain swap and a cross-currency swap in an element
    // each, and both elements are the same type, so the only thing that keeps
    // the two products apart is the code the row states. The document carries
    // one of each.
    const auto f = example_path("IRFX_Cross_Currency_Swap_non_rebalancing.xml");
    const auto imported = importer::import_portfolio_with_context(f);
    REQUIRE(imported.size() == 2);

    int swaps = 0;
    int cross_currency = 0;
    for (const auto& src : imported) {
        const auto* sd = std::get_if<ores::trading::domain::swap_instrument_data>(&src.instrument);
        REQUIRE(sd != nullptr);
        const auto* leaf = std::get_if<ores::trading::domain::vanilla_swap_instrument>(&sd->facts);
        REQUIRE(leaf != nullptr);
        if (sd->header.identity.trade_type_code == "CrossCurrencySwap")
            ++cross_currency;
        else if (sd->header.identity.trade_type_code == "Swap")
            ++swaps;
    }
    CHECK(swaps == 1);
    CHECK(cross_currency == 1);

    auto [items, instruments] = make_export_payload(imported);
    const auto xml = exporter::export_portfolio(items, instruments);
    BOOST_LOG_SEV(lg, debug) << "Exported XML:\n" << xml;

    CHECK(xml.contains("<TradeType>Swap</TradeType>"));
    CHECK(xml.contains("<TradeType>CrossCurrencySwap</TradeType>"));
    CHECK(xml.contains("<SwapData>"));
    CHECK(xml.contains("<CrossCurrencySwapData>"));
}

TEST_CASE("export_portfolio_fx_forward_roundtrip", "[ore][xml][exporter]") {
    auto lg(make_logger(test_suite));

    const auto f = example_path("FX_Forward.xml");
    const auto imported = importer::import_portfolio_with_context(f);
    REQUIRE(imported.size() == 1);

    auto [items, instruments] = make_export_payload(imported);

    const auto xml = exporter::export_portfolio(items, instruments);
    BOOST_LOG_SEV(lg, debug) << "Exported XML:\n" << xml;

    CHECK(!xml.empty());
    CHECK(xml.contains("<FxForwardData>"));
    CHECK(xml.contains(imported.front().ore_id));
}

TEST_CASE("export_portfolio_roundtrips_every_fx_product", "[ore][xml][exporter]") {
    auto lg(make_logger(test_suite));

    // The FX carrier states one array per product, and the batch joins it by
    // trade id in both directions: append_instrument fills the batch's column
    // and the export refills the carrier. A product the two sides pair up
    // wrongly loses its row here and nowhere else, so the document that states
    // each product travels this path.
    struct fx_product_case {
        const char* document;
        const char* element;
    };
    const std::vector<fx_product_case> products{
        {"FX_Forward.xml", "<FxForwardData>"},
        {"FX_Barrier_Option.xml", "<FxBarrierOptionData>"},
        {"FX_Digital_Option.xml", "<FxDigitalOptionData>"},
        {"FX_OneTouch_option.xml", "<FxTouchOptionData>"},
        {"FX_Variance_Swap.xml", "<FxVarianceSwapData>"},
        {"FX_Average_Forward.xml", "<FxAverageForwardData>"},
        {"Exotic_FxAccumulator.xml", "<FxAccumulatorData>"},
        {"Exotic_FxTaRF.xml", "<FxTaRFData>"}};

    for (const auto& product : products) {
        INFO("Example document: " << product.document);
        const auto imported =
            importer::import_portfolio_with_context(example_path(product.document));
        REQUIRE(imported.size() == 1);

        auto [items, instruments] = make_export_payload(imported);
        const auto xml = exporter::export_portfolio(items, instruments);
        BOOST_LOG_SEV(lg, debug) << "Exported XML:\n" << xml;

        CHECK(xml.contains(product.element));
        CHECK(xml.contains(imported.front().ore_id));

        // The accumulator's levels are a child table, so a document that comes
        // back with its entries is the whole join working.
        if (std::string_view(product.element) == "<FxAccumulatorData>") {
            CHECK(xml.contains("<RangeBounds>"));
            CHECK(xml.contains("<RangeTo>122</RangeTo>"));
            CHECK(xml.contains("<RangeFrom>122</RangeFrom>"));
        }
    }
}

TEST_CASE("export_portfolio_ascot_roundtrip", "[ore][xml][exporter]") {
    auto lg(make_logger(test_suite));

    const auto f = example_path("Cash_Ascot.xml");
    const auto imported = importer::import_portfolio_with_context(f);
    REQUIRE(imported.size() == 3);

    auto [items, instruments] = make_export_payload(imported);

    const auto xml = exporter::export_portfolio(items, instruments);
    BOOST_LOG_SEV(lg, debug) << "Exported XML:\n" << xml;

    CHECK(!xml.empty());
    CHECK(xml.contains("<AscotData>"));
    CHECK(xml.contains(imported.front().ore_id));
}

TEST_CASE("export_portfolio_bond_future_roundtrip", "[ore][xml][exporter]") {
    auto lg(make_logger(test_suite));

    // The trade-side BondFutureData holds only these three required
    // elements. The currency, the contract month and the delivery basket
    // sit on the bond future reference datum, keyed by contract name.
    const std::string doc = R"(
<Portfolio>
  <Trade id="RoundtripBondFuture001">
    <TradeType>BondFuture</TradeType>
    <BondFutureData>
      <ContractName>Euro-Bund-Future</ContractName>
      <ContractNotional>100000</ContractNotional>
      <LongShort>Long</LongShort>
    </BondFutureData>
  </Trade>
</Portfolio>
)";
    const auto imported = ores::ore::domain::portfolio_projection::map(
        ores::ore::domain::portfolio_projection::read(doc));
    REQUIRE(imported.size() == 1);
    auto instrument = imported.front().instrument;

    ores::trading::domain::instrument_batch instruments;
    trade_export_item item;
    item.ore_id = imported.front().ore_id;
    item.anchor.id = boost::uuids::random_generator{}();
    item.anchor.trade_type = imported.front().anchor.trade_type;
    ores::trading::domain::stamp_ids(instrument, item.anchor.id);
    ores::trading::domain::append_instrument(instruments, instrument);

    const auto xml = exporter::export_portfolio({item}, instruments);
    BOOST_LOG_SEV(lg, debug) << "Exported XML:\n" << xml;

    CHECK(!xml.empty());
    CHECK(xml.contains("<BondFutureData>"));
    CHECK(xml.contains("RoundtripBondFuture001"));
    CHECK(xml.contains("Euro-Bund-Future"));
}

TEST_CASE("export_portfolio_unmapped_trade_keeps_its_type_and_envelope", "[ore][xml][exporter]") {
    auto lg(make_logger(test_suite));

    trade_export_item item;
    item.ore_id = "FlexiSwap001";
    item.anchor.trade_type = "FlexiSwap";
    // instrument left as monostate: no reverse mapper exists for this type

    ores::trading::domain::trade_envelope_data env;
    env.counter_party = "CPTY";
    env.netting_set_id = "NS";
    item.envelope = env;

    const auto xml = exporter::export_portfolio({item}, {});
    BOOST_LOG_SEV(lg, debug) << "Exported XML:\n" << xml;

    // The schema makes the product data optional, so the trade goes out as
    // its type and its envelope rather than being dropped whole.
    CHECK(xml.contains("FlexiSwap001"));
    CHECK(xml.contains("<TradeType>FlexiSwap</TradeType>"));
    CHECK(xml.contains("<CounterParty>CPTY</CounterParty>"));
    CHECK(xml.contains("<NettingSetId>NS</NettingSetId>"));
    CHECK(!xml.contains("<SwapData>"));
}

TEST_CASE("export_portfolio_unknown_type_is_skipped", "[ore][xml][exporter]") {
    auto lg(make_logger(test_suite));

    trade_export_item item;
    item.ore_id = "UnmappedTrade001";
    item.anchor.trade_type = "UnknownType";
    // instrument left as monostate

    const auto xml = exporter::export_portfolio({item}, {});
    BOOST_LOG_SEV(lg, debug) << "Exported XML:\n" << xml;

    // A type the schema does not name leaves no valid document to write.
    CHECK(!xml.empty());
    CHECK(!xml.contains("UnmappedTrade001"));
}

TEST_CASE("export_portfolio_names_a_bonds_reference_curve_the_run_forwards",
          "[ore][xml][exporter]") {
    auto lg(make_logger(test_suite));

    /*
     * A trade states the curve it is discounted on as the market series' own
     * URI; ORE resolves the name its market configuration forwards that series
     * under. The run supplies the mapping, so the document the engine reads
     * carries ORE's name and not ours.
     */
    const auto f = example_path("Cash_Bonds.xml");
    const auto imported = importer::import_portfolio_with_context(f);
    REQUIRE(!imported.empty());

    auto [items, instruments] = make_export_payload(imported);

    const auto xml = exporter::export_portfolio(items, instruments);
    REQUIRE(xml.contains("EUR-EURIBOR-6M"));

    const auto translated =
        exporter::export_portfolio(items, instruments, [](const std::string& uri) -> std::string {
            return uri == "EUR-EURIBOR-6M" ? "EUR-ESTER" : uri;
        });
    BOOST_LOG_SEV(lg, debug) << "Exported XML:\n" << translated;

    /*
     * Only the reference curve is translated: the same name is also the
     * documents' index and income curve, and those are not the run's to
     * rename.
     */
    CHECK(!translated.contains("<ReferenceCurveId>EUR-EURIBOR-6M</ReferenceCurveId>"));
    CHECK(translated.contains("<ReferenceCurveId>EUR-ESTER</ReferenceCurveId>"));
    CHECK(translated.contains("<IncomeCurveId>EUR-EURIBOR-6M</IncomeCurveId>"));
}

// =============================================================================
// export_pricing_engines tests
// =============================================================================

namespace {

/** One row of the configuration, with the fields the document reads. */
ores::analytics::domain::pricing_model_product
product_at(const std::string& type,
           const std::string& model,
           const std::string& engine,
           int position,
           const boost::uuids::uuid& id = boost::uuids::random_generator()()) {
    ores::analytics::domain::pricing_model_product row;
    row.id = id;
    row.pricing_engine_type_code = type;
    row.model = model;
    row.engine = engine;
    row.position = position;
    return row;
}

ores::analytics::domain::pricing_model_product_parameter
parameter_of(const std::string& scope,
             const std::string& name,
             const std::string& value,
             int position,
             const std::optional<boost::uuids::uuid>& product_id) {
    ores::analytics::domain::pricing_model_product_parameter row;
    row.id = boost::uuids::random_generator()();
    row.pricing_model_product_id = product_id;
    row.parameter_scope = scope;
    row.parameter_name = name;
    row.parameter_value = value;
    row.position = position;
    return row;
}

/** The document a configuration of one swap holds, as the export writes it. */
ores::analytics::messaging::pricing_engines_document swap_configuration() {
    ores::analytics::messaging::pricing_engines_document document;
    const auto swap = product_at("Swap", "DiscountedCashflows", "DiscountingSwapEngine", 0);
    document.products.push_back(swap);
    document.parameters.push_back(parameter_of("model", "Calibration", "Bootstrap", 0, swap.id));
    document.parameters.push_back(
        parameter_of("global", "ContinueOnCalibrationError", "true", 0, std::nullopt));
    return document;
}

}

TEST_CASE("export_pricing_engines_writes_the_document_ore_reads", "[ore][xml][exporter]") {
    auto lg(make_logger(test_suite));

    const auto xml = exporter::export_pricing_engines(swap_configuration());
    BOOST_LOG_SEV(lg, debug) << "Exported XML:\n" << xml;

    CHECK(xml == R"(<?xml version="1.0"?>
<PricingEngines>
  <Product type="Swap">
    <Model>DiscountedCashflows</Model>
    <ModelParameters>
      <Parameter name="Calibration">Bootstrap</Parameter>
    </ModelParameters>
    <Engine>DiscountingSwapEngine</Engine>
    <EngineParameters />
  </Product>
  <GlobalParameters>
    <Parameter name="ContinueOnCalibrationError">true</Parameter>
  </GlobalParameters>
</PricingEngines>
)");
}

TEST_CASE("export_pricing_engines_writes_the_products_in_position_order", "[ore][xml][exporter]") {
    auto lg(make_logger(test_suite));

    ores::analytics::messaging::pricing_engines_document document;
    const auto second = product_at("ForwardBond", "DiscountedCashflows", "Second", 1);
    const auto first = product_at("BondTRS", "DiscountedCashflows", "First", 0);
    document.products.push_back(second);
    document.products.push_back(first);

    const auto xml = exporter::export_pricing_engines(document);
    BOOST_LOG_SEV(lg, debug) << "Exported XML:\n" << xml;

    CHECK(xml.find("BondTRS") < xml.find("ForwardBond"));
}

TEST_CASE("export_pricing_engines_writes_each_products_parameters_under_its_own_scope",
          "[ore][xml][exporter]") {
    auto lg(make_logger(test_suite));

    ores::analytics::messaging::pricing_engines_document document;
    const auto swap = product_at("Swap", "DiscountedCashflows", "DiscountingSwapEngine", 0);
    document.products.push_back(swap);
    document.parameters.push_back(parameter_of("engine", "TimestepPeriod", "3M", 1, swap.id));
    document.parameters.push_back(parameter_of("model", "Calibration", "Bootstrap", 0, swap.id));

    const auto xml = exporter::export_pricing_engines(document);
    BOOST_LOG_SEV(lg, debug) << "Exported XML:\n" << xml;

    // The model parameter sits under ModelParameters and the engine parameter
    // under EngineParameters, each in its own scope's order.
    const auto model_at = xml.find("<ModelParameters>");
    const auto calibration_at = xml.find(R"(<Parameter name="Calibration">Bootstrap</Parameter>)");
    const auto engine_at = xml.find("<EngineParameters>");
    const auto timestep_at = xml.find(R"(<Parameter name="TimestepPeriod">3M</Parameter>)");
    REQUIRE(model_at != std::string::npos);
    REQUIRE(calibration_at != std::string::npos);
    REQUIRE(engine_at != std::string::npos);
    REQUIRE(timestep_at != std::string::npos);
    CHECK(model_at < calibration_at);
    CHECK(calibration_at < engine_at);
    CHECK(engine_at < timestep_at);
}

TEST_CASE("export_pricing_engines_escapes_the_values_it_writes", "[ore][xml][exporter]") {
    auto lg(make_logger(test_suite));

    ores::analytics::messaging::pricing_engines_document document;
    const auto product = product_at("Scripted&Co", "Model<One>", "Engine", 0);
    document.products.push_back(product);
    document.parameters.push_back(
        parameter_of("engine", "Script", R"(x < 1 && y > 2 && s == "z")", 0, product.id));

    const auto xml = exporter::export_pricing_engines(document);
    BOOST_LOG_SEV(lg, debug) << "Exported XML:\n" << xml;

    CHECK(xml.contains(R"(type="Scripted&amp;Co")"));
    CHECK(xml.contains("<Model>Model&lt;One&gt;</Model>"));
    CHECK(xml.contains("x &lt; 1 &amp;&amp; y &gt; 2 &amp;&amp; s == \"z\""));

    // The escaped document is one the reader takes back without a loss.
    const auto read = ores::ore::domain::pricing_engine_projection::read(xml);
    REQUIRE(read.product.size() == 1);
    const auto& parameters = read.product.front().engine_parameters.parameter;
    REQUIRE(parameters.size() == 1);
    CHECK(parameters.front().name == "Script");
    CHECK(parameters.front().value == R"(x < 1 && y > 2 && s == "z")");
}

TEST_CASE("export_pricing_engines_writes_an_empty_configuration_as_empty_sections",
          "[ore][xml][exporter]") {
    auto lg(make_logger(test_suite));

    const auto xml =
        exporter::export_pricing_engines(ores::analytics::messaging::pricing_engines_document{});
    BOOST_LOG_SEV(lg, debug) << "Exported XML:\n" << xml;

    CHECK(xml == R"(<?xml version="1.0"?>
<PricingEngines>
  <GlobalParameters />
</PricingEngines>
)");
}

TEST_CASE("export_pricing_engines_states_the_document_the_round_trip_states",
          "[ore][xml][exporter]") {
    auto lg(make_logger(test_suite));

    const auto configuration = swap_configuration();
    const auto from_rows = exporter::export_pricing_engines(configuration);

    // One writer, two entry points: the document the projection regroups the
    // rows into is written by the same body, and the round trip that delegates
    // to it cannot state a different document.
    const auto document = ores::ore::domain::pricing_engine_projection::reverse(configuration);
    const auto from_document = exporter::export_pricing_engines(document);
    const auto from_round_trip = ores::ore::domain::pricing_engine_projection::write(document);

    BOOST_LOG_SEV(lg, debug) << "From rows:\n" << from_rows;
    CHECK(from_document == from_rows);
    CHECK(from_round_trip == from_rows);
}

TEST_CASE("export_pricing_engines_refuses_a_row_the_document_has_no_place_for",
          "[ore][xml][exporter]") {
    auto lg(make_logger(test_suite));

    ores::analytics::messaging::pricing_engines_document document;
    document.parameters.push_back(
        parameter_of("model", "Calibration", "Bootstrap", 0, boost::uuids::random_generator()()));

    CHECK_THROWS_AS(exporter::export_pricing_engines(document), std::runtime_error);
}
