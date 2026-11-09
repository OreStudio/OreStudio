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
#include "ores.marketdata.api/datum/market_reference.hpp"
#include "ores.ore.core/xml/document.hpp"
#include "ores.ore.core/xml/exporter.hpp"
#include "ores.ore.core/xml/value_differ.hpp"
#include "ores.ore.core/xml/value_fingerprint.hpp"
#include "ores.platform/filesystem/file.hpp"
#include "ores.testing/project_root.hpp"
#include "ores.trading.api/builders/vanilla_swap_builder.hpp"
#include "ores.trading.api/domain/instrument_batch_mapper.hpp"
#include <catch2/catch_test_macros.hpp>
#include <chrono>
#include <string>
#include <vector>

namespace {

using namespace std::chrono_literals;
using namespace ores::trading::builders;
using ores::ore::xml::document;
using ores::ore::xml::value_entry;
namespace conv = ores::marketdata::conventions;

constexpr auto euribor_6m = "oresmd://ir/EUR?type=fixing&index=ibor&name=EURIBOR&tenor=6M";
constexpr auto euribor = "oresmd://ir/EUR?type=fixing&index=ibor&name=EURIBOR";

/// EUR-6M-SWAP-CONVENTIONS from ORE's examples/Input/conventions.xml.
conv::convention_set ore_example_conventions() {
    ores::refdata::domain::ibor_index_convention index;
    index.id = "EUR-EURIBOR";
    index.fixing_calendar = "TARGET";
    index.day_count_fraction = "A360";
    index.settlement_days = 2;
    index.business_day_convention = "MF";
    index.oresmd_uri = euribor;
    ores::refdata::domain::swap_convention swap;
    swap.id = "EUR-6M-SWAP-CONVENTIONS";
    swap.fixed_calendar = "TARGET";
    swap.fixed_frequency = "Annual";
    swap.fixed_convention = "MF";
    swap.fixed_day_count_fraction = "30/360";
    swap.index = "EUR-EURIBOR-6M";
    swap.oresmd_uri = euribor_6m;
    return {.ibor_indices = {index}, .swaps = {swap}};
}

/// ORE reads a date with or without hyphens; the sample writes it without.
value_entry with_plain_date(value_entry entry) {
    if (entry.path.ends_with("Date"))
        std::erase(entry.value, '-');
    return entry;
}

std::vector<value_entry> swap_data_of(const std::string& xml) {
    const auto doc = document::read(xml);
    REQUIRE(doc.has_value());
    const auto trades = ores::ore::xml::value_fingerprints_of(*doc, "Trade", "id");
    REQUIRE(trades.size() == 1);
    std::vector<value_entry> out;
    for (const auto& entry : trades[0].values)
        if (entry.path.find("/SwapData/") != std::string::npos)
            out.push_back(with_plain_date(entry));
    return out;
}

}

TEST_CASE("a_built_swap_writes_the_swap_data_of_ores_own_sample", "[builders][ore_sample]") {
    using ores::utility::decimal::decimal;
    const auto sample = ores::platform::filesystem::file::read_content(
        ores::testing::project_root::resolve(
            "external/ore/examples/Legacy/Example_1/Input/portfolio_swap.xml"));

    conv::convention_lookup lookup(ore_example_conventions());
    const vanilla_swap_terms terms{
        .notional = *decimal::from_string("10000000"),
        .currency = std::string("EUR"),
        .start = 2016y / 3 / 1,
        .maturity = 2036y / 3 / 1,
        .fixed_rate = *decimal::from_string("0.02"),
        .index = *ores::marketdata::datum::market_reference::parse(euribor_6m),
        .fixed_side = side::receive};
    // The sample rolls its fixed leg Following, where the convention says Modified Following.
    auto built = vanilla_swap_builder(lookup, terms).fixed_adjustment("F").build();
    REQUIRE(built.has_value());

    ores::trading::domain::instrument_batch batch;
    ores::trading::domain::append_instrument(batch, *built);
    ores::trading::messaging::trade_export_item item;
    item.anchor.trade_type = std::string(vanilla_swap_builder::trade_type);
    item.ore_id = "built";

    const auto xml = ores::ore::xml::exporter::export_portfolio({item}, batch);
    std::string differences;
    for (const auto& difference :
         ores::ore::xml::diff_values(swap_data_of(sample), swap_data_of(xml)))
        differences += ores::ore::xml::describe(difference) + "\n";
    INFO(differences);
    CHECK(differences.empty());
}
