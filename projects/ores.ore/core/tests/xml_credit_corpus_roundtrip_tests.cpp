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
#include "ores.ore.core/xml/exporter.hpp"
#include "ores.ore.core/xml/importer.hpp"
#include "ores.platform/filesystem/file.hpp"
#include "ores.testing/project_root.hpp"
#include "ores.trading.api/domain/credit_basket_constituent_json_io.hpp"
#include "ores.trading.api/domain/credit_default_swap_instrument_json_io.hpp"
#include "ores.trading.api/domain/credit_instrument_json_io.hpp"
#include "ores.trading.api/domain/credit_linked_swap_instrument_json_io.hpp"
#include "ores.trading.api/domain/index_credit_default_swap_instrument_json_io.hpp"
#include "ores.trading.api/domain/index_credit_default_swap_option_instrument_json_io.hpp"
#include "ores.trading.api/domain/instrument_batch_mapper.hpp"
#include "ores.trading.api/domain/instrument_option_exercise_fee_json_io.hpp"
#include "ores.trading.api/domain/instrument_option_exercise_price_json_io.hpp"
#include "ores.trading.api/domain/instrument_option_json_io.hpp"
#include "ores.trading.api/domain/instrument_option_payment_date_json_io.hpp"
#include "ores.trading.api/domain/instrument_option_premium_json_io.hpp"
#include "ores.trading.api/domain/instrument_schedule_date_json_io.hpp"
#include "ores.trading.api/domain/instrument_schedule_json_io.hpp"
#include "ores.trading.api/domain/instrument_strike_json_io.hpp"
#include "ores.trading.api/domain/risk_participation_agreement_instrument_json_io.hpp"
#include "ores.trading.api/domain/swap_leg_amount_json_io.hpp"
#include "ores.trading.api/domain/swap_leg_json_io.hpp"
#include "ores.trading.api/domain/swap_leg_rate_json_io.hpp"
#include "ores.trading.api/domain/synthetic_cdo_instrument_json_io.hpp"
#include "ores.utility/rfl/reflectors.hpp"       // IWYU pragma: keep.
#include "ores.utility/streaming/std_vector.hpp" // IWYU pragma: keep.
#include <boost/uuid/string_generator.hpp>
#include <catch2/catch_test_macros.hpp>
#include <filesystem>
#include <set>
#include <sstream>
#include <string>
#include <variant>
#include <vector>

/**
 * @file xml_credit_corpus_roundtrip_tests.cpp
 * @brief Proves the credit family's round trip over the ORE example corpus.
 *
 * The directory walk only shows that an exported document reads back. This
 * suite shows the stronger thing: every credit trade in the corpus, imported,
 * exported on its own and imported again, yields the same family rows as the
 * first import. The trade is compared as the rows the family stores, so a
 * field the mapper drops on the way out shows up as a difference.
 */

namespace {

const std::string tags("[ore][xml][roundtrip][credit][corpus][external]");
namespace fs = std::filesystem;

using ores::ore::xml::exporter;
using ores::ore::xml::importer;
using ores::ore::xml::trade_import_item;
using ores::trading::domain::credit_instrument_data;
using ores::trading::domain::instrument_batch;

/**
 * @brief The trade id both imports are stamped with, so the rows compare
 * without the identity the planner would otherwise mint.
 */
boost::uuids::uuid fixed_trade_id() {
    return boost::uuids::string_generator()("11111111-2222-3333-4444-555555555555");
}

/**
 * @brief Every table the credit family writes for a trade, rendered in a fixed
 * order, so two renderings are equal exactly when the family rows are.
 */
std::string family_rows(const trade_import_item& item) {
    auto instrument = item.instrument;
    ores::trading::domain::stamp_ids(instrument, fixed_trade_id());
    instrument_batch b;
    ores::trading::domain::append_instrument(b, instrument);

    std::ostringstream s;
    s << "credit_instruments " << b.credit_instruments << '\n'
      << "credit_default_swap_instruments " << b.credit_default_swap_instruments << '\n'
      << "index_credit_default_swap_instruments " << b.index_credit_default_swap_instruments << '\n'
      << "index_credit_default_swap_option_instruments "
      << b.index_credit_default_swap_option_instruments << '\n'
      << "synthetic_cdo_instruments " << b.synthetic_cdo_instruments << '\n'
      << "credit_linked_swap_instruments " << b.credit_linked_swap_instruments << '\n'
      << "risk_participation_agreement_instruments " << b.risk_participation_agreement_instruments
      << '\n'
      << "credit_basket_constituents " << b.credit_basket_constituents << '\n'
      << "swap_legs " << b.swap_legs << '\n'
      << "swap_leg_amounts " << b.swap_leg_amounts << '\n'
      << "swap_leg_rates " << b.swap_leg_rates << '\n'
      << "instrument_options " << b.instrument_options << '\n'
      << "instrument_option_exercise_fees " << b.instrument_option_exercise_fees << '\n'
      << "instrument_option_exercise_prices " << b.instrument_option_exercise_prices << '\n'
      << "instrument_option_payment_dates " << b.instrument_option_payment_dates << '\n'
      << "instrument_option_premiums " << b.instrument_option_premiums << '\n'
      << "instrument_schedules " << b.instrument_schedules << '\n'
      << "instrument_schedule_dates " << b.instrument_schedule_dates << '\n'
      << "instrument_strikes " << b.instrument_strikes << '\n';
    return s.str();
}

/**
 * @brief Exports one imported trade as a document of its own.
 */
std::string export_one(const trade_import_item& item) {
    auto instrument = item.instrument;
    ores::trading::domain::stamp_ids(instrument, fixed_trade_id());

    ores::trading::messaging::trade_export_item export_item;
    export_item.anchor = item.anchor;
    export_item.anchor.id = fixed_trade_id();
    export_item.ore_id = item.ore_id;
    export_item.envelope = item.envelope;

    instrument_batch b;
    ores::trading::domain::append_instrument(b, instrument);
    return exporter::export_portfolio({export_item}, b);
}

bool is_credit(const trade_import_item& item) {
    return std::holds_alternative<credit_instrument_data>(item.instrument);
}

/**
 * @brief The first line on which two renderings differ, so a failure names
 * the table and the row instead of dumping both.
 */
std::string first_difference(const std::string& a, const std::string& b) {
    std::istringstream sa(a), sb(b);
    std::string la, lb;
    while (std::getline(sa, la) && std::getline(sb, lb)) {
        if (la == lb)
            continue;
        std::size_t i = 0;
        while (i < la.size() && i < lb.size() && la[i] == lb[i])
            ++i;
        const auto from = i > 40 ? i - 40 : 0;
        return la.substr(0, la.find(' ')) + " differs near: first=..." + la.substr(from, 90) +
               " second=..." + lb.substr(from, 90);
    }
    return "the renderings differ in length";
}

/**
 * @brief Credit trades whose round trip is known to change a row. Each name is
 * a document, and the entry stays until the mapper carries what it loses.
 * The list is closed in both directions: a name that stops differing fails
 * the suite until it leaves the list.
 */
const std::set<std::string>& known_row_differences() {
    static const std::set<std::string> differences = {};
    return differences;
}

} // namespace

TEST_CASE("every credit trade in the corpus comes back as the same family rows", tags) {
    const auto dir =
        ores::testing::project_root::resolve("external/ore/examples/Products/Example_Trades");
    REQUIRE(fs::exists(dir));

    const auto scratch = fs::temp_directory_path() / "ores_ore_credit_corpus_roundtrip";
    fs::remove_all(scratch);
    fs::create_directories(scratch);

    std::size_t documents = 0;
    std::size_t trades = 0;
    std::set<std::string> differing;
    std::string detail;

    for (const auto& entry : fs::recursive_directory_iterator(dir)) {
        if (!entry.is_regular_file() || entry.path().extension() != ".xml")
            continue;

        std::vector<trade_import_item> items;
        try {
            items = importer::import_portfolio_with_context(entry.path());
        } catch (const std::exception&) {
            continue;
        }

        bool counted = false;
        for (const auto& item : items) {
            if (!is_credit(item))
                continue;
            if (!counted) {
                ++documents;
                counted = true;
            }
            ++trades;

            const auto name = entry.path().filename().string();
            const auto before = family_rows(item);
            // A rendering with no header or no leg would make two empty
            // renderings equal, so each trade has to hold both.
            CHECK(before.find("credit_instruments []") == std::string::npos);
            CHECK(before.find("swap_legs []") == std::string::npos);
            const auto copy = scratch / (std::to_string(trades) + ".xml");
            ores::platform::filesystem::file::write_content(copy, export_one(item));
            const auto again = importer::import_portfolio_with_context(copy);
            REQUIRE(again.size() == 1);
            const auto after = family_rows(again.front());
            if (before != after) {
                if (differing.insert(name).second)
                    detail += "\n  " + name + " (" + item.ore_id +
                              "): " + first_difference(before, after);
            }
        }
    }
    fs::remove_all(scratch);

    INFO("credit documents=" << documents << " trades=" << trades);
    // A document that stops importing drops out of the walk, so the corpus
    // size is pinned: a change to it is a change to this proof.
    CHECK(documents == 8);
    CHECK(trades == 8);
    if (differing != known_row_differences())
        FAIL_CHECK("the credit documents whose family rows change on the round trip are not the "
                   "known ones:" +
                   detail);
}

TEST_CASE("the credit family rows change when a stored field changes", tags) {
    const auto dir =
        ores::testing::project_root::resolve("external/ore/examples/Products/Example_Trades");
    auto items = importer::import_portfolio_with_context(dir / "Credit_Default_Swap.xml");
    REQUIRE(items.size() == 1);
    REQUIRE(is_credit(items.front()));

    const auto before = family_rows(items.front());
    auto& data = std::get<credit_instrument_data>(items.front().instrument);
    auto* fact = std::get_if<ores::trading::domain::credit_default_swap_instrument>(&data.facts);
    REQUIRE(fact != nullptr);
    fact->recovery_rate = 0.123;

    CHECK(family_rows(items.front()) != before);
}
