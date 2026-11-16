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
#include "ores.ore.core/xml/importer.hpp"
#include "ores.testing/project_root.hpp"
#include "ores.trading.api/domain/bond_instrument_rows.hpp"
#include <catch2/catch_test_macros.hpp>
#include <filesystem>
#include <set>
#include <string>
#include <tuple>
#include <variant>
#include <vector>

namespace {

namespace fs = std::filesystem;

using ores::ore::xml::importer;
using ores::trading::domain::bond_instrument_data;

/*
 * The container a booking starts from: the members that are not rows of the
 * trade or the security. Assembling the rows onto it must give back the whole.
 */
bond_instrument_data header_of(const bond_instrument_data& data) {
    bond_instrument_data out;
    out.instrument = data.instrument;
    out.issue = data.issue;
    out.call_dates = data.call_dates;
    out.conversion_targets = data.conversion_targets;
    return out;
}

#define ORES_DIFF_MEMBER(prefix, a, b, m)                                      \
    if (!((a).m == (b).m))                                                    \
        out.push_back(std::string(prefix) + #m)

std::vector<std::string> leg_differences(const std::string& prefix,
                                         const ores::trading::domain::bond_leg_data& a,
                                         const ores::trading::domain::bond_leg_data& b) {
    std::vector<std::string> out;
    ORES_DIFF_MEMBER(prefix, a, b, payer);
    ORES_DIFF_MEMBER(prefix, a, b, leg_type);
    ORES_DIFF_MEMBER(prefix, a, b, currency);
    ORES_DIFF_MEMBER(prefix, a, b, payment_convention);
    ORES_DIFF_MEMBER(prefix, a, b, payment_lag);
    ORES_DIFF_MEMBER(prefix, a, b, payment_calendar);
    ORES_DIFF_MEMBER(prefix, a, b, day_counter);
    ORES_DIFF_MEMBER(prefix, a, b, last_period_day_counter);
    ORES_DIFF_MEMBER(prefix, a, b, notional_payment_lag);
    ORES_DIFF_MEMBER(prefix, a, b, strict_notional_dates);
    ORES_DIFF_MEMBER(prefix, a, b, schedule.rules);
    ORES_DIFF_MEMBER(prefix, a, b, schedule.dates);
    ORES_DIFF_MEMBER(prefix, a, b, amortizations);
    ORES_DIFF_MEMBER(prefix, a, b, notionals);
    ORES_DIFF_MEMBER(prefix, a, b, payment_dates);
    ORES_DIFF_MEMBER(prefix, a, b, indexings_from_asset_leg);
    ORES_DIFF_MEMBER(prefix, a, b, rate);
    ORES_DIFF_MEMBER(prefix, a, b, payment_schedule.rules);
    ORES_DIFF_MEMBER(prefix, a, b, payment_schedule.dates);
    ORES_DIFF_MEMBER(prefix, a, b, settlement);
    return out;
}

/*
 * The members of two bonds that differ, a leg's members named under the leg,
 * so a failure says where a bond stops coming back whole.
 */
std::vector<std::string> differences(const bond_instrument_data& a,
                                     const bond_instrument_data& b) {
    std::vector<std::string> out;
    ORES_DIFF_MEMBER("", a, b, instrument);
    ORES_DIFF_MEMBER("", a, b, issue);
    ORES_DIFF_MEMBER("", a, b, call_dates);
    ORES_DIFF_MEMBER("", a, b, conversion_targets);
    ORES_DIFF_MEMBER("", a, b, option);
    ORES_DIFF_MEMBER("", a, b, trs);
    ORES_DIFF_MEMBER("", a, b, repo);
    ORES_DIFF_MEMBER("", a, b, future);
    ORES_DIFF_MEMBER("", a, b, ascot_row);
    ORES_DIFF_MEMBER("", a, b, option_exercise_dates);
    ORES_DIFF_MEMBER("", a, b, option_data);
    ORES_DIFF_MEMBER("", a, b, strike_data);
    ORES_DIFF_MEMBER("", a, b, option_exercise_schedule);
    ORES_DIFF_MEMBER("", a, b, option_redemption);
    ORES_DIFF_MEMBER("", a, b, option_price_type);
    ORES_DIFF_MEMBER("", a, b, option_knocks_out);
    ORES_DIFF_MEMBER("", a, b, trs_price_type);
    ORES_DIFF_MEMBER("", a, b, forward_long_in_forward);
    ORES_DIFF_MEMBER("", a, b, forward_settlement);
    ORES_DIFF_MEMBER("", a, b, forward_premium);
    ORES_DIFF_MEMBER("", a, b, trs_payer);
    ORES_DIFF_MEMBER("", a, b, trs_initial_price);
    ORES_DIFF_MEMBER("", a, b, trs_schedule);
    ORES_DIFF_MEMBER("", a, b, instrument_underlyings);
    if (a.bond_legs.size() != b.bond_legs.size()) {
        out.push_back("bond_legs.size");
    } else {
        for (std::size_t i = 0; i < a.bond_legs.size(); ++i) {
            const auto legs = leg_differences(
                "bond_legs[" + std::to_string(i) + "].", a.bond_legs[i], b.bond_legs[i]);
            out.insert(out.end(), legs.begin(), legs.end());
        }
    }
    for (const auto& [name, leg_a, leg_b] :
         {std::tuple{"trs_funding_leg.", &a.trs_funding_leg, &b.trs_funding_leg},
          std::tuple{"repo_leg.", &a.repo_leg, &b.repo_leg},
          std::tuple{"ascot_swap_leg.", &a.ascot_swap_leg, &b.ascot_swap_leg}}) {
        const auto legs = leg_differences(name, *leg_a, *leg_b);
        out.insert(out.end(), legs.begin(), legs.end());
    }
    return out;
}

#undef ORES_DIFF_MEMBER

}

TEST_CASE("every bond in the corpus comes back whole from its rows",
          "[ore][xml][bond][rows][corpus][external]") {
    const auto dir = ores::testing::project_root::resolve("external/ore/examples");
    REQUIRE(fs::exists(dir));

    std::size_t bonds = 0;
    std::set<std::string> differing;
    for (const auto& entry : fs::recursive_directory_iterator(dir)) {
        if (!entry.is_regular_file() || entry.path().extension() != ".xml")
            continue;

        std::vector<ores::ore::xml::trade_import_item> items;
        try {
            items = importer::import_portfolio_with_context(entry.path());
        } catch (const std::exception&) {
            continue;
        }

        for (const auto& item : items) {
            const auto* bond = std::get_if<bond_instrument_data>(&item.instrument);
            if (bond == nullptr)
                continue;
            ++bonds;

            const auto rows = ores::trading::domain::disassemble_bond(*bond);
            auto rebuilt = header_of(*bond);
            ores::trading::domain::assemble_bond(rebuilt, rows.trade, rows.issue);
            if (!(rebuilt == *bond)) {
                std::string members;
                for (const auto& m : differences(*bond, rebuilt))
                    members += " " + m;
                differing.insert(entry.path().filename().string() + " (" + item.ore_id +
                                 "):" + members);
            }
        }
    }

    INFO("bonds=" << bonds);
    CHECK(bonds > 0);
    std::string listed;
    for (const auto& name : differing)
        listed += "\n  " + name;
    if (!differing.empty())
        FAIL_CHECK("these bonds do not come back whole from their rows:" + listed);
}
