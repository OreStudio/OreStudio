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
#include "ores.platform/time/datetime.hpp"
#include "ores.testing/project_root.hpp"
#include "ores.trading.api/domain/bond_instrument_rows.hpp"
#include <catch2/catch_test_macros.hpp>
#include <filesystem>
#include <optional>
#include <set>
#include <string>
#include <tuple>
#include <utility>
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

/*
 * The store keeps a date, not its spelling, so a document's 20160203 comes
 * back as 2016-02-03. Spell the original's dates the way the store does.
 */
void canonical_date(std::string& text) {
    if (!text.empty())
        text = ores::platform::time::datetime::to_iso8601_date(
            ores::platform::time::datetime::from_iso8601_date(text));
}

// An empty optional date element states no date, which is what the store keeps.
void canonical_date(std::optional<std::string>& text) {
    if (text && text->empty())
        text.reset();
    else if (text)
        canonical_date(*text);
}

void canonical_dates(ores::trading::domain::bond_schedule_data& schedule) {
    for (auto& rule : schedule.rules) {
        canonical_date(rule.start_date);
        canonical_date(rule.end_date);
        canonical_date(rule.first_date);
        canonical_date(rule.last_date);
    }
    for (auto& dates : schedule.dates)
        for (auto& date : dates.dates)
            canonical_date(date);
}

void canonical_dates(ores::trading::domain::bond_leg_data& leg) {
    canonical_dates(leg.schedule);
    canonical_dates(leg.payment_schedule);
    for (auto& date : leg.payment_dates)
        canonical_date(date);
    if (leg.rate && leg.rate->floating) {
        canonical_dates(leg.rate->floating->fixing_schedule);
        canonical_dates(leg.rate->floating->reset_schedule);
    }
}

void canonical_dates(bond_instrument_data& data) {
    for (auto& leg : data.bond_legs)
        canonical_dates(leg);
    canonical_dates(data.trs_funding_leg);
    canonical_dates(data.repo_leg);
    canonical_dates(data.ascot_swap_leg);
    canonical_dates(data.trs_schedule);
    if (data.option_exercise_schedule)
        canonical_dates(*data.option_exercise_schedule);
    for (auto& date : data.option_exercise_dates)
        canonical_date(date);
    if (auto& block = data.option_data) {
        canonical_date(block->premium_pay_date);
        for (auto& premium : block->premiums)
            canonical_date(premium.pay_date);
        for (auto& fee : block->exercise_fees)
            canonical_date(fee.start_date);
        if (block->exercise_data)
            canonical_date(block->exercise_data->date);
        if (block->payment_data)
            for (auto& date : block->payment_data->dates)
                canonical_date(date);
    }
}

#define ORES_DIFF_MEMBER(prefix, a, b, m)                                      \
    if (!((a).m == (b).m))                                                    \
        out.push_back(std::string(prefix) + #m)

std::string shown(const std::string& v) { return "'" + v + "'"; }

std::string shown(const std::optional<std::string>& v) { return v ? shown(*v) : "null"; }

std::string shown(const std::optional<bool>& v) {
    return v ? (*v ? "true" : "false") : "null";
}

#define ORES_DIFF_RULE(m)                                                      \
    if (!(a.m == b.m))                                                        \
        out.push_back(prefix + #m + "=" + shown(a.m) + "|" + shown(b.m))

/*
 * The fields of each schedule rule that differ, with both values, so a
 * failure says which spelling the store does not keep.
 */
std::vector<std::string> rule_differences(const std::string& prefix,
                                          const ores::trading::domain::bond_schedule_data& x,
                                          const ores::trading::domain::bond_schedule_data& y) {
    std::vector<std::string> out;
    if (x.rules.size() != y.rules.size()) {
        out.push_back(prefix + "size");
        return out;
    }
    for (std::size_t i = 0; i < x.rules.size(); ++i) {
        const auto& a = x.rules[i];
        const auto& b = y.rules[i];
        ORES_DIFF_RULE(start_date);
        ORES_DIFF_RULE(end_date);
        ORES_DIFF_RULE(adjust_end_date_to_previous_month_end);
        ORES_DIFF_RULE(tenor);
        ORES_DIFF_RULE(calendar);
        ORES_DIFF_RULE(convention);
        ORES_DIFF_RULE(term_convention);
        ORES_DIFF_RULE(rule);
        ORES_DIFF_RULE(end_of_month);
        ORES_DIFF_RULE(end_of_month_convention);
        ORES_DIFF_RULE(first_date);
        ORES_DIFF_RULE(last_date);
        ORES_DIFF_RULE(remove_first_date);
        ORES_DIFF_RULE(remove_last_date);
    }
    return out;
}

#undef ORES_DIFF_RULE

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
    if (!(a.schedule.rules == b.schedule.rules)) {
        const auto rules = rule_differences(prefix + "schedule.rules.", a.schedule, b.schedule);
        out.insert(out.end(), rules.begin(), rules.end());
    }
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
    if (a.option_data && b.option_data) {
        const auto& x = *a.option_data;
        const auto& y = *b.option_data;
        for (const auto& [name, same] : {
                 std::pair{"long_short", x.long_short == y.long_short},
                 std::pair{"option_type", x.option_type == y.option_type},
                 std::pair{"payoff_type", x.payoff_type == y.payoff_type},
                 std::pair{"style", x.style == y.style},
                 std::pair{"settlement", x.settlement == y.settlement},
                 std::pair{"premium_amount", x.premium_amount == y.premium_amount},
                 std::pair{"premium_pay_date", x.premium_pay_date == y.premium_pay_date},
                 std::pair{"premiums", x.premiums == y.premiums},
                 std::pair{"exercise_prices", x.exercise_prices == y.exercise_prices},
                 std::pair{"exercise_fees", x.exercise_fees == y.exercise_fees},
                 std::pair{"exercise_data", x.exercise_data == y.exercise_data},
                 std::pair{"payment_data", x.payment_data == y.payment_data},
                 std::pair{"settlement_data", x.settlement_data == y.settlement_data}})
            if (!same)
                out.push_back(std::string("option_data.") + name);
        if (out.empty() || !out.back().starts_with("option_data."))
            ORES_DIFF_MEMBER("", a, b, option_data);
    } else {
        ORES_DIFF_MEMBER("", a, b, option_data);
    }
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

            auto original = *bond;
            canonical_dates(original);
            const auto rows = ores::trading::domain::disassemble_bond(original);
            auto rebuilt = header_of(original);
            ores::trading::domain::assemble_bond(rebuilt, rows.trade, rows.issue);
            if (!(rebuilt == original)) {
                std::string members;
                for (const auto& m : differences(original, rebuilt))
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
