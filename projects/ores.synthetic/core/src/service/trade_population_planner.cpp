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
#include "ores.synthetic.core/service/trade_population_planner.hpp"
#include <boost/uuid/name_generator_sha1.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <algorithm>
#include <array>
#include <chrono>
#include <cmath>
#include <format>
#include <map>
#include <string>
#include <string_view>
#include <vector>

namespace ores::synthetic::service {

namespace {

namespace domain = ores::synthetic::domain;
using namespace std::chrono;

/*
 * The namespace the population's names are minted in. It is a constant of the
 * generator rather than a value a caller supplies, so a trade id is a function
 * of the scope and never of a deployment.
 */
const boost::uuids::uuid population_namespace = {{0x3f,
                                                  0x2a,
                                                  0x9c,
                                                  0x18,
                                                  0x6d,
                                                  0x4b,
                                                  0x4c,
                                                  0x61,
                                                  0x9e,
                                                  0x0d,
                                                  0x7b,
                                                  0x21,
                                                  0x5a,
                                                  0x88,
                                                  0x0c,
                                                  0x94}};

boost::uuids::uuid named(const std::string& name) {
    static const boost::uuids::name_generator_sha1 generator(population_namespace);
    return generator(name);
}

/*
 * A tenor code such as 3M or 10Y reduces to a count of years. The library
 * resolves a code to a date; a band's ends only have to order, so the years a
 * code names are enough and an unrecognised code says so rather than
 * defaulting to one.
 */
std::uint32_t tenor_years_of(const std::string& code) {
    if (code.size() < 2)
        throw trade_population_error("Tenor code '" + code + "' is too short to read.");

    const auto unit = code.back();
    const auto digits = code.substr(0, code.size() - 1);
    std::uint32_t count = 0;
    try {
        count = static_cast<std::uint32_t>(std::stoul(digits));
    } catch (const std::exception&) {
        throw trade_population_error("Tenor code '" + code + "' does not start with a number.");
    }
    if (count == 0)
        throw trade_population_error("Tenor code '" + code + "' names no period.");

    switch (unit) {
        case 'D':
            return std::max<std::uint32_t>(1, count / 365);
        case 'W':
            return std::max<std::uint32_t>(1, count / 52);
        case 'M':
            return std::max<std::uint32_t>(1, count / 12);
        case 'Y':
            return count;
        default:
            throw trade_population_error("Tenor code '" + code + "' names an unknown unit '" +
                                         std::string(1, unit) + "'.");
    }
}

/*
 * How long before the anchor every generated security was issued. It is longer
 * than the span of trade dates, so no trade is dated before the issue of the
 * security it holds.
 */
constexpr int security_age_months = 6;

std::string format_date(const year_month_day& date) {
    return std::format("{}-{:02}-{:02}",
                       static_cast<int>(date.year()),
                       static_cast<unsigned>(date.month()),
                       static_cast<unsigned>(date.day()));
}

year_month_day add_years(const year_month_day& date, int years) {
    return date + std::chrono::years{years};
}

year_month_day add_months(const year_month_day& date, int months) {
    return date + std::chrono::months{months};
}

/*
 * A draw in [0, 1) that depends only on the scope and the slot. It is built
 * from the same name generator the ids are, so nothing here reads a clock or
 * a global engine and the same scope draws the same numbers.
 */
double draw(const std::string& name, std::uint32_t stream) {
    const auto digest = named(std::format("{}:draw:{}", name, stream));
    std::uint64_t value = 0;
    for (int i = 0; i < 6; ++i)
        value = (value << 8) | digest.data[i];
    return static_cast<double>(value) / static_cast<double>(1ULL << 48);
}

/*
 * The notional band, quantised to a hundredth so the value is one a decimal
 * column holds exactly rather than a binary fraction that reads back with a
 * different tail.
 */
double notional_from(const domain::trade_generation_scope& scope, double unit) {
    const auto span = scope.max_notional - scope.min_notional;
    const auto value = scope.min_notional + span * unit;
    return std::round(value * 100.0) / 100.0;
}

/*
 * The name a slot's identity is minted from. The trade id, the draws and the
 * security all read it, so there is one statement of what a slot is.
 */
std::string slot_name(const domain::trade_generation_scope& scope, std::uint32_t slot) {
    const auto& book_id = scope.book_ids[slot % scope.book_ids.size()];
    const auto& counterparty_id = scope.counterparty_ids[slot % scope.counterparty_ids.size()];
    return std::format("{}:{}:{}:{}:{}:{}",
                       boost::uuids::to_string(scope.config_id),
                       boost::uuids::to_string(scope.party_id),
                       boost::uuids::to_string(book_id),
                       boost::uuids::to_string(counterparty_id),
                       slot,
                       scope.seed);
}

}

void validate_scope(const domain::trade_generation_scope& scope) {
    if (scope.book_ids.empty())
        throw trade_population_error("The config states no book, so the population has nowhere "
                                     "to land.");
    if (scope.counterparty_ids.empty())
        throw trade_population_error("The config states no counterparty, so the population faces "
                                     "nobody.");
    if (scope.trader_usernames.empty())
        throw trade_population_error("The party holds no account that may trade, so no trade can "
                                     "name the person who entered it.");
    if (scope.max_notional < scope.min_notional)
        throw trade_population_error("The notional band's ceiling is below its floor.");

    const auto min_years = tenor_years_of(scope.min_tenor_code);
    const auto max_years = tenor_years_of(scope.max_tenor_code);
    if (max_years < min_years)
        throw trade_population_error("The tenor band's long end '" + scope.max_tenor_code +
                                     "' is shorter than its short end '" + scope.min_tenor_code +
                                     "'.");
}

std::vector<std::string_view> planned_asset_classes() {
    static const std::vector<std::string_view> codes{"bond"};
    return codes;
}

const trade_planner& planner_for(const std::string& asset_class_code) {
    static const bond_trade_planner bond;
    static const std::map<std::string, const trade_planner*> planners{{"bond", &bond}};

    const auto entry = planners.find(asset_class_code);
    if (entry == planners.end()) {
        std::string known;
        for (const auto code : planned_asset_classes()) {
            if (!known.empty())
                known += ", ";
            known += code;
        }
        throw trade_population_error("Asset class '" + asset_class_code +
                                     "' has no trade planner; the planners are " + known + ".");
    }
    return *entry->second;
}

std::string_view bond_trade_planner::asset_class_code() const { return "bond"; }

domain::trade_population_plan
bond_trade_planner::plan(const domain::trade_generation_scope& scope) const {
    validate_scope(scope);

    const auto min_years = tenor_years_of(scope.min_tenor_code);
    const auto max_years = tenor_years_of(scope.max_tenor_code);

    domain::trade_population_plan plan;
    plan.config_id = scope.config_id;
    plan.party_id = scope.party_id;
    plan.as_of_date = scope.as_of_date;

    const auto total = trades_per_book * static_cast<std::uint32_t>(scope.book_ids.size());
    for (std::uint32_t slot = 0; slot < total; ++slot) {
        const auto& book_id = scope.book_ids[slot % scope.book_ids.size()];
        const auto& counterparty_id = scope.counterparty_ids[slot % scope.counterparty_ids.size()];
        const auto& trader = scope.trader_usernames[slot % scope.trader_usernames.size()];
        const auto name = slot_name(scope, slot);

        const auto span = max_years - min_years + 1;
        const auto drawn =
            min_years + static_cast<std::uint32_t>(draw(name, 0) * static_cast<double>(span));

        domain::trade_population_slot entry;
        entry.slot = slot;
        entry.book_id = book_id;
        entry.counterparty_id = counterparty_id;
        entry.trader_username = trader;
        entry.trade_type_code = std::string(bond_trade_type_code);
        entry.trade_id = trade_id_for(scope, slot);
        entry.internal_identifier = internal_identifier_for(scope, slot);
        entry.tenor_years = std::clamp(drawn, min_years, max_years);
        entry.security_id = security_id_for(scope, entry.tenor_years);
        entry.issue_id = issue_id_for_security(entry.security_id);
        entry.issuer_id = issuer_id_for(scope, entry.tenor_years);

        /*
         * The anchor is the config's as-of and never the clock, so a
         * re-provision at the same anchor states the same dates and a later
         * anchor moves them deliberately. The security's dates come from the
         * anchor and its original term alone, so every trade of it states the
         * same ones; the trade date is the only date that belongs to the trade.
         */
        entry.trade_date = add_months(scope.as_of_date, -static_cast<int>(slot % 6));
        entry.issue_date = add_months(scope.as_of_date, -security_age_months);
        entry.maturity_date = add_years(entry.issue_date, static_cast<int>(entry.tenor_years));

        entry.notional = notional_from(scope, draw(name, 1));
        entry.currency = scope.currency_code;
        entry.reference_curve_id = scope.reference_curve_id;

        for (const auto& pair : scope.netting_sets) {
            if (pair.first == counterparty_id) {
                entry.netting_set_id = pair.second;
                break;
            }
        }

        plan.slots.push_back(std::move(entry));
    }

    return plan;
}

boost::uuids::uuid trade_id_for(const domain::trade_generation_scope& scope, std::uint32_t slot) {
    if (scope.book_ids.empty() || scope.counterparty_ids.empty())
        throw trade_population_error("A trade id needs the scope's books and counterparties.");
    return named("trade:" + slot_name(scope, slot));
}

boost::uuids::uuid config_id_for(const boost::uuids::uuid& party_id, const std::string& name) {
    return named(std::format("config:{}:{}", boost::uuids::to_string(party_id), name));
}

boost::uuids::uuid book_scope_id_for(const boost::uuids::uuid& config_id,
                                     const boost::uuids::uuid& book_id) {
    return named(std::format(
        "config_book:{}:{}", boost::uuids::to_string(config_id), boost::uuids::to_string(book_id)));
}

boost::uuids::uuid counterparty_scope_id_for(const boost::uuids::uuid& config_id,
                                             const boost::uuids::uuid& counterparty_id) {
    return named(std::format("config_counterparty:{}:{}",
                             boost::uuids::to_string(config_id),
                             boost::uuids::to_string(counterparty_id)));
}

boost::uuids::uuid issue_id_for_security(const std::string& security_id) {
    return named("bond_issue:" + security_id);
}

std::string internal_identifier_for(const domain::trade_generation_scope& scope,
                                    std::uint32_t slot) {
    const auto config = boost::uuids::to_string(scope.config_id);
    const auto prefix = config.size() >= 8 ? config.substr(0, 8) : config;
    return std::format("POP-{}-{:06}", prefix, slot);
}

std::string security_id_for(const domain::trade_generation_scope& scope,
                            std::uint32_t tenor_years) {
    /*
     * A security is named by its currency, the anchor and its original term,
     * and by nothing about a book or a trade. Any book that keeps the currency
     * may hold it, and two trades of it name one issue.
     */
    const auto digest = named(std::format(
        "bond:{}:{}:{}Y", scope.currency_code, format_date(scope.as_of_date), tenor_years));
    static constexpr char alphabet[] = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ";
    std::string body;
    body.reserve(9);
    for (int i = 0; i < 9; ++i)
        body.push_back(alphabet[digest.data[i] % (sizeof(alphabet) - 1)]);

    /*
     * An ISIN-shaped identifier carries a check digit, and a reader of the
     * population should not have to know that this one's is synthetic.
     */
    const std::string without_check = "POP" + body;
    int sum = 0;
    bool double_it = true;
    for (auto it = without_check.rbegin(); it != without_check.rend(); ++it) {
        const auto digit = (*it >= '0' && *it <= '9') ? (*it - '0') : (10 + (*it - 'A'));
        auto value = double_it ? digit * 2 : digit;
        sum += (value / 10) + (value % 10);
        double_it = !double_it;
    }
    const auto check = (10 - (sum % 10)) % 10;
    return without_check + static_cast<char>('0' + check);
}

boost::uuids::uuid issuer_id_for(const domain::trade_generation_scope& scope,
                                 std::uint32_t tenor_years) {
    if (scope.issuer_ids.empty())
        throw trade_population_error("An issuer needs the tenant's counterparties.");
    auto issuers = scope.issuer_ids;
    std::ranges::sort(issuers);
    return issuers[tenor_years % issuers.size()];
}

}
