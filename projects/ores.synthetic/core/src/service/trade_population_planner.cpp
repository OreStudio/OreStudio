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
#include "ores.marketdata.api/datum/market_datum.hpp"
#include "ores.marketdata.api/datum/market_index.hpp"
#include "ores.marketdata.api/datum/ore_key_codec.hpp"
#include "ores.marketdata.api/projection/ore_projection.hpp"
#include "ores.trading.api/builders/barrier_product_builders.hpp"
#include "ores.trading.api/builders/basket_option_builders.hpp"
#include "ores.trading.api/builders/bond_builders.hpp"
#include "ores.trading.api/builders/commodity_builders.hpp"
#include "ores.trading.api/builders/composite_builders.hpp"
#include "ores.trading.api/builders/credit_builders.hpp"
#include "ores.trading.api/builders/entry_reset_option_builders.hpp"
#include "ores.trading.api/builders/equity_builders.hpp"
#include "ores.trading.api/builders/fx_builders.hpp"
#include "ores.trading.api/builders/multi_asset_builders.hpp"
#include "ores.trading.api/builders/rate_builders.hpp"
#include "ores.trading.api/builders/scripted_builders.hpp"
#include "ores.trading.api/builders/volatility_product_builders.hpp"
#include "ores.trading.api/builders/worst_of_basket_swap_builders.hpp"
#include "ores.trading.api/domain/bond_product_batch.hpp"
#include "ores.trading.api/domain/instrument_batch_mapper.hpp"
#include <boost/uuid/name_generator_sha1.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <algorithm>
#include <array>
#include <chrono>
#include <cmath>
#include <format>
#include <map>
#include <stdexcept>
#include <string>
#include <string_view>
#include <vector>

namespace ores::synthetic::service {

namespace {

constexpr double coupon_rate = 0.05;

/*
 * The face value of a generated security. A trade's size is the instrument's
 * notional, which the engine reads as the bond notional, so the security itself
 * states a unit face and carries no trade's size.
 */
constexpr double security_face_value = 1.0;

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

/*
 * The date a year, a month and a day state, with a day the month does not have
 * held to the month's last. Calendar arithmetic that keeps the day of the month
 * invents days: a month taken from the 31st of October is the 31st of
 * September, which is not a date, and a year added to the 29th of February is
 * the 29th of a year that has none.
 */
year_month_day state_day(year y, month m, day d) {
    const auto last_day = year_month_day_last{y, month_day_last{m}};
    return year_month_day{y, m, d > last_day.day() ? last_day.day() : d};
}

year_month_day add_years(const year_month_day& date, int years) {
    return state_day(year{static_cast<int>(date.year()) + years}, date.month(), date.day());
}

year_month_day add_months(const year_month_day& date, int months) {
    const auto shifted = year_month{date.year(), date.month()} + std::chrono::months{months};
    return state_day(shifted.year(), shifted.month(), date.day());
}

/*
 * Quarterly observation dates after @p after and by @p until, each moved off a
 * weekend to the Monday. A span shorter than a quarter observes on @p until.
 */
std::vector<year_month_day> quarterly_dates(const year_month_day& after,
                                            const year_month_day& until) {
    std::vector<year_month_day> out;
    for (int months = 3;; months += 3) {
        auto day = std::chrono::sys_days{add_months(after, months)};
        const auto weekday = std::chrono::weekday{day};
        if (weekday == std::chrono::Saturday)
            day += std::chrono::days{2};
        else if (weekday == std::chrono::Sunday)
            day += std::chrono::days{1};
        if (year_month_day{day} > until)
            break;
        out.emplace_back(day);
    }
    if (out.empty())
        out.push_back(until);
    return out;
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
 * The trade types a config books: the types its class cycles, narrowed to those
 * the config states and kept in the class's own order. A config that states none
 * books every type of its class. A stated type the class does not book is
 * refused, because the population would otherwise leave it out without a word.
 */
template <typename Type, std::size_t Count, typename CodeOf>
std::vector<const Type*> booked_types(const Type (&types)[Count],
                                      const domain::trade_generation_scope& scope,
                                      CodeOf code_of) {
    const auto stated = [&](std::string_view code) {
        return std::ranges::find(scope.trade_type_codes, code) != scope.trade_type_codes.end();
    };
    for (const auto& code : scope.trade_type_codes) {
        if (std::ranges::none_of(types, [&](const Type& type) { return code_of(type) == code; }))
            throw trade_population_error("The config states trade type '" + code +
                                         "', which its asset class does not book.");
    }
    std::vector<const Type*> booked;
    for (const auto& type : types) {
        if (scope.trade_type_codes.empty() || stated(code_of(type)))
            booked.push_back(&type);
    }
    return booked;
}

template <std::size_t Count>
std::vector<const std::string_view*> booked_types(const std::string_view (&types)[Count],
                                                  const domain::trade_generation_scope& scope) {
    return booked_types(types, scope, [](std::string_view type) { return type; });
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
    /*
     * A population is dated from its anchor, so an anchor that is not a date
     * states no population: every date the run mints would be the absent one.
     */
    if (!scope.as_of_date.ok())
        throw trade_population_error("The config states no anchor date, so no trade can be "
                                     "dated.");
    if (scope.book_ids.empty())
        throw trade_population_error("The config states no book, so the population has nowhere "
                                     "to land.");
    if (scope.counterparty_ids.empty())
        throw trade_population_error("The config states no counterparty, so the population faces "
                                     "nobody.");
    if (scope.traders.empty())
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
    static const std::vector<std::string_view> codes{
        "bond", "commodity", "credit", "equity", "fx", "inflation", "interest_rates", "scripted"};
    return codes;
}

const trade_planner& planner_for(const std::string& asset_class_code) {
    static const bond_trade_planner bond;
    static const commodity_trade_planner commodity;
    static const rate_trade_planner rates;
    static const fx_trade_planner fx;
    static const credit_trade_planner credit;
    static const equity_trade_planner equity;
    static const inflation_trade_planner inflation;
    static const scripted_trade_planner scripted;
    static const std::map<std::string, const trade_planner*> planners{{"bond", &bond},
                                                                      {"commodity", &commodity},
                                                                      {"credit", &credit},
                                                                      {"equity", &equity},
                                                                      {"fx", &fx},
                                                                      {"inflation", &inflation},
                                                                      {"interest_rates", &rates},
                                                                      {"scripted", &scripted}};

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

std::size_t routed_code_count(std::string_view asset_class_code) {
    return planner_for(std::string(asset_class_code)).code_count();
}

std::uint32_t slot_count(const domain::trade_generation_scope& scope) {
    return scope.trades_per_book * static_cast<std::uint32_t>(scope.book_ids.size());
}

/*
 * How many times the class's types have cycled before this slot. A type
 * recurs once per turn, so a choice picked from the turn changes for the type,
 * where one picked from the slot's position alone would repeat with it.
 */
std::uint32_t turn_of(std::uint32_t slot, std::size_t type_count) {
    return static_cast<std::uint32_t>(slot / std::max<std::size_t>(type_count, 1));
}

/*
 * The book is the slot's position shifted once every book's worth of turns.
 * The shift is constant across each aligned run of as many slots as books, so
 * each run still names every book once and a book holds exactly its trades
 * per book; and a type's next turns land it in the other books.
 */
std::size_t book_index(std::uint32_t slot, std::uint32_t turn, std::size_t book_count) {
    const auto books = std::max<std::size_t>(book_count, 1);
    return (slot + turn / books) % books;
}

/*
 * Which way a closed either-or falls: long or short, pay or receive, call or
 * put. Each either-or of one trade reads its own stream, the stream-th bit of
 * the turn, so the choices of a trade are independent: over eight turns every
 * pair of them states all four combinations, and stream 0 flips each turn
 * while the book moves only once per books' worth of turns.
 */
bool first_way(const domain::trade_population_slot& entry, unsigned stream = 0) {
    return ((entry.turn >> stream) & 1u) == 0;
}

domain::trade_population_slot common_slot(const domain::trade_generation_scope& scope,
                                          std::uint32_t slot,
                                          std::size_t type_count) {
    domain::trade_population_slot entry;
    entry.slot = slot;
    entry.turn = turn_of(slot, type_count);
    entry.book_id = scope.book_ids[book_index(slot, entry.turn, scope.book_ids.size())];
    entry.counterparty_id = scope.counterparty_ids[slot % scope.counterparty_ids.size()];
    entry.trader = scope.traders[slot % scope.traders.size()];
    entry.trade_id = trade_id_for(scope, slot);
    entry.internal_identifier = internal_identifier_for(scope, slot);
    entry.trade_date = add_months(scope.as_of_date, -static_cast<int>(slot % 6));
    entry.currency = scope.currency_code;

    /*
     * The set a counterparty is cleared under is a fact of the counterparty, so
     * every slot of it states the same one. A counterparty with no set is
     * ordinary: a booking without a set is one the database accepts.
     */
    for (const auto& pair : scope.netting_sets) {
        if (pair.first == entry.counterparty_id) {
            entry.netting_set_id = pair.second;
            break;
        }
    }

    return entry;
}

std::string_view bond_trade_planner::asset_class_code() const {
    return "bond";
}


namespace {

/*
 * The swaps a rate trade draws: one for each swap the tenant's conventions
 * state on an index with a tenor, so a builder can read each leg's terms and a
 * forward rate agreement or a cap resets on an Ibor index. The domestic ones
 * are in the scope's currency, the foreign ones in any other.
 *
 * A swap is the index with the legs of one convention. Conventions that state
 * the same legs on one index under several ids, such as GBP-3M-SWAP and
 * GBP-LIBOR-3M-SWAP, are one swap, drawn by the least id, so an index with
 * three aliases is not three times as likely. Conventions that state other
 * legs on one index, such as USD LIBOR 3M with an annual and a semiannual
 * fixed leg, are two swaps, and the builder is told which by id.
 */
struct swap_choice final {
    ores::marketdata::datum::market_reference index;
    std::string convention;
    /// Whether the swap floats on an Ibor index rather than an overnight one,
    /// which is what the index's own address says.
    bool ibor = false;
};

struct swap_choices final {
    std::vector<swap_choice> domestic;
    std::vector<swap_choice> foreign;
};

swap_choices swap_choices_of(const domain::trade_generation_scope& scope) {
    namespace md = ores::marketdata::datum;
    const auto legs_of = [](const ores::refdata::domain::swap_convention& c) {
        return std::format("{}|{}|{}|{}|{}|{}|{}|{}",
                           *c.oresmd_uri,
                           c.fixed_calendar.value_or(""),
                           c.fixed_frequency,
                           c.fixed_convention.value_or(""),
                           c.fixed_day_count_fraction,
                           c.index,
                           c.float_frequency.value_or(""),
                           c.sub_periods_coupon_type.value_or(""));
    };
    std::map<std::string, const ores::refdata::domain::swap_convention*> swaps;
    for (const auto& swap : scope.conventions.swaps) {
        if (!swap.oresmd_uri)
            continue;
        auto [it, added] = swaps.emplace(legs_of(swap), &swap);
        if (!added && swap.id < it->second->id)
            it->second = &swap;
    }
    /*
     * A swap floats on an index convention, and the convention's id is the
     * index's name without the tenor a swap's own index states. A forward rate
     * agreement may only float on an Ibor, so the pool says which swaps do.
     */
    const auto floats_on_an_ibor = [&](const std::string& index_code) {
        for (const auto& ibor : scope.conventions.ibor_indices)
            if (!ibor.id.empty() &&
                (index_code == ibor.id || index_code.starts_with(ibor.id + "-")))
                return true;
        return false;
    };
    swap_choices out;
    for (const auto& [legs, swap] : swaps) {
        auto index = md::market_reference::parse(*swap->oresmd_uri);
        if (!index || index->index() == nullptr || index->index()->get("tenor") == nullptr)
            continue;
        auto& pool = index->index()->subject() == scope.currency_code ? out.domestic : out.foreign;
        pool.push_back(swap_choice{std::move(*index), swap->id, floats_on_an_ibor(swap->index)});
    }
    return out;
}

/*
 * The ten codes the catalogue routes to the bond instrument table. The planner
 * draws them by position, so a config that wants every code states one trade per
 * code. BondFutureOption is a bond product the catalogue does not route to a
 * table, so the planner cannot book it and does not draw it.
 */
constexpr std::string_view bond_types[] = {"Bond",
                                           "BondOption",
                                           "ForwardBond",
                                           "BondFuture",
                                           "BondRepo",
                                           "BondTRS",
                                           "BondPosition",
                                           "CallableBond",
                                           "ConvertibleBond",
                                           "Ascot",
                                           "BondFutureOption"};
constexpr std::size_t bond_type_count = sizeof(bond_types) / sizeof(bond_types[0]);

constexpr std::string_view bond_futures[] = {"FGBL", "FGBM", "ZN", "ZB"};

ores::trading::domain::bond_product bond_product_of(
    std::expected<ores::trading::domain::bond_product, ores::trading::builders::build_error> built,
    const std::string& trade_type_code) {
    if (!built)
        throw trade_population_error("The " + trade_type_code + " builder refuses the drawn " +
                                     built.error().term + ": " + built.error().message);
    return std::move(*built);
}

/*
 * The bond family builds each trade through its builder, on the security the
 * slot holds. The security is the slot's to state, because it is written
 * before the trade.
 */
ores::trading::domain::bond_product build_bond_trade(const domain::trade_generation_scope& scope,
                                                     const swap_choices& swaps_of_scope,
                                                     const domain::trade_population_slot& entry,
                                                     const std::string& security_id,
                                                     const boost::uuids::uuid& issue_id,
                                                     year_month_day maturity) {
    namespace tb = ores::trading::builders;
    namespace md = ores::marketdata::datum;
    using ores::utility::decimal::decimal;
    const auto money = [](double value) {
        return decimal::from_double(value).value();
    };

    const auto slot = entry.slot;
    const auto& type = entry.trade_type_code;
    const auto name = slot_name(scope, slot);
    const tb::bond_holding holding{
        .issue_id = issue_id, .notional = money(entry.notional), .currency = entry.currency};
    const auto kind = draw(name, 5) < 0.5 ? tb::option_kind::call : tb::option_kind::put;
    const auto position =
        first_way(entry) ? tb::option_position::long_option : tb::option_position::short_option;
    const auto rate = [&](double low, double span, std::uint32_t stream) {
        return money(std::round((low + span * draw(name, stream)) * 10000.0) / 10000.0);
    };
    const auto built = [&](auto result) {
        return bond_product_of(std::move(result), type);
    };
    const auto convertible = [&]() {
        if (scope.equity_underlyings.empty())
            throw trade_population_error(
                "A convertible bond states the share it may convert into, and the tenant holds no "
                "equity underlying. Publish the equity catalogue first.");
        /*
         * ORE converts into a share of another currency only through an FX
         * index the conversion data states, so the share is drawn from those
         * in the bond's currency when the catalogue holds one.
         */
        std::vector<const domain::equity_underlying_reference*> candidates;
        for (const auto& underlying : scope.equity_underlyings)
            if (underlying.currency == entry.currency)
                candidates.push_back(&underlying);
        if (candidates.empty())
            for (const auto& underlying : scope.equity_underlyings)
                candidates.push_back(&underlying);
        const auto position_drawn = static_cast<std::size_t>(
            draw(security_id, 10) * static_cast<double>(candidates.size()));
        return tb::convertible_bond_terms{
            .holding = holding,
            .share = candidates[std::min(position_drawn, candidates.size() - 1)]->code,
            .conversion_ratio = std::round((5.0 + 10.0 * draw(security_id, 11)) * 100.0) / 100.0};
    };

    if (type == "Bond")
        return built(tb::bond_builder(holding).build());
    if (type == "BondPosition") {
        auto security = md::market_index::make(md::index_family::bond, security_id, {});
        if (!security)
            throw trade_population_error("The security " + security_id +
                                         " has no address: " + security.error());
        auto reference = md::market_reference::of(std::move(*security));
        if (!reference)
            throw trade_population_error("The security " + security_id +
                                         " has no address: " + reference.error());
        return built(
            tb::bond_position_builder(
                {.holding = holding, .security = std::move(*reference), .identifier = security_id})
                .build());
    }
    if (type == "BondOption")
        return built(
            tb::bond_option_builder(
                {.holding = holding,
                 .kind = kind,
                 .strike = money(std::round((95.0 + 10.0 * draw(name, 6)) * 100.0) / 100.0),
                 .expiry = add_years(scope.as_of_date, 1),
                 .position = position})
                .build());
    if (type == "ForwardBond") {
        /*
         * A forward on the bond matures a year out, or halfway to the bond's
         * maturity when the bond matures sooner: ORE prices no forward past it.
         */
        const auto halfway = year_month_day{sys_days{scope.as_of_date} +
                                            (sys_days{maturity} - sys_days{scope.as_of_date}) / 2};
        const auto forward = std::min(add_years(scope.as_of_date, 1), halfway);
        return built(tb::forward_bond_builder({.holding = holding,
                                               .long_in_forward = first_way(entry),
                                               .forward_maturity = forward,
                                               .settlement = forward,
                                               .lock_rate = rate(0.02, 0.03, 7)})
                         .build());
    }
    if (type == "BondFutureOption")
        // The option expires a month out, before any listed contract's delivery.
        return built(
            tb::bond_future_option_builder(
                {.holding = holding,
                 .contract = std::string(bond_futures[slot % 4]),
                 .contract_notional = money(100000.0),
                 .strike = money(std::round((100.0 + 30.0 * draw(name, 6)) * 100.0) / 100.0),
                 .kind = draw(name, 5) < 0.5 ? tb::option_kind::call : tb::option_kind::put,
                 .position = position,
                 .expiry = add_months(scope.as_of_date, 1)})
                .build());
    if (type == "BondFuture")
        return built(tb::bond_future_builder({.holding = holding,
                                              .contract = std::string(bond_futures[slot % 4]),
                                              .contract_notional = money(100000.0),
                                              .position = position})
                         .build());
    if (type == "BondRepo")
        return built(tb::bond_repo_builder({.holding = holding,
                                            .repo_rate = rate(0.03, 0.02, 8),
                                            .start = scope.as_of_date,
                                            .end = add_years(scope.as_of_date, 1)})
                         .build());
    if (type == "BondTRS")
        return built(tb::bond_trs_builder({.holding = holding,
                                           .side = first_way(entry) ? tb::return_side::receive :
                                                                      tb::return_side::pay,
                                           .funding_rate = rate(0.02, 0.01, 9),
                                           .start = scope.as_of_date,
                                           .end = add_years(scope.as_of_date, 1)})
                         .build());
    if (type == "CallableBond")
        return built(tb::callable_bond_builder(
                         {.holding = holding,
                          .calls = std::vector<tb::bond_call>{{.date = add_years(maturity, -2),
                                                               .style = "American",
                                                               .price = money(1.02),
                                                               .price_type = "Clean",
                                                               .include_accrual = true},
                                                              {.date = add_years(maturity, -1),
                                                               .style = "Bermudan",
                                                               .price = money(1.0),
                                                               .price_type = "Dirty",
                                                               .include_accrual = false}}})
                         .build());
    if (type == "ConvertibleBond")
        return built(tb::convertible_bond_builder(convertible()).build());
    if (type == "Ascot") {
        const auto& swaps = swaps_of_scope.domestic;
        if (swaps.empty())
            throw trade_population_error("No swap convention of the tenant names an index in " +
                                         scope.currency_code +
                                         ", so no Ascot can state its swap leg.");
        return built(tb::ascot_builder({.convertible = convertible(),
                                        .kind = kind,
                                        .position = position,
                                        .funding_index = swaps[slot % swaps.size()].index,
                                        .recall_spread = money(0.02),
                                        .start = scope.as_of_date,
                                        .end = maturity})
                         .build());
    }
    throw trade_population_error("The bond trade type " + type +
                                 " states no builder this planner knows.");
}

}

std::size_t bond_trade_planner::code_count() const {
    return bond_type_count;
}

namespace {

ores::marketdata::datum::market_reference credit_curve_of(const std::string& lei,
                                                          const std::string& currency);

}

domain::trade_population_plan
bond_trade_planner::plan(const domain::trade_generation_scope& scope) const {
    validate_scope(scope);

    const auto min_years = tenor_years_of(scope.min_tenor_code);
    const auto max_years = tenor_years_of(scope.max_tenor_code);
    const auto swaps = swap_choices_of(scope);

    domain::trade_population_plan plan;
    plan.config_id = scope.config_id;
    plan.party_id = scope.party_id;
    plan.as_of_date = scope.as_of_date;

    const auto types = booked_types(bond_types, scope);
    for (std::uint32_t slot = 0; slot < slot_count(scope); ++slot) {
        const auto name = slot_name(scope, slot);

        const auto span = max_years - min_years + 1;
        const auto drawn =
            min_years + static_cast<std::uint32_t>(draw(name, 0) * static_cast<double>(span));

        const auto tenor_years = std::clamp(drawn, min_years, max_years);
        const auto issuer = issuer_for(scope, tenor_years);
        const auto security_id =
            security_id_for(scope, tenor_years, *types[slot % types.size()], issuer.id);
        const auto issue_id = issue_id_for_security(security_id);
        const auto issue_date = add_months(scope.as_of_date, -security_age_months);
        const auto maturity = add_years(issue_date, static_cast<int>(tenor_years));

        auto entry = common_slot(scope, slot, types.size());
        entry.trade_type_code = std::string(*types[slot % types.size()]);
        entry.notional = notional_from(scope, draw(name, 1));

        const auto money = [](double value) {
            return ores::utility::decimal::decimal::from_double(std::round(value * 100.0) / 100.0)
                .value();
        };
        auto security = ores::trading::builders::bond_issue_builder(
                            {.issue_id = issue_id,
                             .security_id = security_id,
                             .issuer = issuer.name,
                             .currency = entry.currency,
                             .face_value = money(security_face_value),
                             .coupon_rate = money(coupon_rate),
                             .issue_date = issue_date,
                             .maturity = maturity,
                             .calendar = std::string("TARGET"),
                             .reference_curve = scope.reference_curve_id})
                            .build();
        if (!security)
            throw trade_population_error("The bond issue builder refuses the drawn " +
                                         security.error().term + ": " + security.error().message);
        /*
         * ORE prices a bond option on the yield volatility its bond names, and
         * refuses the trade when the bond names none. The name follows the
         * Products market's BondOptionVols-<currency>.
         */
        if (entry.trade_type_code == "BondOption")
            security->issue.volatility_curve_id = "BondOptionVols-" + entry.currency;
        /*
         * A callable bond's engine reads its default curve, recovery and
         * spread from the credit curve its bond names, and dereferences an
         * empty handle when it names none. The curve is the one a credit trade
         * on the same entity names.
         */
        if (entry.trade_type_code == "CallableBond" && scope.credit_reference_entities.empty())
            throw trade_population_error(
                "The tenant holds no legal entity, so no callable bond can name a credit "
                "curve. Publish the LEI dataset first.");
        /*
         * The curve follows the security, not the slot: slots that hold one
         * security state one issue, and the store refuses an issue restated
         * with another curve.
         */
        if (entry.trade_type_code == "CallableBond") {
            const auto& entities = scope.credit_reference_entities;
            const auto drawn = static_cast<std::size_t>(draw(security_id, 12) *
                                                        static_cast<double>(entities.size()));
            security->issue.credit_curve_id =
                credit_curve_of(entities[std::min(drawn, entities.size() - 1)], entry.currency)
                    .uri();
        }
        entry.bond_issue = std::move(security->issue);
        entry.bond_issue_legs = std::move(security->legs);
        entry.bond_issue_leg_amounts = std::move(security->amounts);
        entry.bond_issue_leg_rates = std::move(security->rates);
        entry.bond_issue_leg_schedules = std::move(security->schedules);
        auto product = build_bond_trade(scope, swaps, entry, security_id, issue_id, maturity);
        entry.bond_issue_call_dates = product.call_dates;
        entry.bond_issue_conversion_targets = product.conversion_targets;
        append(entry.instruments, std::move(product));

        plan.slots.push_back(std::move(entry));
    }

    return plan;
}

namespace {

/*
 * The commodity codes the catalogue routes to commodity_instrument, in the
 * order the catalogue states them, each with the shape the ORE importer reads
 * it as. The shape decides which fields a slot states; the other twenty-odd
 * columns stay empty the way a flat data-oriented row expects.
 */
/**
 * @brief One member of a commodity basket, as the catalogue spells it.
 */
struct drawn_basket_member final {
    std::string code;
    std::optional<double> weight;
};

/**
 * @brief What the commodity planner draws for one slot, before it states rows.
 *
 * The family states rows in the plan, so this is the drawing's own shape and
 * never a slot's bag. A commodity table is wide and flat: the same one holds a
 * forward, an option, an Asian or a variance swap, and each product states the
 * fields its own terms need and leaves the rest empty.
 */
struct drawn_commodity final {
    /**
     * @brief The commodity the trade names, as the catalogue spells it, such
     * as =NYMEX:CL=.
     */
    std::string commodity_code;
    /// The unit the commodity is quoted in, taken from its catalogue row.
    std::string unit;
    /// The quantity in that unit.
    double quantity = 0.0;
    std::chrono::year_month_day start_date;
    std::chrono::year_month_day maturity_date;
    /// The price a forward or a fixed leg states.
    std::optional<double> fixed_price;
    /// The option's side, as the catalogue spells it: =Call= or =Put=.
    std::string option_type;
    std::optional<double> strike_price;
    std::string exercise_type;
    std::string average_type;
    std::optional<std::chrono::year_month_day> averaging_start_date;
    std::optional<std::chrono::year_month_day> averaging_end_date;
    std::string spread_commodity_code;
    std::optional<double> spread_amount;
    std::string strip_frequency_code;
    std::optional<double> variance_strike;
    std::optional<double> accumulation_amount;
    std::optional<double> knock_out_barrier;
    std::string barrier_type;
    std::optional<double> lower_barrier;
    std::optional<double> upper_barrier;
    std::optional<std::chrono::year_month_day> swaption_expiry_date;
    /// The members a basket product names, in the order it draws them.
    std::vector<drawn_basket_member> basket;
};

enum class commodity_shape {
    forward,
    option,
    asian_option,
    average_price_option,
    option_strip,
    barrier_option,
    accumulator,
    target_redemption,
    spread_option,
    variance_swap,
    swap,
    swaption,
    basket_option
};

/// The barrier a drawn barrier type names, as the builder takes it.
ores::trading::builders::barrier_kind commodity_barrier_kind_of(const std::string& type) {
    namespace tb = ores::trading::builders;
    if (type == "UpAndIn")
        return tb::barrier_kind::up_and_in;
    if (type == "UpAndOut")
        return tb::barrier_kind::up_and_out;
    if (type == "DownAndIn")
        return tb::barrier_kind::down_and_in;
    if (type == "DownAndOut")
        return tb::barrier_kind::down_and_out;
    throw trade_population_error("The barrier type " + type + " is not one a builder takes.");
}

/// The oresmd address of a commodity the catalogue names, which the projection writes as ORE names
/// it.
ores::marketdata::datum::market_reference commodity_address_of(const std::string& code) {
    namespace md = ores::marketdata::datum;
    auto index = md::market_index::make(md::index_family::commodity, code, {});
    if (!index)
        throw trade_population_error("The commodity " + code + " has no address: " + index.error());
    auto reference = md::market_reference::of(std::move(*index));
    if (!reference)
        throw trade_population_error("The commodity " + code +
                                     " has no address: " + reference.error());
    return std::move(*reference);
}

/*
 * The commodity types a builder states, which is every type the generator
 * books: a type no builder answers for throws rather than falling back to a
 * second constructor this planner once carried.
 */
std::optional<swap_choice> funding_index_in(const domain::trade_generation_scope& scope,
                                            const std::string& currency);

ores::trading::domain::instrument_batch
built_commodity_trade(const domain::trade_generation_scope& scope,
                      const domain::trade_population_slot& entry,
                      const drawn_commodity& drawn,
                      const std::string& currency) {
    namespace tb = ores::trading::builders;
    using ores::utility::decimal::decimal;
    const auto money = [](double value) {
        return decimal::from_double(std::round(value * 100.0) / 100.0).value();
    };
    const auto& type = entry.trade_type_code;
    const auto name = slot_name(scope, entry.slot);
    const auto underlying = commodity_address_of(drawn.commodity_code);
    const auto quantity = money(drawn.quantity);
    const auto start = drawn.start_date;
    const auto end = drawn.maturity_date;
    const auto position =
        first_way(entry) ? tb::option_position::long_option : tb::option_position::short_option;
    const auto kind = drawn.option_type == "Put" ? tb::option_kind::put : tb::option_kind::call;
    const auto strike = money(drawn.strike_price.value_or(0.0));
    // A swaption's fixed price is its strike.
    const auto price = money(drawn.fixed_price.value_or(drawn.strike_price.value_or(0.0)));
    const auto built = [&](tb::commodity_result result) {
        if (!result)
            throw trade_population_error("The " + type + " builder refuses the drawn " +
                                         result.error().term + ": " + result.error().message);
        ores::trading::domain::instrument_batch batch;
        ores::trading::domain::append_instrument(
            batch, ores::trading::domain::trade_instrument{std::move(*result)});
        return batch;
    };
    const tb::commodity_option_terms option{.underlying = underlying,
                                            .currency = currency,
                                            .quantity = quantity,
                                            .kind = kind,
                                            .strike = strike,
                                            .expiry = end,
                                            .position = position,
                                            .unit = drawn.unit};
    const auto swap_from = [&](year_month_day swap_start) {
        return tb::commodity_swap_terms{.underlying = underlying,
                                        .currency = currency,
                                        .quantity = quantity,
                                        .price = price,
                                        .start = swap_start,
                                        .maturity = end,
                                        .fixed_side = draw(name, 10) < 0.5 ? tb::side::pay :
                                                                             tb::side::receive,
                                        .unit = drawn.unit};
    };

    if (type == "CommodityForward")
        return built(tb::commodity_forward_builder({.underlying = underlying,
                                                    .currency = currency,
                                                    .quantity = quantity,
                                                    .price = price,
                                                    .maturity = end,
                                                    .position = position,
                                                    .unit = drawn.unit})
                         .build());
    if (type == "CommodityOption")
        return built(tb::commodity_option_builder(option).build());
    if (type == "CommodityAveragePriceOption")
        return built(
            tb::commodity_average_price_option_builder({.option = option, .averaging_start = start})
                .build());
    if (type == "CommodityOptionStrip")
        return built(tb::commodity_option_strip_builder({.underlying = underlying,
                                                         .currency = currency,
                                                         .quantity = quantity,
                                                         .kind = kind,
                                                         .strike = strike,
                                                         .start = start,
                                                         .maturity = end,
                                                         .roll_tenor = std::string("1M"),
                                                         .position = position,
                                                         .unit = drawn.unit})
                         .build());
    if (type == "CommoditySwap")
        return built(tb::commodity_swap_builder(swap_from(start)).build());
    if (type == "CommoditySwaption") {
        // The swaption expires six months in, and the swap it enters starts then.
        const auto expiry = add_months(start, 6);
        return built(tb::commodity_swaption_builder(
                         {.swap = swap_from(expiry), .expiry = expiry, .position = position})
                         .build());
    }
    if (type == "CommodityVarianceSwap")
        return built(tb::commodity_variance_swap_builder(
                         {.underlying = underlying,
                          .currency = currency,
                          .notional = quantity,
                          .variance_strike = drawn.variance_strike.value_or(0.04),
                          .start = start,
                          .maturity = end,
                          .unit = drawn.unit})
                         .build());
    if (type == "CommodityDigitalOption" || type == "CommodityDigitalAveragePriceOption") {
        // A digital pays the drawn quantity as cash.
        const tb::commodity_digital_option_terms digital{.underlying = underlying,
                                                         .currency = currency,
                                                         .payoff = quantity,
                                                         .kind = kind,
                                                         .strike = strike,
                                                         .expiry = end,
                                                         .position = position};
        if (type == "CommodityDigitalOption")
            return built(tb::commodity_digital_option_builder(digital).build());
        return built(tb::commodity_digital_average_price_option_builder(
                         {.option = digital, .averaging_start = start})
                         .build());
    }
    if (type == "CommodityPosition")
        return built(tb::commodity_position_builder({.underlying = underlying,
                                                     .currency = currency,
                                                     .quantity = quantity,
                                                     .unit = drawn.unit})
                         .build());
    if (type == "CommoditySpreadOption") {
        if (drawn.spread_commodity_code.empty())
            throw trade_population_error("A commodity spread option needs two commodities quoted "
                                         "in one currency, and the tenant holds fewer.");
        return built(tb::commodity_spread_option_builder(
                         {.long_underlying = underlying,
                          .short_underlying = commodity_address_of(drawn.spread_commodity_code),
                          .currency = currency,
                          .quantity = quantity,
                          .kind = kind,
                          .spread_strike = money(drawn.spread_amount.value_or(0.0)),
                          .start = start,
                          .maturity = end,
                          .position = position,
                          .unit = drawn.unit})
                         .build());
    }
    if (type == "IntradayPowerForward") {
        // One hour of the delivery day, delivered a week after the forward matures.
        const auto hour = static_cast<int>(draw(name, 11) * 23.0);
        return built(tb::intraday_power_forward_builder(
                         {.underlying = underlying,
                          .currency = currency,
                          .quantity = quantity,
                          .price = price,
                          .maturity = end,
                          .delivery_date = year_month_day{sys_days{end} + days{7}},
                          .delivery_start = hour * 3600,
                          .delivery_end = (hour + 1) * 3600,
                          .position = position,
                          .unit = drawn.unit})
                         .build());
    }
    if (type == "CommodityAsianOption")
        return built(
            tb::commodity_asian_option_builder({.option = option, .averaging_start = start})
                .build());
    if (type == "CommodityAccumulator")
        return built(tb::commodity_accumulator_builder(
                         {.underlying = underlying,
                          .currency = currency,
                          .fixing_amount = money(drawn.accumulation_amount.value_or(0.0)),
                          .strike = strike,
                          .knock_out = money(drawn.knock_out_barrier.value_or(0.0)),
                          .start = start,
                          .expiry = end,
                          .position = position,
                          .unit = drawn.unit})
                         .build());
    if (type == "CommodityTaRF") {
        // The gains stop at a quarter of one fixing's value.
        const auto fixing_amount = drawn.accumulation_amount.value_or(0.0);
        return built(tb::commodity_tarf_builder(
                         {.underlying = underlying,
                          .currency = currency,
                          .fixing_amount = money(fixing_amount),
                          .strike = strike,
                          .target_amount = money(fixing_amount * strike.to_double() * 0.25),
                          .start = start,
                          .expiry = end,
                          .position = position,
                          .unit = drawn.unit})
                         .build());
    }
    if (type == "CommodityGenericBarrierOption")
        return built(tb::commodity_generic_barrier_option_builder(
                         {.option = option,
                          .barrier = commodity_barrier_kind_of(drawn.barrier_type),
                          .barrier_level = money(drawn.lower_barrier.value_or(0.0)),
                          .monitoring_start = start})
                         .build());
    if (type == "CommodityWindowBarrierOption")
        // The barrier is watched from the start to halfway to the expiry.
        return built(tb::commodity_window_barrier_option_builder(
                         {.option = option,
                          .barrier = commodity_barrier_kind_of(drawn.barrier_type),
                          .barrier_level = money(drawn.lower_barrier.value_or(0.0)),
                          .window_start = start,
                          .window_end = year_month_day{sys_days{start} +
                                                       (sys_days{end} - sys_days{start}) / 2}})
                         .build());
    if (type == "CommodityStrikeResettableOption") {
        // A call resets lower when the price falls, a put higher when it rises.
        const bool call = kind == tb::option_kind::call;
        const auto reset = money(strike.to_double() * (call ? 0.9 : 1.1));
        return built(tb::commodity_strike_resettable_option_builder(
                         {.option = option,
                          .reset_strike = reset,
                          .trigger = call ? tb::trigger_direction::down : tb::trigger_direction::up,
                          .trigger_price = reset,
                          .observation_start = start,
                          .settlement = year_month_day{sys_days{end} + days{2}},
                          .premium = money(quantity.to_double() * strike.to_double() * 0.02),
                          .premium_date = year_month_day{sys_days{start} + days{2}}})
                         .build());
    }
    if (type == "CommodityForwardVolatilityAgreement") {
        // The volatility fixes halfway to the expiry, which the straddle runs to.
        const auto fva_date =
            year_month_day{sys_days{start} + (sys_days{end} - sys_days{start}) / 2};
        return built(
            tb::commodity_forward_volatility_agreement_builder(
                {.underlying = underlying,
                 .currency = currency,
                 .quantity = quantity,
                 .fva_date = fva_date,
                 .expiry = end,
                 .settlement = year_month_day{sys_days{end} + days{2}},
                 .premium_date = year_month_day{sys_days{fva_date} + days{2}},
                 .implied_vol_strike = std::round((0.2 + 0.2 * draw(name, 12)) * 100.0) / 100.0,
                 .position = position,
                 .unit = drawn.unit})
                .build());
    }
    // A basket holds each drawn commodity once, equally weighted.
    const auto distinct_basket = [&]() {
        std::vector<tb::commodity_basket_member> basket;
        for (const auto& member : drawn.basket)
            if (std::ranges::none_of(basket, [&](const auto& held) {
                    return held.underlying == commodity_address_of(member.code);
                }))
                basket.push_back({commodity_address_of(member.code), 0.0});
        if (basket.size() < 2)
            throw trade_population_error("A commodity basket names two distinct commodities, and "
                                         "the tenant holds fewer than two.");
        for (auto& member : basket)
            member.weight = 1.0 / static_cast<double>(basket.size());
        return basket;
    };
    if (type == "CommodityRainbowOption") {
        constexpr tb::rainbow_payoff payoffs[] = {tb::rainbow_payoff::best_of_asset_or_cash,
                                                  tb::rainbow_payoff::worst_of_asset_or_cash,
                                                  tb::rainbow_payoff::max_rainbow,
                                                  tb::rainbow_payoff::min_rainbow};
        return built(tb::commodity_rainbow_option_builder(
                         {.basket = distinct_basket(),
                          .currency = currency,
                          .notional = quantity,
                          .kind = kind,
                          .payoff = payoffs[static_cast<std::size_t>(draw(name, 13) * 4.0)],
                          .strike = strike,
                          .expiry = end,
                          .position = position})
                         .build());
    }
    if (type == "CommodityBasketVarianceSwap")
        return built(tb::commodity_basket_variance_swap_builder(
                         {.basket = distinct_basket(),
                          .currency = currency,
                          .notional = quantity,
                          .variance_strike = drawn.variance_strike.value_or(0.0),
                          .start = start,
                          .maturity = end,
                          .settlement = year_month_day{sys_days{end} + days{2}},
                          .position = position})
                         .build());
    if (type == "CommodityBestEntryOption")
        return built(tb::commodity_best_entry_option_builder(
                         {.underlying = underlying,
                          .currency = currency,
                          .notional = quantity,
                          .strike = strike,
                          .multiplier = money(1.0),
                          .cap = money(0.5),
                          .trigger_level = money(0.9),
                          .reset_minimum = money(0.8),
                          .premium = money(quantity.to_double() * strike.to_double() * 0.03),
                          .premium_date = year_month_day{sys_days{start} + days{2}},
                          .strike_date = start,
                          .expiry = end,
                          .settlement = year_month_day{sys_days{end} + days{2}},
                          .position = position,
                          .unit = drawn.unit})
                         .build());
    if (type == "CommodityWorstOfBasketSwap") {
        const auto funding = funding_index_in(scope, currency);
        if (!funding)
            throw trade_population_error("A commodity worst-of basket swap funds on an index in " +
                                         currency +
                                         ", and the tenant's swap conventions name none.");
        // Each member starts from the drawn price, stepped so no two start equal.
        std::vector<tb::commodity_worst_of_member> basket;
        for (const auto& member : distinct_basket())
            basket.push_back(
                {.underlying = member.underlying,
                 .initial_price =
                     money(price.to_double() * (1.0 + 0.1 * static_cast<double>(basket.size())))});
        std::vector<year_month_day> observations;
        for (int months = 3; add_months(start, months) < end; months += 3)
            observations.push_back(add_months(start, months));
        observations.push_back(end);
        const ores::marketdata::conventions::convention_lookup conventions(scope.conventions);
        return built(tb::commodity_worst_of_basket_swap_builder(
                         conventions,
                         {.basket = std::move(basket),
                          .observation_dates = std::move(observations),
                          .funding_index = funding->index,
                          .currency = currency,
                          .quantity = quantity,
                          .fixed_rate = money(0.03 + 0.04 * draw(name, 14)),
                          .start = start,
                          .maturity = end,
                          .position = position})
                         .build());
    }
    if (type == "CommodityBasketOption") {
        return built(tb::commodity_basket_option_builder({.basket = distinct_basket(),
                                                          .currency = currency,
                                                          .notional = quantity,
                                                          .kind = kind,
                                                          .strike = strike,
                                                          .expiry = end,
                                                          .position = position})
                         .build());
    }
    throw trade_population_error("The commodity trade type " + type +
                                 " states no builder this planner knows.");
}

struct commodity_type final {
    std::string_view code;
    commodity_shape shape;
};

/*
 * The commodity codes the population books. CommodityPairwiseVarianceSwap is
 * absent: ORE's trade factory has no builder for it, so the catalogue tags it
 * unsupported and the trade is refused while the portfolio is parsed. A type
 * ORE cannot carry leaves the generator rather than being booked into a run
 * that must fail.
 */
constexpr commodity_type commodity_types[] = {
    {"CommodityForward", commodity_shape::forward},
    {"IntradayPowerForward", commodity_shape::forward},
    {"CommodityForwardVolatilityAgreement", commodity_shape::forward},
    {"CommodityOption", commodity_shape::option},
    {"CommodityDigitalOption", commodity_shape::option},
    {"CommodityAsianOption", commodity_shape::asian_option},
    {"CommodityStrikeResettableOption", commodity_shape::option},
    {"CommodityPosition", commodity_shape::forward},
    {"CommodityAveragePriceOption", commodity_shape::average_price_option},
    {"CommodityDigitalAveragePriceOption", commodity_shape::average_price_option},
    {"CommodityOptionStrip", commodity_shape::option_strip},
    {"CommodityGenericBarrierOption", commodity_shape::barrier_option},
    {"CommodityWindowBarrierOption", commodity_shape::barrier_option},
    {"CommodityAccumulator", commodity_shape::accumulator},
    {"CommodityTaRF", commodity_shape::target_redemption},
    {"CommoditySpreadOption", commodity_shape::spread_option},
    {"CommodityVarianceSwap", commodity_shape::variance_swap},
    {"CommodityBasketVarianceSwap", commodity_shape::variance_swap},
    {"CommoditySwap", commodity_shape::swap},
    {"CommodityWorstOfBasketSwap", commodity_shape::swap},
    {"CommoditySwaption", commodity_shape::swaption},
    {"CommodityBasketOption", commodity_shape::basket_option},
    {"CommodityRainbowOption", commodity_shape::basket_option},
    {"CommodityBestEntryOption", commodity_shape::basket_option},
};

constexpr std::size_t commodity_type_count = sizeof(commodity_types) / sizeof(commodity_types[0]);

/*
 * A price in the band a commodity plausibly trades in, quantised to a
 * hundredth so the value is one a decimal column holds exactly.
 */
double price_from(const std::string& name, std::uint32_t stream, double floor, double span) {
    const auto value = floor + span * draw(name, stream);
    return std::round(value * 100.0) / 100.0;
}

}

std::string_view commodity_trade_planner::asset_class_code() const {
    return "commodity";
}

std::size_t commodity_trade_planner::code_count() const {
    return commodity_type_count;
}

domain::trade_population_plan
commodity_trade_planner::plan(const domain::trade_generation_scope& scope) const {
    validate_scope(scope);

    if (scope.commodities.empty())
        throw trade_population_error(
            "The tenant's commodity catalogue holds nothing, so no trade can name a commodity. "
            "Publish the refdata.commodities dataset first.");

    const auto min_years = tenor_years_of(scope.min_tenor_code);
    const auto max_years = tenor_years_of(scope.max_tenor_code);

    domain::trade_population_plan plan;
    plan.config_id = scope.config_id;
    plan.party_id = scope.party_id;
    plan.as_of_date = scope.as_of_date;

    const auto types =
        booked_types(commodity_types, scope, [](const commodity_type& type) { return type.code; });
    for (std::uint32_t slot = 0; slot < slot_count(scope); ++slot) {
        const auto name = slot_name(scope, slot);

        /*
         * The type cycles through the catalogue rather than being drawn, so a
         * population long enough to reach a type always states it. A draw
         * would leave the rare ones out of a small population and the
         * coverage claim could not be tested.
         */
        const auto& type = *types[slot % types.size()];
        const auto& commodity = scope.commodities[slot % scope.commodities.size()];

        const auto span = max_years - min_years + 1;
        const auto drawn =
            min_years + static_cast<std::uint32_t>(draw(name, 0) * static_cast<double>(span));
        const auto tenor_years = std::clamp(drawn, min_years, max_years);

        drawn_commodity terms;
        terms.commodity_code = commodity.code;
        terms.unit = commodity.unit;
        terms.quantity = price_from(name, 3, 1000.0, 100000.0);
        terms.start_date = add_months(scope.as_of_date, -static_cast<int>(slot % 6));
        terms.maturity_date = add_years(terms.start_date, static_cast<int>(tenor_years));

        const auto price = price_from(name, 4, 40.0, 120.0);
        const auto strike = std::round(price * (0.9 + 0.2 * draw(name, 5)) * 100.0) / 100.0;

        switch (type.shape) {
            case commodity_shape::forward:
                terms.fixed_price = price;
                break;
            case commodity_shape::option:
            case commodity_shape::basket_option:
                terms.option_type = (draw(name, 6) < 0.5) ? "Call" : "Put";
                terms.strike_price = strike;
                terms.exercise_type = "European";
                /*
                 * A basket product names its members; the family has a
                 * constituent table for them. Three over the catalogue, the
                 * way the equity basket draws its own.
                 */
                if (type.shape == commodity_shape::basket_option) {
                    for (std::uint32_t member = 0; member < 3; ++member) {
                        const auto& drawn_member =
                            scope.commodities[(slot + member) % scope.commodities.size()];
                        terms.basket.push_back(drawn_basket_member{drawn_member.code, 1.0 / 3.0});
                    }
                }
                break;
            case commodity_shape::asian_option:
                terms.option_type = (draw(name, 6) < 0.5) ? "Call" : "Put";
                terms.strike_price = strike;
                terms.exercise_type = "European";
                terms.average_type = "Arithmetic";
                terms.averaging_start_date = terms.start_date;
                terms.averaging_end_date = terms.maturity_date;
                break;
            case commodity_shape::average_price_option:
            case commodity_shape::option_strip:
                terms.option_type = (draw(name, 6) < 0.5) ? "Call" : "Put";
                terms.strike_price = strike;
                terms.average_type = "Arithmetic";
                terms.averaging_start_date = terms.start_date;
                terms.averaging_end_date = terms.maturity_date;
                if (type.shape == commodity_shape::option_strip)
                    terms.strip_frequency_code = "1M";
                break;
            case commodity_shape::barrier_option:
                terms.option_type = (draw(name, 6) < 0.5) ? "Call" : "Put";
                terms.strike_price = strike;
                terms.barrier_type = "DownAndOut";
                terms.lower_barrier = std::round(price * 0.8 * 100.0) / 100.0;
                break;
            case commodity_shape::accumulator:
            case commodity_shape::target_redemption:
                terms.option_type = "Call";
                terms.strike_price = strike;
                terms.accumulation_amount = std::round(terms.quantity / 12.0 * 100.0) / 100.0;
                terms.knock_out_barrier = std::round(strike * 1.2 * 100.0) / 100.0;
                break;
            case commodity_shape::spread_option:
                terms.option_type = (draw(name, 6) < 0.5) ? "Call" : "Put";
                terms.strike_price = strike;
                // ORE prices a spread on two commodities quoted in one currency.
                for (std::size_t step = 1; step < scope.commodities.size(); ++step) {
                    const auto& other = scope.commodities[(slot + step) % scope.commodities.size()];
                    if (other.code != commodity.code &&
                        other.quotation_currency == commodity.quotation_currency) {
                        terms.spread_commodity_code = other.code;
                        break;
                    }
                }
                terms.spread_amount = price_from(name, 7, 1.0, 20.0);
                break;
            case commodity_shape::variance_swap:
                terms.variance_strike = price_from(name, 8, 0.01, 0.2);
                if (type.code == "CommodityBasketVarianceSwap") {
                    for (std::uint32_t member = 0; member < 3; ++member) {
                        const auto& drawn_member =
                            scope.commodities[(slot + member) % scope.commodities.size()];
                        terms.basket.push_back(drawn_basket_member{drawn_member.code, 1.0 / 3.0});
                    }
                }
                break;
            case commodity_shape::swap:
                terms.fixed_price = price;
                if (type.code == "CommodityWorstOfBasketSwap") {
                    for (std::uint32_t member = 0; member < 3; ++member) {
                        const auto& drawn_member =
                            scope.commodities[(slot + member) % scope.commodities.size()];
                        terms.basket.push_back(drawn_basket_member{drawn_member.code, 1.0 / 3.0});
                    }
                }
                break;
            case commodity_shape::swaption:
                terms.option_type = "Call";
                terms.strike_price = strike;
                terms.swaption_expiry_date = add_months(terms.start_date, 6);
                break;
        }

        auto entry = common_slot(scope, slot, types.size());
        entry.trade_type_code = std::string(type.code);
        entry.notional =
            price_from(name, 1, scope.min_notional, scope.max_notional - scope.min_notional);
        /*
         * The commodity family states its rows here, where it draws them, and
         * the plan carries no bag for it.
         */
        // A commodity trade is in the currency its commodity is quoted in.
        const auto currency =
            commodity.quotation_currency.empty() ? entry.currency : commodity.quotation_currency;
        entry.instruments = built_commodity_trade(scope, entry, terms, currency);

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

boost::uuids::uuid trade_type_scope_id_for(const boost::uuids::uuid& config_id,
                                           const std::string& trade_type_code) {
    return named(std::format(
        "config_trade_type:{}:{}", boost::uuids::to_string(config_id), trade_type_code));
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
                            std::uint32_t tenor_years,
                            std::string_view product,
                            const boost::uuids::uuid& issuer_id) {
    /*
     * A security is named by its issuer, its currency, the anchor, its original
     * term and the product it is. The issuer is part of the name because a bond
     * is its issuer's: two legal entities of one tenant each hold their own
     * issuer catalogue, so one currency, anchor, term and product under two
     * issuers is two securities and cannot share an issue. The product is in the
     * name for the same reason: a callable bond states the dates its issuer may
     * call it and a plain bond of the same term states none. Nothing about a
     * book or a trade enters the name, so any book that keeps the currency may
     * hold the security and two trades of it name one issue.
     */
    const auto digest = named(std::format("bond:{}:{}:{}:{}Y:{}",
                                          boost::uuids::to_string(issuer_id),
                                          scope.currency_code,
                                          format_date(scope.as_of_date),
                                          tenor_years,
                                          product));
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

domain::issuer_reference issuer_for(const domain::trade_generation_scope& scope,
                                    std::uint32_t tenor_years) {
    if (scope.issuers.empty())
        throw trade_population_error("An issuer needs the tenant's counterparties.");
    auto issuers = scope.issuers;
    std::ranges::sort(issuers, {}, &domain::issuer_reference::id);
    return issuers[tenor_years % issuers.size()];
}

}

namespace ores::synthetic::service {

namespace {

/*
 * The trade types the routing table sends to rate_instrument, less the
 * inflation swap. That one is booked by the inflation family, because its fact
 * row states an inflation index and this generator has no such vocabulary. The
 * list is stated rather than read from the table, because a planner is pure and
 * has no database: a type the table routes later has to be added here.
 */
constexpr std::string_view rate_types[] = {"Swap",
                                           "CrossCurrencySwap",
                                           "ForwardRateAgreement",
                                           "CapFloor",
                                           "Swaption",
                                           "FlexiSwap",
                                           "BalanceGuaranteedSwap",
                                           "CallableSwap",
                                           "KnockOutSwap",
                                           "SwaptionStraddle",
                                           "RateDigitalOption"};
constexpr std::size_t rate_type_count = sizeof(rate_types) / sizeof(rate_types[0]);

}

namespace {

ores::trading::domain::instrument_batch
swap_batch_of(std::expected<ores::trading::domain::swap_instrument_data,
                            ores::trading::builders::build_error> built,
              const std::string& trade_type_code,
              std::string description) {
    if (!built)
        throw trade_population_error("The " + trade_type_code + " builder refuses the drawn " +
                                     built.error().term + ": " + built.error().message);
    built->header.description = std::move(description);
    built->header.identity.trade_type_code = trade_type_code;
    ores::trading::domain::instrument_batch batch;
    ores::trading::domain::append_instrument(batch,
                                             ores::trading::domain::trade_instrument{*built});
    return batch;
}

/*
 * The rate family builds each trade through its builder, from what the planner
 * draws. Each type wraps a vanilla swap on a domestic index; the type decides
 * what it adds and when its dates fall.
 */
ores::trading::domain::instrument_batch
build_rate_trade(const domain::trade_generation_scope& scope,
                 const ores::marketdata::conventions::convention_lookup& conventions,
                 const swap_choices& swaps,
                 const domain::trade_population_slot& entry,
                 std::uint32_t years) {
    namespace tb = ores::trading::builders;
    using ores::utility::decimal::decimal;
    const auto money = [](double value) {
        return decimal::from_double(value).value();
    };

    const auto& domestic = swaps.domestic;
    if (domestic.empty())
        throw trade_population_error("No swap convention of the tenant names an index in " +
                                     scope.currency_code +
                                     ", so no interest rate trade can state its legs.");

    const auto slot = entry.slot;
    const auto& type = entry.trade_type_code;
    const auto name = slot_name(scope, slot);
    const auto position =
        first_way(entry) ? tb::option_position::long_option : tb::option_position::short_option;
    /*
     * A rate is a decimal in the store, not a percentage: one per cent to six
     * per cent, quantised to a ten-thousandth so a decimal column holds it
     * exactly.
     */
    const auto fixed_rate = money(std::round((0.01 + 0.05 * draw(name, 2)) * 10000.0) / 10000.0);
    const auto terms_with =
        [&](const swap_choice& chosen, year_month_day start, year_month_day maturity) {
            return tb::vanilla_swap_terms{.notional = money(entry.notional),
                                          .currency = scope.currency_code,
                                          .start = start,
                                          .maturity = maturity,
                                          .fixed_rate = fixed_rate,
                                          .index = chosen.index,
                                          .fixed_side = first_way(entry, 1) ? tb::side::pay :
                                                                              tb::side::receive,
                                          .convention = chosen.convention};
        };
    /*
     * A forward rate agreement fixes on an Ibor index, and the tenant's swap
     * conventions also state overnight ones. So the agreement draws from the
     * swaps whose index is an Ibor, never from the whole pool.
     */
    std::vector<const swap_choice*> ibor_swaps;
    for (const auto& choice : domestic)
        if (choice.ibor)
            ibor_swaps.push_back(&choice);

    const auto start = add_months(scope.as_of_date, -static_cast<int>(slot % 6));
    const auto swap = terms_with(
        domestic[slot % domestic.size()], start, add_years(start, static_cast<int>(years)));
    const auto built = [&](auto result) {
        return swap_batch_of(std::move(result), type, "Generated interest rate trade.");
    };

    if (type == "Swap")
        return built(tb::vanilla_swap_builder(conventions, swap).build());
    if (type == "CrossCurrencySwap") {
        const auto& foreign = swaps.foreign;
        if (foreign.empty())
            throw trade_population_error("No swap convention of the tenant names an index "
                                         "outside " +
                                         scope.currency_code +
                                         ", so no cross currency swap can state its foreign leg.");
        const auto& chosen = foreign[slot % foreign.size()];
        return built(
            tb::cross_currency_swap_builder(conventions,
                                            {.domestic = swap,
                                             .foreign_notional = money(entry.notional),
                                             .foreign_currency = chosen.index.index()->subject(),
                                             .foreign_index = chosen.index,
                                             .foreign_convention = chosen.convention})
                .build());
    }
    if (type == "ForwardRateAgreement") {
        /*
         * A forward rate agreement is a short contract that settles in the
         * future: it starts in three months and runs for six.
         */
        if (ibor_swaps.empty())
            throw trade_population_error(
                "No swap convention of the tenant names an Ibor index in " + scope.currency_code +
                ", so no forward rate agreement can state its index.");
        const auto from = add_months(scope.as_of_date, 3);
        const auto& chosen = *ibor_swaps[slot % ibor_swaps.size()];
        return built(
            tb::forward_rate_agreement_builder(
                conventions,
                {.period = terms_with(chosen, from, add_months(from, 6)), .position = position})
                .build());
    }
    if (type == "CapFloor")
        return built(tb::cap_floor_builder(conventions,
                                           {.swap = swap,
                                            .kind = first_way(entry, 2) ? tb::cap_floor_kind::cap :
                                                                          tb::cap_floor_kind::floor,
                                            .position = position})
                         .build());
    if (type == "Swaption" || type == "SwaptionStraddle") {
        /*
         * An option is exercised after the population is written, and the
         * swap it opens starts two days after its expiry.
         */
        const auto expiry = add_years(scope.as_of_date, 1);
        const auto swap_start = year_month_day{sys_days{expiry} + days{2}};
        const tb::swaption_terms terms{
            .swap = terms_with(domestic[slot % domestic.size()],
                               swap_start,
                               add_years(swap_start, static_cast<int>(years))),
            .expiry = expiry,
            .position = position,
            .settles = tb::settlement::cash};
        if (type == "Swaption")
            return built(tb::swaption_builder(conventions, terms).build());
        return built(tb::swaption_straddle_builder(conventions, terms).build());
    }
    if (type == "RateDigitalOption") {
        // The digital fixes on the domestic index a year out and pays two days later.
        const auto* index = domestic[slot % domestic.size()].index.index();
        if (index == nullptr)
            throw trade_population_error("A RateDigitalOption fixes on an index, and the drawn "
                                         "swap convention names a datum.");
        const auto fixing = add_years(scope.as_of_date, 1);
        auto digital =
            tb::rate_digital_option_builder(
                {.index = index->subject(),
                 .strike = fixed_rate,
                 .payoff_amount = money(std::max(1.0, std::round(entry.notional / 100.0))),
                 .payoff_currency = scope.currency_code,
                 .kind = first_way(entry, 2) ? tb::option_kind::call : tb::option_kind::put,
                 .position = position,
                 .fixing_date = fixing,
                 .payment_date = year_month_day{sys_days{fixing} + days{2}}})
                .build();
        if (!digital)
            throw trade_population_error("The RateDigitalOption builder refuses the drawn " +
                                         digital.error().term + ": " + digital.error().message);
        ores::trading::domain::instrument_batch batch;
        ores::trading::domain::append_instrument(
            batch, ores::trading::domain::trade_instrument{std::move(*digital)});
        return batch;
    }
    if (type == "CallableSwap")
        return built(tb::callable_swap_builder(
                         conventions,
                         {.swap = swap, .first_call = add_months(start, 6), .position = position})
                         .build());
    if (type == "KnockOutSwap")
        return built(tb::knock_out_swap_builder(conventions,
                                                {.swap = swap,
                                                 .barrier_type = std::string("UpAndOut"),
                                                 .barrier_level = money(0.08),
                                                 .barrier_start = start})
                         .build());
    if (type == "FlexiSwap")
        return built(
            tb::flexi_swap_builder(conventions, {.swap = swap, .position = position}).build());
    if (type == "BalanceGuaranteedSwap")
        return built(
            tb::balance_guaranteed_swap_builder(
                conventions, {.swap = swap, .reference_security = entry.internal_identifier})
                .build());
    throw trade_population_error("The rate trade type " + type +
                                 " states no builder this planner knows.");
}

}

std::string_view rate_trade_planner::asset_class_code() const {
    return "interest_rates";
}

std::size_t rate_trade_planner::code_count() const {
    return rate_type_count;
}

domain::trade_population_plan
rate_trade_planner::plan(const domain::trade_generation_scope& scope) const {
    validate_scope(scope);

    const auto min_years = tenor_years_of(scope.min_tenor_code);
    const auto max_years = tenor_years_of(scope.max_tenor_code);

    domain::trade_population_plan plan;
    plan.config_id = scope.config_id;
    plan.party_id = scope.party_id;
    plan.as_of_date = scope.as_of_date;

    const ores::marketdata::conventions::convention_lookup conventions(scope.conventions);
    const auto swaps = swap_choices_of(scope);
    const auto types = booked_types(rate_types, scope);
    for (std::uint32_t slot = 0; slot < slot_count(scope); ++slot) {
        const auto name = slot_name(scope, slot);

        /*
         * The type cycles rather than being drawn, so a population long enough
         * to reach a type always states it and the coverage claim is testable.
         */
        const auto& type = *types[slot % types.size()];

        const auto span = max_years - min_years + 1;
        const auto drawn =
            min_years + static_cast<std::uint32_t>(draw(name, 0) * static_cast<double>(span));

        auto entry = common_slot(scope, slot, types.size());
        entry.trade_type_code = std::string(type);
        entry.notional =
            price_from(name, 1, scope.min_notional, scope.max_notional - scope.min_notional);
        entry.instruments = build_rate_trade(
            scope, conventions, swaps, entry, std::clamp(drawn, min_years, max_years));

        plan.slots.push_back(std::move(entry));
    }

    return plan;
}

}

namespace ores::synthetic::service {

namespace {

/*
 * The sixteen trade types the routing table sends to an FX table, in the order
 * the router states them. The list is stated rather than read from the table,
 * because a planner is pure and has no database.
 */
constexpr std::string_view fx_types[] = {"FxForward",
                                         "FxSwap",
                                         "FxOption",
                                         "FxDigitalOption",
                                         "FxAverageForward",
                                         "FxBarrierOption",
                                         "FxDoubleBarrierOption",
                                         "FxEuropeanBarrierOption",
                                         "FxGenericBarrierOption",
                                         "FxKIKOBarrierOption",
                                         "FxTouchOption",
                                         "FxDoubleTouchOption",
                                         "FxDigitalBarrierOption",
                                         "FxVarianceSwap",
                                         "FxAccumulator",
                                         "FxTaRF",
                                         "FxBasketOption",
                                         "FxRainbowOption",
                                         "FxBestEntryOption",
                                         "FxStrikeResettableOption",
                                         "FxWindowBarrierOption",
                                         "FxBasketVarianceSwap",
                                         "FxPairwiseVarianceSwap",
                                         "FxForwardVolatilityAgreement",
                                         "FxWorstOfBasketSwap",
                                         "FxAsianOption"};
constexpr std::size_t fx_type_count = sizeof(fx_types) / sizeof(fx_types[0]);

/// The products on two pairs in one quote currency, which they pay in.
bool needs_second_pair(std::string_view type) {
    return type == "FxBasketOption" || type == "FxRainbowOption" ||
           type == "FxBasketVarianceSwap" || type == "FxPairwiseVarianceSwap" ||
           type == "FxWorstOfBasketSwap";
}

/// Whether a pair can stand beside another in a product on two pairs: quoted in the same
/// currency, which the product pays in, on a different base. The pair choice and the
/// builder's second pair both read this, so a slot is never given a pair the builder refuses.
bool is_basket_partner(const domain::currency_pair_reference& pair,
                       const domain::currency_pair_reference& candidate) {
    return candidate.quote == pair.quote && candidate.base != pair.base;
}

/// Whether the tenant holds a second pair for a product on two pairs.
bool has_basket_partner(const domain::trade_generation_scope& scope,
                        const domain::currency_pair_reference& pair) {
    return std::ranges::any_of(scope.currency_pairs, [&](const auto& candidate) {
        return is_basket_partner(pair, candidate);
    });
}

/*
 * The pair a slot trades. A desk books what it quotes, so the pairs that name
 * the book's own currency come first; a tenant holding none of those trades the
 * whole catalogue rather than refusing, because a pair in another currency is
 * still a pair it holds. A slot of a product on two pairs draws only from the
 * pairs that have a partner in their quote currency, so a book whose own
 * currency is quoted in no second pair still books those products.
 */
const domain::currency_pair_reference&
pair_for(const domain::trade_generation_scope& scope, std::uint32_t slot, std::string_view type) {
    const bool basket = needs_second_pair(type);
    const auto fits = [&](const domain::currency_pair_reference& pair) {
        return !basket || has_basket_partner(scope, pair);
    };
    const auto& own = scope.currency_code;
    std::vector<const domain::currency_pair_reference*> matching;
    std::vector<const domain::currency_pair_reference*> fitting;
    for (const auto& pair : scope.currency_pairs) {
        if (!fits(pair))
            continue;
        fitting.push_back(&pair);
        if (pair.base == own || pair.quote == own)
            matching.push_back(&pair);
    }
    if (!matching.empty())
        return *matching[slot % matching.size()];
    if (!fitting.empty())
        return *fitting[slot % fitting.size()];
    return scope.currency_pairs[slot % scope.currency_pairs.size()];
}

/*
 * The fixing source ORE's examples state for every pair. The source belongs to
 * the tenant's market data configuration, which the market data task gives a
 * home; until then every generated trade observes this one.
 */
constexpr std::string_view fx_fixing_source = "TR20H";

/// The FX index of a pair, built as an oresmd index: the projection writes it as ORE names it.
ores::marketdata::datum::market_reference fx_index_of(const domain::currency_pair_reference& pair) {
    namespace md = ores::marketdata::datum;
    auto index =
        md::market_index::make(md::index_family::fx,
                               pair.base,
                               {{"source", std::string(fx_fixing_source)}, {"ccy", pair.quote}});
    if (!index)
        throw trade_population_error("The pair " + pair.base + pair.quote +
                                     " has no FX index: " + index.error());
    auto reference = md::market_reference::of(std::move(*index));
    if (!reference)
        throw trade_population_error("The pair " + pair.base + pair.quote +
                                     " has no FX index: " + reference.error());
    return std::move(*reference);
}

/// The rows of a product ORE reads through a class of its own, as a batch.
ores::trading::domain::instrument_batch
scripted_batch_of(ores::trading::builders::scripted_result built,
                  const std::string& trade_type_code) {
    if (!built)
        throw trade_population_error("The " + trade_type_code + " builder refuses the drawn " +
                                     built.error().term + ": " + built.error().message);
    ores::trading::domain::instrument_batch batch;
    ores::trading::domain::append_instrument(
        batch, ores::trading::domain::trade_instrument{std::move(*built)});
    return batch;
}

ores::trading::domain::instrument_batch
fx_batch_of(std::expected<ores::trading::domain::fx_instrument_data,
                          ores::trading::builders::build_error> built,
            const std::string& trade_type_code) {
    if (!built)
        throw trade_population_error("The " + trade_type_code + " builder refuses the drawn " +
                                     built.error().term + ": " + built.error().message);
    const auto describe = [](auto& rows) {
        for (auto& row : rows)
            row.description = "Generated FX trade.";
    };
    describe(built->forwards);
    describe(built->vanilla_options);
    describe(built->barrier_options);
    describe(built->digital_options);
    describe(built->asian_forwards);
    describe(built->accumulators);
    describe(built->variance_swaps);
    ores::trading::domain::instrument_batch batch;
    ores::trading::domain::append_instrument(batch,
                                             ores::trading::domain::trade_instrument{*built});
    return batch;
}

}

std::string_view fx_trade_planner::asset_class_code() const {
    return "fx";
}

std::size_t fx_trade_planner::code_count() const {
    return fx_type_count;
}

/*
 * The fx family states its own rows. The mapping from what the planner draws to
 * each product's constructor lives here, beside the drawing, and not in the
 * writer: the plan carries the rows and the writer persists them.
 */
namespace {

/*
 * The FX family builds each trade through its builder. The drawn rate is the
 * pair's level the trade is struck at; barriers sit a tenth either side of it,
 * and a touch or a single barrier is the one above it.
 */
ores::trading::domain::instrument_batch build_fx_trade(const domain::trade_generation_scope& scope,
                                                       const domain::trade_population_slot& entry,
                                                       const domain::currency_pair_reference& pair,
                                                       year_month_day start,
                                                       year_month_day end,
                                                       double rate) {
    namespace tb = ores::trading::builders;
    using ores::utility::decimal::decimal;
    const auto money = [](double value) {
        return decimal::from_double(std::round(value * 100.0) / 100.0).value();
    };
    const auto level = [](double value) {
        return decimal::from_double(std::round(value * 10000.0) / 10000.0).value();
    };

    const auto slot = entry.slot;
    const auto name = slot_name(scope, slot);
    const auto& type = entry.trade_type_code;
    const auto index = fx_index_of(pair);
    const auto notional = entry.notional;
    const auto position =
        first_way(entry) ? tb::option_position::long_option : tb::option_position::short_option;
    const auto kind = draw(name, 3) < 0.5 ? tb::option_kind::call : tb::option_kind::put;
    const tb::fx_exchange exchange{.bought_currency = pair.base,
                                   .bought_amount = money(notional),
                                   .sold_currency = pair.quote,
                                   .sold_amount = money(notional * rate)};
    const tb::fx_option_terms option{
        .exchange = exchange, .kind = kind, .expiry = end, .position = position};
    const auto lower = level(rate * 0.9);
    const auto upper = level(rate * 1.1);
    const auto single = std::vector<tb::fx_barrier>{{"UpAndOut", {upper}}};
    const auto both = std::vector<tb::fx_barrier>{{"KnockOut", {lower, upper}}};
    const auto built = [&](auto result) {
        return fx_batch_of(std::move(result), type);
    };
    if (type == "FxWindowBarrierOption") {
        // The barrier is watched from the start to halfway to the expiry, a tenth away.
        const bool call = kind == tb::option_kind::call;
        return built(
            tb::fx_window_barrier_option_builder(
                {.underlying = index,
                 .currency = pair.quote,
                 .fixing_amount = money(notional),
                 .strike = level(rate),
                 .kind = kind,
                 .position = position,
                 .expiry = end,
                 .window_start = start,
                 .window_end =
                     year_month_day{sys_days{start} + (sys_days{end} - sys_days{start}) / 2},
                 .barrier = tb::scripted_barrier{.type = call ? "DownAndOut" : "UpAndOut",
                                                 .level = level(rate * (call ? 0.9 : 1.1))}})
                .build());
    }
    if (type == "FxBestEntryOption")
        return built(tb::fx_best_entry_option_builder(
                         {.underlying = index,
                          .currency = pair.quote,
                          .notional = money(notional),
                          .multiplier = level(1.0),
                          .strike = level(1.0),
                          .cap = level(0.2),
                          .trigger_level = level(0.98),
                          .reset_minimum = level(0.95),
                          .premium = money(0.0),
                          .premium_date = start,
                          .strike_date = start,
                          .strike_observation_dates = quarterly_dates(start, end),
                          .expiry = end,
                          .settlement = end,
                          .position = position})
                         .build());
    if (type == "FxStrikeResettableOption") {
        /*
         * A call resets lower when the rate falls, a put higher when it rises. A
         * rate moves less than an equity price, so the trigger and the reset sit
         * closer to the strike than the equity case's.
         */
        const bool call = kind == tb::option_kind::call;
        const auto reset = level(rate * (call ? 0.98 : 1.02));
        return built(tb::fx_strike_resettable_option_builder(
                         {.underlying = index,
                          .currency = pair.quote,
                          .kind = kind,
                          .position = position,
                          .quantity = money(notional),
                          .strike = level(rate),
                          .reset_strike = reset,
                          .trigger = call ? tb::trigger_direction::down : tb::trigger_direction::up,
                          .trigger_price = level(rate * (call ? 0.93 : 1.07)),
                          .observation_dates = quarterly_dates(start, end),
                          .expiry = end,
                          .settlement = end,
                          .premium = money(0.0),
                          .premium_date = start})
                         .build());
    }
    /*
     * A product on two pairs holds the slot's pair and another the tenant
     * holds in the same quote currency, which the product pays in.
     */
    const auto second_pair = [&]() -> const domain::currency_pair_reference& {
        // A product that asks for a second pair must be listed, or the pair choice gives its
        // slot a pair that may have none.
        if (!needs_second_pair(type))
            throw std::logic_error("The " + type +
                                   " asks for a second pair but needs_second_pair "
                                   "does not list it.");
        for (const auto& candidate : scope.currency_pairs)
            if (is_basket_partner(pair, candidate))
                return candidate;
        throw trade_population_error("A " + type +
                                     " names two pairs in one quote currency, and the tenant "
                                     "holds no second pair quoted in " +
                                     pair.quote + ".");
    };
    if (type == "FxBasketVarianceSwap" || type == "FxPairwiseVarianceSwap") {
        const auto other = fx_index_of(second_pair());
        // A volatility strike of a fifth, observed daily over the trade's life.
        const auto strike = level(0.2);
        if (type == "FxBasketVarianceSwap")
            return built(
                tb::fx_basket_variance_swap_builder(
                    {.members =
                         std::vector<tb::basket_option_member>{
                             {.underlying = index, .currency = std::nullopt, .weight = level(0.5)},
                             {.underlying = other, .currency = std::nullopt, .weight = level(0.5)}},
                     .currency = pair.quote,
                     .notional = money(notional / 100.0),
                     .strike = strike,
                     .squared_payoff = false,
                     .start = start,
                     .end = end,
                     .settlement = year_month_day{sys_days{end} + days{5}},
                     .position = position})
                    .build());
        return built(tb::fx_pairwise_variance_swap_builder(
                         {.first = tb::pairwise_member{.underlying = index,
                                                       .strike = strike,
                                                       .notional = money(notional / 200.0)},
                          .second = tb::pairwise_member{.underlying = other,
                                                        .strike = strike,
                                                        .notional = money(notional / 200.0)},
                          .basket_notional = money(notional / 100.0),
                          .basket_strike = strike,
                          .pay_currency = pair.quote,
                          .start = start,
                          .end = end,
                          .settlement = year_month_day{sys_days{end} + days{5}},
                          .position = position})
                         .build());
    }
    if (type == "FxAsianOption")
        // The rate is averaged daily over the trade's life and settles two days after.
        return built(
            tb::fx_asian_option_builder({.fx_index = index,
                                         .currency = pair.quote,
                                         .quantity = money(notional),
                                         .strike = level(rate),
                                         .kind = kind,
                                         .position = position,
                                         .observation_start = start,
                                         .expiry = end,
                                         .settlement = year_month_day{sys_days{end} + days{2}}})
                .build());
    if (type == "FxWorstOfBasketSwap") {
        /*
         * Half-yearly periods over the trade's life, each paying two days after
         * it ends. The coupons depend on each pair's performance, a ratio, so the
         * swap pays in its floating index's currency: the quote currency's index
         * when the tenant holds one, else the first index it holds.
         */
        if (scope.floating_index_codes.empty())
            throw trade_population_error(
                "An FxWorstOfBasketSwap floats on an index, and the tenant holds none.");
        std::string floating_index = scope.floating_index_codes.front();
        for (const auto& code : scope.floating_index_codes)
            if (code.starts_with(pair.quote + "-")) {
                floating_index = code;
                break;
            }
        const auto pay_currency = floating_index.substr(0, floating_index.find('-'));
        std::vector<year_month_day> periods;
        for (int months = 0; add_months(start, months) <= end; months += 6)
            periods.push_back(add_months(start, months));
        std::vector<year_month_day> pays;
        for (std::size_t i = 1; i < periods.size(); ++i)
            pays.emplace_back(sys_days{periods[i]} + days{2});
        const auto count = pays.size();
        return built(
            tb::fx_worst_of_basket_swap_builder(
                {.members =
                     std::vector<tb::worst_of_member>{
                         {.underlying = index, .initial_price = level(rate)},
                         {.underlying = fx_index_of(second_pair()), .initial_price = level(rate)}},
                 .currency = pay_currency,
                 .quantity = money(notional),
                 .initial_fixed_rate = level(0.02),
                 .period_dates = periods,
                 .pay_dates = pays,
                 .floating_index = floating_index,
                 .floating_spread = level(0.0),
                 .floating_day_count = std::string("A360"),
                 .knock_out_levels = std::vector<decimal>(count, level(1.0)),
                 .fixed_trigger_levels = std::vector<decimal>(count, level(0.75)),
                 .fixed_rate = level(0.05),
                 .knock_in_level = level(0.75),
                 .strike = level(1.0),
                 .position = position})
                .build());
    }
    if (type == "FxForwardVolatilityAgreement") {
        // The volatility fixes halfway to the expiry, which the straddle runs to.
        const auto fva_date =
            year_month_day{sys_days{start} + (sys_days{end} - sys_days{start}) / 2};
        return built(tb::fx_forward_volatility_agreement_builder(
                         {.underlying = index,
                          .pay_currency = pair.quote,
                          .quantity = money(notional),
                          .implied_vol_strike = level(0.1),
                          .fva_date = fva_date,
                          .option_expiry = end,
                          .premium_date = year_month_day{sys_days{fva_date} + days{2}},
                          .settlement = year_month_day{sys_days{end} + days{5}},
                          .position = position})
                         .build());
    }
    if (type == "FxBasketOption" || type == "FxRainbowOption") {
        const auto* other = &second_pair();
        // Two unit weights put the basket near twice one member, so the strike sits there.
        const tb::basket_option_terms basket{
            .currency = pair.quote,
            .notional = money(notional),
            .strike = level(rate * 2.0),
            .members =
                std::vector<tb::basket_option_member>{
                    {.underlying = index, .currency = std::nullopt, .weight = level(1.0)},
                    {.underlying = fx_index_of(*other),
                     .currency = std::nullopt,
                     .weight = level(1.0)}},
            .payoff_type = std::string(type == "FxBasketOption" ? "Vanilla" : "MaxRainbow"),
            .kind = kind,
            .position = position,
            .expiry = end,
            .settlement = end};
        if (type == "FxBasketOption")
            return built(tb::basket_option_builder<tb::basket_product::fx_basket>(basket).build());
        return built(tb::basket_option_builder<tb::basket_product::fx_rainbow>(basket).build());
    }
    const auto barrier = [&]<tb::fx_barrier_product Product>(std::vector<tb::fx_barrier> barriers) {
        return built(tb::fx_barrier_option_builder<Product>(
                         {.option = option, .barriers = std::move(barriers), .fx_index = index})
                         .build());
    };
    const auto digital = [&]<tb::fx_digital_product Product>(std::optional<decimal> strike,
                                                             std::vector<tb::fx_barrier> barriers) {
        return built(tb::fx_digital_option_builder<Product>({.foreign_currency = pair.base,
                                                             .domestic_currency = pair.quote,
                                                             .payoff_amount = money(notional),
                                                             .kind = kind,
                                                             .expiry = end,
                                                             .position = position,
                                                             .strike = strike,
                                                             .barriers = std::move(barriers),
                                                             .fx_index = index})
                         .build());
    };

    if (type == "FxForward")
        return built(tb::fx_forward_builder(
                         {.exchange = exchange, .value_date = end, .settles = tb::settlement::cash})
                         .build());
    if (type == "FxSwap")
        return built(tb::fx_swap_builder({.near_leg = exchange,
                                          .near_date = start,
                                          .far_date = end,
                                          .far_bought_amount = money(notional * rate),
                                          .far_sold_amount = money(notional)})
                         .build());
    if (type == "FxOption")
        return built(tb::fx_option_builder(option).build());
    if (type == "FxBarrierOption")
        return barrier.template operator()<tb::fx_barrier_product::barrier>(single);
    if (type == "FxDoubleBarrierOption")
        return barrier.template operator()<tb::fx_barrier_product::double_barrier>(both);
    if (type == "FxEuropeanBarrierOption")
        return barrier.template operator()<tb::fx_barrier_product::european_barrier>(single);
    if (type == "FxKIKOBarrierOption")
        return barrier.template operator()<tb::fx_barrier_product::kiko>(
            {{"DownAndIn", {lower}}, {"UpAndOut", {upper}}});
    if (type == "FxGenericBarrierOption")
        return built(tb::fx_generic_barrier_option_builder({.option = option,
                                                            .barrier = single.front(),
                                                            .fx_index = index,
                                                            .monitoring_start = start})
                         .build());
    if (type == "FxDigitalOption")
        return digital.template operator()<tb::fx_digital_product::digital>(level(rate), {});
    if (type == "FxTouchOption")
        return digital.template operator()<tb::fx_digital_product::touch>(std::nullopt,
                                                                          {{"UpAndIn", {upper}}});
    if (type == "FxDoubleTouchOption")
        return digital.template operator()<tb::fx_digital_product::double_touch>(
            std::nullopt, {{"KnockIn", {lower, upper}}});
    if (type == "FxDigitalBarrierOption")
        return digital.template operator()<tb::fx_digital_product::digital_barrier>(level(rate),
                                                                                    single);
    if (type == "FxAverageForward")
        return built(tb::fx_average_forward_builder(
                         {.fx_index = index,
                          .reference_currency = pair.base,
                          .reference_notional = money(notional),
                          .settlement_currency = pair.quote,
                          .settlement_notional = money(notional * rate),
                          .start = start,
                          .payment_date = end,
                          .fixed_payer = position == tb::option_position::short_option})
                         .build());
    if (type == "FxTaRF")
        return built(tb::fx_tarf_builder({.fx_index = index,
                                          .currency = pair.quote,
                                          .fixing_amount = money(notional),
                                          .strike = level(rate),
                                          .target_amount = money(notional * 0.1),
                                          .start = start,
                                          .end = end,
                                          .position = position})
                         .build());
    if (type == "FxVarianceSwap")
        return built(tb::fx_variance_swap_builder({.fx_index = index,
                                                   .currency = pair.quote,
                                                   .start = start,
                                                   .end = end,
                                                   .strike = 0.1,
                                                   .notional = money(notional),
                                                   .position = position})
                         .build());
    if (type == "FxAccumulator")
        return built(tb::fx_accumulator_builder({.fx_index = index,
                                                 .currency = pair.quote,
                                                 .fixing_amount = money(notional),
                                                 .strike = level(rate),
                                                 .knock_out = upper,
                                                 .start = start,
                                                 .end = end,
                                                 .position = position})
                         .build());
    throw trade_population_error("The FX trade type " + type +
                                 " states no builder this planner knows.");
}

}

domain::trade_population_plan
fx_trade_planner::plan(const domain::trade_generation_scope& scope) const {
    validate_scope(scope);

    if (scope.currency_pairs.empty())
        throw trade_population_error(
            "The tenant holds no currency pair, so no FX trade can name one. Publish the "
            "currency pair catalogue first.");

    const auto min_years = tenor_years_of(scope.min_tenor_code);
    const auto max_years = tenor_years_of(scope.max_tenor_code);

    domain::trade_population_plan plan;
    plan.config_id = scope.config_id;
    plan.party_id = scope.party_id;
    plan.as_of_date = scope.as_of_date;

    const auto types = booked_types(fx_types, scope);
    for (std::uint32_t slot = 0; slot < slot_count(scope); ++slot) {
        const auto name = slot_name(scope, slot);
        const auto& type = *types[slot % types.size()];
        const auto& pair = pair_for(scope, slot, type);

        const auto span = max_years - min_years + 1;
        const auto drawn =
            min_years + static_cast<std::uint32_t>(draw(name, 0) * static_cast<double>(span));
        const auto tenor_years = std::clamp(drawn, min_years, max_years);
        const auto start = add_months(scope.as_of_date, -static_cast<int>(slot % 6));
        const auto end = add_years(start, static_cast<int>(tenor_years));
        const auto notional =
            price_from(name, 1, scope.min_notional, scope.max_notional - scope.min_notional);
        const auto rate = std::round((0.5 + draw(name, 2)) * 10000.0) / 10000.0;

        auto entry = common_slot(scope, slot, types.size());
        entry.trade_type_code = std::string(type);
        entry.notional = notional;
        entry.instruments = build_fx_trade(scope, entry, pair, start, end, rate);
        plan.slots.push_back(std::move(entry));
    }

    return plan;
}


namespace {

constexpr std::string_view equity_types[] = {"EquityAccumulator",
                                             "EquityAsianOption",
                                             "EquityBarrierOption",
                                             "EquityCliquetOption",
                                             "EquityDigitalOption",
                                             "EquityDoubleBarrierOption",
                                             "EquityEuropeanBarrierOption",
                                             "EquityForward",
                                             "EquityOption",
                                             "EquityOptionPosition",
                                             "EquityPosition",
                                             "EquitySwap",
                                             "EquityTaRF",
                                             "EquityTouchOption",
                                             "EquityVarianceSwap",
                                             "EquityWorstOfBasketSwap",
                                             "CompositeTrade",
                                             "ContractForDifference",
                                             "TotalReturnSwap",
                                             "EquityOutperformanceOption",
                                             "EquityBasketOption",
                                             "EquityRainbowOption",
                                             "EquityBestEntryOption",
                                             "EquityStrikeResettableOption",
                                             "EquityWindowBarrierOption",
                                             "EquityGenericBarrierOption",
                                             "EquityBasketVarianceSwap",
                                             "EquityPairwiseVarianceSwap",
                                             "EquityForwardVolatilityAgreement",
                                             "EquityFutureOption",
                                             "EquityDoubleTouchOption",
                                             "CashPosition",
                                             "EquityAutoDeltaHedgedOption"};

constexpr std::size_t equity_type_count = std::size(equity_types);

}

std::string_view equity_trade_planner::asset_class_code() const {
    return "equity";
}

std::size_t equity_trade_planner::code_count() const {
    return equity_type_count;
}


/*
 * The credit codes a population draws. A synthetic CDO needs a qualifier the
 * family has nowhere to state, so it stays out; the five here each state a
 * reference entity, the dates and nothing else.
 */
constexpr std::string_view credit_types[] = {"CreditDefaultSwap",
                                             "IndexCreditDefaultSwap",
                                             "IndexCreditDefaultSwapOption",
                                             "CreditLinkedSwap",
                                             "RiskParticipationAgreement",
                                             "SyntheticCDO",
                                             "CreditDefaultSwapOption",
                                             "CBO"};

constexpr std::size_t credit_type_count = std::size(credit_types);

std::string_view credit_trade_planner::asset_class_code() const {
    return "credit";
}

std::size_t credit_trade_planner::code_count() const {
    return credit_type_count;
}

namespace {

/// The CDS convention ORE's examples and the seeded conventions give standard contracts.
constexpr std::string_view cds_convention_id = "CDS-STANDARD-CONVENTIONS";

/*
 * A generated credit trade names its curve by its reference entity: the senior
 * unsecured credit spread series of the LEI, in the trade currency. The market
 * data task gives each entity its curve; until then ORE fails the trade only
 * because the curve is missing.
 */
ores::marketdata::datum::market_reference credit_curve_of(const std::string& lei,
                                                          const std::string& currency) {
    namespace md = ores::marketdata::datum;
    const auto key = std::format("CDS/CREDIT_SPREAD/{}/SNRFOR/{}/5Y", lei, currency);
    const auto quote = md::ore_key_codec::read(key);
    if (!quote)
        throw trade_population_error("The credit curve of " + lei +
                                     " has no address: " + quote.error());
    auto series = md::market_reference::of(md::series_of(*quote));
    if (!series)
        throw trade_population_error("The credit curve of " + lei +
                                     " has no address: " + series.error());
    return std::move(*series);
}

/*
 * The index an RPA's underlying swap floats on, with its convention: the first
 * swap convention whose index is in the trade currency, so the underlying is
 * one the tenant's conventions can build. Several conventions can name one
 * index, so the underlying is built by the convention found here.
 */
swap_choice underlying_index_of(const domain::trade_generation_scope& scope) {
    namespace md = ores::marketdata::datum;
    for (const auto& swap : scope.conventions.swaps) {
        if (!swap.oresmd_uri)
            continue;
        auto index = md::market_reference::parse(*swap.oresmd_uri);
        if (index && index->index() != nullptr && index->index()->subject() == scope.currency_code)
            return swap_choice{std::move(*index), swap.id};
    }
    throw trade_population_error("No swap convention of the tenant floats on an index in " +
                                 scope.currency_code +
                                 ", so no floating leg in it can be stated: a risk "
                                 "participation agreement's swap and a total return swap's "
                                 "funding need one.");
}

ores::trading::domain::instrument_batch
batch_of(std::expected<ores::trading::domain::credit_instrument_data,
                       ores::trading::builders::build_error> built,
         const std::string& trade_type_code) {
    if (!built)
        throw trade_population_error("The " + trade_type_code + " builder refuses the drawn " +
                                     built.error().term + ": " + built.error().message);
    built->instrument.description = "Generated credit trade.";
    ores::trading::domain::instrument_batch batch;
    ores::trading::domain::append_instrument(batch,
                                             ores::trading::domain::trade_instrument{*built});
    return batch;
}

/*
 * The credit family builds each trade through its builder, from what the
 * planner draws. The standard running coupons are 100 and 500 basis points.
 */
ores::trading::domain::instrument_batch
build_credit_trade(const domain::trade_generation_scope& scope,
                   const ores::marketdata::conventions::convention_lookup& conventions,
                   const domain::trade_population_slot& entry,
                   const std::string& reference_entity,
                   std::uint32_t years) {
    namespace tb = ores::trading::builders;
    using ores::utility::decimal::decimal;
    const auto money = [](double value) {
        return decimal::from_double(value).value();
    };

    const auto& type = entry.trade_type_code;
    const auto name = slot_name(scope, entry.slot);
    const auto side = first_way(entry, 1) ? tb::protection::buy : tb::protection::sell;
    const auto start = add_months(scope.as_of_date, -static_cast<int>(entry.slot % 6));
    const auto protection = [&](year_month_day from) {
        return tb::credit_protection_terms{
            .notional = money(entry.notional),
            .currency = entry.currency,
            .start = from,
            .maturity = add_years(from, static_cast<int>(years)),
            .running_spread = money(draw(name, 2) < 0.5 ? 0.01 : 0.05),
            .reference_entity = reference_entity,
            .credit_curve = credit_curve_of(reference_entity, entry.currency),
            .side = side,
            .convention = std::string(cds_convention_id)};
    };

    if (type == "CreditDefaultSwap")
        return batch_of(tb::credit_default_swap_builder(conventions, protection(start)).build(),
                        type);
    if (type == "IndexCreditDefaultSwap")
        return batch_of(
            tb::index_credit_default_swap_builder(conventions, protection(start)).build(), type);
    if (type == "CreditLinkedSwap")
        return batch_of(tb::credit_linked_swap_builder(conventions, protection(start)).build(),
                        type);
    if (type == "CreditDefaultSwapOption") {
        // The option expires before the swap it enters starts, as the index option does.
        const auto expiry = add_months(scope.as_of_date, 3);
        const tb::credit_default_swap_option_terms terms{
            .swap = protection(year_month_day{sys_days{expiry} + days{1}}),
            .expiry = expiry,
            .strike = money(draw(name, 2) < 0.5 ? 0.01 : 0.05),
            .kind = first_way(entry, 2) ? tb::option_kind::call : tb::option_kind::put,
            .position = first_way(entry) ? tb::option_position::long_option :
                                           tb::option_position::short_option};
        return batch_of(tb::credit_default_swap_option_builder(conventions, terms).build(), type);
    }
    if (type == "CBO") {
        /*
         * An investment in the junior note of the structure ORE's own example
         * names, stated inline with the example's fees, tranches and the first
         * bonds of its basket, so the trade carries its structure whole.
         */
        tb::cbo_structure_terms structure{
            .currency = std::string("USD"),
            .day_counter = std::string("A365"),
            .payment_convention = std::string("F"),
            .senior_fee = money(0.001),
            .fee_day_counter = std::string("A365"),
            .subordinated_fee = money(0.005),
            .equity_kicker = money(0.01),
            .tranches = {{.name = "SeniorNote",
                          .notional = money(16224751.80),
                          .ic_ratio = money(1.0),
                          .oc_ratio = money(1.0),
                          .fixed_rate = money(0.04)},
                         {.name = "JuniorNote",
                          .notional = money(1802750.20),
                          .ic_ratio = money(-1.0),
                          .oc_ratio = money(-1.0),
                          .fixed_rate = money(0.0)}},
            .basket = {{.security_id = "ISIN:US15135BAW19", .notional = money(1000000.0)},
                       {.security_id = "ISIN:US16411QAK76", .notional = money(1000000.0)},
                       {.security_id = "ISIN:US16411QAL59", .notional = money(1000000.0)}},
            .start = scope.as_of_date,
            .end = add_years(scope.as_of_date, 15),
            .tenor = std::string("3M"),
            .calendar = std::string("US")};
        return batch_of(tb::cbo_builder({.tranche_name = std::string("JuniorNote"),
                                         .notional = money(entry.notional),
                                         .structure_id = std::string("Pegasus"),
                                         .structure = std::move(structure)})
                            .build(),
                        type);
    }
    if (type == "IndexCreditDefaultSwapOption") {
        const auto expiry = add_months(scope.as_of_date, 3);
        const tb::index_credit_default_swap_option_terms terms{
            .swap = protection(year_month_day{sys_days{expiry} + days{1}}),
            .expiry = expiry,
            .strike = money(1.0),
            .index_term = std::format("{}Y", years),
            .position = first_way(entry) ? tb::option_position::long_option :
                                           tb::option_position::short_option};
        return batch_of(tb::index_credit_default_swap_option_builder(conventions, terms).build(),
                        type);
    }
    if (type == "RiskParticipationAgreement") {
        const auto underlying = underlying_index_of(scope);
        const tb::risk_participation_agreement_terms terms{
            .underlying = {.notional = money(entry.notional),
                           .currency = entry.currency,
                           .start = start,
                           .maturity = add_years(start, static_cast<int>(years)),
                           .fixed_rate =
                               money(std::round((0.02 + 0.02 * draw(name, 3)) * 10000.0) / 10000.0),
                           .index = underlying.index,
                           .fixed_side = draw(name, 4) < 0.5 ? tb::side::pay : tb::side::receive,
                           .convention = underlying.convention},
            .participation_rate = 0.5,
            .reference_entity = reference_entity,
            .credit_curve = credit_curve_of(reference_entity, entry.currency),
            .side = side,
            .fee = money(std::round(entry.notional * 0.001 * 100.0) / 100.0),
            .convention = std::string(cds_convention_id)};
        return batch_of(tb::risk_participation_agreement_builder(conventions, terms).build(), type);
    }
    if (type == "SyntheticCDO") {
        // The tranches cut the index at the standard points of the iTraxx and
        // CDX tranches, so each trade is one a desk would quote.
        constexpr std::pair<const char*, const char*> tranches[] = {
            {"0", "0.03"}, {"0.03", "0.06"}, {"0.06", "0.12"}, {"0.12", "1"}};
        const auto& [attachment, detachment] = tranches[entry.slot % std::size(tranches)];
        const tb::synthetic_cdo_terms terms{.protection = protection(start),
                                            .attachment_point = *decimal::from_string(attachment),
                                            .detachment_point = *decimal::from_string(detachment)};
        return batch_of(tb::synthetic_cdo_builder(conventions, terms).build(), type);
    }
    throw trade_population_error("The credit trade type " + type +
                                 " states no builder this planner knows.");
}

}

domain::trade_population_plan
credit_trade_planner::plan(const domain::trade_generation_scope& scope) const {
    validate_scope(scope);

    if (scope.credit_reference_entities.empty())
        throw trade_population_error(
            "The tenant holds no legal entity, so no credit trade can state a reference "
            "entity. Publish the LEI dataset first.");

    const auto min_years = tenor_years_of(scope.min_tenor_code);
    const auto max_years = tenor_years_of(scope.max_tenor_code);

    domain::trade_population_plan plan;
    plan.config_id = scope.config_id;
    plan.party_id = scope.party_id;
    plan.as_of_date = scope.as_of_date;

    const ores::marketdata::conventions::convention_lookup conventions(scope.conventions);
    const auto types = booked_types(credit_types, scope);
    for (std::uint32_t slot = 0; slot < slot_count(scope); ++slot) {
        const auto name = slot_name(scope, slot);

        const auto span = max_years - min_years + 1;
        const auto drawn =
            min_years + static_cast<std::uint32_t>(draw(name, 0) * static_cast<double>(span));

        auto entry = common_slot(scope, slot, types.size());
        entry.trade_type_code = std::string(*types[slot % types.size()]);
        entry.notional = notional_from(scope, draw(name, 1));
        entry.instruments = build_credit_trade(
            scope,
            conventions,
            entry,
            scope.credit_reference_entities[slot % scope.credit_reference_entities.size()],
            std::clamp(drawn, min_years, max_years));

        plan.slots.push_back(std::move(entry));
    }

    return plan;
}

namespace {

/// The oresmd address of an equity the catalogue names, which the projection
/// writes as ORE names it.
ores::marketdata::datum::market_reference equity_address_of(const std::string& code) {
    namespace md = ores::marketdata::datum;
    auto index = md::market_index::make(md::index_family::equity, code, {});
    if (!index)
        throw trade_population_error("The equity " + code + " has no address: " + index.error());
    auto reference = md::market_reference::of(std::move(*index));
    if (!reference)
        throw trade_population_error("The equity " + code +
                                     " has no address: " + reference.error());
    return std::move(*reference);
}

/// The scripted codes the population books, in the order the routing table states them.
constexpr std::string_view scripted_types[] = {"ScriptedTrade",
                                               "DoubleDigitalOption",
                                               "PerformanceOption_01",
                                               "Autocallable_01",
                                               "EuropeanOptionBarrier"};
constexpr std::size_t scripted_type_count = std::size(scripted_types);


/*
 * The scripted family builds each trade through its builder. The generic form
 * names a library script and states its variables, and its builder checks them
 * against the inputs the library holds for that script; the class-read products
 * each have a builder of their own.
 */
ores::trading::domain::instrument_batch
build_scripted_trade(const domain::trade_generation_scope& scope,
                     const domain::trade_population_slot& entry,
                     const std::chrono::year_month_day& end,
                     double notional,
                     double strike) {
    namespace tb = ores::trading::builders;
    using ores::utility::decimal::decimal;
    const auto money = [](double value) {
        return decimal::from_double(std::round(value * 100.0) / 100.0).value();
    };
    const auto level = [](double value) {
        return decimal::from_double(std::round(value * 10000.0) / 10000.0).value();
    };

    /*
     * The underlyings cycle the tenant's catalogue, so a population longer than
     * the catalogue states every name it holds and none it does not. The double
     * digital and the performance option observe a pair: a trade given one
     * name exports with no second underlying and the engine refuses it.
     */
    const auto slot = entry.slot;
    const auto& first = scope.equity_underlyings[slot % scope.equity_underlyings.size()];
    const auto& second = scope.equity_underlyings[(slot + 1) % scope.equity_underlyings.size()];
    const auto first_address = equity_address_of(first.code);
    const auto second_address = equity_address_of(second.code);
    const auto& type = entry.trade_type_code;
    const auto position =
        first_way(entry) ? tb::option_position::long_option : tb::option_position::short_option;
    const auto settlement = add_months(end, 2);

    const auto built = [&](auto result) {
        if (!result)
            throw trade_population_error("The " + type + " builder refuses the drawn " +
                                         result.error().term + ": " + result.error().message);
        ores::trading::domain::instrument_batch batch;
        ores::trading::domain::append_instrument(
            batch, ores::trading::domain::trade_instrument{std::move(*result)});
        return batch;
    };

    if (type == "ScriptedTrade") {
        /*
         * The generic form of the library's AsianBasketOption, stating every
         * variable the script reads, as ORE's own example does. PutCall and
         * LongShort are signs, 1 or -1, and the index ones are addresses, which
         * the projection writes as the ORE names they resolve to.
         */
        std::vector<ores::trading::domain::script_input> inputs;
        for (const auto& input : scope.script_inputs)
            if (input.script_name == "AsianBasketOption")
                inputs.push_back(input);
        const auto sign = [](bool first_way) {
            return std::string(first_way ? "1" : "-1");
        };
        return built(
            tb::scripted_trade_builder(
                {.script_name = std::string("AsianBasketOption"),
                 .inputs = std::move(inputs),
                 .variables = {{"Expiry", "Event", std::format("{:%Y-%m-%d}", end)},
                               {"Settlement", "Event", std::format("{:%Y-%m-%d}", settlement)},
                               {"ObservationDates",
                                "Event",
                                tb::scripted_schedule{scope.as_of_date, end, "1M"}},
                               {"PutCall", "Number", sign(first_way(entry, 1))},
                               {"LongShort", "Number", sign(first_way(entry))},
                               {"Notional", "Number", money(notional).to_string()},
                               {"Strike", "Number", level(strike).to_string()},
                               {"Underlyings",
                                "Index",
                                std::vector<std::string>{first_address.uri(),
                                                         second_address.uri()}},
                               {"Weights", "Number", std::vector<std::string>{"0.5", "0.5"}},
                               {"PayCcy", "Currency", scope.currency_code}}})
                .build());
    }

    if (type == "DoubleDigitalOption")
        return built(tb::double_digital_option_builder({.expiry = end,
                                                        .settlement = settlement,
                                                        .binary_payout = money(notional),
                                                        .binary_level_1 = level(strike),
                                                        .binary_level_2 = level(strike * 1.2),
                                                        .kind_1 = tb::digital_kind::call,
                                                        .kind_2 = tb::digital_kind::put,
                                                        .position = position,
                                                        .pay_currency = scope.currency_code,
                                                        .underlying_1 = first_address,
                                                        .underlying_2 = second_address})
                         .build());

    if (type == "EuropeanOptionBarrier")
        // The option is on the first name and knocks out on the second's level.
        return built(
            tb::european_option_barrier_builder(
                {.family = tb::scripted_family::equity,
                 .option_underlying = first_address,
                 .barrier_underlying = second_address,
                 .quantity = money(notional / strike),
                 .kind = first_way(entry, 1) ? tb::option_kind::call : tb::option_kind::put,
                 .position = position,
                 .strike = level(strike),
                 .premium_amount = money(0.0),
                 .premium_currency = scope.currency_code,
                 .premium_date = scope.as_of_date,
                 .expiry = end,
                 .barrier =
                     tb::scripted_barrier{.type = "DownAndOut", .level = level(strike * 0.9)},
                 .settlement = settlement,
                 .pay_currency = scope.currency_code})
                .build());

    if (type == "Autocallable_01") {
        /*
         * The autocallable fixes once a year from a year after the anchor, and
         * settles a month after each fixing; a fixing on the anchor has no path
         * to observe. It accumulates one factor a fixing, capped at the last.
         */
        const auto years =
            std::max(2, static_cast<int>(end.year()) - static_cast<int>(scope.as_of_date.year()));
        std::vector<decimal> factors;
        for (int year = 1; year <= years; ++year)
            factors.push_back(level(0.05 * year));
        const auto first_fixing = add_years(scope.as_of_date, 1);
        const auto last_fixing = add_years(scope.as_of_date, years);
        return built(tb::autocallable_builder(
                         {.notional = money(notional),
                          .determination_level = level(strike * 0.7),
                          .trigger_level = level(strike),
                          .underlying = first_address,
                          .position = position,
                          .pay_currency = scope.currency_code,
                          .fixing_dates = tb::scripted_schedule{first_fixing, last_fixing, "1Y"},
                          .settlement_dates = tb::scripted_schedule{add_months(first_fixing, 1),
                                                                    add_months(last_fixing, 1),
                                                                    "1Y"},
                          .accumulation_factors = factors,
                          .cap = level(0.05 * years)})
                         .build());
    }

    return built(
        tb::performance_option_builder(
            {.notional = money(notional),
             .participation_rate = level(1.0),
             .valuation_date = end,
             .settlement_date = settlement,
             .strike_prices =
                 std::vector<decimal>{level(strike), level(strike * 1.1), level(strike * 1.2)},
             .strike = level(strike),
             .position = position,
             .pay_currency = scope.currency_code,
             .underlyings = std::vector{first_address, second_address}})
            .build());
}
}

std::string_view scripted_trade_planner::asset_class_code() const {
    return "scripted";
}

std::size_t scripted_trade_planner::code_count() const {
    return scripted_type_count;
}

domain::trade_population_plan
scripted_trade_planner::plan(const domain::trade_generation_scope& scope) const {
    validate_scope(scope);

    if (scope.equity_underlyings.empty())
        throw trade_population_error(
            "The tenant holds no equity underlying, so no scripted trade can name one to observe. "
            "Publish the equity catalogue first.");

    const auto min_years = tenor_years_of(scope.min_tenor_code);
    const auto max_years = tenor_years_of(scope.max_tenor_code);

    domain::trade_population_plan plan;
    plan.config_id = scope.config_id;
    plan.party_id = scope.party_id;
    plan.as_of_date = scope.as_of_date;

    const auto types = booked_types(scripted_types, scope);
    for (std::uint32_t slot = 0; slot < slot_count(scope); ++slot) {
        const auto name = slot_name(scope, slot);
        const auto& type = *types[slot % types.size()];

        const auto span = max_years - min_years + 1;
        const auto drawn =
            min_years + static_cast<std::uint32_t>(draw(name, 0) * static_cast<double>(span));

        const auto start = add_months(scope.as_of_date, -static_cast<int>(slot % 6));
        const auto end =
            add_years(start, static_cast<int>(std::clamp(drawn, min_years, max_years)));
        const auto notional =
            price_from(name, 1, scope.min_notional, scope.max_notional - scope.min_notional);
        const auto strike = std::round((0.8 + 0.4 * draw(name, 2)) * 10000.0) / 10000.0;

        auto entry = common_slot(scope, slot, types.size());
        entry.trade_type_code = std::string(type);
        entry.notional = notional;
        entry.instruments = build_scripted_trade(scope, entry, end, notional, strike);

        plan.slots.push_back(std::move(entry));
    }

    return plan;
}

namespace {

/*
 * The one code the routing table sends to an inflation product. It is stated
 * rather than read from the table, because a planner is pure and has no
 * database, and the list has one member because the catalogue has one.
 */
constexpr std::string_view inflation_types[] = {"InflationSwap"};

/*
 * The base level the trade is struck from. The index is a level rather than a
 * rate, so the draw sits inside the band the seeded indices live in rather
 * than inside the notional band.
 */
double base_cpi_from(const std::string& name) {
    return std::round((100.0 + 60.0 * draw(name, 5)) * 100.0) / 100.0;
}

}

std::string_view inflation_trade_planner::asset_class_code() const {
    return "inflation";
}

std::size_t inflation_trade_planner::code_count() const {
    return std::size(inflation_types);
}

domain::trade_population_plan
inflation_trade_planner::plan(const domain::trade_generation_scope& scope) const {
    validate_scope(scope);

    if (scope.inflation_index_codes.empty())
        throw trade_population_error(
            "The tenant holds no inflation curve, so no inflation swap can name an index. Publish "
            "the inflation curve configuration first.");

    const auto min_years = tenor_years_of(scope.min_tenor_code);
    const auto max_years = tenor_years_of(scope.max_tenor_code);

    domain::trade_population_plan plan;
    plan.config_id = scope.config_id;
    plan.party_id = scope.party_id;
    plan.as_of_date = scope.as_of_date;

    const ores::marketdata::conventions::convention_lookup conventions(scope.conventions);
    const auto types = booked_types(inflation_types, scope);
    for (std::uint32_t slot = 0; slot < slot_count(scope); ++slot) {
        const auto name = slot_name(scope, slot);

        const auto span = max_years - min_years + 1;
        const auto drawn =
            min_years + static_cast<std::uint32_t>(draw(name, 0) * static_cast<double>(span));

        const auto& code = scope.inflation_index_codes[slot % scope.inflation_index_codes.size()];
        const auto index = ores::marketdata::datum::market_reference::parse(
            ores::marketdata::projection::ore_projection::address_of_index_or_stored(code));
        if (!index)
            throw trade_population_error("The inflation index " + code + " has no address.");
        const auto start = add_months(scope.as_of_date, -static_cast<int>(slot % 6));

        auto entry = common_slot(scope, slot, types.size());
        entry.trade_type_code = std::string(*types[slot % types.size()]);
        entry.notional =
            price_from(name, 1, scope.min_notional, scope.max_notional - scope.min_notional);
        const ores::trading::builders::inflation_swap_terms terms{
            .notional = ores::utility::decimal::decimal::from_double(entry.notional).value(),
            .currency = scope.currency_code,
            .start = start,
            .maturity = add_years(start, static_cast<int>(std::clamp(drawn, min_years, max_years))),
            .fixed_rate = ores::utility::decimal::decimal::from_double(
                              std::round((0.01 + 0.05 * draw(name, 2)) * 10000.0) / 10000.0)
                              .value(),
            .index = *index,
            .base_cpi = base_cpi_from(name),
            .fixed_side = first_way(entry) ? ores::trading::builders::side::pay :
                                             ores::trading::builders::side::receive};
        entry.instruments = swap_batch_of(
            ores::trading::builders::inflation_swap_builder(conventions, terms).build(),
            entry.trade_type_code,
            "Generated inflation swap.");

        plan.slots.push_back(std::move(entry));
    }

    return plan;
}

namespace {


/// The underlying the slot draws, cycling the catalogue the tenant published.
const domain::equity_underlying_reference& equity_for(const domain::trade_generation_scope& scope,
                                                      std::uint32_t slot) {
    return scope.equity_underlyings[slot % scope.equity_underlyings.size()];
}

}

namespace {

/**
 * @brief The first index the tenant's swap conventions name with a tenor in
 *        @p currency, with the convention that names it.
 *
 * Several conventions can name one index, so the funding leg is built by the
 * convention found here rather than by its index.
 */
std::optional<swap_choice> funding_index_in(const domain::trade_generation_scope& scope,
                                            const std::string& currency) {
    for (const auto& swap : scope.conventions.swaps) {
        if (!swap.oresmd_uri)
            continue;
        auto index = ores::marketdata::datum::market_reference::parse(*swap.oresmd_uri);
        if (index && index->index() != nullptr && index->index()->get("tenor") != nullptr &&
            index->index()->subject() == currency)
            return swap_choice{std::move(*index), swap.id};
    }
    return std::nullopt;
}

/// Two distinct equities in one currency, and the index that funds a basket of them.
struct basket_members final {
    const domain::equity_underlying_reference& first;
    const domain::equity_underlying_reference& second;
    ores::marketdata::datum::market_reference funding;
};

/*
 * A worst-of basket is in one currency and funds in it, so it takes the first
 * equity from the slot on whose currency the tenant's swap conventions name an
 * index, and the next equity in that currency.
 */
basket_members basket_members_of(const domain::trade_generation_scope& scope, std::uint32_t slot) {
    const auto& underlyings = scope.equity_underlyings;
    const auto currency_of = [](const domain::equity_underlying_reference& equity) {
        return equity.currency.empty() ? std::string("USD") : equity.currency;
    };
    for (std::size_t i = 0; i < underlyings.size(); ++i) {
        const auto& first = underlyings[(slot + i) % underlyings.size()];
        auto funding = funding_index_in(scope, currency_of(first));
        if (!funding)
            continue;
        for (std::size_t j = 1; j < underlyings.size(); ++j) {
            const auto& second = underlyings[(slot + i + j) % underlyings.size()];
            if (second.code != first.code && currency_of(second) == currency_of(first))
                return {first, second, std::move(funding->index)};
        }
    }
    throw trade_population_error("No two equity underlyings of the tenant share a currency its "
                                 "swap conventions name an index in, so no worst-of basket swap "
                                 "can be drawn.");
}

ores::trading::domain::instrument_batch
equity_batch_of(std::expected<ores::trading::domain::equity_instrument_data,
                              ores::trading::builders::build_error> built,
                const std::string& trade_type_code) {
    if (!built)
        throw trade_population_error("The " + trade_type_code + " builder refuses the drawn " +
                                     built.error().term + ": " + built.error().message);
    ores::trading::domain::instrument_batch batch;
    ores::trading::domain::append_instrument(batch,
                                             ores::trading::domain::trade_instrument{*built});
    return batch;
}

/*
 * The equity family builds each trade through its builder. The drawn level is
 * the equity's price the trade is struck at; barriers sit a tenth either side
 * of it.
 */
ores::trading::domain::instrument_batch
build_equity_trade(const domain::trade_generation_scope& scope,
                   const ores::marketdata::conventions::convention_lookup& conventions,
                   const domain::trade_population_slot& entry,
                   const domain::equity_underlying_reference& underlying,
                   year_month_day start,
                   year_month_day end,
                   double level) {
    namespace tb = ores::trading::builders;
    using ores::utility::decimal::decimal;
    const auto money = [](double value) {
        return decimal::from_double(std::round(value * 100.0) / 100.0).value();
    };

    const auto slot = entry.slot;
    const auto name = slot_name(scope, slot);
    const auto& type = entry.trade_type_code;
    const auto address = equity_address_of(underlying.code);
    const auto currency = underlying.currency.empty() ? std::string("USD") : underlying.currency;
    const auto position =
        first_way(entry) ? tb::option_position::long_option : tb::option_position::short_option;
    const auto kind = draw(name, 4) < 0.5 ? tb::option_kind::call : tb::option_kind::put;
    const auto notional = entry.notional;
    const tb::equity_option_terms option{.underlying = address,
                                         .currency = currency,
                                         .quantity = money(notional / level),
                                         .kind = kind,
                                         .strike = money(level),
                                         .expiry = end,
                                         .position = position};
    const auto built = [&](auto result) {
        return equity_batch_of(std::move(result), type);
    };
    const auto barrier = [&]<tb::equity_barrier_product Product>(std::string barrier_type,
                                                                 std::vector<decimal> levels) {
        return built(
            tb::equity_barrier_option_builder<Product>({.option = option,
                                                        .barrier_type = std::move(barrier_type),
                                                        .levels = std::move(levels)})
                .build());
    };
    const auto lower = money(level * 0.9);
    const auto upper = money(level * 1.1);

    if (type == "EquityOption")
        return built(tb::equity_option_builder(option).build());
    /*
     * A product on two equities draws them quoted in one currency, so no
     * price it states needs FX terms to meet the option's currency.
     */
    const auto same_currency_pair = [&] {
        std::vector<std::pair<const domain::equity_underlying_reference*,
                              const domain::equity_underlying_reference*>>
            pairs;
        for (const auto& first : scope.equity_underlyings)
            for (const auto& second : scope.equity_underlyings)
                if (&first != &second && first.code != second.code &&
                    first.currency == second.currency)
                    pairs.emplace_back(&first, &second);
        if (pairs.empty())
            throw trade_population_error("A " + type +
                                         " names two equities in one currency, "
                                         "and the tenant holds no such pair.");
        return pairs[slot % pairs.size()];
    };
    if (type == "EquityBasketOption" || type == "EquityRainbowOption") {
        const auto [first, second] = same_currency_pair();
        // Two unit weights put the basket near twice one member, so the strike sits there.
        const tb::basket_option_terms basket{
            .currency = first->currency.empty() ? std::string("USD") : first->currency,
            .notional = money(notional / level),
            .strike = money(level * 2.0),
            .members =
                std::vector<tb::basket_option_member>{
                    {.underlying = equity_address_of(first->code),
                     .currency = std::nullopt,
                     .weight = money(1.0)},
                    {.underlying = equity_address_of(second->code),
                     .currency = std::nullopt,
                     .weight = money(1.0)}},
            .payoff_type = std::string(type == "EquityBasketOption" ? "Vanilla" : "MaxRainbow"),
            .kind = kind,
            .position = position,
            .expiry = end,
            .settlement = end};
        if (type == "EquityBasketOption")
            return built(
                tb::basket_option_builder<tb::basket_product::equity_basket>(basket).build());
        return built(tb::basket_option_builder<tb::basket_product::equity_rainbow>(basket).build());
    }
    if (type == "EquityWindowBarrierOption" || type == "EquityGenericBarrierOption") {
        // A call knocks out a tenth below the level, a put a tenth above it.
        const bool call = kind == tb::option_kind::call;
        const tb::scripted_barrier knock_out{.type = call ? "DownAndOut" : "UpAndOut",
                                             .level = money(level * (call ? 0.9 : 1.1))};
        if (type == "EquityWindowBarrierOption")
            return built(tb::equity_window_barrier_option_builder(
                             {.underlying = address,
                              .currency = currency,
                              .fixing_amount = money(notional / level),
                              .strike = money(level),
                              .kind = kind,
                              .position = position,
                              .expiry = end,
                              .window_start = start,
                              .window_end = year_month_day{sys_days{start} +
                                                           (sys_days{end} - sys_days{start}) / 2},
                              .barrier = knock_out})
                             .build());
        return built(tb::equity_generic_barrier_option_builder(
                         {.underlying = address,
                          .pay_currency = currency,
                          .quantity = money(notional / level),
                          .strike = money(level),
                          .kind = kind,
                          .position = position,
                          .expiry = end,
                          // ORE rolls an expiry on a holiday forward.
                          .settlement = year_month_day{sys_days{end} + days{5}},
                          .monitoring_start = start,
                          .barriers = std::vector<tb::scripted_barrier>{knock_out},
                          .kiko_type = std::nullopt})
                         .build());
    }
    if (type == "EquityBestEntryOption")
        return built(tb::equity_best_entry_option_builder(
                         {.underlying = address,
                          .currency = currency,
                          .notional = money(notional),
                          .multiplier = money(1.0),
                          .strike = money(0.85),
                          .cap = money(0.06),
                          .trigger_level = money(0.95),
                          .reset_minimum = money(0.85),
                          .premium = money(0.0),
                          .premium_date = start,
                          .strike_date = start,
                          .strike_observation_dates = quarterly_dates(start, end),
                          .expiry = end,
                          .settlement = end,
                          .position = position})
                         .build());
    if (type == "EquityStrikeResettableOption") {
        // A call resets lower when the price falls, a put higher when it rises.
        const bool call = kind == tb::option_kind::call;
        const auto reset = money(level * (call ? 0.9 : 1.1));
        return built(tb::equity_strike_resettable_option_builder(
                         {.underlying = address,
                          .currency = currency,
                          .kind = kind,
                          .position = position,
                          .quantity = money(notional / level),
                          .strike = money(level),
                          .reset_strike = reset,
                          .trigger = call ? tb::trigger_direction::down : tb::trigger_direction::up,
                          .trigger_price = reset,
                          .observation_dates = quarterly_dates(start, end),
                          .expiry = end,
                          .settlement = end,
                          .premium = money(0.0),
                          .premium_date = start})
                         .build());
    }
    if (type == "EquityBasketVarianceSwap" || type == "EquityPairwiseVarianceSwap") {
        const auto [first, second] = same_currency_pair();
        const auto pay = first->currency.empty() ? std::string("USD") : first->currency;
        // A volatility strike of a fifth, observed daily over the trade's life.
        const auto strike = money(0.2);
        if (type == "EquityBasketVarianceSwap")
            return built(tb::equity_basket_variance_swap_builder(
                             {.members =
                                  std::vector<tb::basket_option_member>{
                                      {.underlying = equity_address_of(first->code),
                                       .currency = std::nullopt,
                                       .weight = money(0.5)},
                                      {.underlying = equity_address_of(second->code),
                                       .currency = std::nullopt,
                                       .weight = money(0.5)}},
                              .currency = pay,
                              .notional = money(notional / 100.0),
                              .strike = strike,
                              .squared_payoff = false,
                              .start = start,
                              .end = end,
                              .settlement = year_month_day{sys_days{end} + days{5}},
                              .position = position})
                             .build());
        return built(
            tb::equity_pairwise_variance_swap_builder(
                {.first = tb::pairwise_member{.underlying = equity_address_of(first->code),
                                              .strike = strike,
                                              .notional = money(notional / 200.0)},
                 .second = tb::pairwise_member{.underlying = equity_address_of(second->code),
                                               .strike = strike,
                                               .notional = money(notional / 200.0)},
                 .basket_notional = money(notional / 100.0),
                 .basket_strike = strike,
                 .pay_currency = pay,
                 .start = start,
                 .end = end,
                 .settlement = year_month_day{sys_days{end} + days{5}},
                 .position = position})
                .build());
    }
    if (type == "EquityFutureOption")
        // The future expires a month after the option does.
        return built(tb::equity_future_option_builder(
                         {.option = option, .future_expiry = add_months(end, 1)})
                         .build());
    if (type == "EquityForwardVolatilityAgreement") {
        // The volatility fixes halfway to the expiry, which the straddle runs to.
        const auto fva_date =
            year_month_day{sys_days{start} + (sys_days{end} - sys_days{start}) / 2};
        return built(tb::equity_forward_volatility_agreement_builder(
                         {.underlying = address,
                          .pay_currency = currency,
                          .quantity = money(notional / level),
                          .implied_vol_strike = money(0.2),
                          .fva_date = fva_date,
                          .option_expiry = end,
                          .premium_date = year_month_day{sys_days{fva_date} + days{2}},
                          .settlement = year_month_day{sys_days{end} + days{5}},
                          .position = position})
                         .build());
    }
    if (type == "EquityOutperformanceOption") {
        const auto [first, second] = same_currency_pair();
        const auto pair_currency = first->currency.empty() ? std::string("USD") : first->currency;
        return built(tb::equity_outperformance_option_builder(
                         {.underlying_1 = equity_address_of(first->code),
                          .underlying_2 = equity_address_of(second->code),
                          .currency = pair_currency,
                          .notional = money(notional),
                          .kind = kind,
                          .expiry = end,
                          .position = position,
                          .initial_price_1 = money(level),
                          .initial_price_2 = money(level * 1.1),
                          .strike_return = money(0.01),
                          .knock_in_price = money(level * 1.05),
                          .knock_out_price = std::nullopt})
                         .build());
    }
    if (type == "EquityCliquetOption")
        return built(tb::equity_cliquet_option_builder(
                         {.option = option, .start = start, .reset_tenor = std::string("3M")})
                         .build());
    if (type == "EquityAsianOption")
        return built(
            tb::equity_asian_option_builder({.option = option, .averaging_start = start}).build());
    if (type == "EquityBarrierOption")
        return barrier.template operator()<tb::equity_barrier_product::barrier>("DownAndOut",
                                                                                {lower});
    if (type == "EquityEuropeanBarrierOption")
        return barrier.template operator()<tb::equity_barrier_product::european_barrier>(
            "DownAndOut", {lower});
    if (type == "EquityDoubleBarrierOption")
        return barrier.template operator()<tb::equity_barrier_product::double_barrier>(
            "KnockOut", {lower, upper});
    if (type == "EquityDigitalOption")
        return built(
            tb::equity_digital_option_builder({.option = option, .payout = money(notional)})
                .build());
    if (type == "EquityTouchOption")
        return built(tb::equity_touch_option_builder({.underlying = address,
                                                      .currency = currency,
                                                      .payout = money(notional),
                                                      .barrier_type = std::string("UpAndIn"),
                                                      .level = upper,
                                                      .expiry = end,
                                                      .position = position})
                         .build());
    /*
     * Cash in the underlying's currency, the trade's notional. The slot's
     * position, drawn for every equity trade, says whether the cash is held
     * long or short.
     */
    if (type == "CashPosition")
        return built(tb::cash_position_builder(
                         {.currency = currency,
                          .amount = money(
                              position == tb::option_position::long_option ? notional : -notional)})
                         .build());
    if (type == "EquityAutoDeltaHedgedOption") {
        /*
         * A call and a put at the level on the slot's equity, half the
         * notional each, hedged daily from the start and settled five days
         * after they expire.
         */
        const auto batch = [&](tb::option_kind batch_kind) {
            return tb::delta_hedged_batch{.underlying = address,
                                          .currency = currency,
                                          .strike = money(level),
                                          .quantity = money(notional / level / 2.0),
                                          .kind = batch_kind,
                                          .position = position,
                                          .expiry = end,
                                          .premium = money(notional * 0.01),
                                          .premium_date = start};
        };
        return built(
            tb::equity_auto_delta_hedged_option_builder(
                {.volatility = money(0.2),
                 .drift_rate = money(0.0),
                 .observation_start = start,
                 .payment_date = year_month_day{sys_days{end} + days{5}},
                 .batches = std::vector<tb::delta_hedged_batch>{batch(tb::option_kind::call),
                                                                batch(tb::option_kind::put)}})
                .build());
    }
    if (type == "EquityDoubleTouchOption")
        return built(tb::equity_double_touch_option_builder({.underlying = address,
                                                             .currency = currency,
                                                             .payout = money(notional),
                                                             .barrier_type = std::string("KnockIn"),
                                                             .lower = lower,
                                                             .upper = upper,
                                                             .expiry = end,
                                                             .position = position})
                         .build());
    if (type == "EquityForward")
        return built(
            tb::equity_forward_builder({.underlying = address,
                                        .currency = currency,
                                        .quantity = std::max(1.0, std::round(notional / level)),
                                        .forward_price = money(level),
                                        .maturity = end,
                                        .position = position})
                .build());
    if (type == "EquitySwap") {
        /*
         * The funding leg is in the equity's own currency, so the swap draws
         * from the underlyings whose currency the tenant's swap conventions
         * name an index in.
         */
        for (std::size_t i = 0; i < scope.equity_underlyings.size(); ++i) {
            const auto& candidate =
                scope.equity_underlyings[(slot + i) % scope.equity_underlyings.size()];
            const auto candidate_currency =
                candidate.currency.empty() ? std::string("USD") : candidate.currency;
            auto funding = funding_index_in(scope, candidate_currency);
            if (!funding)
                continue;
            return built(
                tb::equity_swap_builder(conventions,
                                        {.underlying = equity_address_of(candidate.code),
                                         .quantity = std::max(1.0, std::round(notional / level)),
                                         .position = position,
                                         .funding = {.notional = money(notional),
                                                     .currency = candidate_currency,
                                                     .start = start,
                                                     .maturity = end,
                                                     .fixed_rate = money(0.0),
                                                     .index = std::move(funding->index),
                                                     .fixed_side = tb::side::pay,
                                                     .convention = funding->convention}})
                    .build());
        }
        throw trade_population_error("No equity underlying of the tenant is in a currency its "
                                     "swap conventions name an index in, so no equity swap can "
                                     "state its funding leg.");
    }
    if (type == "EquityWorstOfBasketSwap") {
        if (scope.equity_underlyings.size() < 2)
            throw trade_population_error("A worst-of basket names two distinct equities, and the "
                                         "tenant holds fewer than two equity underlyings.");
        const auto& [first, second, funding] = basket_members_of(scope, slot);
        const auto basket_currency = first.currency.empty() ? std::string("USD") : first.currency;
        std::vector<tb::equity_basket_member> basket{
            {.underlying = equity_address_of(first.code), .initial_price = money(level)},
            {.underlying = equity_address_of(second.code), .initial_price = money(level * 1.1)}};
        std::vector<year_month_day> observations;
        for (int months = 3; add_months(start, months) < end; months += 3)
            observations.push_back(add_months(start, months));
        observations.push_back(end);
        return built(
            tb::equity_worst_of_basket_swap_builder(conventions,
                                                    {.basket = std::move(basket),
                                                     .observation_dates = std::move(observations),
                                                     .funding_index = funding,
                                                     .currency = basket_currency,
                                                     .notional = money(notional),
                                                     .start = start,
                                                     .maturity = end,
                                                     .position = position})
                .build());
    }
    if (type == "EquityVarianceSwap")
        return built(tb::equity_variance_swap_builder({.underlying = address,
                                                       .currency = currency,
                                                       .notional = money(notional),
                                                       .variance_strike = 0.04,
                                                       .start = start,
                                                       .maturity = end,
                                                       .position = position})
                         .build());
    if (type == "EquityAccumulator")
        return built(tb::equity_accumulator_builder({.underlying = address,
                                                     .currency = currency,
                                                     .fixing_amount = money(notional / level),
                                                     .strike = money(level),
                                                     .knock_out = upper,
                                                     .start = start,
                                                     .expiry = end,
                                                     .position = position})
                         .build());
    if (type == "EquityTaRF")
        return built(tb::equity_tarf_builder({.underlying = address,
                                              .currency = currency,
                                              .fixing_amount = money(notional / level),
                                              .strike = money(level),
                                              .target_amount = money(notional * 0.1),
                                              .start = start,
                                              .expiry = end,
                                              .position = position})
                         .build());
    if (type == "EquityPosition")
        return built(
            tb::equity_position_builder({.underlying = address,
                                         .currency = currency,
                                         .quantity = std::max(1.0, std::round(notional / level)),
                                         .price = money(level)})
                .build());
    if (type == "EquityOptionPosition")
        /*
         * The position holds one entry per underlying it observes, and each
         * entry states its own option: the strike it is written at, the
         * direction the slot drew, and a European cash settlement that expires
         * with the trade. A position with no entry has nothing for the engine
         * to price, so one entry is the fewest it can hold.
         */
        return built(
            tb::equity_option_position_builder(
                {.currency = currency,
                 .quantity = std::max(1.0, std::round(notional / level)),
                 .price = money(level),
                 .entries =
                     std::vector<tb::equity_position_option_entry>{tb::equity_position_option_entry{
                         .underlying = address,
                         .strike = money(level),
                         .long_short = tb::long_short_of(position),
                         .option_type = kind == tb::option_kind::call ? "Call" : "Put",
                         .exercise_type = "European",
                         .settlement_type = "Cash",
                         .exercise_date = end,
                         .weight = std::nullopt}}})
                .build());
    throw trade_population_error("The equity trade type " + type +
                                 " states no builder this planner knows.");
}

}

namespace {

boost::uuids::uuid structure_id_for(const boost::uuids::uuid& first_leg);

bool is_container(std::string_view type) {
    return type == "CompositeTrade" || type == "ContractForDifference" || type == "TotalReturnSwap";
}

ores::trading::domain::instrument_batch
composite_batch_of(std::expected<ores::trading::domain::composite_instrument_data,
                                 ores::trading::builders::build_error> built,
                   const std::string& trade_type_code) {
    if (!built)
        throw trade_population_error("The " + trade_type_code + " builder refuses the drawn " +
                                     built.error().term + ": " + built.error().message);
    ores::trading::domain::instrument_batch batch;
    ores::trading::domain::append_instrument(batch,
                                             ores::trading::domain::trade_instrument{*built});
    return batch;
}

/*
 * The equity a return is paid on: one quoted in the scope's currency, so the
 * return and the funding leg need no FX terms to meet.
 */
const domain::equity_underlying_reference&
local_equity_for(const domain::trade_generation_scope& scope, std::uint32_t slot) {
    std::vector<const domain::equity_underlying_reference*> local;
    for (const auto& underlying : scope.equity_underlyings)
        if (underlying.currency == scope.currency_code)
            local.push_back(&underlying);
    if (local.empty())
        throw trade_population_error("The tenant holds no equity quoted in " + scope.currency_code +
                                     ", so no return on one can be paid in it.");
    return *local[slot % local.size()];
}

/*
 * A container books the trades it holds as trades of their own, joined to it
 * by a structure of its template, the way an imported one is booked. A
 * composite is a synthetic forward: a long call and a short put at one strike
 * and expiry. A total return swap and a contract for difference pay the return
 * on a position in an equity.
 */
void add_container(domain::trade_population_plan& plan,
                   const domain::trade_generation_scope& scope,
                   const ores::marketdata::conventions::convention_lookup& conventions,
                   domain::trade_population_slot parent,
                   year_month_day start,
                   year_month_day end,
                   double level) {
    namespace tb = ores::trading::builders;
    using ores::utility::decimal::decimal;
    const auto money = [](double value) {
        return decimal::from_double(std::round(value * 100.0) / 100.0).value();
    };
    const auto& type = parent.trade_type_code;
    const bool composite = type == "CompositeTrade";
    const auto& underlying =
        composite ? equity_for(scope, parent.slot) : local_equity_for(scope, parent.slot);
    const auto structure_id = structure_id_for(parent.trade_id);

    const auto child = [&](std::string child_type, int sequence) {
        auto out = parent;
        out.trade_type_code = std::move(child_type);
        out.trade_id = named("trade:" + boost::uuids::to_string(parent.trade_id) +
                             ":sub_trade:" + std::to_string(sequence));
        out.internal_identifier = parent.internal_identifier + "-" + std::to_string(sequence);
        out.structure_id = structure_id;
        out.structure_role = "sub_trade";
        out.structure_sequence = static_cast<std::uint32_t>(sequence);
        out.instruments = {};
        return out;
    };

    std::vector<domain::trade_population_slot> children;
    if (composite) {
        const auto currency =
            underlying.currency.empty() ? std::string("USD") : underlying.currency;
        const auto leg = [&](tb::option_kind kind, tb::option_position position, int sequence) {
            auto out = child("EquityOption", sequence);
            out.instruments = equity_batch_of(
                tb::equity_option_builder({.underlying = equity_address_of(underlying.code),
                                           .currency = currency,
                                           .quantity = money(parent.notional / level),
                                           .kind = kind,
                                           .strike = money(level),
                                           .expiry = end,
                                           .position = position})
                    .build(),
                out.trade_type_code);
            return out;
        };
        children.push_back(leg(tb::option_kind::call, tb::option_position::long_option, 1));
        children.push_back(leg(tb::option_kind::put, tb::option_position::short_option, 2));
        parent.instruments = composite_batch_of(
            tb::composite_trade_builder(
                {.currency = currency, .notional_calculation = std::string("Mean")})
                .build(),
            type);
    } else {
        auto position = child("EquityPosition", 1);
        position.instruments =
            build_equity_trade(scope, conventions, position, underlying, start, end, level);
        children.push_back(std::move(position));
        const tb::total_return_terms returns{.return_side = first_way(parent) ? tb::side::receive :
                                                                                tb::side::pay,
                                             .currency = scope.currency_code,
                                             .start = start,
                                             .end = end,
                                             .initial_price = money(level)};
        if (type == "ContractForDifference") {
            parent.instruments =
                composite_batch_of(tb::contract_for_difference_builder(returns).build(), type);
        } else {
            parent.instruments = composite_batch_of(
                tb::total_return_swap_builder(conventions,
                                              {.return_leg = returns,
                                               .funding_notional = money(parent.notional),
                                               .funding_index = underlying_index_of(scope).index})
                    .build(),
                type);
        }
    }

    domain::trade_population_structure structure;
    structure.structure_id = structure_id;
    structure.counterparty_id = parent.counterparty_id;
    structure.kind = "Typed";
    structure.template_code = composite ? "CompositeTrade" : "TotalReturnSwap";
    plan.structures.push_back(std::move(structure));

    parent.structure_id = structure_id;
    parent.structure_role = composite ? "composite" : "total_return_swap";
    parent.structure_sequence = 1;
    plan.slots.push_back(std::move(parent));
    for (auto& entry : children)
        plan.slots.push_back(std::move(entry));
}

}

domain::trade_population_plan
equity_trade_planner::plan(const domain::trade_generation_scope& scope) const {
    validate_scope(scope);

    if (scope.equity_underlyings.empty())
        throw trade_population_error(
            "The tenant holds no equity underlying, so no equity trade can name one. Publish "
            "the equity underlying catalogue first.");

    const auto min_years = tenor_years_of(scope.min_tenor_code);
    const auto max_years = tenor_years_of(scope.max_tenor_code);

    domain::trade_population_plan plan;
    plan.config_id = scope.config_id;
    plan.party_id = scope.party_id;
    plan.as_of_date = scope.as_of_date;

    const ores::marketdata::conventions::convention_lookup conventions(scope.conventions);
    const auto types = booked_types(equity_types, scope);
    for (std::uint32_t slot = 0; slot < slot_count(scope); ++slot) {
        const auto name = slot_name(scope, slot);
        const auto& type = *types[slot % types.size()];
        const auto& underlying = equity_for(scope, slot);

        const auto span = max_years - min_years + 1;
        const auto drawn =
            min_years + static_cast<std::uint32_t>(draw(name, 0) * static_cast<double>(span));
        const auto tenor_years = std::clamp(drawn, min_years, max_years);
        const auto start = add_months(scope.as_of_date, -static_cast<int>(slot % 6));
        const auto end = add_years(start, static_cast<int>(tenor_years));
        const auto notional =
            price_from(name, 1, scope.min_notional, scope.max_notional - scope.min_notional);
        const auto level = std::round(price_from(name, 2, 10.0, 4990.0) * 100.0) / 100.0;

        auto entry = common_slot(scope, slot, types.size());
        entry.trade_type_code = std::string(type);
        entry.notional = notional;
        if (is_container(type)) {
            add_container(plan, scope, conventions, std::move(entry), start, end, level);
            continue;
        }
        entry.instruments =
            build_equity_trade(scope, conventions, entry, underlying, start, end, level);
        plan.slots.push_back(std::move(entry));
    }

    return plan;
}


namespace {

/**
 * @brief The id a grouped structure takes, derived from its first leg.
 *
 * A run that is repeated names the same structures, which is what lets the
 * service skip the ones already there instead of grouping the same trades
 * again under new ids. One byte is flipped so a structure's id can never be a
 * trade's.
 */
boost::uuids::uuid structure_id_for(const boost::uuids::uuid& first_leg) {
    auto id = first_leg;
    id.data[15] = static_cast<unsigned char>(id.data[15] ^ 0x5A);
    return id;
}

}

void group_into_structures(domain::trade_population_plan& plan, std::uint32_t structures_per_book) {
    if (structures_per_book == 0 || plan.slots.empty())
        return;

    // Trades are grouped within their own book: a structure belongs to the book
    // its legs were booked into, so pairing across books would state a deal no
    // desk holds.
    std::vector<std::size_t> order(plan.slots.size());
    for (std::size_t i = 0; i < order.size(); ++i)
        order[i] = i;

    std::vector<bool> grouped(plan.slots.size(), false);
    for (std::size_t i = 0; i < plan.slots.size(); ++i)
        grouped[i] = plan.slots[i].structure_id.has_value();
    for (std::uint32_t n = 0; n < structures_per_book; ++n) {
        std::optional<std::size_t> first;
        bool paired = false;
        for (const auto i : order) {
            if (grouped[i])
                continue;
            if (!first) {
                first = i;
                continue;
            }
            if (plan.slots[i].book_id != plan.slots[*first].book_id)
                continue;

            domain::trade_population_structure structure;
            structure.structure_id = structure_id_for(plan.slots[*first].trade_id);
            structure.counterparty_id = plan.slots[*first].counterparty_id;
            structure.kind = "Package";

            plan.slots[*first].structure_id = structure.structure_id;
            plan.slots[*first].structure_role = "leg";
            plan.slots[*first].structure_sequence = 1;
            plan.slots[i].structure_id = structure.structure_id;
            plan.slots[i].structure_role = "leg";
            plan.slots[i].structure_sequence = 2;
            grouped[*first] = true;
            grouped[i] = true;

            plan.structures.push_back(std::move(structure));
            paired = true;
            break;
        }
        if (!first || !paired)
            break;
    }
}

}
