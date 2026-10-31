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
#include "ores.trading.api/domain/bond_instruments.hpp"
#include "ores.trading.api/domain/bond_product_batch.hpp"
#include "ores.trading.api/domain/commodity_instruments.hpp"
#include "ores.trading.api/domain/credit_instruments.hpp"
#include "ores.trading.api/domain/equity_instruments.hpp"
#include "ores.trading.api/domain/fx_instruments.hpp"
#include "ores.trading.api/domain/rate_instruments.hpp"
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
    const auto last = year_month_day_last{y, month_day_last{m}};
    return year_month_day{y, m, d > last.day() ? last.day() : d};
}

year_month_day add_years(const year_month_day& date, int years) {
    return state_day(year{static_cast<int>(date.year()) + years}, date.month(), date.day());
}

year_month_day add_months(const year_month_day& date, int months) {
    const auto shifted = year_month{date.year(), date.month()} + std::chrono::months{months};
    return state_day(shifted.year(), shifted.month(), date.day());
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
        "bond", "commodity", "credit", "equity", "fx", "interest_rates"};
    return codes;
}

const trade_planner& planner_for(const std::string& asset_class_code) {
    static const bond_trade_planner bond;
    static const commodity_trade_planner commodity;
    static const rate_trade_planner rates;
    static const fx_trade_planner fx;
    static const credit_trade_planner credit;
    static const equity_trade_planner equity;
    static const std::map<std::string, const trade_planner*> planners{{"bond", &bond},
                                                                      {"commodity", &commodity},
                                                                      {"credit", &credit},
                                                                      {"equity", &equity},
                                                                      {"fx", &fx},
                                                                      {"interest_rates", &rates}};

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

std::uint32_t slot_count(const domain::trade_generation_scope& scope) {
    return scope.trades_per_book * static_cast<std::uint32_t>(scope.book_ids.size());
}

domain::trade_population_slot common_slot(const domain::trade_generation_scope& scope,
                                          std::uint32_t slot) {
    domain::trade_population_slot entry;
    entry.slot = slot;
    entry.book_id = scope.book_ids[slot % scope.book_ids.size()];
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

/*
 * The bond family states its security before it states its instrument: a trade
 * is written against an issue, and the issue with its rows is the security's
 * own description. The construction lives here, beside the drawing, so the plan
 * carries the issue and not a bag of fields.
 */
void state_bond_security(const domain::bond_terms& terms,
                         const std::string& currency,
                         domain::trade_population_slot& entry,
                         const ores::trading::domain::bond_product& product) {
    namespace tdom = ores::trading::domain;
    const auto money = [](double value) {
        return tdom::decimal::from_double(value).value();
    };

    tdom::bond_issue issue;
    issue.issue_id = terms.issue_id;
    issue.security_id = terms.security_id;
    issue.issuer = boost::uuids::to_string(terms.issuer_id);
    issue.face_value = money(security_face_value);
    issue.issue_date = terms.issue_date;
    issue.settlement_days = 2;
    issue.calendar = terms.calendar;
    issue.sub_type = "Fixed";
    issue.price_type = "Clean";
    /*
     * The curve the bond is discounted on. The engine resolves the name the
     * trade states through the market's own forwarding, so a bond that states
     * none is refused with a curve it could not find rather than priced.
     */
    if (!terms.reference_curve_id.empty())
        issue.reference_curve_id = terms.reference_curve_id;
    entry.bond_issue = std::move(issue);

    tdom::bond_issue_leg leg;
    leg.issue_id = terms.issue_id;
    leg.leg_number = 1;
    leg.payer = false;
    leg.leg_type = "Fixed";
    leg.currency = currency;
    leg.payment_convention = "F";
    leg.day_counter = "ACT/ACT";
    entry.bond_issue_legs.push_back(std::move(leg));

    tdom::bond_issue_leg_amount notional;
    notional.issue_id = terms.issue_id;
    notional.leg_number = 1;
    notional.amount_role = "notional";
    notional.sequence_number = 1;
    notional.value = money(security_face_value);
    entry.bond_issue_leg_amounts.push_back(std::move(notional));

    /*
     * The coupon: a fixed leg states a rate and no index, and the reader takes
     * the rate itself from the amount list under the "rate" role. Without both,
     * the leg the engine rebuilds has no FixedLegData and refuses to price.
     */
    tdom::bond_issue_leg_rate rate;
    rate.issue_id = terms.issue_id;
    rate.leg_number = 1;
    rate.rate_kind = "fixed";
    entry.bond_issue_leg_rates.push_back(std::move(rate));

    tdom::bond_issue_leg_amount coupon;
    coupon.issue_id = terms.issue_id;
    coupon.leg_number = 1;
    coupon.amount_role = "rate";
    coupon.sequence_number = 2;
    coupon.value = money(coupon_rate);
    entry.bond_issue_leg_amounts.push_back(std::move(coupon));

    /*
     * The schedule is a rule and not a date list, so the terms stay relative to
     * the anchor: the tenor and the convention are what the document states,
     * and the dates follow from them.
     */
    tdom::bond_issue_leg_schedule schedule;
    schedule.issue_id = terms.issue_id;
    schedule.leg_number = 1;
    schedule.schedule_role = "schedule";
    schedule.sequence_number = 1;
    schedule.schedule_kind = "rules";
    schedule.start_date = terms.issue_date;
    schedule.end_date = terms.maturity_date;
    schedule.tenor = "1Y";
    schedule.calendar = terms.calendar;
    schedule.convention = "F";
    schedule.term_convention = "F";
    schedule.rule = "Forward";
    entry.bond_issue_leg_schedules.push_back(std::move(schedule));

    /*
     * A callable security's calls and a convertible's targets belong to the
     * issue, so they join it here rather than travelling with the trade.
     */
    for (const auto& call : product.call_dates)
        entry.bond_issue_call_dates.push_back(call);
    for (const auto& target : product.conversion_targets)
        entry.bond_issue_conversion_targets.push_back(target);
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
                                           "Ascot"};
constexpr std::size_t bond_type_count = sizeof(bond_types) / sizeof(bond_types[0]);

constexpr std::string_view bond_futures[] = {"FGBL", "FGBM", "ZN", "ZB"};

/*
 * The product a bond trade states, constructed through the constructor its code
 * names. What a code cannot state it does not fill: an option states a strike, a
 * repo states its rate, a convertible states what it may convert into.
 */
ores::trading::domain::bond_product
construct_bond_product(const domain::bond_terms& terms,
                       const std::string& type,
                       const std::string& name,
                       std::uint32_t slot,
                       const domain::trade_generation_scope& scope,
                       double notional) {
    namespace tdom = ores::trading::domain;
    const auto money = [](double value) { return tdom::decimal::from_double(value).value(); };

    tdom::bond_common_terms common;
    common.issue_id = terms.issue_id;
    common.notional = money(notional);

    const auto option_type = draw(name, 5) < 0.5 ? std::string("Call") : std::string("Put");
    const auto strike = money(std::round((95.0 + 10.0 * draw(name, 6)) * 100.0) / 100.0);

    if (type == "Bond") {
        tdom::bond_terms bond;
        bond.issue_id = common.issue_id;
        bond.notional = common.notional;
        tdom::bond_product product;
        product.row = tdom::construct_bond(bond);
        return product;
    }

    if (type == "BondPosition")
        return tdom::construct_bond_position(common);

    if (type == "BondOption") {
        tdom::bond_option_terms option;
        option.common = common;
        option.option_type = option_type;
        option.strike = strike;
        option.redemption = "100";
        option.price_type = "Clean";
        option.knocks_out = "false";
        return tdom::construct_bond_option(option);
    }

    if (type == "ForwardBond") {
        tdom::bond_forward_terms forward;
        forward.common = common;
        forward.long_in_forward = slot % 2 == 0 ? "Long" : "Short";
        forward.forward_maturity_date = format_date(terms.maturity_date);
        forward.forward_settlement_date = format_date(add_months(terms.maturity_date, -1));
        forward.settlement = "Cash";
        forward.lock_rate = money(std::round((0.02 + 0.03 * draw(name, 7)) * 10000.0) / 10000.0);
        forward.settlement_dirty = "false";
        return tdom::construct_forward_bond(forward);
    }

    const auto future_terms = [&] {
        tdom::bond_future_terms future;
        future.common = common;
        future.contract_name = std::string(bond_futures[slot % 4]);
        future.contract_notional = money(100000.0);
        future.long_short = slot % 2 == 0 ? "Long" : "Short";
        return future;
    };

    if (type == "BondFuture")
        return tdom::construct_bond_future(future_terms());

    if (type == "BondFutureOption") {
        tdom::bond_future_option_terms terms;
        terms.future = future_terms();
        terms.option_type = option_type;
        terms.strike = strike;
        terms.redemption = "100";
        terms.price_type = "Clean";
        return tdom::construct_bond_future_option(terms);
    }

    if (type == "BondRepo") {
        tdom::bond_repo_terms repo;
        repo.common = common;
        // The store states a repo as fixed or floating, which is how its
        // rate or its index is read; a repo that says neither is refused.
        repo.repo_type = slot % 2 == 0 ? "Fixed" : "Floating";
        repo.repo_rate = money(std::round((0.03 + 0.02 * draw(name, 8)) * 10000.0) / 10000.0);
        return tdom::construct_bond_repo(repo);
    }

    if (type == "BondTRS") {
        tdom::bond_trs_terms trs;
        trs.common = common;
        trs.return_type = "Price";
        trs.funding_leg_type = "Fixed";
        trs.funding_rate = money(std::round((0.02 + 0.01 * draw(name, 9)) * 10000.0) / 10000.0);
        trs.price_type = "Clean";
        trs.initial_price = money(100.0);
        return tdom::construct_bond_trs(trs);
    }

    if (type == "CallableBond") {
        /*
         * The issuer may call the bond on the two dates before it matures, which
         * is the shape a callable states and the reason its calls belong to the
         * security rather than to the trade.
         */
        tdom::callable_bond_terms callable;
        callable.common = common;
        callable.calls.push_back({.call_date = add_years(terms.maturity_date, -2)});
        callable.calls.push_back({.call_date = add_years(terms.maturity_date, -1)});
        return tdom::construct_callable_bond(callable);
    }

    if (type == "ConvertibleBond") {
        if (scope.equity_underlyings.empty())
            throw trade_population_error(
                "A convertible bond states the share it may convert into, and the tenant holds no "
                "equity underlying. Publish the equity catalogue first.");
        tdom::convertible_bond_terms convertible;
        convertible.common = common;
        convertible.targets.push_back(
            {.underlying_id = scope.equity_underlyings[slot % scope.equity_underlyings.size()].code,
             .conversion_ratio = std::round((5.0 + 10.0 * draw(name, 10)) * 100.0) / 100.0});
        return tdom::construct_convertible_bond(convertible);
    }

    tdom::ascot_terms ascot_terms_;
    ascot_terms_.common = common;
    ascot_terms_.option_type = option_type;
    return tdom::construct_ascot(ascot_terms_);
}

domain::trade_population_plan
bond_trade_planner::plan(const domain::trade_generation_scope& scope) const {
    validate_scope(scope);

    const auto min_years = tenor_years_of(scope.min_tenor_code);
    const auto max_years = tenor_years_of(scope.max_tenor_code);

    domain::trade_population_plan plan;
    plan.config_id = scope.config_id;
    plan.party_id = scope.party_id;
    plan.as_of_date = scope.as_of_date;

    for (std::uint32_t slot = 0; slot < slot_count(scope); ++slot) {
        const auto name = slot_name(scope, slot);

        const auto span = max_years - min_years + 1;
        const auto drawn =
            min_years + static_cast<std::uint32_t>(draw(name, 0) * static_cast<double>(span));

        domain::bond_terms terms;
        terms.tenor_years = std::clamp(drawn, min_years, max_years);
        terms.security_id = security_id_for(
            scope, terms.tenor_years, bond_types[slot % bond_type_count]);
        terms.issue_id = issue_id_for_security(terms.security_id);
        terms.issuer_id = issuer_id_for(scope, terms.tenor_years);
        terms.reference_curve_id = scope.reference_curve_id;

        /*
         * The anchor is the config's as-of and never the clock, so a
         * re-provision at the same anchor states the same dates and a later
         * anchor moves them deliberately. The security's dates come from the
         * anchor and its original term alone, so every trade of it states the
         * same ones; the trade date is the only date that belongs to the trade.
         */
        terms.issue_date = add_months(scope.as_of_date, -security_age_months);
        terms.maturity_date = add_years(terms.issue_date, static_cast<int>(terms.tenor_years));

        auto entry = common_slot(scope, slot);
        entry.trade_type_code = std::string(bond_types[slot % bond_type_count]);
        entry.notional = notional_from(scope, draw(name, 1));
        /*
         * The bond family states its row and its product's own facts: the code
         * decides which constructor runs, and what a code cannot state it does
         * not fill.
         */
        auto product = construct_bond_product(
            terms, entry.trade_type_code, name, slot, scope, entry.notional);
        state_bond_security(terms, entry.currency, entry, product);
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


/*
 * The commodity family states its own rows. The mapping from what the planner
 * draws to each product's constructor lives here, beside the drawing, and not
 * in the writer: the plan carries the rows and the writer persists them.
 */
ores::trading::domain::instrument_batch
construct_commodity_batch(const std::string& trade_type_code,
                          const domain::commodity_terms& drawn,
                          const std::string& currency) {
    namespace tdom = ores::trading::domain;

    ores::trading::domain::instrument_batch batch;
    const auto money = [](std::optional<double> value) -> std::optional<tdom::decimal> {
        if (!value.has_value())
            return std::nullopt;
        return tdom::decimal::from_double(*value).value();
    };

    tdom::commodity_common_terms common;
    common.commodity_code = drawn.commodity_code;
    common.currency = currency;
    common.quantity = drawn.quantity;
    common.unit = drawn.unit;
    common.start_date = drawn.start_date;
    common.maturity_date = drawn.maturity_date;

    const auto& type = trade_type_code;
    auto basket_members = std::vector<tdom::commodity_basket_member_terms>{};
    for (const auto& member : drawn.basket) {
        tdom::commodity_basket_member_terms terms{member.code, member.weight};
        basket_members.push_back(terms);
    }
    auto built = [&]() -> tdom::commodity_basket_product {
        if (type == "CommodityForward" || type == "IntradayPowerForward" ||
            type == "CommodityForwardVolatilityAgreement" || type == "CommodityPosition") {
            tdom::commodity_forward_terms terms{common, money(drawn.fixed_price)};
            if (type == "CommodityForward")
                return {tdom::construct_commodity_forward(terms), {}};
            if (type == "IntradayPowerForward")
                return {tdom::construct_intraday_power_forward(terms), {}};
            if (type == "CommodityForwardVolatilityAgreement")
                return {tdom::construct_commodity_forward_volatility_agreement(terms), {}};
            return {tdom::construct_commodity_position(terms), {}};
        }
        if (type == "CommodityOption" || type == "CommodityDigitalOption" ||
            type == "CommodityStrikeResettableOption") {
            tdom::commodity_option_terms terms{common,
                                               drawn.option_type,
                                               money(drawn.strike_price).value_or(tdom::decimal{}),
                                               drawn.exercise_type};
            if (type == "CommodityOption")
                return {tdom::construct_commodity_option(terms), {}};
            if (type == "CommodityDigitalOption")
                return {tdom::construct_commodity_digital_option(terms), {}};
            return {tdom::construct_commodity_strike_resettable_option(terms), {}};
        }
        if (type == "CommodityAsianOption") {
            tdom::commodity_asian_option_terms terms{
                common,
                drawn.option_type,
                money(drawn.strike_price).value_or(tdom::decimal{}),
                drawn.average_type,
                drawn.averaging_start_date,
                drawn.averaging_end_date};
            return {tdom::construct_commodity_asian_option(terms), {}};
        }
        if (type == "CommodityAveragePriceOption" || type == "CommodityDigitalAveragePriceOption") {
            tdom::commodity_average_price_option_terms terms{
                common,
                drawn.option_type,
                money(drawn.strike_price).value_or(tdom::decimal{}),
                drawn.average_type,
                drawn.averaging_start_date,
                drawn.averaging_end_date};
            if (type == "CommodityAveragePriceOption")
                return {tdom::construct_commodity_average_price_option(terms), {}};
            return {tdom::construct_commodity_digital_average_price_option(terms), {}};
        }
        if (type == "CommodityOptionStrip") {
            tdom::commodity_option_strip_terms terms{
                common,
                drawn.option_type,
                money(drawn.strike_price).value_or(tdom::decimal{}),
                drawn.average_type,
                drawn.averaging_start_date,
                drawn.averaging_end_date,
                drawn.strip_frequency_code};
            return {tdom::construct_commodity_option_strip(terms), {}};
        }
        if (type == "CommodityWindowBarrierOption" || type == "CommodityGenericBarrierOption") {
            tdom::commodity_barrier_option_terms terms{
                common,
                drawn.option_type,
                money(drawn.strike_price).value_or(tdom::decimal{}),
                drawn.barrier_type,
                money(drawn.lower_barrier).value_or(tdom::decimal{}),
                money(drawn.upper_barrier)};
            if (type == "CommodityWindowBarrierOption")
                return {tdom::construct_commodity_window_barrier_option(terms), {}};
            return {tdom::construct_commodity_generic_barrier_option(terms), {}};
        }
        if (type == "CommodityAccumulator" || type == "CommodityTaRF") {
            tdom::commodity_accumulation_terms terms{
                common,
                drawn.option_type,
                money(drawn.accumulation_amount).value_or(tdom::decimal{}),
                money(drawn.knock_out_barrier)};
            if (type == "CommodityAccumulator")
                return {tdom::construct_commodity_accumulator(terms), {}};
            return {tdom::construct_commodity_target_redemption_forward(terms), {}};
        }
        if (type == "CommoditySpreadOption") {
            tdom::commodity_spread_option_terms terms{
                common,
                drawn.option_type,
                money(drawn.strike_price).value_or(tdom::decimal{}),
                drawn.spread_commodity_code,
                money(drawn.spread_amount).value_or(tdom::decimal{})};
            return {tdom::construct_commodity_spread_option(terms), {}};
        }
        if (type == "CommodityVarianceSwap" || type == "CommodityPairwiseVarianceSwap" ||
            type == "CommodityBasketVarianceSwap") {
            tdom::commodity_variance_swap_terms terms{common, drawn.variance_strike.value_or(0.0)};
            if (type == "CommodityVarianceSwap")
                return {tdom::construct_commodity_variance_swap(terms), {}};
            if (type == "CommodityPairwiseVarianceSwap")
                return {tdom::construct_commodity_pairwise_variance_swap(terms), {}};
            return {tdom::construct_commodity_basket_variance_swap(terms), {}};
        }
        if (type == "CommoditySwap") {
            tdom::commodity_swap_terms terms{common, money(drawn.fixed_price)};
            return {tdom::construct_commodity_swap(terms), {}};
        }
        if (type == "CommodityWorstOfBasketSwap") {
            tdom::commodity_worst_of_basket_swap_terms terms{
                common, money(drawn.fixed_price), basket_members};
            return tdom::construct_commodity_worst_of_basket_swap(terms);
        }
        if (type == "CommodityBasketOption" || type == "CommodityRainbowOption" ||
            type == "CommodityBestEntryOption") {
            tdom::commodity_basket_option_terms terms{
                common,
                drawn.option_type,
                money(drawn.strike_price).value_or(tdom::decimal{}),
                drawn.exercise_type,
                basket_members};
            if (type == "CommodityBasketOption")
                return tdom::construct_commodity_basket_option(terms);
            if (type == "CommodityRainbowOption")
                return tdom::construct_commodity_rainbow_option(terms);
            return tdom::construct_commodity_best_entry_option(terms);
        }
        if (type == "CommoditySwaption") {
            tdom::commodity_swaption_terms terms{
                common,
                drawn.option_type,
                money(drawn.strike_price).value_or(tdom::decimal{}),
                drawn.swaption_expiry_date};
            return {tdom::construct_commodity_swaption(terms), {}};
        }
        throw trade_population_error("The commodity trade type " + type +
                                     " states no product this planner knows.");
    }();
    batch.commodity_instruments.push_back(built.row);
    for (const auto& constituent : built.constituents)
        batch.commodity_basket_constituents.push_back(constituent);
    return batch;
}

struct commodity_type final {
    std::string_view code;
    commodity_shape shape;
};

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
    {"CommodityWindowBarrierOption", commodity_shape::barrier_option},
    {"CommodityGenericBarrierOption", commodity_shape::barrier_option},
    {"CommodityAccumulator", commodity_shape::accumulator},
    {"CommodityTaRF", commodity_shape::target_redemption},
    {"CommoditySpreadOption", commodity_shape::spread_option},
    {"CommodityVarianceSwap", commodity_shape::variance_swap},
    {"CommodityPairwiseVarianceSwap", commodity_shape::variance_swap},
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

    for (std::uint32_t slot = 0; slot < slot_count(scope); ++slot) {
        const auto name = slot_name(scope, slot);

        /*
         * The type cycles through the catalogue rather than being drawn, so a
         * population long enough to reach a type always states it. A draw
         * would leave the rare ones out of a small population and the
         * coverage claim could not be tested.
         */
        const auto& type = commodity_types[slot % commodity_type_count];
        const auto& commodity = scope.commodities[slot % scope.commodities.size()];

        const auto span = max_years - min_years + 1;
        const auto drawn =
            min_years + static_cast<std::uint32_t>(draw(name, 0) * static_cast<double>(span));
        const auto tenor_years = std::clamp(drawn, min_years, max_years);

        domain::commodity_terms terms;
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
                        terms.basket.push_back(
                            domain::commodity_basket_member{drawn_member.code, 1.0 / 3.0});
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
                terms.option_type = "Call";
                terms.strike_price = strike;
                terms.barrier_type = "DownAndOut";
                terms.lower_barrier = std::round(price * 0.8 * 100.0) / 100.0;
                break;
            case commodity_shape::accumulator:
            case commodity_shape::target_redemption:
                terms.option_type = "Call";
                terms.accumulation_amount = std::round(terms.quantity / 12.0 * 100.0) / 100.0;
                terms.knock_out_barrier = std::round(price * 1.1 * 100.0) / 100.0;
                break;
            case commodity_shape::spread_option:
                terms.option_type = "Call";
                terms.strike_price = strike;
                terms.spread_commodity_code =
                    scope.commodities[(slot + 1) % scope.commodities.size()].code;
                terms.spread_amount = price_from(name, 7, 1.0, 20.0);
                break;
            case commodity_shape::variance_swap:
                terms.variance_strike = price_from(name, 8, 0.01, 0.2);
                break;
            case commodity_shape::swap:
                terms.fixed_price = price;
                if (type.code == "CommodityWorstOfBasketSwap") {
                    for (std::uint32_t member = 0; member < 3; ++member) {
                        const auto& drawn_member =
                            scope.commodities[(slot + member) % scope.commodities.size()];
                        terms.basket.push_back(
                            domain::commodity_basket_member{drawn_member.code, 1.0 / 3.0});
                    }
                }
                break;
            case commodity_shape::swaption:
                terms.option_type = "Call";
                terms.strike_price = strike;
                terms.swaption_expiry_date = add_months(terms.start_date, 6);
                break;
        }

        auto entry = common_slot(scope, slot);
        entry.trade_type_code = std::string(type.code);
        entry.notional =
            price_from(name, 1, scope.min_notional, scope.max_notional - scope.min_notional);
        /*
         * The commodity family states its rows here, where it draws them, and
         * the plan carries no bag for it.
         */
        entry.instruments = construct_commodity_batch(entry.trade_type_code, terms, entry.currency);

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
                            std::uint32_t tenor_years,
                            std::string_view product) {
    /*
     * A security is named by its currency, the anchor, its original term and the
     * product it is. The product is part of the name because the security's own
     * rows are: a callable bond states the dates its issuer may call it and a
     * plain bond of the same term states none, so the two are different
     * securities and cannot share an issue. Nothing about a book or a trade
     * enters the name, so any book that keeps the currency may hold it and two
     * trades of it name one issue.
     */
    const auto digest = named(std::format("bond:{}:{}:{}Y:{}",
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

boost::uuids::uuid issuer_id_for(const domain::trade_generation_scope& scope,
                                 std::uint32_t tenor_years) {
    if (scope.issuer_ids.empty())
        throw trade_population_error("An issuer needs the tenant's counterparties.");
    auto issuers = scope.issuer_ids;
    std::ranges::sort(issuers);
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
                                           "KnockOutSwap"};
constexpr std::size_t rate_type_count = sizeof(rate_types) / sizeof(rate_types[0]);

/*
 * The day counts a fixed leg cycles through. A leg that states a code the
 * store does not hold is refused, so these are reference data codes and not a
 * vocabulary of the generator's own.
 */
constexpr std::string_view fixed_day_counts[] = {"30/360", "ActAct(ISDA)", "A360"};
constexpr std::size_t fixed_day_count_count =
    sizeof(fixed_day_counts) / sizeof(fixed_day_counts[0]);

}


namespace {

/*
 * The index a slot resets on. A catalogue code carries its currency as a
 * prefix, such as =GBP-SONIA=, so a population denominated in one currency
 * picks from the indices of that currency rather than from every currency the
 * tenant holds. A set with no index of the scope's currency falls back to the
 * whole catalogue, because a leg in a currency with no index of its own is
 * still better stated than refused.
 */
/*
 * The index a cross currency swap resets on: one of a currency other than the
 * book's, because two legs in one currency are not a cross currency swap. A
 * catalogue with no such index falls back to the whole set, and the leg then
 * states the book's currency rather than pretending to be cross currency.
 */
const std::string* foreign_index_for(const domain::trade_generation_scope& scope,
                                     std::uint32_t slot) {
    const auto own = std::string(scope.currency_code) + "-";
    std::vector<const std::string*> foreign;
    for (const auto& code : scope.floating_index_codes) {
        if (!code.starts_with(own))
            foreign.push_back(&code);
    }
    if (foreign.empty())
        return nullptr;
    return foreign[slot % foreign.size()];
}

const std::string& index_for(const domain::trade_generation_scope& scope, std::uint32_t slot) {
    const auto own = std::string(scope.currency_code) + "-";
    std::vector<const std::string*> matching;
    for (const auto& code : scope.floating_index_codes) {
        if (code.starts_with(own))
            matching.push_back(&code);
    }
    if (!matching.empty())
        return *matching[slot % matching.size()];
    return scope.floating_index_codes[slot % scope.floating_index_codes.size()];
}

/*
 * The rate family states its own rows. The mapping from what the planner draws
 * to each product's constructor lives here, beside the drawing, and not in the
 * writer: the plan carries the rows and the writer persists them.
 */
ores::trading::domain::instrument_batch
construct_rate_batch(const domain::rate_terms& drawn,
                     const std::string& trade_type_code,
                     std::uint32_t slot,
                     const std::string& internal_identifier) {
    namespace tdom = ores::trading::domain;

    const auto money = [](double value) {
        return tdom::decimal::from_double(value).value();
    };

    tdom::rate_common_terms common;
    common.start_date = drawn.start_date;
    common.maturity_date = drawn.maturity_date;
    common.fixed.leg_type_code = "Fixed";
    common.fixed.day_count_fraction_code = drawn.fixed_day_count_fraction_code;
    common.fixed.business_day_convention_code = drawn.fixed_business_day_convention_code;
    common.fixed.payment_frequency_code = drawn.fixed_payment_frequency_code;
    common.fixed.currency = drawn.currency;
    common.fixed.rate_role = "fixed";
    common.fixed.value = money(drawn.fixed_rate);
    common.fixed.notional = money(drawn.notional);
    common.fixed.payer = true;
    common.floating.leg_type_code = "Floating";
    common.floating.day_count_fraction_code = drawn.floating_day_count_fraction_code;
    common.floating.business_day_convention_code = drawn.floating_business_day_convention_code;
    common.floating.payment_frequency_code = drawn.floating_payment_frequency_code;
    common.floating.floating_index_code = drawn.floating_index_code;
    /*
     * A leg without a currency of its own is in the trade's, which is what a
     * single-currency rate product states.
     */
    common.floating.currency =
        drawn.floating_currency.empty() ? drawn.currency : drawn.floating_currency;
    common.floating.rate_role = "spread";
    common.floating.value = money(drawn.spread);
    common.floating.notional = money(drawn.notional);
    common.floating.payer = false;

    /*
     * The direction alternates with the slot so a population states both sides
     * of the book rather than every trade the same way round.
     */
    const auto direction = std::string(slot % 2 == 0 ? "Long" : "Short");
    const auto& type = trade_type_code;
    auto product = [&]() -> tdom::rate_product {
        if (type == "Swap")
            return tdom::construct_rate_swap(tdom::rate_swap_terms{common, 2});
        if (type == "CrossCurrencySwap")
            return tdom::construct_rate_cross_currency_swap(tdom::rate_swap_terms{common, 2});
        if (type == "ForwardRateAgreement")
            return tdom::construct_rate_fra(tdom::rate_fra_terms{common, direction});
        if (type == "CapFloor")
            return tdom::construct_rate_cap_floor(
                tdom::rate_cap_floor_terms{common, money(drawn.strike.value_or(0.0))});
        if (type == "Swaption") {
            tdom::rate_swaption_terms terms{common,
                                            drawn.expiry_date.value_or(drawn.start_date),
                                            direction,
                                            "European",
                                            "Cash"};
            return tdom::construct_rate_swaption(terms);
        }
        if (type == "CallableSwap")
            return tdom::construct_rate_callable_swap(tdom::rate_callable_swap_terms{common});
        if (type == "KnockOutSwap")
            return tdom::construct_rate_knock_out_swap(
                tdom::rate_knock_out_swap_terms{common, drawn.start_date, "UpAndOut"});
        if (type == "FlexiSwap")
            return tdom::construct_rate_flexi_swap(tdom::rate_flexi_swap_terms{common, direction});
        if (type == "BalanceGuaranteedSwap")
            return tdom::construct_rate_balance_guaranteed_swap(
                tdom::rate_balance_guaranteed_swap_terms{common, internal_identifier});
        throw trade_population_error("The rate trade type " + type +
                                     " states no product this planner knows.");
    }();

    tdom::instrument_batch batch;
    batch.rate_instruments.push_back(product.row);
    for (const auto& leg : product.legs)
        batch.swap_legs.push_back(leg);
    for (const auto& amount : product.amounts)
        batch.swap_leg_amounts.push_back(amount);
    for (const auto& leg_rate : product.rates)
        batch.swap_leg_rates.push_back(leg_rate);
    for (const auto& strike : product.strikes)
        batch.instrument_strikes.push_back(strike);
    std::visit(
        [&batch](const auto& fact) {
            using fact_type = std::decay_t<decltype(fact)>;
            if constexpr (std::is_same_v<fact_type, tdom::vanilla_swap_instrument>)
                batch.vanilla_swap_instruments.push_back(fact);
            else if constexpr (std::is_same_v<fact_type, tdom::fra_instrument>)
                batch.fra_instruments.push_back(fact);
            else if constexpr (std::is_same_v<fact_type, tdom::cap_floor_instrument>)
                batch.cap_floor_instruments.push_back(fact);
            else if constexpr (std::is_same_v<fact_type, tdom::swaption_instrument>)
                batch.swaption_instruments.push_back(fact);
            else if constexpr (std::is_same_v<fact_type, tdom::callable_swap_instrument>)
                batch.callable_swap_instruments.push_back(fact);
            else if constexpr (std::is_same_v<fact_type, tdom::knock_out_swap_instrument>)
                batch.knock_out_swap_instruments.push_back(fact);
            else if constexpr (std::is_same_v<fact_type, tdom::flexi_swap_instrument>)
                batch.flexi_swap_instruments.push_back(fact);
            else
                batch.balance_guaranteed_swap_instruments.push_back(fact);
        },
        product.fact);
    return batch;
}

}

std::string_view rate_trade_planner::asset_class_code() const {
    return "interest_rates";
}

domain::trade_population_plan
rate_trade_planner::plan(const domain::trade_generation_scope& scope) const {
    validate_scope(scope);

    if (scope.floating_index_codes.empty())
        throw trade_population_error(
            "The tenant holds no benchmark rate, so no floating leg can name an index. Publish "
            "the benchmark rate catalogue first.");

    const auto min_years = tenor_years_of(scope.min_tenor_code);
    const auto max_years = tenor_years_of(scope.max_tenor_code);

    domain::trade_population_plan plan;
    plan.config_id = scope.config_id;
    plan.party_id = scope.party_id;
    plan.as_of_date = scope.as_of_date;

    for (std::uint32_t slot = 0; slot < slot_count(scope); ++slot) {
        const auto name = slot_name(scope, slot);

        /*
         * The type cycles rather than being drawn, so a population long enough
         * to reach a type always states it and the coverage claim is testable.
         */
        const auto& type = rate_types[slot % rate_type_count];
        const auto* foreign =
            (type == "CrossCurrencySwap") ? foreign_index_for(scope, slot) : nullptr;
        const auto& index = foreign != nullptr ? *foreign : index_for(scope, slot);

        const auto span = max_years - min_years + 1;
        const auto drawn =
            min_years + static_cast<std::uint32_t>(draw(name, 0) * static_cast<double>(span));
        auto tenor_years = std::clamp(drawn, min_years, max_years);

        /*
         * A forward rate agreement is a short-dated contract: a ten year one is
         * not the product, so the band is capped here rather than left to a
         * config that states one band for the whole family.
         */
        if (type == "ForwardRateAgreement")
            tenor_years = std::min<std::uint32_t>(tenor_years, 1U);

        domain::rate_terms terms;
        terms.currency = scope.currency_code;
        terms.start_date = add_months(scope.as_of_date, -static_cast<int>(slot % 6));
        terms.maturity_date = add_years(terms.start_date, static_cast<int>(tenor_years));
        terms.notional =
            price_from(name, 1, scope.min_notional, scope.max_notional - scope.min_notional);
        /*
         * A rate is a decimal in the store, not a percentage: one per cent to
         * six per cent, quantised to a ten-thousandth so a decimal column holds
         * it exactly.
         */
        terms.fixed_rate = std::round((0.01 + 0.05 * draw(name, 2)) * 10000.0) / 10000.0;
        terms.fixed_day_count_fraction_code =
            std::string(fixed_day_counts[slot % fixed_day_count_count]);
        terms.fixed_business_day_convention_code = "ModifiedFollowing";
        terms.fixed_payment_frequency_code = "Annual";
        terms.floating_index_code = index;
        terms.spread = std::round(draw(name, 3) * 50.0) / 10000.0;
        if (foreign != nullptr)
            terms.floating_currency = foreign->substr(0, foreign->find('-'));
        if (type == "CapFloor")
            terms.strike = std::round((0.01 + 0.05 * draw(name, 4)) * 10000.0) / 10000.0;
        if (type == "Swaption") {
            /*
             * An option is exercised after the population is written and
             * before the swap it opens matures, so its expiry sits a year in.
             */
            terms.expiry_date = add_years(terms.start_date, 1);
            terms.strike = terms.fixed_rate;
        }
        terms.floating_day_count_fraction_code = "ActAct(ISDA)";
        terms.floating_business_day_convention_code = "ModifiedFollowing";
        terms.floating_payment_frequency_code = "Semiannual";

        auto entry = common_slot(scope, slot);
        entry.trade_type_code = std::string(type);
        entry.notional = terms.notional;
        /*
         * The rate family states its rows here, where it draws them, and the
         * plan carries no bag for it.
         */
        entry.instruments =
            construct_rate_batch(terms, entry.trade_type_code, slot, entry.internal_identifier);

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
                                         "FxTaRF"};
constexpr std::size_t fx_type_count = sizeof(fx_types) / sizeof(fx_types[0]);

/*
 * The pair a slot trades. A desk books what it quotes, so the pairs that name
 * the book's own currency come first; a tenant holding none of those trades the
 * whole catalogue rather than refusing, because a pair in another currency is
 * still a pair it holds.
 */
const domain::currency_pair_reference& pair_for(const domain::trade_generation_scope& scope,
                                                std::uint32_t slot) {
    const auto& own = scope.currency_code;
    std::vector<const domain::currency_pair_reference*> matching;
    for (const auto& pair : scope.currency_pairs) {
        if (pair.base == own || pair.quote == own)
            matching.push_back(&pair);
    }
    if (!matching.empty())
        return *matching[slot % matching.size()];
    return scope.currency_pairs[slot % scope.currency_pairs.size()];
}

std::string pair_name(const domain::currency_pair_reference& pair) {
    return pair.base + "/" + pair.quote;
}

}

std::string_view fx_trade_planner::asset_class_code() const {
    return "fx";
}

/*
 * The fx family states its own rows. The mapping from what the planner draws to
 * each product's constructor lives here, beside the drawing, and not in the
 * writer: the plan carries the rows and the writer persists them.
 */
ores::trading::domain::instrument_batch construct_fx_batch(const domain::fx_terms& drawn,
                                                           const std::string& trade_type_code) {
    namespace tdom = ores::trading::domain;
    ores::trading::domain::instrument_batch batch;
    const auto money = [](double value) {
        return tdom::decimal::from_double(value).value();
    };
    const auto maybe_money = [&money](std::optional<double> value) -> std::optional<tdom::decimal> {
        if (!value.has_value())
            return std::nullopt;
        return money(*value);
    };
    const auto& type = trade_type_code;

    tdom::fx_legs_terms legs;
    legs.bought_currency = drawn.bought_currency;
    legs.bought_amount = money(drawn.bought_amount);
    legs.sold_currency = drawn.sold_currency;
    legs.sold_amount = money(drawn.sold_amount);

    /*
     * One routed table per shape. An FX row is the whole instrument, so there
     * is no fact row beside it and nothing to forget: the fields the table
     * states are the fields the row carries.
     */
    /*
     * One routed table per shape. An FX row is the whole instrument, so
     * there is no fact row beside it and nothing to forget: the fields the
     * table states are the fields the row carries.
     */
    if (type == "FxForward" || type == "FxSwap") {
        tdom::fx_forward_terms terms{legs, drawn.value_date, drawn.settlement};
        auto row = type == "FxForward" ? tdom::construct_fx_forward(terms) :
                                         tdom::construct_fx_swap(terms);
        row.description = "Generated FX trade.";
        batch.fx_forward_instruments.push_back(row);
    } else if (type == "FxOption") {
        tdom::fx_vanilla_option_terms terms{
            legs, drawn.option_type, drawn.expiry_date, drawn.exercise_style, drawn.settlement};
        auto row = tdom::construct_fx_option(terms);
        row.description = "Generated FX trade.";
        batch.fx_vanilla_option_instruments.push_back(row);
    } else if (type.starts_with("FxBarrier") || type == "FxDoubleBarrierOption" ||
               type == "FxEuropeanBarrierOption" || type == "FxGenericBarrierOption" ||
               type == "FxKIKOBarrierOption") {
        tdom::fx_barrier_option_terms terms{legs,
                                            drawn.option_type,
                                            drawn.expiry_date,
                                            drawn.settlement,
                                            drawn.barrier_type,
                                            maybe_money(drawn.lower_barrier),
                                            maybe_money(drawn.upper_barrier),
                                            drawn.underlying_code};
        auto row = [&] {
            if (type == "FxBarrierOption")
                return tdom::construct_fx_barrier_option(terms);
            if (type == "FxDoubleBarrierOption")
                return tdom::construct_fx_double_barrier_option(terms);
            if (type == "FxEuropeanBarrierOption")
                return tdom::construct_fx_european_barrier_option(terms);
            if (type == "FxGenericBarrierOption")
                return tdom::construct_fx_generic_barrier_option(terms);
            return tdom::construct_fx_kiko_barrier_option(terms);
        }();
        row.description = "Generated FX trade.";
        batch.fx_barrier_option_instruments.push_back(row);
    } else if (type == "FxDigitalOption" || type == "FxTouchOption" ||
               type == "FxDoubleTouchOption" || type == "FxDigitalBarrierOption") {
        tdom::fx_digital_option_terms terms{drawn.foreign_currency,
                                            drawn.domestic_currency,
                                            drawn.payoff_currency,
                                            money(drawn.payoff_amount.value_or(0.0)),
                                            drawn.option_type,
                                            drawn.expiry_date,
                                            drawn.long_short,
                                            maybe_money(drawn.strike),
                                            drawn.barrier_type,
                                            maybe_money(drawn.lower_barrier),
                                            maybe_money(drawn.upper_barrier)};
        auto row = [&] {
            if (type == "FxDigitalOption")
                return tdom::construct_fx_digital_option(terms);
            if (type == "FxTouchOption")
                return tdom::construct_fx_touch_option(terms);
            if (type == "FxDoubleTouchOption")
                return tdom::construct_fx_double_touch_option(terms);
            return tdom::construct_fx_digital_barrier_option(terms);
        }();
        row.description = "Generated FX trade.";
        batch.fx_digital_option_instruments.push_back(row);
    } else if (type == "FxAverageForward") {
        tdom::fx_asian_forward_terms terms{drawn.fx_index,
                                           drawn.reference_currency,
                                           maybe_money(drawn.reference_notional),
                                           drawn.settlement_currency,
                                           maybe_money(drawn.settlement_notional),
                                           drawn.payment_date,
                                           drawn.long_short,
                                           drawn.currency,
                                           maybe_money(drawn.fixing_amount),
                                           maybe_money(drawn.strike)};
        auto row = tdom::construct_fx_average_forward(terms);
        row.description = "Generated FX trade.";
        batch.fx_asian_forward_instruments.push_back(row);
    } else if (type == "FxTaRF") {
        tdom::fx_tarf_terms terms{drawn.fx_index,
                                  drawn.reference_currency,
                                  maybe_money(drawn.reference_notional),
                                  drawn.settlement_currency,
                                  maybe_money(drawn.settlement_notional),
                                  drawn.payment_date,
                                  drawn.long_short,
                                  drawn.currency,
                                  maybe_money(drawn.fixing_amount),
                                  maybe_money(drawn.strike),
                                  money(drawn.target_amount.value_or(0.0))};
        auto row = tdom::construct_fx_target_redemption_forward(terms);
        row.description = "Generated FX trade.";
        batch.fx_asian_forward_instruments.push_back(row);
    } else if (type == "FxVarianceSwap") {
        tdom::fx_variance_swap_terms terms{drawn.start_date,
                                           drawn.end_date,
                                           drawn.currency,
                                           drawn.underlying_code,
                                           drawn.long_short,
                                           drawn.strike.value_or(0.0),
                                           money(drawn.reference_notional),
                                           drawn.moment_type};
        auto row = tdom::construct_fx_variance_swap(terms);
        row.description = "Generated FX trade.";
        batch.fx_variance_swap_instruments.push_back(row);
    } else if (type == "FxAccumulator") {
        tdom::fx_accumulator_terms terms{drawn.currency,
                                         money(drawn.fixing_amount),
                                         drawn.underlying_code,
                                         drawn.long_short,
                                         drawn.start_date,
                                         money(drawn.strike.value_or(0.0)),
                                         maybe_money(drawn.knock_out_barrier)};
        auto built = tdom::construct_fx_accumulator(terms);
        built.row.description = "Generated FX trade.";
        batch.fx_accumulator_instruments.push_back(built.row);
        for (const auto& bound : built.range_bounds)
            batch.range_bounds.push_back(bound);
    } else {
        throw trade_population_error("The FX trade type " + type +
                                     " states no product this planner knows.");
    }
    return batch;
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

    for (std::uint32_t slot = 0; slot < slot_count(scope); ++slot) {
        const auto name = slot_name(scope, slot);
        const auto& type = fx_types[slot % fx_type_count];
        const auto& pair = pair_for(scope, slot);

        const auto span = max_years - min_years + 1;
        const auto drawn =
            min_years + static_cast<std::uint32_t>(draw(name, 0) * static_cast<double>(span));
        const auto tenor_years = std::clamp(drawn, min_years, max_years);
        const auto start = add_months(scope.as_of_date, -static_cast<int>(slot % 6));
        const auto end = add_years(start, static_cast<int>(tenor_years));
        const auto notional =
            price_from(name, 1, scope.min_notional, scope.max_notional - scope.min_notional);
        const auto rate = std::round((0.5 + draw(name, 2)) * 10000.0) / 10000.0;

        domain::fx_terms terms;
        terms.settlement = "Cash";
        terms.value_date = end;
        terms.expiry_date = end;
        terms.start_date = start;
        terms.end_date = end;
        /*
         * An fx config may state a tenor band whose short end is less than a
         * year, and a variance swap is the one product that requires its end to
         * be after its start. A zero-length observation window is not the
         * product, so a short draw states the shortest window that is one.
         */
        if (type == "FxVarianceSwap" && !(terms.end_date > terms.start_date))
            terms.end_date = add_months(terms.start_date, 1);

        /*
         * The three idioms. A bought-and-sold trade states the two currencies
         * and their amounts; a digital states a payoff struck against a
         * foreign and a domestic; an index trade states the index and the
         * reference notional it is written on.
         */
        if (type == "FxForward" || type == "FxSwap" || type == "FxOption" ||
            type.starts_with("FxBarrier") || type == "FxDoubleBarrierOption" ||
            type == "FxEuropeanBarrierOption" || type == "FxGenericBarrierOption" ||
            type == "FxKIKOBarrierOption") {
            terms.bought_currency = pair.base;
            terms.sold_currency = pair.quote;
            terms.sold_amount = notional;
            terms.bought_amount = std::round(notional * rate * 100.0) / 100.0;
        } else if (type == "FxDigitalOption" || type == "FxTouchOption" ||
                   type == "FxDoubleTouchOption" || type == "FxDigitalBarrierOption") {
            terms.foreign_currency = pair.base;
            terms.domestic_currency = pair.quote;
            terms.payoff_currency = pair.quote;
            terms.payoff_amount = notional;
            terms.strike = rate;
        } else {
            terms.fx_index = pair_name(pair);
            terms.reference_currency = pair.base;
            terms.reference_notional = notional;
            terms.settlement_currency = pair.quote;
            terms.settlement_notional = std::round(notional * rate * 100.0) / 100.0;
            terms.currency = pair.quote;
            terms.fixing_amount = notional;
            terms.strike = rate;
            terms.payment_date = end;
        }

        if (type.ends_with("Option") || type.starts_with("FxBarrier") ||
            type.starts_with("FxKIKO") || type.starts_with("FxTouch") ||
            type.starts_with("FxDouble") || type.starts_with("FxEuropean") ||
            type.starts_with("FxGeneric") || type.starts_with("FxDigital")) {
            terms.option_type = (draw(name, 3) < 0.5) ? "Call" : "Put";
            terms.exercise_style = "European";
            terms.long_short = (draw(name, 4) < 0.5) ? "Long" : "Short";
        }

        if (type.find("Barrier") != std::string::npos || type.starts_with("FxKIKO") ||
            type.starts_with("FxTouch") || type.starts_with("FxDouble")) {
            terms.barrier_type = type.starts_with("FxKIKO") ? "KnockIn" : "KnockOut";
            terms.lower_barrier = std::round(rate * 0.9 * 10000.0) / 10000.0;
            terms.upper_barrier = std::round(rate * 1.1 * 10000.0) / 10000.0;
            terms.underlying_code = pair_name(pair);
        }

        if (type == "FxVarianceSwap") {
            terms.moment_type = "Variance";
            terms.long_short = (draw(name, 5) < 0.5) ? "Long" : "Short";
            terms.underlying_code = pair_name(pair);
        }

        if (type == "FxAccumulator") {
            terms.knock_out_barrier = std::round(rate * 1.1 * 10000.0) / 10000.0;
            terms.underlying_code = pair_name(pair);
            terms.long_short = "Long";
        }

        if (type == "FxTaRF" || type == "FxAverageForward") {
            terms.target_amount = std::round(notional * 2.0 * 100.0) / 100.0;
            terms.long_short = "Long";
        }

        auto entry = common_slot(scope, slot);
        entry.trade_type_code = std::string(type);
        entry.notional = notional;
        /*
         * The fx family states its rows here, where it draws them, and the plan
         * carries no bag for it.
         */
        entry.instruments = construct_fx_batch(terms, entry.trade_type_code);
        plan.slots.push_back(std::move(entry));
    }

    return plan;
}


std::string_view equity_trade_planner::asset_class_code() const {
    return "equity";
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
                                             "RiskParticipationAgreement"};

constexpr std::size_t credit_type_count = std::size(credit_types);

std::string_view credit_trade_planner::asset_class_code() const {
    return "credit";
}

/*
 * The credit family states its own row. The mapping from what the planner draws
 * to each product's constructor lives here, beside the drawing, and not in the
 * writer: the plan carries the row and the writer persists it.
 */
ores::trading::domain::credit_instrument construct_credit_row(const domain::credit_terms& drawn,
                                                              const std::string& trade_type_code) {
    namespace tdom = ores::trading::domain;
    tdom::credit_common_terms terms;
    terms.trade_type_code = trade_type_code;
    terms.reference_entity_id_type = drawn.reference_entity_id_type;
    terms.reference_entity_id = drawn.reference_entity_id;
    terms.start_date = drawn.start_date;
    terms.maturity_date = drawn.maturity_date;

    const auto& type = trade_type_code;
    auto row = [&] {
        if (type == "CreditDefaultSwap")
            return tdom::construct_credit_default_swap(terms);
        if (type == "IndexCreditDefaultSwap")
            return tdom::construct_index_credit_default_swap(terms);
        if (type == "IndexCreditDefaultSwapOption")
            return tdom::construct_index_credit_default_swap_option(terms);
        if (type == "CreditLinkedSwap")
            return tdom::construct_credit_linked_swap(terms);
        if (type == "RiskParticipationAgreement")
            return tdom::construct_risk_participation_agreement(terms);
        return tdom::construct_synthetic_cdo(terms);
    }();
    row.description = "Generated credit trade.";
    return row;
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

    for (std::uint32_t slot = 0; slot < slot_count(scope); ++slot) {
        const auto name = slot_name(scope, slot);

        const auto span = max_years - min_years + 1;
        const auto drawn =
            min_years + static_cast<std::uint32_t>(draw(name, 0) * static_cast<double>(span));

        domain::credit_terms terms;
        terms.reference_entity_id_type = "LEI";
        terms.reference_entity_id =
            scope.credit_reference_entities[slot % scope.credit_reference_entities.size()];
        terms.start_date = add_months(scope.as_of_date, -static_cast<int>(slot % 6));
        terms.maturity_date =
            add_years(terms.start_date, static_cast<int>(std::clamp(drawn, min_years, max_years)));

        auto entry = common_slot(scope, slot);
        entry.trade_type_code = std::string(credit_types[slot % credit_type_count]);
        entry.notional = notional_from(scope, draw(name, 1));
        /*
         * The credit family states its row here, where it draws it, and the
         * plan carries no bag for it.
         */
        entry.instruments.credit_instruments.push_back(
            construct_credit_row(terms, entry.trade_type_code));

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
                                             "EquityWorstOfBasketSwap"};
/*
 * EquityOutperformanceOption is routed to the option table and that table's
 * check admits EquityOption and EquityCliquetOption only, so no outperformance
 * row can satisfy it. The type stays out until the check admits it, and the
 * gap is recorded rather than worked around by booking it as an option.
 */

constexpr std::size_t equity_type_count = std::size(equity_types);

/// The underlying the slot draws, cycling the catalogue the tenant published.
const domain::equity_underlying_reference& equity_for(const domain::trade_generation_scope& scope,
                                                      std::uint32_t slot) {
    return scope.equity_underlyings[slot % scope.equity_underlyings.size()];
}

}

/*
 * The equity family states its own rows. The mapping from what the planner
 * draws to each product's constructor lives here, beside the drawing, and not
 * in the writer: the plan carries the rows and the writer persists them.
 */
ores::trading::domain::instrument_batch construct_equity_batch(const domain::equity_terms& drawn,
                                                               const std::string& trade_type_code) {
    namespace tdom = ores::trading::domain;
    ores::trading::domain::instrument_batch batch;
    const auto money = [](double value) {
        return ores::utility::decimal::decimal::from_double(value).value();
    };
    const auto maybe_money =
        [&money](std::optional<double> value) -> std::optional<ores::utility::decimal::decimal> {
        if (!value.has_value())
            return std::nullopt;
        return money(*value);
    };
    const auto& type = trade_type_code;

    /*
     * The routing table puts seventeen codes in nine tables, so a table
     * serves more than one type and the payload states the fields all of
     * them share. The basket is the one product that also states rows: its
     * constituents and its periods are children of the swap.
     */
    if (type == "EquityOption") {
        ores::trading::domain::equity_option_terms terms;
        terms.underlying_name = drawn.underlying_name;
        terms.currency = drawn.currency;
        terms.notional = money(drawn.notional);
        terms.option_type = drawn.option_type;
        terms.strike = money(drawn.strike.value_or(0.0));
        terms.expiry_date = drawn.expiry_date;
        terms.exercise_type = drawn.exercise_type;
        terms.long_short = drawn.long_short;
        terms.settlement_type = drawn.settlement_type;
        auto row = ores::trading::domain::construct_equity_option(terms);
        row.description = "Generated equity trade.";
        batch.equity_option_instruments.push_back(row);
    } else if (type == "EquityCliquetOption") {
        ores::trading::domain::equity_cliquet_option_terms terms;
        terms.underlying_name = drawn.underlying_name;
        terms.currency = drawn.currency;
        terms.notional = money(drawn.notional);
        terms.option_type = drawn.option_type;
        terms.strike = money(drawn.strike.value_or(0.0));
        terms.expiry_date = drawn.expiry_date;
        terms.exercise_type = drawn.exercise_type;
        terms.long_short = drawn.long_short;
        terms.settlement_type = drawn.settlement_type;
        terms.frequency = drawn.cliquet_frequency;
        auto row = ores::trading::domain::construct_equity_cliquet_option(terms);
        row.description = "Generated equity trade.";
        batch.equity_option_instruments.push_back(row);
    } else if (type == "EquityAsianOption") {
        ores::trading::domain::equity_asian_option_terms terms;
        terms.underlying_name = drawn.underlying_name;
        terms.currency = drawn.currency;
        terms.notional = money(drawn.notional);
        terms.option_type = drawn.option_type;
        terms.strike = money(drawn.strike.value_or(0.0));
        terms.expiry_date = drawn.expiry_date;
        terms.exercise_type = drawn.exercise_type;
        terms.long_short = drawn.long_short;
        terms.average_type = drawn.average_type;
        terms.averaging_start_date = drawn.averaging_start_date.value_or(drawn.start_date);
        terms.averaging_end_date = drawn.averaging_end_date.value_or(drawn.expiry_date);
        auto row = ores::trading::domain::construct_equity_asian_option(terms);
        row.description = "Generated equity trade.";
        batch.equity_asian_option_instruments.push_back(row);
    } else if (type == "EquityBarrierOption" || type == "EquityEuropeanBarrierOption") {
        ores::trading::domain::equity_barrier_option_terms terms;
        terms.underlying_name = drawn.underlying_name;
        terms.currency = drawn.currency;
        terms.notional = money(drawn.notional);
        terms.option_type = drawn.option_type;
        terms.strike = money(drawn.strike.value_or(0.0));
        terms.expiry_date = drawn.expiry_date;
        terms.exercise_type = drawn.exercise_type;
        terms.long_short = drawn.long_short;
        terms.lower_barrier = money(drawn.lower_barrier.value_or(0.0));
        terms.lower_barrier_type = drawn.lower_barrier_type;
        terms.rebate = maybe_money(drawn.rebate);
        auto row = type == "EquityEuropeanBarrierOption" ?
                       ores::trading::domain::construct_equity_european_barrier_option(terms) :
                       ores::trading::domain::construct_equity_barrier_option(terms);
        row.description = "Generated equity trade.";
        batch.equity_barrier_option_instruments.push_back(row);
    } else if (type == "EquityDoubleBarrierOption") {
        ores::trading::domain::equity_double_barrier_option_terms terms;
        terms.underlying_name = drawn.underlying_name;
        terms.currency = drawn.currency;
        terms.notional = money(drawn.notional);
        terms.option_type = drawn.option_type;
        terms.strike = money(drawn.strike.value_or(0.0));
        terms.expiry_date = drawn.expiry_date;
        terms.exercise_type = drawn.exercise_type;
        terms.long_short = drawn.long_short;
        terms.lower_barrier = money(drawn.lower_barrier.value_or(0.0));
        terms.lower_barrier_type = drawn.lower_barrier_type;
        terms.upper_barrier = money(drawn.upper_barrier.value_or(0.0));
        terms.upper_barrier_type = drawn.upper_barrier_type;
        terms.rebate = maybe_money(drawn.rebate);
        auto row = ores::trading::domain::construct_equity_double_barrier_option(terms);
        row.description = "Generated equity trade.";
        batch.equity_barrier_option_instruments.push_back(row);
    } else if (type == "EquityDigitalOption") {
        ores::trading::domain::equity_digital_option_terms terms;
        terms.underlying_name = drawn.underlying_name;
        terms.currency = drawn.currency;
        terms.notional = money(drawn.notional);
        terms.option_type = drawn.option_type;
        terms.strike = money(drawn.strike.value_or(0.0));
        terms.expiry_date = drawn.expiry_date;
        terms.long_short = drawn.long_short;
        terms.payout_amount = maybe_money(drawn.payout_amount);
        auto row = ores::trading::domain::construct_equity_digital_option(terms);
        row.description = "Generated equity trade.";
        batch.equity_digital_option_instruments.push_back(row);
    } else if (type == "EquityTouchOption") {
        ores::trading::domain::equity_touch_option_terms terms;
        terms.underlying_name = drawn.underlying_name;
        terms.currency = drawn.currency;
        terms.notional = money(drawn.notional);
        terms.barrier_level = money(drawn.barrier_level.value_or(0.0));
        terms.barrier_type = drawn.barrier_type;
        terms.expiry_date = drawn.expiry_date;
        terms.long_short = drawn.long_short;
        terms.payout_amount = maybe_money(drawn.payout_amount);
        auto row = ores::trading::domain::construct_equity_touch_option(terms);
        row.description = "Generated equity trade.";
        batch.equity_digital_option_instruments.push_back(row);
    } else if (type == "EquityForward") {
        ores::trading::domain::equity_forward_terms terms;
        terms.underlying_name = drawn.underlying_name;
        terms.currency = drawn.currency;
        terms.quantity = drawn.quantity;
        terms.forward_price = maybe_money(drawn.fixed_price);
        terms.expiry_date = drawn.expiry_date;
        terms.long_short = drawn.long_short;
        terms.settlement_type = drawn.settlement_type;
        auto row = ores::trading::domain::construct_equity_forward(terms);
        row.description = "Generated equity trade.";
        batch.equity_forward_instruments.push_back(row);
    } else if (type == "EquitySwap" || type == "EquityWorstOfBasketSwap") {
        const auto return_type = drawn.return_type.empty() ? "Price" : drawn.return_type;
        const auto payment_frequency =
            drawn.payment_frequency_code.empty() ? "Quarterly" : drawn.payment_frequency_code;
        auto basket = std::vector<ores::trading::domain::equity_basket_member_terms>{};
        for (const auto& member : drawn.basket) {
            ores::trading::domain::equity_basket_member_terms terms;
            terms.underlying_name = member.name;
            terms.underlying_kind = member.kind;
            terms.weight = member.weight;
            terms.initial_price = maybe_money(member.initial_price);
            basket.push_back(terms);
        }
        auto periods = std::vector<ores::trading::domain::equity_swap_period_terms>{};
        for (const auto& period : drawn.periods) {
            ores::trading::domain::equity_swap_period_terms terms;
            terms.period_date = period.period_date.value_or(drawn.start_date);
            terms.knock_out_level = period.knock_out_level;
            terms.fixed_trigger_level = period.fixed_trigger_level;
            periods.push_back(terms);
        }
        auto product = [&] {
            if (type == "EquitySwap") {
                ores::trading::domain::equity_swap_terms terms;
                terms.underlying_name = drawn.underlying_name;
                terms.currency = drawn.currency;
                terms.notional = money(drawn.notional);
                terms.return_type = return_type;
                terms.start_date = drawn.start_date;
                terms.maturity_date = drawn.maturity_date;
                terms.long_short = drawn.long_short;
                terms.payment_frequency_code = payment_frequency;
                terms.basket = basket;
                terms.periods = periods;
                return ores::trading::domain::construct_equity_swap(terms);
            }
            ores::trading::domain::equity_basket_swap_terms terms;
            terms.currency = drawn.currency;
            terms.notional = money(drawn.notional);
            terms.return_type = return_type;
            terms.start_date = drawn.start_date;
            terms.maturity_date = drawn.maturity_date;
            terms.long_short = drawn.long_short;
            terms.payment_frequency_code = payment_frequency;
            terms.basket = basket;
            terms.periods = periods;
            return ores::trading::domain::construct_equity_worst_of_basket_swap(terms);
        }();
        product.row.description = "Generated equity trade.";
        batch.equity_swap_instruments.push_back(product.row);
        for (auto& constituent : product.constituents) {
            batch.equity_basket_constituents.push_back(constituent);
        }
        for (const auto& child : product.periods)
            batch.equity_swap_periods.push_back(child);
    } else if (type == "EquityVarianceSwap") {
        ores::trading::domain::equity_variance_swap_terms terms;
        terms.underlying_name = drawn.underlying_name;
        terms.currency = drawn.currency;
        terms.notional = money(drawn.notional);
        terms.variance_strike = drawn.variance_strike.value_or(0.04);
        terms.start_date = drawn.start_date;
        terms.maturity_date = drawn.maturity_date;
        terms.long_short = drawn.long_short;
        auto row = ores::trading::domain::construct_equity_variance_swap(terms);
        row.description = "Generated equity trade.";
        batch.equity_variance_swap_instruments.push_back(row);
    } else if (type == "EquityAccumulator" || type == "EquityTaRF") {
        auto bounds = std::vector<ores::trading::domain::equity_range_bound_terms>{};
        for (const auto& bound : drawn.range_bounds) {
            ores::trading::domain::equity_range_bound_terms terms;
            terms.range_from = maybe_money(bound.range_from);
            terms.range_to = maybe_money(bound.range_to);
            terms.leverage = bound.leverage;
            bounds.push_back(terms);
        }
        auto product = [&] {
            if (type == "EquityAccumulator") {
                ores::trading::domain::equity_accumulator_terms terms;
                terms.underlying_name = drawn.underlying_name;
                terms.currency = drawn.currency;
                terms.strike = money(drawn.strike.value_or(0.0));
                terms.fixing_amount = money(drawn.notional);
                terms.start_date = drawn.start_date;
                terms.expiry_date = drawn.expiry_date;
                terms.fixing_frequency =
                    drawn.fixing_frequency.empty() ? "Daily" : drawn.fixing_frequency;
                terms.long_short = drawn.long_short;
                terms.knock_out_level = maybe_money(drawn.knock_out_level);
                terms.payoff_type = drawn.payoff_type.empty() ? "Accumulator" : drawn.payoff_type;
                terms.range_bounds = bounds;
                return ores::trading::domain::construct_equity_accumulator(terms);
            }
            ores::trading::domain::equity_tarf_terms terms;
            terms.underlying_name = drawn.underlying_name;
            terms.currency = drawn.currency;
            terms.strike = money(drawn.strike.value_or(0.0));
            terms.fixing_amount = money(drawn.notional);
            terms.start_date = drawn.start_date;
            terms.expiry_date = drawn.expiry_date;
            terms.fixing_frequency =
                drawn.fixing_frequency.empty() ? "Daily" : drawn.fixing_frequency;
            terms.long_short = drawn.long_short;
            terms.knock_out_level = maybe_money(drawn.knock_out_level);
            terms.payoff_type = drawn.payoff_type.empty() ? "Accumulator" : drawn.payoff_type;
            terms.target_amount = money(drawn.target_amount.value_or(0.0));
            terms.target_type = drawn.target_type;
            terms.range_bounds = bounds;
            return ores::trading::domain::construct_equity_target_redemption_forward(terms);
        }();
        product.row.description = "Generated equity trade.";
        batch.equity_accumulator_instruments.push_back(product.row);
        for (auto& bound : product.range_bounds)
            batch.range_bounds.push_back(bound);
    } else if (type == "EquityPosition" || type == "EquityOptionPosition") {
        ores::trading::domain::equity_position_terms terms;
        terms.underlying_name = drawn.underlying_name;
        terms.currency = drawn.currency;
        terms.quantity = drawn.quantity;
        terms.price = maybe_money(drawn.fixed_price);
        auto row = type == "EquityOptionPosition" ?
                       ores::trading::domain::construct_equity_option_position(terms) :
                       ores::trading::domain::construct_equity_position(terms);
        row.description = "Generated equity trade.";
        batch.equity_position_instruments.push_back(row);
    } else {
        throw trade_population_error("The equity trade type " + type +
                                     " states no product this planner knows.");
    }
    return batch;
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

    for (std::uint32_t slot = 0; slot < slot_count(scope); ++slot) {
        const auto name = slot_name(scope, slot);
        const auto& type = equity_types[slot % equity_type_count];
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

        domain::equity_terms terms;
        terms.underlying_name = underlying.code;
        terms.underlying_kind = underlying.kind;
        terms.currency = underlying.currency.empty() ? "USD" : underlying.currency;
        terms.notional = notional;
        terms.quantity = notional;
        terms.start_date = start;
        terms.maturity_date = end;
        terms.expiry_date = end;
        terms.long_short = (draw(name, 3) < 0.5) ? "Long" : "Short";
        terms.exercise_type = "European";
        terms.settlement_type = "Cash";

        /*
         * The option type and the strike are the option family's own terms. A
         * touch states neither: the schema refuses an option type on a touch
         * and requires one on a digital, so the two are split below.
         */
        if (type == "EquityOption" || type == "EquityAsianOption" ||
            type == "EquityOutperformanceOption" || type == "EquityCliquetOption") {
            terms.strike = level;
            terms.option_type = (draw(name, 4) < 0.5) ? "Call" : "Put";
        }

        if (type.ends_with("BarrierOption")) {
            terms.strike = level;
            terms.option_type = (draw(name, 4) < 0.5) ? "Call" : "Put";
        }

        if (type == "EquityPosition" || type == "EquityOptionPosition")
            terms.strike = level;

        if (type == "EquityForward") {
            terms.fixed_price = level;
            terms.quantity = notional;
        }

        if (type == "EquityPosition" || type == "EquityOptionPosition")
            terms.fixed_price = level;

        if (type.ends_with("BarrierOption")) {
            terms.lower_barrier = std::round(level * 0.9 * 100.0) / 100.0;
            terms.lower_barrier_type = "DownAndOut";
            if (type == "EquityDoubleBarrierOption") {
                terms.upper_barrier = std::round(level * 1.1 * 100.0) / 100.0;
                terms.upper_barrier_type = "UpAndOut";
            }
            terms.rebate = 0.0;
        }

        if (type == "EquityDigitalOption") {
            terms.option_type = (draw(name, 4) < 0.5) ? "Call" : "Put";
            terms.strike = level;
            terms.payout_amount = notional;
        }

        if (type == "EquityTouchOption") {
            terms.barrier_level = std::round(level * 0.9 * 100.0) / 100.0;
            terms.barrier_type = "DownAndOut";
            terms.payout_amount = notional;
        }

        if (type == "EquityAsianOption") {
            terms.average_type = "Arithmetic";
            terms.averaging_start_date = start;
            terms.averaging_end_date = end;
        }

        if (type == "EquitySwap") {
            terms.return_type = "Price";
            terms.payment_frequency_code = "Quarterly";
        }

        if (type == "EquityVarianceSwap") {
            terms.variance_strike = 0.04;
            terms.moment_type = "Variance";
        }

        if (type == "EquityAccumulator") {
            terms.fixing_frequency = "Daily";
            terms.knock_out_level = std::round(level * 1.1 * 100.0) / 100.0;
            // An accumulator states no target: the schema refuses one, and
            // the target is the TaRF's own term.
            terms.payoff_type = "Accumulator";
            terms.range_bounds.push_back(domain::equity_range_bound{std::nullopt, level, 1.0});
            terms.range_bounds.push_back(domain::equity_range_bound{level, std::nullopt, 2.0});
        }

        if (type == "EquityTaRF") {
            terms.target_amount = notional * 2.0;
            terms.target_type = "TargetFull";
            terms.target_points = 2.0;
            terms.knock_out_level = std::round(level * 1.1 * 100.0) / 100.0;
        }

        if (type == "EquityCliquetOption") {
            terms.cliquet_frequency = "Quarterly";
            terms.moneyness = 1.0;
            terms.local_cap = 0.05;
            terms.global_cap = 0.2;
        }

        /*
         * A worst-of basket states rows: its constituents carry the kind and
         * the price each opened at, and its periods carry the levels the
         * determination dates observe. Two constituents over four dates is the
         * shape the corpus states, and the ordinal pairs each list.
         */
        if (type == "EquityWorstOfBasketSwap") {
            for (std::uint32_t member = 0; member < 2; ++member) {
                const auto& drawn_underlying = equity_for(scope, slot + member);
                domain::equity_basket_member constituent;
                constituent.name = drawn_underlying.code;
                constituent.kind = drawn_underlying.kind;
                constituent.initial_price =
                    std::round(level * (1.0 + 0.1 * member) * 100.0) / 100.0;
                terms.basket.push_back(std::move(constituent));
            }
            for (std::uint32_t period = 0; period < 4; ++period) {
                domain::equity_basket_period row;
                row.period_date = add_months(start, static_cast<int>(period));
                row.knock_out_level = 1.0;
                row.fixed_trigger_level = 0.75;
                terms.periods.push_back(std::move(row));
            }
        }

        auto entry = common_slot(scope, slot);
        entry.trade_type_code = std::string(type);
        entry.notional = notional;
        /*
         * The equity family states its rows here, where it draws them, and the
         * plan carries no bag for it.
         */
        entry.instruments = construct_equity_batch(terms, entry.trade_type_code);
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
