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
#include "ores.trading.api/builders/bond_builders.hpp"
#include "ores.trading.api/builders/credit_builders.hpp"
#include "ores.trading.api/builders/fx_builders.hpp"
#include "ores.trading.api/builders/rate_builders.hpp"
#include "ores.trading.api/domain/bond_product_batch.hpp"
#include "ores.trading.api/domain/commodity_instruments.hpp"
#include "ores.trading.api/domain/equity_instruments.hpp"
#include "ores.trading.api/domain/instrument_batch_mapper.hpp"
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


namespace {

/*
 * The indices a rate trade draws: those the tenant's swap conventions name
 * with a tenor, so a builder can read each leg's terms and a forward rate
 * agreement or a cap resets on an Ibor index. The domestic ones are in the
 * scope's currency, the foreign ones in any other.
 */
std::vector<ores::marketdata::datum::market_reference>
swap_indices_of(const domain::trade_generation_scope& scope, bool domestic) {
    namespace md = ores::marketdata::datum;
    std::vector<md::market_reference> out;
    for (const auto& swap : scope.conventions.swaps) {
        if (!swap.oresmd_uri)
            continue;
        auto index = md::market_reference::parse(*swap.oresmd_uri);
        if (!index || index->index() == nullptr || index->index()->get("tenor") == nullptr)
            continue;
        if ((index->index()->subject() == scope.currency_code) == domestic)
            out.push_back(std::move(*index));
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
                                           "Ascot"};
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
        slot % 2 == 0 ? tb::option_position::long_option : tb::option_position::short_option;
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
        const auto position_drawn = static_cast<std::size_t>(
            draw(security_id, 10) * static_cast<double>(scope.equity_underlyings.size()));
        return tb::convertible_bond_terms{
            .holding = holding,
            .share = scope
                         .equity_underlyings[std::min(position_drawn,
                                                      scope.equity_underlyings.size() - 1)]
                         .code,
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
    if (type == "ForwardBond")
        // A forward on the bond matures a year out, well before the bond does.
        return built(tb::forward_bond_builder({.holding = holding,
                                               .long_in_forward = slot % 2 == 0,
                                               .forward_maturity = add_years(scope.as_of_date, 1),
                                               .settlement = add_years(scope.as_of_date, 1),
                                               .lock_rate = rate(0.02, 0.03, 7)})
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
                                           .funding_rate = rate(0.02, 0.01, 9),
                                           .start = scope.as_of_date,
                                           .end = add_years(scope.as_of_date, 1)})
                         .build());
    if (type == "CallableBond")
        return built(tb::callable_bond_builder(
                         {.holding = holding,
                          .calls = std::vector<year_month_day>{add_years(maturity, -2),
                                                               add_years(maturity, -1)}})
                         .build());
    if (type == "ConvertibleBond")
        return built(tb::convertible_bond_builder(convertible()).build());
    if (type == "Ascot") {
        const auto indices = swap_indices_of(scope, true);
        if (indices.empty())
            throw trade_population_error("No swap convention of the tenant names an index in " +
                                         scope.currency_code +
                                         ", so no Ascot can state its swap leg.");
        return built(tb::ascot_builder({.convertible = convertible(),
                                        .kind = kind,
                                        .funding_index = indices[slot % indices.size()],
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

domain::trade_population_plan
bond_trade_planner::plan(const domain::trade_generation_scope& scope) const {
    validate_scope(scope);

    const auto min_years = tenor_years_of(scope.min_tenor_code);
    const auto max_years = tenor_years_of(scope.max_tenor_code);

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
        const auto issuer_id = issuer_id_for(scope, tenor_years);
        const auto security_id =
            security_id_for(scope, tenor_years, *types[slot % types.size()], issuer_id);
        const auto issue_id = issue_id_for_security(security_id);
        const auto issue_date = add_months(scope.as_of_date, -security_age_months);
        const auto maturity = add_years(issue_date, static_cast<int>(tenor_years));

        auto entry = common_slot(scope, slot);
        entry.trade_type_code = std::string(*types[slot % types.size()]);
        entry.notional = notional_from(scope, draw(name, 1));

        namespace tdom = ores::trading::domain;
        auto security = ores::trading::builders::bond_issue_builder(
                            {.issue_id = issue_id,
                             .security_id = security_id,
                             .issuer = boost::uuids::to_string(issuer_id),
                             .currency = entry.currency,
                             .face_value = tdom::decimal::from_double(security_face_value).value(),
                             .coupon_rate = tdom::decimal::from_double(coupon_rate).value(),
                             .issue_date = issue_date,
                             .maturity = maturity,
                             .calendar = std::string("TARGET"),
                             .reference_curve = scope.reference_curve_id})
                            .build();
        if (!security)
            throw trade_population_error("The bond issue builder refuses the drawn " +
                                         security.error().term + ": " + security.error().message);
        auto product = build_bond_trade(scope, entry, security_id, issue_id, maturity);
        entry.bond_issue = std::move(security->issue);
        entry.bond_issue_legs = std::move(security->legs);
        entry.bond_issue_leg_amounts = std::move(security->amounts);
        entry.bond_issue_leg_rates = std::move(security->rates);
        entry.bond_issue_leg_schedules = std::move(security->schedules);
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
                 const domain::trade_population_slot& entry,
                 std::uint32_t years) {
    namespace tb = ores::trading::builders;
    using ores::utility::decimal::decimal;
    const auto money = [](double value) {
        return decimal::from_double(value).value();
    };

    const auto domestic = swap_indices_of(scope, true);
    if (domestic.empty())
        throw trade_population_error("No swap convention of the tenant names an index in " +
                                     scope.currency_code +
                                     ", so no interest rate trade can state its legs.");

    const auto slot = entry.slot;
    const auto& type = entry.trade_type_code;
    const auto name = slot_name(scope, slot);
    const auto position =
        slot % 2 == 0 ? tb::option_position::long_option : tb::option_position::short_option;
    /*
     * A rate is a decimal in the store, not a percentage: one per cent to six
     * per cent, quantised to a ten-thousandth so a decimal column holds it
     * exactly.
     */
    const auto fixed_rate = money(std::round((0.01 + 0.05 * draw(name, 2)) * 10000.0) / 10000.0);
    const auto swap_from = [&](year_month_day start, year_month_day maturity) {
        return tb::vanilla_swap_terms{.notional = money(entry.notional),
                                      .currency = scope.currency_code,
                                      .start = start,
                                      .maturity = maturity,
                                      .fixed_rate = fixed_rate,
                                      .index = domestic[slot % domestic.size()],
                                      .fixed_side =
                                          slot % 2 == 0 ? tb::side::pay : tb::side::receive};
    };
    const auto start = add_months(scope.as_of_date, -static_cast<int>(slot % 6));
    const auto swap = swap_from(start, add_years(start, static_cast<int>(years)));
    const auto built = [&](auto result) {
        return swap_batch_of(std::move(result), type, "Generated interest rate trade.");
    };

    if (type == "Swap")
        return built(tb::vanilla_swap_builder(conventions, swap).build());
    if (type == "CrossCurrencySwap") {
        const auto foreign = swap_indices_of(scope, false);
        if (foreign.empty())
            throw trade_population_error("No swap convention of the tenant names an index "
                                         "outside " +
                                         scope.currency_code +
                                         ", so no cross currency swap can state its foreign leg.");
        const auto& index = foreign[slot % foreign.size()];
        return built(tb::cross_currency_swap_builder(conventions,
                                                     {.domestic = swap,
                                                      .foreign_notional = money(entry.notional),
                                                      .foreign_currency = index.index()->subject(),
                                                      .foreign_index = index})
                         .build());
    }
    if (type == "ForwardRateAgreement") {
        /*
         * A forward rate agreement is a short contract that settles in the
         * future: it starts in three months and runs for six.
         */
        const auto from = add_months(scope.as_of_date, 3);
        return built(
            tb::forward_rate_agreement_builder(
                conventions, {.period = swap_from(from, add_months(from, 6)), .position = position})
                .build());
    }
    if (type == "CapFloor")
        return built(tb::cap_floor_builder(conventions,
                                           {.swap = swap,
                                            .kind = slot % 2 == 0 ? tb::cap_floor_kind::cap :
                                                                    tb::cap_floor_kind::floor,
                                            .position = position})
                         .build());
    if (type == "Swaption") {
        /*
         * An option is exercised after the population is written, and the
         * swap it opens starts two days after its expiry.
         */
        const auto expiry = add_years(scope.as_of_date, 1);
        const auto swap_start = year_month_day{sys_days{expiry} + days{2}};
        return built(
            tb::swaption_builder(
                conventions,
                {.swap = swap_from(swap_start, add_years(swap_start, static_cast<int>(years))),
                 .expiry = expiry,
                 .position = position,
                 .settles = tb::settlement::cash})
                .build());
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

        auto entry = common_slot(scope, slot);
        entry.trade_type_code = std::string(type);
        entry.notional =
            price_from(name, 1, scope.min_notional, scope.max_notional - scope.min_notional);
        entry.instruments =
            build_rate_trade(scope, conventions, entry, std::clamp(drawn, min_years, max_years));

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
        draw(name, 4) < 0.5 ? tb::option_position::long_option : tb::option_position::short_option;
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
        return built(tb::fx_swap_builder({.near = exchange,
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
        return built(tb::fx_average_forward_builder({.fx_index = index,
                                                     .reference_currency = pair.base,
                                                     .reference_notional = money(notional),
                                                     .settlement_currency = pair.quote,
                                                     .settlement_notional = money(notional * rate),
                                                     .start = start,
                                                     .payment_date = end})
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

        auto entry = common_slot(scope, slot);
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
                                             "EquityWorstOfBasketSwap"};
/*
 * EquityOutperformanceOption is routed to the option table and that table's
 * check admits EquityOption and EquityCliquetOption only, so no outperformance
 * row can satisfy it. The type stays out until the check admits it, and the
 * gap is recorded rather than worked around by booking it as an option.
 */

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
                                             "RiskParticipationAgreement"};

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
 * The index an RPA's underlying swap floats on: that of the first swap
 * convention whose index is in the trade currency, so the underlying is one the
 * tenant's conventions can build.
 */
ores::marketdata::datum::market_reference
underlying_index_of(const domain::trade_generation_scope& scope) {
    namespace md = ores::marketdata::datum;
    for (const auto& swap : scope.conventions.swaps) {
        if (!swap.oresmd_uri)
            continue;
        auto index = md::market_reference::parse(*swap.oresmd_uri);
        if (index && index->index() != nullptr && index->index()->subject() == scope.currency_code)
            return std::move(*index);
    }
    throw trade_population_error("No swap convention of the tenant floats on an index in " +
                                 scope.currency_code +
                                 ", so no risk participation agreement can state its "
                                 "underlying swap.");
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
    const auto side = entry.slot % 2 == 0 ? tb::protection::buy : tb::protection::sell;
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
    if (type == "IndexCreditDefaultSwapOption") {
        const auto expiry = add_months(scope.as_of_date, 3);
        const tb::index_credit_default_swap_option_terms terms{
            .swap = protection(year_month_day{sys_days{expiry} + days{1}}),
            .expiry = expiry,
            .strike = money(1.0),
            .index_term = std::format("{}Y", years),
            .position = entry.slot % 2 == 0 ? tb::option_position::long_option :
                                              tb::option_position::short_option};
        return batch_of(tb::index_credit_default_swap_option_builder(conventions, terms).build(),
                        type);
    }
    if (type == "RiskParticipationAgreement") {
        const tb::risk_participation_agreement_terms terms{
            .underlying = {.notional = money(entry.notional),
                           .currency = entry.currency,
                           .start = start,
                           .maturity = add_years(start, static_cast<int>(years)),
                           .fixed_rate =
                               money(std::round((0.02 + 0.02 * draw(name, 3)) * 10000.0) / 10000.0),
                           .index = underlying_index_of(scope),
                           .fixed_side = draw(name, 4) < 0.5 ? tb::side::pay : tb::side::receive},
            .participation_rate = 0.5,
            .reference_entity = reference_entity,
            .credit_curve = credit_curve_of(reference_entity, entry.currency),
            .side = side,
            .fee = money(std::round(entry.notional * 0.001 * 100.0) / 100.0),
            .convention = std::string(cds_convention_id)};
        return batch_of(tb::risk_participation_agreement_builder(conventions, terms).build(), type);
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

        auto entry = common_slot(scope, slot);
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

/*
 * The scripted codes the population books, in the order the routing table
 * states them. Autocallable_01 is routed like the others and is left out
 * deliberately: the ORE projection has no writer for its data element, so a
 * trade of it would book and then export as an empty product.
 */
constexpr std::string_view scripted_types[] = {
    "ScriptedTrade", "DoubleDigitalOption", "PerformanceOption_01"};
constexpr std::size_t scripted_type_count = std::size(scripted_types);

/*
 * The script each code is written from. The document names the script the
 * engine runs, and the projection reads the code's element back into the same
 * name, so the two sides agree on what the trade is.
 */
std::string_view script_of(std::string_view trade_type) {
    if (trade_type == "ScriptedTrade")
        return "AsianBasketOption";
    if (trade_type == "DoubleDigitalOption")
        return "DoubleDigitalOption";
    return "PerformanceOption_01";
}

/*
 * The parameters the script needs. They are the document's own keys, because
 * the projection writes each key into the element and reads it back from it, so
 * a trade that states them exports whole rather than as a name with nothing
 * behind it.
 */
std::string parameters_of(std::string_view trade_type,
                          const std::string& currency,
                          const std::chrono::year_month_day& end,
                          double notional,
                          double strike) {
    const auto money = std::format("{:.2f}", notional);
    const auto level = std::format("{:.4f}", strike);
    const auto expiry = std::format("{:%Y-%m-%d}", end);
    const auto settlement = std::format("{:%Y-%m-%d}", add_months(end, 2));
    if (trade_type == "ScriptedTrade")
        return std::format(R"({{"Notional":"{}","PutCall":"Call","Strike":"{}"}})", money, level);
    if (trade_type == "DoubleDigitalOption")
        /*
         * The document's own members, which is what the projection writes from
         * these keys and reads back into them. A key the document has no member
         * for is written as an element the schema does not hold.
         */
        return std::format(
            R"({{"Type1":"Equity","Type2":"Equity","Expiry":"{}","Settlement":"{}","Position":"Long","PayCcy":"{}","BinaryPayout":"{}","BinaryLevel1":"{}","BinaryLevel2":"{}","Underlying1Type":"Equity","Underlying2Type":"Equity"}})",
            expiry,
            settlement,
            currency,
            money,
            level,
            std::format("{:.4f}", strike * 1.2));
    return std::format(
        R"({{"NotionalAmount":"{}","ParticipationRate":"1.0","Strike":"{}","ValuationDate":"{}","SettlementDate":"{}","Position":"Long","PayCcy":"{}","StrikePrices":"{},{},{}","Underlying1Type":"Equity","Underlying2Type":"Equity"}})",
        money,
        level,
        expiry,
        settlement,
        currency,
        level,
        std::format("{:.4f}", strike * 1.1),
        std::format("{:.4f}", strike * 1.2));
}

/*
 * The scripted family states its row here, where it draws it: the table is flat
 * and the script's own fields are text, so the row is what the plan carries.
 */
ores::trading::domain::instrument_batch
construct_scripted_batch(const domain::trade_generation_scope& scope,
                         std::string_view trade_type,
                         std::uint32_t slot,
                         const std::chrono::year_month_day& end,
                         double notional,
                         double strike) {
    namespace tdom = ores::trading::domain;

    /*
     * The underlyings cycle the tenant's catalogue, so a population longer than
     * the catalogue states every name it holds and none it does not.
     */
    const auto& first = scope.equity_underlyings[slot % scope.equity_underlyings.size()];
    const auto& second = scope.equity_underlyings[(slot + 1) % scope.equity_underlyings.size()];
    const auto underlyings = trade_type == "PerformanceOption_01" ?
                                 std::format(R"(["{}","{}"])", first.code, second.code) :
                                 std::format(R"(["{}"])", first.code);

    tdom::scripted_instrument row;
    row.identity.trade_type_code = std::string(trade_type);
    row.script_name = std::string(script_of(trade_type));
    row.underlyings_json = underlyings;
    row.parameters_json = parameters_of(trade_type, scope.currency_code, end, notional, strike);
    row.description = "Generated scripted trade.";

    tdom::instrument_batch batch;
    batch.scripted_instruments.push_back(std::move(row));
    return batch;
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

        auto entry = common_slot(scope, slot);
        entry.trade_type_code = std::string(type);
        entry.notional = notional;
        entry.instruments = construct_scripted_batch(scope, type, slot, end, notional, strike);

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

        auto entry = common_slot(scope, slot);
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
            .fixed_side = slot % 2 == 0 ? ores::trading::builders::side::pay :
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
