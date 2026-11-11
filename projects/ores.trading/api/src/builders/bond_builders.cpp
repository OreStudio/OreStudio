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
#include "ores.trading.api/builders/bond_builders.hpp"
#include "refusals.hpp"
#include <boost/uuid/nil_generator.hpp>
#include <algorithm>
#include <format>
#include <functional>
#include <utility>

namespace ores::trading::builders {

namespace {

using utility::decimal::decimal;
using ymd = std::chrono::year_month_day;

std::optional<build_error> holding_refusal(const bond_holding& holding) {
    if (holding.issue_id.get() == boost::uuids::nil_uuid())
        return build_error{"issue_id", "must name the security"};
    if (holding.notional.get() <= decimal{})
        return build_error{"notional", "must be positive"};
    if (holding.currency.get().empty())
        return build_error{"currency", "must not be empty"};
    return std::nullopt;
}

domain::bond_product product_of(std::string_view trade_type, const bond_holding& holding) {
    domain::bond_product product{};
    auto& instrument = product.row.instrument;
    instrument.identity.trade_type_code = std::string(trade_type);
    instrument.issue_id = holding.issue_id.get();
    instrument.notional = holding.notional.get();
    return product;
}

/// The price a TRS states its initial price at: par, per hundred.
constexpr double par_price = 100.0;

std::string iso(ymd date) {
    return std::format("{:%F}", std::chrono::sys_days{date});
}

/*
 * One of the trade's own legs, under the role the reader joins it back by:
 * its row, its notional and its schedule from @p start to @p end.
 */
void add_trade_leg(domain::bond_product& product,
                   std::string role,
                   std::string type,
                   bool payer,
                   const bond_holding& holding,
                   std::string tenor,
                   ymd start,
                   ymd end) {
    domain::bond_leg leg{};
    leg.leg_role = role;
    leg.leg_number = 1;
    leg.payer = payer;
    leg.leg_type = std::move(type);
    leg.currency = holding.currency.get();
    leg.day_counter = "A360";
    leg.payment_convention = "F";
    product.legs.push_back(std::move(leg));

    domain::bond_leg_amount notional{};
    notional.leg_role = role;
    notional.leg_number = 1;
    notional.amount_role = "notional";
    notional.sequence_number = 1;
    notional.value = holding.notional.get();
    product.leg_amounts.push_back(std::move(notional));

    domain::instrument_schedule schedule{};
    schedule.owner_role = std::move(role);
    schedule.owner_number = 1;
    schedule.schedule_role = "schedule";
    schedule.sequence_number = 1;
    schedule.schedule_kind = "rules";
    schedule.start_date = start;
    schedule.end_date = end;
    schedule.tenor = std::move(tenor);
    schedule.calendar = "TARGET";
    schedule.convention = "F";
    schedule.rule = "Forward";
    product.schedules.push_back(std::move(schedule));
}

/// The rate a trade leg pays: fixed at a rate, or floating on an index plus a spread.
void add_leg_rate(domain::bond_product& product,
                  const std::string& role,
                  std::string kind,
                  std::optional<std::string> index,
                  std::string amount_role,
                  decimal value) {
    domain::bond_leg_rate rate{};
    rate.leg_role = role;
    rate.leg_number = 1;
    rate.rate_kind = std::move(kind);
    rate.index = std::move(index);
    product.leg_rates.push_back(std::move(rate));

    domain::bond_leg_amount amount{};
    amount.leg_role = role;
    amount.leg_number = 1;
    amount.amount_role = std::move(amount_role);
    amount.sequence_number = 1;
    amount.value = value;
    product.leg_amounts.push_back(std::move(amount));
}

}

bond_issue_builder::bond_issue_builder(bond_issue_terms terms)
    : terms_(std::move(terms)) {}

std::expected<domain::bond_security, build_error> bond_issue_builder::build() const {
    if (terms_.issue_id.get() == boost::uuids::nil_uuid())
        return std::unexpected(build_error{"issue_id", "must not be nil"});
    if (terms_.security_id.get().empty())
        return std::unexpected(build_error{"security_id", "must not be empty"});
    if (terms_.face_value.get() <= decimal{})
        return std::unexpected(build_error{"face_value", "must be positive"});
    if (terms_.coupon_rate.get() < decimal{})
        return std::unexpected(build_error{"coupon_rate", "must not be negative"});
    if (auto refusal = refusals::period(terms_.issue_date.get(), terms_.maturity.get()))
        return std::unexpected(build_error{"maturity", "must be after the issue date"});

    const auto id = terms_.issue_id.get();
    domain::bond_security out{};
    auto& issue = out.issue;
    issue.issue_id = id;
    issue.security_id = terms_.security_id.get();
    issue.issuer = terms_.issuer.get();
    issue.face_value = terms_.face_value.get();
    issue.issue_date = terms_.issue_date.get();
    issue.settlement_days = 2;
    issue.calendar = terms_.calendar.get();
    issue.sub_type = "Fixed";
    issue.price_type = "Clean";
    if (!terms_.reference_curve.empty())
        issue.reference_curve_id = terms_.reference_curve;

    domain::bond_issue_leg leg{};
    leg.issue_id = id;
    leg.leg_number = 1;
    leg.payer = false;
    leg.leg_type = "Fixed";
    leg.currency = terms_.currency.get();
    leg.payment_convention = "F";
    leg.day_counter = "ACT/ACT";
    out.legs.push_back(std::move(leg));

    domain::bond_issue_leg_amount notional{};
    notional.issue_id = id;
    notional.leg_number = 1;
    notional.amount_role = "notional";
    notional.sequence_number = 1;
    notional.value = terms_.face_value.get();
    out.amounts.push_back(std::move(notional));

    domain::bond_issue_leg_rate rate{};
    rate.issue_id = id;
    rate.leg_number = 1;
    rate.rate_kind = "fixed";
    out.rates.push_back(std::move(rate));

    domain::bond_issue_leg_amount coupon{};
    coupon.issue_id = id;
    coupon.leg_number = 1;
    coupon.amount_role = "rate";
    coupon.sequence_number = 2;
    coupon.value = terms_.coupon_rate.get();
    out.amounts.push_back(std::move(coupon));

    domain::bond_issue_leg_schedule schedule{};
    schedule.issue_id = id;
    schedule.leg_number = 1;
    schedule.schedule_role = "schedule";
    schedule.sequence_number = 1;
    schedule.schedule_kind = "rules";
    schedule.start_date = terms_.issue_date.get();
    schedule.end_date = terms_.maturity.get();
    schedule.tenor = "1Y";
    schedule.calendar = terms_.calendar.get();
    schedule.convention = "F";
    schedule.term_convention = "F";
    schedule.rule = "Forward";
    out.schedules.push_back(std::move(schedule));
    return out;
}

bond_builder::bond_builder(bond_holding holding)
    : holding_(std::move(holding)) {}

bond_result bond_builder::build() const {
    if (auto refusal = holding_refusal(holding_))
        return std::unexpected(std::move(*refusal));
    return product_of(trade_type, holding_);
}

bond_position_builder::bond_position_builder(bond_position_terms terms)
    : terms_(std::move(terms)) {}

bond_result bond_position_builder::build() const {
    if (auto refusal = holding_refusal(terms_.holding))
        return std::unexpected(std::move(*refusal));
    if (terms_.identifier.get().empty())
        return std::unexpected(build_error{"identifier", "must not be empty"});
    auto product = product_of(trade_type, terms_.holding);
    product.row.instrument.position_identifier = terms_.identifier.get();
    domain::instrument_underlying underlying{};
    underlying.underlying_role = "underlying";
    underlying.sequence_number = 1;
    underlying.underlying_type = "Bond";
    underlying.name = terms_.security.get().uri();
    underlying.weight = 1.0;
    product.underlyings.push_back(std::move(underlying));
    return product;
}

bond_option_builder::bond_option_builder(bond_option_terms terms)
    : terms_(std::move(terms)) {}

bond_result bond_option_builder::build() const {
    if (auto refusal = holding_refusal(terms_.holding))
        return std::unexpected(std::move(*refusal));
    if (terms_.strike.get() <= decimal{})
        return std::unexpected(build_error{"strike", "must be positive"});
    auto product = product_of(trade_type, terms_.holding);

    domain::bond_option option{};
    option.option_type = option_type_of(terms_.kind.get());
    option.option_strike = terms_.strike.get();
    option.redemption = "100";
    option.price_type = "Clean";
    option.knocks_out = "false";
    product.options.push_back(std::move(option));

    domain::instrument_option block{};
    block.long_short = long_short_of(terms_.position.get());
    block.option_type = option_type_of(terms_.kind.get());
    block.style = "European";
    block.settlement = "Cash";
    product.option_block.push_back(std::move(block));

    domain::instrument_schedule exercise{};
    exercise.owner_role = "option";
    exercise.owner_number = 1;
    exercise.schedule_role = "exercise_dates";
    exercise.sequence_number = 1;
    exercise.schedule_kind = "dates";
    product.schedules.push_back(std::move(exercise));
    domain::instrument_schedule_date date{};
    date.owner_role = "option";
    date.owner_number = 1;
    date.schedule_role = "exercise_dates";
    date.schedule_sequence_number = 1;
    date.sequence_number = 1;
    date.schedule_date = terms_.expiry.get();
    product.schedule_dates.push_back(std::move(date));
    return product;
}

forward_bond_builder::forward_bond_builder(forward_bond_terms terms)
    : terms_(std::move(terms)) {}

bond_result forward_bond_builder::build() const {
    if (auto refusal = holding_refusal(terms_.holding))
        return std::unexpected(std::move(*refusal));
    if (terms_.settlement.get() > terms_.forward_maturity.get())
        return std::unexpected(
            build_error{"settlement", "must not be after the forward's maturity"});
    auto product = product_of(trade_type, terms_.holding);
    domain::bond_forward forward{};
    // ORE reads LongInForward as a boolean.
    forward.long_in_forward = terms_.long_in_forward.get() ? "true" : "false";
    forward.forward_maturity_date = iso(terms_.forward_maturity.get());
    forward.forward_settlement_date = iso(terms_.settlement.get());
    forward.settlement = "Cash";
    // ORE states a forward by its lock rate or its amount, not both.
    forward.lock_rate = terms_.lock_rate.get();
    forward.settlement_dirty = "false";
    product.forwards.push_back(std::move(forward));
    return product;
}

bond_future_builder::bond_future_builder(bond_future_terms terms)
    : terms_(std::move(terms)) {}

bond_result bond_future_builder::build() const {
    if (auto refusal = holding_refusal(terms_.holding))
        return std::unexpected(std::move(*refusal));
    if (terms_.contract.get().empty())
        return std::unexpected(build_error{"contract", "must not be empty"});
    if (terms_.contract_notional.get() <= decimal{})
        return std::unexpected(build_error{"contract_notional", "must be positive"});
    auto product = product_of(trade_type, terms_.holding);
    domain::bond_future future{};
    future.contract_name = terms_.contract.get();
    future.contract_notional = terms_.contract_notional.get();
    future.long_short = long_short_of(terms_.position.get());
    future.apply_conversion_factor = true;
    future.use_future_price = true;
    product.futures.push_back(std::move(future));
    return product;
}

bond_repo_builder::bond_repo_builder(bond_repo_terms terms)
    : terms_(std::move(terms)) {}

bond_result bond_repo_builder::build() const {
    if (auto refusal = holding_refusal(terms_.holding))
        return std::unexpected(std::move(*refusal));
    if (auto refusal = refusals::period(terms_.start.get(), terms_.end.get()))
        return std::unexpected(std::move(*refusal));
    auto product = product_of(trade_type, terms_.holding);
    domain::bond_repo repo{};
    repo.repo_type = "Fixed";
    repo.repo_rate = terms_.repo_rate.get();
    product.repos.push_back(std::move(repo));
    // The cash lent against the bond, repaid with interest at the repo rate.
    add_trade_leg(
        product, "repo", "Fixed", true, terms_.holding, "1Y", terms_.start.get(), terms_.end.get());
    add_leg_rate(product, "repo", "fixed", std::nullopt, "rate", terms_.repo_rate.get());
    return product;
}

bond_trs_builder::bond_trs_builder(bond_trs_terms terms)
    : terms_(std::move(terms)) {}

bond_result bond_trs_builder::build() const {
    if (auto refusal = holding_refusal(terms_.holding))
        return std::unexpected(std::move(*refusal));
    if (auto refusal = refusals::period(terms_.start.get(), terms_.end.get()))
        return std::unexpected(std::move(*refusal));
    auto product = product_of(trade_type, terms_.holding);
    domain::bond_trs trs{};
    trs.return_type = "Price";
    trs.funding_leg_type = "Fixed";
    trs.funding_rate = terms_.funding_rate.get();
    // The book receives the bond's return and pays the funding leg.
    trs.payer = "false";
    trs.price_type = "Clean";
    trs.initial_price = decimal::from_double(par_price).value();
    product.trs.push_back(std::move(trs));
    add_trade_leg(product,
                  "trs_funding",
                  "Fixed",
                  true,
                  terms_.holding,
                  "3M",
                  terms_.start.get(),
                  terms_.end.get());
    add_leg_rate(product, "trs_funding", "fixed", std::nullopt, "rate", terms_.funding_rate.get());

    // The dates the bond's return is measured over, quarterly like the funding.
    domain::instrument_schedule returns{};
    returns.owner_role = "trs";
    returns.owner_number = 1;
    returns.schedule_role = "schedule";
    returns.sequence_number = 1;
    returns.schedule_kind = "rules";
    returns.start_date = terms_.start.get();
    returns.end_date = terms_.end.get();
    returns.tenor = "3M";
    returns.calendar = "TARGET";
    returns.convention = "F";
    returns.rule = "Forward";
    product.schedules.push_back(std::move(returns));
    return product;
}

callable_bond_builder::callable_bond_builder(callable_bond_terms terms)
    : terms_(std::move(terms)) {}

bond_result callable_bond_builder::build() const {
    if (auto refusal = holding_refusal(terms_.holding))
        return std::unexpected(std::move(*refusal));
    const auto& calls = terms_.calls.get();
    if (calls.empty())
        return std::unexpected(build_error{"calls", "must state at least one date"});
    if (std::ranges::adjacent_find(calls, std::greater_equal<>{}) != calls.end())
        return std::unexpected(build_error{"calls", "must be in order with no date twice"});
    auto product = product_of(trade_type, terms_.holding);
    int number = 0;
    for (const auto& when : terms_.calls.get()) {
        domain::bond_issue_call_date call{};
        call.issue_id = terms_.holding.issue_id.get();
        call.sequence_number = ++number;
        call.call_date = when;
        product.call_dates.push_back(std::move(call));
    }
    return product;
}

convertible_bond_builder::convertible_bond_builder(convertible_bond_terms terms)
    : terms_(std::move(terms)) {}

bond_result convertible_bond_builder::build() const {
    if (auto refusal = holding_refusal(terms_.holding))
        return std::unexpected(std::move(*refusal));
    if (terms_.share.get().empty())
        return std::unexpected(build_error{"share", "must not be empty"});
    if (!(terms_.conversion_ratio.get() > 0.0))
        return std::unexpected(build_error{"conversion_ratio", "must be positive"});
    auto product = product_of(trade_type, terms_.holding);
    domain::bond_issue_conversion_target target{};
    target.issue_id = terms_.holding.issue_id.get();
    target.sequence_number = 1;
    target.underlying_id = terms_.share.get();
    target.conversion_ratio = terms_.conversion_ratio.get();
    product.conversion_targets.push_back(std::move(target));
    return product;
}

ascot_builder::ascot_builder(ascot_terms terms)
    : terms_(std::move(terms)) {}

bond_result ascot_builder::build() const {
    if (auto refusal = refusals::period(terms_.start.get(), terms_.end.get()))
        return std::unexpected(std::move(*refusal));
    auto product = convertible_bond_builder(terms_.convertible).build();
    if (!product)
        return product;
    product->row.instrument.identity.trade_type_code = std::string(trade_type);
    domain::ascot ascot{};
    ascot.ascot_option_type = option_type_of(terms_.kind.get());
    product->ascots.push_back(std::move(ascot));

    // The holder may recall the bond on any day from the start to the end.
    domain::instrument_option block{};
    block.long_short = "Long";
    block.option_type = option_type_of(terms_.kind.get());
    block.style = "American";
    block.settlement = "Cash";
    product->option_block.push_back(std::move(block));
    domain::instrument_schedule exercise{};
    exercise.owner_role = "option";
    exercise.owner_number = 1;
    exercise.schedule_role = "exercise_dates";
    exercise.sequence_number = 1;
    exercise.schedule_kind = "dates";
    product->schedules.push_back(std::move(exercise));
    int number = 0;
    for (const auto when : {terms_.start.get(), terms_.end.get()}) {
        domain::instrument_schedule_date date{};
        date.owner_role = "option";
        date.owner_number = 1;
        date.schedule_role = "exercise_dates";
        date.schedule_sequence_number = 1;
        date.sequence_number = ++number;
        date.schedule_date = when;
        product->schedule_dates.push_back(std::move(date));
    }
    // The holder recalls the bond against the swap's floating leg plus the spread.
    const auto& holding = terms_.convertible.holding;
    add_trade_leg(*product,
                  "ascot_swap",
                  "Floating",
                  false,
                  holding,
                  "3M",
                  terms_.start.get(),
                  terms_.end.get());
    add_leg_rate(*product,
                 "ascot_swap",
                 "floating",
                 terms_.funding_index.get().uri(),
                 "spread",
                 terms_.recall_spread.get());
    return product;
}

}
