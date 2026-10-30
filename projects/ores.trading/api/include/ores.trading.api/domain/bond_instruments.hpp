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
#ifndef ORES_TRADING_API_DOMAIN_BOND_INSTRUMENTS_HPP
#define ORES_TRADING_API_DOMAIN_BOND_INSTRUMENTS_HPP

#include "ores.trading.api/domain/ascot.hpp"
#include "ores.trading.api/domain/bond_forward.hpp"
#include "ores.trading.api/domain/bond_future.hpp"
#include "ores.trading.api/domain/bond_instrument_data.hpp"
#include "ores.trading.api/domain/bond_issue_call_date.hpp"
#include "ores.trading.api/domain/bond_issue_conversion_target.hpp"
#include "ores.trading.api/domain/bond_option.hpp"
#include "ores.trading.api/domain/bond_repo.hpp"
#include "ores.trading.api/domain/bond_trs.hpp"
#include "ores.utility/decimal/decimal.hpp"
#include <boost/uuid/nil_generator.hpp>
#include <boost/uuid/uuid.hpp>
#include <stdexcept>
#include <string>
#include <string_view>
#include <optional>
#include <vector>

namespace ores::trading::domain {

/**
 * @brief Refuses to construct a product from facts that do not state one.
 */
class bond_product_error final : public std::invalid_argument {
public:
    using std::invalid_argument::invalid_argument;
};

/**
 * @brief The facts a bond trade states about its security.
 */
struct bond_terms final {
    boost::uuids::uuid issue_id;
    ores::utility::decimal::decimal notional;
};

/**
 * @brief Constructs the one bond product the population draws.
 *
 * The security and its terms live in the issue, which the run writes before it
 * books the trade, so the instrument states the issue it holds and the amount
 * of that issue the trade is for.
 */
[[nodiscard]] inline bond_instrument_data construct_bond(const bond_terms& terms) {
    if (terms.issue_id == boost::uuids::nil_uuid())
        throw bond_product_error("A bond states the security issue it holds.");
    if (!(terms.notional > ores::utility::decimal::decimal{}))
        throw bond_product_error("A bond states a positive notional.");

    bond_instrument_data data;
    data.instrument.identity.trade_type_code = "Bond";
    data.instrument.issue_id = terms.issue_id;
    data.instrument.notional = terms.notional;
    return data;
}


/**
 * @brief A bond product: the header every bond states, plus the rows its own
 * product states beside it.
 *
 * A product fills the one vector that is its own and leaves the rest empty, so a
 * reader can tell a repo from an option by what is there rather than by what the
 * header claims.
 */
struct bond_product final {
    bond_instrument_data row;
    std::vector<bond_option> options;
    std::vector<bond_forward> forwards;
    std::vector<bond_future> futures;
    std::vector<bond_repo> repos;
    std::vector<bond_trs> trs;
    std::vector<ascot> ascots;
    std::vector<bond_issue_call_date> call_dates;
    std::vector<bond_issue_conversion_target> conversion_targets;
};

/**
 * @brief The facts every bond product states, whatever else it states.
 */
struct bond_common_terms final {
    boost::uuids::uuid issue_id;
    ores::utility::decimal::decimal notional;
};

namespace bond_rules {

inline void require_common(std::string_view product, const bond_common_terms& terms) {
    if (terms.issue_id == boost::uuids::nil_uuid())
        throw bond_product_error(std::string(product) + " states the security issue it holds.");
    if (!(terms.notional > ores::utility::decimal::decimal{}))
        throw bond_product_error(std::string(product) + " states a positive notional.");
}

inline void require_text(std::string_view product, std::string_view what, std::string_view value) {
    if (value.empty())
        throw bond_product_error(std::string(product) + " states " + std::string(what) + ".");
}

}

/**
 * @brief The facts a bond option states, and nothing else.
 *
 * The option prices the security it names: a strike and a type are what make it
 * an option rather than a holding of the same bond.
 */
struct bond_option_terms final {
    bond_common_terms common;
    std::string option_type;
    ores::utility::decimal::decimal strike;
    std::string redemption;
    std::string price_type;
    std::string knocks_out;
};

[[nodiscard]] inline bond_product construct_bond_option(const bond_option_terms& terms) {
    using namespace bond_rules;
    require_common("A bond option", terms.common);
    require_text("A bond option", "its type", terms.option_type);

    bond_product product;
    product.row.instrument.identity.trade_type_code = "BondOption";
    product.row.instrument.issue_id = terms.common.issue_id;
    product.row.instrument.notional = terms.common.notional;

    bond_option option;
    option.trade_id = product.row.instrument.identity.trade_id;
    option.option_type = terms.option_type;
    option.option_strike = terms.strike;
    if (!terms.redemption.empty())
        option.redemption = terms.redemption;
    if (!terms.price_type.empty())
        option.price_type = terms.price_type;
    if (!terms.knocks_out.empty())
        option.knocks_out = terms.knocks_out;
    product.options.push_back(std::move(option));
    return product;
}

/**
 * @brief The facts a forward bond states: a settlement date the trade waits for.
 */
struct bond_forward_terms final {
    bond_common_terms common;
    std::string long_in_forward;
    std::string forward_maturity_date;
    std::string forward_settlement_date;
    std::string settlement;
    std::optional<ores::utility::decimal::decimal> lock_rate;
    std::optional<ores::utility::decimal::decimal> dv01;
    std::string lock_rate_day_counter;
    std::string settlement_dirty;
    std::optional<ores::utility::decimal::decimal> premium_amount;
    std::string premium_date;
};

[[nodiscard]] inline bond_product construct_forward_bond(const bond_forward_terms& terms) {
    using namespace bond_rules;
    require_common("A forward bond", terms.common);
    require_text("A forward bond", "the date it settles", terms.forward_settlement_date);

    bond_product product;
    product.row.instrument.identity.trade_type_code = "ForwardBond";
    product.row.instrument.issue_id = terms.common.issue_id;
    product.row.instrument.notional = terms.common.notional;

    bond_forward forward;
    forward.trade_id = product.row.instrument.identity.trade_id;
    if (!terms.long_in_forward.empty())
        forward.long_in_forward = terms.long_in_forward;
    if (!terms.forward_maturity_date.empty())
        forward.forward_maturity_date = terms.forward_maturity_date;
    forward.forward_settlement_date = terms.forward_settlement_date;
    if (!terms.settlement.empty())
        forward.settlement = terms.settlement;
    forward.amount = terms.common.notional;
    forward.lock_rate = terms.lock_rate;
    forward.dv01 = terms.dv01;
    if (!terms.lock_rate_day_counter.empty())
        forward.lock_rate_day_counter = terms.lock_rate_day_counter;
    if (!terms.settlement_dirty.empty())
        forward.settlement_dirty = terms.settlement_dirty;
    forward.premium_amount = terms.premium_amount;
    if (!terms.premium_date.empty())
        forward.premium_date = terms.premium_date;
    product.forwards.push_back(std::move(forward));
    return product;
}

/**
 * @brief The facts a bond future states: the contract it names.
 */
struct bond_future_terms final {
    bond_common_terms common;
    std::string contract_name;
    ores::utility::decimal::decimal contract_notional;
    std::string long_short;
    bool apply_conversion_factor = true;
    bool use_future_price = true;
};

[[nodiscard]] inline bond_future make_bond_future(const bond_future_terms& terms) {
    using namespace bond_rules;
    require_text("A bond future", "the contract it names", terms.contract_name);
    if (!(terms.contract_notional > ores::utility::decimal::decimal{}))
        throw bond_product_error("A bond future states a positive contract notional.");

    bond_future future;
    future.contract_name = terms.contract_name;
    future.contract_notional = terms.contract_notional;
    future.long_short = terms.long_short.empty() ? std::string("Long") : terms.long_short;
    future.apply_conversion_factor = terms.apply_conversion_factor;
    future.use_future_price = terms.use_future_price;
    return future;
}

[[nodiscard]] inline bond_product construct_bond_future(const bond_future_terms& terms) {
    using namespace bond_rules;
    require_common("A bond future", terms.common);

    bond_product product;
    product.row.instrument.identity.trade_type_code = "BondFuture";
    product.row.instrument.issue_id = terms.common.issue_id;
    product.row.instrument.notional = terms.common.notional;
    auto future = make_bond_future(terms);
    future.trade_id = product.row.instrument.identity.trade_id;
    product.futures.push_back(std::move(future));
    return product;
}

/**
 * @brief The facts an option on a bond future states: the contract and the
 * option written on it, together, because neither is the product alone.
 */
struct bond_future_option_terms final {
    bond_future_terms future;
    std::string option_type;
    ores::utility::decimal::decimal strike;
    std::string redemption;
    std::string price_type;
    std::string knocks_out;
};

[[nodiscard]] inline bond_product construct_bond_future_option(
    const bond_future_option_terms& terms) {
    using namespace bond_rules;
    require_text("A bond future option", "its option type", terms.option_type);
    auto product = construct_bond_future(terms.future);
    product.row.instrument.identity.trade_type_code = "BondFutureOption";
    product.row.instrument.issue_id = terms.future.common.issue_id;

    bond_option option;
    option.trade_id = product.row.instrument.identity.trade_id;
    option.option_type = terms.option_type;
    option.option_strike = terms.strike;
    if (!terms.redemption.empty())
        option.redemption = terms.redemption;
    if (!terms.price_type.empty())
        option.price_type = terms.price_type;
    if (!terms.knocks_out.empty())
        option.knocks_out = terms.knocks_out;
    product.options.push_back(std::move(option));
    return product;
}

/**
 * @brief The facts a repo states: the rate it lends at and the index it pays.
 *
 * A repo states one funding leg or the other, never both and never neither.
 */
struct bond_repo_terms final {
    bond_common_terms common;
    std::string repo_type;
    std::optional<ores::utility::decimal::decimal> repo_rate;
    std::string repo_index;
};

[[nodiscard]] inline bond_product construct_bond_repo(const bond_repo_terms& terms) {
    using namespace bond_rules;
    require_common("A bond repo", terms.common);
    require_text("A bond repo", "its type", terms.repo_type);
    if (!terms.repo_rate.has_value() && terms.repo_index.empty())
        throw bond_product_error("A bond repo states a rate or the index it pays, not neither.");

    bond_product product;
    product.row.instrument.identity.trade_type_code = "BondRepo";
    product.row.instrument.issue_id = terms.common.issue_id;
    product.row.instrument.notional = terms.common.notional;

    bond_repo repo;
    repo.trade_id = product.row.instrument.identity.trade_id;
    repo.repo_type = terms.repo_type;
    repo.repo_rate = terms.repo_rate;
    if (!terms.repo_index.empty())
        repo.repo_index = terms.repo_index;
    product.repos.push_back(std::move(repo));
    return product;
}

/**
 * @brief The facts a total return swap states: the return it pays and the leg
 * that funds it.
 */
struct bond_trs_terms final {
    bond_common_terms common;
    std::string return_type;
    std::string funding_leg_type;
    std::optional<ores::utility::decimal::decimal> funding_rate;
    std::string funding_index;
    std::string payer;
    std::string price_type;
    std::optional<ores::utility::decimal::decimal> initial_price;
};

[[nodiscard]] inline bond_product construct_bond_trs(const bond_trs_terms& terms) {
    using namespace bond_rules;
    require_common("A bond total return swap", terms.common);
    require_text("A bond total return swap", "the return it pays", terms.return_type);
    require_text("A bond total return swap", "the leg that funds it", terms.funding_leg_type);
    if (!terms.funding_rate.has_value() && terms.funding_index.empty())
        throw bond_product_error(
            "A bond total return swap states a funding rate or the index it funds at, not neither.");

    bond_product product;
    product.row.instrument.identity.trade_type_code = "BondTRS";
    product.row.instrument.issue_id = terms.common.issue_id;
    product.row.instrument.notional = terms.common.notional;

    bond_trs trs;
    trs.trade_id = product.row.instrument.identity.trade_id;
    trs.return_type = terms.return_type;
    trs.funding_leg_type = terms.funding_leg_type;
    trs.funding_rate = terms.funding_rate;
    if (!terms.funding_index.empty())
        trs.funding_index = terms.funding_index;
    if (!terms.payer.empty())
        trs.payer = terms.payer;
    if (!terms.price_type.empty())
        trs.price_type = terms.price_type;
    trs.initial_price = terms.initial_price;
    product.trs.push_back(std::move(trs));
    return product;
}

/**
 * @brief The facts an asset swapped convertible states.
 */
struct ascot_terms final {
    bond_common_terms common;
    std::string option_type;
};

[[nodiscard]] inline bond_product construct_ascot(const ascot_terms& terms) {
    using namespace bond_rules;
    require_common("An asset swapped convertible", terms.common);
    require_text("An asset swapped convertible", "its option type", terms.option_type);

    bond_product product;
    product.row.instrument.identity.trade_type_code = "Ascot";
    product.row.instrument.issue_id = terms.common.issue_id;
    product.row.instrument.notional = terms.common.notional;

    ascot row;
    row.trade_id = product.row.instrument.identity.trade_id;
    row.ascot_option_type = terms.option_type;
    product.ascots.push_back(std::move(row));
    return product;
}

/**
 * @brief One call date a callable bond states, with its place in the schedule.
 */
struct bond_call_date_terms final {
    std::chrono::year_month_day call_date;
};

/**
 * @brief The facts a callable bond states: the dates its issuer may call it.
 *
 * The calls belong to the security and not to the trade, which is why they join
 * the issue and not the instrument.
 */
struct callable_bond_terms final {
    bond_common_terms common;
    std::vector<bond_call_date_terms> calls;
};

[[nodiscard]] inline bond_product construct_callable_bond(const callable_bond_terms& terms) {
    using namespace bond_rules;
    require_common("A callable bond", terms.common);
    if (terms.calls.empty())
        throw bond_product_error("A callable bond states at least one date its issuer may call it.");

    bond_product product;
    product.row.instrument.identity.trade_type_code = "CallableBond";
    product.row.instrument.issue_id = terms.common.issue_id;
    product.row.instrument.notional = terms.common.notional;

    std::uint32_t ordinal = 1;
    for (const auto& call : terms.calls) {
        bond_issue_call_date row;
        row.issue_id = terms.common.issue_id;
        row.sequence_number = static_cast<int>(ordinal++);
        row.call_date = call.call_date;
        product.call_dates.push_back(std::move(row));
    }
    return product;
}

/**
 * @brief One conversion target a convertible bond may convert into.
 */
struct bond_conversion_target_terms final {
    std::string underlying_id;
    double conversion_ratio = 0.0;
};

/**
 * @brief The facts a convertible bond states: what it may convert into.
 *
 * The targets belong to the security and not to the trade, so they join the
 * issue. A convertible that names no target cannot convert, so it is refused.
 */
struct convertible_bond_terms final {
    bond_common_terms common;
    std::vector<bond_conversion_target_terms> targets;
};

[[nodiscard]] inline bond_product construct_convertible_bond(
    const convertible_bond_terms& terms) {
    using namespace bond_rules;
    require_common("A convertible bond", terms.common);
    if (terms.targets.empty())
        throw bond_product_error("A convertible bond states at least one target it may convert into.");

    bond_product product;
    product.row.instrument.identity.trade_type_code = "ConvertibleBond";
    product.row.instrument.issue_id = terms.common.issue_id;
    product.row.instrument.notional = terms.common.notional;

    std::uint32_t ordinal = 1;
    for (const auto& target : terms.targets) {
        if (target.underlying_id.empty())
            throw bond_product_error("A conversion target states the underlying it converts into.");
        if (!(target.conversion_ratio > 0.0))
            throw bond_product_error("A conversion target states a positive conversion ratio.");
        bond_issue_conversion_target row;
        row.issue_id = terms.common.issue_id;
        row.sequence_number = static_cast<int>(ordinal++);
        row.underlying_id = target.underlying_id;
        row.conversion_ratio = target.conversion_ratio;
        product.conversion_targets.push_back(std::move(row));
    }
    return product;
}

/**
 * @brief A holding of a bond, which is the security and the amount of it.
 */
[[nodiscard]] inline bond_product construct_bond_position(const bond_common_terms& terms) {
    using namespace bond_rules;
    require_common("A bond position", terms);

    bond_product product;
    product.row.instrument.identity.trade_type_code = "BondPosition";
    product.row.instrument.issue_id = terms.issue_id;
    product.row.instrument.notional = terms.notional;
    return product;
}

}

#endif
