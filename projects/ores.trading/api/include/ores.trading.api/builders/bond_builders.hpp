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
#ifndef ORES_TRADING_API_BUILDERS_BOND_BUILDERS_HPP
#define ORES_TRADING_API_BUILDERS_BOND_BUILDERS_HPP

#include "ores.marketdata.api/datum/market_reference.hpp"
#include "ores.trading.api/builders/build_error.hpp"
#include "ores.trading.api/builders/option_position.hpp"
#include "ores.trading.api/builders/required.hpp"
#include "ores.trading.api/domain/bond_product.hpp"
#include "ores.trading.api/export.hpp"
#include "ores.utility/decimal/decimal.hpp"
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <expected>
#include <string>
#include <string_view>
#include <vector>

/**
 * @file bond_builders.hpp
 * @brief A bond security and the trades the generator books on one.
 *
 * The security is a fixed coupon bond: annual coupons on ACT/ACT, rolled
 * following. A trade states the rows its product needs, keyed by the trade,
 * and holds the security by its issue. A product leg, such as a repo's cash
 * leg, accrues on A360 and rolls following, as ORE's samples do.
 */
namespace ores::trading::builders {

using bond_result = std::expected<domain::bond_product, build_error>;

struct bond_issue_terms final {
    required<boost::uuids::uuid> issue_id;
    required<std::string> security_id;
    required<std::string> issuer;
    required<std::string> currency;
    required<utility::decimal::decimal> face_value;
    required<utility::decimal::decimal> coupon_rate;
    required<std::chrono::year_month_day> issue_date;
    required<std::chrono::year_month_day> maturity;
    required<std::string> calendar;
    /// The curve the bond discounts on, as the generation config names it; may be empty.
    std::string reference_curve;
};

class ORES_TRADING_API_EXPORT bond_issue_builder final {
public:
    explicit bond_issue_builder(bond_issue_terms terms);
    [[nodiscard]] std::expected<domain::bond_security, build_error> build() const;

private:
    bond_issue_terms terms_;
};

/// The security a trade holds and how much of it.
struct bond_holding final {
    required<boost::uuids::uuid> issue_id;
    required<utility::decimal::decimal> notional;
    required<std::string> currency;
};

class ORES_TRADING_API_EXPORT bond_builder final {
public:
    static constexpr std::string_view trade_type = "Bond";
    explicit bond_builder(bond_holding holding);
    [[nodiscard]] bond_result build() const;

private:
    bond_holding holding_;
};

/// A position in a security, which ORE reads as a basket of one bond.
struct bond_position_terms final {
    bond_holding holding;
    required<marketdata::datum::market_reference> security;
    /// The position's own name, which ORE requires beside the underlying.
    required<std::string> identifier;
};

class ORES_TRADING_API_EXPORT bond_position_builder final {
public:
    static constexpr std::string_view trade_type = "BondPosition";
    explicit bond_position_builder(bond_position_terms terms);
    [[nodiscard]] bond_result build() const;

private:
    bond_position_terms terms_;
};

/// A European option on the bond, struck at a clean price per hundred.
struct bond_option_terms final {
    bond_holding holding;
    required<option_kind> kind;
    required<utility::decimal::decimal> strike;
    required<std::chrono::year_month_day> expiry;
    required<option_position> position;
};

class ORES_TRADING_API_EXPORT bond_option_builder final {
public:
    static constexpr std::string_view trade_type = "BondOption";
    explicit bond_option_builder(bond_option_terms terms);
    [[nodiscard]] bond_result build() const;

private:
    bond_option_terms terms_;
};

/// A forward on the bond, settled in cash at the lock rate.
struct forward_bond_terms final {
    bond_holding holding;
    required<bool> long_in_forward;
    required<std::chrono::year_month_day> forward_maturity;
    required<std::chrono::year_month_day> settlement;
    required<utility::decimal::decimal> lock_rate;
};

class ORES_TRADING_API_EXPORT forward_bond_builder final {
public:
    static constexpr std::string_view trade_type = "ForwardBond";
    explicit forward_bond_builder(forward_bond_terms terms);
    [[nodiscard]] bond_result build() const;

private:
    forward_bond_terms terms_;
};

/// A position in a bond future contract.
struct bond_future_terms final {
    bond_holding holding;
    required<std::string> contract;
    required<utility::decimal::decimal> contract_notional;
    required<option_position> position;
};

class ORES_TRADING_API_EXPORT bond_future_builder final {
public:
    static constexpr std::string_view trade_type = "BondFuture";
    explicit bond_future_builder(bond_future_terms terms);
    [[nodiscard]] bond_result build() const;

private:
    bond_future_terms terms_;
};

/// A repo of the bond: cash lent at a fixed rate from the start to the end.
struct bond_repo_terms final {
    bond_holding holding;
    required<utility::decimal::decimal> repo_rate;
    required<std::chrono::year_month_day> start;
    required<std::chrono::year_month_day> end;
};

class ORES_TRADING_API_EXPORT bond_repo_builder final {
public:
    static constexpr std::string_view trade_type = "BondRepo";
    explicit bond_repo_builder(bond_repo_terms terms);
    [[nodiscard]] bond_result build() const;

private:
    bond_repo_terms terms_;
};

/// The bond's price return against a quarterly fixed funding leg.
struct bond_trs_terms final {
    bond_holding holding;
    required<utility::decimal::decimal> funding_rate;
    required<std::chrono::year_month_day> start;
    required<std::chrono::year_month_day> end;
};

class ORES_TRADING_API_EXPORT bond_trs_builder final {
public:
    static constexpr std::string_view trade_type = "BondTRS";
    explicit bond_trs_builder(bond_trs_terms terms);
    [[nodiscard]] bond_result build() const;

private:
    bond_trs_terms terms_;
};

/// A callable bond, whose issuer may call it on each call date.
struct callable_bond_terms final {
    bond_holding holding;
    required<std::vector<std::chrono::year_month_day>> calls;
};

class ORES_TRADING_API_EXPORT callable_bond_builder final {
public:
    static constexpr std::string_view trade_type = "CallableBond";
    explicit callable_bond_builder(callable_bond_terms terms);
    [[nodiscard]] bond_result build() const;

private:
    callable_bond_terms terms_;
};

/// A convertible bond, convertible into one share at a ratio.
struct convertible_bond_terms final {
    bond_holding holding;
    required<std::string> share;
    required<double> conversion_ratio;
};

class ORES_TRADING_API_EXPORT convertible_bond_builder final {
public:
    static constexpr std::string_view trade_type = "ConvertibleBond";
    explicit convertible_bond_builder(convertible_bond_terms terms);
    [[nodiscard]] bond_result build() const;

private:
    convertible_bond_terms terms_;
};

/**
 * @brief An asset swapped convertible: an option on the convertible, which
 * the holder recalls against a quarterly floating swap leg plus a spread.
 */
struct ascot_terms final {
    convertible_bond_terms convertible;
    required<option_kind> kind;
    required<marketdata::datum::market_reference> funding_index;
    required<utility::decimal::decimal> recall_spread;
    required<std::chrono::year_month_day> start;
    required<std::chrono::year_month_day> end;
};

class ORES_TRADING_API_EXPORT ascot_builder final {
public:
    static constexpr std::string_view trade_type = "Ascot";
    explicit ascot_builder(ascot_terms terms);
    [[nodiscard]] bond_result build() const;

private:
    ascot_terms terms_;
};

}

#endif
