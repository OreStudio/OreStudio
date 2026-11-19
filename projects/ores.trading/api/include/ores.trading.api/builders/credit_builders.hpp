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
#ifndef ORES_TRADING_API_BUILDERS_CREDIT_BUILDERS_HPP
#define ORES_TRADING_API_BUILDERS_CREDIT_BUILDERS_HPP

#include "ores.marketdata.api/conventions/convention_lookup.hpp"
#include "ores.marketdata.api/datum/market_reference.hpp"
#include "ores.trading.api/builders/build_error.hpp"
#include "ores.trading.api/builders/option_position.hpp"
#include "ores.trading.api/builders/required.hpp"
#include "ores.trading.api/builders/vanilla_swap_builder.hpp"
#include "ores.trading.api/domain/instrument.hpp"
#include "ores.trading.api/export.hpp"
#include "ores.utility/decimal/decimal.hpp"
#include <chrono>
#include <expected>
#include <optional>
#include <string>
#include <string_view>
#include <vector>

namespace ores::trading::builders {

/// Which side of the default risk a trade is on.
enum class protection { buy, sell };

/**
 * @brief The terms of protection on one reference entity.
 *
 * The CDS convention, which ORE names by id, supplies the premium leg's
 * frequency, day count, calendar, adjustment and date rule, and how accrual and
 * the protection payment settle.
 */
struct credit_protection_terms final {
    required<utility::decimal::decimal> notional;
    required<std::string> currency;
    required<std::chrono::year_month_day> start;
    required<std::chrono::year_month_day> maturity;
    required<utility::decimal::decimal> running_spread;
    /// The LEI of the entity the protection is on.
    required<std::string> reference_entity;
    required<marketdata::datum::market_reference> credit_curve;
    required<protection> side;
    required<std::string> convention;
};

/// Protection bought or sold on a single name, paid for by a running premium.
class ORES_TRADING_API_EXPORT credit_default_swap_builder final {
public:
    static constexpr std::string_view trade_type = "CreditDefaultSwap";

    credit_default_swap_builder(const marketdata::conventions::convention_lookup& conventions,
                                credit_protection_terms terms);
    credit_default_swap_builder(marketdata::conventions::convention_lookup&&,
                                credit_protection_terms) = delete;

    [[nodiscard]] std::expected<domain::credit_instrument_data, build_error> build() const;

private:
    const marketdata::conventions::convention_lookup& conventions_;
    credit_protection_terms terms_;
};

/// Protection on an index, paid for by a running premium.
class ORES_TRADING_API_EXPORT index_credit_default_swap_builder final {
public:
    static constexpr std::string_view trade_type = "IndexCreditDefaultSwap";

    index_credit_default_swap_builder(const marketdata::conventions::convention_lookup& conventions,
                                      credit_protection_terms terms);
    index_credit_default_swap_builder(marketdata::conventions::convention_lookup&&,
                                      credit_protection_terms) = delete;

    [[nodiscard]] std::expected<domain::credit_instrument_data, build_error> build() const;

private:
    const marketdata::conventions::convention_lookup& conventions_;
    credit_protection_terms terms_;
};

/// The terms of a European option on an index CDS that starts after the expiry.
struct index_credit_default_swap_option_terms final {
    credit_protection_terms swap;
    required<std::chrono::year_month_day> expiry;
    /// The strike as a price, where 1 is par.
    required<utility::decimal::decimal> strike;
    /// The index's own term, such as 5Y, which ORE reads the volatility by.
    required<std::string> index_term;
    required<option_position> position;
};

class ORES_TRADING_API_EXPORT index_credit_default_swap_option_builder final {
public:
    static constexpr std::string_view trade_type = "IndexCreditDefaultSwapOption";

    index_credit_default_swap_option_builder(
        const marketdata::conventions::convention_lookup& conventions,
        index_credit_default_swap_option_terms terms);
    index_credit_default_swap_option_builder(marketdata::conventions::convention_lookup&&,
                                             index_credit_default_swap_option_terms) = delete;

    [[nodiscard]] std::expected<domain::credit_instrument_data, build_error> build() const;

private:
    const marketdata::conventions::convention_lookup& conventions_;
    index_credit_default_swap_option_terms terms_;
};

/**
 * @brief An option to enter a single name credit default swap at a strike
 * spread, on a date before the swap starts.
 */
struct credit_default_swap_option_terms final {
    credit_protection_terms swap;
    required<std::chrono::year_month_day> expiry;
    /// The strike as a spread, such as 0.01 for a hundred basis points.
    required<utility::decimal::decimal> strike;
    required<option_kind> kind;
    required<option_position> position;
};

class ORES_TRADING_API_EXPORT credit_default_swap_option_builder final {
public:
    static constexpr std::string_view trade_type = "CreditDefaultSwapOption";

    credit_default_swap_option_builder(
        const marketdata::conventions::convention_lookup& conventions,
        credit_default_swap_option_terms terms);
    credit_default_swap_option_builder(marketdata::conventions::convention_lookup&&,
                                       credit_default_swap_option_terms) = delete;

    [[nodiscard]] std::expected<domain::credit_instrument_data, build_error> build() const;

private:
    const marketdata::conventions::convention_lookup& conventions_;
    credit_default_swap_option_terms terms_;
};

/**
 * @brief A funded note on the reference entity.
 *
 * The protection seller pays the notional at the start, whatever happens. It
 * then receives the running spread as a coupon on the premium schedule and the
 * notional at maturity, while the entity survives. The buyer takes the other
 * side of each payment.
 */
class ORES_TRADING_API_EXPORT credit_linked_swap_builder final {
public:
    static constexpr std::string_view trade_type = "CreditLinkedSwap";

    credit_linked_swap_builder(const marketdata::conventions::convention_lookup& conventions,
                               credit_protection_terms terms);
    credit_linked_swap_builder(marketdata::conventions::convention_lookup&&,
                               credit_protection_terms) = delete;

    [[nodiscard]] std::expected<domain::credit_instrument_data, build_error> build() const;

private:
    const marketdata::conventions::convention_lookup& conventions_;
    credit_protection_terms terms_;
};

/// The terms of a share of the counterparty risk on a vanilla swap.
struct risk_participation_agreement_terms final {
    vanilla_swap_terms underlying;
    required<double> participation_rate;
    required<std::string> reference_entity;
    required<marketdata::datum::market_reference> credit_curve;
    required<protection> side;
    /// Paid once, at the start, by the protection buyer.
    required<utility::decimal::decimal> fee;
    required<std::string> convention;
};

class ORES_TRADING_API_EXPORT risk_participation_agreement_builder final {
public:
    static constexpr std::string_view trade_type = "RiskParticipationAgreement";

    risk_participation_agreement_builder(
        const marketdata::conventions::convention_lookup& conventions,
        risk_participation_agreement_terms terms);
    risk_participation_agreement_builder(marketdata::conventions::convention_lookup&&,
                                         risk_participation_agreement_terms) = delete;

    [[nodiscard]] std::expected<domain::credit_instrument_data, build_error> build() const;

private:
    const marketdata::conventions::convention_lookup& conventions_;
    risk_participation_agreement_terms terms_;
};

/**
 * @brief The terms of a tranche of protection on a credit index.
 *
 * The tranche covers the index's losses between the attachment and the
 * detachment points, as fractions of the index notional. The index's names come
 * from its reference data, which ORE reads by the index the qualifier names.
 */
struct synthetic_cdo_terms final {
    /// The premium leg's notional, the price of the protection, and its side.
    credit_protection_terms protection;
    required<utility::decimal::decimal> attachment_point;
    required<utility::decimal::decimal> detachment_point;
};

class ORES_TRADING_API_EXPORT synthetic_cdo_builder final {
public:
    static constexpr std::string_view trade_type = "SyntheticCDO";

    synthetic_cdo_builder(const marketdata::conventions::convention_lookup& conventions,
                          synthetic_cdo_terms terms);
    synthetic_cdo_builder(marketdata::conventions::convention_lookup&&,
                          synthetic_cdo_terms) = delete;

    [[nodiscard]] std::expected<domain::credit_instrument_data, build_error> build() const;

private:
    const marketdata::conventions::convention_lookup& conventions_;
    synthetic_cdo_terms terms_;
};

/// One tranche of a CBO structure, with the fixed coupon its leg pays.
struct cbo_tranche_terms final {
    std::string name;
    utility::decimal::decimal notional;
    utility::decimal::decimal ic_ratio;
    utility::decimal::decimal oc_ratio;
    utility::decimal::decimal fixed_rate;
};

/// One bond of a CBO's basket, named by its security id, and the notional held.
struct cbo_basket_bond final {
    std::string security_id;
    utility::decimal::decimal notional;
};

/**
 * @brief A CBO structure stated inline: its fees, its tranches, its bond basket
 * and its payment schedule, one date a tenor from the start to the end.
 */
struct cbo_structure_terms final {
    std::string currency;
    std::string day_counter;
    std::string payment_convention;
    utility::decimal::decimal senior_fee;
    std::string fee_day_counter;
    utility::decimal::decimal subordinated_fee;
    utility::decimal::decimal equity_kicker;
    std::vector<cbo_tranche_terms> tranches;
    std::vector<cbo_basket_bond> basket;
    std::chrono::year_month_day start;
    std::chrono::year_month_day end;
    std::string tenor;
    std::string calendar;
};

/**
 * @brief An investment in one tranche of a collateralised bond obligation,
 * whose structure the reference data holds under the structure id, or the
 * trade states inline.
 */
struct cbo_terms final {
    required<std::string> tranche_name;
    required<utility::decimal::decimal> notional;
    required<std::string> structure_id;
    std::optional<cbo_structure_terms> structure;
};

class ORES_TRADING_API_EXPORT cbo_builder final {
public:
    static constexpr std::string_view trade_type = "CBO";
    explicit cbo_builder(cbo_terms terms);
    [[nodiscard]] std::expected<domain::credit_instrument_data, build_error> build() const;

private:
    cbo_terms terms_;
};

}

#endif
