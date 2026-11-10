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
#include "ores.trading.api/builders/required.hpp"
#include "ores.trading.api/builders/vanilla_swap_builder.hpp"
#include "ores.trading.api/domain/instrument.hpp"
#include "ores.trading.api/export.hpp"
#include "ores.utility/decimal/decimal.hpp"
#include <chrono>
#include <expected>
#include <string>
#include <string_view>

namespace ores::trading::builders {

/// Which side of the default risk a trade is on.
enum class protection { buy, sell };

enum class option_position { long_option, short_option };

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

}

#endif
