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
#include "ores.trading.api/builders/credit_builders.hpp"
#include "leg_rows.hpp"
#include "ores.trading.api/builders/cadence.hpp"
#include <algorithm>
#include <optional>
#include <utility>

namespace ores::trading::builders {

namespace {

using marketdata::conventions::convention_lookup;
using refdata::domain::cds_convention;
using utility::decimal::decimal;

/// An LEI is twenty letters and digits.
bool is_lei(std::string_view text) {
    return text.size() == 20 && std::ranges::all_of(text, [](char c) {
               return (c >= '0' && c <= '9') || (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z');
           });
}

std::optional<build_error> entity_refusal_of(std::string_view reference_entity) {
    if (!is_lei(reference_entity))
        return build_error{"reference_entity", "must be an LEI of twenty letters and digits"};
    return std::nullopt;
}

std::optional<build_error> refusal_of(const credit_protection_terms& terms) {
    if (terms.notional.get() <= decimal{})
        return build_error{"notional", "must be positive"};
    if (terms.currency.get().empty())
        return build_error{"currency", "must not be empty"};
    if (terms.maturity.get() <= terms.start.get())
        return build_error{"maturity", "must be after the start"};
    if (terms.running_spread.get() < decimal{})
        return build_error{"running_spread", "must not be negative"};
    return entity_refusal_of(terms.reference_entity.get());
}

/// The CDS convention and the tenor of the premium leg it states.
struct premium_conventions final {
    cds_convention convention;
    std::string tenor;
};

std::expected<premium_conventions, build_error>
premium_conventions_of(const convention_lookup& conventions, const std::string& id) {
    auto found = conventions.cds_convention_of(id);
    if (!found)
        return std::unexpected(
            build_error{"convention", found.error().reference + ": " + found.error().reason});
    const auto period = cadence_of(found->frequency);
    if (!period)
        return std::unexpected(
            build_error{"convention", id + ": no tenor for " + found->frequency});
    return premium_conventions{std::move(*found), std::string(period->tenor)};
}

std::string protection_payment_time_of(const cds_convention& convention) {
    return convention.pays_at_default_time ? "atDefault" : "atPeriodEnd";
}

domain::credit_instrument header_of(std::string_view trade_type,
                                    const std::string& reference_entity,
                                    std::chrono::year_month_day start,
                                    std::chrono::year_month_day maturity) {
    domain::credit_instrument row{};
    row.identity.trade_type_code = std::string(trade_type);
    row.reference_entity_id_type = "LEI";
    row.reference_entity_id = reference_entity;
    row.start_date = start;
    row.maturity_date = maturity;
    return row;
}

/// A credit leg belongs to one of its product's groups of payments.
domain::swap_leg grouped_leg(
    int leg_number, std::string group, bool payer, std::string type, const std::string& currency) {
    auto row = rows::swap_leg(leg_number, payer, std::move(type), currency);
    row.leg_group = std::move(group);
    return row;
}

/*
 * A coupon leg on the CDS convention's schedule, paying the running spread on
 * the notional. The protection buyer pays it.
 */
void add_coupon_leg(domain::credit_instrument_data& out,
                    int leg_number,
                    std::string group,
                    bool payer,
                    const credit_protection_terms& terms,
                    const premium_conventions& premium) {
    const auto& convention = premium.convention;
    auto leg = grouped_leg(leg_number, std::move(group), payer, "Fixed", terms.currency.get());
    leg.day_count_fraction_code = convention.day_count_fraction;
    leg.business_day_convention_code = convention.payment_convention;
    leg.payment_frequency_code = convention.frequency;
    out.legs.push_back(std::move(leg));
    out.leg_amounts.push_back(rows::leg_amount(leg_number, terms.notional.get()));

    out.leg_rates.push_back(rows::leg_rate(leg_number, "fixed", terms.running_spread.get()));

    domain::instrument_schedule schedule{};
    schedule.owner_role = "swap";
    schedule.owner_number = leg_number;
    schedule.schedule_role = "schedule";
    schedule.sequence_number = 1;
    schedule.schedule_kind = "rules";
    schedule.start_date = terms.start.get();
    schedule.end_date = terms.maturity.get();
    schedule.tenor = premium.tenor;
    schedule.calendar = convention.calendar;
    schedule.convention = convention.payment_convention;
    schedule.rule = convention.rule;
    out.schedules.push_back(std::move(schedule));
}

template <typename Fact>
Fact protection_fact_of(const credit_protection_terms& terms, const cds_convention& convention) {
    Fact fact;
    fact.credit_curve_id = terms.credit_curve.get().uri();
    fact.settles_accrual = convention.settles_accrual;
    fact.protection_payment_time = protection_payment_time_of(convention);
    return fact;
}

/// The header, fact and premium leg an index swap states, alone or inside an option.
std::expected<domain::credit_instrument_data, build_error>
index_swap_of(std::string_view trade_type,
              const convention_lookup& conventions,
              const credit_protection_terms& terms) {
    if (auto refusal = refusal_of(terms))
        return std::unexpected(std::move(*refusal));
    const auto premium = premium_conventions_of(conventions, terms.convention.get());
    if (!premium)
        return std::unexpected(premium.error());

    domain::credit_instrument_data out{};
    out.instrument = header_of(
        trade_type, terms.reference_entity.get(), terms.start.get(), terms.maturity.get());
    out.index_credit_default_swap_instruments.push_back(
        protection_fact_of<domain::index_credit_default_swap_instrument>(terms,
                                                                         premium->convention));
    add_coupon_leg(out, 1, "premium", terms.side.get() == protection::buy, terms, *premium);
    return out;
}

}

credit_default_swap_builder::credit_default_swap_builder(const convention_lookup& conventions,
                                                         credit_protection_terms terms)
    : conventions_(conventions)
    , terms_(std::move(terms)) {}

std::expected<domain::credit_instrument_data, build_error>
credit_default_swap_builder::build() const {
    if (auto refusal = refusal_of(terms_))
        return std::unexpected(std::move(*refusal));
    const auto premium = premium_conventions_of(conventions_, terms_.convention.get());
    if (!premium)
        return std::unexpected(premium.error());

    domain::credit_instrument_data out{};
    out.instrument = header_of(
        trade_type, terms_.reference_entity.get(), terms_.start.get(), terms_.maturity.get());
    auto fact =
        protection_fact_of<domain::credit_default_swap_instrument>(terms_, premium->convention);
    fact.issuer_id = terms_.reference_entity.get();
    fact.protection_start_date = terms_.start.get();
    fact.upfront_date = terms_.start.get();
    fact.upfront_fee = decimal{};
    out.credit_default_swap_instruments.push_back(std::move(fact));
    add_coupon_leg(out, 1, "premium", terms_.side.get() == protection::buy, terms_, *premium);
    return out;
}

index_credit_default_swap_builder::index_credit_default_swap_builder(
    const convention_lookup& conventions, credit_protection_terms terms)
    : conventions_(conventions)
    , terms_(std::move(terms)) {}

std::expected<domain::credit_instrument_data, build_error>
index_credit_default_swap_builder::build() const {
    return index_swap_of(trade_type, conventions_, terms_);
}

index_credit_default_swap_option_builder::index_credit_default_swap_option_builder(
    const convention_lookup& conventions, index_credit_default_swap_option_terms terms)
    : conventions_(conventions)
    , terms_(std::move(terms)) {}

std::expected<domain::credit_instrument_data, build_error>
index_credit_default_swap_option_builder::build() const {
    if (terms_.expiry.get() >= terms_.swap.start.get())
        return std::unexpected(build_error{"expiry", "must be before the swap starts"});
    if (terms_.strike.get() <= decimal{})
        return std::unexpected(build_error{"strike", "must be positive"});
    if (terms_.index_term.get().empty())
        return std::unexpected(build_error{"index_term", "must not be empty"});

    auto out = index_swap_of(trade_type, conventions_, terms_.swap);
    if (!out)
        return out;

    // The swap's fact row is the wrapped swap's; the option states its own.
    out->wrapped_index_swap = out->index_credit_default_swap_instruments.front();
    out->index_credit_default_swap_instruments.clear();
    out->wrapped_index_swap->protection_start_date = terms_.swap.start.get();
    out->wrapped_index_swap->upfront_fee = decimal{};

    domain::index_credit_default_swap_option_instrument fact{};
    fact.knock_out = false;
    fact.index_term = terms_.index_term.get();
    fact.strike_type = "Price";
    out->index_credit_default_swap_option_instruments.push_back(std::move(fact));

    domain::instrument_strike strike{};
    strike.strike_role = "single";
    strike.sequence_number = 1;
    strike.bare_value = terms_.strike.get();
    out->strikes.push_back(std::move(strike));

    domain::instrument_option option{};
    option.long_short = long_short_of(terms_.position.get());
    option.style = "European";
    option.settlement = "Cash";
    option.pay_off_at_expiry = "false";
    out->options.push_back(std::move(option));

    domain::instrument_schedule exercise{};
    exercise.owner_role = "option";
    exercise.owner_number = 1;
    exercise.schedule_role = "exercise_dates";
    exercise.sequence_number = 1;
    exercise.schedule_kind = "dates";
    out->schedules.push_back(std::move(exercise));

    domain::instrument_schedule_date date{};
    date.owner_role = "option";
    date.owner_number = 1;
    date.schedule_role = "exercise_dates";
    date.schedule_sequence_number = 1;
    date.sequence_number = 1;
    date.schedule_date = terms_.expiry.get();
    out->schedule_dates.push_back(std::move(date));
    return out;
}

credit_linked_swap_builder::credit_linked_swap_builder(const convention_lookup& conventions,
                                                       credit_protection_terms terms)
    : conventions_(conventions)
    , terms_(std::move(terms)) {}

std::expected<domain::credit_instrument_data, build_error>
credit_linked_swap_builder::build() const {
    if (auto refusal = refusal_of(terms_))
        return std::unexpected(std::move(*refusal));
    const auto premium = premium_conventions_of(conventions_, terms_.convention.get());
    if (!premium)
        return std::unexpected(premium.error());

    domain::credit_instrument_data out{};
    out.instrument = header_of(
        trade_type, terms_.reference_entity.get(), terms_.start.get(), terms_.maturity.get());
    domain::credit_linked_swap_instrument fact{};
    fact.credit_curve_id = terms_.credit_curve.get().uri();
    fact.settles_accrual = premium->convention.settles_accrual;
    fact.default_payment_time = protection_payment_time_of(premium->convention);
    out.credit_linked_swap_instruments.push_back(std::move(fact));

    // ORE takes the trade's currency from its independent payments, so the
    // note's funding at the start is one. The funding flows the other way to
    // the coupons and the redemption: the protection seller pays it and
    // receives them.
    const bool payer = terms_.side.get() == protection::buy;
    out.legs.push_back(grouped_leg(1, "independent", !payer, "Cashflow", terms_.currency.get()));
    out.leg_amounts.push_back(rows::leg_amount(1, terms_.notional.get(), terms_.start.get()));
    add_coupon_leg(out, 2, "contingent", payer, terms_, *premium);
    out.legs.push_back(grouped_leg(3, "contingent", payer, "Cashflow", terms_.currency.get()));
    out.leg_amounts.push_back(rows::leg_amount(3, terms_.notional.get(), terms_.maturity.get()));
    return out;
}

risk_participation_agreement_builder::risk_participation_agreement_builder(
    const convention_lookup& conventions, risk_participation_agreement_terms terms)
    : conventions_(conventions)
    , terms_(std::move(terms)) {}

std::expected<domain::credit_instrument_data, build_error>
risk_participation_agreement_builder::build() const {
    const auto rate = terms_.participation_rate.get();
    if (!(rate > 0.0 && rate <= 1.0))
        return std::unexpected(
            build_error{"participation_rate", "must be above zero and at most one"});
    if (terms_.fee.get() < decimal{})
        return std::unexpected(build_error{"fee", "must not be negative"});
    if (auto refusal = entity_refusal_of(terms_.reference_entity.get()))
        return std::unexpected(std::move(*refusal));
    const auto premium = premium_conventions_of(conventions_, terms_.convention.get());
    if (!premium)
        return std::unexpected(premium.error());

    auto swap = vanilla_swap_builder(conventions_, terms_.underlying).build();
    if (!swap)
        return std::unexpected(
            build_error{"underlying." + swap.error().term, std::move(swap.error().message)});

    const auto& underlying = terms_.underlying;
    domain::credit_instrument_data out{};
    out.instrument = header_of(trade_type,
                               terms_.reference_entity.get(),
                               underlying.start.get(),
                               underlying.maturity.get());
    domain::risk_participation_agreement_instrument fact{};
    fact.credit_curve_id = terms_.credit_curve.get().uri();
    fact.participation_rate = rate;
    fact.issuer_id = terms_.reference_entity.get();
    fact.settles_accrual = premium->convention.settles_accrual;
    out.risk_participation_agreement_instruments.push_back(std::move(fact));

    for (auto& leg : swap->legs) {
        leg.leg_group = "underlying";
        out.legs.push_back(std::move(leg));
    }
    out.leg_amounts = std::move(swap->leg_amounts);
    out.leg_rates = std::move(swap->leg_rates);
    out.schedules = std::move(swap->schedules);

    const int fee_leg = static_cast<int>(out.legs.size()) + 1;
    out.legs.push_back(grouped_leg(fee_leg,
                                   "protection_fee",
                                   terms_.side.get() == protection::buy,
                                   "Cashflow",
                                   underlying.currency.get()));
    out.leg_amounts.push_back(rows::leg_amount(fee_leg, terms_.fee.get(), underlying.start.get()));
    return out;
}

}
