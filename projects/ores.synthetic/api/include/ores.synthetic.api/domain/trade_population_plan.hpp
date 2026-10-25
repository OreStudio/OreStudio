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
#ifndef ORES_SYNTHETIC_API_DOMAIN_TRADE_POPULATION_PLAN_HPP
#define ORES_SYNTHETIC_API_DOMAIN_TRADE_POPULATION_PLAN_HPP

#include "ores.synthetic.api/export.hpp"
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <variant>
#include <vector>

namespace ores::synthetic::domain {

/**
 * @brief The bands and the scope a trade generation config states, as the
 * planner reads them.
 *
 * A plain value and not the config entity, because the planner decides what a
 * population looks like from these inputs alone: the entity's audit tail and
 * its surrogate keys say nothing about the trades.
 *
 * =party_id= is the booking party, and it is the party the writer must act
 * as. The trading write path sets the instrument's party from the session's
 * =app.current_party_id=, so a caller cannot state it on a row; the planner
 * carries it only so the caller can check it is acting as the right entity.
 */
/**
 * @brief One commodity as the tenant's catalogue states it.
 *
 * The code is what a trade names and the unit is how the commodity is
 * quoted, so a population that draws this states both without asking the
 * catalogue again per trade.
 */
struct commodity_reference final {
    std::string code;
    std::string unit;
    std::string quotation_currency;
};

struct trade_generation_scope final {
    boost::uuids::uuid config_id;
    boost::uuids::uuid party_id;
    std::string asset_class_code;
    std::string min_tenor_code;
    std::string max_tenor_code;
    double min_notional = 0.0;
    double max_notional = 0.0;
    std::chrono::year_month_day as_of_date;
    std::uint64_t seed = 0;
    /**
     * @brief How many trades each book of the scope receives.
     *
     * The size belongs to the configuration and not to the generator: a tenant
     * that wants a thinner or a fuller book changes a row. It is a count per
     * book, so a scope over four books states four times this many trades.
     */
    std::uint32_t trades_per_book = 8;
    /**
     * @brief The commodities the tenant's catalogue holds.
     *
     * A commodity is a fact about the tenant's reference data and not about
     * this configuration, in the way an issuer is: two configs of one tenant
     * draw from the same catalogue. The unit travels with the code because it
     * is the catalogue's fact too, and a population whose catalogue is empty
     * cannot state a commodity at all.
     */
    std::vector<commodity_reference> commodities;
    std::vector<boost::uuids::uuid> book_ids;
    std::vector<boost::uuids::uuid> counterparty_ids;
    /**
     * @brief Every counterparty the tenant holds, from which the issuer of a
     * security is drawn. A security belongs to the tenant and not to a config,
     * so its issuer cannot depend on the counterparties one config scopes.
     */
    std::vector<boost::uuids::uuid> issuer_ids;
    /**
     * @brief The netting set each counterparty of the scope is cleared under,
     * keyed by counterparty. Absent for a counterparty with no set, which is
     * ordinary: a booking without a set is one the database accepts.
     */
    std::vector<std::pair<boost::uuids::uuid, boost::uuids::uuid>> netting_sets;
    /**
     * @brief The staff who may trade for the booking party, one username each.
     * A party with none is refused rather than defaulted to the caller.
     */
    std::vector<std::string> trader_usernames;
    /**
     * @brief The currency the population's books are kept in. A bond's own
     * issue carries no currency: the coupon terms are the leg's, so the unit
     * of the notional travels on the leg and comes from the book.
     */
    std::string currency_code = "USD";
    /**
     * @brief The curve the population's bonds are discounted on, as the oresmd
     * URI of the market series that curve is. Empty when the config states
     * none, and a bond then names no curve.
     */
    std::string reference_curve_id;
};

/**
 * @brief The facts a bond slot carries and no other family does.
 *
 * A bond is a security, so its terms are the security's and not the trade's:
 * several trades of one security state the same issue and the same maturity.
 * Keeping them here rather than on the slot is what lets a slot describe a
 * family that has no security at all.
 */
struct bond_terms final {
    std::string security_id;
    boost::uuids::uuid issue_id;
    /**
     * @brief The counterparty that issued the security. It is a fact of the
     * security and not of the trade, so it need not be the trade's own.
     */
    boost::uuids::uuid issuer_id;
    std::chrono::year_month_day issue_date;
    std::chrono::year_month_day maturity_date;
    /// The security's original term in years, not the trade's remaining life.
    std::uint32_t tenor_years = 1;
    std::string calendar = "TARGET";
    /**
     * @brief The curve this trade's bond is discounted on, as the oresmd URI of
     * the market series that curve is, or empty when the population states
     * none.
     */
    std::string reference_curve_id;
};

/**
 * @brief The facts a commodity slot carries and no other family does.
 *
 * A commodity row is wide and flat: the same table holds a forward, an
 * option, an Asian or a variance swap, and each shape states the fields its
 * own terms need and leaves the rest empty. This mirrors that shape rather
 * than inventing a hierarchy the store does not have.
 */
struct commodity_terms final {
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
};

/**
 * @brief The terms of one slot, in the shape its asset class states them.
 *
 * The slot carries the keys every family shares and this carries the rest, so
 * a reader of a bond slot sees bond facts and a reader of another family never
 * has to ask what a security is.
 */
using trade_terms = std::variant<bond_terms, commodity_terms>;

/**
 * @brief One trade the population states, with every reference already
 * resolved.
 *
 * The ids are derived from the scope rather than drawn, so the same (seed,
 * as-of) pair states the same trade id on every run and a re-provision
 * upserts rather than duplicating. That is what =slot= makes concrete: it is
 * the trade's position in the population, and it is the only input to the
 * derivation that is not the scope itself.
 */
struct trade_population_slot final {
    std::uint32_t slot = 0;
    boost::uuids::uuid trade_id;
    boost::uuids::uuid book_id;
    boost::uuids::uuid counterparty_id;
    std::optional<boost::uuids::uuid> netting_set_id;
    std::string trader_username;
    std::string trade_type_code;
    std::string internal_identifier;
    std::chrono::year_month_day trade_date;
    double notional = 0.0;
    std::string currency;
    bool intra_group = false;
    /// The facts that belong to this slot's own family, and to no other.
    trade_terms terms;
};

/**
 * @brief The population a scope states, one entry per trade.
 */
struct trade_population_plan final {
    boost::uuids::uuid config_id;
    boost::uuids::uuid party_id;
    std::chrono::year_month_day as_of_date;
    std::vector<trade_population_slot> slots;
};

}

#endif
