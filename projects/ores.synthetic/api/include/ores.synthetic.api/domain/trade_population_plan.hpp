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
#include "ores.trading.api/domain/instrument_batch.hpp"
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

/**
 * @brief One currency pair the tenant's reference data holds.
 *
 * A pair is the tenant's own, in the way a commodity is, and it states the two
 * currencies in the order the catalogue states them: the base is bought and the
 * quote sold when the rate is quoted the way the market quotes it.
 */
struct currency_pair_reference final {
    std::string base;
    std::string quote;
};

/**
 * @brief One person who may enter a trade for a party: their account, and the name the rows state.
 *
 * The account is what a booking's session is minted for, so the rows the booking writes and the
 * authorisation request it raises all name that person.
 */
/**
 * @brief One equity underlying as the tenant's catalogue states it.
 *
 * The code is what a trade names, and the kind is the vocabulary that holds it,
 * because a basket constituent states both and the store refuses a code its
 * kind's catalogue does not hold. The currency is what the underlying is
 * quoted in, and it is empty when the source never said.
 */
struct equity_underlying_reference final {
    std::string code;
    std::string kind;
    std::string currency;
};

struct trade_population_trader final {
    boost::uuids::uuid account_id;
    std::string username;
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
     * @brief How many structures each book's trades are grouped into.
     *
     * Zero states that the scope groups nothing. The generator decides which
     * trades go together and in what role, because grouping is a property of
     * the population it made rather than of the configuration.
     */
    std::uint32_t structures_per_book = 0;
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
    /**
     * @brief The floating indices the tenant holds, from which a floating
     * leg's index is drawn.
     *
     * An index is the tenant's reference data and not this configuration's, in
     * the way the commodity catalogue is. It is the floating index catalogue
     * and not the benchmark rate set: a leg states the FpML name of an index,
     * such as =GBP-SONIA=, and the store refuses anything outside that
     * vocabulary. A population whose set is empty cannot state a floating leg
     * at all, so the planner refuses rather than naming an index the store does
     * not hold.
     */
    std::vector<std::string> floating_index_codes;
    /**
     * @brief The currency pairs the tenant's reference data holds.
     *
     * A pair is the tenant's own and not this configuration's, in the way the
     * commodity catalogue is. A population whose set is empty cannot state an
     * FX trade at all, so the planner refuses rather than inventing a pair the
     * store does not hold.
     */
    std::vector<currency_pair_reference> currency_pairs;
    /**
     * @brief The equity underlyings the tenant's catalogue holds.
     *
     * A population draws from this list rather than inventing a name, so every
     * constituent it writes resolves against the catalogue that holds it.
     */
    std::vector<equity_underlying_reference> equity_underlyings;
    /// The LEIs the credit family draws its reference entities from.
    std::vector<std::string> credit_reference_entities;
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
     * @brief The staff who may trade for the booking party.
     *
     * Each carries the account as well as the name, because the population is
     * entered as that person: the trade states them as its author and the
     * authorisation request the booking raises is theirs. A party with none is
     * refused rather than defaulted to the caller.
     */
    std::vector<trade_population_trader> traders;
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
/**
 * @brief One constituent of a worst-of basket.
 *
 * The kind travels with the code because the store refuses a code the kind's
 * catalogue does not hold, and the price is what the constituent opened at.
 */
struct equity_basket_member final {
    std::string name;
    std::string kind;
    std::optional<double> weight;
    std::optional<double> initial_price;
};

/**
 * @brief One determination period of a worst-of basket.
 *
 * The document states its two level lists one entry per date, so a period
 * carries both levels and the ordinal that pairs them with the schedule.
 */
struct equity_basket_period final {
    std::optional<std::chrono::year_month_day> period_date;
    std::optional<double> knock_out_level;
    std::optional<double> fixed_trigger_level;
};

/**
 * @brief One range an accumulator pays inside, and the leverage it pays at.
 */
struct equity_range_bound final {
    std::optional<double> range_from;
    std::optional<double> range_to;
    std::optional<double> leverage;
};

/**
 * @brief The facts a credit trade states.
 *
 * The reference is typed, so a plan states whether it names a legal entity or
 * an identifier of the system's own data.
 */
struct credit_terms final {
    std::string reference_entity_id_type;
    std::string reference_entity_id;
    std::chrono::year_month_day start_date;
    std::chrono::year_month_day maturity_date;
};

/**
 * @brief The facts a bond trade states.
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
 * @brief The facts the commodity planner draws, before it states its rows.
 *
 * The family states rows in the plan, so this is the drawing's own shape and
 * never a slot's bag.
 *
 * A commodity row is wide and flat: the same table holds a forward, an
 * option, an Asian or a variance swap, and each shape states the fields its
 * own terms need and leaves the rest empty. This mirrors that shape rather
 * than inventing a hierarchy the store does not have.
 */
/**
 * @brief One member of a commodity basket, as the catalogue spells it.
 */
struct commodity_basket_member final {
    std::string code;
    std::optional<double> weight;
};

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
    /// The members a basket product names, in the order it draws them.
    std::vector<commodity_basket_member> basket;
};

/**
 * @brief The facts the rate planner draws, before it states its rows.
 *
 * The family states rows in the plan, so this is the drawing's own shape and
 * never a slot's bag.
 *
 * An interest rate trade reduces to one fixed leg against one floating leg,
 * whatever its trade type: the type decides the exercise and payment features
 * the specialised tables would hold, and this states the legs every routed
 * type has. Every code here is the reference data's own, so the generator
 * invents no vocabulary of its own.
 */
struct rate_terms final {
    /// The currency both legs are denominated in.
    std::string currency;
    std::chrono::year_month_day start_date;
    std::chrono::year_month_day maturity_date;
    /// The notional both legs state, in the currency above.
    double notional = 0.0;
    /// What the fixed leg pays, as a decimal: 0.05 is five per cent, which is
    /// the unit the store holds the rate in.
    double fixed_rate = 0.0;
    std::string fixed_day_count_fraction_code;
    std::string fixed_business_day_convention_code;
    std::string fixed_payment_frequency_code;
    /**
     * @brief The index the floating leg resets on, as the floating index
     * catalogue spells it, such as =USD-SOFR= or =GBP-SONIA=.
     */
    std::string floating_index_code;
    /// What the floating leg pays over that index, as a decimal.
    double spread = 0.0;
    std::string floating_day_count_fraction_code;
    std::string floating_business_day_convention_code;
    std::string floating_payment_frequency_code;

    /**
     * @brief The currency the floating leg is denominated in, when it differs
     * from the fixed leg's. Empty for a trade in one currency, which is most
     * of them: only a cross currency swap states two.
     */
    std::string floating_currency;
    /// The strike a cap, a floor or a swaption states, as a decimal.
    std::optional<double> strike;
    /**
     * @brief The date a swaption may be exercised.
     *
     * It belongs to the trade and not to the maturity, and it falls after the
     * as-of date: an option that expired before the population was written
     * exercises nothing.
     */
    std::optional<std::chrono::year_month_day> expiry_date;
};

/**
 * @brief The terms of one slot, in the shape its asset class states them.
 *
 * The slot carries the keys every family shares and this carries the rest, so
 * a reader of a bond slot sees bond facts and a reader of another family never
 * has to ask what a security is.
 */

/**
 * @brief The facts the fx planner draws, before it states its rows.
 *
 * The family states rows in the plan, so this is the drawing's own shape and
 * never a slot's bag.
 *
 * The sixteen routed types reach seven tables, and the tables state three
 * idioms: a bought currency against a sold one, a digital payoff struck against
 * a foreign and a domestic currency, and an index with a reference notional.
 * One payload states all three with the fields a type does not use left empty,
 * which is the shape @c commodity_terms already takes for its eight.
 */
struct fx_terms final {
    /// The currency bought, and how much of it.
    std::string bought_currency;
    double bought_amount = 0.0;
    /// The currency sold, and how much of it.
    std::string sold_currency;
    double sold_amount = 0.0;
    /// The date a forward settles on.
    std::chrono::year_month_day value_date;
    /// How the trade settles: =Cash= or =Physical=.
    std::string settlement;

    /// The side an option or a one-directional trade takes.
    std::string option_type;
    /// =European=, =American= or =Bermudan=.
    std::string exercise_style;
    /// The date an option expires or a term starts.
    std::chrono::year_month_day expiry_date;
    /// =Long= or =Short=.
    std::string long_short;
    /// The strike an option, a variance swap or an accumulator states.
    std::optional<double> strike;

    /// The barrier a knock-out or a knock-in trade states.
    std::string barrier_type;
    std::optional<double> lower_barrier;
    std::optional<double> upper_barrier;

    /// The currencies a digital states, and the payoff it makes.
    std::string foreign_currency;
    std::string domestic_currency;
    std::string payoff_currency;
    std::optional<double> payoff_amount;

    /// The index and notional an average forward, a variance swap or an
    /// accumulator is written on.
    std::string underlying_code;
    std::string fx_index;
    std::string reference_currency;
    double reference_notional = 0.0;
    std::string settlement_currency;
    double settlement_notional = 0.0;
    /// The currencies and amounts an accumulator fixes against.
    std::string currency;
    double fixing_amount = 0.0;
    std::optional<double> target_amount;
    std::chrono::year_month_day start_date;
    std::chrono::year_month_day end_date;
    std::optional<std::chrono::year_month_day> payment_date;
    std::optional<double> knock_out_barrier;
    /// =Variance= or =Volatility=, the moment a variance swap states.
    std::string moment_type;
};

/**
 * @brief What the equity family's nine tables state.
 *
 * The union of the family rather than a struct per product, because the
 * planner makes one payload and the writer reads from it the fields the routed
 * table states. A member a product does not state keeps its default and the
 * writer never reads it.
 *
 * A basket is rows and not a string: the constituents carry their kind and the
 * price each opened at, and the periods carry the levels the determination
 * dates state, so a worst-of basket of two names over four dates states two
 * constituents and four periods.
 */
struct equity_terms final {
    /// The underlying a single-name product states.
    std::string underlying_name;
    /// The kind the underlying belongs to: =Equity=, =FX= or =Commodity=.
    std::string underlying_kind;
    std::string currency;
    /// The notional a swap or an option states, or the quantity of a forward
    /// or a position.
    double notional = 0.0;
    double quantity = 0.0;
    /// The price a forward, a position or an option states.
    std::optional<double> fixed_price;
    std::optional<double> strike;

    std::chrono::year_month_day start_date;
    std::chrono::year_month_day maturity_date;
    std::chrono::year_month_day expiry_date;
    std::optional<std::chrono::year_month_day> averaging_start_date;
    std::optional<std::chrono::year_month_day> averaging_end_date;

    /// =Call= or =Put=, and =European=, =American= or =Bermudan=.
    std::string option_type;
    std::string exercise_type;
    /// =Cash= or =Physical=.
    std::string settlement_type;
    /// =Long= or =Short=.
    std::string long_short;

    /// The barriers a barrier or a digital product states.
    std::optional<double> lower_barrier;
    std::optional<double> upper_barrier;
    std::string lower_barrier_type;
    std::string upper_barrier_type;
    std::optional<double> barrier_level;
    std::string barrier_type;
    std::optional<double> rebate;
    std::optional<double> payout_amount;

    /// How a swap returns, and how often it pays.
    std::string return_type;
    std::string payment_frequency_code;
    /// How an Asian averages, and how often a cliquet observes.
    std::string average_type;
    std::string cliquet_frequency;
    /// =Variance= or =Volatility=, the moment a variance swap states.
    std::string moment_type;
    std::optional<double> variance_strike;

    /// The accumulator's own terms.
    std::string fixing_frequency;
    std::optional<double> knock_out_level;
    std::optional<double> target_amount;
    std::string target_type;
    std::string payoff_type;
    /// The TaRF's points and the cliquet's caps.
    std::optional<double> target_points;
    std::optional<double> moneyness;
    std::optional<double> local_cap;
    std::optional<double> local_floor;
    std::optional<double> global_cap;
    std::optional<double> global_floor;
    std::uint32_t settlement_days = 0;
    std::optional<double> premium;
    std::string premium_currency;
    std::optional<std::chrono::year_month_day> premium_payment_date;

    /// The constituents of a worst-of basket, in the document's order.
    std::vector<equity_basket_member> basket;
    /// The periods a worst-of basket observes, in the document's order.
    std::vector<equity_basket_period> periods;
    /// The ranges an accumulator pays inside.
    std::vector<equity_range_bound> range_bounds;
};

/**
 * @brief The bag a family fills while it still draws facts rather than rows.
 *
 * The monostate comes first so a slot whose family states its rows holds no
 * bag at all. Without it the variant's default is the first alternative, and
 * a slot that states rows would read as a bond with empty terms.
 */
using trade_terms = std::variant<std::monostate, bond_terms>;

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
    /// The person the trade is entered as, who then asks for its authorisation.
    trade_population_trader trader;
    std::string trade_type_code;
    std::string internal_identifier;
    std::chrono::year_month_day trade_date;
    double notional = 0.0;
    std::string currency;
    bool intra_group = false;
    /**
     * @brief The facts that belong to this slot's own family, and to no other.
     *
     * A family that states rows holds the monostate here, so nothing reads its
     * empty slot as facts of another family.
     */
    trade_terms terms;
    /**
     * @brief The domain rows this slot states, when its family has been flipped.
     *
     * A family that has flipped fills this and leaves =terms= empty; the writer
     * persists what it is given instead of translating a bag. The two coexist
     * until the last family moves.
     */
    ores::trading::domain::instrument_batch instruments;
    /// The structure this trade is a leg of, when the population groups one.
    std::optional<boost::uuids::uuid> structure_id;
    /// The role it takes in that structure, and its place among the legs.
    std::string structure_role;
    std::uint32_t structure_sequence = 0;
};

/**
 * @brief A deal a customer sees, holding trades the same run generated.
 *
 * The population groups its own trades, so a fresh environment has structures
 * whose legs are real trades rather than rows invented for a screen. Only the
 * kind is stated: a package is a structure whose legs confirm on their own, and
 * it needs no template, which is what makes grouping possible before a tenant
 * has chosen a template of its own.
 */
struct trade_population_structure final {
    boost::uuids::uuid structure_id;
    /// The party on the other side, which the structure states once for all
    /// its legs.
    boost::uuids::uuid counterparty_id;
    std::string kind;
    std::string template_code;
};

/**
 * @brief The population a scope states, one entry per trade.
 */
struct trade_population_plan final {
    boost::uuids::uuid config_id;
    boost::uuids::uuid party_id;
    std::chrono::year_month_day as_of_date;
    std::vector<trade_population_slot> slots;
    std::vector<trade_population_structure> structures;
};

}

#endif
