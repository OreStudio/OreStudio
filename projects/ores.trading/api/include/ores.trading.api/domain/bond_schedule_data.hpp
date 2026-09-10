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
#ifndef ORES_TRADING_API_DOMAIN_BOND_SCHEDULE_DATA_HPP
#define ORES_TRADING_API_DOMAIN_BOND_SCHEDULE_DATA_HPP

#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::trading::domain {

/**
 * @brief A schedule the document states as a tenor expanded against a calendar.
 *
 * Dates are ISO 8601 and the codes (calendar, convention, rule and the
 * rest) are the canonical spellings the ORE schema uses. A member the
 * schema declares optional is an optional here, so an element the
 * document states empty stays distinct from one it omits.
 *
 * Two of the members are flags the schema types as its own bool rather
 * than as the XML boolean. That type enumerates thirteen spellings,
 * including the empty one, and the generated writer emits the spelling
 * the reader stored. Carrying the text keeps the spelling the document
 * chose: the corpus writes these elements empty for the on state. The
 * two remove flags are XML booleans and stay boolean here.
 */
struct bond_schedule_rules final {
    std::string start_date;
    std::optional<std::string> end_date;
    std::optional<std::string> adjust_end_date_to_previous_month_end;
    std::string tenor;
    std::optional<std::string> calendar;
    std::string convention;
    std::optional<std::string> term_convention;
    std::optional<std::string> rule;
    std::optional<std::string> end_of_month;
    std::optional<std::string> end_of_month_convention;
    std::optional<std::string> first_date;
    std::optional<std::string> last_date;
    std::optional<bool> remove_first_date;
    std::optional<bool> remove_last_date;
};

/**
 * @brief A schedule the document states as an explicit date list.
 *
 * The two flags carry the document's spelling of the schema's own bool
 * type, as the rule block above does.
 */
struct bond_schedule_dates final {
    std::optional<std::string> calendar;
    std::optional<std::string> convention;
    std::optional<std::string> tenor;
    std::optional<std::string> end_of_month;
    std::optional<std::string> include_duplicate_dates;
    std::vector<std::string> dates;
};

/**
 * @brief A document schedule that no column of the nine bond tables holds.
 *
 * The ORE schema states a schedule as a choice between a rule block
 * and a date list, and the generated code mirrors that choice as two
 * lists. This type mirrors the generated shape, so the mapping between
 * the two is total and needs no case analysis.
 *
 * The nine tables carry no schedule column. The org models record the
 * destination as the parent story's shared instrument-keyed schedule
 * tables, and until those land the container carries the structure
 * whole so that export re-emits what the document held. This type is
 * handcrafted for the same reason the container is, and codegen
 * absorbs it when the container grammar lands.
 */
struct bond_schedule_data final {
    std::vector<bond_schedule_rules> rules;
    std::vector<bond_schedule_dates> dates;
};

/**
 * @brief A number the document states with an optional start date.
 *
 * The fixed rates, the spreads, the caps, the floors and the gearings all
 * carry this shape, and so does a notional. The value is required in the
 * schema, so an unengaged start date is the only presence question here.
 */
struct bond_float_data final {
    double value{};
    std::optional<std::string> start_date;
};

/**
 * @brief One row of an amortization block.
 */
struct bond_amortization_data final {
    std::string type;
    std::optional<double> value;
    std::optional<std::string> start_date;
    std::optional<std::string> end_date;
    std::optional<std::string> frequency;
    std::optional<bool> underflow;
};

/**
 * @brief The leg's settlement block: the index and the date it fixes on.
 */
struct bond_settlement_data final {
    std::string fx_index;
    std::optional<std::string> fixing_date;
};

/**
 * @brief The two indices a stub interpolates between, and the rounding.
 */
struct bond_stub_interpolation final {
    std::string short_index;
    std::string long_index;
    std::optional<std::string> rounding_type;
    std::optional<std::int64_t> rounding_precision;
};

/**
 * @brief A floating leg's rate terms.
 *
 * The three lists of numbers (spreads, caps, floors, gearings) all carry
 * the bond_float_data shape. The two schedules are schedules the document
 * states on the leg itself rather than on the leg's outer block.
 */
struct bond_floating_leg_data final {
    std::string index;
    std::optional<bool> is_in_arrears;
    std::optional<std::string> last_recent_period;
    std::optional<std::string> last_recent_period_calendar;
    std::optional<std::uint64_t> fixing_days;
    std::optional<std::string> lookback;
    std::optional<std::int64_t> rate_cutoff;
    std::optional<bool> is_averaged;
    std::optional<bool> has_sub_periods;
    std::optional<bool> include_spread;
    std::optional<bool> is_not_resetting_xccy;
    std::vector<bond_float_data> spreads;
    std::vector<bond_float_data> caps;
    std::vector<bond_float_data> floors;
    std::vector<bond_float_data> gearings;
    std::optional<bool> naked_option;
    std::optional<bool> local_cap_floor;
    bond_schedule_data fixing_schedule;
    bond_schedule_data reset_schedule;
    std::optional<bond_stub_interpolation> front_stub_interpolation;
    std::optional<bond_stub_interpolation> back_stub_interpolation;
    std::optional<bool> stub_use_original_curve;
    std::optional<bool> observation_shift;
};

/**
 * @brief A fixed leg's rate terms.
 */
struct bond_fixed_leg_data final {
    std::vector<bond_float_data> rates;
};

/**
 * @brief A formula-based leg's rate terms.
 */
struct bond_formula_based_leg_data final {
    std::string index;
    std::optional<bool> is_in_arrears;
    std::int64_t fixing_days{};
    std::optional<std::string> fixing_calendar;
};

/**
 * @brief The rate group of a leg, one alternative per leg type.
 *
 * The ORE schema states the group as a choice of eighteen alternatives.
 * A bond leg carries a coupon, and the corpus states three of them:
 * fixed, floating and formula-based. The other fifteen are a recorded
 * boundary, and the alternative a document states is the engaged member
 * here.
 */
struct bond_leg_rate_data final {
    std::optional<bond_fixed_leg_data> fixed;
    std::optional<bond_floating_leg_data> floating;
    std::optional<bond_formula_based_leg_data> formula_based;
};

/**
 * @brief A leg as the document states it, member by member.
 *
 * The fact rows hold the leg's rate and its index. Everything else the
 * leg states is either a member here or a term on the issue row, and
 * the two directions are:
 *
 * - currency and day_counter have issue columns, so the mapper mirrors
 *   them there and this type carries the document's own statement. On
 *   export the document wins and the row is the fallback, which matters
 *   when the container came from a row set rather than a document.
 * - payer, leg_type and the payment terms have no column: the payer and
 *   the leg type are document flags, and the payment terms, the payment
 *   calendar and the two lag members are the destination of the shared
 *   instrument-keyed tables the parent story describes.
 * - the schedules and the three lists of rows go to those same tables.
 *
 * Every member is an optional, so a member the document states and this
 * container does not hold stays distinguishable from one the document
 * omits. A member the schema declares required is engaged by every
 * document, and an unengaged one then means the container came from a
 * row set.
 *
 * A bond states one leg per coupon, and the schema declares the element
 * unbounded, so the instrument holds a list of these and not one.
 */
struct bond_leg_data final {
    std::optional<bool> payer;
    std::optional<std::string> leg_type;
    std::optional<std::string> currency;
    std::optional<std::string> payment_convention;
    std::optional<std::string> payment_lag;
    std::optional<std::string> payment_calendar;
    std::optional<std::string> day_counter;
    std::optional<std::string> last_period_day_counter;
    std::optional<std::int64_t> notional_payment_lag;
    std::optional<bool> strict_notional_dates;
    bond_schedule_data schedule;
    std::vector<bond_amortization_data> amortizations;
    std::vector<bond_float_data> notionals;
    std::vector<std::string> payment_dates;
    std::optional<bool> indexings_from_asset_leg;
    std::optional<bond_leg_rate_data> rate;
    bond_schedule_data payment_schedule;
    std::optional<bond_settlement_data> settlement;

    /**
     * @brief True when the leg carries nothing, so a writer can skip it.
     *
     * The schema requires the leg's type and its payer, so a leg a writer
     * emits empty is not a document the reader accepts. The guard belongs
     * beside the members, so a member added here reaches it in one place.
     */
    bool is_empty() const {
        return !payer && !leg_type && !currency && !payment_convention && !payment_lag &&
               !payment_calendar && !day_counter && !last_period_day_counter &&
               !notional_payment_lag && !strict_notional_dates && !rate &&
               !indexings_from_asset_leg && !settlement && schedule.rules.empty() &&
               schedule.dates.empty() && amortizations.empty() && notionals.empty() &&
               payment_dates.empty() && payment_schedule.rules.empty() &&
               payment_schedule.dates.empty();
    }
};

/**
 * @brief A forward bond's settlement block.
 *
 * The forward maturity date is required and every other member is
 * optional. The nine tables carry no forward settlement column, so the
 * container holds the block whole and export re-emits it.
 */
struct bond_forward_settlement final {
    std::string forward_maturity_date;
    std::optional<std::string> forward_settlement_date;
    std::optional<std::string> settlement;
    std::optional<double> amount;
    std::optional<double> lock_rate;
    std::optional<double> dv01;
    std::optional<std::string> lock_rate_day_counter;
    std::optional<std::string> settlement_dirty;
};

/**
 * @brief A forward bond's premium: the amount and the date it pays.
 *
 * Both members are required strings, and the schema spells the amount as
 * text rather than as a number.
 */
struct bond_forward_premium final {
    std::string amount;
    std::string date;
};

/**
 * @brief The settlement terms an option or a premium pays under.
 *
 * The pay currency and the FX index are required and the fixing date is
 * optional. The schema spells the same three members twice, once under
 * the option and once under each premium, so the container holds one
 * type for both.
 */
struct bond_option_settlement final {
    std::string pay_currency;
    std::string fx_index;
    std::optional<std::string> fixing_date;
};

/**
 * @brief One entry of an option's premium list.
 *
 * The amount, the currency and the pay date are required, and the
 * settlement block beside them is optional.
 */
struct bond_option_premium final {
    double amount;
    std::string currency;
    std::string pay_date;
    std::optional<bond_option_settlement> settlement;
};

/**
 * @brief One exercise fee: an amount with three optional attributes.
 *
 * The schema states the amount as text with the attributes on the same
 * element, so the container keeps the number and the attributes apart.
 */
struct bond_option_exercise_fee final {
    double amount;
    std::optional<std::string> type;
    std::optional<std::string> start_date;
    std::optional<std::string> currency;
};

/**
 * @brief One exercise the document states outright: a date and a price.
 */
struct bond_option_exercise final {
    std::string date;
    std::optional<double> price;
};

/**
 * @brief The rule an option's payment dates are derived from.
 */
struct bond_option_payment_rules final {
    std::uint64_t lag = 0;
    std::string calendar;
    std::string convention;
    std::optional<std::string> relative_to;
};

/**
 * @brief An option's payment dates, stated either as a list or as a rule.
 */
struct bond_option_payment_data final {
    std::vector<std::string> dates;
    std::optional<bond_option_payment_rules> rules;
};

/**
 * @brief The option block, shared by every product that states one.
 *
 * bondOptionData and AscotData hold the same optionData element, and the
 * equity, FX and commodity products hold it too. The nine tables carry
 * no option column beyond the type and the strike, so the container
 * holds the block whole and export re-emits it.
 *
 * The schema requires the long and short flag and makes every other
 * member optional, so a member the document omits stays absent. The
 * premium amount and the exercise price list are text in the schema,
 * not numbers, and stay text here, and the automatic exercise flag
 * carries the document's own spelling of the schema's bool type.
 */
struct bond_option_data final {
    std::string long_short;
    std::optional<std::string> option_type;
    std::optional<std::string> payoff_type;
    std::optional<std::string> payoff_type_2;
    std::optional<std::string> style;
    std::optional<std::string> notice_period;
    std::optional<std::string> notice_calendar;
    std::optional<std::string> notice_convention;
    std::optional<std::string> mid_coupon_exercise;
    std::optional<std::string> settlement;
    std::optional<std::string> settlement_method;
    std::optional<std::string> pay_off_at_expiry;
    std::optional<std::string> premium_amount;
    std::optional<std::string> premium_currency;
    std::optional<std::string> premium_pay_date;
    std::vector<bond_option_premium> premiums;
    std::optional<std::string> exercise_prices;
    std::vector<bond_option_exercise_fee> exercise_fees;
    std::optional<std::string> exercise_fee_settlement_period;
    std::optional<std::string> exercise_fee_settlement_calendar;
    std::optional<std::string> exercise_fee_settlement_convention;
    std::optional<std::string> automatic_exercise;
    std::optional<bond_option_exercise> exercise_data;
    std::optional<bond_option_payment_data> payment_data;
    std::optional<bond_option_settlement> settlement_data;
};

/**
 * @brief The strike, stated as a price, as a yield or as a bare number.
 *
 * A strike the document states as a bare element reaches the option fact
 * row. The schema states the element form as a choice of three: a price
 * with its currency, a yield with its compounding, or a number with an
 * optional currency. Each alternative is a pair here, and the three are
 * mutually exclusive in a document.
 */
struct bond_strike_data final {
    std::optional<double> price_value;
    std::optional<std::string> price_currency;
    std::optional<double> yield_value;
    std::optional<std::string> yield_compounding;
    std::optional<double> bare_value;
    std::optional<std::string> bare_currency;
};

}

#endif
