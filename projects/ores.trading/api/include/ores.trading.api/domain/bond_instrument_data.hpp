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
#ifndef ORES_TRADING_API_DOMAIN_BOND_INSTRUMENT_DATA_HPP
#define ORES_TRADING_API_DOMAIN_BOND_INSTRUMENT_DATA_HPP

#include "ores.trading.api/domain/ascot.hpp"
#include "ores.trading.api/domain/bond_future.hpp"
#include "ores.trading.api/domain/bond_instrument.hpp"
#include "ores.trading.api/domain/bond_issue.hpp"
#include "ores.trading.api/domain/bond_issue_call_date.hpp"
#include "ores.trading.api/domain/bond_issue_conversion_target.hpp"
#include "ores.trading.api/domain/bond_option.hpp"
#include "ores.trading.api/domain/bond_repo.hpp"
#include "ores.trading.api/domain/bond_schedule_data.hpp"
#include "ores.trading.api/domain/bond_trs.hpp"
#include "ores.trading.api/domain/instrument.hpp"
#include <boost/uuid/uuid.hpp>
#include <optional>
#include <string>
#include <vector>

namespace ores::trading::domain {

/**
 * @brief The assembled bond instrument: the header row, its issue row and the product's fact row.
 *
 * Handcrafted carrier for the bond family, the way the rates family
 * assembles its generated part types with with_legs (instrument.hpp);
 * codegen absorbs it only when the parent story's container grammar
 * lands. The variant trade_instrument carries this type between import
 * and export, so it must hold every row the export path needs: the
 * slim instrument header (identity with the ten-code trade_type_code,
 * issue_id), the issue row with the bond terms map_bond_data reads,
 * the fact row of the product (option, trs, repo, future or ascot),
 * and the issue's child rows (call dates, conversion targets). The
 * instrument.issue_id member pins issue.issue_id: the mapper looks the
 * issue up by security_id and mints one only when the lookup misses.
 *
 * The remainder members carry what the nine tables cannot store, so
 * that export re-emits what the document held. Each one is a recorded
 * scope limit of the fidelity work. The exercise dates, the four
 * leg schedules and the delivery basket go to the parent story's shared
 * instrument-keyed schedule and underlyings tables; the leg payer flags
 * and the total return price type have no column anywhere.
 */
struct bond_instrument_data final {
    /**
     * @brief The instrument header row: identity, issue_id and audit.
     */
    bond_instrument instrument;

    /**
     * @brief The issue row holding the bond terms of this trade's security.
     */
    bond_issue issue;

    /**
     * @brief The bond members the issue row has no column for.
     *
     * The reference calendar, the credit, reference and income curve
     * identifiers, and the stated notional. The corpus states four of
     * them on every bond product, and the issue row holds none. Unit B
     * adds them as columns on the issue table; until then the container
     * is their only home, so export re-emits them from here and an
     * unengaged member means the document omitted the element.
     */
    std::optional<std::string> calendar;
    std::optional<std::string> credit_curve_id;
    std::optional<std::string> reference_curve_id;
    std::optional<std::string> income_curve_id;
    std::optional<std::string> bond_notional;

    /**
     * @brief The issue's call dates, one row per date the call schedule names.
     */
    std::vector<bond_issue_call_date> call_dates;

    /**
     * @brief The issue's conversion targets, one row per conversion ratio.
     */
    std::vector<bond_issue_conversion_target> conversion_targets;

    /**
     * @brief The bond_option fact row, engaged for BondOption products.
     */
    std::optional<bond_option> option;

    /**
     * @brief The bond_trs fact row, engaged for BondTRS products.
     */
    std::optional<bond_trs> trs;

    /**
     * @brief The bond_repo fact row, engaged for BondRepo products.
     */
    std::optional<bond_repo> repo;

    /**
     * @brief The bond_future fact row, engaged for BondFuture products.
     */
    std::optional<bond_future> future;

    /**
     * @brief The ascot fact row, engaged for Ascot products.
     */
    std::optional<ascot> ascot;

    /**
     * @brief Every exercise date the option's schedule lists, in document order.
     */
    std::vector<std::string> option_exercise_dates;

    /**
     * @brief The option block no row holds.
     *
     * bondOptionData and AscotData both state it and the two fact rows
     * carry only the option type, so the container holds the block whole
     * and export re-emits it. The long and short flag is required, so a
     * product that states an option always engages this member.
     */
    std::optional<bond_option_data> option_data;

    /**
     * @brief The strike group, when the document states a price or a yield.
     *
     * A bare strike reaches the option fact row instead. This holds the
     * richer spellings, which the row has no column for.
     */
    std::optional<bond_strike_data> strike_data;

    /**
     * @brief The schedule the option's exercise dates are generated from.
     *
     * An option states its exercise dates either as a list, which the
     * member above holds, or as a schedule, which this one does.
     */
    std::optional<bond_schedule_data> option_exercise_schedule;

    /**
     * @brief The three BondOption members that sit beside the option block.
     *
     * The redemption code, the price type and the knock-out flag belong
     * to BondOptionData rather than to the shared option element, so an
     * Ascot never states them. No row holds any of the three. The
     * knock-out flag carries the document's own spelling of the schema's
     * bool type, which is one of thirteen, so a writer re-emits the
     * spelling the document chose rather than a canonical one.
     */
    std::optional<std::string> option_redemption;
    std::optional<std::string> option_price_type;
    std::optional<std::string> option_knocks_out;

    /**
     * @brief Every delivery basket identifier the future's basket lists.
     */
    std::vector<std::string> future_delivery_basket;

    /**
     * @brief The total return price type the document states (Dirty or Clean).
     */
    std::string trs_price_type;

    /**
     * @brief The forward bond members no row holds.
     *
     * The long-in-forward flag, the settlement block and the premium. The
     * schema declares the flag and the settlement block required, so a
     * forward bond always states them and an unengaged member here means
     * the container came from a row set rather than from a document.
     */
    std::optional<std::string> forward_long_in_forward;
    std::optional<bond_forward_settlement> forward_settlement;
    std::optional<bond_forward_premium> forward_premium;

    /**
     * @brief The total return members of a TRS that no row holds.
     *
     * The payer flag, the initial price and the return schedule. The
     * payer and the schedule are required, so a TRS always states them.
     */
    std::optional<std::string> trs_payer;
    std::optional<double> trs_initial_price;
    bond_schedule_data trs_schedule;

    /**
     * @brief The bond's own coupon legs, re-emitted whole and in order.
     *
     * A bond states one leg per coupon and the schema declares the
     * element unbounded, so three documents in the corpus carry two. The
     * issue row carries the first leg's rate, frequency and maturity, so
     * export rebuilds a leg from those when this list is empty or short.
     * The issue cannot carry the schedule: its start date is the leg's
     * own datum and differs from the issue date, and the rules block
     * names a calendar, a term convention and date rules the nine tables
     * have no column for. Export writes the legs from here, and the
     * issue terms stay the fallback for a container that did not come
     * from a document.
     */
    std::vector<bond_leg_data> bond_legs;

    /**
     * @brief The TRS funding leg's payer flag and schedule, re-emitted whole.
     */
    bond_leg_data trs_funding_leg;

    /**
     * @brief The repo leg's payer flag and schedule, re-emitted whole.
     */
    bond_leg_data repo_leg;

    /**
     * @brief The ascot reference swap's payer flag and schedule, re-emitted whole.
     */
    bond_leg_data ascot_swap_leg;
};

inline void stamp_ids(bond_instrument_data& data,
                      boost::uuids::uuid instrument_id,
                      boost::uuids::uuid trade_id) {
    stamp_ids(data.instrument, instrument_id, trade_id);
    if (data.option)
        data.option->instrument_id = instrument_id;
    if (data.trs)
        data.trs->instrument_id = instrument_id;
    if (data.repo)
        data.repo->instrument_id = instrument_id;
    if (data.future)
        data.future->instrument_id = instrument_id;
    if (data.ascot)
        data.ascot->instrument_id = instrument_id;
}

}

#endif
