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
/**
 * AUTO-GENERATED FILE - DO NOT EDIT MANUALLY
 * Template: cpp_field_group.hpp.mustache
 * To modify, update the template and regenerate.
 */
#ifndef ORES_TRADING_API_DOMAIN_BOND_INSTRUMENT_DATA_HPP
#define ORES_TRADING_API_DOMAIN_BOND_INSTRUMENT_DATA_HPP

#include "ores.trading.api/domain/ascot.hpp"
#include "ores.trading.api/domain/bond_forward_premium.hpp"
#include "ores.trading.api/domain/bond_forward_settlement.hpp"
#include "ores.trading.api/domain/bond_future.hpp"
#include "ores.trading.api/domain/bond_instrument.hpp"
#include "ores.trading.api/domain/bond_issue.hpp"
#include "ores.trading.api/domain/bond_issue_call_date.hpp"
#include "ores.trading.api/domain/bond_issue_conversion_target.hpp"
#include "ores.trading.api/domain/bond_leg_data.hpp"
#include "ores.trading.api/domain/bond_option.hpp"
#include "ores.trading.api/domain/bond_option_data.hpp"
#include "ores.trading.api/domain/bond_repo.hpp"
#include "ores.trading.api/domain/bond_schedule_data.hpp"
#include "ores.trading.api/domain/bond_strike_data.hpp"
#include "ores.trading.api/domain/bond_trs.hpp"
#include "ores.utility/decimal/decimal.hpp"
#include <optional>
#include <string>
#include <vector>

namespace ores::trading::domain {

/**
 * @brief The assembled bond instrument: the header row, its issue row and the product's fact row.
 *
 * The assembled bond instrument: the container the variant trade_instrument
 * carries between import and export. It holds every row the export path needs:
 * the slim instrument header (identity with the ten-code trade_type_code,
 * issue_id), the issue row with the bond terms map_bond_data reads, the
 * product's fact row (option, trs, repo, future or ascot), and the issue's child
 * rows (call dates, conversion targets). The instrument.issue_id member pins
 * issue.issue_id: the mapper looks the issue up by security id and mints one
 * only when the lookup misses. Every bond-level member of the document reaches
 * the issue row, so the container holds no second copy of one.
 *
 * The remainder members carry what the nine bond tables cannot store, so that
 * export re-emits what the document held. Each one is a recorded scope limit of
 * the fidelity work. The exercise dates and the four leg schedules go to the
 * shared instrument-keyed schedule tables; the leg payer flags and the total
 * return price type have no column anywhere; and the tables carry no option
 * column beyond the type and the strike, so the option block is held whole.
 */
struct bond_instrument_data {
    /**
     * @brief The instrument header row: identity, issue_id and audit.
     */
    bond_instrument instrument;

    /**
     * @brief The issue row holding the bond terms of this trade's security.
     */
    bond_issue issue;

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
    std::optional<ascot> ascot_row;

    /**
     * @brief Every exercise date the option's schedule lists, in document order.
     */
    std::vector<std::string> option_exercise_dates;

    /**
     * @brief The option block no row holds, when the document states one.
     */
    std::optional<bond_option_data> option_data;

    /**
     * @brief The strike group, when the document states a price or a yield.
     */
    std::optional<bond_strike_data> strike_data;

    /**
     * @brief The schedule the option's exercise dates are generated from, when the document states
     * one.
     */
    std::optional<bond_schedule_data> option_exercise_schedule;

    /**
     * @brief The redemption code the document states beside the option block.
     */
    std::optional<std::string> option_redemption;

    /**
     * @brief The price type the document states beside the option block.
     */
    std::optional<std::string> option_price_type;

    /**
     * @brief The document's own spelling of the knock-out flag.
     */
    std::optional<std::string> option_knocks_out;

    /**
     * @brief The total return price type the document states (Dirty or Clean).
     */
    std::string trs_price_type;

    /**
     * @brief The document's own spelling of the long-in-forward flag.
     */
    std::optional<std::string> forward_long_in_forward;

    /**
     * @brief The forward's settlement block, when the document states one.
     */
    std::optional<bond_forward_settlement> forward_settlement;

    /**
     * @brief The forward's premium, when the document states one.
     */
    std::optional<bond_forward_premium> forward_premium;

    /**
     * @brief The document's own spelling of the TRS payer flag.
     */
    std::optional<std::string> trs_payer;

    /**
     * @brief The TRS initial price, when the document states one.
     */
    std::optional<ores::utility::decimal::decimal> trs_initial_price;

    /**
     * @brief The schedule the TRS total return follows.
     */
    bond_schedule_data trs_schedule;

    /**
     * @brief The bond's own coupon legs, re-emitted whole and in order.
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

    /**
     * @brief Value equality.
     *
     * A field group is a value like the entity that holds it: the entity's
     * comparison is defaulted and reads every member, so a group without a
     * comparison deletes the entity's and fails a build that treats that as
     * an error.
     */
    friend bool operator==(const bond_instrument_data&, const bond_instrument_data&) = default;
};

}

#endif
