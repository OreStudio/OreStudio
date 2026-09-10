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
 * scope limit of task D7943D7E wave 1.3. The exercise dates, the three
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
     * @brief Every delivery basket identifier the future's basket lists.
     */
    std::vector<std::string> future_delivery_basket;

    /**
     * @brief The total return price type the document states (Dirty or Clean).
     */
    std::string trs_price_type;

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
