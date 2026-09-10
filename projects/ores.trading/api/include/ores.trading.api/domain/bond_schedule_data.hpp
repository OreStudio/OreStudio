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
 */
struct bond_schedule_rules final {
    std::string start_date;
    std::optional<std::string> end_date;
    std::optional<bool> adjust_end_date_to_previous_month_end;
    std::string tenor;
    std::optional<std::string> calendar;
    std::string convention;
    std::optional<std::string> term_convention;
    std::optional<std::string> rule;
    std::optional<bool> end_of_month;
    std::optional<std::string> end_of_month_convention;
    std::optional<std::string> first_date;
    std::optional<std::string> last_date;
    std::optional<bool> remove_first_date;
    std::optional<bool> remove_last_date;
};

/**
 * @brief A schedule the document states as an explicit date list.
 */
struct bond_schedule_dates final {
    std::optional<std::string> calendar;
    std::optional<std::string> convention;
    std::optional<std::string> tenor;
    std::optional<bool> end_of_month;
    std::optional<bool> include_duplicate_dates;
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
 * @brief A leg whose terms the fact row does not carry.
 *
 * The fact rows hold the leg's type, its rate and its index. Two leg
 * members have no column: the payer direction, and the schedule the
 * leg's dates come from. The payer is a document flag with no table
 * destination; the schedule's destination is the same shared
 * instrument-keyed schedule tables the standalone schedules go to.
 *
 * Export writes both back, so a leg whose payer the document states
 * as true no longer exports as false.
 *
 * A leg with a fact row leaves leg_type empty, because the fact row
 * holds the type. A bond's own coupon leg has no fact row: the issue's
 * terms are its only other home, and they carry the coupon rate and the
 * maturity but not the leg type, so the type rides here.
 */
struct bond_leg_data final {
    bool payer = false;
    std::string leg_type;
    bond_schedule_data schedule;

    /**
     * @brief True when the leg carries nothing, so a writer can skip it.
     *
     * The schema requires the leg's type and its payer, so a leg a writer
     * emits empty is not a document the reader accepts. The guard belongs
     * beside the members, so a member added here reaches it in one place.
     */
    bool is_empty() const {
        return !payer && leg_type.empty() && schedule.rules.empty() && schedule.dates.empty();
    }
};

}

#endif
