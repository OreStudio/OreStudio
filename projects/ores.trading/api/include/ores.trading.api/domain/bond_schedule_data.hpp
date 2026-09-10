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
 * - the schedule goes to those same tables.
 *
 * Every member is an optional, so a member the document states and this
 * container does not hold stays distinguishable from one the document
 * omits. A member the schema declares required is engaged by every
 * document, and an unengaged one then means the container came from a
 * row set.
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
               !notional_payment_lag && !strict_notional_dates && schedule.rules.empty() &&
               schedule.dates.empty();
    }
};

}

#endif
