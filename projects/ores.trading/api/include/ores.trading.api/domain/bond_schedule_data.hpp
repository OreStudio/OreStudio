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
#ifndef ORES_TRADING_API_DOMAIN_BOND_SCHEDULE_DATA_HPP
#define ORES_TRADING_API_DOMAIN_BOND_SCHEDULE_DATA_HPP

#include "ores.trading.api/domain/bond_schedule_dates.hpp"
#include "ores.trading.api/domain/bond_schedule_rules.hpp"
#include "ores.utility/codec/wire_fields.hpp"
#include <tuple>
#include <vector>

namespace ores::trading::domain {

/**
 * @brief A document schedule that no column of the nine bond tables holds.
 *
 * The ORE schema states a schedule as a choice between a rule block
 * (bond_schedule_rules) and a date list (bond_schedule_dates). This type
 * mirrors that choice as two lists, so the mapping between the document and
 * the two is total and needs no case analysis: a document states one arm or
 * the other, and the other list is empty.
 *
 * The nine bond tables carry no schedule column. The org models record the
 * destination as the shared instrument-keyed schedule tables, and until those
 * land the container carries the structure whole so that export re-emits what
 * the document held.
 */
struct bond_schedule_data {
    /**
     * @brief The rule blocks the document states, in document order. Empty for a schedule the
     * document states as a date list.
     */
    std::vector<bond_schedule_rules> rules;

    /**
     * @brief The date lists the document states, in document order. Empty for a schedule the
     * document states as a rule block.
     */
    std::vector<bond_schedule_dates> dates;

    /**
     * @brief Value equality.
     *
     * A field group is a value like the entity that holds it: the entity's
     * comparison is defaulted and reads every member, so a group without a
     * comparison deletes the entity's and fails a build that treats that as
     * an error.
     */
    friend bool operator==(const bond_schedule_data&, const bond_schedule_data&) = default;
};
/**
 * @brief The members of bond_schedule_data, named as the wire names
 * them and in the order the type declares them.
 *
 * A group is a member of the entity that holds it, so the carriers reach this
 * shape through it: the parent's field names the member, and this one names the
 * members inside it.
 */
[[nodiscard]] constexpr auto wire_fields(const bond_schedule_data&) {
    return std::tuple{
        utility::codec::field{"rules", &bond_schedule_data::rules},
        utility::codec::field{"dates", &bond_schedule_data::dates},
    };
}

}

#endif
