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
#ifndef ORES_REPORTING_CORE_SERVICE_CUT_STATE_HPP
#define ORES_REPORTING_CORE_SERVICE_CUT_STATE_HPP

#include "ores.reporting.api/domain/cut.hpp"
#include "ores.reporting.api/domain/cut_kind.hpp"
#include "ores.reporting.core/export.hpp"
#include <string>
#include <string_view>
#include <vector>

namespace ores::reporting::service {

/**
 * @brief The state a reader sees for a cut: one of seven, derived and never stored.
 */
enum class cut_state {
    /** No taking instant holds and no gap stands. */
    draft,

    /** No taking instant holds and a gap stands. */
    incomplete,

    /** The body is fixed and waits on an authorisation the kind requires. */
    taken,

    /** The body is fixed and the kind requires no authorisation. */
    in_use,

    /** The body is fixed and the authorisation is recorded. */
    authorised,

    /** The authorisation was refused. Terminal for that body. */
    refused,

    /** A later cut of the same kind and scope holds its place. */
    superseded
};

/**
 * @brief The state a cut's own facts state, before supersession is known.
 *
 * @param cut The record, as the store holds it.
 * @param open_gap_count How many parts the assembly could not fill.
 */
[[nodiscard]] ORES_REPORTING_CORE_EXPORT cut_state
derive_state(const domain::cut& cut, int open_gap_count);

/**
 * @brief The state a reader sees, once a later cut's place is known.
 *
 * Supersession moves a usable cut and leaves every other state as it was.
 *
 * @param cut The record, as the store holds it.
 * @param open_gap_count How many parts the assembly could not fill.
 * @param later_cut_holds_its_place Whether a later cut of the same kind and
 * scope stands in this one's place.
 */
[[nodiscard]] ORES_REPORTING_CORE_EXPORT cut_state
derive_state(const domain::cut& cut, int open_gap_count, bool later_cut_holds_its_place);

/**
 * @brief Whether one body stands in another's place.
 *
 * The two facts are the same wherever they are read: the body is later, it is of
 * the same kind, party, scope and centre, and a run may price from it. A later
 * cut that waits on a person, or one that was refused, holds nothing.
 *
 * @param later The body whose place is in question.
 * @param cut The body it would replace.
 */
[[nodiscard]] ORES_REPORTING_CORE_EXPORT bool
stands_in_the_place_of(const domain::cut& later, const domain::cut& cut);

/**
 * @brief Whether a later cut of the same kind, party, scope and centre replaced this one.
 *
 * @param cut The record whose place is in question.
 * @param candidates The cuts read for the same scope, in any order.
 */
[[nodiscard]] ORES_REPORTING_CORE_EXPORT bool
later_cut_holds_its_place(const domain::cut& cut, const std::vector<domain::cut>& candidates);

/**
 * @brief Whether a run may price from a cut in this state.
 */
[[nodiscard]] ORES_REPORTING_CORE_EXPORT bool is_usable(cut_state state);

/**
 * @brief The state's name, as the wire and the screens spell it.
 */
[[nodiscard]] ORES_REPORTING_CORE_EXPORT std::string_view to_string(cut_state state);

/**
 * @brief The label a person quotes for a cut, derived and never typed.
 *
 * =EOD.ACME-LONDON.2026-10-06=, =EOD.ACME-LONDON.2026-10-06.2= for a retake,
 * and =LIVE.ACME-LONDON.2026-10-07T07:40:00Z= for a kind whose bodies are
 * separated by their instant rather than by an ordinal. The date is the date of
 * the stored instant, which the platform holds in UTC.
 *
 * @param cut The record the label names.
 * @param kind The kind the cut was taken under.
 */
[[nodiscard]] ORES_REPORTING_CORE_EXPORT std::string
cut_label_of(const domain::cut& cut, const domain::cut_kind& kind);

}

#endif
