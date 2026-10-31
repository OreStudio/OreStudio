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

namespace ores::reporting::service {

/**
 * @brief The state a reader sees for a cut.
 *
 * A derivation and not a lifecycle: the state is one of the seven and never a
 * mixture of two. It is stored nowhere, for the reason the published set states
 * its own state nowhere: a stored state follows a write that may fail, may be
 * repeated or may be missed, so the row and its state can disagree.
 */
enum class cut_state {
    /** The attempt is saved and no taking instant holds, and no gap stands. */
    draft,

    /** The attempt is saved, no taking instant holds, and a gap stands. */
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
 * The order of the tests is the order a reader needs them: a row that no take
 * completed is not a cut yet, whatever else it holds, and a refused body is
 * never a usable one.
 *
 * @param cut The record, as the store holds it.
 * @param open_gap_count How many parts the assembly could not fill.
 */
[[nodiscard]] ORES_REPORTING_CORE_EXPORT cut_state
derive_state(const domain::cut& cut, int open_gap_count);

/**
 * @brief The state a reader sees, once a later cut's place is known.
 *
 * @param cut The record, as the store holds it.
 * @param open_gap_count How many parts the assembly could not fill.
 * @param later_cut_holds_its_place Whether a later cut of the same kind and
 * scope stands in this one's place. It moves a usable cut to superseded and
 * leaves every other state as it was, because a superseded cut is still a
 * record of an instant and a run that priced from it still reproduces from it.
 */
[[nodiscard]] ORES_REPORTING_CORE_EXPORT cut_state
derive_state(const domain::cut& cut, int open_gap_count, bool later_cut_holds_its_place);

/**
 * @brief Whether a run may price from a cut in this state.
 *
 * A body that is fixed and signed where a signature was required is usable, and
 * a superseded one still is: the run reads an older body, and the state says a
 * later cut is in use rather than refusing the read.
 */
[[nodiscard]] ORES_REPORTING_CORE_EXPORT bool is_usable(cut_state state);

/**
 * @brief The state's name, as the wire and the screens spell it.
 */
[[nodiscard]] ORES_REPORTING_CORE_EXPORT std::string_view to_string(cut_state state);

/**
 * @brief The label a person quotes for a cut.
 *
 * Derived from the kind, the scope and the timepoint, and never typed:
 * =EOD.ACME-LONDON.2026-10-06=, =EOD.ACME-LONDON.2026-10-06.2= for a retake,
 * and =LIVE.ACME-LONDON.2026-10-07T07:40:00Z= for a kind whose bodies are
 * separated by their instant rather than by an ordinal.
 *
 * The date is the date of the stored instant, which the platform holds in UTC.
 * The cut's own =timepoint_timezone= states the clock the instant was taken in,
 * so a date that falls on either side of midnight for a centre east or west of
 * UTC is read through the two columns together.
 *
 * A kind of the consumer family is the one whose instant the run supplies, so
 * its bodies are separated by their instant and not by an ordinal: the kind is
 * what tells the two shapes apart, which is why it is stated here.
 *
 * @param cut The record the label names.
 * @param kind The kind the cut was taken under.
 */
[[nodiscard]] ORES_REPORTING_CORE_EXPORT std::string
cut_label_of(const domain::cut& cut, const domain::cut_kind& kind);

}

#endif
