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
#ifndef ORES_ANALYTICS_QUANT_DOMAIN_TENOR_RESOLUTION_HPP
#define ORES_ANALYTICS_QUANT_DOMAIN_TENOR_RESOLUTION_HPP

#include "ores.analytics.quant/domain/date_convention.hpp"
#include "ores.analytics.quant/export.hpp"
#include <chrono>
#include <cstddef>
#include <optional>
#include <string_view>

namespace ores::analytics::quant::domain {

/// Whether a tenor states its own duration. Mirrors the PERIOD/SPECIAL split
/// in @c ores_refdata_tenors_tbl.kind: a PERIOD tenor (3M, 1Y) carries its own
/// unit and multiplier, while a SPECIAL one (O/N, T/N, SPOT) has no duration
/// of its own and takes one from the convention that resolves it.
enum class tenor_kind {
    period,
    special,
};

/// The reference point a tenor counts from -- the
/// @c ores_refdata_tenor_anchors_tbl code, less the ones this engine refuses
/// to guess at. See @c resolution_status::unsupported_anchor.
enum class tenor_anchor {
    /// The convention states no anchor of its own, so nothing resolves.
    none,
    /// The spot date the caller supplies.
    spot,
    /// The horizon date itself.
    today,
    /// The horizon date plus one calendar day.
    tomorrow,
    /// The near leg of a short-dated FX swap: a date the swap's own terms
    /// fix, not one derivable from the horizon.
    near_leg,
    /// The nearest IMM quarterly roll date. Refused: the persisted definition
    /// says "nearest" where QuantLib defines only "next", and no convention
    /// resolves from it yet, so the engine declines rather than picks one.
    imm_roll,
};

/// Which of the two rules a convention names for resolving a tenor. Mirrors
/// @c ores_refdata_tenor_resolution_algorithms_tbl.code.
enum class resolution_algorithm {
    /// The anchor plus a fixed period.
    anchor_offset,
    /// The anchor plus a period, then a whole number of steps along a named
    /// schedule -- see @c schedule_kind.
    schedule_step,
};

/// The schedule axis a @c schedule_step row walks.
enum class schedule_kind {
    /// No axis. The row resolves on the calendar axis, exactly as
    /// @c anchor_offset does -- the fallback a membership-only row states.
    none,
    /// The CDS quarterly roll date: the 20th of March, June, September and
    /// December, by closed form. Not the IMM third Wednesday; the two
    /// conventions trade under one name and are different dates
    /// (doc/knowledge/domain/imm_dates.org).
    cds_roll_quarter,
    /// Dates the caller supplies, read from a calendar's diary events
    /// (@c ores_refdata_tenor_schedules_tbl.schedule_source = EVENT_LOOKUP).
    event_lookup,
};

/// A tenor's own duration. For a SPECIAL tenor this is empty, and the
/// resolution rule's offset applies instead.
struct tenor_spec final {
    tenor_kind kind = tenor_kind::period;
    date_period period;

    friend bool operator==(const tenor_spec&, const tenor_spec&) = default;
};

/// Everything a convention and its resolution row decide about one tenor.
/// The caller reads these off persisted rows; the engine never sees a code.
struct tenor_rule final {
    /// Which date the offset counts from.
    tenor_anchor anchor = tenor_anchor::none;
    /// Anchor plus period, or anchor plus period and a schedule walk.
    resolution_algorithm algorithm = resolution_algorithm::anchor_offset;
    /// How the resolved date moves off a non-business day.
    business_day_convention roll = business_day_convention::modified_following;
    /// The duration a SPECIAL tenor takes from the convention, and the offset
    /// a schedule walk starts from. Ignored for a PERIOD tenor, whose own
    /// period applies.
    date_period offset;
    /// The schedule axis for a @c schedule_step row.
    schedule_kind schedule = schedule_kind::none;
    /// Which schedule's dates this row walks, in the index space of the
    /// @c schedule_date span the batch was handed.
    std::size_t schedule_index = 0;
    /// How many schedule dates to step to, counting the walk start. At least
    /// one whenever @c schedule names an axis.
    int schedule_step_count = 0;
    /// Snap a month- or year-counted period to the end of its month, the way
    /// QuantLib's @c Calendar::advance does when its @c endOfMonth flag is
    /// set. No persisted column states this yet; it is here because the
    /// arithmetic distinguishes it and a schedule will need it.
    bool end_of_month = false;

    friend bool operator==(const tenor_rule&, const tenor_rule&) = default;
};

/// One date of a named schedule, the flat shape an ordered bulk read of
/// calendar events returns. Rows for one schedule must be contiguous and
/// sorted by date, which an @c ORDER BY schedule, event_date gives.
struct schedule_date final {
    std::size_t schedule_index = 0;
    std::chrono::year_month_day date{};

    friend bool operator==(const schedule_date&, const schedule_date&) = default;
};

/// One question: what date does this tenor resolve to, under this rule, for
/// this horizon, on this calendar?
struct tenor_resolution_request final {
    tenor_spec tenor;
    /// The rule for this (tenor, convention) pair, or nothing when no such
    /// row exists -- the refusal @c resolution_status::tenor_not_in_convention
    /// reports. Absent rather than impossible, because "no row" is a state the
    /// persisted data is in and a caller must be able to ask about it.
    std::optional<tenor_rule> rule;
    /// The as-of date the anchors count from.
    std::chrono::year_month_day horizon{};
    /// The spot date, when the rule's anchor is @c tenor_anchor::spot. The
    /// engine does not derive it: spot is the currency pair's own settlement
    /// date, and QuantLib derives it from the index's definition, not from a
    /// tenor. A caller without one states the lag instead and resolves it with
    /// @c calendar_arithmetic::advance before building the request.
    std::optional<std::chrono::year_month_day> spot_date;
    /// Which calendar to roll on, in the index space of the
    /// @c business_day_calendar_set the batch was handed.
    std::size_t calendar_index = 0;
};

/// Why a request resolved or did not. A batch reports per row rather than
/// throwing, so one malformed rule cannot take the other rows of the same
/// batch down with it.
enum class resolution_status {
    ok,
    /// The tenor has no resolution row under this convention.
    tenor_not_in_convention,
    /// The request names a calendar the set was not built with.
    calendar_index_out_of_range,
    /// The anchor is @c none, @c near_leg or @c imm_roll: a date this engine
    /// will not invent.
    unsupported_anchor,
    /// The anchor is @c spot and no spot date was supplied.
    missing_spot_date,
    /// A PERIOD tenor with no period of its own.
    period_tenor_without_period,
    /// A SPECIAL tenor whose rule states no offset, so it has no duration.
    special_tenor_without_offset,
    /// A @c schedule_step row that names an axis but no step count, or a step
    /// count but no axis.
    inconsistent_schedule_declaration,
    /// An @c event_lookup row whose schedule has no dates.
    schedule_without_dates,
    /// The caller's schedule dates are not ordered by (schedule_index, date),
    /// so the requested step would name a different date than it looks up.
    schedule_dates_not_ordered,
    /// The schedule has fewer dates on or after the walk start than the row
    /// asks to step to.
    schedule_exhausted,
};

/// The persisted-name-free spelling of a status, for log lines and for a
/// refusal payload that has to say why.
[[nodiscard]] ORES_ANALYTICS_QUANT_EXPORT std::string_view to_string(resolution_status status);

/// One answer, aligned by index with the request that asked it. Both dates
/// are meaningful only when @c status is @c ok.
struct tenor_resolution_result final {
    resolution_status status = resolution_status::ok;
    /// The date the rule counted from -- the anchor the answer is relative to.
    std::chrono::year_month_day anchor_date{};
    /// The resolved date, after the rule's business-day convention. The date a
    /// screen shows and a contract matures on.
    std::chrono::year_month_day resolved_date{};

    friend bool operator==(const tenor_resolution_result&,
                           const tenor_resolution_result&) = default;
};

/// Parses an @c ores_refdata_tenors_tbl.kind code.
///
/// @throws std::invalid_argument if @p code names no kind.
[[nodiscard]] ORES_ANALYTICS_QUANT_EXPORT tenor_kind parse_tenor_kind_code(std::string_view code);

/// The persisted code a kind was parsed from.
[[nodiscard]] ORES_ANALYTICS_QUANT_EXPORT std::string_view to_tenor_kind_code(tenor_kind kind);

/// Parses an @c ores_refdata_tenor_anchors_tbl.code.
///
/// @throws std::invalid_argument if @p code names no anchor.
[[nodiscard]] ORES_ANALYTICS_QUANT_EXPORT tenor_anchor
parse_tenor_anchor_code(std::string_view code);

/// The persisted code an anchor was parsed from.
[[nodiscard]] ORES_ANALYTICS_QUANT_EXPORT std::string_view
to_tenor_anchor_code(tenor_anchor anchor);

/// Parses an @c ores_refdata_tenor_resolution_algorithms_tbl.code.
///
/// @throws std::invalid_argument if @p code names no algorithm.
[[nodiscard]] ORES_ANALYTICS_QUANT_EXPORT resolution_algorithm
parse_resolution_algorithm_code(std::string_view code);

/// The persisted code an algorithm was parsed from.
[[nodiscard]] ORES_ANALYTICS_QUANT_EXPORT std::string_view
to_resolution_algorithm_code(resolution_algorithm algorithm);

} // namespace ores::analytics::quant::domain

#endif
