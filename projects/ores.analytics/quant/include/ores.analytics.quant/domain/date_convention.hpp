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
#ifndef ORES_ANALYTICS_QUANT_DOMAIN_DATE_CONVENTION_HPP
#define ORES_ANALYTICS_QUANT_DOMAIN_DATE_CONVENTION_HPP

#include "ores.analytics.quant/export.hpp"
#include <string>
#include <string_view>

namespace ores::analytics::quant::domain {

/// How a date that falls on a non-business day moves to one that does not.
/// Enumerator for enumerator this is QuantLib's @c BusinessDayConvention
/// (ql/time/businessdayconvention.hpp), in its order, so a reader who knows
/// QuantLib reads this without a comment.
enum class business_day_convention {
    /// The date is returned exactly as it stands, business day or not.
    unadjusted,
    /// The next business day, however far away.
    following,
    /// @c following, unless that crosses into the next month, in which case
    /// the previous business day instead.
    modified_following,
    /// The previous business day, however far back.
    preceding,
    /// @c preceding, unless that crosses into the previous month, in which
    /// case the next business day instead.
    modified_preceding,
    /// Whichever of the two neighbouring business days is closer; a date
    /// equidistant from both takes the later one.
    nearest,
    /// @c modified_following, except that a date in the second half of a
    /// month may roll back into that same half rather than into the previous
    /// month -- see @c calendar_arithmetic::adjust for the exact boundary.
    half_month_modified_following,
};

/// A length in calendar units. QuantLib's @c TimeUnit, restricted to the
/// units a tenor or an offset may carry: @c once is absent because this
/// engine has no zero-length period -- @c none stands in its place and means
/// "no period at all", which every call site must treat as its own case
/// rather than as a length of zero.
enum class period_unit {
    /// Denominated in days. In QuantLib's @c Calendar::advance a day-counted
    /// period walks *business* days, while in @c Date::advance it does not;
    /// calendar_arithmetic keeps that distinction rather than hiding it.
    day,
    week,
    month,
    year,
    /// No unit: an absent period. Distinct from a length of zero days.
    none,
};

/// A signed count of calendar units. The unit is always stated, so a period
/// can never mean "however many of whatever was assumed".
struct date_period final {
    period_unit unit = period_unit::none;
    int multiplier = 0;

    /// True when this states no length at all -- the shape every call site
    /// must reject or treat as a fallback, never silently read as zero.
    [[nodiscard]] bool is_empty() const noexcept {
        return unit == period_unit::none;
    }

    friend bool operator==(const date_period&, const date_period&) = default;
};

/// Parses an @c ores_refdata_business_day_convention_types_tbl code. The one
/// place the persisted spellings meet this component's enum; every caller
/// beyond it passes the enum.
///
/// @throws std::invalid_argument if @p code names no convention.
[[nodiscard]] ORES_ANALYTICS_QUANT_EXPORT business_day_convention
parse_business_day_convention_code(std::string_view code);

/// The persisted code a convention was parsed from, for messages that must
/// name a value the way the database does.
[[nodiscard]] ORES_ANALYTICS_QUANT_EXPORT std::string_view
to_business_day_convention_code(business_day_convention convention);

/// Parses an @c ores_refdata_tenor_units_tbl code.
///
/// @throws std::invalid_argument if @p code names no unit.
[[nodiscard]] ORES_ANALYTICS_QUANT_EXPORT period_unit parse_period_unit_code(std::string_view code);

/// The persisted code a unit was parsed from.
[[nodiscard]] ORES_ANALYTICS_QUANT_EXPORT std::string_view to_period_unit_code(period_unit unit);

} // namespace ores::analytics::quant::domain

#endif
