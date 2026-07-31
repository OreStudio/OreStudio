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
#ifndef ORES_REFDATA_CLIENT_PRESENTATION_CURRENCY_PAIR_RATE_FORMATTER_HPP
#define ORES_REFDATA_CLIENT_PRESENTATION_CURRENCY_PAIR_RATE_FORMATTER_HPP

#include "ores.refdata.api/domain/currency_pair_convention.hpp"
#include <algorithm>
#include <charconv>
#include <cmath>
#include <optional>
#include <string>

namespace ores::refdata::client::presentation {

/**
 * @brief Stateless, convention-aware formatter for a single currency-pair
 * rate. The one place any FX rate-display surface (CRM matrix, FX Spot
 * grid, and any future one) turns a raw double into a display string --
 * see the "Rate display conventions" story. Deliberately has no notion of
 * "currency" itself: every precision/tick-snapping decision comes from the
 * caller-supplied currency_pair_convention, never a hardcoded currency
 * code (e.g. no special-cased JPY anywhere in this codebase).
 *
 * Header-only, matching ores.refdata.client's current INTERFACE-library
 * convention (see its CMakeLists.txt) -- promote to a compiled .cpp if this
 * grows non-trivial state or a second translation unit needs it without
 * risking ODR-sensitive inlining.
 */
class currency_pair_rate_formatter final {
public:
    /**
     * @brief Formats @p rate against its (optional) convention.
     *
     * With a convention and @p convention_reversed false: snaps to the
     * pair's minimum tick (tick_size * pip_factor) before rendering at
     * decimal_places precision -- a rate whose last digit doesn't fall on
     * a real tick is not a value the pair can actually quote.
     *
     * With a convention and @p convention_reversed true: the convention
     * was resolved against the *reverse* of this rate's own direction
     * (e.g. rate is USD/EUR but the stored convention is EUR/USD) -- a
     * real market convention only ever quotes one direction of a pair, so
     * the reciprocal direction is always derived, never separately
     * stored. decimal_places/tick_size are calibrated for the
     * convention's own direction and don't transfer as-is to the
     * reciprocal's magnitude, so this re-derives an equivalent precision
     * (preserving significant figures) instead of reusing them blindly,
     * and skips tick-snapping (the reciprocal of an on-tick rate isn't
     * itself generally on-tick).
     *
     * With no convention: rendered unsnapped at a fixed default precision
     * (5 decimal places, matching the pre-formatter Qt default of
     * QString::number(rate, 'f', 5)).
     */
    [[nodiscard]] static std::string
    format_rate(double rate,
                const std::optional<ores::refdata::domain::currency_pair_convention>& convention,
                bool convention_reversed) {
        if (!convention)
            return to_fixed_string(rate, default_decimal_places);

        if (convention_reversed)
            return to_fixed_string(rate, reciprocal_decimal_places(rate, *convention));

        const double absolute_tick = convention->tick_size * convention->pip_factor;
        double snapped = rate;
        if (absolute_tick > 0.0)
            snapped = std::round(rate / absolute_tick) * absolute_tick;

        return to_fixed_string(snapped, convention->decimal_places);
    }

private:
    static constexpr int default_decimal_places = 5;

    /// Allocation-free fixed-precision double->string via to_chars, rather
    /// than ostringstream/iomanip -- this can run per-cell across hundreds
    /// of rows per reload/tick, and stream construction/imbue is a real
    /// cost at that scale to_chars avoids entirely.
    [[nodiscard]] static std::string to_fixed_string(double value, int precision) {
        char buf[64];
        const auto res =
            std::to_chars(buf, buf + sizeof(buf), value, std::chars_format::fixed, precision);
        return std::string(buf, res.ptr);
    }

    /// floor(log10(x)) for x > 0 -- the base-10 order of magnitude (e.g.
    /// 82.99 -> 1, 0.012 -> -2). Nudges by a small epsilon before flooring:
    /// log10 isn't guaranteed correctly-rounded on every libm, so an x
    /// that's an exact (or near-exact) power of ten can evaluate
    /// fractionally below the integer (e.g. 1.9999999999998 for
    /// log10(100.0)), which floor() would then round down one too many.
    [[nodiscard]] static int order_of_magnitude(double x) {
        return static_cast<int>(std::floor(std::log10(x) + 1e-9));
    }

    /// Derives decimal_places for the reciprocal of a rate whose *own*
    /// direction is described by convention.decimal_places, preserving
    /// significant figures across the reciprocal instead of reusing
    /// decimal_places verbatim (which is only valid for the convention's
    /// own direction/magnitude). rate is the reciprocal value being
    /// rendered, so 1/rate recovers the direct-direction magnitude the
    /// convention assumes.
    [[nodiscard]] static int
    reciprocal_decimal_places(double rate,
                              const ores::refdata::domain::currency_pair_convention& convention) {
        if (rate <= 0.0)
            return default_decimal_places;

        const int direct_order = order_of_magnitude(1.0 / rate);
        const int significant_figures = direct_order + 1 + convention.decimal_places;
        const int reciprocal_order = order_of_magnitude(rate);
        return std::max(0, significant_figures - 1 - reciprocal_order);
    }
};

}

#endif
