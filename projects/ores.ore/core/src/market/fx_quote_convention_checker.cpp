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
#include "ores.ore.core/market/fx_quote_convention_checker.hpp"

namespace ores::ore::market {

using namespace ores::logging;

fx_quote_convention_checker::fx_quote_convention_checker(std::set<currency_pair> known_pairs)
    : known_pairs_(std::move(known_pairs)) {
    BOOST_LOG_SEV(lg(), debug) << "Constructed with " << known_pairs_.size() << " known pair(s).";
}

fx_quote_check_result
fx_quote_convention_checker::check(const std::string& base, const std::string& quote) const {
    BOOST_LOG_SEV(lg(), debug) << "Checking quote " << base << "/" << quote << " against "
                               << known_pairs_.size() << " known pair(s).";

    if (known_pairs_.contains({base, quote})) {
        BOOST_LOG_SEV(lg(), debug) << base << "/" << quote << " matches known canonical order — "
                                   << "unchanged.";
        return {base, quote, fx_quote_status::unchanged};
    }

    if (known_pairs_.contains({quote, base})) {
        BOOST_LOG_SEV(lg(), debug)
            << base << "/" << quote << " is reversed relative to known pair " << quote << "/" << base
            << " — swapping qualifier, value untouched.";
        return {quote, base, fx_quote_status::key_swapped};
    }

    BOOST_LOG_SEV(lg(), debug) << base << "/" << quote
                               << " is not a known pair in either order — unchanged.";
    return {base, quote, fx_quote_status::unknown_pair};
}

}
