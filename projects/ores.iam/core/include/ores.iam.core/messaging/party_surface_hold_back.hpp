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
#ifndef ORES_IAM_CORE_MESSAGING_PARTY_SURFACE_HOLD_BACK_HPP
#define ORES_IAM_CORE_MESSAGING_PARTY_SURFACE_HOLD_BACK_HPP

#include "ores.marketdata.api/messaging/fx_vol_surface_operation_protocol.hpp"
#include <algorithm>
#include <optional>
#include <string>

namespace ores::iam::messaging {

/**
 * @brief Why provisioning holds a calibrated surface back instead of publishing it, or nothing.
 *
 * Arbitrage is a blocking failure, which the surface publish refuses under any rule as well
 * (marketdata's blocking_failure_of); a change to one changes the other. A fit that did not
 * converge, or a pillar beyond the tolerance, is advisory: a decider would weigh it, and a
 * provisioning run has none, so it publishes only surfaces with neither.
 */
[[nodiscard]] inline std::optional<std::string>
party_surface_hold_back(const ores::marketdata::messaging::calibrate_fx_vol_surface_response& c) {
    if (!c.violations.empty()) {
        std::string out =
            "breaks " + std::to_string(c.violations.size()) + " arbitrage condition(s):";
        for (const auto& v : c.violations)
            out += " " + v.kind + " at " + v.expiry + ";";
        return out;
    }
    if (!c.converged)
        return std::string("the fit did not converge");
    const auto beyond = std::ranges::count_if(
        c.pillars, [](const auto& pillar) { return pillar.beyond_tolerance; });
    if (beyond > 0)
        return std::to_string(beyond) + " pillar(s) are beyond the tolerance, the largest error " +
               std::to_string(c.max_abs_error);
    return std::nullopt;
}

}

#endif
