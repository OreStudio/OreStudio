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
#include "ores.analytics.quant/service/rate_delta_tracker.hpp"
#include "ores.analytics.quant/domain/rate_status.hpp"

namespace ores::analytics::quant::service {

using domain::crm_rate_view;
using domain::rate_status;

void rate_delta_tracker::apply(std::vector<crm_rate_view>& views) {
    std::lock_guard lock(mutex_);

    for (auto& view : views) {
        const pair_key key{view.base_code, view.quote_code};
        const auto previous_it = previous_.find(key);

        if (view.status == rate_status::unavailable) {
            // Not a valid data point: no delta now, and it must not
            // overwrite whatever "previous" was recorded from the last
            // valid reading, else a transient outage would silently
            // erase the base for the next delta once data resumes.
            continue;
        }

        if (previous_it != previous_.end() && previous_it->second != 0.0) {
            view.delta_pct = (view.rate - previous_it->second) / previous_it->second * 100.0;
        }

        previous_[key] = view.rate;
    }
}

} // namespace ores::analytics::quant::service
