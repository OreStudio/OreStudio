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
#include "ores.marketdata.service/app/delivery_meter.hpp"

namespace ores::marketdata::service::app {

void delivery_meter::add(const delivery_key& key, std::int64_t count) {
    if (count <= 0)
        return;
    std::lock_guard lock(mu_);
    counts_[key] += count;
}

void delivery_meter::add_many(const std::vector<delivery_entry>& counts) {
    std::lock_guard lock(mu_);
    for (const auto& c : counts) {
        if (c.deliveries > 0)
            counts_[c.key] += c.deliveries;
    }
}

std::vector<delivery_entry> delivery_meter::drain() {
    std::map<delivery_key, std::int64_t> taken;
    {
        std::lock_guard lock(mu_);
        taken.swap(counts_);
    }
    std::vector<delivery_entry> out;
    out.reserve(taken.size());
    for (auto& [key, count] : taken)
        out.push_back({key, count});
    return out;
}

std::size_t delivery_meter::pending() const {
    std::lock_guard lock(mu_);
    return counts_.size();
}

}
