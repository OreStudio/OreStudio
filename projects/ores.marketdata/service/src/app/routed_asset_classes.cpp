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
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51 Franklin
 * Street, Fifth Floor, Boston, MA 02110-1301, USA.
 */
#include "ores.marketdata.service/app/routed_asset_classes.hpp"

namespace ores::marketdata::service::app {

std::vector<std::string> routed_asset_classes(const std::set<std::string>& mapped,
                                              const std::set<std::string>& served) {
    std::vector<std::string> out;
    for (const auto& candidate : mapped)
        if (served.empty() || served.contains(candidate))
            out.push_back(candidate);
    if (out.empty() && !served.empty())
        out.assign(served.begin(), served.end());
    if (out.empty())
        out.emplace_back("fx");
    return out;
}

}
