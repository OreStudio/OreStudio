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
#ifndef ORES_MARKETDATA_SERVICE_APP_NAMED_BY_HPP
#define ORES_MARKETDATA_SERVICE_APP_NAMED_BY_HPP

#include <algorithm>
#include <string>
#include <utility>
#include <vector>

namespace ores::marketdata::service::app {

/**
 * @brief The items a provisioning step names, in the catalogue's own order.
 *
 * A step that names nothing keeps the catalogue whole. A name the catalogue does
 * not hold is left out rather than refused: a profile that states an item the
 * installation dropped keeps working, and the run's record says what was copied.
 */
template <typename T, typename NameOf>
std::vector<T>
named_by(std::vector<T> items, const std::vector<std::string>& names, NameOf name_of) {
    if (names.empty())
        return items;

    std::vector<T> kept;
    for (auto& item : items)
        if (std::ranges::find(names, name_of(item)) != names.end())
            kept.push_back(std::move(item));
    return kept;
}

}

#endif
