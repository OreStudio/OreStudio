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
#ifndef ORES_TESTING_CONVENTIONS_ROWS_HPP
#define ORES_TESTING_CONVENTIONS_ROWS_HPP

#include "ores.ore.core/domain/conventions_package.hpp"
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include <rfl/json.hpp>
#include <stdexcept>
#include <string>
#include <string_view>
#include <vector>

namespace ores::testing {

/**
 * @brief One family's rows of a package, each decoded as that family's own type.
 *
 * A test that checks a mapped field names the family and the type it expects,
 * so the package never needs a type that holds every family.
 */
template <typename Row>
std::vector<Row> rows_of(const ores::ore::domain::conventions_package& package,
                         std::string_view family) {
    std::vector<Row> out;
    for (const auto& row : package.bundle.rows) {
        if (row.family != family)
            continue;
        auto decoded = rfl::json::read<Row>(row.payload);
        if (!decoded)
            throw std::runtime_error("A package payload does not decode: " + row.family);
        out.push_back(std::move(*decoded));
    }
    return out;
}

}

#endif
