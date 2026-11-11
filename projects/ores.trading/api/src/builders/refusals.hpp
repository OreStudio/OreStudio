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
#ifndef ORES_TRADING_API_SRC_BUILDERS_REFUSALS_HPP
#define ORES_TRADING_API_SRC_BUILDERS_REFUSALS_HPP

#include "ores.trading.api/builders/build_error.hpp"
#include <chrono>
#include <optional>

/// The refusals more than one builder family states.
namespace ores::trading::builders::refusals {

/// A period that does not end after it starts, named by its end.
[[nodiscard]] inline std::optional<build_error> period(std::chrono::year_month_day start,
                                                       std::chrono::year_month_day end) {
    if (end <= start)
        return build_error{"end", "must be after the start"};
    return std::nullopt;
}

}

#endif
