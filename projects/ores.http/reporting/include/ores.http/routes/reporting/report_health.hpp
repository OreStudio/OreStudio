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
#ifndef ORES_HTTP_ROUTES_REPORTING_REPORT_HEALTH_HPP
#define ORES_HTTP_ROUTES_REPORTING_REPORT_HEALTH_HPP

#include <algorithm>
#include <array>
#include <optional>
#include <string>
#include <string_view>

namespace ores::http::routes::reporting {

/**
 * @brief The health word a run reads as, as the reporting service derives it.
 *
 * The words are the service's vocabulary and this only names them: a word the
 * service states that is not here reads as absent, so a screen draws no cell
 * rather than a word nobody defined.
 */
namespace report_health {

/// The words a run's health cell can hold.
inline constexpr std::array<std::string_view, 5> words{
    "failed", "completed with errors", "running", "never run", "completed clean"};

/**
 * @brief The word a stated health is, or nothing when it is no word.
 */
[[nodiscard]] inline std::optional<std::string> word_of(const std::string& stated) {
    const auto found = std::ranges::find(words, stated);
    if (found == words.end())
        return std::nullopt;
    return std::string(*found);
}

}

}

#endif
