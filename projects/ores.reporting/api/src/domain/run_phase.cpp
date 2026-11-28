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
#include "ores.reporting.api/domain/run_phase.hpp"
#include <utility>

namespace ores::reporting::domain {

namespace {

/**
 * @brief The three phases and the word the store holds for each, stated once.
 *
 * One table rather than a switch and a chain of comparisons, so a phase added
 * here is one word to add in one place. That word is also the one the model's
 * check holds, which the store refuses a row without.
 */
constexpr std::pair<run_phase, std::string_view> phases[] = {
    {run_phase::preprocess, "preprocess"},
    {run_phase::engine, "engine"},
    {run_phase::postprocess, "postprocess"}};

}

std::string_view to_code(run_phase phase) {
    for (const auto& [one, code] : phases) {
        if (one == phase)
            return code;
    }
    return {};
}

std::optional<run_phase> phase_of_code(std::string_view code) {
    for (const auto& [phase, one] : phases) {
        if (one == code)
            return phase;
    }
    return std::nullopt;
}

}
