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

namespace ores::reporting::domain {

std::string_view to_code(run_phase phase) {
    switch (phase) {
        case run_phase::preprocess:
            return "preprocess";
        case run_phase::engine:
            return "engine";
        case run_phase::postprocess:
            return "postprocess";
    }
    return {};
}

std::optional<run_phase> phase_of_code(std::string_view code) {
    if (code == "preprocess")
        return run_phase::preprocess;
    if (code == "engine")
        return run_phase::engine;
    if (code == "postprocess")
        return run_phase::postprocess;
    return std::nullopt;
}

}
