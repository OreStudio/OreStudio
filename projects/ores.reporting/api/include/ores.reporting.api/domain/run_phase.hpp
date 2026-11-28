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
#ifndef ORES_REPORTING_API_DOMAIN_RUN_PHASE_HPP
#define ORES_REPORTING_API_DOMAIN_RUN_PHASE_HPP

#include "ores.reporting.api/export.hpp"
#include <optional>
#include <string_view>

namespace ores::reporting::domain {

/**
 * @brief The part of a report run an error came from.
 *
 * A run has three phases, and each is a different failure: the run's own work
 * before the engine is handed anything, the engine's own execution, and the
 * reading and closing of what the engine produced. A reader who cannot tell
 * them apart reads a step the platform could not finish as a valuation the
 * engine got wrong.
 *
 * The three are the platform's own closed set, and the store checks the same
 * three codes in @c report_error_event.phase_code. A phase this enumeration
 * cannot state is a phase no row can hold.
 *
 * The engine's own vocabulary is not here: what an error is about is its
 * @c scope_code, which says @c Trade or @c Curve in the engine's words.
 */
enum class run_phase {
    /// The run's own work before the engine is handed anything.
    preprocess,
    /// The engine's own execution, as its log states it.
    engine,
    /// The reading, loading and closing of what the engine produced.
    postprocess
};

/**
 * @brief The code the store holds for a phase.
 *
 * These are the words @c report_error_event.phase_code holds, so the store
 * refuses a row whose phase is not one of them.
 */
[[nodiscard]] ORES_REPORTING_API_EXPORT std::string_view to_code(run_phase phase);

/**
 * @brief The phase a code names, or nothing when no phase is called that.
 *
 * A phase read from a message is external input: it is a string a sender
 * stated, and a string no phase is called is a sender that has a vocabulary
 * this process does not.
 */
[[nodiscard]] ORES_REPORTING_API_EXPORT std::optional<run_phase>
phase_of_code(std::string_view code);

}

#endif
