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
#ifndef ORES_REPORTING_CORE_SERVICE_REPORT_HEALTH_HPP
#define ORES_REPORTING_CORE_SERVICE_REPORT_HEALTH_HPP

#include <cstdint>
#include <string>
#include <string_view>

namespace ores::reporting::service {

/**
 * @brief The health word the reports-a-party-runs screen draws for a run.
 *
 * The mapping is stated here, once, because the word is the server's to state:
 * a client that mapped a state name itself would have to be found and changed
 * when the mapping changes, and two clients could disagree about one run.
 *
 * The states are the seven =report_instance_lifecycle= seeds. =pending= and
 * =queued= are not words of their own: a run that has not started is in flight,
 * and the screen has one word for that. =cancelled= and =skipped= have no word
 * yet: whether a cancelled run reads apart from a clean one, and as what, is
 * still undecided (G12 of the reporting gaps task), and this function invents
 * no answer for them. A state the session cannot read, and a run that holds no
 * state, state no word either.
 *
 * @param state_name The name of the run's lifecycle state, or empty.
 * @param error_count How many errors the run recorded.
 * @return One of =failed=, =completed with errors=, =running= or
 * =completed clean=, or empty when the state has no word.
 */
inline std::string health_word(std::string_view state_name, std::int64_t error_count) {
    if (state_name == "failed")
        return "failed";
    if (state_name == "completed")
        return error_count == 0 ? "completed clean" : "completed with errors";
    if (state_name == "pending" || state_name == "queued" || state_name == "running")
        return "running";
    return {};
}

}

#endif
