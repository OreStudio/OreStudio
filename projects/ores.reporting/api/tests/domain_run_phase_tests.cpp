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
#include "ores.logging/logger.hpp"
#include "ores.reporting.api/domain/run_phase.hpp"
#include "test_suite.hpp"
#include <catch2/catch_test_macros.hpp>
#include <string>

/**
 * @file domain_run_phase_tests.cpp
 * @brief The part of a report run an error came from.
 *
 * The three phases are the platform's own closed set, and the store holds one
 * word for each in report_error_event.phase_code. The cases below pin the pair
 * of words the platform writes and reads, and pin that a word no phase is
 * called names no phase rather than the nearest one.
 */

namespace {

using namespace ores::logging;
using ores::reporting::domain::phase_of_code;
using ores::reporting::domain::run_phase;
using ores::reporting::domain::to_code;

}

TEST_CASE("a phase states the code the store holds for it", "[domain][reporting]") {
    auto lg(make_logger(test_suite));

    ORES_LOG_SEV(lg, debug) << "Codes: " << to_code(run_phase::preprocess) << ", "
                            << to_code(run_phase::engine) << ", "
                            << to_code(run_phase::postprocess);
    CHECK(to_code(run_phase::preprocess) == "preprocess");
    CHECK(to_code(run_phase::engine) == "engine");
    CHECK(to_code(run_phase::postprocess) == "postprocess");
}

TEST_CASE("a code names the phase it is called", "[domain][reporting]") {
    auto lg(make_logger(test_suite));

    ORES_LOG_SEV(lg, debug) << "An empty code names no phase: " << !phase_of_code("").has_value();
    CHECK(phase_of_code("preprocess") == run_phase::preprocess);
    CHECK(phase_of_code("engine") == run_phase::engine);
    CHECK(phase_of_code("postprocess") == run_phase::postprocess);
}

TEST_CASE("a code no phase is called names no phase", "[domain][reporting]") {
    auto lg(make_logger(test_suite));

    ORES_LOG_SEV(lg, debug) << "A capitalized phase name: "
                            << !phase_of_code("Preprocess").has_value();
    CHECK_FALSE(phase_of_code("").has_value());
    CHECK_FALSE(phase_of_code("Preprocess").has_value());
    CHECK_FALSE(phase_of_code("preprocessing").has_value());
    CHECK_FALSE(phase_of_code("engine ").has_value());
}
