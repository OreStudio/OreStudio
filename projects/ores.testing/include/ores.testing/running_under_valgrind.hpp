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
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */
#ifndef ORES_TESTING_RUNNING_UNDER_VALGRIND_HPP
#define ORES_TESTING_RUNNING_UNDER_VALGRIND_HPP

#include <cstdlib>
#include <string_view>

namespace ores::testing {

/**
 * @brief Whether the test process runs under valgrind.
 *
 * A test that measures time or resident memory cannot hold under valgrind,
 * which runs the code many times slower and owns the process's memory. Such a
 * test skips there rather than fail or hang the nightly memcheck. Valgrind
 * preloads its core into every process it runs, so the preload names it. The
 * check is a heuristic: a test that clears its environment is not detected,
 * which only costs that test its skip.
 */
inline bool running_under_valgrind() {
    const char* preload = std::getenv("LD_PRELOAD");
    return preload != nullptr && std::string_view(preload).find("vgpreload") != std::string_view::npos;
}

}

#endif
