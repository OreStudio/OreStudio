/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2025 Marco Craveiro <marco.craveiro@gmail.com>
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
#include <sstream>
#include <optional>
#include "ores.utility/log/logging_options.hpp"
#include "ores.utility/test/logging.hpp"

namespace {

/*
 * By default we don't do any logging to make test execution snappy for the
 * happy path. Set on when investigating test breaks. WARNING: remember to set
 * it back afterwards.
 */
const bool logging_enabled_globally(false);

}

namespace ores::utility::test {

ores::utility::log::scoped_lifecycle_manager
scoped_lifecycle_manager_factory(std::string test_module,
    std::string test_suite, std::string function_name,
    const bool logging_enabled_locally, const bool log_to_console) {

    using namespace ores::utility::log;
    if (!logging_enabled_globally && !logging_enabled_locally) {
        std::optional<logging_options> cfg;
        return {cfg };
    }

    std::ostringstream s;
    s << "../log/" << test_module << "/" << test_suite << "/" << function_name;
    logging_options cfg;
    cfg.filename = s.str();
    cfg.severity = "trace";
    cfg.output_to_console = log_to_console;
    return {cfg };
}

}
