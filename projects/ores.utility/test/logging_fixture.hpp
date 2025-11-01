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
#ifndef ORES_UTILITY_TEST_LOGGING_FIXTURE_HPP
#define ORES_UTILITY_TEST_LOGGING_FIXTURE_HPP

#if defined(_MSC_VER) && (_MSC_VER >= 1200)
#pragma once
#endif

#include <string>
#include "ores.utility/log/make_logger.hpp"
#include "ores.utility/log/scoped_lifecycle_manager.hpp"

namespace ores::utility::test {

/**
 * @brief Base fixture for test logging setup.
 *
 * Encapsulates the logging infrastructure required for tests, including
 * lifecycle management and logger creation. Intended to be used as a base
 * class for test-specific fixtures.
 */
struct logging_fixture_base {
    logging_fixture_base(const std::string& module,
                        const std::string& suite,
                        const std::string& function_name,
                        bool enable_debug = true);

    ~logging_fixture_base() = default;

    auto& lg() { return lg_; }

private:
    ores::utility::log::scoped_lifecycle_manager sl_;
    decltype(ores::utility::log::make_logger("")) lg_;
};

/**
 * @brief Macro to define a logging fixture for a test case.
 *
 * Creates a fixture structure named <test_name>_fixture that inherits from
 * logging_fixture_base. The fixture automatically sets up logging using the
 * test_module and test_suite constants that must be defined in scope.
 *
 * Usage:
 *   LOGGING_FIXTURE(my_test);
 *   BOOST_FIXTURE_TEST_CASE(my_test, my_test_fixture) {
 *       BOOST_LOG_SEV(lg(), info) << "Test message";
 *   }
 */
#define LOGGING_FIXTURE(test_name) \
    struct test_name##_fixture : ores::utility::test::logging_fixture_base { \
        test_name##_fixture() \
            : logging_fixture_base(test_module, test_suite, #test_name, true) {} \
    }

}

#endif
