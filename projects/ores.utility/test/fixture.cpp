/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2024 Marco Craveiro <marco.craveiro@gmail.com>
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
#include <iostream>
#include <boost/exception/info.hpp>
#include <boost/test/unit_test_monitor.hpp>
#include <boost/exception/diagnostic_information.hpp>
#include "ores.utility/test/fixture.hpp"

namespace  {

const std::string error_msg("Error during test");

inline void translate(const boost::exception& e) {
    std::cerr << std::endl << boost::diagnostic_information(e);
    throw std::runtime_error(error_msg);
}

}

namespace ores::utility::test {

exception_fixture::exception_fixture() {
    using boost::exception;
    using boost::unit_test::unit_test_monitor;
    unit_test_monitor.register_exception_translator<exception>(&translate);
}

}
