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
#include <openssl/crypto.h>
#include <boost/scope_exit.hpp>
#include <catch2/catch_session.hpp>
#include <catch2/reporters/catch_reporter_registrars.hpp>
#include "ores.testing/logging_listener.hpp"
#include "ores.testing/database_lifecycle_listener.hpp"

CATCH_REGISTER_LISTENER(ores::testing::logging_listener)
CATCH_REGISTER_LISTENER(ores::testing::database_lifecycle_listener)

int main(int argc, char* argv[]) {
    BOOST_SCOPE_EXIT(void) {
        OPENSSL_cleanup();
    } BOOST_SCOPE_EXIT_END

    ores::testing::logging_listener::set_test_module_name("ores.utility.tests");
    return Catch::Session().run(argc, argv);
}
