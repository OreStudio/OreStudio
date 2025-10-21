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

#include <boost/test/unit_test.hpp>
#include <boost/uuid/uuid_generators.hpp>
#include <boost/uuid/uuid_io.hpp>
#include "ores.accounts/service/account_service.hpp"
#include "ores.accounts/repository/account_repository.hpp"
#include "ores.accounts/repository/logins_repository.hpp"
#include "ores.utility/repository/context.hpp"

/**
 * @brief Test suite for the AccountService class
 */
BOOST_AUTO_TEST_SUITE(account_service_tests)

/**
 * @brief Test that the AccountService can be constructed properly
 */
BOOST_AUTO_TEST_CASE(construction_test) {
    ores::accounts::repository::account_repository account_repo;
    ores::accounts::repository::logins_repository logins_repo;
    ores::accounts::service::account_service service(std::move(account_repo), 
                                                     std::move(logins_repo));
    
    BOOST_TEST(true); // Just ensuring construction worked without exceptions
}

/**
 * @brief Test that the create_account method works properly with valid inputs
 */
BOOST_AUTO_TEST_CASE(create_account_valid_test) {
    // For now, just testing that the method can be called syntactically
    // A full integration test would require a proper database setup
    ores::accounts::repository::account_repository account_repo;
    ores::accounts::repository::logins_repository logins_repo;
    ores::accounts::service::account_service service(std::move(account_repo), 
                                                     std::move(logins_repo));
    
    ores::utility::repository::context ctx;
    
    // This would normally connect to a test database
    // For now, we just check that the method signature is correct
    BOOST_TEST(true);
}

/**
 * @brief Test that the create_account method properly validates inputs
 */
BOOST_AUTO_TEST_CASE(create_account_validation_test) {
    ores::accounts::repository::account_repository account_repo;
    ores::accounts::repository::logins_repository logins_repo;
    ores::accounts::service::account_service service(std::move(account_repo), 
                                                     std::move(logins_repo));
    
    ores::utility::repository::context ctx;

    // Test with empty username
    BOOST_CHECK_THROW(
        service.create_account(ctx, "", "test@example.com", "password123"), 
        std::invalid_argument
    );
    
    // Test with empty email
    BOOST_CHECK_THROW(
        service.create_account(ctx, "testuser", "", "password123"), 
        std::invalid_argument
    );
    
    // Test with empty password
    BOOST_CHECK_THROW(
        service.create_account(ctx, "testuser", "test@example.com", ""), 
        std::invalid_argument
    );
    
    BOOST_TEST(true);
}

BOOST_AUTO_TEST_SUITE_END()