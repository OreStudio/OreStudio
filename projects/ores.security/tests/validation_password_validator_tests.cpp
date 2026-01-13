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
#include "ores.security/validation/password_validator.hpp"

#include <catch2/catch_test_macros.hpp>
#include "ores.logging/make_logger.hpp"

namespace {

const std::string_view test_suite("ores.security.tests");
const std::string tags("[validation]");

}

using ores::security::validation::password_validator;
using namespace ores::logging;

TEST_CASE("valid_password_passes_validation", tags) {
    auto lg(make_logger(test_suite));

    const std::string password = "ValidPass123!";
    BOOST_LOG_SEV(lg, info) << "Testing valid password: " << password;

    auto result = password_validator::validate(password);
    CHECK(result.is_valid);
    CHECK(result.error_message.empty());
}

TEST_CASE("valid_password_with_all_special_chars", tags) {
    auto lg(make_logger(test_suite));

    const std::string password = "P@ssw0rd!#$%";
    BOOST_LOG_SEV(lg, info) << "Testing password with multiple special chars: " << password;

    auto result = password_validator::validate(password);
    CHECK(result.is_valid);
    CHECK(result.error_message.empty());
}

TEST_CASE("password_exactly_12_chars_passes", tags) {
    auto lg(make_logger(test_suite));

    const std::string password = "Pass1234567!";
    BOOST_LOG_SEV(lg, info) << "Testing 12-char password: " << password;

    auto result = password_validator::validate(password);
    CHECK(result.is_valid);
    CHECK(result.error_message.empty());
}

TEST_CASE("password_too_short_fails", tags) {
    auto lg(make_logger(test_suite));

    const std::string password = "Pass123!";
    BOOST_LOG_SEV(lg, info) << "Testing short password: " << password;

    auto result = password_validator::validate(password);
    CHECK(!result.is_valid);
    CHECK(result.error_message.find("at least 12 characters") != std::string::npos);
}

TEST_CASE("password_without_uppercase_fails", tags) {
    auto lg(make_logger(test_suite));

    const std::string password = "lowercase123!";
    BOOST_LOG_SEV(lg, info) << "Testing password without uppercase: " << password;

    auto result = password_validator::validate(password);
    CHECK(!result.is_valid);
    CHECK(result.error_message.find("uppercase") != std::string::npos);
}

TEST_CASE("password_without_lowercase_fails", tags) {
    auto lg(make_logger(test_suite));

    const std::string password = "UPPERCASE123!";
    BOOST_LOG_SEV(lg, info) << "Testing password without lowercase: " << password;

    auto result = password_validator::validate(password);
    CHECK(!result.is_valid);
    CHECK(result.error_message.find("lowercase") != std::string::npos);
}

TEST_CASE("password_without_digit_fails", tags) {
    auto lg(make_logger(test_suite));

    const std::string password = "PasswordOnly!";
    BOOST_LOG_SEV(lg, info) << "Testing password without digit: " << password;

    auto result = password_validator::validate(password);
    CHECK(!result.is_valid);
    CHECK(result.error_message.find("digit") != std::string::npos);
}

TEST_CASE("password_without_special_char_fails", tags) {
    auto lg(make_logger(test_suite));

    const std::string password = "Password1234";
    BOOST_LOG_SEV(lg, info) << "Testing password without special char: " << password;

    auto result = password_validator::validate(password);
    CHECK(!result.is_valid);
    CHECK(result.error_message.find("special") != std::string::npos);
}

TEST_CASE("empty_password_fails", tags) {
    auto lg(make_logger(test_suite));

    const std::string password = "";
    BOOST_LOG_SEV(lg, info) << "Testing empty password";

    auto result = password_validator::validate(password);
    CHECK(!result.is_valid);
    CHECK(result.error_message.find("at least 12 characters") != std::string::npos);
}

TEST_CASE("password_with_all_special_chars_types", tags) {
    auto lg(make_logger(test_suite));

    std::string password = "Pass123" + std::string("!@#$%^&*()_+-=[]{}|;:,.<>?");
    BOOST_LOG_SEV(lg, info) << "Testing password with all special char types";

    auto result = password_validator::validate(password);
    CHECK(result.is_valid);
    CHECK(result.error_message.empty());
}

TEST_CASE("long_valid_password_passes", tags) {
    auto lg(make_logger(test_suite));

    const std::string password = "VeryLongPassword123!WithManyCharacters";
    BOOST_LOG_SEV(lg, info) << "Testing long valid password";

    auto result = password_validator::validate(password);
    CHECK(result.is_valid);
    CHECK(result.error_message.empty());
}

TEST_CASE("password_with_spaces_passes_if_valid", tags) {
    auto lg(make_logger(test_suite));

    const std::string password = "Pass Word 123!";
    BOOST_LOG_SEV(lg, info) << "Testing password with spaces: " << password;

    auto result = password_validator::validate(password);
    CHECK(result.is_valid);
    CHECK(result.error_message.empty());
}

TEST_CASE("weak_password_passes_when_validation_disabled", tags) {
    auto lg(make_logger(test_suite));

    const std::string weak_password = "weak";
    BOOST_LOG_SEV(lg, info) << "Testing weak password with validation disabled: " << weak_password;

    // Pass false to disable policy enforcement
    auto result = password_validator::validate(weak_password, false);
    CHECK(result.is_valid);
    CHECK(result.error_message.empty());
}

TEST_CASE("validation_enabled_by_default", tags) {
    auto lg(make_logger(test_suite));

    const std::string weak_password = "weak";
    BOOST_LOG_SEV(lg, info) << "Testing weak password with validation enabled: " << weak_password;

    // Default parameter should enforce policy
    auto result = password_validator::validate(weak_password);
    CHECK(!result.is_valid);
    CHECK(!result.error_message.empty());
}
