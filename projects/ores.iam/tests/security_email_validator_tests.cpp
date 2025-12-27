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
#include "ores.iam/security/email_validator.hpp"

#include <catch2/catch_test_macros.hpp>
#include "ores.telemetry/log/make_logger.hpp"

namespace {

const std::string_view test_suite("ores.iam.tests");
const std::string tags("[security][email]");

}

using ores::iam::security::email_validator;
using namespace ores::telemetry::log;

TEST_CASE("valid_email_passes_validation", tags) {
    auto lg(make_logger(test_suite));

    const std::string email = "user@example.com";
    BOOST_LOG_SEV(lg, info) << "Testing valid email: " << email;

    auto result = email_validator::validate(email);
    CHECK(result.is_valid);
    CHECK(result.error_message.empty());
}

TEST_CASE("valid_email_with_subdomain", tags) {
    auto lg(make_logger(test_suite));

    const std::string email = "user@mail.example.com";
    BOOST_LOG_SEV(lg, info) << "Testing email with subdomain: " << email;

    auto result = email_validator::validate(email);
    CHECK(result.is_valid);
    CHECK(result.error_message.empty());
}

TEST_CASE("valid_email_with_plus_sign", tags) {
    auto lg(make_logger(test_suite));

    const std::string email = "user+tag@example.com";
    BOOST_LOG_SEV(lg, info) << "Testing email with plus sign: " << email;

    auto result = email_validator::validate(email);
    CHECK(result.is_valid);
    CHECK(result.error_message.empty());
}

TEST_CASE("valid_email_with_dots_in_local_part", tags) {
    auto lg(make_logger(test_suite));

    const std::string email = "first.last@example.com";
    BOOST_LOG_SEV(lg, info) << "Testing email with dots in local part: " << email;

    auto result = email_validator::validate(email);
    CHECK(result.is_valid);
    CHECK(result.error_message.empty());
}

TEST_CASE("empty_email_fails", tags) {
    auto lg(make_logger(test_suite));

    const std::string email = "";
    BOOST_LOG_SEV(lg, info) << "Testing empty email";

    auto result = email_validator::validate(email);
    CHECK(!result.is_valid);
    CHECK(result.error_message.find("empty") != std::string::npos);
}

TEST_CASE("email_without_at_symbol_fails", tags) {
    auto lg(make_logger(test_suite));

    const std::string email = "userexample.com";
    BOOST_LOG_SEV(lg, info) << "Testing email without @: " << email;

    auto result = email_validator::validate(email);
    CHECK(!result.is_valid);
    CHECK(result.error_message.find("'@'") != std::string::npos);
}

TEST_CASE("email_with_multiple_at_symbols_fails", tags) {
    auto lg(make_logger(test_suite));

    const std::string email = "user@@example.com";
    BOOST_LOG_SEV(lg, info) << "Testing email with multiple @: " << email;

    auto result = email_validator::validate(email);
    CHECK(!result.is_valid);
    CHECK(result.error_message.find("only one '@'") != std::string::npos);
}

TEST_CASE("email_starting_with_at_fails", tags) {
    auto lg(make_logger(test_suite));

    const std::string email = "@example.com";
    BOOST_LOG_SEV(lg, info) << "Testing email starting with @: " << email;

    auto result = email_validator::validate(email);
    CHECK(!result.is_valid);
    CHECK(result.error_message.find("start with '@'") != std::string::npos);
}

TEST_CASE("email_ending_with_at_fails", tags) {
    auto lg(make_logger(test_suite));

    const std::string email = "user@";
    BOOST_LOG_SEV(lg, info) << "Testing email ending with @: " << email;

    auto result = email_validator::validate(email);
    CHECK(!result.is_valid);
    CHECK(result.error_message.find("end with '@'") != std::string::npos);
}

TEST_CASE("email_without_dot_in_domain_fails", tags) {
    auto lg(make_logger(test_suite));

    const std::string email = "user@localhost";
    BOOST_LOG_SEV(lg, info) << "Testing email without dot in domain: " << email;

    auto result = email_validator::validate(email);
    CHECK(!result.is_valid);
    CHECK(result.error_message.find("'.'") != std::string::npos);
}

TEST_CASE("email_with_domain_starting_with_dot_fails", tags) {
    auto lg(make_logger(test_suite));

    const std::string email = "user@.example.com";
    BOOST_LOG_SEV(lg, info) << "Testing email with domain starting with dot: " << email;

    auto result = email_validator::validate(email);
    CHECK(!result.is_valid);
    CHECK(result.error_message.find("start with '.'") != std::string::npos);
}

TEST_CASE("email_with_domain_ending_with_dot_fails", tags) {
    auto lg(make_logger(test_suite));

    const std::string email = "user@example.com.";
    BOOST_LOG_SEV(lg, info) << "Testing email with domain ending with dot: " << email;

    auto result = email_validator::validate(email);
    CHECK(!result.is_valid);
    CHECK(result.error_message.find("end with '.'") != std::string::npos);
}

TEST_CASE("valid_email_with_numbers", tags) {
    auto lg(make_logger(test_suite));

    const std::string email = "user123@example123.com";
    BOOST_LOG_SEV(lg, info) << "Testing email with numbers: " << email;

    auto result = email_validator::validate(email);
    CHECK(result.is_valid);
    CHECK(result.error_message.empty());
}

TEST_CASE("valid_email_with_hyphen_in_domain", tags) {
    auto lg(make_logger(test_suite));

    const std::string email = "user@my-example.com";
    BOOST_LOG_SEV(lg, info) << "Testing email with hyphen in domain: " << email;

    auto result = email_validator::validate(email);
    CHECK(result.is_valid);
    CHECK(result.error_message.empty());
}
