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
#include "ores.assets.api/generators/image_generator.hpp"
#include "ores.assets.core/repository/image_repository.hpp"
#include "ores.dq.api/domain/change_reason_constants.hpp"
#include "ores.iam.api/domain/account_json_io.hpp" // IWYU pragma: keep.
#include "ores.iam.api/domain/login_info.hpp"
#include "ores.iam.api/generators/account_contact_information_generator.hpp"
#include "ores.iam.api/generators/account_generator.hpp"
#include "ores.iam.api/generators/tenant_generator.hpp"
#include "ores.iam.core/repository/account_contact_information_repository.hpp"
#include "ores.iam.core/repository/account_repository.hpp"
#include "ores.iam.core/repository/login_info_repository.hpp"
#include "ores.iam.core/repository/tenant_repository.hpp"
#include "ores.iam.core/service/account_operations_service.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.security/crypto/password_hasher.hpp"
#include "ores.testing/make_generation_context.hpp"
#include "ores.testing/scoped_database_helper.hpp"
#include "ores.utility/faker/internet.hpp"
#include "ores.utility/rfl/reflectors.hpp"       // IWYU pragma: keep.
#include "ores.utility/streaming/std_vector.hpp" // IWYU pragma: keep.
#include <boost/asio/ip/address.hpp>
#include <boost/uuid/uuid_generators.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <catch2/catch_test_macros.hpp>
#include <faker-cxx/faker.h> // IWYU pragma: keep.
#include <faker-cxx/internet.h>

namespace {

const std::string_view test_suite("ores.iam.tests");
const std::string tags("[service]");

}

using namespace ores::iam;
using namespace ores::logging;
using ores::utility::faker::internet;
using ores::testing::scoped_database_helper;
using namespace ores::iam::generators;

TEST_CASE("create_account_with_valid_data", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    auto ctx = ores::testing::make_generation_context(h);
    service::account_operations_service sut(h.context());

    const auto e = generate_synthetic_account(ctx);
    BOOST_LOG_SEV(lg, info) << "Expected: " << e;

    const std::string password = faker::internet::password();
    // Note: Admin privileges are now managed via RBAC role assignments
    const auto a = sut.create_account(e.username, e.email, password, e.modified_by);
    BOOST_LOG_SEV(lg, info) << "Actual: " << a;

    CHECK(a.username == e.username);
    CHECK(a.email == e.email);

    CHECK(!a.id.is_nil());
    CHECK(!a.password_hash.value().empty());
}

TEST_CASE("create_multiple_accounts", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    auto ctx = ores::testing::make_generation_context(h);
    service::account_operations_service sut(h.context());

    for (int i = 0; i < 5; ++i) {
        BOOST_LOG_SEV(lg, info) << "Creating account: " << i;

        const auto e = generate_synthetic_account(ctx);
        BOOST_LOG_SEV(lg, info) << "Expected: " << e;

        const std::string password = faker::internet::password();
        const auto a = sut.create_account(e.username, e.email, password, e.modified_by);
        BOOST_LOG_SEV(lg, info) << "Actual: " << a;

        CHECK(a.username == e.username);
        CHECK(a.email == e.email);
        CHECK(!a.id.is_nil());
    }
}

TEST_CASE("create_account_with_empty_username_throws", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    auto ctx = ores::testing::make_generation_context(h);
    service::account_operations_service sut(h.context());

    auto e = generate_synthetic_account(ctx);
    e.username = "";
    BOOST_LOG_SEV(lg, info) << "Expected: " << e;

    const std::string password = faker::internet::password();
    CHECK_THROWS_AS(sut.create_account(e.username, e.email, password, e.modified_by),
                    std::invalid_argument);
}

TEST_CASE("create_account_with_empty_email_throws", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    auto ctx = ores::testing::make_generation_context(h);
    service::account_operations_service sut(h.context());

    auto e = generate_synthetic_account(ctx);
    e.email = "";
    BOOST_LOG_SEV(lg, info) << "Expected: " << e;

    const std::string password = faker::internet::password();
    CHECK_THROWS_AS(sut.create_account(e.username, e.email, password, e.modified_by),
                    std::invalid_argument);
}

TEST_CASE("create_account_with_empty_password_throws", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    auto ctx = ores::testing::make_generation_context(h);
    service::account_operations_service sut(h.context());

    const auto e = generate_synthetic_account(ctx);
    BOOST_LOG_SEV(lg, info) << "Expected: " << e;

    const std::string empty_password;
    CHECK_THROWS_AS(sut.create_account(e.username, e.email, empty_password, e.modified_by),
                    std::invalid_argument);
}

TEST_CASE("list_accounts_returns_existing_accounts", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    service::account_operations_service sut(h.context());
    const auto a = sut.list_accounts();
    BOOST_LOG_SEV(lg, info) << "Current accounts in database: " << a.size();
    // Test database may have accounts from previous runs; just verify
    // the method returns successfully
    INFO("Number of accounts: " << a.size());
}

TEST_CASE("list_accounts_returns_created_accounts", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    auto ctx = ores::testing::make_generation_context(h);
    service::account_operations_service sut(h.context());

    // Count existing accounts from previous test runs
    const auto initial_count = sut.list_accounts().size();
    BOOST_LOG_SEV(lg, info) << "Initial accounts: " << initial_count;

    const int new_accounts = 3;
    const auto expected_list = generate_synthetic_accounts(new_accounts, ctx);

    for (const auto& e : expected_list) {
        const std::string password = faker::internet::password();
        BOOST_LOG_SEV(lg, info) << "Creating: " << e;
        sut.create_account(e.username, e.email, password, e.modified_by);
    }

    auto actual_list = sut.list_accounts();
    BOOST_LOG_SEV(lg, info) << "Actual count: " << actual_list.size();
    CHECK(actual_list.size() == initial_count + new_accounts);
}

TEST_CASE("login_with_valid_credentials", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    auto ctx = ores::testing::make_generation_context(h);
    service::account_operations_service sut(h.context());

    auto e = generate_synthetic_account(ctx);
    BOOST_LOG_SEV(lg, info) << "Expected: " << e;

    const std::string password = faker::internet::password();
    const auto account = sut.create_account(e.username, e.email, password, e.modified_by);

    auto ip = internet::ipv4();
    auto a = sut.login(account.username, password, ip).account;

    CHECK(a.username == e.username);
    CHECK(a.id == account.id);
}

TEST_CASE("login_with_invalid_password_throws", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    auto ctx = ores::testing::make_generation_context(h);
    service::account_operations_service sut(h.context());

    auto e = generate_synthetic_account(ctx);
    BOOST_LOG_SEV(lg, info) << "Expected: " << e;

    const std::string password = faker::internet::password();
    const auto account = sut.create_account(e.username, e.email, password, e.modified_by);

    auto ip = internet::ipv4();
    CHECK_THROWS_AS(sut.login(e.username, "wrong_password", ip), std::runtime_error);
}

TEST_CASE("login_with_nonexistent_username_throws", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    service::account_operations_service sut(h.context());

    BOOST_LOG_SEV(lg, info) << "Attempting login with nonexistent username";
    const std::string username = std::string(faker::internet::username());
    const std::string password = faker::internet::password();
    auto ip = internet::ipv4();
    BOOST_LOG_SEV(lg, info) << "Creating account: - username: " << username
                            << " password: " << password << " IP: " << ip;

    CHECK_THROWS_AS(sut.login(username, password, ip), std::runtime_error);
}

TEST_CASE("login_with_empty_username_throws", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    service::account_operations_service sut(h.context());

    BOOST_LOG_SEV(lg, info) << "Attempting login with empty username";
    const std::string username;
    const std::string password = faker::internet::password();
    auto ip = internet::ipv4();
    BOOST_LOG_SEV(lg, info) << "Creating account: - username: " << username
                            << " password: " << password << " IP: " << ip;

    CHECK_THROWS_AS(sut.login(username, password, ip), std::invalid_argument);
}

TEST_CASE("login_with_empty_password_throws", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    auto ctx = ores::testing::make_generation_context(h);
    service::account_operations_service sut(h.context());

    BOOST_LOG_SEV(lg, info) << "Attempting login with empty password";

    auto account = generate_synthetic_account(ctx);
    BOOST_LOG_SEV(lg, info) << "Account: " << account;

    const std::string password = faker::internet::password();
    sut.create_account(account.username, account.email, password, account.modified_by);

    const std::string empty_password;
    auto ip = internet::ipv4();
    CHECK_THROWS_AS(sut.login(account.username, empty_password, ip), std::invalid_argument);
}

TEST_CASE("account_locks_after_multiple_failed_logins", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    auto ctx = ores::testing::make_generation_context(h);
    service::account_operations_service sut(h.context());

    auto account = generate_synthetic_account(ctx);
    BOOST_LOG_SEV(lg, info) << "Account: " << account;

    const std::string password = faker::internet::password();
    sut.create_account(account.username, account.email, password, account.modified_by);

    BOOST_LOG_SEV(lg, info) << "Attempting 5 failed logins to lock account";

    auto ip = internet::ipv4();
    for (int i = 0; i < 5; ++i) {
        try {
            sut.login(account.username, "wrong_password", ip);
        } catch (const std::runtime_error& e) {
            BOOST_LOG_SEV(lg, info) << "Failed login attempt " << (i + 1) << ": " << e.what();
        }
    }

    // Next attempt should indicate account is locked
    try {
        sut.login(account.username, password, ip);
        FAIL("Expected account to be locked.");
    } catch (const std::runtime_error& e) {
        BOOST_LOG_SEV(lg, info) << "Account locked: " << e.what();
        CHECK(std::string(e.what()).find("locked") != std::string::npos);
    }
}

TEST_CASE("lock_account_successful", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    auto ctx = ores::testing::make_generation_context(h);
    service::account_operations_service sut(h.context());

    const auto account = generate_synthetic_account(ctx);
    BOOST_LOG_SEV(lg, info) << "Account: " << account;
    const std::string password = faker::internet::password();

    const auto generated =
        sut.create_account(account.username, account.email, password, account.modified_by);

    BOOST_LOG_SEV(lg, info) << "Locking account.";
    bool lock_result = sut.lock_account(generated.id);
    CHECK(lock_result == true);

    BOOST_LOG_SEV(lg, info) << "Attempting login after lock";
    auto ip = internet::ipv4();
    CHECK_THROWS_AS(sut.login(generated.username, password, ip), std::runtime_error);
}

TEST_CASE("lock_nonexistent_account_returns_false", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    service::account_operations_service sut(h.context());

    boost::uuids::random_generator gen;
    const auto non_existent_id = gen();
    BOOST_LOG_SEV(lg, info) << "Attempting to lock nonexistent account: " << non_existent_id;

    CHECK(sut.lock_account(non_existent_id) == false);
}

TEST_CASE("unlock_account_successful", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    auto ctx = ores::testing::make_generation_context(h);
    service::account_operations_service sut(h.context());

    const auto account = generate_synthetic_account(ctx);
    BOOST_LOG_SEV(lg, info) << "Account: " << account;
    const std::string password = faker::internet::password();

    const auto generated =
        sut.create_account(account.username, account.email, password, account.modified_by);

    BOOST_LOG_SEV(lg, info) << "Locking account by failing 5 login attempts";
    auto ip = internet::ipv4();
    for (int i = 0; i < 5; ++i) {
        try {
            sut.login(account.username, "wrong_password", ip);
        } catch (...) {
        }
    }

    BOOST_LOG_SEV(lg, info) << "Unlocking account.";
    bool unlock_result = sut.unlock_account(generated.id);
    CHECK(unlock_result == true);

    BOOST_LOG_SEV(lg, info) << "Attempting login after unlock";

    // Should now be able to login successfully
    auto logged_in_account = sut.login(generated.username, password, ip).account;

    CHECK(logged_in_account.username == account.username);
}

TEST_CASE("unlock_nonexistent_account_returns_false", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    service::account_operations_service sut(h.context());

    boost::uuids::random_generator gen;
    const auto non_existent_id = gen();
    BOOST_LOG_SEV(lg, info) << "Attempting to unlock nonexistent account: " << non_existent_id;

    CHECK(sut.unlock_account(non_existent_id) == false);
}

TEST_CASE("delete_nonexistent_account_throws", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    service::account_operations_service sut(h.context());

    boost::uuids::random_generator gen;
    const auto non_existent_id = gen();
    BOOST_LOG_SEV(lg, info) << "Attempting to delete nonexistent account: " << non_existent_id;

    CHECK_THROWS_AS(sut.delete_account(non_existent_id), std::invalid_argument);
}

TEST_CASE("login_with_different_ip_addresses", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    auto ctx = ores::testing::make_generation_context(h);
    service::account_operations_service sut(h.context());

    auto account = generate_synthetic_account(ctx);
    BOOST_LOG_SEV(lg, info) << "Account: " << account;

    const std::string password = faker::internet::password();
    sut.create_account(account.username, account.email, password, account.modified_by);

    BOOST_LOG_SEV(lg, info) << "Testing logins from different IPs.";
    for (int i = 0; i < 3; ++i) {
        auto ip = internet::ipv4();
        auto login = sut.login(account.username, password, ip).account;

        BOOST_LOG_SEV(lg, info) << "Login " << i << " from IP: " << ip
                                << " - account: " << account.username;
        CHECK(account.username == login.username);
    }
}

TEST_CASE("set_my_default_party_persists_the_new_default", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    auto ctx = ores::testing::make_generation_context(h);
    service::account_operations_service sut(h.context());

    const auto e = generate_synthetic_account(ctx);
    const std::string password = faker::internet::password();
    const auto a = sut.create_account(e.username, e.email, password, e.modified_by);

    boost::uuids::random_generator gen;
    const auto party_id = gen();

    const auto err = sut.set_my_default_party(a.id, party_id);
    CHECK(err.empty());

    const auto reloaded = sut.find_account_by_id(a.id);
    REQUIRE(reloaded.has_value());
    REQUIRE(reloaded->default_party_id.has_value());
    CHECK(*reloaded->default_party_id == party_id);
}

TEST_CASE("set_my_default_party_is_idempotent_when_already_the_default", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    auto ctx = ores::testing::make_generation_context(h);
    service::account_operations_service sut(h.context());

    const auto e = generate_synthetic_account(ctx);
    const std::string password = faker::internet::password();
    const auto a = sut.create_account(e.username, e.email, password, e.modified_by);

    boost::uuids::random_generator gen;
    const auto party_id = gen();

    CHECK(sut.set_my_default_party(a.id, party_id).empty());
    // Re-running with the same party must succeed silently (no-op), not
    // fail — provisioning scripts and the shell command may legitimately
    // repeat this call against an already-provisioned system.
    CHECK(sut.set_my_default_party(a.id, party_id).empty());
}

TEST_CASE("set_my_default_party_for_nonexistent_account_returns_error", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    service::account_operations_service sut(h.context());

    boost::uuids::random_generator gen;
    const auto non_existent_id = gen();
    const auto party_id = gen();

    const auto err = sut.set_my_default_party(non_existent_id, party_id);
    CHECK(!err.empty());
}

TEST_CASE("update_account_sets_and_clears_default_party_id", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    auto ctx = ores::testing::make_generation_context(h);
    service::account_operations_service sut(h.context());

    const auto e = generate_synthetic_account(ctx);
    const std::string password = faker::internet::password();
    const auto a = sut.create_account(e.username, e.email, password, e.modified_by);

    boost::uuids::random_generator gen;
    const auto party_id = gen();

    CHECK(sut.update_account(a.id,
                             a.email,
                             "",
                             party_id,
                             "",
                             boost::uuids::nil_uuid(),
                             boost::uuids::nil_uuid(),
                             a.modified_by,
                             "common.non_material_update",
                             ""));

    auto reloaded = sut.find_account_by_id(a.id);
    REQUIRE(reloaded.has_value());
    REQUIRE(reloaded->default_party_id.has_value());
    CHECK(*reloaded->default_party_id == party_id);

    CHECK(sut.update_account(a.id,
                             a.email,
                             "",
                             std::nullopt,
                             "",
                             boost::uuids::nil_uuid(),
                             boost::uuids::nil_uuid(),
                             a.modified_by,
                             "common.non_material_update",
                             ""));

    reloaded = sut.find_account_by_id(a.id);
    REQUIRE(reloaded.has_value());
    CHECK(!reloaded->default_party_id.has_value());
}

TEST_CASE("update_self_account_writes_the_fields_a_member_owns", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    auto ctx = ores::testing::make_generation_context(h);
    service::account_operations_service sut(h.context());

    const auto e = generate_synthetic_account(ctx);
    const std::string password = faker::internet::password();
    const auto a = sut.create_account(e.username, e.email, password, e.modified_by);

    // A real image row, because the account's image_id soft-FK must point at
    // one.
    auto image = ores::assets::generators::generate_synthetic_image(ctx);
    ores::assets::repository::image_repository image_repo;
    image_repo.write(h.context(), image);

    messaging::update_self_account_request request;
    request.full_name = "Ada Lovelace";
    request.job_title = "Analyst";
    request.image_id = boost::uuids::to_string(image.id);
    request.change_reason_code = "common.non_material_update";
    request.change_commentary = "Set the profile fields I own";

    const auto response = sut.update_self_account(request, a.id);
    CHECK(response.result.outcome == ores::utility::domain::outcome::ok);
    REQUIRE(response.account.has_value());

    const auto reloaded = sut.find_account_by_id(a.id);
    REQUIRE(reloaded.has_value());
    CHECK(reloaded->full_name == "Ada Lovelace");
    CHECK(reloaded->job_title == "Analyst");
    REQUIRE(reloaded->image_id.has_value());
    CHECK(*reloaded->image_id == image.id);
    // A field the request leaves empty keeps its value.
    CHECK(reloaded->email == a.email);
}

TEST_CASE("update_self_account_refuses_a_field_only_an_administrator_owns", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    auto ctx = ores::testing::make_generation_context(h);
    service::account_operations_service sut(h.context());

    const auto e = generate_synthetic_account(ctx);
    const std::string password = faker::internet::password();
    const auto a = sut.create_account(e.username, e.email, password, e.modified_by);

    const auto before = sut.find_account_by_id(a.id);
    REQUIRE(before.has_value());

    messaging::update_self_account_request request;
    request.full_name = "Ada Lovelace";
    request.email = "ada@example.org";
    request.reports_to_account_id = boost::uuids::to_string(boost::uuids::random_generator()());

    const auto response = sut.update_self_account(request, a.id);
    CHECK(response.result.outcome == ores::utility::domain::outcome::denied);
    CHECK(response.result.code == "field_not_self_writable");
    // One entry per refused field, so the screen can point at each one.
    REQUIRE(response.result.fields.size() == 2);
    CHECK(response.result.fields[0].field == "email");
    CHECK(response.result.fields[1].field == "reports_to_account_id");
    CHECK(!response.account.has_value());

    // The refusal writes nothing, not even the owned field the request states.
    const auto reloaded = sut.find_account_by_id(a.id);
    REQUIRE(reloaded.has_value());
    CHECK(reloaded->full_name == before->full_name);
    CHECK(reloaded->email == before->email);
}

TEST_CASE("update_self_account_contact_information_creates_the_record_when_missing", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    auto ctx = ores::testing::make_generation_context(h);
    service::account_operations_service sut(h.context());

    const auto e = generate_synthetic_account(ctx);
    const std::string password = faker::internet::password();
    const auto a = sut.create_account(e.username, e.email, password, e.modified_by);

    messaging::update_self_account_contact_information_request request;
    request.street_line_1 = "1 Bridge Street";
    request.street_line_2 = "Flat 2";
    request.city = "London";
    request.state = "Greater London";
    request.country_code = "GB";
    request.postal_code = "SW1A 1AA";
    request.phone = "+44 20 7925 0918";
    request.email = "ada@example.org";
    request.web_page = "https://example.org/ada";
    request.change_reason_code = "common.non_material_update";
    request.change_commentary = "Set my contact details";

    const auto response = sut.update_self_account_contact_information(request, a.id);
    CHECK(response.result.outcome == ores::utility::domain::outcome::ok);
    REQUIRE(response.account_contact_information.has_value());
    const auto record_id = response.account_contact_information->id;
    CHECK(response.account_contact_information->account_id == a.id);
    CHECK(response.account_contact_information->city == "London");

    // A second write lands on the same record: an account keeps one contact
    // record rather than gaining a row per write.
    request.city = "Cambridge";
    const auto second = sut.update_self_account_contact_information(request, a.id);
    CHECK(second.result.outcome == ores::utility::domain::outcome::ok);
    REQUIRE(second.account_contact_information.has_value());
    CHECK(second.account_contact_information->id == record_id);
    CHECK(second.account_contact_information->city == "Cambridge");

    repository::account_contact_information_repository contact_repo;
    const auto stored =
        contact_repo.read_latest_by_account_id(h.context(), boost::uuids::to_string(a.id), 0, 100);
    REQUIRE(stored.size() == 1);
    CHECK(stored.front().id == record_id);
    CHECK(stored.front().city == "Cambridge");
}

TEST_CASE("update_self_account_reports_an_account_that_is_not_there", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    service::account_operations_service sut(h.context());

    messaging::update_self_account_request request;
    request.full_name = "Nobody";
    request.change_reason_code = "common.non_material_update";

    const auto response = sut.update_self_account(request, boost::uuids::random_generator()());
    CHECK(response.result.outcome == ores::utility::domain::outcome::missing);
    CHECK(response.result.code == "not_found");
    CHECK(!response.account.has_value());
}

TEST_CASE("update_self_account_refuses_an_image_id_that_is_not_a_uuid", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    auto ctx = ores::testing::make_generation_context(h);
    service::account_operations_service sut(h.context());

    const auto e = generate_synthetic_account(ctx);
    const std::string password = faker::internet::password();
    const auto a = sut.create_account(e.username, e.email, password, e.modified_by);

    messaging::update_self_account_request request;
    request.full_name = "Ada Lovelace";
    request.image_id = "not-a-uuid";
    request.change_reason_code = "common.non_material_update";

    const auto response = sut.update_self_account(request, a.id);
    CHECK(response.result.outcome == ores::utility::domain::outcome::invalid);
    CHECK(response.result.code == "invalid_image_id");
    REQUIRE(response.result.fields.size() == 1);
    CHECK(response.result.fields[0].field == "image_id");
    CHECK(!response.account.has_value());

    const auto reloaded = sut.find_account_by_id(a.id);
    REQUIRE(reloaded.has_value());
    CHECK(reloaded->full_name == a.full_name);
    CHECK(!reloaded->image_id.has_value());
}

TEST_CASE("update_self_account_contact_information_updates_a_record_that_already_exists", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    auto ctx = ores::testing::make_generation_context(h);
    service::account_operations_service sut(h.context());

    const auto e = generate_synthetic_account(ctx);
    const std::string password = faker::internet::password();
    const auto a = sut.create_account(e.username, e.email, password, e.modified_by);

    // A record an administrator wrote before the member ever signed in.
    auto existing = generate_synthetic_account_contact_information(ctx);
    existing.account_id = a.id;
    repository::account_contact_information_repository contact_repo;
    contact_repo.write(h.context(), existing);

    messaging::update_self_account_contact_information_request request;
    request.city = "Cambridge";
    request.country_code = "GB";
    request.change_reason_code = "common.non_material_update";

    const auto response = sut.update_self_account_contact_information(request, a.id);
    CHECK(response.result.outcome == ores::utility::domain::outcome::ok);
    REQUIRE(response.account_contact_information.has_value());
    CHECK(response.account_contact_information->id == existing.id);
    CHECK(response.account_contact_information->city == "Cambridge");
    // The write states the fields whole: the synthetic street line the request
    // does not state is replaced with nothing.
    CHECK(response.account_contact_information->street_line_1.empty());

    const auto stored =
        contact_repo.read_latest_by_account_id(h.context(), boost::uuids::to_string(a.id), 0, 100);
    REQUIRE(stored.size() == 1);
}

TEST_CASE("update_self_account_contact_information_refuses_a_bad_email_and_country_code", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    auto ctx = ores::testing::make_generation_context(h);
    service::account_operations_service sut(h.context());

    const auto e = generate_synthetic_account(ctx);
    const std::string password = faker::internet::password();
    const auto a = sut.create_account(e.username, e.email, password, e.modified_by);

    messaging::update_self_account_contact_information_request request;
    request.city = "London";
    request.country_code = "United Kingdom";
    request.email = "not-an-address";
    request.change_reason_code = "common.non_material_update";

    const auto response = sut.update_self_account_contact_information(request, a.id);
    CHECK(response.result.outcome == ores::utility::domain::outcome::invalid);
    CHECK(response.result.code == "invalid_field_value");
    REQUIRE(response.result.fields.size() == 2);
    CHECK(response.result.fields[0].field == "email");
    CHECK(response.result.fields[1].field == "country_code");
    CHECK(!response.account_contact_information.has_value());

    // The refusal writes nothing, not even the record the member lacks.
    repository::account_contact_information_repository contact_repo;
    const auto stored =
        contact_repo.read_latest_by_account_id(h.context(), boost::uuids::to_string(a.id), 0, 100);
    CHECK(stored.empty());
}

TEST_CASE("login_refused_when_the_accounts_tenant_is_suspended", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    auto sys_ctx = h.context().with_tenant(ores::utility::uuid::tenant_id::system(), h.db_user());

    // A tenant of this test's own, so the shared system tenant is never
    // suspended. Tenant rows live under the system tenant, so the writes need
    // the system tenant's context.
    repository::tenant_repository tenants;
    auto gen_ctx = ores::testing::make_generation_context(h);
    auto tenant = generate_synthetic_tenant(gen_ctx);
    tenants.write(sys_ctx, tenant);

    // The account is written through the repository rather than through
    // create_account, which leaves tenant_id at its system default. The row
    // must belong to the tenant whose suspension is under test.
    const auto tid = ores::utility::uuid::tenant_id::from_uuid(tenant.id);
    REQUIRE(tid.has_value());
    auto tenant_ctx = h.context().with_tenant(*tid, h.db_user());

    const std::string password = faker::internet::password();
    domain::account probe;
    probe.version = 0;
    probe.id = boost::uuids::random_generator()();
    probe.tenant_id = *tid;
    probe.username = "suspended.login.probe";
    probe.account_type = "user";
    probe.password_hash = ores::security::crypto::password_hasher::hash(password);
    probe.password_salt = "";
    probe.totp_secret = "";
    probe.email = "probe@example.com";
    probe.modified_by = h.db_user();
    probe.change_reason_code =
        std::string{ores::dq::domain::change_reason_constants::codes::new_record};
    probe.change_commentary = "suspended tenant login probe";

    repository::account_repository accounts;
    accounts.write(tenant_ctx, std::vector<domain::account>{probe});

    domain::login_info li{.account_id = probe.id,
                          .last_ip = {},
                          .last_attempt_ip = {},
                          .failed_logins = 0,
                          .locked = false,
                          .last_login = {},
                          .online = false};
    repository::login_info_repository logins;
    logins.write(tenant_ctx, std::vector<domain::login_info>{li});

    // Suspend the tenant and prove that its own account can no longer sign in.
    auto suspended = tenant;
    suspended.status = "suspended";
    tenants.write(sys_ctx, suspended);

    service::account_operations_service tenant_sut(tenant_ctx);
    bool refused = false;
    std::string message;
    try {
        auto ip = internet::ipv4();
        tenant_sut.login(probe.username, password, ip);
    } catch (const std::runtime_error& ex) {
        refused = true;
        message = ex.what();
    }
    CHECK(refused);
    CHECK(message == "Tenant is not active");

    // The same credentials under the system context, which is where a principal
    // with no resolvable hostname stays, reach no account at all: the account
    // row level security policy confines the read to the caller's tenant, so
    // that path cannot reach another tenant's account to begin with.
    service::account_operations_service sys_sut(h.context());
    bool system_refused = false;
    try {
        auto ip = internet::ipv4();
        sys_sut.login(probe.username, password, ip);
    } catch (const std::runtime_error&) {
        system_refused = true;
    }
    CHECK(system_refused);

    // Leave the row active. It belongs to this test alone, but the suite shares
    // one database and a later test reading it should find it usable.
    auto current = tenants.read_latest(sys_ctx, boost::uuids::to_string(tenant.id));
    if (!current.empty()) {
        auto active = current.front();
        active.status = "active";
        tenants.write(sys_ctx, active);
    }
}
