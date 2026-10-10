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
#include "ores.iam.api/generators/account_party_generator.hpp"
#include "ores.iam.core/repository/account_party_repository.hpp"
#include "ores.iam.core/repository/account_repository.hpp"
#include "ores.iam.core/repository/login_info_repository.hpp"
#include "ores.iam.core/repository/tenant_repository.hpp"
#include "ores.iam.core/service/account_operations_service.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.refdata.api/generators/party_generator.hpp"
#include "ores.refdata.core/repository/party_repository.hpp"
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
#include <optional>
#include <set>

namespace {

const std::string_view test_suite("ores.iam.tests");
const std::string tags("[service]");

/** The depth the tree states for one account, or a value no depth can be. */
int tree_depth(const ores::iam::messaging::get_reporting_tree_response& tree,
               const boost::uuids::uuid& id) {
    const auto wanted = boost::uuids::to_string(id);
    for (const auto& node : tree.nodes) {
        if (node.account_id == wanted) {
            return node.depth;
        }
    }
    return -100;
}

/** How many report directly to one account, as the tree states it. */
int tree_direct_reports(const ores::iam::messaging::get_reporting_tree_response& tree,
                        const boost::uuids::uuid& id) {
    const auto wanted = boost::uuids::to_string(id);
    for (const auto& node : tree.nodes) {
        if (node.account_id == wanted) {
            return node.direct_reports;
        }
    }
    return -100;
}

/** A party under the tenant's system party, which is what a seeded party sits under. */
boost::uuids::uuid new_party(ores::testing::scoped_database_helper& h,
                             ores::utility::generation::generation_context& ctx) {
    ores::refdata::repository::party_repository repo;
    boost::uuids::uuid system_party;
    bool found = false;
    for (const auto& p : repo.read_latest(h.context())) {
        if (p.tenant_id == h.tenant_id() && p.party_category == "System") {
            system_party = p.id;
            found = true;
            break;
        }
    }
    REQUIRE(found);
    auto party = ores::refdata::generators::generate_synthetic_party(ctx);
    party.change_reason_code = "system.test";
    party.parent_party_id = system_party;
    repo.write(h.context(), party);
    return party.id;
}

/** An account the store accepts, made the way the other cases make theirs. */
boost::uuids::uuid new_account(ores::iam::service::account_operations_service& sut,
                               ores::utility::generation::generation_context& ctx) {
    const auto e = ores::iam::generators::generate_synthetic_account(ctx);
    return sut.create_account(e.username, e.email, faker::internet::password(), e.modified_by).id;
}

/** Makes an account work in a party. */
void link_to(ores::testing::scoped_database_helper& h,
             ores::utility::generation::generation_context& ctx,
             const boost::uuids::uuid& account_id,
             const boost::uuids::uuid& party_id) {
    ores::iam::repository::account_party_repository links(h.context());
    auto ap = ores::iam::generators::generate_synthetic_account_party(ctx);
    ap.account_id = account_id;
    ap.party_id = party_id;
    links.write(ap);
}

/** The accounts a tree names, as text, so a case can compare them as a set. */
std::set<std::string> tree_accounts(const ores::iam::messaging::get_reporting_tree_response& tree) {
    std::set<std::string> ids;
    for (const auto& node : tree.nodes) {
        ids.insert(node.account_id);
    }
    return ids;
}

const ores::iam::messaging::reporting_tree_node*
tree_node(const ores::iam::messaging::get_reporting_tree_response& tree,
          const boost::uuids::uuid& id) {
    const auto wanted = boost::uuids::to_string(id);
    for (const auto& node : tree.nodes) {
        if (node.account_id == wanted) {
            return &node;
        }
    }
    return nullptr;
}

std::string text(const boost::uuids::uuid& id) {
    return boost::uuids::to_string(id);
}

/** Sets one account's manager through the narrow write, which must succeed. */
void set_manager(ores::iam::service::account_operations_service& sut,
                 const boost::uuids::uuid& account_id,
                 const boost::uuids::uuid& manager_id) {
    ores::iam::messaging::set_reporting_line_request req;
    req.account_id = boost::uuids::to_string(account_id);
    req.reports_to_account_id = boost::uuids::to_string(manager_id);
    REQUIRE(sut.set_reporting_line(req).result.outcome == ores::utility::domain::outcome::ok);
}

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
}

/**
 * The own-account read answers the account the session names, and only that
 * one: the request carries no account id, so there is nothing to name another
 * account with. It checks no permission, because it is a self read.
 */
TEST_CASE("get_my_account_reads_the_callers_own_account_and_no_other", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    auto ctx = ores::testing::make_generation_context(h);
    service::account_operations_service sut(h.context());

    const auto mine = generate_synthetic_account(ctx);
    const auto theirs = generate_synthetic_account(ctx);
    const auto a = sut.create_account(
        mine.username, mine.email, faker::internet::password(), mine.modified_by);
    const auto b = sut.create_account(
        theirs.username, theirs.email, faker::internet::password(), theirs.modified_by);

    const auto response = sut.get_my_account(a.id);
    BOOST_LOG_SEV(lg, info) << "Own account: " << response.account.has_value();

    CHECK(response.result.outcome == ores::utility::domain::outcome::ok);
    REQUIRE(response.account.has_value());
    CHECK(response.account->id == a.id);
    CHECK(response.account->username == mine.username);
    CHECK(response.account->id != b.id);
}

TEST_CASE("get_my_account_states_no_account_for_a_session_that_names_none", tags) {
    scoped_database_helper h;
    service::account_operations_service sut(h.context());

    const auto response = sut.get_my_account(boost::uuids::random_generator()());

    CHECK(response.result.outcome == ores::utility::domain::outcome::ok);
    CHECK(!response.account.has_value());
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

TEST_CASE("set_my_default_party_clears_the_default_when_no_party_is_stated", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    auto ctx = ores::testing::make_generation_context(h);
    service::account_operations_service sut(h.context());

    const auto e = generate_synthetic_account(ctx);
    const std::string password = faker::internet::password();
    const auto a = sut.create_account(e.username, e.email, password, e.modified_by);

    boost::uuids::random_generator gen;
    const auto party_id = gen();

    REQUIRE(sut.set_my_default_party(a.id, party_id).empty());
    CHECK(sut.set_my_default_party(a.id, std::nullopt).empty());

    const auto reloaded = sut.find_account_by_id(a.id);
    REQUIRE(reloaded.has_value());
    CHECK_FALSE(reloaded->default_party_id.has_value());

    // Clearing a default that is not set is a no-op, not a failure.
    CHECK(sut.set_my_default_party(a.id, std::nullopt).empty());
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

TEST_CASE("set_reporting_line_writes_one_field_and_records_the_reason", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    auto ctx = ores::testing::make_generation_context(h);
    service::account_operations_service sut(h.context());

    const std::string password = faker::internet::password();
    const auto e1 = generate_synthetic_account(ctx);
    const auto report = sut.create_account(e1.username, e1.email, password, e1.modified_by);
    const auto e2 = generate_synthetic_account(ctx);
    const auto boss = sut.create_account(e2.username, e2.email, password, e2.modified_by);

    const auto before = sut.find_account_by_id(report.id);
    REQUIRE(before.has_value());

    ores::iam::messaging::set_reporting_line_request req;
    req.account_id = boost::uuids::to_string(report.id);
    req.reports_to_account_id = boost::uuids::to_string(boss.id);
    req.expected_version = std::to_string(before->version);
    req.change_reason_code = "common.non_material_update";
    req.change_commentary = "Moved desk";

    const auto response = sut.set_reporting_line(req);

    REQUIRE(response.result.outcome == ores::utility::domain::outcome::ok);
    REQUIRE(response.account.has_value());
    REQUIRE(response.account->reports_to_account_id.has_value());
    CHECK(*response.account->reports_to_account_id == boss.id);
    CHECK(response.account->change_commentary == "Moved desk");
    // Every other field keeps the value the account already had: this is the
    // point of a write narrower than update_account.
    CHECK(response.account->email == before->email);
    CHECK(response.account->full_name == before->full_name);
    CHECK(response.account->job_title == before->job_title);
}

TEST_CASE("set_reporting_line_refuses_a_version_the_caller_has_not_seen", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    auto ctx = ores::testing::make_generation_context(h);
    service::account_operations_service sut(h.context());

    const std::string password = faker::internet::password();
    const auto e1 = generate_synthetic_account(ctx);
    const auto report = sut.create_account(e1.username, e1.email, password, e1.modified_by);
    const auto e2 = generate_synthetic_account(ctx);
    const auto boss = sut.create_account(e2.username, e2.email, password, e2.modified_by);

    const auto before = sut.find_account_by_id(report.id);
    REQUIRE(before.has_value());

    ores::iam::messaging::set_reporting_line_request req;
    req.account_id = boost::uuids::to_string(report.id);
    req.reports_to_account_id = boost::uuids::to_string(boss.id);
    req.expected_version = std::to_string(before->version + 1);

    const auto response = sut.set_reporting_line(req);

    CHECK(response.result.outcome == ores::utility::domain::outcome::conflict);
    CHECK_FALSE(response.account.has_value());
    const auto unchanged = sut.find_account_by_id(report.id);
    REQUIRE(unchanged.has_value());
    CHECK_FALSE(unchanged->reports_to_account_id.has_value());
}

TEST_CASE("set_reporting_line_clears_the_line_and_refuses_self_reporting", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    auto ctx = ores::testing::make_generation_context(h);
    service::account_operations_service sut(h.context());

    const std::string password = faker::internet::password();
    const auto e1 = generate_synthetic_account(ctx);
    const auto report = sut.create_account(e1.username, e1.email, password, e1.modified_by);
    const auto e2 = generate_synthetic_account(ctx);
    const auto boss = sut.create_account(e2.username, e2.email, password, e2.modified_by);

    ores::iam::messaging::set_reporting_line_request req;
    req.account_id = boost::uuids::to_string(report.id);
    req.reports_to_account_id = boost::uuids::to_string(boss.id);
    REQUIRE(sut.set_reporting_line(req).result.outcome == ores::utility::domain::outcome::ok);

    // An empty manager clears the line, with no version stated.
    req.reports_to_account_id.clear();
    req.expected_version.clear();
    const auto cleared = sut.set_reporting_line(req);
    REQUIRE(cleared.result.outcome == ores::utility::domain::outcome::ok);
    REQUIRE(cleared.account.has_value());
    CHECK_FALSE(cleared.account->reports_to_account_id.has_value());

    // A person cannot report to themselves.
    req.reports_to_account_id = boost::uuids::to_string(report.id);
    const auto self = sut.set_reporting_line(req);
    CHECK(self.result.outcome == ores::utility::domain::outcome::invalid);
    CHECK(self.result.code == "self_reporting");
}

TEST_CASE("get_reporting_tree_answers_depth_and_the_direct_reports", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    auto ctx = ores::testing::make_generation_context(h);
    service::account_operations_service sut(h.context());

    const std::string password = faker::internet::password();
    const auto e = [&] {
        const auto generated = generate_synthetic_account(ctx);
        return sut.create_account(
            generated.username, generated.email, password, generated.modified_by);
    };
    const auto boss = e();
    const auto first = e();
    const auto second = e();
    const auto junior = e();

    set_manager(sut, first.id, boss.id);
    set_manager(sut, second.id, boss.id);
    set_manager(sut, junior.id, first.id);

    ores::iam::messaging::get_reporting_tree_request req;
    const auto tree = sut.get_reporting_tree(req);

    REQUIRE(tree.result.outcome == ores::utility::domain::outcome::ok);
    CHECK(tree_depth(tree, boss.id) == 0);
    CHECK(tree_depth(tree, first.id) == 1);
    CHECK(tree_depth(tree, second.id) == 1);
    CHECK(tree_depth(tree, junior.id) == 2);
    CHECK(tree_direct_reports(tree, boss.id) == 2);
    CHECK(tree_direct_reports(tree, first.id) == 1);
    CHECK(tree_direct_reports(tree, junior.id) == 0);

    // Shallowest first, so a reader meets the shape in the order it is drawn.
    // The unrooted rows come last by design, so the run stops at the first.
    for (std::size_t i = 1; i < tree.nodes.size(); ++i) {
        if (tree.nodes[i].depth < 0) {
            break;
        }
        CHECK(tree.nodes[i - 1].depth <= tree.nodes[i].depth);
    }
}

/**
 * A member sees the people of the parties they work in, and nobody else's. The
 * tenant holds more people than that, and the read must not answer them.
 */
TEST_CASE("the_reporting_tree_of_a_viewer_holds_the_people_of_their_parties_only", tags) {
    scoped_database_helper h;
    auto ctx = ores::testing::make_generation_context(h);
    service::account_operations_service sut(h.context());

    const auto north = new_party(h, ctx);
    const auto south = new_party(h, ctx);
    const auto viewer = new_account(sut, ctx);
    const auto colleague = new_account(sut, ctx);
    const auto stranger = new_account(sut, ctx);
    link_to(h, ctx, viewer, north);
    link_to(h, ctx, colleague, north);
    link_to(h, ctx, stranger, south);

    ores::iam::messaging::get_reporting_tree_request req;
    const auto tree = sut.get_reporting_tree(req, viewer);

    CHECK(tree_accounts(tree) == std::set<std::string>{text(viewer), text(colleague)});
    REQUIRE(tree.parties.size() == 1);
    CHECK(tree.parties.front().party_id == text(north));
}

TEST_CASE("a_viewer_who_works_in_several_parties_sees_all_of_them", tags) {
    scoped_database_helper h;
    auto ctx = ores::testing::make_generation_context(h);
    service::account_operations_service sut(h.context());

    const auto north = new_party(h, ctx);
    const auto south = new_party(h, ctx);
    const auto east = new_party(h, ctx);
    const auto head = new_account(sut, ctx);
    const auto in_north = new_account(sut, ctx);
    const auto in_south = new_account(sut, ctx);
    const auto in_east = new_account(sut, ctx);
    link_to(h, ctx, head, north);
    link_to(h, ctx, head, south);
    link_to(h, ctx, in_north, north);
    link_to(h, ctx, in_south, south);
    link_to(h, ctx, in_east, east);

    ores::iam::messaging::get_reporting_tree_request req;
    const auto tree = sut.get_reporting_tree(req, head);

    CHECK(tree_accounts(tree) ==
          std::set<std::string>{text(head), text(in_north), text(in_south)});
    const auto* node = tree_node(tree, head);
    REQUIRE(node != nullptr);
    CHECK(node->party_ids.size() == 2);
}

/**
 * The head of a group sees everyone under them, including people who work in a
 * party the head is not linked to: who reports to you is yours to see.
 */
TEST_CASE("a_viewer_sees_everyone_who_reports_to_them_whatever_their_party", tags) {
    scoped_database_helper h;
    auto ctx = ores::testing::make_generation_context(h);
    service::account_operations_service sut(h.context());

    const auto holding = new_party(h, ctx);
    const auto subsidiary = new_party(h, ctx);
    const auto ceo = new_account(sut, ctx);
    const auto director = new_account(sut, ctx);
    const auto analyst = new_account(sut, ctx);
    const auto outsider = new_account(sut, ctx);
    link_to(h, ctx, ceo, holding);
    link_to(h, ctx, director, subsidiary);
    link_to(h, ctx, analyst, subsidiary);
    link_to(h, ctx, outsider, subsidiary);
    set_manager(sut, director, ceo);
    set_manager(sut, analyst, director);

    ores::iam::messaging::get_reporting_tree_request req;
    const auto tree = sut.get_reporting_tree(req, ceo);

    // The outsider shares the subsidiary with the people below the CEO, but the
    // CEO is not linked to it and the outsider does not report to the CEO.
    CHECK(tree_accounts(tree) ==
          std::set<std::string>{text(ceo), text(director), text(analyst)});
    CHECK(tree_depth(tree, analyst) == 2);
    // The parties of the people below the CEO are drawn too.
    CHECK(tree.parties.size() == 2);
}

/**
 * A colleague who shares one of the viewer's parties and works in another one
 * must not make that other party visible to the viewer.
 */
TEST_CASE("a_peers_other_party_is_not_named_to_the_viewer", tags) {
    scoped_database_helper h;
    auto ctx = ores::testing::make_generation_context(h);
    service::account_operations_service sut(h.context());

    const auto north = new_party(h, ctx);
    const auto south = new_party(h, ctx);
    const auto viewer = new_account(sut, ctx);
    const auto peer = new_account(sut, ctx);
    link_to(h, ctx, viewer, north);
    link_to(h, ctx, peer, north);
    link_to(h, ctx, peer, south);

    ores::iam::messaging::get_reporting_tree_request req;
    const auto tree = sut.get_reporting_tree(req, viewer);

    REQUIRE(tree.parties.size() == 1);
    CHECK(tree.parties.front().party_id == text(north));
    const auto* node = tree_node(tree, peer);
    REQUIRE(node != nullptr);
    CHECK(node->party_ids == std::vector<std::string>{text(north)});
}

/**
 * A person with a manager is never drawn as one without. A manager the viewer
 * may not see is not named, and the person carries a marker saying so.
 */
TEST_CASE("a_manager_the_viewer_may_not_see_is_marked_and_not_dropped", tags) {
    scoped_database_helper h;
    auto ctx = ores::testing::make_generation_context(h);
    service::account_operations_service sut(h.context());

    const auto north = new_party(h, ctx);
    const auto south = new_party(h, ctx);
    const auto viewer = new_account(sut, ctx);
    const auto report = new_account(sut, ctx);
    const auto boss = new_account(sut, ctx);
    link_to(h, ctx, viewer, north);
    link_to(h, ctx, report, north);
    link_to(h, ctx, boss, south);
    set_manager(sut, report, boss);

    ores::iam::messaging::get_reporting_tree_request req;
    const auto tree = sut.get_reporting_tree(req, viewer);

    const auto* node = tree_node(tree, report);
    REQUIRE(node != nullptr);
    CHECK(node->reports_outside_scope);
    CHECK(node->reports_to_account_id.empty());
    CHECK(node->depth == 0);
    CHECK(tree.unrooted == 0);
    CHECK(tree_node(tree, boss) == nullptr);
}

TEST_CASE("the_tenants_reporting_tree_lists_every_party_and_each_accounts_parties", tags) {
    scoped_database_helper h;
    auto ctx = ores::testing::make_generation_context(h);
    service::account_operations_service sut(h.context());

    const auto north = new_party(h, ctx);
    const auto south = new_party(h, ctx);
    const auto both = new_account(sut, ctx);
    link_to(h, ctx, both, north);
    link_to(h, ctx, both, south);

    ores::iam::messaging::get_reporting_tree_request req;
    const auto tree = sut.get_reporting_tree(req);

    std::set<std::string> parties;
    for (const auto& party : tree.parties) {
        parties.insert(party.party_id);
    }
    CHECK(parties.count(text(north)) == 1);
    CHECK(parties.count(text(south)) == 1);
    const auto* node = tree_node(tree, both);
    REQUIRE(node != nullptr);
    CHECK(node->party_ids.size() == 2);
    CHECK(!node->reports_outside_scope);
}

TEST_CASE("get_reporting_tree_answers_one_branch_from_a_stated_root", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    auto ctx = ores::testing::make_generation_context(h);
    service::account_operations_service sut(h.context());

    const std::string password = faker::internet::password();
    const auto e = [&] {
        const auto generated = generate_synthetic_account(ctx);
        return sut.create_account(
            generated.username, generated.email, password, generated.modified_by);
    };
    const auto boss = e();
    const auto first = e();
    const auto other = e();
    const auto junior = e();

    set_manager(sut, first.id, boss.id);
    set_manager(sut, other.id, boss.id);
    set_manager(sut, junior.id, first.id);

    ores::iam::messaging::get_reporting_tree_request req;
    req.root_account_id = boost::uuids::to_string(first.id);
    const auto tree = sut.get_reporting_tree(req);

    REQUIRE(tree.result.outcome == ores::utility::domain::outcome::ok);
    REQUIRE(tree.nodes.size() == 2);
    CHECK(tree_depth(tree, first.id) == 0);
    CHECK(tree_depth(tree, junior.id) == 1);
    // The branch is the branch: what sits above the root, and what belongs to
    // no root elsewhere in the tenant, is not part of this answer.
    CHECK(tree_depth(tree, boss.id) == -100);
    CHECK(tree_depth(tree, other.id) == -100);
}

TEST_CASE("get_reporting_tree_counts_an_account_whose_manager_is_gone", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    auto ctx = ores::testing::make_generation_context(h);
    service::account_operations_service sut(h.context());

    const std::string password = faker::internet::password();
    const auto e = [&] {
        const auto generated = generate_synthetic_account(ctx);
        return sut.create_account(
            generated.username, generated.email, password, generated.modified_by);
    };
    const auto boss = e();
    const auto report = e();
    set_manager(sut, report.id, boss.id);

    // The manager leaves. The line still names them, and the read states that
    // the person reaches no root rather than drawing them as one.
    repository::account_repository accounts;
    accounts.remove(h.context(), boost::uuids::to_string(boss.id));

    ores::iam::messaging::get_reporting_tree_request req;
    const auto tree = sut.get_reporting_tree(req);

    REQUIRE(tree.result.outcome == ores::utility::domain::outcome::ok);
    // The suite shares a tenant, so the total is a floor rather than a count:
    // what matters is that the person with the vanished manager is in it.
    CHECK(tree.unrooted >= 1);
    CHECK(tree_depth(tree, report.id) == -1);
}

TEST_CASE("a_reporting_line_may_not_close_a_cycle", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    auto ctx = ores::testing::make_generation_context(h);
    service::account_operations_service sut(h.context());

    const std::string password = faker::internet::password();
    const auto e1 = generate_synthetic_account(ctx);
    const auto boss = sut.create_account(e1.username, e1.email, password, e1.modified_by);
    const auto e2 = generate_synthetic_account(ctx);
    const auto report = sut.create_account(e2.username, e2.email, password, e2.modified_by);

    // The report answers to the boss.
    CHECK(sut.update_account(report.id,
                             report.email,
                             "",
                             std::nullopt,
                             "",
                             boss.id,
                             boost::uuids::nil_uuid(),
                             report.modified_by,
                             "common.non_material_update",
                             ""));

    // The boss cannot then answer to the report: the pair is a cycle, and the
    // store refuses the write rather than leaving a hierarchy with no root.
    CHECK_THROWS(sut.update_account(boss.id,
                                    boss.email,
                                    "",
                                    std::nullopt,
                                    "",
                                    report.id,
                                    boost::uuids::nil_uuid(),
                                    boss.modified_by,
                                    "common.non_material_update",
                                    ""));

    const auto unchanged = sut.find_account_by_id(boss.id);
    REQUIRE(unchanged.has_value());
    CHECK_FALSE(unchanged->reports_to_account_id.has_value());

    // A person cannot report to themselves either.
    CHECK_THROWS(sut.update_account(report.id,
                                    report.email,
                                    "",
                                    std::nullopt,
                                    "",
                                    report.id,
                                    boost::uuids::nil_uuid(),
                                    report.modified_by,
                                    "common.non_material_update",
                                    ""));
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
    probe.email = "probe@example.com";
    probe.modified_by = h.db_user();
    probe.change_reason_code =
        std::string{ores::dq::domain::change_reason_constants::codes::new_record};
    probe.change_commentary = "suspended tenant login probe";

    repository::account_repository accounts;
    accounts.write(tenant_ctx, std::vector<domain::account>{probe});

    domain::account_credential probe_credential;
    probe_credential.version = 0;
    probe_credential.id = boost::uuids::random_generator()();
    probe_credential.tenant_id = *tid;
    probe_credential.account_id = probe.id;
    probe_credential.password_hash = ores::security::crypto::password_hasher::hash(password);
    probe_credential.modified_by = h.db_user();
    probe_credential.change_reason_code =
        std::string{ores::dq::domain::change_reason_constants::codes::new_record};
    probe_credential.change_commentary = "suspended tenant login probe";

    repository::account_credential_repository credentials;
    credentials.write(tenant_ctx, std::vector<domain::account_credential>{probe_credential});

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
