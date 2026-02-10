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
#include "ores.iam/repository/login_info_repository.hpp"

#include <catch2/catch_test_macros.hpp>
#include <faker-cxx/faker.h>
#include <boost/uuid/uuid_io.hpp>
#include <boost/uuid/uuid_generators.hpp>
#include <boost/asio/ip/address.hpp>
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include "ores.logging/make_logger.hpp"
#include "ores.utility/streaming/std_vector.hpp" // IWYU pragma: keep.
#include "ores.iam/domain/login_info.hpp"
#include "ores.iam/domain/login_info_json_io.hpp" // IWYU pragma: keep.
#include "ores.testing/database_helper.hpp"

namespace {

const std::string_view test_suite("ores.iam.tests");
const std::string tags("[repository]");

using ores::iam::domain::login_info;

login_info make_login_info(ores::testing::database_helper& h) {
    login_info li;
    li.tenant_id = h.tenant_id();
    li.last_login = std::chrono::system_clock::now();
    li.account_id = boost::uuids::random_generator()();
    li.failed_logins = 0;
    li.locked = false;
    li.online = false;
    li.password_reset_required = false;
    li.last_ip = boost::asio::ip::make_address("127.0.0.1");
    li.last_attempt_ip = boost::asio::ip::make_address("127.0.0.1");
    return li;
}

}

using namespace ores::logging;

using ores::testing::database_helper;
using ores::iam::repository::login_info_repository;

TEST_CASE("write_login_infos", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;

    login_info_repository repo(h.context());

    std::vector<login_info> lis;
    for (int i = 0; i < 3; ++i)
        lis.push_back(make_login_info(h));

    BOOST_LOG_SEV(lg, debug) << "Login infos: " << lis;
    CHECK_NOTHROW(repo.write(lis));
}

TEST_CASE("read_login_infos", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;

    login_info_repository repo(h.context());

    auto li = make_login_info(h);
    const auto target_account_id = li.account_id;
    repo.write({li});

    BOOST_LOG_SEV(lg, debug) << "Target account ID: " << target_account_id;

    auto read_lis = repo.read(target_account_id);
    BOOST_LOG_SEV(lg, debug) << "Read login infos: " << read_lis;

    REQUIRE(read_lis.size() == 1);
    CHECK(read_lis[0].account_id == target_account_id);
}

TEST_CASE("read_nonexistent_login_info", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;

    login_info_repository repo(h.context());

    const auto nonexistent_id = boost::uuids::random_generator()();
    BOOST_LOG_SEV(lg, debug) << "Non-existent account ID: " << nonexistent_id;

    auto read_lis = repo.read(nonexistent_id);
    BOOST_LOG_SEV(lg, debug) << "Read login infos: " << read_lis;

    CHECK(read_lis.size() == 0);
}
