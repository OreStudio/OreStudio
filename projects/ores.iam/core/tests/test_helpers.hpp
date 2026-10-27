/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2026 Marco Craveiro <marco.craveiro@gmail.com>
 *
 * This program is free software; you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation; either version 3 of the License, or (at your option) any later
 * version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */
#ifndef ORES_IAM_CORE_TESTS_TEST_HELPERS_HPP_
#define ORES_IAM_CORE_TESTS_TEST_HELPERS_HPP_

#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.refdata.core/repository/party_repository.hpp"
#include "ores.testing/scoped_database_helper.hpp"
#include "test_suite.hpp"
#include <boost/uuid/uuid_generators.hpp>

using ores::database::context;
using ores::refdata::repository::party_repository;

// The database stamps every row with the account that wrote it, as a signed-in
// context always names one.
inline ores::database::context acting(ores::testing::scoped_database_helper& h) {
    return h.context().with_tenant(h.tenant_id(), h.db_user());
}

inline boost::uuids::uuid find_system_party_id(party_repository& repo,
                                               ores::database::context ctx,
                                               const ores::utility::uuid::tenant_id& tid) {
    auto parties = repo.read_latest(ctx);
    for (const auto& p : parties)
        if (p.tenant_id == tid && p.party_category == "System")
            return p.id;
    throw std::runtime_error("No system party for tenant: " + tid.to_string());
}

inline const std::string granted_code("iam::accounts:read");

inline boost::uuids::uuid id_of(const std::string& s) {
    return boost::uuids::string_generator()(s);
}

inline std::string resource_name() {
    return "reporting.report_definition/" +
           boost::uuids::to_string(boost::uuids::random_generator()());
}

inline auto& test_helpers_lg() {
    static auto instance = ores::logging::make_logger(test_suite);
    return instance;
}

inline void run(ores::testing::scoped_database_helper& h,
                const std::string& sql,
                const std::vector<std::string>& params) {
    ores::database::repository::execute_parameterized_command(
        acting(h), sql, params, test_helpers_lg(), sql);
}

inline context system_context(ores::testing::database_helper& h) {
    return h.context().with_tenant(ores::utility::uuid::tenant_id::system(), "");
}

inline const std::string test_secret("super-secret-key-for-testing-purposes-only-32bytes!");

#endif
