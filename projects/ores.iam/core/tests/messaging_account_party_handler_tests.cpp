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
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
 * details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */
#include "ores.dq.api/domain/change_reason_constants.hpp"
#include "ores.iam.api/domain/account_party.hpp"
#include "ores.iam.api/domain/account_party_json_io.hpp" // IWYU pragma: keep.
#include "ores.iam.core/messaging/account_party_handler.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.refdata.api/generators/party_generator.hpp"
#include "ores.refdata.core/repository/party_repository.hpp"
#include "ores.testing/database_helper.hpp"
#include "ores.testing/make_generation_context.hpp"
#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/uuid_generators.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <catch2/catch_test_macros.hpp>

using namespace ores::logging;

using ores::iam::domain::account_party;
using ores::testing::database_helper;
using ores::refdata::repository::party_repository;

namespace {

const std::string_view test_suite("ores.iam.tests.account_party_handler");
const std::string tags("[messaging]");

boost::uuids::uuid find_system_party_id(party_repository& repo,
                                        const ores::utility::uuid::tenant_id& tid) {
    auto parties = repo.read_latest();
    for (const auto& p : parties)
        if (p.tenant_id == tid && p.party_category == "System")
            return p.id;
    throw std::runtime_error("No system party for tenant: " + tid.to_string());
}

}

/**
 * Regression test for the bug where the generic
 * ores::service::messaging::stamp() helper clobbered account_party's
 * party_id (a client-supplied association target) with the caller's own
 * current party from the context, instead of leaving it as requested.
 * stamp_account_party() must preserve it.
 */
TEST_CASE("stamp_account_party_preserves_target_party_id", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;
    auto ctx = ores::testing::make_generation_context(h);

    party_repository party_repo(h.context());
    const auto system_party_id = find_system_party_id(party_repo, h.tenant_id());

    // The association target: a party distinct from whatever party the
    // caller's own context is currently scoped to.
    auto target_party = ores::refdata::generators::generate_synthetic_party(ctx);
    target_party.parent_party_id = system_party_id;
    target_party.change_reason_code =
        std::string(ores::dq::domain::change_reason_constants::codes::new_record);
    party_repo.write(target_party);

    // Simulate the caller being logged in under a different party than the
    // one they're targeting with this association — the exact scenario
    // that exposed the original bug.
    h.set_party(system_party_id);

    account_party ap;
    ap.account_id = boost::uuids::random_generator()();
    ap.party_id = target_party.id;
    ap.tenant_id.clear();
    ap.modified_by.clear();
    ap.performed_by.clear();
    ap.change_reason_code.clear();

    ores::iam::messaging::stamp_account_party(ap, h.context());

    BOOST_LOG_SEV(lg, debug) << "Stamped account_party: " << ap;

    CHECK(ap.party_id == target_party.id);
    CHECK(!ap.tenant_id.empty());
    CHECK(!ap.change_reason_code.empty());
}
