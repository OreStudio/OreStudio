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
#include "ores.logging/make_logger.hpp"
#include "ores.refdata.api/domain/party_counterparty.hpp"
#include "ores.refdata.api/domain/party_counterparty_json_io.hpp" // IWYU pragma: keep.
#include "ores.refdata.api/generators/counterparty_generator.hpp"
#include "ores.refdata.api/generators/party_generator.hpp"
#include "ores.refdata.core/repository/counterparty_repository.hpp"
#include "ores.refdata.core/repository/party_counterparty_repository.hpp"
#include "ores.refdata.core/repository/party_repository.hpp"
#include "ores.testing/make_generation_context.hpp"
#include "ores.testing/scoped_database_helper.hpp"
#include "ores.utility/rfl/reflectors.hpp"       // IWYU pragma: keep.
#include "ores.utility/streaming/std_vector.hpp" // IWYU pragma: keep.
#include "ores.utility/uuid/tenant_id.hpp"
#include <algorithm>
#include <boost/uuid/random_generator.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <catch2/catch_test_macros.hpp>

using namespace ores::logging;
using namespace ores::refdata::generators;

using ores::refdata::domain::party_counterparty;
using ores::refdata::repository::party_counterparty_repository;
using ores::refdata::repository::party_repository;
using ores::refdata::repository::counterparty_repository;
using ores::testing::scoped_database_helper;

namespace {

const std::string_view test_suite("ores.refdata.tests");
const std::string tags("[repository][party_counterparty]");

boost::uuids::uuid find_system_party_id(party_repository& repo,
                                        ores::database::context ctx,
                                        const ores::utility::uuid::tenant_id& tid) {
    auto parties = repo.read_latest(ctx);
    for (const auto& p : parties)
        if (p.tenant_id == tid && p.party_category == "System")
            return p.id;
    throw std::runtime_error("No system party for tenant: " + tid.to_string());
}

party_counterparty make_party_counterparty(scoped_database_helper& h,
                                           const boost::uuids::uuid& party_id,
                                           const boost::uuids::uuid& counterparty_id) {
    party_counterparty pc;
    pc.tenant_id = h.tenant_id().to_string();
    pc.party_id = party_id;
    pc.counterparty_id = counterparty_id;
    pc.modified_by = h.db_user();
    pc.change_reason_code = "system.test";
    pc.change_commentary = "Synthetic test data";
    pc.performed_by = h.db_user();
    return pc;
}

}

TEST_CASE("write_single_party_counterparty", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    auto ctx = ores::testing::make_generation_context(h);

    party_repository party_repo;
    counterparty_repository cp_repo;
    party_counterparty_repository repo(h.context());

    const auto party_id = find_system_party_id(party_repo, h.context(), h.tenant_id());

    auto cp = generate_synthetic_counterparty(ctx);
    cp.change_reason_code = "system.test";
    cp_repo.write(h.context(), cp);

    auto pc = make_party_counterparty(h, party_id, cp.id);
    BOOST_LOG_SEV(lg, debug) << "Party counterparty: " << pc;
    repo.write(pc);

    const auto read_pcs = repo.read_latest(pc.party_id, pc.counterparty_id);
    REQUIRE(read_pcs.size() == 1);
    CHECK(read_pcs[0].change_commentary == pc.change_commentary);
}

TEST_CASE("write_multiple_party_counterparties", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    auto ctx = ores::testing::make_generation_context(h);

    party_repository party_repo;
    counterparty_repository cp_repo;
    party_counterparty_repository repo(h.context());

    const auto system_party_id = find_system_party_id(party_repo, h.context(), h.tenant_id());

    std::vector<party_counterparty> pcs;
    for (int i = 0; i < 3; ++i) {
        auto cp = generate_synthetic_counterparty(ctx);
        cp.change_reason_code = "system.test";
        cp_repo.write(h.context(), cp);

        pcs.push_back(make_party_counterparty(h, system_party_id, cp.id));
    }

    BOOST_LOG_SEV(lg, debug) << "Party counterparties: " << pcs;
    repo.write(pcs);

    const auto read_pcs = repo.read_latest();
    for (const auto& written : pcs) {
        const auto it = std::ranges::find_if(read_pcs, [&](const party_counterparty& r) {
            return r.party_id == written.party_id &&
                   r.counterparty_id == written.counterparty_id;
        });
        REQUIRE(it != read_pcs.end());
        CHECK(it->change_commentary == written.change_commentary);
    }
}

TEST_CASE("read_latest_party_counterparties_by_party", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    auto ctx = ores::testing::make_generation_context(h);

    party_repository party_repo;
    counterparty_repository cp_repo;
    party_counterparty_repository repo(h.context());

    const auto system_party_id = find_system_party_id(party_repo, h.context(), h.tenant_id());

    auto cp = generate_synthetic_counterparty(ctx);
    cp.change_reason_code = "system.test";
    cp_repo.write(h.context(), cp);

    auto pc = make_party_counterparty(h, system_party_id, cp.id);
    repo.write(pc);

    auto read_pcs = repo.read_latest_by_party(system_party_id);
    BOOST_LOG_SEV(lg, debug) << "Read party counterparties: " << read_pcs;

    REQUIRE(!read_pcs.empty());
    bool found = false;
    for (const auto& r : read_pcs) {
        if (r.counterparty_id == cp.id) {
            found = true;
            break;
        }
    }
    CHECK(found);
}

TEST_CASE("read_latest_party_counterparties_by_counterparty", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    auto ctx = ores::testing::make_generation_context(h);

    party_repository party_repo;
    counterparty_repository cp_repo;
    party_counterparty_repository repo(h.context());

    const auto system_party_id = find_system_party_id(party_repo, h.context(), h.tenant_id());

    auto cp = generate_synthetic_counterparty(ctx);
    cp.change_reason_code = "system.test";
    cp_repo.write(h.context(), cp);

    auto pc = make_party_counterparty(h, system_party_id, cp.id);
    repo.write(pc);

    auto read_pcs = repo.read_latest_by_counterparty(cp.id);
    BOOST_LOG_SEV(lg, debug) << "Read party counterparties: " << read_pcs;

    REQUIRE(read_pcs.size() >= 1);
    CHECK(read_pcs[0].party_id == system_party_id);
}

TEST_CASE("remove_party_counterparty", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    auto ctx = ores::testing::make_generation_context(h);

    party_repository party_repo;
    counterparty_repository cp_repo;
    party_counterparty_repository repo(h.context());

    const auto system_party_id = find_system_party_id(party_repo, h.context(), h.tenant_id());

    auto cp = generate_synthetic_counterparty(ctx);
    cp.change_reason_code = "system.test";
    cp_repo.write(h.context(), cp);

    auto keeper_cp = generate_synthetic_counterparty(ctx);
    keeper_cp.change_reason_code = "system.test";
    cp_repo.write(h.context(), keeper_cp);

    auto pc = make_party_counterparty(h, system_party_id, cp.id);
    auto keeper = make_party_counterparty(h, system_party_id, keeper_cp.id);
    repo.write(pc);
    repo.write(keeper);

    repo.remove(system_party_id, cp.id);

    auto read_pcs = repo.read_latest_by_counterparty(cp.id);
    CHECK(read_pcs.empty());

    // The removal took only the row it named.
    const auto keeper_rows = repo.read_latest_by_counterparty(keeper.counterparty_id);
    REQUIRE(keeper_rows.size() == 1);
    CHECK(keeper_rows[0].change_commentary == keeper.change_commentary);
}

TEST_CASE("read_nonexistent_party_counterparty", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    auto ctx = ores::testing::make_generation_context(h);

    party_repository party_repo;
    counterparty_repository cp_repo;
    party_counterparty_repository repo(h.context());

    const auto system_party_id = find_system_party_id(party_repo, h.context(), h.tenant_id());

    auto cp = generate_synthetic_counterparty(ctx);
    cp.change_reason_code = "system.test";
    cp_repo.write(h.context(), cp);

    // Write a row the read can answer with, so that an empty answer is the key
    // at work rather than a read that does nothing.
    const auto keeper = make_party_counterparty(h, system_party_id, cp.id);
    repo.write(keeper);

    const auto nonexistent_id = boost::uuids::random_generator()();
    auto read_pcs = repo.read_latest_by_party(nonexistent_id);

    const auto answered_with_another_key = std::ranges::any_of(
        read_pcs, [&](const party_counterparty& r) { return r.party_id == nonexistent_id; });
    CHECK_FALSE(answered_with_another_key);

    // The same read does answer for the party that was written.
    const auto keeper_rows = repo.read_latest_by_party(keeper.party_id);
    const auto keeper_it = std::ranges::find_if(keeper_rows, [&](const party_counterparty& r) {
        return r.counterparty_id == keeper.counterparty_id;
    });
    REQUIRE(keeper_it != keeper_rows.end());
    CHECK(keeper_it->change_commentary == keeper.change_commentary);
}
