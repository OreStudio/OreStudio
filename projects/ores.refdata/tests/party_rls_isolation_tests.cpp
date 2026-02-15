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
#include <catch2/catch_test_macros.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <boost/uuid/random_generator.hpp>
#include <boost/lexical_cast.hpp>
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include "ores.logging/make_logger.hpp"
#include "ores.utility/streaming/std_vector.hpp" // IWYU pragma: keep.
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.refdata/domain/party_counterparty.hpp"
#include "ores.refdata/domain/party_counterparty_json_io.hpp" // IWYU pragma: keep.
#include "ores.refdata/repository/party_counterparty_repository.hpp"
#include "ores.refdata/repository/party_repository.hpp"
#include "ores.refdata/repository/counterparty_repository.hpp"
#include "ores.refdata/generators/party_generator.hpp"
#include "ores.refdata/generators/counterparty_generator.hpp"
#include "ores.testing/scoped_database_helper.hpp"
#include "ores.testing/make_generation_context.hpp"

using namespace ores::logging;
using namespace ores::refdata::generators;

using ores::refdata::domain::party_counterparty;
using ores::refdata::repository::party_counterparty_repository;
using ores::refdata::repository::party_repository;
using ores::refdata::repository::counterparty_repository;
using ores::testing::scoped_database_helper;

namespace {

const std::string_view test_suite("ores.refdata.tests");
const std::string tags("[rls][party_isolation]");

boost::uuids::uuid find_system_party_id(
    party_repository& repo, const std::string& tid) {
    auto parties = repo.read_latest();
    for (const auto& p : parties)
        if (p.tenant_id == tid && p.party_category == "System")
            return p.id;
    throw std::runtime_error("No system party for tenant: " + tid);
}

party_counterparty make_pc(scoped_database_helper& h,
    const boost::uuids::uuid& party_id,
    const boost::uuids::uuid& counterparty_id) {
    party_counterparty pc;
    pc.tenant_id = h.tenant_id().to_string();
    pc.party_id = party_id;
    pc.counterparty_id = counterparty_id;
    pc.modified_by = h.db_user();
    pc.change_reason_code = "system.test";
    pc.change_commentary = "RLS isolation test data";
    pc.performed_by = h.db_user();
    return pc;
}

/**
 * @brief Compute visible party IDs via the SQL function.
 */
std::vector<boost::uuids::uuid> compute_visible_parties(
    ores::database::context& ctx,
    const ores::utility::uuid::tenant_id& tid,
    const boost::uuids::uuid& party_id) {
    auto lg(make_logger(test_suite));
    auto rows = ores::database::repository::execute_parameterized_string_query(
        ctx,
        "SELECT unnest(ores_refdata_visible_party_ids_fn("
        "$1::uuid, $2::uuid))::text",
        {tid.to_string(), boost::uuids::to_string(party_id)},
        lg, "Computing visible party set");

    std::vector<boost::uuids::uuid> result;
    for (const auto& s : rows)
        result.push_back(boost::lexical_cast<boost::uuids::uuid>(s));
    return result;
}

}

TEST_CASE("system_party_sees_all_assignments", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    auto gen_ctx = ores::testing::make_generation_context(h);
    auto& ctx = h.context();
    const auto tid = h.tenant_id();

    party_repository party_repo(ctx);
    counterparty_repository cp_repo(ctx);

    // Find system party
    const auto system_party_id = find_system_party_id(
        party_repo, tid.to_string());

    // Create two operational parties under the system party
    auto party_a = generate_synthetic_party(gen_ctx);
    party_a.party_category = "Operational";
    party_a.parent_party_id = system_party_id;
    party_a.change_reason_code = "system.test";
    party_repo.write(party_a);

    auto party_b = generate_synthetic_party(gen_ctx);
    party_b.party_category = "Operational";
    party_b.parent_party_id = system_party_id;
    party_b.change_reason_code = "system.test";
    party_repo.write(party_b);

    // Create counterparties
    auto cp1 = generate_synthetic_counterparty(gen_ctx);
    cp1.change_reason_code = "system.test";
    cp_repo.write(cp1);

    auto cp2 = generate_synthetic_counterparty(gen_ctx);
    cp2.change_reason_code = "system.test";
    cp_repo.write(cp2);

    auto cp3 = generate_synthetic_counterparty(gen_ctx);
    cp3.change_reason_code = "system.test";
    cp_repo.write(cp3);

    // Write assignments: cp1->A, cp2->B, cp3->both
    party_counterparty_repository repo(ctx);
    repo.write(make_pc(h, party_a.id, cp1.id));
    repo.write(make_pc(h, party_b.id, cp2.id));
    repo.write(make_pc(h, party_a.id, cp3.id));
    repo.write(make_pc(h, party_b.id, cp3.id));

    // System party should see all parties via visible party set
    auto visible = compute_visible_parties(ctx, tid, system_party_id);
    BOOST_LOG_SEV(lg, debug) << "System party visible set size: "
                             << visible.size();
    REQUIRE(visible.size() >= 3); // system + A + B at minimum

    // Create a system-party-scoped context
    auto sys_ctx = ctx.with_party(tid, system_party_id, visible);
    party_counterparty_repository sys_repo(sys_ctx);

    auto all = sys_repo.read_latest();
    BOOST_LOG_SEV(lg, debug) << "System party sees " << all.size()
                             << " assignments";
    CHECK(all.size() >= 4); // cp1->A, cp2->B, cp3->A, cp3->B
}

TEST_CASE("party_a_sees_only_own_assignments", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    auto gen_ctx = ores::testing::make_generation_context(h);
    auto& ctx = h.context();
    const auto tid = h.tenant_id();

    party_repository party_repo(ctx);
    counterparty_repository cp_repo(ctx);

    const auto system_party_id = find_system_party_id(
        party_repo, tid.to_string());

    // Create two operational parties
    auto party_a = generate_synthetic_party(gen_ctx);
    party_a.party_category = "Operational";
    party_a.parent_party_id = system_party_id;
    party_a.change_reason_code = "system.test";
    party_repo.write(party_a);

    auto party_b = generate_synthetic_party(gen_ctx);
    party_b.party_category = "Operational";
    party_b.parent_party_id = system_party_id;
    party_b.change_reason_code = "system.test";
    party_repo.write(party_b);

    // Create counterparties
    auto cp1 = generate_synthetic_counterparty(gen_ctx);
    cp1.change_reason_code = "system.test";
    cp_repo.write(cp1);

    auto cp2 = generate_synthetic_counterparty(gen_ctx);
    cp2.change_reason_code = "system.test";
    cp_repo.write(cp2);

    auto cp3 = generate_synthetic_counterparty(gen_ctx);
    cp3.change_reason_code = "system.test";
    cp_repo.write(cp3);

    // Assign: cp1->A, cp2->B, cp3->both
    party_counterparty_repository repo(ctx);
    repo.write(make_pc(h, party_a.id, cp1.id));
    repo.write(make_pc(h, party_b.id, cp2.id));
    repo.write(make_pc(h, party_a.id, cp3.id));
    repo.write(make_pc(h, party_b.id, cp3.id));

    // Party A: visible set is just [party_a]
    auto a_ctx = ctx.with_party(tid, party_a.id, {party_a.id});
    party_counterparty_repository a_repo(a_ctx);

    auto a_results = a_repo.read_latest();
    BOOST_LOG_SEV(lg, debug) << "Party A sees " << a_results.size()
                             << " assignments";

    // Party A should see cp1 and cp3 (both assigned to A)
    CHECK(a_results.size() == 2);
    for (const auto& r : a_results)
        CHECK(r.party_id == party_a.id);
}

TEST_CASE("party_b_sees_only_own_assignments", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    auto gen_ctx = ores::testing::make_generation_context(h);
    auto& ctx = h.context();
    const auto tid = h.tenant_id();

    party_repository party_repo(ctx);
    counterparty_repository cp_repo(ctx);

    const auto system_party_id = find_system_party_id(
        party_repo, tid.to_string());

    auto party_a = generate_synthetic_party(gen_ctx);
    party_a.party_category = "Operational";
    party_a.parent_party_id = system_party_id;
    party_a.change_reason_code = "system.test";
    party_repo.write(party_a);

    auto party_b = generate_synthetic_party(gen_ctx);
    party_b.party_category = "Operational";
    party_b.parent_party_id = system_party_id;
    party_b.change_reason_code = "system.test";
    party_repo.write(party_b);

    auto cp1 = generate_synthetic_counterparty(gen_ctx);
    cp1.change_reason_code = "system.test";
    cp_repo.write(cp1);

    auto cp2 = generate_synthetic_counterparty(gen_ctx);
    cp2.change_reason_code = "system.test";
    cp_repo.write(cp2);

    auto cp3 = generate_synthetic_counterparty(gen_ctx);
    cp3.change_reason_code = "system.test";
    cp_repo.write(cp3);

    party_counterparty_repository repo(ctx);
    repo.write(make_pc(h, party_a.id, cp1.id));
    repo.write(make_pc(h, party_b.id, cp2.id));
    repo.write(make_pc(h, party_a.id, cp3.id));
    repo.write(make_pc(h, party_b.id, cp3.id));

    // Party B: visible set is just [party_b]
    auto b_ctx = ctx.with_party(tid, party_b.id, {party_b.id});
    party_counterparty_repository b_repo(b_ctx);

    auto b_results = b_repo.read_latest();
    BOOST_LOG_SEV(lg, debug) << "Party B sees " << b_results.size()
                             << " assignments";

    // Party B should see cp2 and cp3 (both assigned to B)
    CHECK(b_results.size() == 2);
    for (const auto& r : b_results)
        CHECK(r.party_id == party_b.id);
}

TEST_CASE("nested_party_hierarchy_visibility", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    auto gen_ctx = ores::testing::make_generation_context(h);
    auto& ctx = h.context();
    const auto tid = h.tenant_id();

    party_repository party_repo(ctx);
    counterparty_repository cp_repo(ctx);

    const auto system_party_id = find_system_party_id(
        party_repo, tid.to_string());

    // Create parent → child hierarchy
    auto parent = generate_synthetic_party(gen_ctx);
    parent.party_category = "Operational";
    parent.parent_party_id = system_party_id;
    parent.change_reason_code = "system.test";
    party_repo.write(parent);

    auto child = generate_synthetic_party(gen_ctx);
    child.party_category = "Operational";
    child.parent_party_id = parent.id;
    child.change_reason_code = "system.test";
    party_repo.write(child);

    // Create a counterparty and assign to child
    auto cp = generate_synthetic_counterparty(gen_ctx);
    cp.change_reason_code = "system.test";
    cp_repo.write(cp);

    party_counterparty_repository repo(ctx);
    repo.write(make_pc(h, child.id, cp.id));

    // Parent sees child's assignments (visible = [parent, child])
    auto parent_visible = compute_visible_parties(ctx, tid, parent.id);
    BOOST_LOG_SEV(lg, debug) << "Parent visible set size: "
                             << parent_visible.size();
    REQUIRE(parent_visible.size() >= 2); // parent + child

    auto parent_ctx = ctx.with_party(tid, parent.id, parent_visible);
    party_counterparty_repository parent_repo(parent_ctx);
    auto parent_results = parent_repo.read_latest();
    BOOST_LOG_SEV(lg, debug) << "Parent sees " << parent_results.size()
                             << " assignments";
    CHECK(parent_results.size() >= 1);

    // Child sees only its own assignments (visible = [child])
    auto child_ctx = ctx.with_party(tid, child.id, {child.id});
    party_counterparty_repository child_repo(child_ctx);
    auto child_results = child_repo.read_latest();
    BOOST_LOG_SEV(lg, debug) << "Child sees " << child_results.size()
                             << " assignments";
    CHECK(child_results.size() == 1);
    CHECK(child_results[0].counterparty_id == cp.id);
}
