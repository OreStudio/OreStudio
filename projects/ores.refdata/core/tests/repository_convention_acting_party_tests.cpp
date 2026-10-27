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
#include "ores.database/domain/context.hpp"
#include "ores.refdata.api/domain/bma_basis_swap_convention.hpp"
#include "ores.refdata.api/generators/bma_basis_swap_convention_generator.hpp"
#include "ores.refdata.api/generators/party_generator.hpp"
#include "ores.refdata.core/repository/bma_basis_swap_convention_repository.hpp"
#include "ores.refdata.core/repository/party_repository.hpp"
#include "ores.testing/make_generation_context.hpp"
#include "ores.testing/scoped_database_helper.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <boost/uuid/uuid.hpp>
#include <catch2/catch_test_macros.hpp>
#include <optional>

// A convention's store keys a row by its id and its party, while the
// repository takes the id alone. A parent party's session sees its
// subsidiaries' rows, so one id names several rows; these tests prove that a
// read, a write and a removal from that session act on the acting party's row.

namespace {

using ores::refdata::domain::bma_basis_swap_convention;
using ores::refdata::repository::bma_basis_swap_convention_repository;
using ores::testing::scoped_database_helper;

boost::uuids::uuid write_party(scoped_database_helper& h,
                               ores::utility::generation::generation_context& gen,
                               std::optional<boost::uuids::uuid> parent) {
    ores::refdata::repository::party_repository repo;
    auto party = ores::refdata::generators::generate_synthetic_party(gen);
    party.change_reason_code = "system.test";
    if (parent) {
        party.parent_party_id = *parent;
    } else {
        for (const auto& e : repo.read_latest(h.context()))
            if (e.tenant_id == party.tenant_id) {
                party.parent_party_id = e.id;
                break;
            }
    }
    repo.write(h.context(), party);
    return party.id;
}

struct fixture {
    scoped_database_helper h;
    ores::utility::generation::generation_context gen = ores::testing::make_generation_context(h);
    boost::uuids::uuid parent = write_party(h, gen, std::nullopt);
    boost::uuids::uuid child = write_party(h, gen, parent);
    ores::database::context parent_only =
        h.context().with_party(h.tenant_id(), parent, {parent}, h.db_user());
    ores::database::context child_only =
        h.context().with_party(h.tenant_id(), child, {child}, h.db_user());
    ores::database::context parent_session =
        h.context().with_party(h.tenant_id(), parent, {parent, child}, h.db_user());
    bma_basis_swap_convention_repository repo;
    bma_basis_swap_convention row =
        ores::refdata::generators::generate_synthetic_bma_basis_swap_convention(gen);

    // The child's row is one version ahead of the parent's, so a read that
    // lands on the child's row states a version the parent's row does not hold.
    fixture() {
        row.change_reason_code = "system.test";
        auto child_row = row;
        child_row.party_id = child;
        repo.write(child_only, child_row);
        repo.write(child_only, child_row);
        auto parent_row = row;
        parent_row.party_id = parent;
        repo.write(parent_only, parent_row);
    }

    int version_in(const ores::database::context& ctx) {
        const auto rows = repo.read_latest(ctx, row.id);
        return rows.empty() ? 0 : rows.front().version;
    }
};

}

TEST_CASE("a parent session reads its own row of a key its subsidiary also holds",
          "[repository][acting_party]") {
    fixture f;

    const auto rows = f.repo.read_latest(f.parent_session, f.row.id);

    REQUIRE(rows.size() == 1);
    REQUIRE(rows.front().party_id == f.parent);
    REQUIRE(rows.front().version == 1);
}

TEST_CASE("a parent session finds a subsidiary's row of a key it does not hold",
          "[repository][acting_party]") {
    fixture f;
    REQUIRE(f.repo.remove(f.parent_only, f.row.id, std::nullopt) ==
            bma_basis_swap_convention_repository::remove_status::removed);

    const auto rows = f.repo.read_latest(f.parent_session, f.row.id);

    REQUIRE(rows.size() == 1);
    REQUIRE(rows.front().party_id == f.child);
}

TEST_CASE("an unclaimed write from a parent session replaces only its own row",
          "[repository][acting_party]") {
    fixture f;
    auto v = f.row;
    v.party_id = f.parent;

    f.repo.write(f.parent_session, v);
    f.repo.write(
        f.parent_session, v, {ores::utility::domain::precondition_kind::any, std::nullopt});

    REQUIRE(f.version_in(f.parent_only) == 3);
    REQUIRE(f.version_in(f.child_only) == 2);
}

TEST_CASE("a removal from a parent session closes only its own row", "[repository][acting_party]") {
    fixture f;

    const auto status = f.repo.remove(f.parent_session, f.row.id, std::nullopt);

    REQUIRE(status == bma_basis_swap_convention_repository::remove_status::removed);
    REQUIRE(f.repo.read_latest(f.parent_only, f.row.id).empty());
    REQUIRE(f.version_in(f.child_only) == 2);
}

TEST_CASE("a removal that names the parent's version closes the parent's row",
          "[repository][acting_party]") {
    fixture f;

    const auto status = f.repo.remove(f.parent_session, f.row.id, 1u);

    REQUIRE(status == bma_basis_swap_convention_repository::remove_status::removed);
    REQUIRE(f.repo.read_latest(f.parent_only, f.row.id).empty());
    REQUIRE(f.version_in(f.child_only) == 2);
}
