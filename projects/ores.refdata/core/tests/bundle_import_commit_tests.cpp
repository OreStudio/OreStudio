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
 * this program; if not, write to the Free Software Foundation, Inc., 51 Franklin
 * Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */
#include "ores.database/domain/context.hpp"
#include "ores.refdata.api/domain/convention.hpp"
#include "ores.refdata.api/domain/tenor_basis_swap_pay_receive.hpp"
#include "ores.refdata.api/generators/party_generator.hpp"
#include "ores.refdata.api/messaging/bundle_import_operations_protocol.hpp"
#include "ores.refdata.core/repository/bundle_import_repository.hpp"
#include "ores.refdata.core/repository/convention_repository.hpp"
#include "ores.refdata.core/repository/party_repository.hpp"
#include "ores.refdata.core/repository/tenor_basis_swap_pay_receive_repository.hpp"
#include "ores.refdata.core/service/bundle_import_operations_service.hpp"
#include "ores.testing/make_generation_context.hpp"
#include "ores.testing/scoped_database_helper.hpp"
#include "ores.utility/rfl/reflectors.hpp"
#include <boost/uuid/random_generator.hpp>
#include <catch2/catch_test_macros.hpp>
#include <rfl/json.hpp>
#include <string>

namespace {

// An import belongs to a party, so each case writes one and acts for it.
ores::database::context party_context(ores::testing::scoped_database_helper& h) {
    auto gen = ores::testing::make_generation_context(h);
    ores::refdata::repository::party_repository repo;
    auto party = ores::refdata::generators::generate_synthetic_party(gen);
    party.change_reason_code = "system.test";
    for (const auto& e : repo.read_latest(h.context()))
        if (e.tenant_id == party.tenant_id) {
            party.parent_party_id = e.id;
            break;
        }
    repo.write(h.context(), party);
    return h.context().with_party(h.tenant_id(), party.id, {party.id}, h.db_user());
}

boost::uuids::uuid begin_import(ores::database::context ctx, boost::uuids::uuid id) {
    ores::refdata::service::bundle_import_operations_service imports(ctx);
    const auto begun = imports.begin({.id = id});
    REQUIRE(begun.success);
    return id;
}

void stage(ores::database::context ctx,
           const boost::uuids::uuid& import_id,
           const std::string& family,
           int sequence,
           const std::string& payload) {
    ores::refdata::service::bundle_import_operations_service imports(ctx);
    const auto staged = imports.stage(
        {.import_id = import_id, .family = family, .sequence = sequence, .payload = payload});
    REQUIRE(staged.success);
}

}

TEST_CASE("a part staged before its central row still commits into one row each",
          "[bundle_import][database]") {
    ores::testing::scoped_database_helper h;
    const auto ctx = party_context(h);
    boost::uuids::random_generator uuid;

    ores::refdata::domain::convention central;
    central.id = uuid();
    central.kind_code = "TENOR_BASIS_SWAP";
    central.name = "GBP-LIBOR-3M-6M-BASIS-CONVENTIONS";
    central.change_reason_code = "system.test";

    ores::refdata::domain::tenor_basis_swap_pay_receive part;
    part.convention_id = central.id;
    part.pay_index = std::string("GBP-LIBOR-6M");
    part.receive_index = std::string("GBP-LIBOR-3M");
    part.change_reason_code = "system.test";

    // The part is staged first on purpose. The apply writes the central family
    // before the families that name it, so stage order must not decide this.
    const auto import_id = begin_import(ctx, uuid());
    stage(ctx, import_id, "tenor_basis_swap_pay_receive", 0, rfl::json::write(part));
    stage(ctx, import_id, "convention", 1, rfl::json::write(central));

    ores::refdata::service::bundle_import_operations_service imports(ctx);
    const auto committed = imports.commit({.import_id = import_id});
    REQUIRE(committed.success);
    REQUIRE(committed.families.size() == 2);

    ores::refdata::repository::convention_repository conventions;
    const auto held = conventions.read_latest(ctx);
    REQUIRE(held.size() == 1);

    ores::refdata::repository::tenor_basis_swap_pay_receive_repository parts;
    const auto held_parts = parts.read_latest(ctx);
    REQUIRE(held_parts.size() == 1);
    CHECK(held_parts.front().convention_id == held.front().id);
}

TEST_CASE("a commit that cannot decode a family leaves the import staging",
          "[bundle_import][database]") {
    ores::testing::scoped_database_helper h;
    const auto ctx = party_context(h);
    boost::uuids::random_generator uuid;

    ores::refdata::domain::convention central;
    central.id = uuid();
    central.kind_code = "TENOR_BASIS_SWAP";
    central.name = "EUR-3M-6M-BASIS";
    central.change_reason_code = "system.test";

    // An array cannot decode into a deposit convention, so the commit reaches a
    // family it cannot apply and rolls the whole import back.
    const auto import_id = begin_import(ctx, uuid());
    stage(ctx, import_id, "convention", 0, rfl::json::write(central));
    stage(ctx, import_id, "deposit", 1, "[]");

    ores::refdata::service::bundle_import_operations_service imports(ctx);
    CHECK_FALSE(imports.commit({.import_id = import_id}).success);

    ores::refdata::repository::convention_repository conventions;
    CHECK(conventions.read_latest(ctx).empty());

    ores::refdata::repository::bundle_import_repository imports_repo;
    const auto import_row = imports_repo.read_latest(ctx, boost::uuids::to_string(import_id));
    REQUIRE(import_row.size() == 1);
    CHECK(import_row.front().state == "staging");
}

TEST_CASE("a second commit of the same import applies nothing", "[bundle_import][database]") {
    ores::testing::scoped_database_helper h;
    const auto ctx = party_context(h);
    boost::uuids::random_generator uuid;

    ores::refdata::domain::convention central;
    central.id = uuid();
    central.kind_code = "TENOR_BASIS_SWAP";
    central.name = "USD-3M-6M-BASIS";
    central.change_reason_code = "system.test";

    const auto import_id = begin_import(ctx, uuid());
    stage(ctx, import_id, "convention", 0, rfl::json::write(central));

    ores::refdata::service::bundle_import_operations_service imports(ctx);
    REQUIRE(imports.commit({.import_id = import_id}).success);

    ores::refdata::repository::convention_repository conventions;
    const auto after_first = conventions.read_latest(ctx);
    REQUIRE(after_first.size() == 1);

    CHECK_FALSE(imports.commit({.import_id = import_id}).success);

    const auto after_second = conventions.read_latest(ctx);
    REQUIRE(after_second.size() == 1);
    CHECK(after_second.front().version == after_first.front().version);
}
