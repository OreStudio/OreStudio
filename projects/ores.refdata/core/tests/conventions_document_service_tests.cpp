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
#include "ores.refdata.api/generators/deposit_convention_generator.hpp"
#include "ores.refdata.api/generators/ibor_index_convention_generator.hpp"
#include "ores.refdata.api/generators/party_generator.hpp"
#include "ores.refdata.api/messaging/configuration_document_protocol.hpp"
#include "ores.refdata.core/repository/party_repository.hpp"
#include "ores.refdata.core/service/conventions_document_service.hpp"
#include "ores.refdata.core/service/tenor_basis_swap_convention_service.hpp"
#include "ores.testing/make_generation_context.hpp"
#include "ores.testing/scoped_database_helper.hpp"
#include <boost/uuid/random_generator.hpp>
#include <algorithm>
#include <catch2/catch_test_macros.hpp>
#include <string>
#include <vector>

namespace {

const std::string tags("[refdata][conventions][database]");

// A document belongs to a party, so each case writes one and acts for it.
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

}

using ores::refdata::service::conventions_document_service;

TEST_CASE("saving a party's convention again replaces it", tags) {
    ores::testing::scoped_database_helper h;
    const auto ctx = party_context(h);
    auto gen = ores::testing::make_generation_context(h);
    ores::refdata::messaging::conventions_document doc;
    doc.deposit.push_back(ores::refdata::generators::generate_synthetic_deposit_convention(gen));
    doc.deposit[0].change_reason_code = "system.test";

    conventions_document_service conventions(ctx);
    conventions.save(doc);
    CHECK_NOTHROW(conventions.save(doc));

    const auto held = conventions.get().deposit;
    CHECK(std::ranges::count_if(held, [&](const auto& c) { return c.id == doc.deposit[0].id; }) ==
          1);
}

TEST_CASE("a world convention the tenant holds is left as it is", tags) {
    ores::testing::scoped_database_helper h;
    const auto ctx = party_context(h);
    auto gen = ores::testing::make_generation_context(h);
    auto index = ores::refdata::generators::generate_synthetic_ibor_index_convention(gen);
    index.change_reason_code = "system.test";
    ores::refdata::messaging::conventions_document doc;
    doc.ibor_index.push_back(index);

    conventions_document_service conventions(ctx);
    CHECK(conventions.save(doc).world_kept.empty());

    const auto first = conventions.get().ibor_index;
    const auto version =
        std::ranges::find_if(first, [&](const auto& c) { return c.id == index.id; })->version;

    const auto second = conventions.save(doc);
    CHECK(second.world_kept == std::vector<std::string>{index.id});
    const auto after = conventions.get().ibor_index;
    CHECK(std::ranges::find_if(after, [&](const auto& c) { return c.id == index.id; })->version ==
          version);
}

TEST_CASE("an import does not duplicate a convention the DQ path published", tags) {
    ores::testing::scoped_database_helper h;
    const auto ctx = party_context(h);
    boost::uuids::random_generator uuid;

    const std::string name = "GBP-LIBOR-3M-6M-BASIS-CONVENTIONS";
    const auto published_id = uuid();

    // The DQ path publishes the convention first, under a key it chose.
    ores::refdata::service::tenor_basis_swap_convention_service family(ctx);
    ores::refdata::messaging::put_tenor_basis_swap_convention_request put;
    put.item.change.write.id = published_id;
    put.item.change.write.kind_code = "TENOR_BASIS_SWAP";
    put.item.change.write.name = name;
    ores::refdata::messaging::tenor_basis_swap_pay_receive_change part;
    part.write.id = uuid();
    part.write.convention_id = published_id;
    part.write.pay_index = std::string("GBP-LIBOR-6M");
    part.write.receive_index = std::string("GBP-LIBOR-3M");
    put.item.pay_receive.push_back(part);
    put.intent.reason_code = "system.test";
    REQUIRE(family.put_tenor_basis_swap_convention(put).result.outcome ==
            ores::utility::domain::outcome::ok);

    // A later import of a document naming the same party, kind and name resolves
    // to the published row rather than adding a second convention under the
    // provisional key the mapper minted.
    ores::refdata::messaging::conventions_document doc;
    ores::refdata::domain::convention central;
    central.id = uuid();
    central.kind_code = "TENOR_BASIS_SWAP";
    central.name = name;
    central.change_reason_code = "system.test";
    doc.convention.push_back(central);
    ores::refdata::domain::tenor_basis_swap_pay_receive imported;
    imported.id = uuid();
    imported.convention_id = central.id;
    imported.pay_index = std::string("GBP-LIBOR-6M");
    imported.receive_index = std::string("GBP-LIBOR-3M");
    imported.change_reason_code = "system.test";
    doc.tenor_basis_swap_pay_receive.push_back(imported);

    conventions_document_service conventions(ctx);
    conventions.save(doc);

    const auto held = conventions.get();
    REQUIRE(held.convention.size() == 1);
    CHECK(held.convention.front().id == published_id);
    CHECK(held.tenor_basis_swap_pay_receive.size() == 1);
}

TEST_CASE("an import that changes leg form closes the other form", tags) {
    ores::testing::scoped_database_helper h;
    const auto ctx = party_context(h);
    boost::uuids::random_generator uuid;

    // The first import carries the paying and receiving form.
    ores::refdata::messaging::conventions_document first;
    ores::refdata::domain::convention central;
    central.id = uuid();
    central.kind_code = "TENOR_BASIS_SWAP";
    central.name = "EUR-3M-6M-BASIS";
    central.change_reason_code = "system.test";
    first.convention.push_back(central);
    ores::refdata::domain::tenor_basis_swap_pay_receive pay_receive;
    pay_receive.id = uuid();
    pay_receive.convention_id = central.id;
    pay_receive.pay_index = std::string("EUR-EURIBOR-6M");
    pay_receive.receive_index = std::string("EUR-EURIBOR-3M");
    pay_receive.change_reason_code = "system.test";
    first.tenor_basis_swap_pay_receive.push_back(pay_receive);

    conventions_document_service conventions(ctx);
    conventions.save(first);

    const auto held_first = conventions.get();
    REQUIRE(held_first.convention.size() == 1);
    REQUIRE(held_first.tenor_basis_swap_pay_receive.size() == 1);
    CHECK(held_first.tenor_basis_swap_long_short.empty());

    // The second import names the same party, kind and name in the other form,
    // so it resolves to the same central row and must close the first form.
    ores::refdata::messaging::conventions_document second;
    ores::refdata::domain::convention changed;
    changed.id = uuid();
    changed.kind_code = "TENOR_BASIS_SWAP";
    changed.name = central.name;
    changed.change_reason_code = "system.test";
    second.convention.push_back(changed);
    ores::refdata::domain::tenor_basis_swap_long_short long_short;
    long_short.id = uuid();
    long_short.convention_id = changed.id;
    long_short.long_index = std::string("EUR-EURIBOR-6M");
    long_short.short_index = std::string("EUR-EURIBOR-3M");
    long_short.change_reason_code = "system.test";
    second.tenor_basis_swap_long_short.push_back(long_short);

    conventions.save(second);

    const auto held_second = conventions.get();
    REQUIRE(held_second.convention.size() == 1);
    CHECK(held_second.convention.front().id == held_first.convention.front().id);
    CHECK(held_second.tenor_basis_swap_pay_receive.empty());
    REQUIRE(held_second.tenor_basis_swap_long_short.size() == 1);
    CHECK(held_second.tenor_basis_swap_long_short.front().long_index ==
          std::string("EUR-EURIBOR-6M"));
}
