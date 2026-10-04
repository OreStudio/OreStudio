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
#include "ores.refdata.api/generators/deposit_convention_generator.hpp"
#include "ores.refdata.api/generators/ibor_index_convention_generator.hpp"
#include "ores.refdata.api/generators/party_generator.hpp"
#include "ores.refdata.core/repository/deposit_convention_repository.hpp"
#include "ores.refdata.core/repository/ibor_index_convention_repository.hpp"
#include "ores.refdata.core/repository/party_repository.hpp"
#include "ores.refdata.core/service/conventions_document_service.hpp"
#include "ores.testing/make_generation_context.hpp"
#include "ores.testing/scoped_database_helper.hpp"
#include <algorithm>
#include <catch2/catch_test_macros.hpp>
#include <string>

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
    ores::refdata::domain::conventions_document doc;
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
    ores::refdata::domain::conventions_document doc;
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
