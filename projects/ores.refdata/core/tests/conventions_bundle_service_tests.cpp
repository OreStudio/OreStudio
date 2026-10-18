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
#include "ores.refdata.api/domain/tenor_basis_swap_long_short.hpp"
#include "ores.refdata.api/domain/tenor_basis_swap_pay_receive.hpp"
#include "ores.refdata.api/generators/party_generator.hpp"
#include "ores.refdata.core/repository/convention_repository.hpp"
#include "ores.refdata.core/repository/party_repository.hpp"
#include "ores.refdata.core/repository/tenor_basis_swap_long_short_repository.hpp"
#include "ores.refdata.core/repository/tenor_basis_swap_pay_receive_repository.hpp"
#include "ores.refdata.core/service/conventions_bundle.hpp"
#include "ores.testing/make_generation_context.hpp"
#include "ores.testing/scoped_database_helper.hpp"
#include "ores.utility/rfl/reflectors.hpp"
#include <boost/uuid/random_generator.hpp>
#include <catch2/catch_test_macros.hpp>
#include <rfl/json.hpp>
#include <stdexcept>
#include <string>

namespace {

const std::string tags("[refdata][conventions][bundle][database]");

// A bundle belongs to a party, so each case writes one and acts for it.
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

TEST_CASE("a bundle resolves the identity the party already holds", tags) {
    ores::testing::scoped_database_helper h;
    const auto ctx = party_context(h);
    boost::uuids::random_generator uuid;

    // The first bundle carries a convention whose key the mapper minted. The
    // service resolves the identity and gives the row the key it will keep.
    const std::string name = "GBP-LIBOR-3M-6M-BASIS-CONVENTIONS";

    ores::refdata::domain::convention central;
    central.id = uuid();
    central.kind_code = "TENOR_BASIS_SWAP";
    central.name = name;
    central.change_reason_code = "system.test";

    ores::refdata::domain::tenor_basis_swap_pay_receive part;
    part.convention_id = central.id;
    part.pay_index = std::string("GBP-LIBOR-6M");
    part.receive_index = std::string("GBP-LIBOR-3M");
    part.change_reason_code = "system.test";

    ores::refdata::service::conventions_bundle first;
    first.rows.push_back(
        {.family = "convention", .sequence = 0, .payload = rfl::json::write(central)});
    first.rows.push_back({.family = "tenor_basis_swap_pay_receive",
                          .sequence = 0,
                          .payload = rfl::json::write(part)});
    ores::refdata::service::conventions_bundle_service(ctx).apply(first);

    ores::refdata::repository::convention_repository conventions;
    const auto after_first = conventions.read_latest(ctx);
    REQUIRE(after_first.size() == 1);
    const auto resolved_id = after_first.front().id;
    const auto resolved_part = [&] {
        ores::refdata::repository::tenor_basis_swap_pay_receive_repository parts;
        const auto held = parts.read_latest(ctx);
        REQUIRE(held.size() == 1);
        return held.front();
    }();

    // A second bundle names the same party, kind and name under a key of its
    // own. The identity resolves to the row the party holds, so the bundle
    // replaces it rather than adding a second convention or a second part.
    auto second_central = central;
    second_central.id = uuid();
    auto second_part = part;
    second_part.id = uuid();
    second_part.convention_id = second_central.id;

    ores::refdata::service::conventions_bundle second;
    second.rows.push_back(
        {.family = "convention", .sequence = 0, .payload = rfl::json::write(second_central)});
    second.rows.push_back({.family = "tenor_basis_swap_pay_receive",
                           .sequence = 0,
                           .payload = rfl::json::write(second_part)});
    ores::refdata::service::conventions_bundle_service(ctx).apply(second);

    const auto held = conventions.read_latest(ctx);
    REQUIRE(held.size() == 1);
    CHECK(held.front().id == resolved_id);
    // A second bundle that resolved to the first row replaced it, rather than
    // adding another version of a row it minted itself.
    CHECK(held.front().version > after_first.front().version);

    ores::refdata::repository::tenor_basis_swap_pay_receive_repository parts;
    const auto held_parts = parts.read_latest(ctx);
    REQUIRE(held_parts.size() == 1);
    CHECK(held_parts.front().id == resolved_part.id);
    CHECK(held_parts.front().convention_id == resolved_id);
    CHECK(held_parts.front().version > resolved_part.version);
}

TEST_CASE("a bundle that changes leg form closes the other form", tags) {
    ores::testing::scoped_database_helper h;
    const auto ctx = party_context(h);
    boost::uuids::random_generator uuid;

    const std::string name = "EUR-3M-6M-BASIS";
    ores::refdata::domain::convention central;
    central.kind_code = "TENOR_BASIS_SWAP";
    central.name = name;
    central.change_reason_code = "system.test";

    // The first bundle carries the paying and receiving form.
    ores::refdata::domain::tenor_basis_swap_pay_receive pay_receive;
    pay_receive.pay_index = std::string("EUR-LIBOR-6M");
    pay_receive.receive_index = std::string("EUR-LIBOR-3M");
    pay_receive.change_reason_code = "system.test";

    ores::refdata::service::conventions_bundle first;
    first.rows.push_back(
        {.family = "convention", .sequence = 0, .payload = rfl::json::write(central)});
    first.rows.push_back({.family = "tenor_basis_swap_pay_receive",
                          .sequence = 0,
                          .payload = rfl::json::write(pay_receive)});
    ores::refdata::service::conventions_bundle_service(ctx).apply(first);

    // The second carries the long and short form for the same convention. A
    // convention fills one form, so the first form is closed.
    ores::refdata::domain::tenor_basis_swap_long_short long_short;
    long_short.long_index = std::string("EUR-LIBOR-6M");
    long_short.short_index = std::string("EUR-LIBOR-3M");
    long_short.change_reason_code = "system.test";

    ores::refdata::service::conventions_bundle second;
    second.rows.push_back(
        {.family = "convention", .sequence = 0, .payload = rfl::json::write(central)});
    second.rows.push_back({.family = "tenor_basis_swap_long_short",
                           .sequence = 0,
                           .payload = rfl::json::write(long_short)});
    ores::refdata::service::conventions_bundle_service(ctx).apply(second);

    ores::refdata::repository::tenor_basis_swap_pay_receive_repository pay_receives;
    ores::refdata::repository::tenor_basis_swap_long_short_repository long_shorts;
    CHECK(pay_receives.read_latest(ctx).empty());
    const auto held_long_shorts = long_shorts.read_latest(ctx);
    REQUIRE(held_long_shorts.size() == 1);

    ores::refdata::repository::convention_repository conventions;
    const auto held_conventions = conventions.read_latest(ctx);
    REQUIRE(held_conventions.size() == 1);
    CHECK(held_long_shorts.front().convention_id == held_conventions.front().id);
}

TEST_CASE("a bundle refuses a family the registry does not know", tags) {
    ores::testing::scoped_database_helper h;
    const auto ctx = party_context(h);

    ores::refdata::service::conventions_bundle bundle;
    bundle.rows.push_back({.family = "no_such_family", .sequence = 0, .payload = "{}"});

    CHECK_THROWS_AS(ores::refdata::service::conventions_bundle_service(ctx).apply(bundle),
                    std::runtime_error);
}
