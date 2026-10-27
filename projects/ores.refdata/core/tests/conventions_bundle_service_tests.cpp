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
#include "ores.refdata.api/domain/convention_family_registry.hpp"
#include "ores.refdata.api/domain/tenor_basis_swap_long_short.hpp"
#include "ores.refdata.api/domain/tenor_basis_swap_pay_receive.hpp"
#include "ores.refdata.api/generators/deposit_convention_generator.hpp"
#include "ores.refdata.api/generators/ibor_index_convention_generator.hpp"
#include "ores.refdata.api/generators/party_generator.hpp"
#include "ores.refdata.core/repository/convention_repository.hpp"
#include "ores.refdata.core/repository/party_repository.hpp"
#include "ores.refdata.core/repository/tenor_basis_swap_long_short_repository.hpp"
#include "ores.refdata.core/repository/tenor_basis_swap_pay_receive_repository.hpp"
#include "ores.refdata.core/service/conventions_bundle.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.testing/make_generation_context.hpp"
#include "ores.testing/scoped_database_helper.hpp"
#include "ores.utility/rfl/reflectors.hpp"
#include "test_helpers.hpp"
#include <boost/uuid/random_generator.hpp>
#include <algorithm>
#include <catch2/catch_test_macros.hpp>
#include <optional>
#include <rfl/json.hpp>
#include <stdexcept>
#include <string>
#include <vector>


TEST_CASE("a bundle resolves the identity the party already holds",
          "[refdata][conventions][bundle][database]") {
    ores::testing::scoped_database_helper h;
    const auto ctx = party_context(h);
    boost::uuids::random_generator uuid;

    // The first bundle carries a convention whose key the mapper minted. The
    // service resolves the identity and gives the row the key it will keep.
    const std::string name = "GBP-LIBOR-3M-6M-BASIS-CONVENTIONS";

    ores::refdata::domain::convention central;
    central.identity.id = uuid();
    central.identity.kind_code = "TENOR_BASIS_SWAP";
    central.payload.name = name;
    central.audit.change_reason_code = "system.test";

    ores::refdata::domain::tenor_basis_swap_pay_receive part;
    part.identity.convention_id = central.identity.id;
    part.pay_leg.pay_index = std::string("GBP-LIBOR-6M");
    part.rec_leg.receive_index = std::string("GBP-LIBOR-3M");
    part.audit.change_reason_code = "system.test";

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
    const auto resolved_id = after_first.front().identity.id;
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
    second_central.identity.id = uuid();
    auto second_part = part;
    second_part.identity.id = uuid();
    second_part.identity.convention_id = second_central.identity.id;

    ores::refdata::service::conventions_bundle second;
    second.rows.push_back(
        {.family = "convention", .sequence = 0, .payload = rfl::json::write(second_central)});
    second.rows.push_back({.family = "tenor_basis_swap_pay_receive",
                           .sequence = 0,
                           .payload = rfl::json::write(second_part)});
    ores::refdata::service::conventions_bundle_service(ctx).apply(second);

    const auto held = conventions.read_latest(ctx);
    REQUIRE(held.size() == 1);
    CHECK(held.front().identity.id == resolved_id);
    // A second bundle that resolved to the first row replaced it, rather than
    // adding another version of a row it minted itself.
    CHECK(held.front().identity.version > after_first.front().identity.version);

    ores::refdata::repository::tenor_basis_swap_pay_receive_repository parts;
    const auto held_parts = parts.read_latest(ctx);
    REQUIRE(held_parts.size() == 1);
    CHECK(held_parts.front().identity.id == resolved_part.identity.id);
    CHECK(held_parts.front().identity.convention_id == resolved_id);
    CHECK(held_parts.front().identity.version > resolved_part.identity.version);
}

TEST_CASE("a bundle that changes leg form closes the other form",
          "[refdata][conventions][bundle][database]") {
    ores::testing::scoped_database_helper h;
    const auto ctx = party_context(h);
    boost::uuids::random_generator uuid;

    const std::string name = "EUR-3M-6M-BASIS";
    ores::refdata::domain::convention central;
    central.identity.kind_code = "TENOR_BASIS_SWAP";
    central.payload.name = name;
    central.audit.change_reason_code = "system.test";

    // The first bundle carries the paying and receiving form.
    ores::refdata::domain::tenor_basis_swap_pay_receive pay_receive;
    pay_receive.pay_leg.pay_index = std::string("EUR-LIBOR-6M");
    pay_receive.rec_leg.receive_index = std::string("EUR-LIBOR-3M");
    pay_receive.audit.change_reason_code = "system.test";

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
    long_short.long_leg.long_index = std::string("EUR-LIBOR-6M");
    long_short.short_leg.short_index = std::string("EUR-LIBOR-3M");
    long_short.audit.change_reason_code = "system.test";

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
    CHECK(held_long_shorts.front().identity.convention_id == held_conventions.front().identity.id);
}

TEST_CASE("applying a party's convention again replaces it",
          "[refdata][conventions][bundle][database]") {
    ores::testing::scoped_database_helper h;
    const auto ctx = party_context(h);
    auto gen = ores::testing::make_generation_context(h);
    auto deposit = ores::refdata::generators::generate_synthetic_deposit_convention(gen);
    deposit.change_reason_code = "system.test";

    ores::refdata::service::conventions_bundle bundle;
    bundle.rows.push_back(
        {.family = "deposit", .sequence = 0, .payload = rfl::json::write(deposit)});

    ores::refdata::service::conventions_bundle_service service(ctx);
    service.apply(bundle);
    CHECK_NOTHROW(service.apply(bundle));

    const auto held = service.read();
    const auto copies = std::count_if(held.rows.begin(), held.rows.end(), [&](const auto& row) {
        if (row.family != "deposit")
            return false;
        const auto decoded =
            rfl::json::read<ores::refdata::domain::deposit_convention>(row.payload);
        return decoded && decoded->id == deposit.id;
    });
    CHECK(copies == 1);
}

TEST_CASE("a world convention the tenant holds is left as it is",
          "[refdata][conventions][bundle][database]") {
    ores::testing::scoped_database_helper h;
    const auto ctx = party_context(h);
    auto gen = ores::testing::make_generation_context(h);
    auto index = ores::refdata::generators::generate_synthetic_ibor_index_convention(gen);
    index.change_reason_code = "system.test";

    ores::refdata::service::conventions_bundle bundle;
    bundle.rows.push_back(
        {.family = "ibor_index", .sequence = 0, .payload = rfl::json::write(index)});

    const auto version_held = [&](const ores::refdata::service::conventions_bundle& read) {
        for (const auto& row : read.rows) {
            if (row.family != "ibor_index")
                continue;
            const auto decoded =
                rfl::json::read<ores::refdata::domain::ibor_index_convention>(row.payload);
            if (decoded && decoded->id == index.id)
                return decoded->version;
        }
        throw std::logic_error("The tenant does not hold the index convention.");
    };

    ores::refdata::service::conventions_bundle_service service(ctx);
    CHECK(service.apply(bundle).world_kept.empty());
    const auto version = version_held(service.read());

    const auto second = service.apply(bundle);
    CHECK(second.world_kept == std::vector<std::string>{index.id});
    CHECK(version_held(service.read()) == version);
}

TEST_CASE("a bundle refuses a family the registry does not know",
          "[refdata][conventions][bundle][database]") {
    ores::testing::scoped_database_helper h;
    const auto ctx = party_context(h);

    ores::refdata::service::conventions_bundle bundle;
    bundle.rows.push_back({.family = "no_such_family", .sequence = 0, .payload = "{}"});

    CHECK_THROWS_AS(ores::refdata::service::conventions_bundle_service(ctx).apply(bundle),
                    std::runtime_error);
}

TEST_CASE("a bundle whose part comes first still writes the central row first",
          "[refdata][conventions][bundle][database]") {
    ores::testing::scoped_database_helper h;
    const auto ctx = party_context(h);
    boost::uuids::random_generator uuid;

    ores::refdata::domain::convention central;
    central.identity.id = uuid();
    central.identity.kind_code = "TENOR_BASIS_SWAP";
    central.payload.name = "JPY-3M-6M-BASIS";
    central.audit.change_reason_code = "system.test";

    ores::refdata::domain::tenor_basis_swap_long_short long_short;
    long_short.identity.convention_id = central.identity.id;
    long_short.long_leg.long_index = std::string("JPY-LIBOR-6M");
    long_short.short_leg.short_index = std::string("JPY-LIBOR-3M");
    long_short.audit.change_reason_code = "system.test";

    // The bundle carries the part before the central row. The writer owns the
    // order for this family, so the foreign key holds anyway.
    ores::refdata::service::conventions_bundle bundle;
    bundle.rows.push_back({.family = "tenor_basis_swap_long_short",
                           .sequence = 0,
                           .payload = rfl::json::write(long_short)});
    bundle.rows.push_back(
        {.family = "convention", .sequence = 1, .payload = rfl::json::write(central)});
    ores::refdata::service::conventions_bundle_service(ctx).apply(bundle);

    ores::refdata::repository::convention_repository conventions;
    const auto held = conventions.read_latest(ctx);
    REQUIRE(held.size() == 1);

    ores::refdata::repository::tenor_basis_swap_long_short_repository parts;
    const auto held_parts = parts.read_latest(ctx);
    REQUIRE(held_parts.size() == 1);
    CHECK(held_parts.front().identity.convention_id == held.front().identity.id);
}

TEST_CASE("a bundle reads back the rows an apply wrote",
          "[refdata][conventions][bundle][database]") {
    ores::testing::scoped_database_helper h;
    const auto ctx = party_context(h);
    boost::uuids::random_generator uuid;

    ores::refdata::domain::convention central;
    central.identity.id = uuid();
    central.identity.kind_code = "TENOR_BASIS_SWAP";
    central.payload.name = "CHF-3M-6M-BASIS";
    central.audit.change_reason_code = "system.test";

    ores::refdata::domain::tenor_basis_swap_pay_receive part;
    part.identity.convention_id = central.identity.id;
    part.pay_leg.pay_index = std::string("CHF-LIBOR-6M");
    part.rec_leg.receive_index = std::string("CHF-LIBOR-3M");
    part.audit.change_reason_code = "system.test";

    ores::refdata::service::conventions_bundle written;
    written.rows.push_back(
        {.family = "convention", .sequence = 0, .payload = rfl::json::write(central)});
    written.rows.push_back({.family = "tenor_basis_swap_pay_receive",
                            .sequence = 0,
                            .payload = rfl::json::write(part)});
    ores::refdata::service::conventions_bundle_service service(ctx);
    service.apply(written);

    // The read carries the tenant's world conventions as well, which another
    // case may have written, so this case looks only at the families it wrote.
    const auto read = service.read();
    std::vector<ores::refdata::service::conventions_bundle_row> central_rows;
    std::vector<ores::refdata::service::conventions_bundle_row> part_rows;
    for (const auto& row : read.rows) {
        if (row.family == "convention")
            central_rows.push_back(row);
        if (row.family == "tenor_basis_swap_pay_receive")
            part_rows.push_back(row);
    }
    REQUIRE(central_rows.size() == 1);
    REQUIRE(part_rows.size() == 1);
    const auto& central_row = central_rows.front();
    const auto& part_row = part_rows.front();

    // The payloads are the family's own bytes, so they decode into its type and
    // the part names the row the identity resolved to.
    const auto read_central =
        rfl::json::read<ores::refdata::domain::convention>(central_row.payload);
    REQUIRE(read_central.has_value());
    CHECK(read_central->payload.name == "CHF-3M-6M-BASIS");

    const auto read_part =
        rfl::json::read<ores::refdata::domain::tenor_basis_swap_pay_receive>(part_row.payload);
    REQUIRE(read_part.has_value());
    CHECK(read_part->identity.convention_id == read_central->identity.id);
    CHECK(read_part->pay_leg.pay_index == std::optional<std::string>("CHF-LIBOR-6M"));
}

TEST_CASE("the shared convention family reads back only the kind the bundle covers",
          "[refdata][conventions][bundle][database]") {
    ores::testing::scoped_database_helper h;
    const auto ctx = party_context(h);
    boost::uuids::random_generator uuid;

    ores::refdata::domain::convention covered;
    covered.identity.id = uuid();
    covered.identity.kind_code = "TENOR_BASIS_SWAP";
    covered.payload.name = "NOK-3M-6M-BASIS";
    covered.audit.change_reason_code = "system.test";

    ores::refdata::domain::convention other;
    other.identity.id = uuid();
    other.identity.kind_code = "DEPOSIT";
    other.payload.name = "NOK-DEPOSIT";
    other.audit.change_reason_code = "system.test";

    ores::refdata::service::conventions_bundle written;
    written.rows.push_back(
        {.family = "convention", .sequence = 0, .payload = rfl::json::write(covered)});
    ores::refdata::service::conventions_bundle_service(ctx).apply(written);

    // The shared table carries every kind, so a row of another kind is written
    // straight to it and must not appear in the bundle's read.
    ores::refdata::repository::convention_repository conventions;
    ores::service::messaging::stamp_document(other, ctx);
    conventions.write(ctx, other);

    const auto read = ores::refdata::service::conventions_bundle_service(ctx).read();
    std::vector<std::string> central_names;
    for (const auto& row : read.rows) {
        if (row.family != "convention")
            continue;
        const auto decoded = rfl::json::read<ores::refdata::domain::convention>(row.payload);
        REQUIRE(decoded.has_value());
        central_names.push_back(decoded->payload.name);
    }
    // One central row, and it is the kind the bundle covers.
    REQUIRE(central_names.size() == 1);
    CHECK(central_names.front() == "NOK-3M-6M-BASIS");
}

TEST_CASE("a read bundle applies into another party unchanged",
          "[refdata][conventions][bundle][database]") {
    ores::testing::scoped_database_helper h;
    const auto first = party_context(h);
    const auto second = party_context(h);
    boost::uuids::random_generator uuid;

    ores::refdata::domain::convention central;
    central.identity.id = uuid();
    central.identity.kind_code = "TENOR_BASIS_SWAP";
    central.payload.name = "SEK-3M-6M-BASIS";
    central.audit.change_reason_code = "system.test";

    ores::refdata::domain::tenor_basis_swap_pay_receive part;
    part.identity.convention_id = central.identity.id;
    part.pay_leg.pay_index = std::string("SEK-LIBOR-6M");
    part.rec_leg.receive_index = std::string("SEK-LIBOR-3M");
    part.audit.change_reason_code = "system.test";

    ores::refdata::service::conventions_bundle written;
    written.rows.push_back(
        {.family = "convention", .sequence = 0, .payload = rfl::json::write(central)});
    written.rows.push_back({.family = "tenor_basis_swap_pay_receive",
                            .sequence = 0,
                            .payload = rfl::json::write(part)});

    ores::refdata::service::conventions_bundle_service(first).apply(written);
    const auto read = ores::refdata::service::conventions_bundle_service(first).read();

    ores::refdata::service::conventions_bundle_service(second).apply(read);
    const auto read_again = ores::refdata::service::conventions_bundle_service(second).read();

    // Both parties see the tenant's world conventions, so the rows this case
    // wrote are found by family rather than by excluding one world family at a
    // time.
    const auto rows_of = [](const ores::refdata::service::conventions_bundle& bundle) {
        std::vector<ores::refdata::service::conventions_bundle_row> rows;
        for (const auto& row : bundle.rows)
            if (row.family == "convention" || row.family == "tenor_basis_swap_pay_receive")
                rows.push_back(row);
        return rows;
    };
    const auto written_rows = rows_of(read);
    const auto copied_rows = rows_of(read_again);
    REQUIRE(written_rows.size() == 2);
    REQUIRE(copied_rows.size() == 2);
    for (std::size_t i = 0; i < written_rows.size(); ++i)
        CHECK(copied_rows[i].family == written_rows[i].family);

    // A copy into another party mints its own keys, so the payloads are not
    // byte-equal. The fields the document carries are what has to survive.
    const auto copied_central =
        rfl::json::read<ores::refdata::domain::convention>(copied_rows.front().payload);
    REQUIRE(copied_central.has_value());
    CHECK(copied_central->payload.name == "SEK-3M-6M-BASIS");

    const auto copied_part = rfl::json::read<ores::refdata::domain::tenor_basis_swap_pay_receive>(
        copied_rows.back().payload);
    REQUIRE(copied_part.has_value());
    CHECK(copied_part->pay_leg.pay_index == std::optional<std::string>("SEK-LIBOR-6M"));
    CHECK(copied_part->rec_leg.receive_index == std::optional<std::string>("SEK-LIBOR-3M"));
    CHECK(copied_part->identity.convention_id == copied_central->identity.id);
}

TEST_CASE("a bundle read carries a row for every family an apply wrote",
          "[refdata][conventions][bundle][database]") {
    ores::testing::scoped_database_helper h;
    const auto ctx = party_context(h);
    boost::uuids::random_generator uuid;

    ores::refdata::domain::convention central;
    central.identity.id = uuid();
    central.identity.kind_code = "TENOR_BASIS_SWAP";
    central.payload.name = "AUD-3M-6M-BASIS";
    central.audit.change_reason_code = "system.test";

    ores::refdata::domain::tenor_basis_swap_pay_receive part;
    part.identity.convention_id = central.identity.id;
    part.pay_leg.pay_index = std::string("AUD-LIBOR-6M");
    part.rec_leg.receive_index = std::string("AUD-LIBOR-3M");
    part.audit.change_reason_code = "system.test";

    // One family of each write mode, so each way a family reaches its table is
    // read back with a row in it rather than with nothing.
    auto gen = ores::testing::make_generation_context(h);
    auto deposit = ores::refdata::generators::generate_synthetic_deposit_convention(gen);
    deposit.change_reason_code = "system.test";
    auto index = ores::refdata::generators::generate_synthetic_ibor_index_convention(gen);
    index.change_reason_code = "system.test";

    ores::refdata::service::conventions_bundle bundle;
    bundle.rows.push_back(
        {.family = "convention", .sequence = 0, .payload = rfl::json::write(central)});
    bundle.rows.push_back({.family = "tenor_basis_swap_pay_receive",
                           .sequence = 0,
                           .payload = rfl::json::write(part)});
    bundle.rows.push_back(
        {.family = "deposit", .sequence = 0, .payload = rfl::json::write(deposit)});
    bundle.rows.push_back(
        {.family = "ibor_index", .sequence = 0, .payload = rfl::json::write(index)});

    ores::refdata::service::conventions_bundle_service service(ctx);
    service.apply(bundle);
    const auto read = service.read();

    const auto rows_of = [&read](const std::string& family) {
        return static_cast<std::size_t>(
            std::count_if(read.rows.begin(), read.rows.end(), [&family](const auto& row) {
                return row.family == family;
            }));
    };

    // The party's own families hold exactly the row written. The world family
    // may also hold rows the tenant already had, so it holds at least one.
    CHECK(rows_of("convention") == 1);
    CHECK(rows_of("tenor_basis_swap_pay_receive") == 1);
    CHECK(rows_of("deposit") == 1);
    CHECK(rows_of("ibor_index") >= 1);
}

TEST_CASE("the bundle read drops a part whose central row it does not carry",
          "[refdata][conventions][bundle][database]") {
    ores::testing::scoped_database_helper h;
    const auto ctx = party_context(h);
    boost::uuids::random_generator uuid;

    // A central row of another kind, and a part that names it. The bundle
    // read keeps neither, because the part belongs to a convention this bundle
    // does not carry.
    ores::refdata::domain::convention other;
    other.identity.id = uuid();
    other.identity.kind_code = "DEPOSIT";
    other.payload.name = "AUD-DEPOSIT";
    other.audit.change_reason_code = "system.test";
    ores::service::messaging::stamp_document(other, ctx);
    ores::refdata::repository::convention_repository conventions;
    conventions.write(ctx, other);

    ores::refdata::domain::tenor_basis_swap_pay_receive orphan;
    orphan.identity.id = uuid();
    orphan.identity.convention_id = other.identity.id;
    orphan.pay_leg.pay_index = std::string("AUD-LIBOR-6M");
    orphan.rec_leg.receive_index = std::string("AUD-LIBOR-3M");
    orphan.audit.change_reason_code = "system.test";
    ores::service::messaging::stamp_document(orphan, ctx);
    ores::refdata::repository::tenor_basis_swap_pay_receive_repository parts;
    parts.write(ctx, orphan);

    // A convention of the kind the bundle covers, with a part that names it.
    // That part survives, and the orphan does not.
    ores::refdata::domain::convention covered;
    covered.identity.id = uuid();
    covered.identity.kind_code = "TENOR_BASIS_SWAP";
    covered.payload.name = "AUD-3M-6M-BASIS";
    covered.audit.change_reason_code = "system.test";

    ores::refdata::domain::tenor_basis_swap_pay_receive attached;
    attached.identity.id = uuid();
    attached.identity.convention_id = covered.identity.id;
    attached.pay_leg.pay_index = std::string("AUD-LIBOR-6M");
    attached.rec_leg.receive_index = std::string("AUD-LIBOR-3M");
    attached.audit.change_reason_code = "system.test";

    ores::refdata::service::conventions_bundle written;
    written.rows.push_back(
        {.family = "convention", .sequence = 0, .payload = rfl::json::write(covered)});
    written.rows.push_back({.family = "tenor_basis_swap_pay_receive",
                            .sequence = 0,
                            .payload = rfl::json::write(attached)});
    ores::refdata::service::conventions_bundle_service(ctx).apply(written);

    const auto read = ores::refdata::service::conventions_bundle_service(ctx).read();
    std::size_t central_rows = 0;
    std::size_t part_rows = 0;
    for (const auto& row : read.rows) {
        INFO("family: " << row.family);
        if (row.family == "convention")
            ++central_rows;
        if (row.family == "tenor_basis_swap_pay_receive")
            ++part_rows;
    }
    CHECK(central_rows == 1);
    CHECK(part_rows == 1);

    const auto held_parts = parts.read_latest(ctx);
    REQUIRE(held_parts.size() == 2);
}
