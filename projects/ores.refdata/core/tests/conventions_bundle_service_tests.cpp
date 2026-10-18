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
#include "ores.refdata.core/service/conventions_document_service.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.testing/make_generation_context.hpp"
#include "ores.testing/scoped_database_helper.hpp"
#include "ores.utility/rfl/reflectors.hpp"
#include <boost/uuid/random_generator.hpp>
#include <algorithm>
#include <catch2/catch_test_macros.hpp>
#include <optional>
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

TEST_CASE("a bundle whose part comes first still writes the central row first", tags) {
    ores::testing::scoped_database_helper h;
    const auto ctx = party_context(h);
    boost::uuids::random_generator uuid;

    ores::refdata::domain::convention central;
    central.id = uuid();
    central.kind_code = "TENOR_BASIS_SWAP";
    central.name = "JPY-3M-6M-BASIS";
    central.change_reason_code = "system.test";

    ores::refdata::domain::tenor_basis_swap_long_short long_short;
    long_short.convention_id = central.id;
    long_short.long_index = std::string("JPY-LIBOR-6M");
    long_short.short_index = std::string("JPY-LIBOR-3M");
    long_short.change_reason_code = "system.test";

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
    CHECK(held_parts.front().convention_id == held.front().id);
}

TEST_CASE("a bundle reads back the rows an apply wrote", tags) {
    ores::testing::scoped_database_helper h;
    const auto ctx = party_context(h);
    boost::uuids::random_generator uuid;

    ores::refdata::domain::convention central;
    central.id = uuid();
    central.kind_code = "TENOR_BASIS_SWAP";
    central.name = "CHF-3M-6M-BASIS";
    central.change_reason_code = "system.test";

    ores::refdata::domain::tenor_basis_swap_pay_receive part;
    part.convention_id = central.id;
    part.pay_index = std::string("CHF-LIBOR-6M");
    part.receive_index = std::string("CHF-LIBOR-3M");
    part.change_reason_code = "system.test";

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
    CHECK(read_central->name == "CHF-3M-6M-BASIS");

    const auto read_part =
        rfl::json::read<ores::refdata::domain::tenor_basis_swap_pay_receive>(part_row.payload);
    REQUIRE(read_part.has_value());
    CHECK(read_part->convention_id == read_central->id);
    CHECK(read_part->pay_index == std::optional<std::string>("CHF-LIBOR-6M"));
}

TEST_CASE("the shared convention family reads back only the kind the bundle covers", tags) {
    ores::testing::scoped_database_helper h;
    const auto ctx = party_context(h);
    boost::uuids::random_generator uuid;

    ores::refdata::domain::convention covered;
    covered.id = uuid();
    covered.kind_code = "TENOR_BASIS_SWAP";
    covered.name = "NOK-3M-6M-BASIS";
    covered.change_reason_code = "system.test";

    ores::refdata::domain::convention other;
    other.id = uuid();
    other.kind_code = "DEPOSIT";
    other.name = "NOK-DEPOSIT";
    other.change_reason_code = "system.test";

    ores::refdata::service::conventions_bundle written;
    written.rows.push_back(
        {.family = "convention", .sequence = 0, .payload = rfl::json::write(covered)});
    ores::refdata::service::conventions_bundle_service(ctx).apply(written);

    // The shared table carries every kind, so a row of another kind is written
    // straight to it and must not appear in the bundle's read.
    ores::refdata::repository::convention_repository conventions;
    ores::service::messaging::stamp(other, ctx);
    conventions.write(ctx, other);

    const auto read = ores::refdata::service::conventions_bundle_service(ctx).read();
    std::vector<std::string> central_names;
    for (const auto& row : read.rows) {
        if (row.family != "convention")
            continue;
        const auto decoded = rfl::json::read<ores::refdata::domain::convention>(row.payload);
        REQUIRE(decoded.has_value());
        central_names.push_back(decoded->name);
    }
    // One central row, and it is the kind the bundle covers.
    REQUIRE(central_names.size() == 1);
    CHECK(central_names.front() == "NOK-3M-6M-BASIS");
}

TEST_CASE("a read bundle applies into another party unchanged", tags) {
    ores::testing::scoped_database_helper h;
    const auto first = party_context(h);
    const auto second = party_context(h);
    boost::uuids::random_generator uuid;

    ores::refdata::domain::convention central;
    central.id = uuid();
    central.kind_code = "TENOR_BASIS_SWAP";
    central.name = "SEK-3M-6M-BASIS";
    central.change_reason_code = "system.test";

    ores::refdata::domain::tenor_basis_swap_pay_receive part;
    part.convention_id = central.id;
    part.pay_index = std::string("SEK-LIBOR-6M");
    part.receive_index = std::string("SEK-LIBOR-3M");
    part.change_reason_code = "system.test";

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
    CHECK(copied_central->name == "SEK-3M-6M-BASIS");

    const auto copied_part = rfl::json::read<ores::refdata::domain::tenor_basis_swap_pay_receive>(
        copied_rows.back().payload);
    REQUIRE(copied_part.has_value());
    CHECK(copied_part->pay_index == std::optional<std::string>("SEK-LIBOR-6M"));
    CHECK(copied_part->receive_index == std::optional<std::string>("SEK-LIBOR-3M"));
    CHECK(copied_part->convention_id == copied_central->id);
}

namespace {

// How many rows of one family a document read holds. The document's member
// names are the family keys, so this is the whole of the mapping between them.
std::size_t document_row_count(const ores::refdata::messaging::conventions_document& document,
                               const std::string& family) {
    if (family == "average_ois")
        return document.average_ois.size();
    if (family == "bma_basis_swap")
        return document.bma_basis_swap.size();
    if (family == "bond_yield")
        return document.bond_yield.size();
    if (family == "cds")
        return document.cds.size();
    if (family == "cms_spread_option")
        return document.cms_spread_option.size();
    if (family == "commodity_forward")
        return document.commodity_forward.size();
    if (family == "commodity_future")
        return document.commodity_future.size();
    if (family == "convention")
        return document.convention.size();
    if (family == "cross_currency_basis")
        return document.cross_currency_basis.size();
    if (family == "cross_currency_fix_float")
        return document.cross_currency_fix_float.size();
    if (family == "deposit")
        return document.deposit.size();
    if (family == "fra")
        return document.fra.size();
    if (family == "future")
        return document.future.size();
    if (family == "fx_option")
        return document.fx_option.size();
    if (family == "ibor_index")
        return document.ibor_index.size();
    if (family == "inflation_swap")
        return document.inflation_swap.size();
    if (family == "intraday_power_load")
        return document.intraday_power_load.size();
    if (family == "ois")
        return document.ois.size();
    if (family == "overnight_index")
        return document.overnight_index.size();
    if (family == "swap")
        return document.swap.size();
    if (family == "swap_index")
        return document.swap_index.size();
    if (family == "tenor_basis_swap_long_short")
        return document.tenor_basis_swap_long_short.size();
    if (family == "tenor_basis_swap_pay_receive")
        return document.tenor_basis_swap_pay_receive.size();
    if (family == "tenor_basis_two_swap")
        return document.tenor_basis_two_swap.size();
    if (family == "zero")
        return document.zero.size();
    if (family == "zero_inflation_index")
        return document.zero_inflation_index.size();
    throw std::logic_error("Not a convention family: " + family);
}

}

TEST_CASE("the bundle read carries what the document read carries", tags) {
    ores::testing::scoped_database_helper h;
    const auto ctx = party_context(h);
    boost::uuids::random_generator uuid;

    ores::refdata::messaging::conventions_document document;
    ores::refdata::domain::convention central;
    central.id = uuid();
    central.kind_code = "TENOR_BASIS_SWAP";
    central.name = "AUD-3M-6M-BASIS";
    central.change_reason_code = "system.test";
    document.convention.push_back(central);

    ores::refdata::domain::tenor_basis_swap_pay_receive part;
    part.convention_id = central.id;
    part.pay_index = std::string("AUD-LIBOR-6M");
    part.receive_index = std::string("AUD-LIBOR-3M");
    part.change_reason_code = "system.test";
    document.tenor_basis_swap_pay_receive.push_back(part);

    // One family of each write mode, so each way a family reaches its table is
    // compared with rows in it rather than with nothing.
    auto gen = ores::testing::make_generation_context(h);
    auto deposit = ores::refdata::generators::generate_synthetic_deposit_convention(gen);
    deposit.change_reason_code = "system.test";
    document.deposit.push_back(deposit);
    auto index = ores::refdata::generators::generate_synthetic_ibor_index_convention(gen);
    index.change_reason_code = "system.test";
    document.ibor_index.push_back(index);

    ores::refdata::service::conventions_document_service documents(ctx);
    documents.save(document);
    const auto held = documents.get();

    REQUIRE_FALSE(held.convention.empty());
    REQUIRE_FALSE(held.tenor_basis_swap_pay_receive.empty());
    REQUIRE_FALSE(held.deposit.empty());
    REQUIRE_FALSE(held.ibor_index.empty());

    const auto read = ores::refdata::service::conventions_bundle_service(ctx).read();
    const auto bundle_count = [&read](const std::string& family) {
        return static_cast<std::size_t>(
            std::count_if(read.rows.begin(), read.rows.end(), [&family](const auto& row) {
                return row.family == family;
            }));
    };

    // Every family the registry names. The three the case seeded are the ones
    // whose comparison can fail on a dropped family; the rest are checked so a
    // read that invents a row is caught too.
    for (std::size_t i = 0; i < ores::refdata::domain::convention_family_count; ++i) {
        const auto family = std::string(ores::refdata::domain::convention_families[i].key);
        INFO("family: " << family);
        CHECK(bundle_count(family) == document_row_count(held, family));
    }
    CHECK(bundle_count("convention") > 0);
    CHECK(bundle_count("tenor_basis_swap_pay_receive") > 0);
    CHECK(bundle_count("deposit") > 0);
    CHECK(bundle_count("ibor_index") > 0);
}

TEST_CASE("the bundle read drops a part whose central row it does not carry", tags) {
    ores::testing::scoped_database_helper h;
    const auto ctx = party_context(h);
    boost::uuids::random_generator uuid;

    // A central row of another kind, and a part that names it. The document
    // read keeps neither, because the part belongs to a convention this bundle
    // does not carry.
    ores::refdata::domain::convention other;
    other.id = uuid();
    other.kind_code = "DEPOSIT";
    other.name = "AUD-DEPOSIT";
    other.change_reason_code = "system.test";
    ores::service::messaging::stamp(other, ctx);
    ores::refdata::repository::convention_repository conventions;
    conventions.write(ctx, other);

    ores::refdata::domain::tenor_basis_swap_pay_receive orphan;
    orphan.id = uuid();
    orphan.convention_id = other.id;
    orphan.pay_index = std::string("AUD-LIBOR-6M");
    orphan.receive_index = std::string("AUD-LIBOR-3M");
    orphan.change_reason_code = "system.test";
    ores::service::messaging::stamp(orphan, ctx);
    ores::refdata::repository::tenor_basis_swap_pay_receive_repository parts;
    parts.write(ctx, orphan);

    // A convention of the kind the bundle covers, with a part that names it.
    // That part survives, and the orphan does not.
    ores::refdata::domain::convention covered;
    covered.id = uuid();
    covered.kind_code = "TENOR_BASIS_SWAP";
    covered.name = "AUD-3M-6M-BASIS";
    covered.change_reason_code = "system.test";

    ores::refdata::domain::tenor_basis_swap_pay_receive attached;
    attached.id = uuid();
    attached.convention_id = covered.id;
    attached.pay_index = std::string("AUD-LIBOR-6M");
    attached.receive_index = std::string("AUD-LIBOR-3M");
    attached.change_reason_code = "system.test";

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
