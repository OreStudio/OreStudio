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
#include "ores.logging/boost_severity.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.refdata.api/domain/party_currency.hpp"
#include "ores.refdata.api/domain/party_currency_json_io.hpp" // IWYU pragma: keep.
#include "ores.refdata.api/generators/currency_generator.hpp"
#include "ores.refdata.core/repository/currency_repository.hpp"
#include "ores.refdata.core/repository/party_currency_repository.hpp"
#include "ores.refdata.core/repository/party_repository.hpp"
#include "ores.testing/make_generation_context.hpp"
#include "ores.testing/scoped_database_helper.hpp"
#include "ores.utility/rfl/reflectors.hpp"       // IWYU pragma: keep.
#include "ores.utility/streaming/std_vector.hpp" // IWYU pragma: keep.
#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/log/sources/severity_feature.hpp>
#include <boost/uuid/random_generator.hpp>
#include <boost/uuid/uuid.hpp>
#include <algorithm>
#include <catch2/catch_test_macros.hpp>
#include <stdexcept>
#include <string>
#include <string_view>
#include <vector>

using namespace ores::logging;
using namespace ores::refdata::generators;

using ores::refdata::domain::party_currency;
using ores::refdata::repository::party_currency_repository;
using ores::refdata::repository::party_repository;
using ores::refdata::repository::currency_repository;
using ores::testing::scoped_database_helper;

namespace {

const std::string_view test_suite("ores.refdata.tests");
const std::string tags("[repository][party_currency]");

boost::uuids::uuid find_system_party_id(party_repository& repo,
                                        ores::database::context ctx,
                                        const ores::utility::uuid::tenant_id& tid) {
    auto parties = repo.read_latest(ctx);
    for (const auto& p : parties)
        if (p.tenant_id == tid && p.party_category == "System")
            return p.id;
    throw std::runtime_error("No system party for tenant: " + tid.to_string());
}

party_currency make_party_currency(scoped_database_helper& h,
                                   const boost::uuids::uuid& party_id,
                                   const std::string& iso_code) {
    party_currency pc;
    pc.tenant_id = h.tenant_id().to_string();
    pc.party_id = party_id;
    pc.currency_iso_code = iso_code;
    pc.modified_by = h.db_user();
    pc.change_reason_code = "system.test";
    pc.change_commentary = "Synthetic test data";
    pc.performed_by = h.db_user();
    return pc;
}

}

TEST_CASE("write_single_party_currency", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;

    party_repository party_repo;
    currency_repository cur_repo;

    const auto party_id = find_system_party_id(party_repo, h.context(), h.tenant_id());
    h.set_party(party_id);
    party_currency_repository repo(h.context());

    auto gctx = ores::testing::make_generation_context(h);
    auto test_currency = generate_synthetic_currency(gctx);
    cur_repo.write(h.context(), test_currency);
    const auto& iso_code = test_currency.iso_code;

    auto pc = make_party_currency(h, party_id, iso_code);
    BOOST_LOG_SEV(lg, debug) << "Party currency: " << pc;
    repo.write(pc);

    const auto read_pcs = repo.read_latest(pc.party_id, pc.currency_iso_code);
    REQUIRE(read_pcs.size() == 1);
    CHECK(read_pcs[0].party_id == pc.party_id);
    CHECK(read_pcs[0].currency_iso_code == pc.currency_iso_code);
    CHECK(read_pcs[0].change_commentary == pc.change_commentary);
}

TEST_CASE("write_multiple_party_currencies", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;

    party_repository party_repo;
    currency_repository cur_repo;

    const auto system_party_id = find_system_party_id(party_repo, h.context(), h.tenant_id());
    h.set_party(system_party_id);
    party_currency_repository repo(h.context());

    auto gctx = ores::testing::make_generation_context(h);
    auto test_currencies = generate_unique_synthetic_currencies(2, gctx);
    cur_repo.write(h.context(), test_currencies);

    std::vector<party_currency> pcs;
    for (const auto& c : test_currencies) {
        pcs.push_back(make_party_currency(h, system_party_id, c.iso_code));
    }

    BOOST_LOG_SEV(lg, debug) << "Party currencies: " << pcs;
    repo.write(pcs);

    const auto read_pcs = repo.read_latest_by_party(system_party_id);
    for (const auto& written : pcs) {
        const auto it = std::ranges::find_if(read_pcs, [&](const party_currency& r) {
            return r.party_id == written.party_id &&
                   r.currency_iso_code == written.currency_iso_code;
        });
        REQUIRE(it != read_pcs.end());
        CHECK(it->change_commentary == written.change_commentary);
    }
}

TEST_CASE("read_latest_party_currencies_by_party", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;

    party_repository party_repo;
    currency_repository cur_repo;

    const auto system_party_id = find_system_party_id(party_repo, h.context(), h.tenant_id());
    h.set_party(system_party_id);
    party_currency_repository repo(h.context());

    auto gctx = ores::testing::make_generation_context(h);
    auto test_currency = generate_synthetic_currency(gctx);
    cur_repo.write(h.context(), test_currency);
    const auto& iso_code = test_currency.iso_code;

    auto pc = make_party_currency(h, system_party_id, iso_code);
    repo.write(pc);

    auto read_pcs = repo.read_latest_by_party(system_party_id);
    BOOST_LOG_SEV(lg, debug) << "Read party currencies: " << read_pcs;

    REQUIRE(!read_pcs.empty());
    bool found = false;
    for (const auto& r : read_pcs) {
        if (r.currency_iso_code == iso_code) {
            found = true;
            break;
        }
    }
    CHECK(found);
}

TEST_CASE("read_latest_party_currencies_by_currency", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;

    party_repository party_repo;
    currency_repository cur_repo;

    const auto system_party_id = find_system_party_id(party_repo, h.context(), h.tenant_id());
    h.set_party(system_party_id);
    party_currency_repository repo(h.context());

    auto gctx = ores::testing::make_generation_context(h);
    auto test_currency = generate_synthetic_currency(gctx);
    cur_repo.write(h.context(), test_currency);
    const auto& iso_code = test_currency.iso_code;

    auto pc = make_party_currency(h, system_party_id, iso_code);
    repo.write(pc);

    auto read_pcs = repo.read_latest_by_currency(iso_code);
    BOOST_LOG_SEV(lg, debug) << "Read party currencies: " << read_pcs;

    REQUIRE(read_pcs.size() >= 1);
    CHECK(read_pcs[0].party_id == system_party_id);
}

TEST_CASE("remove_party_currency", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;

    party_repository party_repo;
    currency_repository cur_repo;

    const auto system_party_id = find_system_party_id(party_repo, h.context(), h.tenant_id());
    h.set_party(system_party_id);
    party_currency_repository repo(h.context());

    auto gctx = ores::testing::make_generation_context(h);
    auto test_currencies = generate_unique_synthetic_currencies(2, gctx);
    cur_repo.write(h.context(), test_currencies);
    const auto& iso_code = test_currencies[0].iso_code;

    auto pc = make_party_currency(h, system_party_id, iso_code);
    repo.write(pc);
    const auto keeper = make_party_currency(h, system_party_id, test_currencies[1].iso_code);
    repo.write(keeper);

    repo.remove(system_party_id, iso_code);

    const auto removed_rows = repo.read_latest_by_currency(iso_code);
    const auto removed_still_present = std::ranges::any_of(
        removed_rows, [&](const party_currency& r) { return r.party_id == system_party_id; });
    CHECK_FALSE(removed_still_present);

    const auto keeper_rows = repo.read_latest_by_currency(keeper.currency_iso_code);
    REQUIRE(keeper_rows.size() == 1);
    CHECK(keeper_rows[0].party_id == keeper.party_id);
    CHECK(keeper_rows[0].change_commentary == keeper.change_commentary);
}

TEST_CASE("read_nonexistent_party_currency", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;

    party_repository party_repo;
    currency_repository cur_repo;

    // Write a row the read can answer with, so that an empty answer is the key
    // at work rather than a read that does nothing.
    const auto system_party_id = find_system_party_id(party_repo, h.context(), h.tenant_id());
    h.set_party(system_party_id);
    party_currency_repository repo(h.context());

    auto gctx = ores::testing::make_generation_context(h);
    auto test_currency = generate_synthetic_currency(gctx);
    cur_repo.write(h.context(), test_currency);
    const auto keeper = make_party_currency(h, system_party_id, test_currency.iso_code);
    repo.write(keeper);

    const auto nonexistent_id = boost::uuids::random_generator()();
    BOOST_LOG_SEV(lg, debug) << "Non-existent party ID: " << nonexistent_id;

    auto read_pcs = repo.read_latest(nonexistent_id, keeper.currency_iso_code);
    BOOST_LOG_SEV(lg, debug) << "Read party currencies: " << read_pcs;

    const auto answered_with_another_key = std::ranges::any_of(
        read_pcs, [&](const party_currency& r) { return r.party_id == nonexistent_id; });
    CHECK_FALSE(answered_with_another_key);

    // The same read does answer for the key pair that was written.
    const auto keeper_rows = repo.read_latest(keeper.party_id, keeper.currency_iso_code);
    REQUIRE(keeper_rows.size() == 1);
    CHECK(keeper_rows[0].currency_iso_code == keeper.currency_iso_code);
    CHECK(keeper_rows[0].change_commentary == keeper.change_commentary);
}
