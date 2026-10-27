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
#include "ores.history.api/messaging/history_protocol.hpp"
#include "ores.history.core/service/dispatch_registry.hpp"
#include "ores.logging/boost_severity.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.refdata.api/domain/country.hpp"
#include "ores.refdata.api/domain/party_country.hpp"
#include "ores.refdata.api/domain/party_country_json_io.hpp" // IWYU pragma: keep.
#include "ores.refdata.api/generators/country_generator.hpp"
#include "ores.refdata.core/messaging/party_country_history_provider_registrar.hpp"
#include "ores.refdata.core/presentation/party_country_history_field_mapper.hpp"
#include "ores.refdata.core/repository/country_repository.hpp"
#include "ores.refdata.core/repository/party_country_repository.hpp"
#include "ores.refdata.core/repository/party_repository.hpp"
#include "ores.testing/make_generation_context.hpp"
#include "ores.testing/scoped_database_helper.hpp"
#include "ores.utility/rfl/reflectors.hpp"       // IWYU pragma: keep.
#include "ores.utility/streaming/std_vector.hpp" // IWYU pragma: keep.
#include "ores.utility/uuid/tenant_id.hpp"
#include "test_suite.hpp"
#include <boost/log/sources/severity_feature.hpp>
#include <boost/uuid/random_generator.hpp>
#include <boost/uuid/uuid.hpp>
#include <algorithm>
#include <catch2/catch_test_macros.hpp>
#include <cstddef>
#include <stdexcept>
#include <string>
#include <string_view>
#include <vector>

using namespace ores::logging;
using namespace ores::refdata::generators;

using ores::refdata::domain::party_country;
using ores::refdata::repository::party_country_repository;
using ores::refdata::repository::party_repository;
using ores::refdata::repository::country_repository;
using ores::testing::scoped_database_helper;

namespace {

// Total number of test cases that write party_country records.
// Each test uses a different slice of the fictional countries list
// to avoid interfering with records written by other tests in the
// same shared tenant database.
constexpr std::size_t total_slots = 12;

boost::uuids::uuid find_system_party_id(party_repository& repo,
                                        ores::database::context ctx,
                                        const ores::utility::uuid::tenant_id& tid) {
    auto parties = repo.read_latest(ctx);
    for (const auto& p : parties)
        if (p.tenant_id == tid && p.party_category == "System")
            return p.id;
    throw std::runtime_error("No system party for tenant: " + tid.to_string());
}

party_country make_party_country(scoped_database_helper& h,
                                 const boost::uuids::uuid& party_id,
                                 const std::string& alpha2_code) {
    party_country pc;
    pc.tenant_id = h.tenant_id().to_string();
    pc.party_id = party_id;
    pc.country_alpha2_code = alpha2_code;
    pc.modified_by = h.db_user();
    pc.change_reason_code = "system.test";
    pc.change_commentary = "Synthetic test data";
    pc.performed_by = h.db_user();
    return pc;
}

}

TEST_CASE("write_single_party_country", "[repository][party_country]") {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;

    party_repository party_repo;
    country_repository cty_repo;

    const auto party_id = find_system_party_id(party_repo, h.context(), h.tenant_id());
    h.set_party(party_id);
    party_country_repository repo(h.context());

    auto gctx = ores::testing::make_generation_context(h);
    auto all = generate_fictional_countries(total_slots, gctx);
    cty_repo.write(h.context(), {all[0]});
    const auto& alpha2_code = all[0].alpha2_code;

    auto pc = make_party_country(h, party_id, alpha2_code);
    BOOST_LOG_SEV(lg, debug) << "Party country: " << pc;
    repo.write(pc);

    const auto read_pcs = repo.read_latest(pc.party_id, pc.country_alpha2_code);
    REQUIRE(read_pcs.size() == 1);
    CHECK(read_pcs[0].party_id == pc.party_id);
    CHECK(read_pcs[0].country_alpha2_code == pc.country_alpha2_code);
    CHECK(read_pcs[0].change_commentary == pc.change_commentary);
}

TEST_CASE("write_multiple_party_countries", "[repository][party_country]") {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;

    party_repository party_repo;
    country_repository cty_repo;

    const auto system_party_id = find_system_party_id(party_repo, h.context(), h.tenant_id());
    h.set_party(system_party_id);
    party_country_repository repo(h.context());

    auto gctx = ores::testing::make_generation_context(h);
    auto all = generate_fictional_countries(total_slots, gctx);
    std::vector<ores::refdata::domain::country> test_countries = {all[1], all[2]};
    cty_repo.write(h.context(), test_countries);

    std::vector<party_country> pcs;
    for (const auto& c : test_countries) {
        pcs.push_back(make_party_country(h, system_party_id, c.alpha2_code));
    }

    BOOST_LOG_SEV(lg, debug) << "Party countries: " << pcs;
    repo.write(pcs);

    const auto read_pcs = repo.read_latest_by_party(system_party_id);
    for (const auto& written : pcs) {
        const auto it = std::ranges::find_if(read_pcs, [&](const party_country& r) {
            return r.party_id == written.party_id &&
                   r.country_alpha2_code == written.country_alpha2_code;
        });
        REQUIRE(it != read_pcs.end());
        CHECK(it->change_commentary == written.change_commentary);
    }
}

TEST_CASE("read_latest_party_countries_by_party", "[repository][party_country]") {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;

    party_repository party_repo;
    country_repository cty_repo;

    const auto system_party_id = find_system_party_id(party_repo, h.context(), h.tenant_id());
    h.set_party(system_party_id);
    party_country_repository repo(h.context());

    auto gctx = ores::testing::make_generation_context(h);
    auto all = generate_fictional_countries(total_slots, gctx);
    cty_repo.write(h.context(), {all[3]});
    const auto& alpha2_code = all[3].alpha2_code;

    auto pc = make_party_country(h, system_party_id, alpha2_code);
    repo.write(pc);

    auto read_pcs = repo.read_latest_by_party(system_party_id);
    BOOST_LOG_SEV(lg, debug) << "Read party countries: " << read_pcs;

    REQUIRE(!read_pcs.empty());
    bool found = false;
    for (const auto& r : read_pcs) {
        if (r.country_alpha2_code == alpha2_code) {
            found = true;
            break;
        }
    }
    CHECK(found);
}

TEST_CASE("read_latest_party_countries_by_country", "[repository][party_country]") {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;

    party_repository party_repo;
    country_repository cty_repo;

    const auto system_party_id = find_system_party_id(party_repo, h.context(), h.tenant_id());
    h.set_party(system_party_id);
    party_country_repository repo(h.context());

    auto gctx = ores::testing::make_generation_context(h);
    auto all = generate_fictional_countries(total_slots, gctx);
    cty_repo.write(h.context(), {all[4]});
    const auto& alpha2_code = all[4].alpha2_code;

    auto pc = make_party_country(h, system_party_id, alpha2_code);
    repo.write(pc);

    auto read_pcs = repo.read_latest_by_country(alpha2_code);
    BOOST_LOG_SEV(lg, debug) << "Read party countries: " << read_pcs;

    REQUIRE(read_pcs.size() >= 1);
    CHECK(read_pcs[0].party_id == system_party_id);
}

TEST_CASE("remove_party_country", "[repository][party_country]") {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;

    party_repository party_repo;
    country_repository cty_repo;

    const auto system_party_id = find_system_party_id(party_repo, h.context(), h.tenant_id());
    h.set_party(system_party_id);
    party_country_repository repo(h.context());

    auto gctx = ores::testing::make_generation_context(h);
    auto all = generate_fictional_countries(total_slots, gctx);
    cty_repo.write(h.context(), {all[5], all[6]});
    const auto& alpha2_code = all[5].alpha2_code;

    auto pc = make_party_country(h, system_party_id, alpha2_code);
    repo.write(pc);
    const auto keeper = make_party_country(h, system_party_id, all[6].alpha2_code);
    repo.write(keeper);

    repo.remove(system_party_id, alpha2_code);

    const auto removed_rows = repo.read_latest_by_country(alpha2_code);
    const auto removed_still_present = std::ranges::any_of(
        removed_rows, [&](const party_country& r) { return r.party_id == system_party_id; });
    CHECK_FALSE(removed_still_present);

    const auto keeper_rows = repo.read_latest_by_country(keeper.country_alpha2_code);
    REQUIRE(keeper_rows.size() == 1);
    CHECK(keeper_rows[0].party_id == keeper.party_id);
    CHECK(keeper_rows[0].change_commentary == keeper.change_commentary);
}

TEST_CASE("read_nonexistent_party_country", "[repository][party_country]") {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;

    party_repository party_repo;
    country_repository cty_repo;

    // Write a row the read can answer with, so that an empty answer is the key
    // at work rather than a read that does nothing.
    const auto system_party_id = find_system_party_id(party_repo, h.context(), h.tenant_id());
    h.set_party(system_party_id);
    party_country_repository repo(h.context());

    auto gctx = ores::testing::make_generation_context(h);
    auto all = generate_fictional_countries(total_slots, gctx);
    cty_repo.write(h.context(), {all[7]});
    const auto keeper = make_party_country(h, system_party_id, all[7].alpha2_code);
    repo.write(keeper);

    const auto nonexistent_id = boost::uuids::random_generator()();
    BOOST_LOG_SEV(lg, debug) << "Non-existent party ID: " << nonexistent_id;

    auto read_pcs = repo.read_latest(nonexistent_id, keeper.country_alpha2_code);
    BOOST_LOG_SEV(lg, debug) << "Read party countries: " << read_pcs;

    const auto answered_with_another_key = std::ranges::any_of(
        read_pcs, [&](const party_country& r) { return r.party_id == nonexistent_id; });
    CHECK_FALSE(answered_with_another_key);

    // The same read does answer for the key pair that was written.
    const auto keeper_rows = repo.read_latest(keeper.party_id, keeper.country_alpha2_code);
    REQUIRE(keeper_rows.size() == 1);
    CHECK(keeper_rows[0].country_alpha2_code == keeper.country_alpha2_code);
    CHECK(keeper_rows[0].change_commentary == keeper.change_commentary);
}

TEST_CASE("a_closed_party_country_is_in_its_history_with_the_time_it_ended", "[repository][party_country]") {
    scoped_database_helper h;

    party_repository party_repo;
    country_repository cty_repo;

    const auto party_id = find_system_party_id(party_repo, h.context(), h.tenant_id());
    h.set_party(party_id);
    party_country_repository repo(h.context());

    auto gctx = ores::testing::make_generation_context(h);
    auto all = generate_fictional_countries(total_slots, gctx);
    cty_repo.write(h.context(), {all[8], all[9]});

    repo.write(make_party_country(h, party_id, all[8].alpha2_code));
    repo.write(make_party_country(h, party_id, all[9].alpha2_code));
    repo.remove(party_id, all[8].alpha2_code);

    // The live reads no longer return the closed link; the history does.
    CHECK(repo.read_latest(party_id, all[8].alpha2_code).empty());
    const auto history = repo.read_all_by_party(party_id);

    const auto closed = std::ranges::find_if(history, [&](const auto& row) {
        return row.link.country_alpha2_code == all[8].alpha2_code;
    });
    REQUIRE(closed != history.end());
    CHECK(closed->closed_at.has_value());

    const auto open = std::ranges::find_if(history, [&](const auto& row) {
        return row.link.country_alpha2_code == all[9].alpha2_code;
    });
    REQUIRE(open != history.end());
    CHECK_FALSE(open->closed_at.has_value());
}

TEST_CASE("a_party_country_history_reads_only_its_own_party", "[repository][party_country]") {
    scoped_database_helper h;

    party_repository party_repo;
    country_repository cty_repo;

    const auto party_id = find_system_party_id(party_repo, h.context(), h.tenant_id());
    h.set_party(party_id);
    party_country_repository repo(h.context());

    auto gctx = ores::testing::make_generation_context(h);
    auto all = generate_fictional_countries(total_slots, gctx);
    cty_repo.write(h.context(), {all[10]});
    repo.write(make_party_country(h, party_id, all[10].alpha2_code));

    boost::uuids::random_generator generate;
    CHECK(repo.read_all_by_party(generate()).empty());
}

TEST_CASE("the_party_country_history_provider_numbers_each_link_on_its_own", "[repository][party_country]") {
    scoped_database_helper h;

    party_repository party_repo;
    country_repository cty_repo;

    const auto party_id = find_system_party_id(party_repo, h.context(), h.tenant_id());
    h.set_party(party_id);
    party_country_repository repo(h.context());

    auto gctx = ores::testing::make_generation_context(h);
    auto all = generate_fictional_countries(total_slots, gctx);
    cty_repo.write(h.context(), {all[11]});

    // Open the link, replace it once, and close it: two versions of one link.
    auto link = make_party_country(h, party_id, all[11].alpha2_code);
    repo.write(link);
    link.version = repo.read_latest(party_id, all[11].alpha2_code).front().version;
    link.change_commentary = "Changed once";
    repo.write(link);
    repo.remove(party_id, all[11].alpha2_code);

    ores::history::service::dispatch_registry registry;
    ores::refdata::messaging::register_party_country_history_provider(registry);
    REQUIRE(registry.has_provider("ores.refdata.party_country"));

    ores::history::messaging::get_entity_history_request request;
    request.entity_type = "ores.refdata.party_country";
    request.entity_id = boost::uuids::to_string(party_id);
    const auto response =
        registry.dispatch(request, h.context().with_roles({"refdata::party_countries:read"}));
    REQUIRE(response.success);

    // Only this link's versions: the others of the party are numbered apart.
    std::vector<ores::history::messaging::entity_history_version> own;
    const std::string wanted = boost::uuids::to_string(party_id) + "/" + all[11].alpha2_code;
    for (const auto& version : response.versions) {
        const auto id =
            std::ranges::find_if(version.fields, [](const auto& f) { return f.name == "ID"; });
        if (id != version.fields.end() && id->value == wanted)
            own.push_back(version);
    }
    REQUIRE(own.size() == 2);
    CHECK(own[0].version == 2);
    CHECK(own[1].version == 1);

    const auto closed_at = [](const auto& version) {
        return std::ranges::find_if(version.fields,
                                    [](const auto& f) { return f.name == "Closed At"; })
            ->value;
    };
    CHECK_FALSE(closed_at(own[0]).empty());
    CHECK(closed_at(own[1]).empty());
}
