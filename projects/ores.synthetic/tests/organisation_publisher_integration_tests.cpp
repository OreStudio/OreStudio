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
#include "ores.synthetic/service/organisation_generator_service.hpp"
#include "ores.synthetic/service/organisation_publisher_service.hpp"

#include <catch2/catch_test_macros.hpp>
#include "ores.logging/make_logger.hpp"
#include "ores.testing/scoped_database_helper.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.refdata/repository/party_repository.hpp"

namespace {

const std::string_view test_suite("ores.synthetic.tests");
const std::string tags("[organisation_publisher]");

using namespace ores::synthetic::service;
using namespace ores::synthetic::domain;
using ores::testing::scoped_database_helper;
using namespace ores::logging;

/**
 * @brief Seeds the minimum reference data needed for the organisation tree.
 *
 * The organisation generator produces parties and counterparties that reference
 * business centres (GBLO, GBED, USNY, etc.) and contact information with
 * country codes (GB, US). These must exist in the test tenant before insertion.
 */
void seed_reference_data(ores::database::context& ctx,
    const std::string& tenant_id, const std::string& country,
    logger_t& lg) {

    const auto seed_bc = [&](const std::string& code,
        const std::string& description) {
        const auto sql =
            "INSERT INTO ores_refdata_business_centres_tbl "
            "(code, tenant_id, version, coding_scheme_code, description, "
            "modified_by, performed_by, change_reason_code, change_commentary) "
            "VALUES ('" + code + "', '" + tenant_id + "'::uuid, 0, 'NONE', "
            "'" + description + "', current_user, current_user, "
            "'system.new_record', 'Test seed') "
            "ON CONFLICT DO NOTHING";
        ores::database::repository::execute_raw_command(
            ctx, sql, lg, "Seeding business centre " + code);
    };

    if (country == "GB") {
        seed_bc("GBLO", "London");
        seed_bc("GBED", "Edinburgh");
    } else {
        seed_bc("USNY", "New York");
        seed_bc("USCH", "Chicago");
        seed_bc("USBO", "Boston");
        seed_bc("USSF", "San Francisco");
    }

    const auto seed_country = [&](const std::string& alpha2,
        const std::string& alpha3, const std::string& numeric,
        const std::string& name, const std::string& official_name) {
        const auto sql =
            "INSERT INTO ores_refdata_countries_tbl "
            "(alpha2_code, tenant_id, version, alpha3_code, numeric_code, "
            "name, official_name, modified_by, performed_by, "
            "change_reason_code, change_commentary) "
            "VALUES ('" + alpha2 + "', '" + tenant_id + "'::uuid, 0, "
            "'" + alpha3 + "', '" + numeric + "', "
            "'" + name + "', '" + official_name + "', "
            "current_user, current_user, "
            "'system.new_record', 'Test seed') "
            "ON CONFLICT DO NOTHING";
        ores::database::repository::execute_raw_command(
            ctx, sql, lg, "Seeding country " + alpha2);
    };

    if (country == "GB") {
        seed_country("GB", "GBR", "826",
            "United Kingdom",
            "United Kingdom of Great Britain and Northern Ireland");
    } else {
        seed_country("US", "USA", "840",
            "United States", "United States of America");
    }
}

/**
 * @brief Stamps tenant_id and modified_by on all entities and parents the
 * root party under the system party.
 */
void prepare_for_publish(generated_organisation& org,
    const std::string& tenant_id, const std::string& modified_by,
    boost::uuids::uuid system_party_id, std::uint64_t seed) {

    // Use the seed as a prefix for short codes and names to ensure
    // uniqueness across multiple tests sharing the same tenant.
    const auto prefix = std::to_string(seed);

    const auto stamp = [&](auto& entities) {
        for (auto& e : entities) {
            e.tenant_id = tenant_id;
            e.modified_by = modified_by;
            e.performed_by = modified_by;
        }
    };
    stamp(org.parties);
    stamp(org.party_contacts);
    stamp(org.party_identifiers);
    stamp(org.counterparties);
    stamp(org.counterparty_contacts);
    stamp(org.counterparty_identifiers);
    stamp(org.party_counterparties);
    stamp(org.business_units);
    stamp(org.portfolios);
    stamp(org.books);

    // The tenant already has a system party as root. Parent the generated
    // root party under it to satisfy the root_party_uniq constraint.
    if (!org.parties.empty() && !org.parties[0].parent_party_id)
        org.parties[0].parent_party_id = system_party_id;

    // Prefix short codes to avoid cross-test collisions within the
    // shared test tenant.
    for (auto& p : org.parties)
        p.short_code = prefix + p.short_code;
    for (auto& c : org.counterparties)
        c.short_code = prefix + c.short_code;

    // Prefix portfolio and book names similarly.
    for (auto& p : org.portfolios)
        p.name = prefix + " " + p.name;
    for (auto& b : org.books)
        b.name = prefix + " " + b.name;

    // Prefix business unit names and codes.
    for (auto& bu : org.business_units) {
        bu.unit_name = prefix + " " + bu.unit_name;
        bu.unit_code = prefix + bu.unit_code;
    }
}

}

TEST_CASE("publish_gb_organisation_succeeds", tags) {
    auto lg(make_logger(test_suite));
    scoped_database_helper h;
    auto& ctx = h.context();
    const auto tid = h.tenant_id().to_string();
    const auto db_user = h.db_user();

    BOOST_LOG_SEV(lg, info) << "Seeding reference data for GB";
    seed_reference_data(ctx, tid, "GB", lg);

    ores::refdata::repository::party_repository repo(ctx);
    const auto system_party_id =
        repo.read_system_party(tid).at(0).id;

    organisation_generator_service gen_svc;
    organisation_generation_options opts;
    opts.seed = 100;
    opts.country = "GB";
    opts.party_count = 5;
    opts.counterparty_count = 10;
    opts.contacts_per_party = 1;
    opts.contacts_per_counterparty = 1;

    BOOST_LOG_SEV(lg, info) << "Generating GB organisation (seed "
                            << *opts.seed << ")";
    auto org = gen_svc.generate(opts);
    prepare_for_publish(org, tid, db_user, system_party_id, *opts.seed);

    BOOST_LOG_SEV(lg, info) << "Publishing organisation";
    organisation_publisher_service pub_svc(ctx);
    auto response = pub_svc.publish(org);

    if (!response.success) {
        BOOST_LOG_SEV(lg, error) << "Publisher error: "
                                 << response.error_message;
        FAIL("Publisher error: " << response.error_message);
    }
    REQUIRE(response.success);
    CHECK(response.parties_count == 5);
    CHECK(response.counterparties_count == 10);
    CHECK(response.portfolios_count > 0);
    CHECK(response.books_count > 0);
    CHECK(response.business_units_count > 0);
    CHECK(response.contacts_count > 0);
    CHECK(response.identifiers_count > 0);
}

TEST_CASE("publish_us_organisation_succeeds", tags) {
    auto lg(make_logger(test_suite));
    scoped_database_helper h;
    auto& ctx = h.context();
    const auto tid = h.tenant_id().to_string();
    const auto db_user = h.db_user();

    BOOST_LOG_SEV(lg, info) << "Seeding reference data for US";
    seed_reference_data(ctx, tid, "US", lg);

    ores::refdata::repository::party_repository repo(ctx);
    const auto system_party_id =
        repo.read_system_party(tid).at(0).id;

    organisation_generator_service gen_svc;
    organisation_generation_options opts;
    opts.seed = 200;
    opts.country = "US";
    opts.party_count = 3;
    opts.counterparty_count = 5;
    opts.contacts_per_party = 2;
    opts.contacts_per_counterparty = 1;

    BOOST_LOG_SEV(lg, info) << "Generating US organisation (seed "
                            << *opts.seed << ")";
    auto org = gen_svc.generate(opts);
    prepare_for_publish(org, tid, db_user, system_party_id, *opts.seed);

    BOOST_LOG_SEV(lg, info) << "Publishing organisation";
    organisation_publisher_service pub_svc(ctx);
    auto response = pub_svc.publish(org);

    if (!response.success) {
        BOOST_LOG_SEV(lg, error) << "Publisher error: "
                                 << response.error_message;
        FAIL("Publisher error: " << response.error_message);
    }
    REQUIRE(response.success);
    CHECK(response.parties_count == 3);
    CHECK(response.counterparties_count == 5);
}

TEST_CASE("publish_organisation_without_addresses_succeeds", tags) {
    auto lg(make_logger(test_suite));
    scoped_database_helper h;
    auto& ctx = h.context();
    const auto tid = h.tenant_id().to_string();
    const auto db_user = h.db_user();

    BOOST_LOG_SEV(lg, info) << "Seeding reference data for GB (no addresses)";
    seed_reference_data(ctx, tid, "GB", lg);

    ores::refdata::repository::party_repository repo(ctx);
    const auto system_party_id =
        repo.read_system_party(tid).at(0).id;

    organisation_generator_service gen_svc;
    organisation_generation_options opts;
    opts.seed = 300;
    opts.country = "GB";
    opts.generate_addresses = false;
    opts.generate_identifiers = false;
    opts.party_count = 3;
    opts.counterparty_count = 5;

    BOOST_LOG_SEV(lg, info) << "Generating GB organisation without "
                            << "addresses (seed " << *opts.seed << ")";
    auto org = gen_svc.generate(opts);
    prepare_for_publish(org, tid, db_user, system_party_id, *opts.seed);

    BOOST_LOG_SEV(lg, info) << "Publishing organisation";
    organisation_publisher_service pub_svc(ctx);
    auto response = pub_svc.publish(org);

    if (!response.success) {
        BOOST_LOG_SEV(lg, error) << "Publisher error: "
                                 << response.error_message;
        FAIL("Publisher error: " << response.error_message);
    }
    REQUIRE(response.success);
    CHECK(response.contacts_count == 0);
    CHECK(response.identifiers_count == 0);
    CHECK(response.parties_count == 3);
    CHECK(response.counterparties_count == 5);
}

TEST_CASE("publish_large_organisation_with_unique_short_codes", tags) {
    auto lg(make_logger(test_suite));
    scoped_database_helper h;
    auto& ctx = h.context();
    const auto tid = h.tenant_id().to_string();
    const auto db_user = h.db_user();

    BOOST_LOG_SEV(lg, info) << "Seeding reference data for GB (large)";
    seed_reference_data(ctx, tid, "GB", lg);

    ores::refdata::repository::party_repository repo(ctx);
    const auto system_party_id =
        repo.read_system_party(tid).at(0).id;

    organisation_generator_service gen_svc;
    organisation_generation_options opts;
    opts.seed = 400;
    opts.country = "GB";
    opts.party_count = 10;
    opts.counterparty_count = 20;
    opts.contacts_per_party = 2;
    opts.contacts_per_counterparty = 2;

    BOOST_LOG_SEV(lg, info) << "Generating large GB organisation (seed "
                            << *opts.seed << ")";
    auto org = gen_svc.generate(opts);
    prepare_for_publish(org, tid, db_user, system_party_id, *opts.seed);

    BOOST_LOG_SEV(lg, info) << "Publishing organisation";
    organisation_publisher_service pub_svc(ctx);
    auto response = pub_svc.publish(org);

    if (!response.success) {
        BOOST_LOG_SEV(lg, error) << "Publisher error: "
                                 << response.error_message;
        FAIL("Publisher error: " << response.error_message);
    }
    REQUIRE(response.success);
    CHECK(response.parties_count == 10);
    CHECK(response.counterparties_count == 20);
}
