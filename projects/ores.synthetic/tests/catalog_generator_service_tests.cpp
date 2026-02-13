/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2025 Marco Craveiro <marco.craveiro@gmail.com>
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
#include "ores.synthetic/service/catalog_generator_service.hpp"

#include <set>
#include <catch2/catch_test_macros.hpp>

namespace {

const std::string tags("[catalog_generator_service]");

using namespace ores::synthetic::service;
using namespace ores::synthetic::domain;

}

TEST_CASE("generate_with_defaults_produces_nonempty_catalog", tags) {
    catalog_generator_service svc;
    auto result = svc.generate();

    REQUIRE(!result.accounts.empty());
    REQUIRE(!result.catalogs.empty());
    REQUIRE(!result.data_domains.empty());
    REQUIRE(!result.subject_areas.empty());
    REQUIRE(!result.origin_dimensions.empty());
    REQUIRE(!result.nature_dimensions.empty());
    REQUIRE(!result.treatment_dimensions.empty());
    REQUIRE(!result.datasets.empty());
}

TEST_CASE("generate_respects_account_count", tags) {
    catalog_generator_service svc;
    generation_options opts;
    opts.seed = 42;
    opts.account_count = 3;

    auto result = svc.generate(opts);
    REQUIRE(result.accounts.size() == 3);
}

TEST_CASE("generate_respects_catalog_count", tags) {
    catalog_generator_service svc;
    generation_options opts;
    opts.seed = 42;
    opts.catalog_count = 2;

    auto result = svc.generate(opts);
    REQUIRE(result.catalogs.size() == 2);
}

TEST_CASE("generate_respects_dataset_count", tags) {
    catalog_generator_service svc;
    generation_options opts;
    opts.seed = 42;
    opts.dataset_count = 10;

    auto result = svc.generate(opts);
    REQUIRE(result.datasets.size() == 10);
}

TEST_CASE("generate_with_seed_is_reproducible", tags) {
    catalog_generator_service svc;
    generation_options opts;
    opts.seed = 42;

    auto result1 = svc.generate(opts);
    auto result2 = svc.generate(opts);

    REQUIRE(result1.seed == result2.seed);
    REQUIRE(result1.accounts.size() == result2.accounts.size());
    REQUIRE(result1.datasets.size() == result2.datasets.size());

    for (std::size_t i = 0; i < result1.accounts.size(); ++i) {
        REQUIRE(result1.accounts[i].username == result2.accounts[i].username);
        REQUIRE(result1.accounts[i].email == result2.accounts[i].email);
    }

    for (std::size_t i = 0; i < result1.datasets.size(); ++i) {
        REQUIRE(result1.datasets[i].name == result2.datasets[i].name);
    }
}

TEST_CASE("generate_stores_seed_in_result", tags) {
    catalog_generator_service svc;
    generation_options opts;
    opts.seed = 777;

    auto result = svc.generate(opts);
    REQUIRE(result.seed == 777);
}

TEST_CASE("generate_accounts_have_valid_fields", tags) {
    catalog_generator_service svc;
    generation_options opts;
    opts.seed = 42;

    auto result = svc.generate(opts);
    for (const auto& acc : result.accounts) {
        CHECK(acc.version == 1);
        CHECK(!acc.id.is_nil());
        CHECK(!acc.username.empty());
        CHECK(!acc.email.empty());
        CHECK(!acc.password_hash.empty());
        CHECK(!acc.password_salt.empty());
    }
}

TEST_CASE("generate_datasets_reference_existing_entities", tags) {
    catalog_generator_service svc;
    generation_options opts;
    opts.seed = 42;

    auto result = svc.generate(opts);

    // Collect valid entity names
    std::set<std::string> account_names;
    for (const auto& acc : result.accounts)
        account_names.insert(acc.username);

    std::set<std::string> domain_names;
    for (const auto& dom : result.data_domains)
        domain_names.insert(dom.name);

    std::set<std::string> origin_codes;
    for (const auto& od : result.origin_dimensions)
        origin_codes.insert(od.code);

    std::set<std::string> nature_codes;
    for (const auto& nd : result.nature_dimensions)
        nature_codes.insert(nd.code);

    std::set<std::string> treatment_codes;
    for (const auto& td : result.treatment_dimensions)
        treatment_codes.insert(td.code);

    for (const auto& ds : result.datasets) {
        CHECK(!ds.name.empty());
        CHECK(!ds.modified_by.empty());
        CHECK(account_names.count(ds.modified_by) == 1);
        CHECK(!ds.domain_name.empty());
        CHECK(!ds.origin_code.empty());
        CHECK(origin_codes.count(ds.origin_code) == 1);
        CHECK(!ds.nature_code.empty());
        CHECK(nature_codes.count(ds.nature_code) == 1);
        CHECK(!ds.treatment_code.empty());
        CHECK(treatment_codes.count(ds.treatment_code) == 1);
    }
}

TEST_CASE("generate_subject_areas_linked_to_domains", tags) {
    catalog_generator_service svc;
    generation_options opts;
    opts.seed = 42;

    auto result = svc.generate(opts);

    std::set<std::string> domain_names;
    for (const auto& dom : result.data_domains)
        domain_names.insert(dom.name);

    for (const auto& sa : result.subject_areas) {
        CHECK(!sa.name.empty());
        CHECK(!sa.domain_name.empty());
        CHECK(domain_names.count(sa.domain_name) == 1);
    }
}

TEST_CASE("generate_stamps_dependencies", tags) {
    catalog_generator_service svc;
    generation_options opts;
    opts.seed = 42;
    opts.dependencies = {"ISO Reference Data", "Core DQ Dimensions"};

    auto result = svc.generate(opts);
    REQUIRE(result.dependencies.size() == 2);
    CHECK(result.dependencies[0] == "ISO Reference Data");
    CHECK(result.dependencies[1] == "Core DQ Dimensions");
}

TEST_CASE("generate_stamps_methodology_id_on_datasets", tags) {
    catalog_generator_service svc;
    generation_options opts;
    opts.seed = 42;
    opts.methodology_id = boost::uuids::uuid{{
        0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08,
        0x09, 0x0a, 0x0b, 0x0c, 0x0d, 0x0e, 0x0f, 0x10
    }};

    auto result = svc.generate(opts);
    for (const auto& ds : result.datasets) {
        CHECK(ds.methodology_id == *opts.methodology_id);
    }
}

TEST_CASE("generate_clamps_counts_to_predefined_data_size", tags) {
    catalog_generator_service svc;
    generation_options opts;
    opts.seed = 42;
    opts.account_count = 100; // Only 10 predefined accounts
    opts.catalog_count = 100; // Only 8 predefined catalogs

    auto result = svc.generate(opts);
    CHECK(result.accounts.size() == 10);
    CHECK(result.catalogs.size() == 8);
}
