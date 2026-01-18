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
#include "ores.synthetic/generators/account_generator.hpp"
#include "ores.synthetic/generators/catalog_generator.hpp"
#include "ores.synthetic/generators/dataset_generator.hpp"

#include <catch2/catch_test_macros.hpp>
#include "ores.logging/make_logger.hpp"

namespace {

const std::string test_suite("ores.synthetic.tests");
const std::string generators_tags("[generators]");

}

using namespace ores::synthetic::generators;
using namespace ores::logging;

TEST_CASE("generate_synthetic_account_creates_valid_account", generators_tags) {
    auto lg(make_logger(test_suite));
    auto account = generate_synthetic_account();

    CHECK(account.version == 1);
    CHECK(!account.id.is_nil());
    CHECK(!account.username.empty());
    CHECK(!account.email.empty());
    CHECK(!account.password_hash.empty());
    CHECK(!account.password_salt.empty());
    CHECK(!account.totp_secret.empty());
    CHECK(!account.recorded_by.empty());
    CHECK(account.change_reason_code == "SYNTHETIC");

    BOOST_LOG_SEV(lg, info) << "Generated account with username: "
                            << account.username;
}

TEST_CASE("generate_synthetic_accounts_creates_multiple", generators_tags) {
    auto lg(make_logger(test_suite));
    const std::size_t count = 5;
    auto accounts = generate_synthetic_accounts(count);

    CHECK(accounts.size() == count);

    for (const auto& account : accounts) {
        CHECK(!account.id.is_nil());
        CHECK(!account.username.empty());
    }

    BOOST_LOG_SEV(lg, info) << "Generated " << count << " accounts";
}

TEST_CASE("generate_synthetic_catalog_creates_valid_catalog", generators_tags) {
    auto lg(make_logger(test_suite));
    auto catalog = generate_synthetic_catalog();

    CHECK(catalog.version == 1);
    CHECK(!catalog.name.empty());
    CHECK(!catalog.description.empty());
    CHECK(!catalog.recorded_by.empty());
    CHECK(catalog.change_commentary == "Synthetic test data");

    BOOST_LOG_SEV(lg, info) << "Generated catalog: " << catalog.name;
}

TEST_CASE("generate_synthetic_catalogs_creates_multiple", generators_tags) {
    auto lg(make_logger(test_suite));
    const std::size_t count = 5;
    auto catalogs = generate_synthetic_catalogs(count);

    CHECK(catalogs.size() == count);

    for (const auto& catalog : catalogs) {
        CHECK(!catalog.name.empty());
    }

    BOOST_LOG_SEV(lg, info) << "Generated " << count << " catalogs";
}

TEST_CASE("generate_synthetic_dataset_creates_valid_dataset", generators_tags) {
    auto lg(make_logger(test_suite));
    auto dataset = generate_synthetic_dataset();

    CHECK(dataset.version == 1);
    CHECK(!dataset.id.is_nil());
    CHECK(!dataset.subject_area_name.empty());
    CHECK(!dataset.domain_name.empty());
    CHECK(!dataset.origin_code.empty());
    CHECK(!dataset.nature_code.empty());
    CHECK(!dataset.treatment_code.empty());
    CHECK(!dataset.name.empty());
    CHECK(!dataset.description.empty());
    CHECK(!dataset.source_system_id.empty());
    CHECK(!dataset.business_context.empty());
    CHECK(dataset.lineage_depth >= 0);
    CHECK(dataset.lineage_depth <= 5);
    CHECK(!dataset.recorded_by.empty());

    BOOST_LOG_SEV(lg, info) << "Generated dataset: " << dataset.name;
}

TEST_CASE("generate_synthetic_datasets_creates_multiple", generators_tags) {
    auto lg(make_logger(test_suite));
    const std::size_t count = 5;
    auto datasets = generate_synthetic_datasets(count);

    CHECK(datasets.size() == count);

    for (const auto& dataset : datasets) {
        CHECK(!dataset.id.is_nil());
        CHECK(!dataset.name.empty());
    }

    BOOST_LOG_SEV(lg, info) << "Generated " << count << " datasets";
}
