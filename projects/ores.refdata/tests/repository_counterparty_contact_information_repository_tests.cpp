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
#include "ores.refdata/repository/counterparty_contact_information_repository.hpp"

#include <catch2/catch_test_macros.hpp>
#include <boost/uuid/random_generator.hpp>
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include "ores.logging/make_logger.hpp"
#include "ores.testing/scoped_database_helper.hpp"
#include "ores.utility/streaming/std_vector.hpp" // IWYU pragma: keep.
#include "ores.refdata/domain/counterparty_contact_information.hpp" // IWYU pragma: keep.
#include "ores.refdata/domain/counterparty_contact_information_json_io.hpp" // IWYU pragma: keep.
#include "ores.refdata/generators/counterparty_contact_information_generator.hpp"

namespace {

const std::string_view test_suite("ores.refdata.tests");
const std::string tags("[repository]");

}

using namespace ores::refdata::generators;
using ores::refdata::domain::counterparty_contact_information;
using ores::refdata::repository::counterparty_contact_information_repository;
using ores::testing::scoped_database_helper;
using namespace ores::logging;

TEST_CASE("write_single_counterparty_contact_information", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    auto cci = generate_synthetic_counterparty_contact_information();
    cci.tenant_id = h.tenant_id().to_string();
    BOOST_LOG_SEV(lg, debug) << "Counterparty contact information: " << cci;

    counterparty_contact_information_repository repo(h.context());
    CHECK_NOTHROW(repo.write(cci));
}

TEST_CASE("write_multiple_counterparty_contact_informations", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    auto counterparty_contact_informations =
        generate_synthetic_counterparty_contact_informations(3);
    for (auto& cci : counterparty_contact_informations)
        cci.tenant_id = h.tenant_id().to_string();
    BOOST_LOG_SEV(lg, debug) << "Counterparty contact informations: "
                             << counterparty_contact_informations;

    counterparty_contact_information_repository repo(h.context());
    CHECK_NOTHROW(repo.write(counterparty_contact_informations));
}

TEST_CASE("read_latest_counterparty_contact_informations", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    auto written_counterparty_contact_informations =
        generate_synthetic_counterparty_contact_informations(3);
    for (auto& cci : written_counterparty_contact_informations)
        cci.tenant_id = h.tenant_id().to_string();
    BOOST_LOG_SEV(lg, debug) << "Written counterparty contact informations: "
                             << written_counterparty_contact_informations;

    counterparty_contact_information_repository repo(h.context());
    repo.write(written_counterparty_contact_informations);

    auto read_counterparty_contact_informations = repo.read_latest();
    BOOST_LOG_SEV(lg, debug) << "Read counterparty contact informations: "
                             << read_counterparty_contact_informations;

    CHECK(read_counterparty_contact_informations.size() >=
        written_counterparty_contact_informations.size());
}

TEST_CASE("read_latest_counterparty_contact_information_by_id", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    auto cci = generate_synthetic_counterparty_contact_information();
    cci.tenant_id = h.tenant_id().to_string();
    const auto original_city = cci.city;
    BOOST_LOG_SEV(lg, debug) << "Counterparty contact information: " << cci;

    counterparty_contact_information_repository repo(h.context());
    repo.write(cci);

    cci.city = original_city + " v2";
    repo.write(cci);

    auto read_counterparty_contact_informations = repo.read_latest(cci.id);
    BOOST_LOG_SEV(lg, debug) << "Read counterparty contact informations: "
                             << read_counterparty_contact_informations;

    REQUIRE(read_counterparty_contact_informations.size() == 1);
    CHECK(read_counterparty_contact_informations[0].id == cci.id);
    CHECK(read_counterparty_contact_informations[0].city ==
        original_city + " v2");
}

TEST_CASE("read_nonexistent_counterparty_contact_information_id", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    counterparty_contact_information_repository repo(h.context());

    const auto nonexistent_id = boost::uuids::random_generator()();
    BOOST_LOG_SEV(lg, debug) << "Non-existent ID: " << nonexistent_id;

    auto read_counterparty_contact_informations =
        repo.read_latest(nonexistent_id);
    BOOST_LOG_SEV(lg, debug) << "Read counterparty contact informations: "
                             << read_counterparty_contact_informations;

    CHECK(read_counterparty_contact_informations.size() == 0);
}
