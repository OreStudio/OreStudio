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
#include "ores.refdata/repository/counterparty_identifier_repository.hpp"

#include <catch2/catch_test_macros.hpp>
#include <boost/uuid/random_generator.hpp>
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include "ores.logging/make_logger.hpp"
#include "ores.testing/scoped_database_helper.hpp"
#include "ores.utility/streaming/std_vector.hpp" // IWYU pragma: keep.
#include "ores.refdata/domain/counterparty_identifier.hpp" // IWYU pragma: keep.
#include "ores.refdata/domain/counterparty_identifier_json_io.hpp" // IWYU pragma: keep.
#include "ores.refdata/generators/counterparty_identifier_generator.hpp"

namespace {

const std::string_view test_suite("ores.refdata.tests");
const std::string tags("[repository]");

}

using namespace ores::refdata::generators;
using ores::refdata::domain::counterparty_identifier;
using ores::refdata::repository::counterparty_identifier_repository;
using ores::testing::scoped_database_helper;
using namespace ores::logging;

TEST_CASE("write_single_counterparty_identifier", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    auto ci = generate_synthetic_counterparty_identifier();
    ci.tenant_id = h.tenant_id().to_string();
    BOOST_LOG_SEV(lg, debug) << "Counterparty identifier: " << ci;

    counterparty_identifier_repository repo(h.context());
    CHECK_NOTHROW(repo.write(ci));
}

TEST_CASE("write_multiple_counterparty_identifiers", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    auto counterparty_identifiers =
        generate_synthetic_counterparty_identifiers(3);
    for (auto& ci : counterparty_identifiers)
        ci.tenant_id = h.tenant_id().to_string();
    BOOST_LOG_SEV(lg, debug) << "Counterparty identifiers: "
                             << counterparty_identifiers;

    counterparty_identifier_repository repo(h.context());
    CHECK_NOTHROW(repo.write(counterparty_identifiers));
}

TEST_CASE("read_latest_counterparty_identifiers", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    auto written_counterparty_identifiers =
        generate_synthetic_counterparty_identifiers(3);
    for (auto& ci : written_counterparty_identifiers)
        ci.tenant_id = h.tenant_id().to_string();
    BOOST_LOG_SEV(lg, debug) << "Written counterparty identifiers: "
                             << written_counterparty_identifiers;

    counterparty_identifier_repository repo(h.context());
    repo.write(written_counterparty_identifiers);

    auto read_counterparty_identifiers = repo.read_latest();
    BOOST_LOG_SEV(lg, debug) << "Read counterparty identifiers: "
                             << read_counterparty_identifiers;

    CHECK(read_counterparty_identifiers.size() >=
        written_counterparty_identifiers.size());
}

TEST_CASE("read_latest_counterparty_identifier_by_id", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    auto ci = generate_synthetic_counterparty_identifier();
    ci.tenant_id = h.tenant_id().to_string();
    const auto original_id_value = ci.id_value;
    BOOST_LOG_SEV(lg, debug) << "Counterparty identifier: " << ci;

    counterparty_identifier_repository repo(h.context());
    repo.write(ci);

    ci.id_value = original_id_value + "_v2";
    repo.write(ci);

    auto read_counterparty_identifiers = repo.read_latest(ci.id);
    BOOST_LOG_SEV(lg, debug) << "Read counterparty identifiers: "
                             << read_counterparty_identifiers;

    REQUIRE(read_counterparty_identifiers.size() == 1);
    CHECK(read_counterparty_identifiers[0].id == ci.id);
    CHECK(read_counterparty_identifiers[0].id_value ==
        original_id_value + "_v2");
}

TEST_CASE("read_nonexistent_counterparty_identifier_id", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    counterparty_identifier_repository repo(h.context());

    const auto nonexistent_id = boost::uuids::random_generator()();
    BOOST_LOG_SEV(lg, debug) << "Non-existent ID: " << nonexistent_id;

    auto read_counterparty_identifiers = repo.read_latest(nonexistent_id);
    BOOST_LOG_SEV(lg, debug) << "Read counterparty identifiers: "
                             << read_counterparty_identifiers;

    CHECK(read_counterparty_identifiers.size() == 0);
}
