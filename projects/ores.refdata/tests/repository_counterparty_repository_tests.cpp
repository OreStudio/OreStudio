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
#include "ores.refdata/repository/counterparty_repository.hpp"

#include <catch2/catch_test_macros.hpp>
#include <boost/uuid/random_generator.hpp>
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include "ores.logging/make_logger.hpp"
#include "ores.testing/scoped_database_helper.hpp"
#include "ores.testing/make_generation_context.hpp"
#include "ores.utility/streaming/std_vector.hpp" // IWYU pragma: keep.
#include "ores.refdata/domain/counterparty.hpp" // IWYU pragma: keep.
#include "ores.refdata/domain/counterparty_json_io.hpp" // IWYU pragma: keep.
#include "ores.refdata/generators/counterparty_generator.hpp"

namespace {

const std::string_view test_suite("ores.refdata.tests");
const std::string tags("[repository]");

}

using namespace ores::refdata::generators;
using ores::refdata::domain::counterparty;
using ores::refdata::repository::counterparty_repository;
using ores::testing::scoped_database_helper;
using namespace ores::logging;

TEST_CASE("write_single_counterparty", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    auto ctx = ores::testing::make_generation_context(h);
    auto cp = generate_synthetic_counterparty(ctx);
    cp.change_reason_code = "system.test";
    BOOST_LOG_SEV(lg, debug) << "Counterparty: " << cp;

    counterparty_repository repo(h.context());
    CHECK_NOTHROW(repo.write(cp));
}

TEST_CASE("write_multiple_counterparties", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    auto ctx = ores::testing::make_generation_context(h);
    auto counterparties = generate_synthetic_counterparties(3, ctx);
    for (auto& cp : counterparties) {
        cp.change_reason_code = "system.test";
    }
    BOOST_LOG_SEV(lg, debug) << "Counterparties: " << counterparties;

    counterparty_repository repo(h.context());
    CHECK_NOTHROW(repo.write(counterparties));
}

TEST_CASE("read_latest_counterparties", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    auto ctx = ores::testing::make_generation_context(h);
    auto written_counterparties = generate_synthetic_counterparties(3, ctx);
    for (auto& cp : written_counterparties) {
        cp.change_reason_code = "system.test";
    }
    BOOST_LOG_SEV(lg, debug) << "Written counterparties: "
                             << written_counterparties;

    counterparty_repository repo(h.context());
    repo.write(written_counterparties);

    auto read_counterparties = repo.read_latest();
    BOOST_LOG_SEV(lg, debug) << "Read counterparties: "
                             << read_counterparties;

    CHECK(read_counterparties.size() >= written_counterparties.size());
}

TEST_CASE("read_latest_counterparty_by_id", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    auto ctx = ores::testing::make_generation_context(h);
    auto cp = generate_synthetic_counterparty(ctx);
    cp.change_reason_code = "system.test";
    const auto original_full_name = cp.full_name;
    BOOST_LOG_SEV(lg, debug) << "Counterparty: " << cp;

    counterparty_repository repo(h.context());
    repo.write(cp);

    cp.full_name = original_full_name + " v2";
    repo.write(cp);

    auto read_counterparties = repo.read_latest(cp.id);
    BOOST_LOG_SEV(lg, debug) << "Read counterparties: "
                             << read_counterparties;

    REQUIRE(read_counterparties.size() == 1);
    CHECK(read_counterparties[0].id == cp.id);
    CHECK(read_counterparties[0].full_name == original_full_name + " v2");
}

TEST_CASE("read_nonexistent_counterparty_id", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    counterparty_repository repo(h.context());

    const auto nonexistent_id = boost::uuids::random_generator()();
    BOOST_LOG_SEV(lg, debug) << "Non-existent ID: " << nonexistent_id;

    auto read_counterparties = repo.read_latest(nonexistent_id);
    BOOST_LOG_SEV(lg, debug) << "Read counterparties: "
                             << read_counterparties;

    CHECK(read_counterparties.size() == 0);
}
