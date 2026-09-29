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
#include "ores.logging/make_logger.hpp"
#include "ores.refdata.api/domain/counterparty_identifier.hpp"         // IWYU pragma: keep.
#include "ores.refdata.api/domain/counterparty_identifier_json_io.hpp" // IWYU pragma: keep.
#include "ores.refdata.api/generators/counterparty_generator.hpp"
#include "ores.refdata.api/generators/counterparty_identifier_generator.hpp"
#include "ores.refdata.core/repository/counterparty_identifier_repository.hpp"
#include "ores.refdata.core/repository/counterparty_repository.hpp"
#include "ores.testing/make_generation_context.hpp"
#include "ores.testing/scoped_database_helper.hpp"
#include "ores.utility/rfl/reflectors.hpp"       // IWYU pragma: keep.
#include "ores.utility/streaming/std_vector.hpp" // IWYU pragma: keep.
#include <boost/uuid/random_generator.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <algorithm>
#include <catch2/catch_test_macros.hpp>

namespace {

const std::string_view test_suite("ores.refdata.tests");
const std::string tags("[repository]");

}

using namespace ores::refdata::generators;
using ores::refdata::domain::counterparty_identifier;
using ores::refdata::repository::counterparty_identifier_repository;
using ores::refdata::repository::counterparty_repository;
using ores::testing::scoped_database_helper;
using namespace ores::logging;

TEST_CASE("write_single_counterparty_identifier", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    auto ctx = ores::testing::make_generation_context(h);
    auto cp = generate_synthetic_counterparty(ctx);
    cp.change_reason_code = "system.test";
    counterparty_repository cp_repo;
    cp_repo.write(h.context(), cp);

    auto ci = generate_synthetic_counterparty_identifier(ctx);
    ci.change_reason_code = "system.test";
    ci.counterparty_id = cp.id;
    BOOST_LOG_SEV(lg, debug) << "Counterparty identifier: " << ci;

    counterparty_identifier_repository repo;
    repo.write(h.context(), ci);

    const auto read_counterparty_identifiers =
        repo.read_latest(h.context(), boost::uuids::to_string(ci.id));
    REQUIRE(read_counterparty_identifiers.size() == 1);
    CHECK(read_counterparty_identifiers[0].id_value == ci.id_value);
}

TEST_CASE("write_multiple_counterparty_identifiers", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    auto ctx = ores::testing::make_generation_context(h);
    auto cp = generate_synthetic_counterparty(ctx);
    cp.change_reason_code = "system.test";
    counterparty_repository cp_repo;
    cp_repo.write(h.context(), cp);

    auto counterparty_identifiers = generate_synthetic_counterparty_identifiers(3, ctx);
    for (auto& ci : counterparty_identifiers) {
        ci.change_reason_code = "system.test";
        ci.counterparty_id = cp.id;
    }
    BOOST_LOG_SEV(lg, debug) << "Counterparty identifiers: " << counterparty_identifiers;

    counterparty_identifier_repository repo;
    repo.write(h.context(), counterparty_identifiers);

    const auto read_counterparty_identifiers = repo.read_latest(h.context());
    for (const auto& written : counterparty_identifiers) {
        const auto it = std::ranges::find_if(
            read_counterparty_identifiers,
            [&](const counterparty_identifier& c) { return c.id == written.id; });
        REQUIRE(it != read_counterparty_identifiers.end());
        CHECK(it->id_value == written.id_value);
    }
}

TEST_CASE("read_latest_counterparty_identifiers", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    auto ctx = ores::testing::make_generation_context(h);
    auto cp = generate_synthetic_counterparty(ctx);
    cp.change_reason_code = "system.test";
    counterparty_repository cp_repo;
    cp_repo.write(h.context(), cp);

    auto written_counterparty_identifiers = generate_synthetic_counterparty_identifiers(3, ctx);
    for (auto& ci : written_counterparty_identifiers) {
        ci.change_reason_code = "system.test";
        ci.counterparty_id = cp.id;
    }
    BOOST_LOG_SEV(lg, debug) << "Written counterparty identifiers: "
                             << written_counterparty_identifiers;

    counterparty_identifier_repository repo;
    repo.write(h.context(), written_counterparty_identifiers);

    auto read_counterparty_identifiers = repo.read_latest(h.context());
    BOOST_LOG_SEV(lg, debug) << "Read counterparty identifiers: " << read_counterparty_identifiers;

    for (const auto& written : written_counterparty_identifiers) {
        const auto it = std::ranges::find_if(
            read_counterparty_identifiers,
            [&](const counterparty_identifier& c) { return c.id == written.id; });
        REQUIRE(it != read_counterparty_identifiers.end());
        CHECK(it->id_value == written.id_value);
    }
}

TEST_CASE("read_latest_counterparty_identifier_by_id", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    auto ctx = ores::testing::make_generation_context(h);
    auto cp = generate_synthetic_counterparty(ctx);
    cp.change_reason_code = "system.test";
    counterparty_repository cp_repo;
    cp_repo.write(h.context(), cp);

    auto ci = generate_synthetic_counterparty_identifier(ctx);
    ci.change_reason_code = "system.test";
    ci.counterparty_id = cp.id;
    const auto original_id_value = ci.id_value;
    BOOST_LOG_SEV(lg, debug) << "Counterparty identifier: " << ci;

    counterparty_identifier_repository repo;
    repo.write(h.context(), ci);

    ci.id_value = original_id_value + "_v2";
    repo.write(h.context(), ci);

    auto read_counterparty_identifiers =
        repo.read_latest(h.context(), boost::uuids::to_string(ci.id));
    BOOST_LOG_SEV(lg, debug) << "Read counterparty identifiers: " << read_counterparty_identifiers;

    REQUIRE(read_counterparty_identifiers.size() == 1);
    CHECK(read_counterparty_identifiers[0].id == ci.id);
    CHECK(read_counterparty_identifiers[0].id_value == original_id_value + "_v2");
}

TEST_CASE("read_nonexistent_counterparty_identifier_id", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    auto ctx = ores::testing::make_generation_context(h);
    auto cp = generate_synthetic_counterparty(ctx);
    cp.change_reason_code = "system.test";
    counterparty_repository cp_repo;
    cp_repo.write(h.context(), cp);

    counterparty_identifier_repository repo;

    // Write a row the read can answer with, so that an empty answer is the key
    // at work rather than a read that does nothing.
    auto keeper = generate_synthetic_counterparty_identifier(ctx);
    keeper.change_reason_code = "system.test";
    keeper.counterparty_id = cp.id;
    repo.write(h.context(), keeper);

    const auto nonexistent_id = boost::uuids::random_generator()();
    BOOST_LOG_SEV(lg, debug) << "Non-existent ID: " << nonexistent_id;

    auto read_counterparty_identifiers =
        repo.read_latest(h.context(), boost::uuids::to_string(nonexistent_id));
    BOOST_LOG_SEV(lg, debug) << "Read counterparty identifiers: " << read_counterparty_identifiers;

    const auto answered_with_another_key =
        std::ranges::any_of(read_counterparty_identifiers, [&](const counterparty_identifier& c) {
            return c.id == nonexistent_id;
        });
    CHECK_FALSE(answered_with_another_key);

    // The same read does answer for the key that was written.
    const auto keeper_rows = repo.read_latest(h.context(), boost::uuids::to_string(keeper.id));
    REQUIRE(keeper_rows.size() == 1);
    CHECK(keeper_rows[0].id_value == keeper.id_value);
}
