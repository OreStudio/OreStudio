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
#include "ores.dq/repository/change_reason_repository.hpp"

#include <catch2/catch_test_macros.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <boost/uuid/uuid_generators.hpp>
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include "ores.logging/make_logger.hpp"
#include "ores.utility/streaming/std_vector.hpp" // IWYU pragma: keep.
#include "ores.dq/domain/change_reason_json_io.hpp" // IWYU pragma: keep.
#include "ores.dq/generators/change_reason_generator.hpp"
#include "ores.testing/database_helper.hpp"

namespace {

const std::string_view test_suite("ores.dq.tests");
const std::string tags("[repository]");

}

using namespace ores::logging;
using namespace ores::dq::generators;

using ores::testing::database_helper;
using ores::dq::repository::change_reason_repository;

TEST_CASE("write_single_change_reason", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;

    change_reason_repository repo(h.context());
    auto change_reason = generate_synthetic_change_reason();
    change_reason.tenant_id = h.tenant_id().to_string();

    BOOST_LOG_SEV(lg, debug) << "Change reason: " << change_reason;
    CHECK_NOTHROW(repo.write(change_reason));
}

TEST_CASE("write_multiple_change_reasons", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;

    change_reason_repository repo(h.context());
    auto change_reasons = generate_synthetic_change_reasons(3);
    for (auto& c : change_reasons)
        c.tenant_id = h.tenant_id().to_string();
    BOOST_LOG_SEV(lg, debug) << "Change reasons: " << change_reasons;

    CHECK_NOTHROW(repo.write(change_reasons));
}

TEST_CASE("read_latest_change_reasons", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;

    change_reason_repository repo(h.context());
    auto written_change_reasons = generate_synthetic_change_reasons(3);
    for (auto& c : written_change_reasons)
        c.tenant_id = h.tenant_id().to_string();
    BOOST_LOG_SEV(lg, debug) << "Written change reasons: " << written_change_reasons;

    repo.write(written_change_reasons);

    auto read_change_reasons = repo.read_latest();
    BOOST_LOG_SEV(lg, debug) << "Read change reasons: " << read_change_reasons;

    CHECK(!read_change_reasons.empty());
    CHECK(read_change_reasons.size() >= written_change_reasons.size());
}

TEST_CASE("read_latest_change_reason_by_code", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;

    change_reason_repository repo(h.context());
    auto change_reasons = generate_synthetic_change_reasons(3);
    for (auto& c : change_reasons)
        c.tenant_id = h.tenant_id().to_string();

    const auto target = change_reasons.front();
    BOOST_LOG_SEV(lg, debug) << "Write change reasons: " << change_reasons;
    repo.write(change_reasons);

    BOOST_LOG_SEV(lg, debug) << "Target change reason: " << target;

    auto read_change_reasons = repo.read_latest(target.code);
    BOOST_LOG_SEV(lg, debug) << "Read change reasons: " << read_change_reasons;

    REQUIRE(read_change_reasons.size() == 1);
    CHECK(read_change_reasons[0].code == target.code);
    CHECK(read_change_reasons[0].description == target.description);
}
