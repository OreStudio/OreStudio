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
#include "ores.dq/repository/subject_area_repository.hpp"

#include <catch2/catch_test_macros.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <boost/uuid/uuid_generators.hpp>
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include "ores.logging/make_logger.hpp"
#include "ores.utility/streaming/std_vector.hpp" // IWYU pragma: keep.
#include "ores.dq/domain/subject_area_json_io.hpp" // IWYU pragma: keep.
#include "ores.dq/generators/subject_area_generator.hpp"
#include "ores.testing/database_helper.hpp"

namespace {

const std::string_view test_suite("ores.dq.tests");
const std::string tags("[repository]");

}

using namespace ores::logging;
using namespace ores::dq::generators;

using ores::testing::database_helper;
using ores::dq::repository::subject_area_repository;

TEST_CASE("write_single_subject_area", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;

    subject_area_repository repo(h.context());
    auto subject_area = generate_synthetic_subject_area();
    subject_area.tenant_id = h.tenant_id().to_string();

    BOOST_LOG_SEV(lg, debug) << "Subject area: " << subject_area;
    CHECK_NOTHROW(repo.write(subject_area));
}

TEST_CASE("write_multiple_subject_areas", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;

    subject_area_repository repo(h.context());
    auto subject_areas = generate_synthetic_subject_areas(3);
    for (auto& s : subject_areas)
        s.tenant_id = h.tenant_id().to_string();
    BOOST_LOG_SEV(lg, debug) << "Subject areas: " << subject_areas;

    CHECK_NOTHROW(repo.write(subject_areas));
}

TEST_CASE("read_latest_subject_areas", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;

    subject_area_repository repo(h.context());
    auto written_subject_areas = generate_synthetic_subject_areas(3);
    for (auto& s : written_subject_areas)
        s.tenant_id = h.tenant_id().to_string();
    BOOST_LOG_SEV(lg, debug) << "Written subject areas: " << written_subject_areas;

    repo.write(written_subject_areas);

    auto read_subject_areas = repo.read_latest();
    BOOST_LOG_SEV(lg, debug) << "Read subject areas: " << read_subject_areas;

    CHECK(!read_subject_areas.empty());
    CHECK(read_subject_areas.size() >= written_subject_areas.size());
}

TEST_CASE("read_latest_subject_areas_by_domain", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;

    subject_area_repository repo(h.context());
    const std::string domain_name = "test_domain_for_subject_area_12345";
    auto subject_area = generate_synthetic_subject_area(domain_name);
    subject_area.tenant_id = h.tenant_id().to_string();
    BOOST_LOG_SEV(lg, debug) << "Write subject area: " << subject_area;
    repo.write(subject_area);

    BOOST_LOG_SEV(lg, debug) << "Target domain name: " << domain_name;

    auto read_subject_areas = repo.read_latest_by_domain(domain_name);
    BOOST_LOG_SEV(lg, debug) << "Read subject areas: " << read_subject_areas;

    REQUIRE(!read_subject_areas.empty());
    for (const auto& sa : read_subject_areas) {
        CHECK(sa.domain_name == domain_name);
    }
}
