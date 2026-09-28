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
#include "ores.analytics.core/repository/credit_simulation_config_repository.hpp"
#include "ores.analytics.core/repository/credit_simulation_entity_config_repository.hpp"
#include "ores.analytics.core/repository/credit_simulation_matrix_config_repository.hpp"
#include "ores.analytics.core/repository/credit_simulation_matrix_row_config_repository.hpp"
#include "ores.analytics.core/repository/credit_simulation_netting_set_config_repository.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.ore.core/domain/credit_simulation_mapper.hpp"
#include "ores.ore.core/domain/domain.hpp"
#include "ores.platform/filesystem/file.hpp"
#include "ores.testing/project_root.hpp"
#include "ores.testing/scoped_database_helper.hpp"
#include <algorithm>
#include <catch2/catch_test_macros.hpp>
#include <filesystem>
#include <boost/uuid/uuid_io.hpp>
#include <cstdint>
#include <string>

namespace {

const std::string_view test_suite("ores.ore.tests");
const std::string tags("[creditsimulation][database][roundtrip]");

std::filesystem::path ore_path(const std::string& relative) {
    return ores::testing::project_root::resolve("external/ore/" + relative);
}

}

using ores::analytics::domain::credit_simulation_matrix_row_config;
using ores::analytics::repository::credit_simulation_config_repository;
using ores::analytics::repository::credit_simulation_entity_config_repository;
using ores::analytics::repository::credit_simulation_matrix_config_repository;
using ores::analytics::repository::credit_simulation_matrix_row_config_repository;
using ores::analytics::repository::credit_simulation_netting_set_config_repository;
using ores::ore::domain::credit_rating_scale;
using ores::ore::domain::credit_simulation_mapper;
using ores::ore::domain::creditsimulation;
using ores::ore::domain::mapped_credit_simulation;
using ores::logging::make_logger;
using ores::testing::scoped_database_helper;

/**
 * The whole path, through the database: a corpus document is parsed, mapped into
 * the analytics entities, written to PostgreSQL, read back, and only then turned
 * into a document again. Passing the in-memory round trip proves the mapper;
 * this proves the tables hold what the mapper produces.
 */
TEST_CASE("credit_simulation_roundtrip_through_the_database", tags) {
    auto lg(make_logger(test_suite));
    scoped_database_helper h;

    const auto f = ore_path("examples/CreditRisk/Input/CreditPortfolioModel/creditsimulation.xml");
    using ores::platform::filesystem::file;
    const std::string content = file::read_content(f);

    creditsimulation original;
    ores::ore::domain::load_data(content, original);

    const mapped_credit_simulation mapped = credit_simulation_mapper::map(original);
    REQUIRE(mapped.rows.size() == credit_rating_scale.size());

    credit_simulation_config_repository config_repo;
    credit_simulation_matrix_config_repository matrix_repo;
    credit_simulation_matrix_row_config_repository row_repo;
    credit_simulation_entity_config_repository entity_repo;
    credit_simulation_netting_set_config_repository netting_set_repo;

    config_repo.write(h.context(), mapped.config);
    matrix_repo.write(h.context(), mapped.matrices);
    row_repo.write(h.context(), mapped.rows);
    netting_set_repo.write(h.context(), mapped.netting_sets);
    entity_repo.write(h.context(), mapped.entities);

    const auto stored_nets = netting_set_repo.read_latest(h.context());
    std::vector<std::string> stored_codes;
    for (const auto& net : stored_nets)
        stored_codes.push_back(net.netting_set_id);
    std::sort(stored_codes.begin(), stored_codes.end());
    std::vector<std::string> expected_codes;
    for (const auto& net : mapped.netting_sets)
        expected_codes.push_back(net.netting_set_id);
    std::sort(expected_codes.begin(), expected_codes.end());
    INFO("netting sets read back: " << stored_codes.size());
    CHECK(stored_codes == expected_codes);

    const auto matrix_id = mapped.matrices.front().id;
    const auto rows = row_repo.read_latest_by_transition_matrix_id(h.context(), boost::uuids::to_string(matrix_id), 0, 100);
    INFO("rows read back: " << rows.size());
    REQUIRE(rows.size() == mapped.rows.size());

    std::vector<std::string> read_ratings;
    for (const auto& row : rows)
        read_ratings.push_back(row.from_rating);
    std::sort(read_ratings.begin(), read_ratings.end());

    std::vector<std::string> expected_ratings;
    for (const auto& rating : credit_rating_scale)
        expected_ratings.emplace_back(rating);
    std::sort(expected_ratings.begin(), expected_ratings.end());
    CHECK(read_ratings == expected_ratings);

    const auto matrices = matrix_repo.read_latest(h.context());
    const auto found = std::find_if(matrices.begin(), matrices.end(), [&](const auto& m) {
        return m.id == matrix_id;
    });
    REQUIRE(found != matrices.end());
    CHECK(found->t0 == mapped.matrices.front().t0);
    CHECK(found->t1 == mapped.matrices.front().t1);

    // Export from what the database returned, not from what was written.
    mapped_credit_simulation from_database = mapped;
    from_database.rows = rows;
    const creditsimulation rebuilt = credit_simulation_mapper::reverse(from_database);
    const std::string exported_xml = ores::ore::domain::save_data(rebuilt);

    // No state comment is written: the ratings come from the type, so the
    // exported document differs from the original by that comment alone.
    CHECK(exported_xml.find("<!--") == std::string::npos);

    creditsimulation exported;
    ores::ore::domain::load_data(exported_xml, exported);
    REQUIRE(exported.TransitionMatrices.TransitionMatrix.size() == 1);
    CHECK(exported.Entities.Entity.size() == original.Entities.Entity.size());
}
