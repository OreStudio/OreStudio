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
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */
#include "ores.logging/make_logger.hpp"
#include "ores.ore.core/domain/credit_simulation_projection.hpp"
#include "ores.ore.core/xml/document_kind.hpp"
#include "ores.testing/project_root.hpp"
#include "test_suite.hpp"
#include <boost/uuid/uuid.hpp>
#include <catch2/catch_test_macros.hpp>
#include <filesystem>
#include <fstream>
#include <stdexcept>
#include <string>

/**
 * @file xml_creditsimulation_projection_tests.cpp
 * @brief The credit simulation document, projected and projected back.
 *
 * The family decomposes a document into five analytics tables: the root
 * configuration with its Risk block, the entities, the matrices with their
 * bounds, one row per source rating, and the netting set codes. The walk
 * compares all five after a round trip, because the text is allowed to spell
 * a flag or a number differently and the entities are not.
 */

namespace {

using namespace ores::logging;
using ores::ore::domain::credit_simulation_projection;
using ores::ore::domain::mapped_credit_simulation;

std::string xml_creditsimulation_projection_read_file(const std::filesystem::path& path) {
    std::ifstream file(path, std::ios::binary);
    return std::string(std::istreambuf_iterator<char>(file), std::istreambuf_iterator<char>());
}

/**
 * @brief Fails the case at the first field the two projections disagree on.
 */
void require_same_entities(const mapped_credit_simulation& lhs,
                           const mapped_credit_simulation& rhs,
                           const std::string& path) {
    INFO(path);
    CHECK(lhs.config.market == rhs.config.market);
    CHECK(lhs.config.credit == rhs.config.credit);
    CHECK(lhs.config.zero_market_pnl == rhs.config.zero_market_pnl);
    CHECK(lhs.config.double_default == rhs.config.double_default);
    CHECK(lhs.config.seed == rhs.config.seed);
    CHECK(lhs.config.paths == rhs.config.paths);
    CHECK(lhs.config.evaluation == rhs.config.evaluation);
    CHECK(lhs.config.credit_mode == rhs.config.credit_mode);
    CHECK(lhs.config.loan_exposure_mode == rhs.config.loan_exposure_mode);

    REQUIRE(lhs.entities.size() == rhs.entities.size());
    for (std::size_t i = 0; i < lhs.entities.size(); ++i) {
        CHECK(lhs.entities[i].name == rhs.entities[i].name);
        CHECK(lhs.entities[i].factor_loadings == rhs.entities[i].factor_loadings);
        CHECK(lhs.entities[i].initial_state == rhs.entities[i].initial_state);
    }
    REQUIRE(lhs.matrices.size() == rhs.matrices.size());
    for (std::size_t i = 0; i < lhs.matrices.size(); ++i) {
        CHECK(lhs.matrices[i].name == rhs.matrices[i].name);
        CHECK(lhs.matrices[i].t0 == rhs.matrices[i].t0);
        CHECK(lhs.matrices[i].t1 == rhs.matrices[i].t1);
    }
    REQUIRE(lhs.rows.size() == rhs.rows.size());
    for (std::size_t i = 0; i < lhs.rows.size(); ++i) {
        CHECK(lhs.rows[i].from_rating == rhs.rows[i].from_rating);
        CHECK(lhs.rows[i].p_aaa == rhs.rows[i].p_aaa);
        CHECK(lhs.rows[i].p_aa == rhs.rows[i].p_aa);
        CHECK(lhs.rows[i].p_a == rhs.rows[i].p_a);
        CHECK(lhs.rows[i].p_baa == rhs.rows[i].p_baa);
        CHECK(lhs.rows[i].p_ba == rhs.rows[i].p_ba);
        CHECK(lhs.rows[i].p_b == rhs.rows[i].p_b);
        CHECK(lhs.rows[i].p_c == rhs.rows[i].p_c);
        CHECK(lhs.rows[i].p_default == rhs.rows[i].p_default);
    }
    // The entity points at a matrix by id, and the ids are minted on each
    // mapping, so the link is compared by the name each id resolves to.
    for (std::size_t i = 0; i < lhs.entities.size(); ++i) {
        const auto name_of = [](const auto& mapped, const boost::uuids::uuid& id) {
            for (const auto& matrix : mapped.matrices)
                if (matrix.id == id)
                    return matrix.name;
            return std::string{};
        };
        CHECK(name_of(lhs, lhs.entities[i].transition_matrix_id) ==
              name_of(rhs, rhs.entities[i].transition_matrix_id));
    }
    REQUIRE(lhs.netting_sets.size() == rhs.netting_sets.size());
    for (std::size_t i = 0; i < lhs.netting_sets.size(); ++i) {
        CHECK(lhs.netting_sets[i].netting_set_id == rhs.netting_sets[i].netting_set_id);
        CHECK(lhs.netting_sets[i].position == rhs.netting_sets[i].position);
    }
}

}

TEST_CASE("a_credit_simulation_survives_the_round_trip",
          "[ore][xml][roundtrip][creditsimulation]") {
    auto lg(make_logger(test_suite));

    // Eight rows of eight, one on the diagonal, which is the shape a
    // transition matrix has to hold.
    std::string grid;
    for (int row = 0; row < 8; ++row) {
        for (int column = 0; column < 8; ++column)
            grid += (row == column ? "1.0 " : "0.0 ");
    }

    const std::string text =
        "<CreditSimulation>"
        "<TransitionMatrices><TransitionMatrix>"
        "<Name>Base</Name>"
        "<Data t0=\"0.0\" t1=\"1.0\">" +
        grid +
        "</Data>"
        "</TransitionMatrix></TransitionMatrices>"
        "<Entities><Entity><Name>A</Name><TransitionMatrix>Base</TransitionMatrix>"
        "<InitialState>0</InitialState><FactorLoadings>1.0</FactorLoadings></Entity></Entities>"
        "<NettingSetIds>CPTY_A, CPTY_B</NettingSetIds>"
        "<Risk><Market>Y</Market><Credit>N</Credit><ZeroMarketPnl>N</ZeroMarketPnl>"
        "<Evaluation>2019-01-01</Evaluation><DoubleDefault>N</DoubleDefault>"
        "<Seed>42</Seed><Paths>1000</Paths><CreditMode>Default</CreditMode>"
        "<LoanExposureMode>Collateralised</LoanExposureMode></Risk>"
        "</CreditSimulation>";

    const auto first = credit_simulation_projection::map(credit_simulation_projection::read(text));
    REQUIRE(first.entities.size() == 1);
    REQUIRE(first.rows.size() == 8);
    CHECK(first.config.market == "Y");
    CHECK(first.config.credit == "N");
    CHECK(first.config.seed == 42);
    REQUIRE(first.netting_sets.size() == 2);
    CHECK(first.netting_sets[1].netting_set_id == "CPTY_B");

    const auto written =
        credit_simulation_projection::write(credit_simulation_projection::reverse(first));
    // A comment carries no data, so the export does not write the label block.
    CHECK(written.find("<!--") == std::string::npos);
    const auto second =
        credit_simulation_projection::map(credit_simulation_projection::read(written));
    require_same_entities(first, second, "literal document");
}

TEST_CASE("a_model_with_no_matrix_is_refused_rather_than_written_empty",
          "[ore][xml][roundtrip][creditsimulation]") {
    auto lg(make_logger(test_suite));

    // The schema requires a matrix and an entity, so an empty model cannot
    // produce a document that validates. The writer says which element is
    // missing rather than emitting one.
    CHECK_THROWS_AS(credit_simulation_projection::write(credit_simulation_projection::reverse(
                        ores::ore::domain::mapped_credit_simulation{})),
                    std::runtime_error);
}

TEST_CASE("every_credit_simulation_in_the_corpus_round_trips",
          "[ore][xml][roundtrip][creditsimulation]") {
    auto lg(make_logger(test_suite));

    const auto root = ores::testing::project_root::resolve("external/ore/examples");
    REQUIRE(std::filesystem::is_directory(root));

    std::size_t files = 0;
    for (const auto& entry : std::filesystem::recursive_directory_iterator(root)) {
        if (!entry.is_regular_file() || entry.path().extension() != ".xml")
            continue;
        if (ores::ore::xml::detect_document_kind(entry.path()) !=
            ores::ore::xml::document_kind::credit_simulation)
            continue;
        ++files;
        const auto first = credit_simulation_projection::map(credit_simulation_projection::read(
            xml_creditsimulation_projection_read_file(entry.path())));
        const auto written =
            credit_simulation_projection::write(credit_simulation_projection::reverse(first));
        const auto second =
            credit_simulation_projection::map(credit_simulation_projection::read(written));
        require_same_entities(first, second, entry.path().string());
    }
    BOOST_LOG_SEV(lg, info) << "credit simulation documents: " << files;
    CHECK(files == 14);
}
