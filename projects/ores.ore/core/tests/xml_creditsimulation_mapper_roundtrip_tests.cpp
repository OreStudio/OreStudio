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
#include "ores.analytics.api/domain/credit_simulation_matrix_row_config.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.ore.core/domain/credit_simulation_grid.hpp"
#include "ores.ore.core/domain/credit_simulation_mapper.hpp"
#include "ores.ore.core/domain/domain.hpp"
#include "ores.platform/filesystem/file.hpp"
#include "ores.testing/project_root.hpp"
#include <catch2/catch_test_macros.hpp>
#include <cstddef>
#include <filesystem>
#include <sstream>
#include <string>
#include <vector>

namespace {

const std::string_view test_suite("ores.ore.creditsimulation.mapper.roundtrip.tests");
const std::string tags("[ore][xml][roundtrip][creditsimulation][mapper]");

std::filesystem::path ore_path(const std::string& relative) {
    return ores::testing::project_root::resolve("external/ore/" + relative);
}

using ores::analytics::domain::credit_simulation_matrix_row_config;
using ores::ore::domain::credit_rating_scale;
using ores::ore::domain::credit_simulation_mapper;
using ores::ore::domain::creditsimulation;
using ores::ore::domain::mapped_credit_simulation;
using ores::ore::domain::parse_credit_simulation_grid;
using namespace ores::logging;

struct mismatch {
    bool equal = true;
    std::string message;
};

std::string describe(const std::string& path, const std::string& detail) {
    return path + ": " + detail;
}


mismatch compare_mapped_rows(const creditsimulation& original,
                             const mapped_credit_simulation& mapped,
                             const std::string& path) {
    const auto& matrices = original.TransitionMatrices.TransitionMatrix;
    if (mapped.matrices.size() != matrices.size())
        return {false,
                describe(path,
                         "mapped matrix count differs: document " +
                             std::to_string(matrices.size()) + ", mapped " +
                             std::to_string(mapped.matrices.size()))};

    for (std::size_t i = 0; i < matrices.size(); ++i) {
        const auto& document_matrix = matrices.at(i);
        const auto& mapped_matrix = mapped.matrices.at(i);
        if (std::string(document_matrix.Name) != mapped_matrix.name)
            return {false,
                    describe(path,
                             "matrix " + std::to_string(i) + " mapped name differs: document '" +
                                 std::string(document_matrix.Name) + "', mapped '" +
                                 mapped_matrix.name + "'")};

        if (!document_matrix.Data.t0 || !document_matrix.Data.t1)
            return {false, describe(path, "matrix '" + mapped_matrix.name + "' lost a bound")};
        if (std::stod(*document_matrix.Data.t0) != mapped_matrix.t0 ||
            std::stod(*document_matrix.Data.t1) != mapped_matrix.t1)
            return {false,
                    describe(path, "matrix '" + mapped_matrix.name + "' mapped bounds differ")};

        const auto grid = parse_credit_simulation_grid(document_matrix.Data);
        const auto side = grid.side();

        std::vector<credit_simulation_matrix_row_config> rows;
        for (const auto& row : mapped.rows) {
            if (row.transition_matrix_id == mapped_matrix.id)
                rows.push_back(row);
        }
        if (rows.size() != side)
            return {false,
                    describe(path,
                             "matrix '" + mapped_matrix.name + "' mapped row count differs: grid " +
                                 std::to_string(side) + ", mapped " + std::to_string(rows.size()))};

        for (std::size_t r = 0; r < side; ++r) {
            if (rows[r].from_rating != credit_rating_scale[r])
                return {false,
                        describe(path,
                                 "matrix '" + mapped_matrix.name + "' mapped state label " +
                                     std::to_string(r) + " differs: expected '" +
                                     std::string(credit_rating_scale[r]) + "', mapped '" +
                                     rows[r].from_rating + "'")};

            const double probabilities[] = {rows[r].p_aaa,
                                            rows[r].p_aa,
                                            rows[r].p_a,
                                            rows[r].p_baa,
                                            rows[r].p_ba,
                                            rows[r].p_b,
                                            rows[r].p_c,
                                            rows[r].p_default};
            for (std::size_t c = 0; c < 8; ++c) {
                if (probabilities[c] != grid.values[r * side + c])
                    return {false,
                            describe(path,
                                     "matrix '" + mapped_matrix.name + "' mapped cell (row " +
                                         std::to_string(r) + ", col " + std::to_string(c) +
                                         ") differs: grid " +
                                         std::to_string(grid.values[r * side + c]) + ", mapped " +
                                         std::to_string(probabilities[c]))};
            }
        }
    }
    return {};
}

void require_in_memory_roundtrip(const std::string& relative_path) {
    auto lg(make_logger(test_suite));

    const auto f = ore_path(relative_path);
    using ores::platform::filesystem::file;
    const std::string content = file::read_content(f);

    creditsimulation original;
    ores::ore::domain::load_data(content, original);

    const mapped_credit_simulation mapped = credit_simulation_mapper::map(original);
    const auto mapped_result = compare_mapped_rows(original, mapped, f.string());
    INFO(mapped_result.message);
    CHECK(mapped_result.equal);

    const creditsimulation rebuilt = credit_simulation_mapper::reverse(mapped);
    const std::string exported_xml = ores::ore::domain::save_data(rebuilt);

    creditsimulation exported;
    ores::ore::domain::load_data(exported_xml, exported);

    const auto difference = credit_simulation_difference(original, exported, f.string());
    INFO(difference);
    CHECK(difference.empty());
    BOOST_LOG_SEV(lg, info) << "In-memory roundtrip passed for: " << f.string();
}

}

#define CREDIT_SIMULATION_MAPPER_ROUNDTRIP(name, path)           \
    TEST_CASE("creditsimulation_mapper_roundtrip_" name, tags) { \
        require_in_memory_roundtrip(path);                       \
    }

CREDIT_SIMULATION_MAPPER_ROUNDTRIP(
    "credit_portfolio_model", "examples/CreditRisk/Input/CreditPortfolioModel/creditsimulation.xml")
CREDIT_SIMULATION_MAPPER_ROUNDTRIP(
    "credit_portfolio_model_1",
    "examples/CreditRisk/Input/CreditPortfolioModel/creditsimulation1.xml")
CREDIT_SIMULATION_MAPPER_ROUNDTRIP(
    "credit_portfolio_model_2",
    "examples/CreditRisk/Input/CreditPortfolioModel/creditsimulation2.xml")
CREDIT_SIMULATION_MAPPER_ROUNDTRIP(
    "credit_portfolio_model_3",
    "examples/CreditRisk/Input/CreditPortfolioModel/creditsimulation3.xml")
CREDIT_SIMULATION_MAPPER_ROUNDTRIP(
    "credit_portfolio_model_3_ts",
    "examples/CreditRisk/Input/CreditPortfolioModel/creditsimulation3_ts.xml")
CREDIT_SIMULATION_MAPPER_ROUNDTRIP(
    "credit_portfolio_model_4",
    "examples/CreditRisk/Input/CreditPortfolioModel/creditsimulation4.xml")
CREDIT_SIMULATION_MAPPER_ROUNDTRIP(
    "credit_portfolio_model_100",
    "examples/CreditRisk/Input/CreditPortfolioModel/creditsimulation100.xml")

CREDIT_SIMULATION_MAPPER_ROUNDTRIP("legacy_example_43",
                                   "examples/Legacy/Example_43/Input/creditsimulation.xml")
CREDIT_SIMULATION_MAPPER_ROUNDTRIP("legacy_example_43_1",
                                   "examples/Legacy/Example_43/Input/creditsimulation1.xml")
CREDIT_SIMULATION_MAPPER_ROUNDTRIP("legacy_example_43_2",
                                   "examples/Legacy/Example_43/Input/creditsimulation2.xml")
CREDIT_SIMULATION_MAPPER_ROUNDTRIP("legacy_example_43_3",
                                   "examples/Legacy/Example_43/Input/creditsimulation3.xml")
CREDIT_SIMULATION_MAPPER_ROUNDTRIP("legacy_example_43_3_ts",
                                   "examples/Legacy/Example_43/Input/creditsimulation3_ts.xml")
CREDIT_SIMULATION_MAPPER_ROUNDTRIP("legacy_example_43_4",
                                   "examples/Legacy/Example_43/Input/creditsimulation4.xml")
CREDIT_SIMULATION_MAPPER_ROUNDTRIP("legacy_example_43_100",
                                   "examples/Legacy/Example_43/Input/creditsimulation100.xml")
