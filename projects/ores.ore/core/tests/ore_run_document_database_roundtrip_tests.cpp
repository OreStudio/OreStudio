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
#include "ores.logging/make_logger.hpp"
#include "ores.ore.core/domain/domain.hpp"
#include "ores.ore.core/domain/run_document_mapper.hpp"
#include "ores.platform/filesystem/file.hpp"
#include "ores.reporting.core/repository/analytic_type_repository.hpp"
#include "ores.reporting.core/repository/report_analytic_repository.hpp"
#include "ores.reporting.core/repository/report_market_binding_repository.hpp"
#include "ores.reporting.core/repository/report_run_setup_repository.hpp"
#include "ores.testing/project_root.hpp"
#include "ores.testing/scoped_database_helper.hpp"
#include <boost/uuid/random_generator.hpp>
#include <algorithm>
#include <catch2/catch_test_macros.hpp>
#include <filesystem>
#include <map>
#include <string>

namespace {

const std::string_view test_suite("ores.ore.run_document.database.tests");
const std::string tags("[ore][run_document][database][roundtrip]");

std::filesystem::path corpus_file(const std::string& relative) {
    return ores::testing::project_root::resolve("external/ore/examples/" + relative);
}

std::map<std::string, std::string> parameters_of(
    const ores::ore::domain::parameterListType& list) {
    std::map<std::string, std::string> out;
    for (const auto& parameter : list.Parameter)
        out[std::string(parameter.name)] = static_cast<const std::string&>(parameter);
    return out;
}

}

using ores::ore::domain::run_document_mapper;
using ores::platform::filesystem::file;
using ores::reporting::repository::analytic_type_repository;
using ores::reporting::repository::report_analytic_repository;
using ores::reporting::repository::report_market_binding_repository;
using ores::reporting::repository::report_run_setup_repository;
using ores::testing::scoped_database_helper;

/**
 * The whole path, through the database: a corpus run document is parsed, mapped
 * into the reporting entities, written to PostgreSQL, read back, and only then
 * turned into ORE parameter lists again. The in-memory cases prove the mapper;
 * this proves the tables hold what the mapper produces.
 */
TEST_CASE("run_document_roundtrips_through_the_database", tags) {
    auto lg(ores::logging::make_logger(test_suite));
    scoped_database_helper h;

    ores::ore::domain::ore original;
    ores::ore::domain::load_data(
        file::read_content(corpus_file("ORE-Python/Notebooks/Example_1/Input/ore.xml")),
        original);

    // The mapper reads a document, so the surrogate keys and the definition the
    // rows hang off are the caller's to set.
    auto next_id = boost::uuids::random_generator();
    const auto definition_id = next_id();

    auto setup = run_document_mapper::map_setup(original);
    setup.id = next_id();
    setup.report_definition_id = definition_id;

    auto analytics = run_document_mapper::map_analytics(original);
    for (auto& row : analytics) {
        row.analytic.id = next_id();
        row.analytic.report_definition_id = definition_id;
    }

    auto bindings = run_document_mapper::map_market_bindings(original);
    for (auto& binding : bindings) {
        binding.id = next_id();
        binding.report_definition_id = definition_id;
    }

    report_run_setup_repository setups;
    analytic_type_repository types_repo;
    report_analytic_repository analytics_repo;
    report_market_binding_repository bindings_repo;

    // The analytic type is a soft foreign key, so the vocabulary the document
    // uses has to be in this tenant before the analytics can be written.
    {
        std::map<std::string, int> distinct;
        for (const auto& row : analytics)
            distinct[row.analytic.analytic_type_code] = 0;
        int order = 0;
        for (auto& [code, unused] : distinct) {
            ores::reporting::domain::analytic_type type;
            type.code = code;
            type.name = code;
            type.description = "written by the run document round trip test";
            type.display_order = ++order;
            types_repo.write(h.context(), type);
        }
    }

    setups.write(h.context(), setup);
    for (const auto& row : analytics)
        analytics_repo.write(h.context(), row.analytic);
    bindings_repo.write(h.context(), bindings);

    const auto stored_setups = setups.read_latest(h.context());
    const auto found_setup = std::find_if(stored_setups.begin(), stored_setups.end(),
                                          [&](const auto& row) {
                                              return row.report_definition_id == definition_id;
                                          });
    REQUIRE(found_setup != stored_setups.end());

    const auto stored_analytics = analytics_repo.read_latest(h.context());
    std::vector<decltype(analytics)::value_type> read_analytics;
    for (const auto& row : stored_analytics) {
        if (row.report_definition_id != definition_id)
            continue;
        read_analytics.push_back({row, {}});
    }
    std::sort(read_analytics.begin(), read_analytics.end(),
              [](const auto& lhs, const auto& rhs) {
                  return lhs.analytic.display_order < rhs.analytic.display_order;
              });

    const auto stored_bindings = bindings_repo.read_latest(h.context());
    std::vector<ores::reporting::domain::report_market_binding> read_bindings;
    for (const auto& row : stored_bindings) {
        if (row.report_definition_id == definition_id)
            read_bindings.push_back(row);
    }
    std::sort(read_bindings.begin(), read_bindings.end(),
              [](const auto& lhs, const auto& rhs) { return lhs.position < rhs.position; });

    // Export from what the database returned, not from what was written.
    const auto exported_setup = run_document_mapper::reverse_setup(*found_setup);
    const auto exported_markets = run_document_mapper::reverse_market_bindings(read_bindings);

    const auto original_setup = parameters_of(original.Setup);
    const auto round_tripped_setup = parameters_of(exported_setup);
    REQUIRE(round_tripped_setup.size() == original_setup.size());
    for (const auto& [name, value] : original_setup) {
        INFO("Setup parameter " << name);
        CHECK(round_tripped_setup.at(name) == value);
    }

    const auto original_markets = parameters_of(*original.Markets);
    const auto round_tripped_markets = parameters_of(exported_markets);
    REQUIRE(round_tripped_markets.size() == original_markets.size());
    for (const auto& [role, configuration] : original_markets) {
        INFO("market role " << role);
        CHECK(round_tripped_markets.at(role) == configuration);
    }

    REQUIRE(read_analytics.size() == analytics.size());
    for (std::size_t i = 0; i < analytics.size(); ++i) {
        INFO("analytic " << i);
        CHECK(read_analytics.at(i).analytic.analytic_type_code ==
              analytics.at(i).analytic.analytic_type_code);
        CHECK(read_analytics.at(i).analytic.display_order ==
              analytics.at(i).analytic.display_order);
        CHECK(read_analytics.at(i).analytic.active == analytics.at(i).analytic.active);
    }
}
