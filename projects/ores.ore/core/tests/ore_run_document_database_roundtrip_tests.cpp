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
#include "ores.database/domain/party_scope.hpp"
#include "ores.ore.core/domain/domain.hpp"
#include "ores.ore.core/domain/run_document_projection.hpp"
#include "ores.platform/filesystem/file.hpp"
#include "ores.reporting.core/repository/configuration_repository.hpp"
#include "ores.reporting.core/repository/risk_report_config_repository.hpp"
#include "ores.testing/project_root.hpp"
#include "ores.testing/scoped_database_helper.hpp"
#include "party_fixture.hpp"
#include <boost/uuid/random_generator.hpp>
#include <algorithm>
#include <catch2/catch_test_macros.hpp>
#include <filesystem>
#include <string>

namespace {

const std::string tags("[ore][run_document][database][roundtrip]");

std::filesystem::path corpus_file(const std::string& relative) {
    return ores::testing::project_root::resolve("external/ore/examples/" + relative);
}

}

using ores::ore::domain::run_document_projection;
using ores::platform::filesystem::file;
using ores::reporting::repository::configuration_repository;
using ores::reporting::repository::risk_report_config_repository;
using ores::testing::scoped_database_helper;

TEST_CASE("a party sees only its own run settings", tags) {
    scoped_database_helper h;
    auto parties = ores::ore::tests::make_two_parties(h);

    const auto original = run_document_projection::read(
        file::read_content(corpus_file("ORE-Python/Notebooks/Example_1/Input/ore.xml")));
    auto next_id = boost::uuids::random_generator();

    ores::reporting::domain::configuration header;
    header.id = next_id();
    header.name = "Example_1";
    header.configuration_type_code = "risk_report_config";
    header.owning_component = "ores.reporting";
    ores::database::domain::assign_party(header, parties.a);
    configuration_repository().write(parties.a_context, header);

    auto settings = run_document_projection::map_setup(original).settings;
    settings.id = next_id();
    settings.name = "Example_1";
    settings.description = "Example_1";
    settings.configuration_id = header.id;
    ores::database::domain::assign_party(settings, parties.a);

    risk_report_config_repository repository;
    repository.write(parties.a_context, settings);

    const auto owns = [&](const auto& rows) {
        return std::ranges::any_of(rows, [&](const auto& r) { return r.id == settings.id; });
    };
    CHECK(owns(repository.read_latest(parties.a_context)));
    CHECK_FALSE(owns(repository.read_latest(parties.b_context)));
}
