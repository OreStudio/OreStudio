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
#include "ores.ore.core/domain/conventions_mapper.hpp"
#include "ores.ore.core/domain/curve_configuration_mapper.hpp"
#include "ores.ore.core/domain/domain.hpp"
#include "ores.ore.core/xml/roundtrip_harness.hpp"
#include "ores.platform/filesystem/file.hpp"
#include "ores.refdata.core/repository/average_ois_convention_repository.hpp"
#include "ores.refdata.core/repository/curve_bootstrap_config_repository.hpp"
#include "ores.refdata.core/repository/curve_configuration_repository.hpp"
#include "ores.refdata.core/repository/curve_configuration_section_repository.hpp"
#include "ores.refdata.core/repository/curve_definition_repository.hpp"
#include "ores.refdata.core/repository/curve_quote_repository.hpp"
#include "ores.refdata.core/repository/curve_segment_curve_repository.hpp"
#include "ores.refdata.core/repository/curve_segment_repository.hpp"
#include "ores.refdata.core/repository/deposit_convention_repository.hpp"
#include "ores.refdata.core/repository/ois_convention_repository.hpp"
#include "ores.refdata.core/repository/yield_curve_repository.hpp"
#include "ores.testing/project_root.hpp"
#include "ores.testing/scoped_database_helper.hpp"
#include <catch2/catch_test_macros.hpp>
#include <set>
#include <stdexcept>
#include <string>
#include <vector>

/**
 * @file curve_configuration_database_roundtrip_tests.cpp
 * @brief The curve configuration through the database.
 *
 * The in-memory walk proves the mapper over the corpus; this proves the tables
 * hold what the mapper produces, and that the database refuses what the model
 * says it must: a convention, a day counter or a segment type it does not hold.
 */

namespace {

const std::string tags("[curveconfig][database][roundtrip]");

using namespace ores::ore::domain;
using namespace ores::refdata::repository;
using ores::platform::filesystem::file;

const std::string example = "external/ore/examples/Legacy/Example_62/Input/";

template <typename Document>
Document load(const std::string& name) {
    Document d;
    load_data(file::read_content(ores::testing::project_root::resolve(example + name)), d);
    return d;
}

// Writes the conventions of one kind that the tenant does not hold yet. The
// test tenant is shared by every case in a run, so a second case finds them
// already written.
template <typename Repository, typename Row>
void write_missing(const ores::database::context& ctx,
                   Repository repo,
                   const std::vector<Row>& rows) {
    std::set<std::string> held;
    for (const auto& r : repo.read_latest(ctx))
        held.insert(r.id);
    std::vector<Row> missing;
    for (const auto& r : rows)
        if (!held.contains(r.id))
            missing.push_back(r);
    if (!missing.empty())
        repo.write(ctx, missing);
}

// The conventions the example's curves name, written so the segment trigger
// can resolve them. Only these three kinds are written, because the curves
// name nothing else.
void write_conventions(const ores::database::context& ctx) {
    const auto mapped = conventions_mapper::map(load<conventions>("conventions.xml"));
    write_missing(ctx, deposit_convention_repository(), mapped.deposit);
    write_missing(ctx, ois_convention_repository(), mapped.ois);
    write_missing(ctx, average_ois_convention_repository(), mapped.average_ois);
}

void write(const ores::database::context& ctx, const mapped_curve_configuration& m) {
    curve_configuration_repository().write(ctx, m.config);
    curve_configuration_section_repository().write(ctx, m.sections);
    curve_definition_repository().write(ctx, m.definitions);
    yield_curve_repository().write(ctx, m.yield_curves);
    curve_bootstrap_config_repository().write(ctx, m.bootstrap_configs);
    curve_segment_repository().write(ctx, m.segments);
    curve_segment_curve_repository().write(ctx, m.segment_curves);
    curve_quote_repository().write(ctx, m.quotes);
}

template <typename Row, typename Repository, typename Keep>
std::vector<Row> read(const ores::database::context& ctx, Repository repo, Keep keep) {
    std::vector<Row> out;
    for (const auto& r : repo.read_latest(ctx))
        if (keep(r))
            out.push_back(r);
    return out;
}

mapped_curve_configuration read_back(const ores::database::context& ctx,
                                     const mapped_curve_configuration& written) {
    mapped_curve_configuration m;
    m.config = written.config;
    const auto id = written.config.id;
    m.sections = read<ores::refdata::domain::curve_configuration_section>(
        ctx, curve_configuration_section_repository(), [&](const auto& r) {
            return r.curve_configuration_id == id;
        });
    m.definitions = read<ores::refdata::domain::curve_definition>(
        ctx, curve_definition_repository(), [&](const auto& r) {
            return r.curve_configuration_id == id;
        });
    std::set<boost::uuids::uuid> definitions;
    for (const auto& d : m.definitions)
        definitions.insert(d.id);
    const auto of_definition = [&](const auto& r) {
        return definitions.contains(r.curve_definition_id);
    };
    m.yield_curves =
        read<ores::refdata::domain::yield_curve>(ctx, yield_curve_repository(), of_definition);
    m.bootstrap_configs = read<ores::refdata::domain::curve_bootstrap_config>(
        ctx, curve_bootstrap_config_repository(), of_definition);
    m.segments =
        read<ores::refdata::domain::curve_segment>(ctx, curve_segment_repository(), of_definition);
    m.quotes = read<ores::refdata::domain::curve_quote>(ctx, curve_quote_repository(), of_definition);
    std::set<boost::uuids::uuid> segments;
    for (const auto& s : m.segments)
        segments.insert(s.id);
    m.segment_curves = read<ores::refdata::domain::curve_segment_curve>(
        ctx, curve_segment_curve_repository(), [&](const auto& r) {
            return segments.contains(r.curve_segment_id);
        });
    return m;
}

}

TEST_CASE("curve_configuration_roundtrip_through_the_database", tags) {
    ores::testing::scoped_database_helper h;
    write_conventions(h.context());

    const auto original = load<curveconfiguration>("curveconfig.xml");
    const auto mapped = curve_configuration_mapper::map(original);
    REQUIRE(!mapped.definitions.empty());
    REQUIRE(!mapped.segments.empty());
    REQUIRE(!mapped.quotes.empty());

    write(h.context(), mapped);
    const auto back = read_back(h.context(), mapped);

    CHECK(back.sections.size() == mapped.sections.size());
    CHECK(back.definitions.size() == mapped.definitions.size());
    CHECK(back.yield_curves.size() == mapped.yield_curves.size());
    CHECK(back.segments.size() == mapped.segments.size());
    CHECK(back.quotes.size() == mapped.quotes.size());

    curveconfiguration exported;
    load_data(save_data(curve_configuration_mapper::reverse(back)), exported);
    const auto difference = ores::ore::xml::parsed_text_difference(original, exported, example);
    INFO(difference);
    CHECK(difference.empty());
}

TEST_CASE("a segment naming a convention the tenant does not hold is refused", tags) {
    ores::testing::scoped_database_helper h;
    write_conventions(h.context());

    auto mapped = curve_configuration_mapper::map(load<curveconfiguration>("curveconfig.xml"));
    REQUIRE(!mapped.segments.empty());
    mapped.segments.front().conventions = "NO-SUCH-CONVENTION";

    curve_configuration_repository().write(h.context(), mapped.config);
    curve_definition_repository().write(h.context(), mapped.definitions);
    CHECK_THROWS(curve_segment_repository().write(h.context(), mapped.segments.front()));
}

TEST_CASE("a yield curve naming a day counter ORE does not spell is refused", tags) {
    ores::testing::scoped_database_helper h;

    auto mapped = curve_configuration_mapper::map(load<curveconfiguration>("curveconfig.xml"));
    REQUIRE(!mapped.yield_curves.empty());
    mapped.yield_curves.front().day_counter = "Actual/365 Fixed";

    curve_configuration_repository().write(h.context(), mapped.config);
    curve_definition_repository().write(h.context(), mapped.definitions);
    CHECK_THROWS(yield_curve_repository().write(h.context(), mapped.yield_curves.front()));
}

TEST_CASE("a segment of a type the vocabulary does not hold is refused", tags) {
    ores::testing::scoped_database_helper h;
    write_conventions(h.context());

    auto mapped = curve_configuration_mapper::map(load<curveconfiguration>("curveconfig.xml"));
    REQUIRE(!mapped.segments.empty());
    mapped.segments.front().segment_type = "Nonexistent";

    curve_configuration_repository().write(h.context(), mapped.config);
    curve_definition_repository().write(h.context(), mapped.definitions);
    CHECK_THROWS(curve_segment_repository().write(h.context(), mapped.segments.front()));
}
