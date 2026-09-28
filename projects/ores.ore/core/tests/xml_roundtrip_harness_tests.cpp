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
#include "ores.ore.core/domain/credit_simulation_mapper.hpp"
#include "ores.ore.core/domain/domain.hpp"
#include "ores.ore.core/xml/roundtrip_harness.hpp"
#include "ores.testing/project_root.hpp"
#include <catch2/catch_test_macros.hpp>
#include <cstddef>
#include <filesystem>
#include <string>

/**
 * @file xml_roundtrip_harness_tests.cpp
 * @brief The harness every document kind registers with.
 *
 * The harness owns the corpus walk, the count and the comparison; a kind owns
 * its mapper pair. These cases prove the comparison can fail, that the walk
 * visits only the kind's files, and that the one kind with a mapper today
 * round trips every file of it in the vendored corpus.
 */

namespace {

const std::string tags("[ore][xml][roundtrip][harness]");

using ores::ore::domain::credit_simulation_difference;
using ores::ore::domain::credit_simulation_mapper;
using ores::ore::domain::creditsimulation;
using ores::ore::domain::mapped_credit_simulation;

std::filesystem::path corpus_root() {
    return ores::testing::project_root::resolve("external/ore/examples");
}

// A mapper pair that loses the document, so the harness's failure path has a
// caller. A rig that can only pass is not evidence that it checks anything.
mapped_credit_simulation lose_everything(const creditsimulation&) {
    return {};
}

creditsimulation rebuild_nothing(const mapped_credit_simulation&) {
    return {};
}

}

TEST_CASE("roundtrip_harness_first_difference_is_empty_for_equal_documents", tags) {
    CHECK(ores::ore::xml::first_difference("<ORE/>", "<ORE/>").empty());
    CHECK(ores::ore::xml::first_difference("", "").empty());
}

TEST_CASE("roundtrip_harness_first_difference_names_the_byte_and_the_line", tags) {
    const std::string lhs = "<ORE>\n  <Setup/>\n</ORE>\n";
    const std::string rhs = "<ORE>\n  <Setap/>\n</ORE>\n";

    const auto difference = ores::ore::xml::first_difference(lhs, rhs);

    CHECK(difference.find("byte 12") != std::string::npos);
    CHECK(difference.find("line 2") != std::string::npos);
    CHECK(difference.find("<Setup/>") != std::string::npos);
    CHECK(difference.find("<Setap/>") != std::string::npos);
}

TEST_CASE("roundtrip_harness_names_the_difference_when_one_document_is_a_prefix", tags) {
    const auto difference = ores::ore::xml::first_difference("<ORE/>", "<ORE/>\n");

    CHECK(difference.find("(end of document)") != std::string::npos);
}

TEST_CASE("roundtrip_harness_walks_only_the_kind_it_is_given", tags) {
    const auto kind = ores::ore::xml::make_roundtrip_kind<creditsimulation, mapped_credit_simulation>(
        "credit simulation", "creditsimulation", credit_simulation_mapper::map,
        credit_simulation_mapper::reverse, credit_simulation_difference);

    const auto walk = ores::ore::xml::walk_kind(kind, corpus_root());

    CHECK(walk.kind == "credit simulation");
    CHECK(walk.files == 14);
}

TEST_CASE("roundtrip_harness_round_trips_every_credit_simulation_in_the_corpus", tags) {
    const auto kind = ores::ore::xml::make_roundtrip_kind<creditsimulation, mapped_credit_simulation>(
        "credit simulation", "creditsimulation", credit_simulation_mapper::map,
        credit_simulation_mapper::reverse, credit_simulation_difference);

    const auto walk = ores::ore::xml::walk_kind(kind, corpus_root());

    for (const auto& failure : walk.failures)
        WARN(failure);

    CHECK(walk.files == 14);
    CHECK(walk.passed == walk.files);
    CHECK(walk.failures.empty());
}

TEST_CASE("roundtrip_harness_reports_a_mapper_that_loses_the_document", tags) {
    const auto kind = ores::ore::xml::make_roundtrip_kind<creditsimulation, mapped_credit_simulation>(
        "credit simulation, lossy", "creditsimulation", lose_everything, rebuild_nothing,
        credit_simulation_difference);

    const auto walk = ores::ore::xml::walk_kind(kind, corpus_root());

    CHECK(walk.files == 14);
    CHECK(walk.passed == 0);
    CHECK(walk.failures.size() == static_cast<std::size_t>(walk.files));
}

TEST_CASE("roundtrip_harness_finds_no_files_for_a_kind_the_corpus_does_not_hold", tags) {
    const auto kind = ores::ore::xml::make_roundtrip_kind<creditsimulation, mapped_credit_simulation>(
        "basel traffic light", "baselTrafficLight", credit_simulation_mapper::map,
        credit_simulation_mapper::reverse, credit_simulation_difference);

    const auto walk = ores::ore::xml::walk_kind(kind, corpus_root());

    CHECK(walk.files == 0);
    CHECK(walk.passed == 0);
}
