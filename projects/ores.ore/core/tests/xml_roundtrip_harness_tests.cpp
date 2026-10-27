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

#include "ores.ore.core/domain/conventions_mapper.hpp"
#include "ores.ore.core/domain/conventions_package.hpp"
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

using ores::ore::domain::conventions;
using ores::ore::domain::conventions_mapper;
using ores::ore::domain::conventions_package;

std::filesystem::path corpus_root() {
    return ores::testing::project_root::resolve("external/ore/examples");
}

// A mapper pair that loses the document, so the harness's failure path has a
// caller. A rig that can only pass is not evidence that it checks anything.
conventions_package lose_everything(const conventions&) {
    return {};
}

conventions rebuild_nothing(const conventions_package&) {
    return {};
}

}

TEST_CASE("roundtrip_harness_first_difference_is_empty_for_equal_documents", "[ore][xml][roundtrip][harness]") {
    CHECK(ores::ore::xml::first_difference("<ORE/>", "<ORE/>").empty());
    CHECK(ores::ore::xml::first_difference("", "").empty());
}

TEST_CASE("roundtrip_harness_first_difference_names_the_byte_and_the_line", "[ore][xml][roundtrip][harness]") {
    const std::string lhs = "<ORE>\n  <Setup/>\n</ORE>\n";
    const std::string rhs = "<ORE>\n  <Setap/>\n</ORE>\n";

    const auto difference = ores::ore::xml::first_difference(lhs, rhs);

    CHECK(difference.find("byte 12") != std::string::npos);
    CHECK(difference.find("line 2") != std::string::npos);
    CHECK(difference.find("<Setup/>") != std::string::npos);
    CHECK(difference.find("<Setap/>") != std::string::npos);
}

TEST_CASE("roundtrip_harness_names_the_difference_when_one_document_is_a_prefix", "[ore][xml][roundtrip][harness]") {
    const auto difference = ores::ore::xml::first_difference("<ORE/>", "<ORE/>\n");

    CHECK(difference.find("(end of document)") != std::string::npos);
}

TEST_CASE("roundtrip_harness_walks_only_the_kind_it_is_given", "[ore][xml][roundtrip][harness]") {
    const auto kind = ores::ore::xml::make_roundtrip_kind<conventions, conventions_package>(
        "conventions",
        "conventions",
        &conventions_mapper::read,
        &conventions_mapper::write,
        &ores::ore::domain::split_conventions,
        &ores::ore::domain::join_conventions,
        ores::ore::domain::conventions_difference);

    const auto walk = ores::ore::xml::walk_kind(kind, corpus_root());

    CHECK(walk.kind == "conventions");
    CHECK(walk.files == 72);
}

TEST_CASE("roundtrip_harness_reports_a_mapper_that_loses_the_document", "[ore][xml][roundtrip][harness]") {
    const auto kind = ores::ore::xml::make_roundtrip_kind<conventions, conventions_package>(
        "conventions, lossy",
        "conventions",
        &conventions_mapper::read,
        &conventions_mapper::write,
        lose_everything,
        rebuild_nothing,
        ores::ore::domain::conventions_difference);

    const auto walk = ores::ore::xml::walk_kind(kind, corpus_root());

    CHECK(walk.files == 72);
    CHECK(walk.passed == 0);
    CHECK(walk.failures.size() == static_cast<std::size_t>(walk.files));
}

TEST_CASE("roundtrip_harness_finds_no_files_for_a_kind_the_corpus_does_not_hold", "[ore][xml][roundtrip][harness]") {
    const auto kind = ores::ore::xml::make_roundtrip_kind<conventions, conventions_package>(
        "basel traffic light",
        "baselTrafficLight",
        &conventions_mapper::read,
        &conventions_mapper::write,
        &ores::ore::domain::split_conventions,
        &ores::ore::domain::join_conventions,
        ores::ore::domain::conventions_difference);

    const auto walk = ores::ore::xml::walk_kind(kind, corpus_root());

    CHECK(walk.files == 0);
    CHECK(walk.passed == 0);
}
