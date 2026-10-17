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
#include "ores.ore.core/xml/exporter.hpp"
#include "ores.ore.core/xml/importer.hpp"
#include "ores.testing/project_root.hpp"
#include <catch2/catch_test_macros.hpp>
#include <filesystem>
#include <string>

namespace {

const std::string_view test_suite("ores.ore.calendaradjustment.roundtrip.tests");
const std::string tags("[ore][xml][roundtrip][calendaradjustment]");

using namespace ores::logging;

/**
 * @brief Reads a document, writes it back, reads the writing, and compares.
 *
 * This is the whole path a user takes: a file arrives, the codec binds it, the
 * message carries it, the table writes a document, and the codec reads that
 * again. What is compared is the message the two readings produce, so a field
 * the writing drops is named rather than counted.
 */
void round_trip_messages(const std::filesystem::path& path) {
    auto lg(make_logger(test_suite));

    BOOST_LOG_SEV(lg, debug) << "Importing: " << path.string();

    const auto original = ores::ore::xml::importer::import_calendar_adjustments(path);
    REQUIRE_FALSE(original.empty());

    const auto written = ores::ore::xml::exporter::export_calendar_adjustments(original);
    CHECK(written.find("<CalendarAdjustments>") != std::string::npos);

    const auto reread = ores::ore::xml::importer::import_calendar_adjustments_text(written);
    REQUIRE(reread.size() == original.size());

    for (std::size_t i = 0; i < original.size(); ++i) {
        INFO("calendar " << i);
        CHECK(reread[i].calendar_name == original[i].calendar_name);
        CHECK(reread[i].base_calendar == original[i].base_calendar);
        CHECK(reread[i].additional_holidays == original[i].additional_holidays);
        CHECK(reread[i].additional_business_days == original[i].additional_business_days);
    }
}

}

TEST_CASE("calendar_adjustments_round_trip_through_the_codec", tags) {
    round_trip_messages(
        ores::testing::project_root::resolve("external/ore/examples/Input/calendaradjustment.xml"));
}

TEST_CASE("every_calendar_adjustment_in_the_corpus_round_trips", tags) {
    auto lg(make_logger(test_suite));

    const auto root = ores::testing::project_root::resolve("external/ore/examples");
    REQUIRE(std::filesystem::is_directory(root));

    std::size_t files = 0;
    for (const auto& entry : std::filesystem::recursive_directory_iterator(root)) {
        if (!entry.is_regular_file() || entry.path().filename() != "calendaradjustment.xml")
            continue;
        ++files;
        INFO(entry.path().string());
        round_trip_messages(entry.path());
    }
    CHECK(files > 0);
}
