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
#include "ores.platform/filesystem/scoped_temp_directory.hpp"
#include "ores.platform/filesystem/scoped_temp_file.hpp"
#include "ores.reporting.core/service/engine_error_log.hpp"
#include "ores.storage.core/filesystem/archiver.hpp"
#include "engine_log_fixture.hpp"
#include <algorithm>
#include <boost/json.hpp>
#include <catch2/catch_test_macros.hpp>
#include <cstddef>
#include <filesystem>
#include <fstream>
#include <string>
#include <string_view>
#include <vector>

namespace {

const std::string tags("[service]");

/**
 * @brief Reads a file whole, as the bytes a transfer would hand the reader.
 */
std::vector<std::byte> read_whole(const std::filesystem::path& file) {
    std::ifstream in(file, std::ios::binary);
    std::vector<std::byte> bytes;
    char buffer[4096];
    while (in.read(buffer, sizeof(buffer)) || in.gcount()) {
        const auto read = static_cast<std::size_t>(in.gcount());
        for (std::size_t i = 0; i < read; ++i)
            bytes.push_back(static_cast<std::byte>(buffer[i]));
    }
    return bytes;
}

/**
 * @brief Packs @p log into an archive under the name the engine gives its log.
 *
 * A run's output is an archive, so a test that reads one builds an archive:
 * that is the shape the reader sees in production, and the archive is the
 * engine's own container rather than a format invented for the test.
 */
std::vector<std::byte> make_archive(const std::string& log) {
    const ores::platform::filesystem::scoped_temp_directory staged;
    std::ofstream stream(staged.path() / ores::reporting::tests::structured_log_name,
                         std::ios::binary);
    stream << log;
    stream.close();

    const ores::platform::filesystem::scoped_temp_file archive;
    ores::storage::filesystem::archiver::pack(staged.path(), archive.path());
    return read_whole(archive.path());
}

}

TEST_CASE("a run that failed its analytics states each error with its own scope and subject",
          tags) {
    using ores::reporting::service::read_engine_errors_from_archive;
    using ores::reporting::tests::failing_run_log;

    const auto archive = make_archive(std::string(failing_run_log));
    const auto read = read_engine_errors_from_archive(archive);

    REQUIRE(read.ok());
    REQUIRE(read.errors.size() == 19);

    const auto count_of = [&](std::string_view scope) {
        return std::ranges::count_if(read.errors, [&](const auto& error) {
            return error.scope == scope;
        });
    };
    CHECK(count_of("Trade") == 16);
    CHECK(count_of("Analytics") == 3);

    const auto& first = read.errors.front();
    CHECK(first.scope == "Trade");
    CHECK(first.severity == "error");
    CHECK(first.message ==
          "did not find object  of type yield curve / ibor index under configuration 'default' or "
          "'default' in YieldCurves");
    CHECK(first.subject == "trade");
    CHECK(first.subject_id == "033b4053-fe1d-5901-bc2f-7d95afe91f03");
    const auto first_detail = boost::json::parse(first.detail).as_object();
    CHECK(first_detail.at("exceptionType") ==
          "Error building trade for context 'analytic/PRICING'");

    // The analytics that had nothing to price: the engine says so once per
    // analytic, and states it about the analytic and not about a trade.
    const auto find_message = [&](std::string_view text) {
        return std::ranges::find_if(read.errors, [&](const auto& error) {
            return error.message == text;
        });
    };

    const auto analytic = find_message(
        "Portfolio does not contain any built trades, context is 'analytic/PRICING'");
    REQUIRE(analytic != read.errors.end());
    CHECK(analytic->scope == "Analytics");
    CHECK(analytic->severity == "error");
    CHECK(analytic->subject == "analytic");
    CHECK(analytic->subject_id == "PRICING");

    const auto warning =
        find_message("Error in ORE analytics: Failed to run analytics PRICING,SA_CCR");
    REQUIRE(warning != read.errors.end());
    CHECK(warning->scope == "Analytics");
    CHECK(warning->severity == "warning");
    const auto warning_detail = boost::json::parse(warning->detail).as_object();
    CHECK(warning_detail.at("warningType") == "Error");
}

TEST_CASE("an analytic error that names a trade is about the trade and not the analytic", tags) {
    using ores::reporting::service::parse_engine_log_line;

    const auto parsed = parse_engine_log_line(
        R"ORELOG({"category": "Error", "group": "Analytics", "message": "SA-CCR trade data not yet implemented for trade type Bond.", "sub_fields": [{"name": "analyticType", "value": "SA-CCR"}, {"name": "tradeId", "value": "51957396-e970-5419-9e0b-b80373a4e30e"}]})ORELOG");

    REQUIRE(parsed.has_value());
    CHECK(parsed->scope == "Analytics");
    CHECK(parsed->subject == "trade");
    CHECK(parsed->subject_id == "51957396-e970-5419-9e0b-b80373a4e30e");
}

TEST_CASE("a curve error names the curve and a configuration warning names the input", tags) {
    using ores::reporting::service::parse_engine_log_line;

    const auto curve = parse_engine_log_line(
        R"ORELOG({"category": "Error", "group": "Curve", "message": "yield curve building failed for curve(s) Yield/EUR/EUR1D on date 2026-10-15 while processing Yield/EUR/EUR1D: Could not find quote for Mandatory ID MM/RATE/EUR/0D/1D with as of date 2026-10-15", "sub_fields": [{"name": "curveId", "value": "[IndexCurve(EUR-ESTER,Yield/EUR/EUR1D)]"}, {"name": "exceptionType", "value": "Failed to Build Curve"}]})ORELOG");

    REQUIRE(curve.has_value());
    CHECK(curve->scope == "Curve");
    CHECK(curve->severity == "error");
    CHECK(curve->subject == "curve");
    CHECK(curve->subject_id == "[IndexCurve(EUR-ESTER,Yield/EUR/EUR1D)]");
    const auto curve_detail = boost::json::parse(curve->detail).as_object();
    CHECK(curve_detail.at("exceptionType") == "Failed to Build Curve");

    const auto configuration = parse_engine_log_line(
        R"ORELOG({"category": "Warning", "group": "Configuration", "message": "Input configuration was not provided. The default values will be used for all netting sets in the portfolio", "sub_fields": [{"name": "configurationType", "value": "Netting set definitions"}, {"name": "warningType", "value": "Validating input configurations"}, {"name": "analyticType", "value": "SA-CCR"}]})ORELOG");

    REQUIRE(configuration.has_value());
    CHECK(configuration->scope == "Configuration");
    CHECK(configuration->severity == "warning");
    CHECK(configuration->subject == "configuration");
    CHECK(configuration->subject_id == "Netting set definitions");
}

TEST_CASE("a run whose analytics all ran records no error", tags) {
    using ores::reporting::service::read_engine_errors_from_archive;
    using ores::reporting::service::parse_engine_log_line;

    // The log the engine writes when everything ran: the same shape as a
    // failing run's, with no message about a subject in it.
    const auto archive = make_archive(
        R"ORELOG({"level": "NOTICE", "message": "ORE analytics starting"})ORELOG" "\n"
        R"ORELOG({"level": "NOTICE", "message": "report pricingstats written to Output/pricingstats.csv"})ORELOG" "\n");
    const auto read = read_engine_errors_from_archive(archive);

    CHECK(read.ok());
    CHECK(read.errors.empty());

    // A message that names no group is not an error the engine stated about a
    // subject, and neither is a plain log line.
    CHECK_FALSE(parse_engine_log_line(
                    R"ORELOG({"level": "NOTICE", "message": "ORE analytics starting"})ORELOG")
                    .has_value());
    CHECK_FALSE(parse_engine_log_line("NOTICE   [2026-Oct-16 13:42:25] : ORE analytics starting")
                    .has_value());
}
