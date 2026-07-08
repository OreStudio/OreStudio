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
#include "ores.qt/TestScenarioResultsWriter.hpp"
#include <QDir>
#include <QFile>
#include <QStandardPaths>
#include <QTemporaryDir>
#include <QTextStream>
#include <catch2/catch_test_macros.hpp>

namespace {

const std::string tags("[test_scenario_results_writer]");

const QString sample_doc = "#+title: Test Scenario: Sample\n"
                           "#+type: test_scenario\n"
                           "\n"
                           "* Scenario Info\n"
                           "\n"
                           "| Field | Value |\n"
                           "|-------+-------|\n"
                           "| Verifies task | [[id:AAAA][Some task]] |\n"
                           "\n"
                           "* Steps\n"
                           "\n"
                           "- [ ] Open the dialog\n"
                           "- [ ] Click save\n"
                           "\n"
                           "* Results\n"
                           "\n"
                           "| Field | Value |\n"
                           "|-------+-------|\n"
                           "| Status |  |\n"
                           "\n"
                           "** Step results\n"
                           "\n"
                           "| Step | Passed |\n"
                           "|------+--------|\n"
                           "\n"
                           "* Notes\n"
                           "\n"
                           "(Pre-existing notes text that should be replaced.)\n";

QString write_temp_doc(const QString& dir, const QString& content) {
    const QString path = dir + "/scenario.org";
    QFile file(path);
    REQUIRE(file.open(QIODevice::WriteOnly | QIODevice::Text));
    QTextStream out(&file);
    out << content;
    file.close();
    return path;
}

}

TEST_CASE("write_scenario_results rewrites Results and Notes, leaving everything else untouched",
          tags) {
    QTemporaryDir tmp;
    REQUIRE(tmp.isValid());
    const QString path = write_temp_doc(tmp.path(), sample_doc);

    ores::qt::scenario_result result;
    result.status = "PASSED";
    result.steps = {
        {"Open the dialog", true, {}},
        {"Click save", true, {}},
    };
    result.notes = "Everything worked as expected.";

    ores::qt::environment_metadata env;
    env.branch = "feature/qa-validation-runner-panel";
    env.commit = "abc1234";
    env.worktree = "bright_faraday";

    REQUIRE(ores::qt::write_scenario_results(path, result, env));

    QFile file(path);
    REQUIRE(file.open(QIODevice::ReadOnly | QIODevice::Text));
    const QString written = QTextStream(&file).readAll();

    // Untouched sections still present verbatim.
    REQUIRE(written.contains("#+title: Test Scenario: Sample"));
    REQUIRE(written.contains("- [ ] Open the dialog"));
    REQUIRE(written.contains("- [ ] Click save"));
    REQUIRE(written.contains("[[id:AAAA][Some task]]"));

    // Results rewritten.
    REQUIRE(written.contains("| Status        | PASSED |"));
    REQUIRE(written.contains("| Branch        | feature/qa-validation-runner-panel |"));
    REQUIRE(written.contains("| Commit        | abc1234 |"));
    REQUIRE(written.contains("| Worktree      | bright_faraday |"));
    REQUIRE(written.contains("| Open the dialog | Yes |"));
    REQUIRE(written.contains("| Click save | Yes |"));

    // Notes rewritten, old placeholder text gone.
    REQUIRE(written.contains("Everything worked as expected."));
    REQUIRE_FALSE(written.contains("Pre-existing notes text"));
}

TEST_CASE("write_scenario_results adds a Client column only for multi-client runs", tags) {
    QTemporaryDir tmp;
    REQUIRE(tmp.isValid());
    const QString path = write_temp_doc(tmp.path(), sample_doc);

    ores::qt::scenario_result result;
    result.status = "PASSED";
    result.steps = {
        {"Open the dialog on blue", true, "blue"},
        {"Confirm notification on red", true, "red"},
    };

    REQUIRE(ores::qt::write_scenario_results(path, result, {}));

    QFile file(path);
    REQUIRE(file.open(QIODevice::ReadOnly | QIODevice::Text));
    const QString written = QTextStream(&file).readAll();

    REQUIRE(written.contains("| Step | Client | Passed |"));
    REQUIRE(written.contains("| Open the dialog on blue | blue | Yes |"));
    REQUIRE(written.contains("| Confirm notification on red | red | Yes |"));
}

TEST_CASE("write_scenario_results returns false when the doc has no Results heading", tags) {
    QTemporaryDir tmp;
    REQUIRE(tmp.isValid());
    const QString path = write_temp_doc(tmp.path(), "#+title: No results section\n\n* Steps\n");

    REQUIRE_FALSE(ores::qt::write_scenario_results(path, {}, {}));
}

TEST_CASE("write_scenario_results returns false for a missing file", tags) {
    REQUIRE_FALSE(ores::qt::write_scenario_results("/no/such/scenario.org", {}, {}));
}
