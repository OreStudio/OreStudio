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
#include "ores.qt.headless/TestScenarioResultsWriter.hpp"
#include <QDir>
#include <QFile>
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
                           "** Open the dialog\n"
                           "\n"
                           "Click the toolbar's Open icon and pick a file.\n"
                           "\n"
                           "** Click save\n"
                           "\n"
                           "Click the Save icon and confirm no error appears.\n"
                           "\n"
                           "* Results\n"
                           "\n"
                           "| Field | Value |\n"
                           "|-------+-------|\n"
                           "| Status |  |\n"
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

TEST_CASE(
    "write_scenario_results rewrites Results and each step's Result, leaving the rest untouched",
    tags) {
    QTemporaryDir tmp;
    REQUIRE(tmp.isValid());
    const QString path = write_temp_doc(tmp.path(), sample_doc);

    ores::qt::scenario_result result;
    result.status = "PASSED";
    result.steps = {
        {"Open the dialog", "PASS", "Opened fine."},
        {"Click save", "PASS", {}},
    };

    ores::qt::environment_metadata env;
    env.branch = "feature/qa-validation-runner-panel";
    env.commit = "abc1234";
    env.worktree = "bright_faraday";

    REQUIRE(ores::qt::write_scenario_results(path, result, env));

    QFile file(path);
    REQUIRE(file.open(QIODevice::ReadOnly | QIODevice::Text));
    const QString written = QTextStream(&file).readAll();

    // Untouched sections/step instructions still present verbatim.
    REQUIRE(written.contains("#+title: Test Scenario: Sample"));
    REQUIRE(written.contains("Click the toolbar's Open icon and pick a file."));
    REQUIRE(written.contains("Click the Save icon and confirm no error appears."));
    REQUIRE(written.contains("[[id:AAAA][Some task]]"));

    // Overall Results rewritten.
    REQUIRE(written.contains("| Status        | PASSED |"));
    REQUIRE(written.contains("| Branch        | feature/qa-validation-runner-panel |"));
    REQUIRE(written.contains("| Commit        | abc1234 |"));
    REQUIRE(written.contains("| Worktree      | bright_faraday |"));

    // Each step gets its own *** Result child heading, nested one level
    // deeper than the step itself (** -> ***).
    REQUIRE(written.contains("*** Result"));
    REQUIRE(written.contains("| Status | PASS |"));
    REQUIRE(written.contains("| Notes  | Opened fine. |"));

    // The scenario-level * Notes section is no longer touched by this
    // writer — per-step notes replaced it — so its content survives.
    REQUIRE(written.contains("Pre-existing notes text"));
}

TEST_CASE("write_scenario_results nests a step's Result one level deeper for a multi-client step",
          tags) {
    QTemporaryDir tmp;
    REQUIRE(tmp.isValid());
    const QString doc = "* Scenario Info\n"
                        "\n"
                        "* Steps\n"
                        "\n"
                        "** blue\n"
                        "*** Open the counterparty lookup\n"
                        "** red\n"
                        "*** Confirm the notification arrived\n"
                        "\n"
                        "* Results\n"
                        "\n"
                        "| Field | Value |\n"
                        "|-------+-------|\n";
    const QString path = write_temp_doc(tmp.path(), doc);

    ores::qt::scenario_result result;
    result.status = "PASSED";
    result.steps = {
        {"Open the counterparty lookup", "PASS", {}},
        {"Confirm the notification arrived", "FAIL", "Never showed up."},
    };

    REQUIRE(ores::qt::write_scenario_results(path, result, {}));

    QFile file(path);
    REQUIRE(file.open(QIODevice::ReadOnly | QIODevice::Text));
    const QString written = QTextStream(&file).readAll();

    // Nested one level deeper than the *** step heading: ****.
    REQUIRE(written.contains("**** Result"));
    REQUIRE(written.contains("| Status | FAIL |"));
    REQUIRE(written.contains("| Notes  | Never showed up. |"));
}

TEST_CASE("write_scenario_results disambiguates a step title shared by two clients", tags) {
    QTemporaryDir tmp;
    REQUIRE(tmp.isValid());
    const QString doc = "* Scenario Info\n"
                        "\n"
                        "* Steps\n"
                        "\n"
                        "** blue\n"
                        "*** Log in\n"
                        "** red\n"
                        "*** Log in\n"
                        "\n"
                        "* Results\n"
                        "\n"
                        "| Field | Value |\n"
                        "|-------+-------|\n";
    const QString path = write_temp_doc(tmp.path(), doc);

    ores::qt::scenario_result result;
    result.status = "FAILED";
    result.steps = {
        {"Log in", "PASS", "Blue logged in fine.", "blue"},
        {"Log in", "FAIL", "Red couldn't log in.", "red"},
    };

    REQUIRE(ores::qt::write_scenario_results(path, result, {}));

    QFile file(path);
    REQUIRE(file.open(QIODevice::ReadOnly | QIODevice::Text));
    const QString written = QTextStream(&file).readAll();

    // Both steps get their own Result, correctly attributed — not
    // collapsed onto the same (first) "Log in" match.
    REQUIRE(written.count(QStringLiteral("**** Result")) == 2);
    REQUIRE(written.contains("| Notes  | Blue logged in fine. |"));
    REQUIRE(written.contains("| Notes  | Red couldn't log in. |"));

    // Blue's Result comes before red's own "Log in" heading.
    const int blueResult = written.indexOf("Blue logged in fine.");
    const int redHeading = written.indexOf("** red");
    const int redResult = written.indexOf("Red couldn't log in.");
    REQUIRE(blueResult < redHeading);
    REQUIRE(redHeading < redResult);
}

TEST_CASE(
    "write_scenario_results re-running a step replaces its previous Result, not duplicates it",
    tags) {
    QTemporaryDir tmp;
    REQUIRE(tmp.isValid());
    const QString path = write_temp_doc(tmp.path(), sample_doc);

    ores::qt::scenario_result first;
    first.status = "FAILED";
    first.steps = {{"Open the dialog", "FAIL", "First attempt failed."}};
    REQUIRE(ores::qt::write_scenario_results(path, first, {}));

    ores::qt::scenario_result second;
    second.status = "PASSED";
    second.steps = {{"Open the dialog", "PASS", "Second attempt worked."}};
    REQUIRE(ores::qt::write_scenario_results(path, second, {}));

    QFile file(path);
    REQUIRE(file.open(QIODevice::ReadOnly | QIODevice::Text));
    const QString written = QTextStream(&file).readAll();

    REQUIRE(written.contains("Second attempt worked."));
    REQUIRE_FALSE(written.contains("First attempt failed."));
    REQUIRE(written.count(QStringLiteral("*** Result")) == 1);
}

TEST_CASE("write_scenario_results skips a step whose title isn't found in the doc", tags) {
    QTemporaryDir tmp;
    REQUIRE(tmp.isValid());
    const QString path = write_temp_doc(tmp.path(), sample_doc);

    ores::qt::scenario_result result;
    result.status = "PASSED";
    result.steps = {{"This step does not exist", "PASS", {}}};

    REQUIRE(ores::qt::write_scenario_results(path, result, {}));

    QFile file(path);
    REQUIRE(file.open(QIODevice::ReadOnly | QIODevice::Text));
    const QString written = QTextStream(&file).readAll();
    REQUIRE_FALSE(written.contains("This step does not exist"));
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
