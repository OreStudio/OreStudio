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
#include <QDateTime>
#include <QFile>
#include <QRegularExpression>
#include <QTextStream>
#include <algorithm>

namespace ores::qt {

namespace {

const QRegularExpression& heading_re() {
    static const QRegularExpression re(R"(^(\*+)\s+(.+?)\s*$)");
    return re;
}

/**
 * @brief Find the line span [begin, end) of the top-level heading
 * whose title matches @p title exactly, where `end` is the first
 * subsequent line that is a heading of level <= the found heading's
 * level (or the file's line count, at EOF).
 *
 * Returns {-1, -1} if no such heading exists.
 */
std::pair<int, int> find_section_span(const QStringList& lines, const QString& title) {
    int begin = -1;
    int level = 0;
    for (int i = 0; i < lines.size(); ++i) {
        const auto match = heading_re().match(lines[i]);
        if (!match.hasMatch())
            continue;
        if (begin < 0) {
            if (match.captured(2) == title) {
                begin = i;
                level = static_cast<int>(match.captured(1).size());
            }
            continue;
        }
        if (static_cast<int>(match.captured(1).size()) <= level)
            return {begin, i};
    }
    if (begin < 0)
        return {-1, -1};
    return {begin, lines.size()};
}

QStringList render_results_section(const scenario_result& result,
                                   const environment_metadata& environment) {
    QStringList out;
    out << "* Results" << "";
    out << "| Field         | Value |" << "|---------------+-------|";
    out << ("| Status        | " + result.status + " |");
    out << ("| Completed at  | " + QDateTime::currentDateTimeUtc().toString(Qt::ISODate) + " |");
    out << ("| Branch        | " + environment.branch + " |");
    out << ("| Commit        | " + environment.commit + " |");
    out << ("| Worktree      | " + environment.worktree + " |");
    out << "";
    out << "** Step results" << "";
    // The Client column only appears when at least one step actually
    // used it — a single-client run's table stays exactly as simple
    // as the common case needs.
    const bool has_clients = std::any_of(result.steps.begin(),
                                         result.steps.end(),
                                         [](const step_result& s) { return !s.client.isEmpty(); });
    if (has_clients) {
        out << "| Step | Client | Passed |" << "|------+--------+--------|";
        for (const auto& step : result.steps) {
            out << ("| " + step.step_text + " | " + step.client + " | " +
                    QString(step.passed ? "Yes" : "No") + " |");
        }
    } else {
        out << "| Step | Passed |" << "|------+--------|";
        for (const auto& step : result.steps)
            out << ("| " + step.step_text + " | " + QString(step.passed ? "Yes" : "No") + " |");
    }
    out << "";
    return out;
}

QStringList render_notes_section(const scenario_result& result) {
    QStringList out;
    out << "* Notes" << "";
    if (!result.notes.isEmpty())
        out << result.notes;
    return out;
}

}

bool write_scenario_results(const QString& path,
                            const scenario_result& result,
                            const environment_metadata& environment) {
    QFile file(path);
    if (!file.open(QIODevice::ReadOnly | QIODevice::Text))
        return false;
    QStringList lines;
    {
        QTextStream in(&file);
        while (!in.atEnd())
            lines << in.readLine();
    }
    file.close();

    const auto results_span = find_section_span(lines, "Results");
    if (results_span.first < 0)
        return false;

    // Splice Notes first if it comes after Results (so Results' span
    // indices, computed against the original list, aren't invalidated
    // by an earlier splice shifting later line numbers).
    const auto notes_span = find_section_span(lines, "Notes");

    struct splice final {
        int begin;
        int end;
        QStringList replacement;
    };
    std::vector<splice> splices;
    splices.push_back(
        {results_span.first, results_span.second, render_results_section(result, environment)});
    if (notes_span.first >= 0)
        splices.push_back({notes_span.first, notes_span.second, render_notes_section(result)});

    std::sort(splices.begin(), splices.end(), [](const splice& a, const splice& b) {
        return a.begin > b.begin;
    });
    for (const auto& s : splices)
        lines = lines.mid(0, s.begin) + s.replacement + lines.mid(s.end);

    if (!file.open(QIODevice::WriteOnly | QIODevice::Text | QIODevice::Truncate))
        return false;
    QTextStream out(&file);
    for (const auto& line : lines)
        out << line << "\n";
    return true;
}

}
