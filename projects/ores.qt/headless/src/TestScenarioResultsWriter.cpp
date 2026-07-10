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
 * @brief A heading's line span [begin, end) — up to the next heading
 * at the same or a shallower level, or EOF — and its star-count level.
 * Default-constructed (begin < 0) means "not found".
 */
struct heading_match final {
    int begin = -1;
    int end = -1;
    int level = 0;

    bool found() const {
        return begin >= 0;
    }
};

/**
 * @brief Find the span of the first heading titled exactly @p title
 * within lines[range_begin, range_end).
 */
heading_match
find_heading(const QStringList& lines, const QString& title, int range_begin, int range_end) {
    int begin = -1;
    int level = 0;
    for (int i = range_begin; i < range_end; ++i) {
        const auto match = heading_re().match(lines[i]);
        if (!match.hasMatch())
            continue;
        const int lvl = static_cast<int>(match.captured(1).size());
        if (begin < 0) {
            if (match.captured(2) == title) {
                begin = i;
                level = lvl;
            }
            continue;
        }
        if (lvl <= level)
            return {begin, i, level};
    }
    if (begin < 0)
        return {};
    return {begin, range_end, level};
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
    return out;
}

/**
 * @brief Render a step's =*** Result= child heading. @p level is the
 * step heading's own level plus one, so it nests correctly whether the
 * step is a direct child of =* Steps= or, for a multi-client scenario,
 * nested one level deeper under a per-client sub-heading.
 */
QStringList render_step_result(int level, const QString& status, const QString& notes) {
    const QString stars(level, QChar('*'));
    QStringList out;
    out << (stars + " Result") << "";
    out << "| Field  | Value |" << "|--------+-------|";
    out << ("| Status | " + status + " |");
    if (!notes.isEmpty()) {
        // Notes render as a single table row/cell — collapse embedded
        // newlines and pipes so they can't be mistaken for row/column
        // separators.
        QString flattened = notes;
        flattened.replace(QChar('\n'), QStringLiteral("; "));
        flattened.replace(QChar('|'), QStringLiteral("/"));
        out << ("| Notes  | " + flattened + " |");
    }
    out << "";
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

    const auto results_span = find_heading(lines, "Results", 0, lines.size());
    if (!results_span.found())
        return false;

    struct splice final {
        int begin;
        int end;
        QStringList replacement;
    };
    std::vector<splice> splices;
    splices.push_back(
        {results_span.begin, results_span.end, render_results_section(result, environment)});

    for (const auto& step : result.steps) {
        // Scope the search to the step's own client sub-heading first,
        // when set — otherwise two clients sharing a step title would
        // both resolve to the same (first) match in the whole document.
        int search_begin = 0;
        int search_end = lines.size();
        if (!step.client.isEmpty()) {
            const auto client_span = find_heading(lines, step.client, 0, lines.size());
            if (!client_span.found())
                continue; // stale/renamed client heading — skip.
            search_begin = client_span.begin + 1;
            search_end = client_span.end;
        }

        const auto step_span = find_heading(lines, step.step_title, search_begin, search_end);
        if (!step_span.found())
            continue; // stale/renamed step — skip, don't fail the whole write.

        const int child_level = step_span.level + 1;
        const auto existing_result =
            find_heading(lines, "Result", step_span.begin + 1, step_span.end);
        const auto content = render_step_result(child_level, step.status, step.notes);

        if (existing_result.found())
            splices.push_back({existing_result.begin, existing_result.end, content});
        else
            splices.push_back({step_span.end, step_span.end, content});
    }

    // Splice bottom-up (descending by start index) so each earlier
    // splice's indices are still valid when it's applied.
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

bool append_scenario_note(const QString& path, const QString& line) {
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

    const auto notes_span = find_heading(lines, "Notes", 0, lines.size());
    if (!notes_span.found())
        return false;

    // A trailing blank line separates the appended line from whatever
    // heading follows Notes — irrelevant when Notes is the last section
    // (the common case, EOF right after), but keeps the doc readable for
    // the (rarer) template variant where something follows it.
    const bool has_following_heading = notes_span.end < lines.size();
    QStringList insertion{line};
    if (has_following_heading)
        insertion << QString();
    lines = lines.mid(0, notes_span.end) + insertion + lines.mid(notes_span.end);

    if (!file.open(QIODevice::WriteOnly | QIODevice::Text | QIODevice::Truncate))
        return false;
    QTextStream out(&file);
    for (const auto& l : lines)
        out << l << "\n";
    return true;
}

}
