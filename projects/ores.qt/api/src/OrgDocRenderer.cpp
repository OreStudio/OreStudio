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
#include "ores.qt/OrgDocRenderer.hpp"
#include <QRegularExpression>
#include <algorithm>

namespace ores::qt {

namespace {

/**
 * @brief Apply org-mode's four common inline markers as light HTML
 * tags, escaping HTML special characters in the surrounding plain
 * text as we go.
 *
 * This must be a single left-to-right pass over the *original* text,
 * not four sequential find-and-replace passes: a tag emitted by an
 * earlier pass (e.g. the `/` inside `</b>`) would otherwise get
 * reinterpreted as markup by a later pass (e.g. italic's `/.../`),
 * corrupting the output.
 */
QString render_inline(const QString& text) {
    static const QRegularExpression marker(R"(\*([^*\n]+)\*|/([^/\n]+)/|=([^=\n]+)=|~([^~\n]+)~)");

    QString out;
    int last = 0;
    auto it = marker.globalMatch(text);
    while (it.hasNext()) {
        const auto m = it.next();
        out += text.mid(last, m.capturedStart() - last).toHtmlEscaped();
        if (!m.captured(1).isNull())
            out += "<b>" + m.captured(1).toHtmlEscaped() + "</b>";
        else if (!m.captured(2).isNull())
            out += "<i>" + m.captured(2).toHtmlEscaped() + "</i>";
        else if (!m.captured(3).isNull())
            out += "<code>" + m.captured(3).toHtmlEscaped() + "</code>";
        else if (!m.captured(4).isNull())
            out += "<code>" + m.captured(4).toHtmlEscaped() + "</code>";
        last = m.capturedEnd();
    }
    out += text.mid(last).toHtmlEscaped();
    return out;
}

/**
 * @brief Render consecutive plain body lines as one or more
 * paragraphs, and consecutive bullet-list runs as =<ul>=. Tables are
 * rendered separately (see render_table) since the parser keeps them
 * structurally distinct from body_lines.
 */
QString render_body_lines(const std::vector<std::string>& lines) {
    QString html;
    bool in_list = false;
    for (const auto& raw_line : lines) {
        const QString line = QString::fromStdString(raw_line);
        const QString trimmed = line.trimmed();
        const bool is_bullet = trimmed.startsWith("- ");

        if (is_bullet) {
            if (!in_list) {
                html += "<ul>";
                in_list = true;
            }
            html += "<li>" + render_inline(trimmed.mid(2)) + "</li>";
            continue;
        }
        if (in_list) {
            html += "</ul>";
            in_list = false;
        }
        if (trimmed.isEmpty())
            continue;
        html += "<p>" + render_inline(trimmed) + "</p>";
    }
    if (in_list)
        html += "</ul>";
    return html;
}

QString render_table(const orgmode::domain::table& table) {
    QString html = "<table border=\"1\" cellspacing=\"0\" cellpadding=\"4\">";
    html += "<tr>";
    for (const auto& header : table.headers)
        html += "<th>" + render_inline(QString::fromStdString(header)) + "</th>";
    html += "</tr>";
    for (const auto& row : table.rows) {
        html += "<tr>";
        for (const auto& cell : row)
            html += "<td>" + render_inline(QString::fromStdString(cell)) + "</td>";
        html += "</tr>";
    }
    html += "</table>";
    return html;
}

QString heading_tag(unsigned int level) {
    // Clamp to h1..h6 — org headings can nest deeper than HTML's six
    // levels; anything beyond h6 just stays at h6 rather than
    // producing an invalid tag.
    const unsigned int clamped = level == 0 ? 1 : std::min(level, 6u);
    return "h" + QString::number(clamped);
}

}

QString render_heading_to_html(const orgmode::domain::heading& heading) {
    QString html;
    const QString tag = heading_tag(heading.level);
    html +=
        "<" + tag + ">" + render_inline(QString::fromStdString(heading.title)) + "</" + tag + ">";
    html += render_body_lines(heading.body_lines);
    for (const auto& table : heading.tables)
        html += render_table(table);
    for (const auto& child : heading.children)
        html += render_heading_to_html(child);
    return html;
}

QString render_org_doc_to_html(const orgmode::domain::document& doc) {
    QString html = "<html><body>";
    if (const auto title = doc.find_keyword("title"))
        html += "<h1>" + render_inline(QString::fromStdString(*title)) + "</h1>";
    for (const auto& heading : doc.headings)
        html += render_heading_to_html(heading);
    html += "</body></html>";
    return html;
}

}
