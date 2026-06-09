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
#include "ores.qt/ScriptHighlighter.hpp"

namespace ores::qt {

ScriptHighlighter::ScriptHighlighter(QTextDocument* parent)
    : QSyntaxHighlighter(parent) {
    // The leading command word on a line (e.g. provision, connect,
    // login, load, bootstrap) — the verb that drives the line.
    QTextCharFormat command;
    command.setForeground(QColor(0x81, 0xA1, 0xC1));
    command.setFontWeight(QFont::Bold);
    rules_.push_back({QRegularExpression(R"(^\s*[A-Za-z][\w-]*)"), command});

    // --flags.
    QTextCharFormat flag;
    flag.setForeground(QColor(0xEB, 0xCB, 0x8B));
    rules_.push_back({QRegularExpression(R"(--[\w-]+)"), flag});

    // Double-quoted strings (e.g. a party's full name).
    QTextCharFormat string;
    string.setForeground(QColor(0xA3, 0xBE, 0x8C));
    rules_.push_back({QRegularExpression(R"("[^"]*")"), string});

    // Bare numbers (ports, seeds, counts).
    QTextCharFormat number;
    number.setForeground(QColor(0xB4, 0x8E, 0xAD));
    rules_.push_back({QRegularExpression(R"(\b\d+\b)"), number});

    // Whole-line comments (the only comment form load recognises).
    comment_format_.setForeground(QColor(0x61, 0x6E, 0x88));
    comment_format_.setFontItalic(true);
    comment_pattern_ = QRegularExpression(R"(^\s*#.*$)");
}

void ScriptHighlighter::highlightBlock(const QString& text) {
    // A comment line is wholly a comment — colour it and stop, so a
    // '#' never gets a command/flag colour underneath.
    auto comment = comment_pattern_.match(text);
    if (comment.hasMatch()) {
        setFormat(0, static_cast<int>(text.length()), comment_format_);
        return;
    }

    for (const auto& r : rules_) {
        auto it = r.pattern.globalMatch(text);
        while (it.hasNext()) {
            const auto m = it.next();
            setFormat(static_cast<int>(m.capturedStart()),
                      static_cast<int>(m.capturedLength()),
                      r.format);
        }
    }
}

}
