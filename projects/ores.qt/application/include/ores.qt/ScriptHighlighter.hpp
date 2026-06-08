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
#ifndef ORES_QT_SCRIPT_HIGHLIGHTER_HPP
#define ORES_QT_SCRIPT_HIGHLIGHTER_HPP

#include <QRegularExpression>
#include <QSyntaxHighlighter>
#include <QTextCharFormat>
#include <vector>

namespace ores::qt {

/**
 * @brief Syntax highlighter for ores-shell (.ores) scripts.
 *
 * Highlights the line shape the shell's load command understands:
 * comments (# ...), the leading command word, --flags, quoted
 * strings, and bare numbers. Deliberately light — it aids reading,
 * it is not a language server.
 */
class ScriptHighlighter : public QSyntaxHighlighter {
    Q_OBJECT

public:
    explicit ScriptHighlighter(QTextDocument* parent);

protected:
    void highlightBlock(const QString& text) override;

private:
    struct rule {
        QRegularExpression pattern;
        QTextCharFormat format;
    };
    std::vector<rule> rules_;
    QTextCharFormat comment_format_;
    QRegularExpression comment_pattern_;
};

}

#endif
