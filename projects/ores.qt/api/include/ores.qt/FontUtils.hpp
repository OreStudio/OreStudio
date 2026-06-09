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
#ifndef ORES_QT_FONT_UTILS_HPP
#define ORES_QT_FONT_UTILS_HPP

#include <QFont>
#include <QString>
#include <QStringList>

namespace ores::qt {

struct FontUtils {
    static constexpr const char* MonospaceFontFamily = "Fira Code";
    static constexpr int DefaultPointSize = 10;
    static constexpr int DefaultPixelSize = 11;

    /**
     * @brief Ordered monospace fallback chain.
     *
     * The preferred face first, then widely-available platform monospace
     * fonts, so the result is genuinely fixed-width even where Fira Code is
     * not installed. Qt tries each family in turn.
     */
    static QStringList monospaceFamilies() {
        return {
            MonospaceFontFamily,  // Fira Code — preferred, if installed
            "DejaVu Sans Mono",   // ships with most Linux desktops
            "Liberation Mono",    // common metric-compatible Linux fallback
            "Menlo",              // macOS
            "Consolas",           // Windows
            "Courier New"         // near-universal last resort
        };
    }

    /**
     * @brief Returns a monospace QFont.
     *
     * Uses an explicit fallback family list (see monospaceFamilies) and, as
     * belt-and-braces, sets fixed pitch and the Monospace style hint so that
     * even if none of the named families match, Qt picks a monospace face
     * rather than a proportional default. A StyleHint alone is not enough —
     * with a single missing family Qt falls back to a proportional font.
     * Use setFont() rather than stylesheets: Qt's QSS engine does not honour
     * the CSS "monospace" generic family.
     */
    static QFont monospace() {
        QFont f;
        f.setFamilies(monospaceFamilies());
        f.setStyleHint(QFont::Monospace, QFont::PreferMatch);
        f.setFixedPitch(true);
        f.setPointSize(DefaultPointSize);
        return f;
    }

    static QFont monospaceBold() {
        QFont f = monospace();
        f.setBold(true);
        return f;
    }

    /**
     * @brief CSS font fragment for use in Qt stylesheets.
     *
     * Note: Qt's QSS engine does not honour the "monospace" generic family as
     * a fallback, so prefer setFont(monospace()) over this where possible.
     */
    static QString monospaceCssFragment() {
        return QString("font-family: \"Fira Code\", \"DejaVu Sans Mono\", "
                       "\"Liberation Mono\", \"Menlo\", \"Consolas\", "
                       "\"Courier New\", monospace; font-size: %1px;")
            .arg(DefaultPixelSize);
    }
};

}

#endif
