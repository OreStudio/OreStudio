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

namespace ores::qt {

struct FontUtils {
    static constexpr const char* MonospaceFontFamily = "Fira Code";
    static constexpr int DefaultPointSize = 10;
    static constexpr int DefaultPixelSize = 11;

    static QFont monospace() {
        QFont f(MonospaceFontFamily);
        f.setPointSize(DefaultPointSize);
        return f;
    }

    static QFont monospaceBold() {
        QFont f = monospace();
        f.setBold(true);
        return f;
    }

    /**
     * @brief Returns a CSS font-family/font-size snippet for use in stylesheets.
     */
    static QString monospaceCssFragment() {
        return QString("font-family: \"%1\"; font-size: %2px;")
            .arg(MonospaceFontFamily)
            .arg(DefaultPixelSize);
    }
};

}

#endif
