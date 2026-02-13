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
#ifndef ORES_QT_TEXT_UTILS_HPP
#define ORES_QT_TEXT_UTILS_HPP

#include <algorithm>
#include <optional>
#include <string>
#include <QString>

namespace ores::qt {

struct TextUtils {
    static constexpr int max_transliterated_length = 30;

    /**
     * @brief Returns true if the string contains non-Latin characters.
     *
     * Checks for scripts other than Common, Latin, and Inherited
     * (e.g. Korean, CJK, Cyrillic, Arabic).
     */
    static bool contains_non_latin(const QString& text) {
        return std::any_of(text.begin(), text.end(), [](const QChar& ch) {
            auto s = ch.script();
            return s != QChar::Script_Common && s != QChar::Script_Latin &&
                   s != QChar::Script_Inherited;
        });
    }

    /**
     * @brief Format a display name with optional transliterated suffix.
     *
     * If the name contains non-Latin characters and a transliteration is
     * available, returns "name (transliteration)" with the transliteration
     * truncated to max_transliterated_length characters.
     */
    static QString display_name_with_transliteration(
        const std::string& name,
        const std::optional<std::string>& transliterated_name) {
        auto qname = QString::fromStdString(name);
        if (transliterated_name.has_value() &&
            !transliterated_name->empty() &&
            contains_non_latin(qname)) {
            auto tl = QString::fromStdString(*transliterated_name);
            if (tl.size() > max_transliterated_length)
                tl = tl.left(max_transliterated_length) + u"\u2026";
            return qname + u" (" + tl + u")";
        }
        return qname;
    }
};

}

#endif
