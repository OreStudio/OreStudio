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

#include "ores.logging/make_logger.hpp"
#include <QFont>
#include <QFontDatabase>
#include <QString>
#include <QStringList>

namespace ores::qt {

struct FontUtils {
    static constexpr const char* MonospaceFontFamily = "Fira Code";
    static constexpr int DefaultPointSize = 10;
    static constexpr int DefaultPixelSize = 11;

    inline static std::string_view logger_name = "ores.qt.font_utils";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

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
     * @brief The first monospace family from the fallback chain that is
     * actually installed AND genuinely fixed-pitch, or empty if none is.
     *
     * Resolving by exact availability + fixed-pitch avoids Qt's fuzzy family
     * matching latching onto a near-name that is not a real monospace text
     * font (e.g. a symbols-only "Fira Code Symbol" matched for "Fira Code").
     * Resolved once and logged at "ores.qt.font_utils" so the chosen face is
     * a trivial grep.
     */
    static QString resolvedMonospaceFamily() {
        using namespace ores::logging;
        static const QString family = [] {
            const QStringList available = QFontDatabase::families();
            for (const QString& candidate : monospaceFamilies()) {
                const bool installed =
                    available.contains(candidate, Qt::CaseInsensitive);
                const bool fixed =
                    installed && QFontDatabase::isFixedPitch(candidate);
                BOOST_LOG_SEV(lg(), debug)
                    << "Monospace candidate '" << candidate.toStdString()
                    << "': installed=" << installed << " fixed_pitch=" << fixed;
                if (fixed) {
                    BOOST_LOG_SEV(lg(), info)
                        << "Monospace font resolved to: '"
                        << candidate.toStdString() << "'";
                    return candidate;
                }
            }
            BOOST_LOG_SEV(lg(), warn)
                << "Monospace font resolved to: <none> — no fixed-pitch family "
                   "from the fallback chain is installed; relying on the Qt "
                   "Monospace style hint.";
            return QString();
        }();
        return family;
    }

    /**
     * @brief Returns a monospace QFont.
     *
     * Sets the single resolved fixed-pitch family (see
     * resolvedMonospaceFamily); only when none is installed does it fall back
     * to the whole chain plus the Monospace style hint. Fixed pitch and the
     * style hint are set as belt-and-braces. Use setFont() rather than
     * stylesheets: Qt's QSS engine does not honour the CSS "monospace"
     * generic family.
     */
    static QFont monospace() {
        QFont f;
        const QString family = resolvedMonospaceFamily();
        if (!family.isEmpty())
            f.setFamily(family);
        else
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
