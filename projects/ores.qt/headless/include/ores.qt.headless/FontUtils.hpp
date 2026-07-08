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
#include <QCoreApplication>
#include <QFont>
#include <QFontDatabase>
#include <QString>
#include <QStringList>

namespace ores::qt {

struct FontUtils {
    static constexpr const char* MonospaceFontFamily = "Fira Code";
    static constexpr int DefaultPointSize = 10;

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
            MonospaceFontFamily, // Fira Code — preferred, if installed
            "DejaVu Sans Mono",  // ships with most Linux desktops
            "Liberation Mono",   // common metric-compatible Linux fallback
            "Menlo",             // macOS
            "Consolas",          // Windows
            "Courier New"        // near-universal last resort
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
        // QFontDatabase needs a running QApplication. If somehow called
        // earlier (e.g. a static initialiser), don't resolve or cache —
        // return empty so monospace() falls back to the family chain.
        if (QCoreApplication::instance() == nullptr)
            return QString();
        static const QString family = [] {
            const QStringList available = QFontDatabase::families();
            for (const QString& candidate : monospaceFamilies()) {
                const bool installed = available.contains(candidate, Qt::CaseInsensitive);
                const bool fixed = installed && QFontDatabase::isFixedPitch(candidate);
                BOOST_LOG_SEV(lg(), debug)
                    << "Monospace candidate '" << candidate.toStdString()
                    << "': installed=" << installed << " fixed_pitch=" << fixed;
                if (fixed) {
                    BOOST_LOG_SEV(lg(), info)
                        << "Monospace font resolved to: '" << candidate.toStdString() << "'";
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
     * style hint are set as belt-and-braces. Apply with QWidget::setFont():
     * font family is owned by the Qt font system (the QSS theme sets no
     * font-family), so setFont() is authoritative.
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
     * @brief Ordered proportional UI fallback chain — the application's
     * default sans-serif stack.
     */
    static QStringList uiFamilies() {
        return {"Inter", // preferred, if installed
                "Roboto",
                "Segoe UI",       // Windows
                "SF Pro Display", // macOS
                "Helvetica Neue",
                "Arial"};
    }

    /**
     * @brief The application's default (proportional) font.
     *
     * Set once via QApplication::setFont() so font family is owned by the
     * Qt font system, not the QSS theme. With no font-family in the
     * stylesheet, QWidget::setFont (e.g. monospace() on the terminal) is
     * authoritative again instead of being overridden by the cascade.
     */
    static QFont applicationFont() {
        QFont f;
        f.setFamilies(uiFamilies());
        f.setStyleHint(QFont::SansSerif);
        f.setPointSize(DefaultPointSize);
        return f;
    }
};

}

#endif
