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
#ifndef ORES_QT_BADGE_LABEL_UTILS_HPP
#define ORES_QT_BADGE_LABEL_UTILS_HPP

#include "ores.qt/export.hpp"
#include <QLabel>
#include <QString>
#include <string>

namespace ores::qt {

class BadgeCache;

/**
 * @brief Styles QLabels as badges in detail dialogs and panels.
 *
 * Counterpart to DelegatePaintUtils::draw_centered_badge for widget-based
 * (non-delegate) contexts. Colours come from the database-driven badge
 * system via BadgeCache; when no definition resolves, the fallback is
 * orange — never gray, which the UX language reserves for inactive,
 * off or negative states.
 */
struct ORES_QT_API BadgeLabelUtils {
    /**
     * @brief Styles a QLabel as a badge for a code domain value.
     *
     * Sets the display text and applies background/text colours from the
     * badge definition resolved via @p cache. Falls back to
     * color_constants::badge_fallback when @p cache is null or has no
     * mapping for the (domain, value) pair.
     *
     * @param label  The label to style; its text is set to @p text.
     * @param cache  Badge cache (may be null).
     * @param domain Code domain (e.g. "account_locked").
     * @param value  Entity value to resolve (e.g. "Locked").
     * @param text   Human-readable badge text.
     */
    static void apply(QLabel* label,
                      const BadgeCache* cache,
                      const std::string& domain,
                      const std::string& value,
                      const QString& text);

    /**
     * @brief Clears badge styling and text from a label.
     */
    static void clear(QLabel* label);
};

}

#endif
