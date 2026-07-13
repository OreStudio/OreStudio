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
#include "ores.qt/BadgeLabelUtils.hpp"
#include "ores.qt/BadgeCache.hpp"
#include "ores.qt/ColorConstants.hpp"
#include <QColor>

namespace ores::qt {

void BadgeLabelUtils::apply(QLabel* label,
                            const BadgeCache* cache,
                            const std::string& domain,
                            const std::string& value,
                            const QString& text) {
    if (!label)
        return;

    // Fallback for unresolved badge definitions — deliberately not gray,
    // which is reserved for inactive/negative states.
    QColor bg = color_constants::badge_fallback;
    QColor fg = color_constants::badge_fallback_text;
    if (cache) {
        if (const auto* def = cache->resolve(domain, value)) {
            // An unparseable colour in the definition is treated the same
            // as a missing definition: keep the orange fallback.
            const QColor def_bg(QString::fromStdString(def->background_colour));
            const QColor def_fg(QString::fromStdString(def->text_colour));
            if (def_bg.isValid() && def_fg.isValid()) {
                bg = def_bg;
                fg = def_fg;
            }
        }
    }

    label->setText(text);
    label->setAlignment(Qt::AlignCenter);
    label->setStyleSheet(QString("QLabel {"
                                 "  background-color: %1;"
                                 "  color: %2;"
                                 "  border-radius: 6px;"
                                 "  padding: 1px 6px;"
                                 "  font-size: 10px;"
                                 "  font-weight: bold;"
                                 "}")
                             .arg(bg.name(), fg.name()));
}

void BadgeLabelUtils::clear(QLabel* label) {
    if (!label)
        return;
    label->clear();
    label->setStyleSheet(QString());
}

}
