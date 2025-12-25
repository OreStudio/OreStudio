/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2024-2025 Marco Craveiro <marco.craveiro@gmail.com>
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
 * MA 02110-1301, USA.
 *
 */
#include "ores.qt/IconUtils.hpp"

#include <QImage>
#include <QPixmap>

namespace ores::qt {

using namespace ores::telemetry::log;

QIcon IconUtils::createRecoloredIcon(const QString& svgPath, const QColor& color) {
    QIcon originalIcon(svgPath);
    if (originalIcon.isNull()) {
        BOOST_LOG_SEV(lg(), warn) << "Failed to load icon: "
                                  << svgPath.toStdString();
        return {};
    }

    QIcon recoloredIcon;
    const QColor disabledColor(50, 50, 50); // Dark gray for disabled state

    for (int size : {16, 20, 24, 32, 48, 64}) {
        QPixmap pixmap = originalIcon.pixmap(size, size);

        // Create normal state image
        QImage normalImage = pixmap.toImage().convertToFormat(QImage::Format_ARGB32);
        for (int y = 0; y < normalImage.height(); ++y) {
            for (int x = 0; x < normalImage.width(); ++x) {
                QColor pixelColor = normalImage.pixelColor(x, y);
                if (pixelColor.alpha() > 0) {
                    pixelColor.setRed(color.red());
                    pixelColor.setGreen(color.green());
                    pixelColor.setBlue(color.blue());
                    normalImage.setPixelColor(x, y, pixelColor);
                }
            }
        }
        recoloredIcon.addPixmap(QPixmap::fromImage(normalImage), QIcon::Normal);

        // Create disabled state image
        QImage disabledImage = pixmap.toImage().convertToFormat(QImage::Format_ARGB32);
        for (int y = 0; y < disabledImage.height(); ++y) {
            for (int x = 0; x < disabledImage.width(); ++x) {
                QColor pixelColor = disabledImage.pixelColor(x, y);
                if (pixelColor.alpha() > 0) {
                    pixelColor.setRed(disabledColor.red());
                    pixelColor.setGreen(disabledColor.green());
                    pixelColor.setBlue(disabledColor.blue());
                    disabledImage.setPixelColor(x, y, pixelColor);
                }
            }
        }
        recoloredIcon.addPixmap(QPixmap::fromImage(disabledImage), QIcon::Disabled);
    }

    return recoloredIcon;
}

}
