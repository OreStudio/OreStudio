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
#include <QPainter>
#include <QSvgRenderer>

namespace ores::qt {

using namespace ores::logging;

QIcon IconUtils::createRecoloredIcon(const QString& svgPath, const QColor& color) {
    QIcon originalIcon(svgPath);
    if (originalIcon.isNull()) {
        BOOST_LOG_SEV(lg(), warn) << "Failed to load icon: "
                                  << svgPath.toStdString();
        return {};
    }

    QIcon recoloredIcon;
    // Disabled color should be visible but clearly dimmed against #1A1A1A background
    // Using #646464 (100, 100, 100) - matches disabled text styling
    const QColor disabledColor(100, 100, 100);

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

QPixmap IconUtils::svgDataToPixmap(const std::string& svg_data, int height) {
    if (svg_data.empty() || height <= 0) {
        return {};
    }

    QByteArray svgBytes(svg_data.data(), static_cast<qsizetype>(svg_data.size()));
    QSvgRenderer renderer(svgBytes);

    if (!renderer.isValid()) {
        BOOST_LOG_SEV(lg(), warn) << "Invalid SVG data, cannot render pixmap.";
        return {};
    }

    // Get SVG's default (viewBox) size to preserve aspect ratio
    QSizeF svgSize = renderer.defaultSize();
    if (svgSize.isEmpty()) {
        svgSize = QSizeF(4, 3);  // Default to 4:3 if no viewBox
    }
    qreal aspectRatio = svgSize.width() / svgSize.height();
    int width = static_cast<int>(height * aspectRatio);

    QPixmap pixmap(width, height);
    pixmap.fill(Qt::transparent);

    QPainter painter(&pixmap);
    painter.setRenderHint(QPainter::Antialiasing);
    painter.setRenderHint(QPainter::SmoothPixmapTransform);
    renderer.render(&painter);
    painter.end();

    return pixmap;
}

QIcon IconUtils::svgDataToIcon(const std::string& svg_data) {
    if (svg_data.empty()) {
        return {};
    }

    QByteArray svgBytes(svg_data.data(), static_cast<qsizetype>(svg_data.size()));
    QSvgRenderer renderer(svgBytes);

    if (!renderer.isValid()) {
        BOOST_LOG_SEV(lg(), warn) << "Invalid SVG data, cannot render icon.";
        return {};
    }

    QIcon icon;

    // Get SVG's default (viewBox) size to preserve aspect ratio
    QSizeF svgSize = renderer.defaultSize();
    if (svgSize.isEmpty()) {
        svgSize = QSizeF(4, 3);  // Default to 4:3 if no viewBox
    }
    qreal aspectRatio = svgSize.width() / svgSize.height();

    // Render at multiple sizes for crisp display
    for (int height : {16, 20, 24, 32, 48}) {
        int width = static_cast<int>(height * aspectRatio);
        QPixmap pixmap(width, height);
        pixmap.fill(Qt::transparent);

        QPainter painter(&pixmap);
        painter.setRenderHint(QPainter::Antialiasing);
        painter.setRenderHint(QPainter::SmoothPixmapTransform);
        renderer.render(&painter);
        painter.end();

        icon.addPixmap(pixmap);
    }

    return icon;
}

}
