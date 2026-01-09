/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2024 Marco Craveiro <marco.craveiro@gmail.com>
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
#include "ores.qt/MdiAreaWithBackground.hpp"

#include <QPainter>

namespace ores::qt {

using namespace ores::logging;

MdiAreaWithBackground::MdiAreaWithBackground(QWidget* parent)
    : QMdiArea(parent) {
    BOOST_LOG_SEV(lg(), debug) << "Creating MDI area with background";

    // Explicitly ensure viewport exists
    if (viewport()) {
        BOOST_LOG_SEV(lg(), debug) << "Viewport exists in constructor";
    } else {
        BOOST_LOG_SEV(lg(), error) << "Viewport is null in constructor!";
    }
}

void MdiAreaWithBackground::setBackgroundLogo(const QString& imagePath) {
    backgroundLogo_ = QPixmap(imagePath);
    if (backgroundLogo_.isNull()) {
        BOOST_LOG_SEV(lg(), warn) << "Failed to load background logo: "
                                 << imagePath.toStdString();
    } else {
        BOOST_LOG_SEV(lg(), info) << "Background logo loaded: "
                                 << imagePath.toStdString();
    }

    if (auto* vp = viewport()) {
        vp->update();
    } else {
        BOOST_LOG_SEV(lg(), error) << "Viewport is null in setBackgroundLogo!";
    }
}

void MdiAreaWithBackground::paintEvent(QPaintEvent* event) {
    QMdiArea::paintEvent(event);

    // Draw logo as background
    if (!backgroundLogo_.isNull()) {
        QPainter painter(viewport());
        painter.setRenderHint(QPainter::SmoothPixmapTransform);

        // Calculate centered position
        const int x = (viewport()->width() - backgroundLogo_.width()) / 2;
        const int y = (viewport()->height() - backgroundLogo_.height()) / 2;

        // Draw logo with slight transparency
        painter.setOpacity(0.3);
        painter.drawPixmap(x, y, backgroundLogo_);
    }
}

}
