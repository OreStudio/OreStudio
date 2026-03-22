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
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51 Franklin
 * Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */
#include "ores.qt/SplashScreen.hpp"
#include "ores.qt/FontUtils.hpp"

#include <QPainter>

namespace ores::qt {

SplashScreen::SplashScreen(const QPixmap& pixmap)
    : QSplashScreen(pixmap) {}

void SplashScreen::setMessage(const QString& message) {
    messageText_ = message;
    repaint();
}

void SplashScreen::paintEvent(QPaintEvent* e) {
    QSplashScreen::paintEvent(e);

    if (!messageText_.isEmpty()) {
        QPainter painter(this);
        painter.setRenderHint(QPainter::Antialiasing);

        QFont font = FontUtils::monospace();
        font.setPointSize(6);
        painter.setFont(font);
        painter.setPen(Qt::white);

        QFontMetrics fm(font);
        const int margin = 20;
        const int textY = height() - fm.descent() - 15;
        const int textX = width() - fm.horizontalAdvance(messageText_) - margin;
        painter.drawText(textX, textY, messageText_);
    }

    painted_ = true;
}

void SplashScreen::ensureFirstPaint() const {
    while (!painted_) {
        QThread::usleep(1e3);
        qApp->processEvents();
    }
}

}
