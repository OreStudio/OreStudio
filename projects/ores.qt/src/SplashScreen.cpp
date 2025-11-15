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
#include "ores.qt/SplashScreen.hpp"

#include <QPainter>
#include <QLinearGradient>

namespace ores::qt {

using namespace ores::utility::log;

SplashScreen::SplashScreen(const QPixmap& pixmap)
    : QSplashScreen(pixmap) {
    progressTimer_ = new QTimer(this);
    connect(progressTimer_, &QTimer::timeout, this,
        &SplashScreen::updateProgress);
}

void SplashScreen::setProgressDuration(int milliseconds) {
    totalDuration_ = milliseconds;
    elapsedTime_ = 0;
    progress_ = 0;

    progressTimer_->start(updateInterval_);
    BOOST_LOG_SEV(lg(), debug) << "Progress bar configured for " << milliseconds << "ms";
}

void SplashScreen::updateProgress() {
    elapsedTime_ += updateInterval_;

    // Calculate progress as percentage of total duration
    progress_ = (elapsedTime_ * 100) / totalDuration_;

    if (progress_ >= 100) {
        progress_ = 100;
        progressTimer_->stop();
        emit progressFinished();
    }
    repaint();
}

void SplashScreen::setMessage(const QString& message) {
    messageText_ = message;
    repaint();
}


void SplashScreen::paintEvent(QPaintEvent* e) {
    QSplashScreen::paintEvent(e);

    // Draw progress bar at the bottom
    QPainter painter(this);
    painter.setRenderHint(QPainter::Antialiasing);

    const int barHeight = 4;
    const int barY = height() - barHeight - 10;  // 10px from bottom
    const int barX = 20;  // 20px from left
    const int barWidth = width() - 40;  // 20px margin on each side

    // Draw background of progress bar (light gray, semi-transparent)
    painter.setPen(Qt::NoPen);
    painter.setBrush(QColor(200, 200, 200, 100));
    painter.drawRoundedRect(barX, barY, barWidth, barHeight, 2, 2);

    // Draw progress (white)
    if (progress_ > 0) {
        const int progressWidth = (barWidth * progress_) / 100;
        painter.setBrush(QColor(255, 255, 255, 220));  // White with slight transparency
        painter.drawRoundedRect(barX, barY, progressWidth, barHeight, 2, 2);
    }

    if (!messageText_.isEmpty()) {
        // Save the current painter state to restore it later
        painter.save();

        // Set the font to a small, sans-serif font
        QFont font("Sans Serif", 6);
        painter.setFont(font);

        // Set the text color to white
        painter.setPen(Qt::white);

        // Calculate the position to center the text horizontally
        QFontMetrics fm(font);
        int textWidth = fm.horizontalAdvance(messageText_);
        int textX = barX;

        // Position the text a few pixels above the progress bar
        int textY = barY - fm.descent() - 5; // 5px padding above the bar

        // Draw the text
        painter.drawText(textX, textY, messageText_);

        // Restore the painter's original state (font, pen, etc.)
        painter.restore();
    }

    painted_ = true;
}

void SplashScreen::ensureFirstPaint() const {
    while(!painted_) {
        QThread::usleep(1e3);
        qApp->processEvents();
    }
}

}
