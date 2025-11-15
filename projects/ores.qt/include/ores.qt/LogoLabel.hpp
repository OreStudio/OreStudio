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
#ifndef ORES_QT_ABOUT_LOGOLABEL_HPP
#define ORES_QT_ABOUT_LOGOLABEL_HPP

#include <QLabel>
#include <QPainter>

namespace ores::qt {

class LogoLabel final : public QLabel {
public:
    explicit LogoLabel(QWidget* parent = nullptr) : QLabel(parent) {}
    void setTextOverlay(const QString& text) {
        overlayText_ = text;
        update();
    }

protected:
    void paintEvent(QPaintEvent* event) override {
        QLabel::paintEvent(event);

        if (!overlayText_.isEmpty()) {
            QPainter painter(this);
            QFont font("Sans Serif", 6);
            painter.setFont(font);
            painter.setPen(Qt::white);

            // Position text at bottom-left of the label
            int x = 20;
            int y = height() - 20;

            painter.drawText(x, y, overlayText_);
        }
    }

private:
    QString overlayText_;
};

}

#endif
