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
#include "ores.qt/MessageBoxHelper.hpp"
#include <QAbstractButton>
#include <QPixmap>
#include <QImage>

namespace ores::qt {

QIcon MessageBoxHelper::createColoredIcon(const QString& svgPath, const QColor& color) {
    QIcon originalIcon(svgPath);
    if (originalIcon.isNull()) {
        return QIcon();
    }

    QIcon coloredIcon;
    for (int size : {16, 20, 24, 32, 48, 64}) {
        QPixmap pixmap = originalIcon.pixmap(size, size);
        QImage image = pixmap.toImage().convertToFormat(QImage::Format_ARGB32);

        for (int y = 0; y < image.height(); ++y) {
            for (int x = 0; x < image.width(); ++x) {
                QColor pixelColor = image.pixelColor(x, y);
                if (pixelColor.alpha() > 0) {
                    pixelColor.setRed(color.red());
                    pixelColor.setGreen(color.green());
                    pixelColor.setBlue(color.blue());
                    image.setPixelColor(x, y, pixelColor);
                }
            }
        }

        coloredIcon.addPixmap(QPixmap::fromImage(image));
    }

    return coloredIcon;
}

QMessageBox::StandardButton MessageBoxHelper::question(
    QWidget* parent,
    const QString& title,
    const QString& text,
    QMessageBox::StandardButtons buttons) {

    QMessageBox msgBox(parent);
    msgBox.setWindowTitle(title);
    msgBox.setText(text);
    msgBox.setStandardButtons(buttons);

    // Set custom question icon
    const QColor iconColor(220, 220, 220);
    QIcon questionIcon = createColoredIcon(":/icons/resources/icons/ic_fluent_question_20_filled.svg", iconColor);
    if (!questionIcon.isNull()) {
        msgBox.setIconPixmap(questionIcon.pixmap(48, 48));
    }

    // Customize button icons
    if (buttons & QMessageBox::Yes) {
        QAbstractButton* yesButton = msgBox.button(QMessageBox::Yes);
        if (yesButton) {
            QIcon checkIcon = createColoredIcon(":/icons/resources/icons/ic_fluent_checkmark_20_filled.svg", iconColor);
            if (!checkIcon.isNull()) {
                yesButton->setIcon(checkIcon);
            }
        }
    }

    if (buttons & QMessageBox::No) {
        QAbstractButton* noButton = msgBox.button(QMessageBox::No);
        if (noButton) {
            QIcon dismissIcon = createColoredIcon(":/icons/resources/icons/ic_fluent_dismiss_20_filled.svg", iconColor);
            if (!dismissIcon.isNull()) {
                noButton->setIcon(dismissIcon);
            }
        }
    }

    return static_cast<QMessageBox::StandardButton>(msgBox.exec());
}

void MessageBoxHelper::warning(
    QWidget* parent,
    const QString& title,
    const QString& text) {

    QMessageBox msgBox(parent);
    msgBox.setWindowTitle(title);
    msgBox.setText(text);
    msgBox.setStandardButtons(QMessageBox::Ok);

    // Use warning icon
    const QColor iconColor(220, 220, 220);
    QIcon warningIcon = createColoredIcon(":/icons/resources/icons/ic_fluent_warning_20_filled.svg", iconColor);
    if (!warningIcon.isNull()) {
        msgBox.setIconPixmap(warningIcon.pixmap(48, 48));
    }

    msgBox.exec();
}

void MessageBoxHelper::critical(
    QWidget* parent,
    const QString& title,
    const QString& text) {

    QMessageBox msgBox(parent);
    msgBox.setWindowTitle(title);
    msgBox.setText(text);
    msgBox.setStandardButtons(QMessageBox::Ok);

    // Use error icon for critical errors
    const QColor iconColor(220, 220, 220);
    QIcon criticalIcon = createColoredIcon(":/icons/resources/icons/ic_fluent_error_circle_20_filled.svg", iconColor);
    if (!criticalIcon.isNull()) {
        msgBox.setIconPixmap(criticalIcon.pixmap(48, 48));
    }

    msgBox.exec();
}

void MessageBoxHelper::information(
    QWidget* parent,
    const QString& title,
    const QString& text) {

    QMessageBox msgBox(parent);
    msgBox.setWindowTitle(title);
    msgBox.setText(text);
    msgBox.setStandardButtons(QMessageBox::Ok);

    // Use info icon for information
    const QColor iconColor(220, 220, 220);
    QIcon infoIcon = createColoredIcon(":/icons/resources/icons/ic_fluent_info_20_filled.svg", iconColor);
    if (!infoIcon.isNull()) {
        msgBox.setIconPixmap(infoIcon.pixmap(48, 48));
    }

    msgBox.exec();
}

} // namespace ores::qt
