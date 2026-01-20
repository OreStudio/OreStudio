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
#include "ores.qt/IconUtils.hpp"

#include <QAbstractButton>
#include <QPixmap>
#include <QImage>

namespace ores::qt {

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
    QIcon questionIcon = IconUtils::createRecoloredIcon(Icon::Question, IconUtils::DefaultIconColor);
    if (!questionIcon.isNull()) {
        msgBox.setIconPixmap(questionIcon.pixmap(48, 48));
    }

    // Customize button icons
    if (buttons & QMessageBox::Yes) {
        QAbstractButton* yesButton = msgBox.button(QMessageBox::Yes);
        if (yesButton) {
            QIcon checkIcon = IconUtils::createRecoloredIcon(Icon::Checkmark, IconUtils::DefaultIconColor);
            if (!checkIcon.isNull()) {
                yesButton->setIcon(checkIcon);
            }
        }
    }

    if (buttons & QMessageBox::No) {
        QAbstractButton* noButton = msgBox.button(QMessageBox::No);
        if (noButton) {
            QIcon dismissIcon = IconUtils::createRecoloredIcon(Icon::Dismiss, IconUtils::DefaultIconColor);
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
    QIcon warningIcon = IconUtils::createRecoloredIcon(Icon::Warning, IconUtils::DefaultIconColor);
    if (!warningIcon.isNull()) {
        msgBox.setIconPixmap(warningIcon.pixmap(48, 48));
    }

    msgBox.exec();
}

void MessageBoxHelper::critical(
    QWidget* parent,
    const QString& title,
    const QString& text) {
    critical(parent, title, text, QString());
}

void MessageBoxHelper::critical(
    QWidget* parent,
    const QString& title,
    const QString& text,
    const QString& detailedText) {

    QMessageBox msgBox(parent);
    msgBox.setWindowTitle(title);
    msgBox.setText(text);
    msgBox.setStandardButtons(QMessageBox::Ok);

    if (!detailedText.isEmpty()) {
        msgBox.setDetailedText(detailedText);
    }

    // Use error icon for critical errors
    QIcon criticalIcon = IconUtils::createRecoloredIcon(Icon::Error, IconUtils::DefaultIconColor);
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
    QIcon infoIcon = IconUtils::createRecoloredIcon(Icon::Info, IconUtils::DefaultIconColor);
    if (!infoIcon.isNull()) {
        msgBox.setIconPixmap(infoIcon.pixmap(48, 48));
    }

    msgBox.exec();
}

}