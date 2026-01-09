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
#include "ores.qt/AboutDialog.hpp"

#include "ores.utility/version/version.hpp"
#include "ores.comms/messaging/message_types.hpp"
#include <QDate>
#include <QTime>
#include <QPainter>
#include <QPixmap>
#include <QApplication>

namespace ores::qt {

using namespace ores::logging;

AboutDialog::AboutDialog(QWidget* parent)
    : QDialog(parent) {

    BOOST_LOG_SEV(lg(), debug) << "Creating about dialog.";
    ui_.setupUi(this);

    // Replace ui_.logoLabel with our custom LogoLabel in the layout
    logoLabel_ = new LogoLabel(ui_.logoContainer);
    logoLabel_->setAlignment(ui_.logoLabel->alignment());
    logoLabel_->setScaledContents(ui_.logoLabel->hasScaledContents());

    // Replace the widget in the layout
    QLayout* layout = ui_.logoLabel->parentWidget()->layout();
    if (layout) {
        layout->replaceWidget(ui_.logoLabel, logoLabel_);
        ui_.logoLabel->deleteLater();
    }
}

AboutDialog::~AboutDialog() {
    BOOST_LOG_SEV(lg(), debug) << "Destroying about dialog.";
}

void AboutDialog::showEvent(QShowEvent* e) {
    QDialog::showEvent(e);

    const char* image(":/images/splash-screen.png");
    BOOST_LOG_SEV(lg(), debug) << "Scaling logo to fit the dialog. Image: "
                               << image;
    QPixmap logo(image);
    if (!logo.isNull()) {
        int targetWidth = width();
        BOOST_LOG_SEV(lg(), debug) << "Scaling to target width: " << targetWidth;

        QPixmap scaledLogo = logo.scaledToWidth(targetWidth, Qt::SmoothTransformation);

        logoLabel_->setPixmap(std::move(scaledLogo));

        const QString text = QString("v%1 %2 Protocol %3.%4")
                                 .arg(ORES_VERSION)
                                 .arg(ORES_BUILD_INFO)
                                 .arg(ores::comms::messaging::PROTOCOL_VERSION_MAJOR)
                                 .arg(ores::comms::messaging::PROTOCOL_VERSION_MINOR);
        logoLabel_->setTextOverlay(text);

        BOOST_LOG_SEV(lg(), debug) << "Scaled successfully.";
    } else {
        BOOST_LOG_SEV(lg(), warn) << "Missing file: splash-screen.png";
    }
}

}
