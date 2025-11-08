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
#include <QApplication>
#include <QDate>
#include <QTime>

namespace ores::qt {

using namespace ores::utility::log;

AboutDialog::AboutDialog(QWidget* parent)
    : QDialog(parent), ui_(new Ui::AboutDialog) {

    BOOST_LOG_SEV(lg(), info) << "Creating about dialog";
    ui_->setupUi(this);

    setupVersionInfo();
}

AboutDialog::~AboutDialog() {
    BOOST_LOG_SEV(lg(), info) << "Destroying about dialog";
}

void AboutDialog::setupVersionInfo() {
    // Get version and build information
    QString version = QString("Version: %1").arg(ORES_VERSION);
    QString build = QString("Build: %1").arg(ORES_BUILD_INFO);

    ui_->versionLabel->setText(version);
    ui_->buildLabel->setText(build);

    // Set window title with version
    setWindowTitle(QString("About OreStudio %1").arg(ORES_VERSION));
}

}
