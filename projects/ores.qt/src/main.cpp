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
#include <QApplication>
#include <QFile>
#include <QIcon>
#include <QTextStream>
#include <QTimer>
#include "ores.utility/version/version.hpp"
#include "ores.utility/log/make_logger.hpp"
#include "ores.utility/version/version.hpp"
#include "ores.utility/log/logging_options.hpp"
#include "ores.utility/log/lifecycle_manager.hpp"
#include "ores.comms/protocol/message_types.hpp"
#include "ores.qt/MainWindow.hpp"
#include "ores.qt/SplashScreen.hpp"

namespace {

using namespace ores::utility::log;
auto lg(make_logger("main"));

const std::string product_version("Qt UI for ORE Studio v" ORES_VERSION);

ores::utility::log::logging_options createLoggingConfiguration() {
    // FIXME: read this from command line
    ores::utility::log::logging_options r;
    r.filename = "ores.qt.log";
    r.output_to_console = false;
    r.output_directory = "../log";
    r.severity = "debug";
    return r;
}

} // namespace

int main(int argc, char *argv[]) {
    auto cfg(createLoggingConfiguration());
    ores::utility::log::lifecycle_manager lm(cfg);

    BOOST_LOG_SEV(lg, info) << "Started Qt UI " << product_version;

    QApplication app(argc, argv);

    // Load the stylesheet
    QFile file(":/TradingStyle.qss");
    if (file.open(QFile::ReadOnly | QFile::Text)) {
        QTextStream stream(&file);
        app.setStyleSheet(stream.readAll());
        file.close();
    } else {
        BOOST_LOG_SEV(lg, warn) << "Could not load stylesheet.";
    }

    // Set application icon
    app.setWindowIcon(QIcon(":/images/modern-icon.png"));

    ores::qt::SplashScreen splash(QPixmap(":/images/splash-screen.png"));
    splash.show();

    // Start progress bar animation
    const int splashDuration = 2000; // in milliseconds
    QString buildInfo = QString("v%1 '%2' Protocol '%3.%4'")
                            .arg(ORES_VERSION)
                            .arg(ORES_BUILD_INFO)
                            .arg(ores::comms::protocol::PROTOCOL_VERSION_MAJOR)
                            .arg(ores::comms::protocol::PROTOCOL_VERSION_MINOR);
    splash.setMessage(buildInfo);
    splash.setProgressDuration(splashDuration);

    ores::qt::MainWindow mainWindow;
    QTimer::singleShot(splashDuration, &splash, SLOT(close()));
    QTimer::singleShot(splashDuration, &mainWindow, SLOT(show()));

    return QApplication::exec();
}
