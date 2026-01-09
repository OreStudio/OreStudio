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
#include <QStyleFactory>
#include <QTextStream>
#include <QTimer>
#include "ores.utility/version/version.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.telemetry/log/lifecycle_manager.hpp"
#include "ores.comms/messaging/message_types.hpp"
#include "ores.comms/service/telemetry_streaming_service.hpp"
#include "ores.qt/CommandLineParser.hpp"
#include "ores.qt/MainWindow.hpp"
#include "ores.qt/SplashScreen.hpp"
#include "ores.qt/TelemetrySettingsDialog.hpp"

namespace {

inline static std::string_view logger_name = "ores.qt.main";

using namespace ores::logging;

} // namespace

int main(int argc, char *argv[]) {
    QApplication app(argc, argv);
    QCoreApplication::setApplicationName("ores.qt");
    QCoreApplication::setApplicationVersion(ORES_VERSION);

    ores::qt::CommandLineParser parser;
    parser.process(app);

    ores::telemetry::log::lifecycle_manager lm(parser.loggingOptions());

    auto lg(make_logger(logger_name));

    BOOST_LOG_SEV(lg, info) << ores::utility::version::format_startup_message(
        "ORE Studio Qt",
        ores::comms::messaging::PROTOCOL_VERSION_MAJOR,
        ores::comms::messaging::PROTOCOL_VERSION_MINOR);

    // Set Fusion style for consistent cross-platform appearance.
    // This is especially important for WSL where Qt may detect a different
    // platform style that conflicts with our dark theme stylesheet.
    app.setStyle(QStyleFactory::create("Fusion"));

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
    QString buildInfo = QString("v%1 %2 Protocol %3.%4")
                            .arg(ORES_VERSION)
                            .arg(ORES_BUILD_INFO)
                            .arg(ores::comms::messaging::PROTOCOL_VERSION_MAJOR)
                            .arg(ores::comms::messaging::PROTOCOL_VERSION_MINOR);
    splash.setMessage(buildInfo);
    splash.setProgressDuration(splashDuration);

    ores::qt::MainWindow mainWindow;
    mainWindow.getClientManager()->setSupportedCompression(parser.supportedCompression());

    // Set instance identification info if provided
    const QString instanceName = parser.instanceName();
    const QColor instanceColor = parser.instanceColor();
    if (!instanceName.isEmpty() || instanceColor.isValid()) {
        mainWindow.setInstanceInfo(instanceName, instanceColor);
        BOOST_LOG_SEV(lg, info) << "Instance info: name='" << instanceName.toStdString()
                                << "', color="
                                << (instanceColor.isValid() ? instanceColor.name().toStdString() : "none");
    }

    // Enable telemetry streaming if configured in settings
    if (ores::qt::TelemetrySettingsDialog::isStreamingEnabled()) {
        BOOST_LOG_SEV(lg, info) << "Telemetry streaming is enabled in settings";
        ores::comms::service::telemetry_streaming_options streaming_opts{
            .source_name = "ores.qt",
            .source_version = ORES_VERSION,
            .batch_size = static_cast<std::size_t>(
                ores::qt::TelemetrySettingsDialog::streamingBatchSize()),
            .flush_interval = std::chrono::seconds(
                ores::qt::TelemetrySettingsDialog::streamingFlushInterval())
        };
        mainWindow.getClientManager()->enableStreaming(streaming_opts);
    }

    QTimer::singleShot(splashDuration, &splash, SLOT(close()));
    QTimer::singleShot(splashDuration, &mainWindow, SLOT(show()));

    return QApplication::exec();
}
