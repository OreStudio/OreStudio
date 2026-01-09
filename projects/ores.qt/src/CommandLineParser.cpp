/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2025 Marco Craveiro <marco.craveiro@gmail.com>
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
#include "ores.qt/CommandLineParser.hpp"

#include <optional>
#include <QSettings>
#include "ores.comms/messaging/handshake_protocol.hpp"
#include "ores.qt/TelemetrySettingsDialog.hpp"

namespace ores::qt {

CommandLineParser::CommandLineParser() {
    setupOptions();
}

void CommandLineParser::setupOptions() {
    parser_.setApplicationDescription("Qt UI for ORE Studio");
    parser_.addHelpOption();
    parser_.addVersionOption();

    // Logging options
    parser_.addOption({
        {"e", "log-enabled"},
        "Generate a log file."
    });

    parser_.addOption({
        {"l", "log-level"},
        "What level to use for logging. Valid values: trace, debug, info, "
        "warn, error.",
        "level",
        "info"
    });

    parser_.addOption({
        "log-to-console",
        "Output logging to the console, as well as to file."
    });

    parser_.addOption({
        "log-directory",
        "Where to place the log files.",
        "directory",
        "log"
    });

    parser_.addOption({
        "log-filename",
        "Name of the log file.",
        "filename",
        "ores.qt.log"
    });

    parser_.addOption({
        "log-include-pid",
        "Include the process ID in the log filename."
    });

    // Compression options
    parser_.addOption({
        {"c", "compression-enabled"},
        "Enable compression for network communication."
    });

    parser_.addOption({
        "compression-algorithm",
        "Compression algorithm to use. Valid values: auto, zlib, gzip, bzip2.",
        "algorithm",
        "auto"
    });

    // Instance identification options
    parser_.addOption({
        {"n", "instance-name"},
        "Name for this application instance (for multi-instance testing).",
        "name"
    });

    parser_.addOption({
        "instance-color",
        "Color for the window title bar as RGB hex (e.g., FF0000 for red).",
        "color"
    });
}

void CommandLineParser::process(const QCoreApplication& app) {
    parser_.process(app);
    saveToSettings();
}

void CommandLineParser::saveToSettings() {
    QSettings settings;

    // If logging options provided via CLI, persist them to QSettings
    if (isLoggingEnabled()) {
        settings.beginGroup(TelemetrySettingsDialog::settingsPrefix());
        settings.beginGroup("logging");
        settings.setValue("enabled", true);
        settings.setValue("level", parser_.value("log-level"));
        settings.setValue("console", parser_.isSet("log-to-console"));
        settings.setValue("directory", parser_.value("log-directory"));
        settings.setValue("filename", parser_.value("log-filename"));
        settings.setValue("include_pid", parser_.isSet("log-include-pid"));
        settings.endGroup();
        settings.endGroup();
    }

    // If compression options provided via CLI, persist them to QSettings
    if (isCompressionEnabled()) {
        settings.beginGroup(TelemetrySettingsDialog::settingsPrefix());
        settings.beginGroup("compression");
        settings.setValue("enabled", true);
        settings.setValue("algorithm", parser_.value("compression-algorithm"));
        settings.endGroup();
        settings.endGroup();
    }
}

bool CommandLineParser::isLoggingEnabled() const {
    return parser_.isSet("log-enabled");
}

std::optional<logging::logging_options> CommandLineParser::loggingOptions() const {
    // Command line takes precedence over QSettings
    if (isLoggingEnabled()) {
        logging::logging_options r;
        r.filename = parser_.value("log-filename").toStdString();
        r.output_to_console = parser_.isSet("log-to-console");
        r.output_directory = parser_.value("log-directory").toStdString();
        r.severity = parser_.value("log-level").toStdString();
        r.include_pid = parser_.isSet("log-include-pid");
        return r;
    }

    // Fall back to QSettings from TelemetrySettingsDialog
    QSettings settings;
    settings.beginGroup(TelemetrySettingsDialog::settingsPrefix());
    settings.beginGroup("logging");

    const bool settingsEnabled = settings.value("enabled", false).toBool();
    const bool consoleEnabled = settings.value("console", false).toBool();

    settings.endGroup();
    settings.endGroup();

    // If neither command line nor settings enable logging, disable it
    if (!settingsEnabled && !consoleEnabled) {
        return std::nullopt;
    }

    // Load options from QSettings via TelemetrySettingsDialog helper
    return TelemetrySettingsDialog::loadLoggingSettings();
}

bool CommandLineParser::isCompressionEnabled() const {
    return parser_.isSet("compression-enabled");
}

std::uint8_t CommandLineParser::supportedCompression() const {
    using namespace comms::messaging;

    // Command line takes precedence over QSettings
    if (isCompressionEnabled()) {
        const auto algorithm = parser_.value("compression-algorithm").toLower().toStdString();

        if (algorithm == "zlib") {
            return COMPRESSION_SUPPORT_ZLIB;
        } else if (algorithm == "gzip") {
            return COMPRESSION_SUPPORT_GZIP;
        } else if (algorithm == "bzip2") {
            return COMPRESSION_SUPPORT_BZIP2;
        } else {
            // "auto" or any unrecognized value defaults to all supported
            return COMPRESSION_SUPPORT_ALL;
        }
    }

    // Fall back to QSettings from TelemetrySettingsDialog
    if (!TelemetrySettingsDialog::isCompressionEnabled()) {
        return 0;
    }

    const auto algorithm = TelemetrySettingsDialog::compressionAlgorithm().toLower().toStdString();

    if (algorithm == "zlib") {
        return COMPRESSION_SUPPORT_ZLIB;
    } else if (algorithm == "gzip") {
        return COMPRESSION_SUPPORT_GZIP;
    } else if (algorithm == "bzip2") {
        return COMPRESSION_SUPPORT_BZIP2;
    } else {
        // "auto" or any unrecognized value defaults to all supported
        return COMPRESSION_SUPPORT_ALL;
    }
}

QString CommandLineParser::instanceName() const {
    return parser_.value("instance-name");
}

QColor CommandLineParser::instanceColor() const {
    const QString colorStr = parser_.value("instance-color");
    if (colorStr.isEmpty()) {
        return QColor();  // Invalid color
    }

    // Prepend # if not present for QColor parsing
    QString hexColor = colorStr;
    if (!hexColor.startsWith('#')) {
        hexColor.prepend('#');
    }

    return QColor(hexColor);
}

}
