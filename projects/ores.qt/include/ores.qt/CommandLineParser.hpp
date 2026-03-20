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
#ifndef ORES_QT_COMMAND_LINE_PARSER_HPP
#define ORES_QT_COMMAND_LINE_PARSER_HPP

#include <optional>
#include <string>
#include <QCoreApplication>
#include <QCommandLineParser>
#include <QString>
#include <QColor>
#include "ores.logging/logging_options.hpp"

namespace ores::qt {

/**
 * @brief Encapsulates Qt command line parsing for the application.
 *
 * Provides a clean interface for parsing command line options and
 * extracting configuration values.
 */
class CommandLineParser final {
public:
    /**
     * @brief Construct the parser and configure all options.
     */
    CommandLineParser();

    /**
     * @brief Process command line arguments.
     *
     * @param app The Qt application instance
     */
    void process(const QCoreApplication& app);

    /**
     * @brief Get logging configuration based on parsed options.
     *
     * Returns std::nullopt if --log-enabled is not set, which disables logging.
     *
     * @return Logging options configured from command line arguments
     */
    [[nodiscard]] std::optional<logging::logging_options> loggingOptions() const;

    /**
     * @brief Check if logging is enabled.
     */
    [[nodiscard]] bool isLoggingEnabled() const;

    /**
     * @brief Get the instance name for identifying this application instance.
     *
     * Used when running multiple instances for testing or debugging.
     * Returns empty string if not specified.
     *
     * @return Instance name or empty string
     */
    [[nodiscard]] QString instanceName() const;

    /**
     * @brief Get the instance color for the title bar.
     *
     * Returns the color specified via --instance-color as a QColor.
     * Returns an invalid color (QColor()) if not specified.
     *
     * @return Instance color or invalid QColor
     */
    [[nodiscard]] QColor instanceColor() const;

    /**
     * @brief Get the HTTP base URL for the compute service.
     *
     * Returns empty string if not specified on the command line.
     */
    [[nodiscard]] std::string httpBaseUrl() const;


private:
    void setupOptions();
    void saveToSettings();

    QCommandLineParser parser_;
};

}

#endif
