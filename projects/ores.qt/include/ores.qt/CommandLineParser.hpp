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

#include <QCoreApplication>
#include <QCommandLineParser>
#include "ores.utility/log/logging_options.hpp"

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
     * Returns empty options (logging disabled) if --log-enabled is not set.
     *
     * @return Logging options configured from command line arguments
     */
    [[nodiscard]] utility::log::logging_options loggingOptions() const;

    /**
     * @brief Check if logging is enabled.
     */
    [[nodiscard]] bool isLoggingEnabled() const;

private:
    void setupOptions();

    QCommandLineParser parser_;
};

}

#endif
