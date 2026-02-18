/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2026 Marco Craveiro <marco.craveiro@gmail.com>
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
#ifndef ORES_QT_TIMESTAMP_FORMAT_HPP
#define ORES_QT_TIMESTAMP_FORMAT_HPP

#include <QString>
#include <chrono>
#include <string>

namespace ores::qt {

/**
 * @brief Display mode for timestamps across the application.
 */
enum class timestamp_display_mode {
    relative, ///< e.g. "5 minutes ago", "yesterday"
    absolute  ///< e.g. "2026-02-17 14:30:00"
};

/**
 * @brief Application-wide timestamp formatter.
 *
 * Provides a single entry point for formatting timestamps throughout
 * the UI. The display mode (relative vs absolute) can be changed at
 * runtime and affects all subsequent formatting calls.
 */
class timestamp_formatter final {
public:
    static timestamp_display_mode mode();
    static void set_mode(timestamp_display_mode m);

    static QString format(const std::chrono::system_clock::time_point& tp);
    static QString format(const std::string& recorded_at);
    static QString format(const QString& recorded_at);

private:
    static timestamp_display_mode mode_;
};

}

#endif
