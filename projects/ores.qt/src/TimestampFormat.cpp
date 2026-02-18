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
#include "ores.qt/TimestampFormat.hpp"

#include "ores.qt/RelativeTimeHelper.hpp"
#include "ores.platform/time/datetime.hpp"

namespace ores::qt {

timestamp_display_mode timestamp_formatter::mode_ = timestamp_display_mode::relative;

timestamp_display_mode timestamp_formatter::mode() {
    return mode_;
}

void timestamp_formatter::set_mode(timestamp_display_mode m) {
    mode_ = m;
}

QString timestamp_formatter::format(
    const std::chrono::system_clock::time_point& tp) {
    if (mode_ == timestamp_display_mode::relative) {
        return relative_time_helper::format_relative(tp);
    }
    return QString::fromStdString(
        platform::time::datetime::format_time_point(tp));
}

QString timestamp_formatter::format(const std::string& recorded_at) {
    if (mode_ == timestamp_display_mode::relative) {
        return relative_time_helper::format_relative(
            QString::fromStdString(recorded_at));
    }
    return QString::fromStdString(recorded_at);
}

QString timestamp_formatter::format(const QString& recorded_at) {
    if (mode_ == timestamp_display_mode::relative) {
        return relative_time_helper::format_relative(recorded_at);
    }
    return recorded_at;
}

}
