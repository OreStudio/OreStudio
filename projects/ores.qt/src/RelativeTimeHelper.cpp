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
#include "ores.qt/RelativeTimeHelper.hpp"

#include <QDateTime>
#include "ores.utility/datetime/relative_time_formatter.hpp"

namespace ores::qt {

namespace {

std::optional<std::chrono::system_clock::time_point>
parse_datetime(const QString& datetime_str) {
    // Try multiple formats
    QDateTime dt = QDateTime::fromString(datetime_str, Qt::ISODate);

    if (!dt.isValid())
        dt = QDateTime::fromString(datetime_str, "yyyy-MM-dd HH:mm:ss");

    if (!dt.isValid())
        dt = QDateTime::fromString(datetime_str, "yyyy-MM-dd");

    if (!dt.isValid())
        return std::nullopt;

    // Convert QDateTime to std::chrono::system_clock::time_point
    const auto msecs_since_epoch = dt.toMSecsSinceEpoch();
    return std::chrono::system_clock::time_point(
        std::chrono::milliseconds(msecs_since_epoch));
}

}

QString relative_time_helper::format(const std::string& recorded_at) {
    return format(QString::fromStdString(recorded_at));
}

QString relative_time_helper::format(const QString& recorded_at) {
    if (recorded_at.isEmpty())
        return recorded_at;

    auto tp = parse_datetime(recorded_at);
    if (!tp.has_value())
        return recorded_at; // Return original if parsing fails

    utility::datetime::relative_time_formatter formatter;
    return QString::fromStdString(formatter.format(*tp));
}

}
