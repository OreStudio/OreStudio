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
#ifndef ORES_QT_CALENDAR_HOLIDAY_FETCHER_HPP
#define ORES_QT_CALENDAR_HOLIDAY_FETCHER_HPP

#include "ores.qt/export.hpp"
#include <QDate>
#include <QMap>
#include <QString>
#include <QStringList>
#include <expected>
#include <string>
#include <vector>

namespace ores::qt {

class ClientManager;

/**
 * @brief Maps each non-business date to the calendar code(s) it is
 * non-business for -- both weekends and holidays, undistinguished
 * (calendar_date::is_business_day covers both; see its own doc comment).
 */
using calendar_holiday_map = QMap<QDate, QStringList>;

/**
 * @brief Fetches materialised non-business dates for @p calendarCodes,
 * clipped to [from, to], from the server's calendar_dates table.
 *
 * Synchronous call intended to be run from within QtConcurrent::run.
 * Pages through refdata.v1.calendar_dates.list_by_calendar_code
 * (materialised, DQ-owned data -- never a live QuantLib computation) per
 * calendar code, stopping early once a page's dates run past @p to.
 * Returns an error message on failure, distinguishing it from a
 * legitimately-empty result. A per-calendar failure (e.g. an unknown
 * code) is skipped rather than failing the whole batch, since the
 * caller typically wants "highlight what's available" over "all or
 * nothing" for a UI picker.
 */
ORES_QT_API std::expected<calendar_holiday_map, QString>
fetch_calendar_holidays(ClientManager* cm,
                        const std::vector<std::string>& calendarCodes,
                        const QDate& from,
                        const QDate& to);

}

#endif
