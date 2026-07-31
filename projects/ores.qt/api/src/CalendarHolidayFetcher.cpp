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
#include "ores.qt/CalendarHolidayFetcher.hpp"
#include "ores.qt/ClientManager.hpp"
#include "ores.refdata.api/messaging/calendar_date_protocol.hpp"

namespace ores::qt {

namespace {

QDate to_qdate(const std::chrono::year_month_day& ymd) {
    return QDate(static_cast<int>(ymd.year()),
                 static_cast<int>(static_cast<unsigned>(ymd.month())),
                 static_cast<int>(static_cast<unsigned>(ymd.day())));
}

// One calendar's non-business dates within [from, to], paged from the
// server. calendar_date rows are ordered by date ascending (see
// calendar_date_repository::read_latest_by_calendar), so paging can stop
// as soon as a page's dates run past `to`, rather than fetching every
// materialised row out to the full multi-decade horizon.
constexpr std::uint32_t k_page_size = 500;

void fetch_one_calendar(ClientManager* cm,
                        const std::string& calendarCode,
                        const QDate& from,
                        const QDate& to,
                        calendar_holiday_map& out) {
    std::uint32_t offset = 0;
    for (;;) {
        refdata::messaging::get_calendar_dates_by_calendar_request request;
        request.calendar_code = calendarCode;
        request.offset = offset;
        request.limit = k_page_size;

        auto response = cm->process_authenticated_request(std::move(request));
        if (!response || !response->success || response->calendar_dates.empty())
            return;

        bool past_window = false;
        for (const auto& row : response->calendar_dates) {
            const QDate date = to_qdate(row.date);
            if (date > to) {
                past_window = true;
                break;
            }
            if (date < from)
                continue;
            if (!row.is_business_day)
                out[date].append(QString::fromStdString(calendarCode));
        }

        if (past_window)
            return;

        offset += static_cast<std::uint32_t>(response->calendar_dates.size());
        if (offset >= static_cast<std::uint32_t>(response->total_available_count))
            return;
    }
}

}

std::expected<calendar_holiday_map, QString>
fetch_calendar_holidays(ClientManager* cm,
                        const std::vector<std::string>& calendarCodes,
                        const QDate& from,
                        const QDate& to) {
    if (!cm)
        return std::unexpected(QStringLiteral("Not connected to server."));

    calendar_holiday_map result;
    for (const auto& code : calendarCodes)
        fetch_one_calendar(cm, code, from, to, result);
    return result;
}

}
