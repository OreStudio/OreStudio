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
#ifndef ORES_QT_HOLIDAY_AWARE_DATE_PICKER_HPP
#define ORES_QT_HOLIDAY_AWARE_DATE_PICKER_HPP

#include "ores.logging/make_logger.hpp"
#include "ores.qt/CalendarHolidayFetcher.hpp"
#include "ores.qt/export.hpp"
#include <QDateEdit>
#include <string>
#include <vector>

class QCalendarWidget;

namespace ores::qt {

class ClientManager;

/**
 * @brief Drop-in replacement for OreDateEdit that highlights, in its
 * popup calendar grid, every date that is a non-business day (weekend or
 * holiday, undistinguished -- see calendar_date::is_business_day) for one
 * or more given calendars, with a tooltip on hover naming which
 * calendar(s) it applies to.
 *
 * Sourced from the server's materialised calendar_dates table (via
 * fetch_calendar_holidays()) -- never a live QuantLib computation, per
 * the parent [[id:7FF8A057-F95D-4654-B709-13D74525892B][materialisation task]].
 * This is the standard widget for any calendar-sensitive date field
 * across the app (trade dates, fixing dates, spot dates, ...); use it
 * instead of a plain QDateEdit/OreDateEdit whenever the field's value is
 * meant to respect one or more calendars. See the Qt UI patterns doc.
 *
 * Highlights only a practical window around the currently-shown month
 * (loaded lazily as the user navigates the popup, expanding forward or
 * backward one year at a time) rather than the full multi-decade
 * materialisation horizon -- a date picker only ever needs to show a
 * handful of months at a time, and eagerly fetching thousands of rows
 * per calendar up front would be wasted work for the common case.
 */
class ORES_QT_API HolidayAwareDatePicker : public QDateEdit {
    Q_OBJECT

private:
    inline static std::string_view logger_name = "ores.qt.holiday_aware_date_picker";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    explicit HolidayAwareDatePicker(QWidget* parent = nullptr);
    ~HolidayAwareDatePicker() override = default;

    void setClientManager(ClientManager* clientManager);

    /**
     * @brief Sets which calendars' non-business dates to highlight.
     * Triggers an async (re)load of the currently-visible window.
     */
    void setCalendarCodes(std::vector<std::string> calendarCodes);
    [[nodiscard]] const std::vector<std::string>& calendarCodes() const;

    /**
     * @brief Returns the selected date as "yyyy-MM-dd", or an empty string
     *        when the widget is in the blank (unset) state.
     */
    [[nodiscard]] std::string isoDate() const;

    /**
     * @brief Sets the widget to the date represented by @p iso ("yyyy-MM-dd").
     *        An empty string resets the widget to the blank (unset) state.
     */
    void setIsoDate(const std::string& iso);

signals:
    void errorMessage(const QString& message);

private slots:
    void onCalendarPageChanged(int year, int month);

protected:
    bool eventFilter(QObject* watched, QEvent* event) override;

private:
    void ensureWindowLoaded(const QDate& around);
    void loadWindow(const QDate& from, const QDate& to);
    void applyHighlights();
    [[nodiscard]] QDate cellDateAt(const QPoint& viewportPos) const;

    ClientManager* clientManager_ = nullptr;
    std::vector<std::string> calendarCodes_;
    calendar_holiday_map holidays_;
    QDate loadedFrom_;
    QDate loadedTo_;
    QCalendarWidget* calendarWidget_ = nullptr;
    bool tooltipFilterInstalled_ = false;
};

}

#endif
