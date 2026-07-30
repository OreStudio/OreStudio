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
#include "ores.qt/HolidayAwareDatePicker.hpp"
#include <QAbstractItemView>
#include <QCalendarWidget>
#include <QFutureWatcher>
#include <QHelpEvent>
#include <QTextCharFormat>
#include <QToolTip>
#include <QtConcurrent>

namespace ores::qt {

using namespace ores::logging;

namespace {

// A practical window around the month the popup happens to be showing --
// wide enough to cover the common case (spot/fixing dates a few months to
// a couple of years out) without eagerly fetching the full multi-decade
// materialisation horizon per calendar. Expanded on demand as the user
// navigates the popup further out; see ensureWindowLoaded().
constexpr int k_window_years_back = 1;
constexpr int k_window_years_forward = 5;

}

HolidayAwareDatePicker::HolidayAwareDatePicker(QWidget* parent)
    : QDateEdit(parent) {
    setCalendarPopup(true);
    setDisplayFormat(QStringLiteral("yyyy-MM-dd"));
    setMinimumDate(QDate(1900, 1, 1));
    // A single space signals the blank/unset state visually, matching
    // OreDateEdit's convention.
    setSpecialValueText(QStringLiteral(" "));
    setDate(minimumDate());

    calendarWidget_ = calendarWidget();
    if (calendarWidget_) {
        connect(calendarWidget_,
               &QCalendarWidget::currentPageChanged,
               this,
               &HolidayAwareDatePicker::onCalendarPageChanged);
        if (auto* view = calendarWidget_->findChild<QAbstractItemView*>()) {
            view->setMouseTracking(true);
            view->viewport()->installEventFilter(this);
            tooltipFilterInstalled_ = true;
        }
    }
}

void HolidayAwareDatePicker::setClientManager(ClientManager* clientManager) {
    clientManager_ = clientManager;
}

void HolidayAwareDatePicker::setCalendarCodes(std::vector<std::string> calendarCodes) {
    calendarCodes_ = std::move(calendarCodes);
    ++generation_; // Invalidates any in-flight loadWindow() future's result.

    // Reset every date this widget previously highlighted -- setDateTextFormat
    // only ever adds/overwrites a per-date format, it has no bulk-clear, so
    // without this a date that was non-business under the old calendar set
    // but isn't under the new one would stay visually highlighted forever.
    if (calendarWidget_) {
        for (auto it = holidays_.constBegin(); it != holidays_.constEnd(); ++it)
            calendarWidget_->setDateTextFormat(it.key(), QTextCharFormat());
    }

    holidays_.clear();
    loadedFrom_ = QDate();
    loadedTo_ = QDate();
    if (calendarWidget_)
        ensureWindowLoaded(calendarWidget_->selectedDate());
}

const std::vector<std::string>& HolidayAwareDatePicker::calendarCodes() const {
    return calendarCodes_;
}

std::string HolidayAwareDatePicker::isoDate() const {
    if (date() == minimumDate())
        return {};
    return date().toString(QStringLiteral("yyyy-MM-dd")).toStdString();
}

void HolidayAwareDatePicker::setIsoDate(const std::string& iso) {
    if (iso.empty()) {
        setDate(minimumDate());
        return;
    }
    const QDate d = QDate::fromString(QString::fromStdString(iso), QStringLiteral("yyyy-MM-dd"));
    setDate(d.isValid() ? d : minimumDate());
}

void HolidayAwareDatePicker::onCalendarPageChanged(int year, int month) {
    ensureWindowLoaded(QDate(year, month, 1));
}

void HolidayAwareDatePicker::ensureWindowLoaded(const QDate& around) {
    if (calendarCodes_.empty() || !clientManager_ || !around.isValid())
        return;

    const QDate desiredFrom = around.addYears(-k_window_years_back);
    const QDate desiredTo = around.addYears(k_window_years_forward);

    if (loadedFrom_.isValid() && loadedTo_.isValid() && desiredFrom >= loadedFrom_ &&
        desiredTo <= loadedTo_) {
        return; // Already covered.
    }

    const QDate from = loadedFrom_.isValid() ? std::min(desiredFrom, loadedFrom_) : desiredFrom;
    const QDate to = loadedTo_.isValid() ? std::max(desiredTo, loadedTo_) : desiredTo;
    loadWindow(from, to);
}

void HolidayAwareDatePicker::loadWindow(const QDate& from, const QDate& to) {
    auto* cm = clientManager_;
    const auto codes = calendarCodes_;
    const auto generation = generation_;

    auto* watcher = new QFutureWatcher<std::expected<calendar_holiday_map, QString>>(this);
    connect(watcher,
           &QFutureWatcher<std::expected<calendar_holiday_map, QString>>::finished,
           this,
           [this, watcher, from, to, generation]() {
               watcher->deleteLater();

               // setCalendarCodes() ran again while this fetch was in
               // flight -- its result is for a calendar set that no
               // longer applies; discard it rather than merging stale
               // data back into holidays_.
               if (generation != generation_)
                   return;

               const auto result = watcher->result();
               if (!result) {
                   BOOST_LOG_SEV(lg(), warn)
                       << "Failed to load calendar holidays: " << result.error().toStdString();
                   emit errorMessage(result.error());
                   return;
               }
               for (auto it = result->constBegin(); it != result->constEnd(); ++it)
                   holidays_[it.key()] = it.value();
               loadedFrom_ = loadedFrom_.isValid() ? std::min(from, loadedFrom_) : from;
               loadedTo_ = loadedTo_.isValid() ? std::max(to, loadedTo_) : to;
               applyHighlights();
           });
    watcher->setFuture(QtConcurrent::run(
        [cm, codes, from, to]() { return fetch_calendar_holidays(cm, codes, from, to); }));
}

void HolidayAwareDatePicker::applyHighlights() {
    if (!calendarWidget_)
        return;

    QTextCharFormat highlight;
    highlight.setBackground(calendarWidget_->palette().alternateBase().color().darker(110));
    highlight.setForeground(calendarWidget_->palette().text().color());

    for (auto it = holidays_.constBegin(); it != holidays_.constEnd(); ++it)
        calendarWidget_->setDateTextFormat(it.key(), highlight);
}

QDate HolidayAwareDatePicker::cellDateAt(const QPoint& viewportPos) const {
    if (!calendarWidget_)
        return {};
    auto* view = calendarWidget_->findChild<QAbstractItemView*>();
    if (!view)
        return {};
    const QModelIndex index = view->indexAt(viewportPos);
    if (!index.isValid())
        return {};

    // QCalendarWidget's internal grid has no header rows/columns in its
    // model (headers are drawn by the view itself), so index (0,0) is
    // the first day cell. Reconstruct its date from the shown month's
    // first-of-month, anchored back to the grid's first visible cell.
    const QDate firstOfMonth(calendarWidget_->yearShown(), calendarWidget_->monthShown(), 1);
    const int firstDayCol =
        (firstOfMonth.dayOfWeek() - static_cast<int>(calendarWidget_->firstDayOfWeek()) + 7) % 7;
    const QDate anchor = firstOfMonth.addDays(-firstDayCol);
    return anchor.addDays(index.row() * 7 + index.column());
}

bool HolidayAwareDatePicker::eventFilter(QObject* watched, QEvent* event) {
    if (tooltipFilterInstalled_ && event->type() == QEvent::ToolTip) {
        auto* helpEvent = static_cast<QHelpEvent*>(event);
        const QDate cellDate = cellDateAt(helpEvent->pos());
        auto it = holidays_.constFind(cellDate);
        if (cellDate.isValid() && it != holidays_.constEnd()) {
            QToolTip::showText(helpEvent->globalPos(),
                               tr("Non-business day for: %1").arg(it.value().join(QStringLiteral(", "))),
                               qobject_cast<QWidget*>(watched));
            return true;
        }
        QToolTip::hideText();
        return true;
    }
    return QDateEdit::eventFilter(watched, event);
}

}
