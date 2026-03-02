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
#ifndef ORES_QT_CRON_EXPRESSION_WIDGET_HPP
#define ORES_QT_CRON_EXPRESSION_WIDGET_HPP

#include <QWidget>
#include <QLineEdit>
#include <QLabel>
#include <QString>

namespace ores::qt {

/**
 * @brief User-friendly widget for entering and editing cron expressions.
 *
 * Provides quick preset buttons (Hourly, Daily, etc.), individual field
 * editors for the five standard cron fields, a read-only expression display,
 * and a human-readable description of the schedule.
 *
 * Layout (top to bottom):
 *   - Quick preset toolbar
 *   - Five field editors (Min, Hour, Day, Month, Weekday)
 *   - Expression display + human-readable description
 */
class CronExpressionWidget : public QWidget {
    Q_OBJECT

public:
    explicit CronExpressionWidget(QWidget* parent = nullptr);

    /**
     * @brief Returns the current cron expression string (5-field).
     */
    QString cronExpression() const;

    /**
     * @brief Sets all fields from a cron expression string.
     */
    void setCronExpression(const QString& expr);

    /**
     * @brief Returns true if the current expression parses successfully.
     */
    bool isValid() const;

    /**
     * @brief Enables or disables editing of all fields and preset buttons.
     */
    void setReadOnly(bool readOnly);

signals:
    void cronChanged(const QString& expression);

private slots:
    void onHourlyClicked();
    void onDailyClicked();
    void onWeekdaysClicked();
    void onWeeklyClicked();
    void onMonthlyClicked();
    void onFieldChanged();

private:
    void updateExpression();
    QString describeExpression() const;

    QLineEdit* minuteEdit_;
    QLineEdit* hourEdit_;
    QLineEdit* dayEdit_;
    QLineEdit* monthEdit_;
    QLineEdit* weekdayEdit_;
    QLineEdit* expressionEdit_;  ///< Read-only display of assembled cron string
    QLabel*    descriptionLabel_;
};

}

#endif
