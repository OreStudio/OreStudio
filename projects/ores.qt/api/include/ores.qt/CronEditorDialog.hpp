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
#ifndef ORES_QT_CRON_EDITOR_DIALOG_HPP
#define ORES_QT_CRON_EDITOR_DIALOG_HPP

#include <QDialog>
#include <QString>
#include "ores.qt/CronFieldWidget.hpp"
#include "ores.qt/export.hpp"

class QLabel;
class QTabWidget;

namespace ores::qt {

/**
 * @brief Tabbed dialog for building cron expressions field by field.
 *
 * Five tabs (Minutes, Hours, Days of Month, Month, Days of Week) each provide
 * Frequency, Range, and Choice modes for the corresponding cron field.
 * A description label at the top summarises the schedule in plain English.
 */
class ORES_QT_API CronEditorDialog : public QDialog {
    Q_OBJECT

public:
    explicit CronEditorDialog(QWidget* parent = nullptr);

    /** Returns the assembled 5-field cron expression. */
    QString cronExpression() const;

    /** Populates all five tabs from a 5-field cron expression string. */
    void setCronExpression(const QString& expr);

private slots:
    void onFieldChanged();

private:
    QString describeExpression() const;

    QLabel*          descriptionLabel_;
    QTabWidget*      tabWidget_;
    CronFieldWidget* minuteField_;
    CronFieldWidget* hourField_;
    CronFieldWidget* dayField_;
    CronFieldWidget* monthField_;
    CronFieldWidget* weekdayField_;
};

}

#endif
