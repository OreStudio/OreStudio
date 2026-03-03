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
#ifndef ORES_QT_CRON_FIELD_WIDGET_HPP
#define ORES_QT_CRON_FIELD_WIDGET_HPP

#include <QWidget>
#include <QList>
#include <QVector>
#include <QString>
#include <QStringList>

class QRadioButton;
class QComboBox;
class QCheckBox;

namespace ores::qt {

/**
 * @brief Configuration for one cron field (minutes, hours, etc.).
 */
struct FieldConfig {
    int          min_val;
    int          max_val;
    QStringList  labels;              // empty -> zero-padded numbers
    QList<int>   freq_steps;          // step values for the Frequency combo
    int          checkboxes_per_row;
};

/**
 * @brief One tab inside CronEditorDialog.
 *
 * Offers three mutually-exclusive modes:
 *   - Frequency: step-based schedule (Every / At + Starting)
 *   - Range:     a min-max span
 *   - Choice:    individually ticked values
 */
class CronFieldWidget : public QWidget {
    Q_OBJECT

public:
    explicit CronFieldWidget(const FieldConfig& cfg, QWidget* parent = nullptr);

    /** Returns the cron field string for the current selection. */
    QString value() const;

    /** Parses a cron field string and updates the UI accordingly. */
    void setValue(const QString& cron_field);

signals:
    void changed();

private slots:
    void onModeClicked();

private:
    void    updateEnabled();
    QString labelFor(int v) const;

    FieldConfig cfg_;

    QRadioButton* freqRadio_;
    QComboBox*    everyCombo_;
    QComboBox*    startCombo_;

    QRadioButton* rangeRadio_;
    QComboBox*    minCombo_;
    QComboBox*    maxCombo_;

    QRadioButton*       choiceRadio_;
    QVector<QCheckBox*> checkboxes_;
};

}

#endif
