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
#include <QString>

class QPushButton;

namespace ores::qt {

/**
 * @brief Compact widget for entering and editing cron expressions.
 *
 * Shows an editable cron string (red border when invalid) and a "..."
 * button that opens the CronEditorDialog for a tab-based visual builder.
 * The public API is identical to the old widget so all dialogs that
 * embed it as a custom widget require no changes.
 */
class CronExpressionWidget : public QWidget {
    Q_OBJECT

public:
    explicit CronExpressionWidget(QWidget* parent = nullptr);

    /** Returns the current cron expression string (5-field). */
    QString cronExpression() const;

    /** Sets the cron expression, updating the line edit. */
    void setCronExpression(const QString& expr);

    /** Returns true if the current expression parses successfully. */
    bool isValid() const;

    /** Enables or disables editing and the builder button. */
    void setReadOnly(bool readOnly);

signals:
    void cronChanged(const QString& expression);

private slots:
    void onExpressionEdited();
    void onBuilderClicked();

private:
    void updateValidationStyle();

    QLineEdit*   expressionEdit_;
    QPushButton* builderButton_;
};

}

#endif
