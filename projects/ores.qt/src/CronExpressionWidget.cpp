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
#include "ores.qt/CronExpressionWidget.hpp"
#include "ores.qt/CronEditorDialog.hpp"

#include <QHBoxLayout>
#include <QPushButton>
#include "ores.scheduler/domain/cron_expression.hpp"

namespace ores::qt {

CronExpressionWidget::CronExpressionWidget(QWidget* parent)
    : QWidget(parent),
      expressionEdit_(new QLineEdit(this)),
      builderButton_(new QPushButton(tr("..."), this)) {

    auto* layout = new QHBoxLayout(this);
    layout->setContentsMargins(0, 0, 0, 0);
    layout->setSpacing(4);

    expressionEdit_->setPlaceholderText("0 * * * *");
    connect(expressionEdit_, &QLineEdit::textChanged,
            this, &CronExpressionWidget::onExpressionEdited);

    builderButton_->setFixedWidth(32);
    builderButton_->setToolTip(tr("Open cron expression builder"));
    connect(builderButton_, &QPushButton::clicked,
            this, &CronExpressionWidget::onBuilderClicked);

    layout->addWidget(expressionEdit_);
    layout->addWidget(builderButton_);

    setCronExpression("0 * * * *");
}

QString CronExpressionWidget::cronExpression() const {
    return expressionEdit_->text().trimmed();
}

void CronExpressionWidget::setCronExpression(const QString& expr) {
    expressionEdit_->blockSignals(true);
    expressionEdit_->setText(expr.trimmed());
    expressionEdit_->blockSignals(false);
    updateValidationStyle();
}

bool CronExpressionWidget::isValid() const {
    const std::string s = expressionEdit_->text().trimmed().toStdString();
    return scheduler::domain::cron_expression::from_string(s).has_value();
}

void CronExpressionWidget::setReadOnly(bool readOnly) {
    expressionEdit_->setReadOnly(readOnly);
    builderButton_->setEnabled(!readOnly);
}

void CronExpressionWidget::onExpressionEdited() {
    updateValidationStyle();
    emit cronChanged(cronExpression());
}

void CronExpressionWidget::onBuilderClicked() {
    CronEditorDialog dlg(this);
    dlg.setCronExpression(cronExpression());
    if (dlg.exec() == QDialog::Accepted) {
        setCronExpression(dlg.cronExpression());
        emit cronChanged(cronExpression());
    }
}

void CronExpressionWidget::updateValidationStyle() {
    if (isValid()) {
        expressionEdit_->setStyleSheet({});
    } else {
        expressionEdit_->setStyleSheet("QLineEdit { border: 1px solid red; }");
    }
}

}
