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

#include <QHBoxLayout>
#include <QVBoxLayout>
#include <QGridLayout>
#include <QPushButton>
#include <QLabel>
#include <QLineEdit>
#include <QFrame>
#include "ores.scheduler/domain/cron_expression.hpp"

namespace ores::qt {

CronExpressionWidget::CronExpressionWidget(QWidget* parent)
    : QWidget(parent),
      minuteEdit_(new QLineEdit(this)),
      hourEdit_(new QLineEdit(this)),
      dayEdit_(new QLineEdit(this)),
      monthEdit_(new QLineEdit(this)),
      weekdayEdit_(new QLineEdit(this)),
      expressionEdit_(new QLineEdit(this)),
      descriptionLabel_(new QLabel(this)) {

    auto* mainLayout = new QVBoxLayout(this);
    mainLayout->setContentsMargins(0, 0, 0, 0);
    mainLayout->setSpacing(4);

    // ── Quick preset buttons ──────────────────────────────────────────────
    auto* presetLayout = new QHBoxLayout;
    presetLayout->setSpacing(4);

    auto makePreset = [&](const QString& label, auto* receiver, auto slot) {
        auto* btn = new QPushButton(label, this);
        btn->setSizePolicy(QSizePolicy::Preferred, QSizePolicy::Fixed);
        connect(btn, &QPushButton::clicked, receiver, slot);
        presetLayout->addWidget(btn);
        return btn;
    };

    makePreset(tr("Hourly"),      this, &CronExpressionWidget::onHourlyClicked);
    makePreset(tr("Daily 6am"),   this, &CronExpressionWidget::onDailyClicked);
    makePreset(tr("Weekdays 6am"),this, &CronExpressionWidget::onWeekdaysClicked);
    makePreset(tr("Weekly Mon"),  this, &CronExpressionWidget::onWeeklyClicked);
    makePreset(tr("Monthly"),     this, &CronExpressionWidget::onMonthlyClicked);
    presetLayout->addStretch();
    mainLayout->addLayout(presetLayout);

    // ── Five field editors ─────────────────────────────────────────────────
    auto* fieldsLayout = new QHBoxLayout;
    fieldsLayout->setSpacing(6);

    auto addField = [&](const QString& label, QLineEdit* edit) {
        auto* lbl = new QLabel(label, this);
        edit->setFixedWidth(48);
        edit->setAlignment(Qt::AlignCenter);
        edit->setPlaceholderText("*");
        fieldsLayout->addWidget(lbl);
        fieldsLayout->addWidget(edit);
        connect(edit, &QLineEdit::textChanged, this,
                &CronExpressionWidget::onFieldChanged);
    };

    addField(tr("Min:"),     minuteEdit_);
    addField(tr("Hour:"),    hourEdit_);
    addField(tr("Day:"),     dayEdit_);
    addField(tr("Month:"),   monthEdit_);
    addField(tr("Weekday:"), weekdayEdit_);
    fieldsLayout->addStretch();
    mainLayout->addLayout(fieldsLayout);

    // ── Expression display + description ──────────────────────────────────
    auto* exprLayout = new QHBoxLayout;
    exprLayout->setSpacing(6);

    auto* exprLabel = new QLabel(tr("Expression:"), this);
    expressionEdit_->setReadOnly(true);
    expressionEdit_->setFixedWidth(160);
    expressionEdit_->setStyleSheet("QLineEdit { background: palette(window); }");

    descriptionLabel_->setWordWrap(false);
    descriptionLabel_->setSizePolicy(QSizePolicy::Expanding, QSizePolicy::Preferred);

    exprLayout->addWidget(exprLabel);
    exprLayout->addWidget(expressionEdit_);
    exprLayout->addWidget(descriptionLabel_);
    mainLayout->addLayout(exprLayout);

    // Initialise with "every hour"
    setCronExpression("0 * * * *");
}

QString CronExpressionWidget::cronExpression() const {
    return expressionEdit_->text();
}

void CronExpressionWidget::setCronExpression(const QString& expr) {
    const QStringList parts = expr.trimmed().split(' ', Qt::SkipEmptyParts);
    if (parts.size() != 5) return;

    // Block all field signals during bulk update to avoid repeated onFieldChanged calls
    minuteEdit_->blockSignals(true);
    hourEdit_->blockSignals(true);
    dayEdit_->blockSignals(true);
    monthEdit_->blockSignals(true);
    weekdayEdit_->blockSignals(true);

    minuteEdit_->setText(parts[0]);
    hourEdit_->setText(parts[1]);
    dayEdit_->setText(parts[2]);
    monthEdit_->setText(parts[3]);
    weekdayEdit_->setText(parts[4]);

    minuteEdit_->blockSignals(false);
    hourEdit_->blockSignals(false);
    dayEdit_->blockSignals(false);
    monthEdit_->blockSignals(false);
    weekdayEdit_->blockSignals(false);

    updateExpression();
}

bool CronExpressionWidget::isValid() const {
    const std::string s = expressionEdit_->text().toStdString();
    return scheduler::domain::cron_expression::from_string(s).has_value();
}

void CronExpressionWidget::setReadOnly(bool readOnly) {
    minuteEdit_->setReadOnly(readOnly);
    hourEdit_->setReadOnly(readOnly);
    dayEdit_->setReadOnly(readOnly);
    monthEdit_->setReadOnly(readOnly);
    weekdayEdit_->setReadOnly(readOnly);

    // Disable preset buttons by finding all child QPushButtons
    for (auto* btn : findChildren<QPushButton*>()) {
        btn->setEnabled(!readOnly);
    }
}

void CronExpressionWidget::onHourlyClicked() {
    setCronExpression("0 * * * *");
    emit cronChanged(cronExpression());
}

void CronExpressionWidget::onDailyClicked() {
    setCronExpression("0 6 * * *");
    emit cronChanged(cronExpression());
}

void CronExpressionWidget::onWeekdaysClicked() {
    setCronExpression("0 6 * * 1-5");
    emit cronChanged(cronExpression());
}

void CronExpressionWidget::onWeeklyClicked() {
    setCronExpression("0 6 * * 1");
    emit cronChanged(cronExpression());
}

void CronExpressionWidget::onMonthlyClicked() {
    setCronExpression("0 6 1 * *");
    emit cronChanged(cronExpression());
}

void CronExpressionWidget::onFieldChanged() {
    updateExpression();
    emit cronChanged(cronExpression());
}

void CronExpressionWidget::updateExpression() {
    const QString min     = minuteEdit_->text().trimmed().isEmpty()  ? "*" : minuteEdit_->text().trimmed();
    const QString hour    = hourEdit_->text().trimmed().isEmpty()    ? "*" : hourEdit_->text().trimmed();
    const QString day     = dayEdit_->text().trimmed().isEmpty()     ? "*" : dayEdit_->text().trimmed();
    const QString month   = monthEdit_->text().trimmed().isEmpty()   ? "*" : monthEdit_->text().trimmed();
    const QString weekday = weekdayEdit_->text().trimmed().isEmpty() ? "*" : weekdayEdit_->text().trimmed();

    const QString expr = QString("%1 %2 %3 %4 %5")
        .arg(min).arg(hour).arg(day).arg(month).arg(weekday);

    expressionEdit_->setText(expr);

    const bool valid = isValid();
    if (valid) {
        expressionEdit_->setStyleSheet("QLineEdit { background: palette(window); }");
        descriptionLabel_->setText(describeExpression());
    } else {
        expressionEdit_->setStyleSheet(
            "QLineEdit { background: palette(window); border: 1px solid red; }");
        descriptionLabel_->setText(tr("(invalid)"));
    }
}

QString CronExpressionWidget::describeExpression() const {
    const QString expr = expressionEdit_->text().trimmed();
    const QStringList p = expr.split(' ', Qt::SkipEmptyParts);
    if (p.size() != 5)
        return expr;

    const QString& min     = p[0];
    const QString& hour    = p[1];
    const QString& day     = p[2];
    const QString& month   = p[3];
    const QString& weekday = p[4];

    // Every minute
    if (min == "*" && hour == "*" && day == "*" && month == "*" && weekday == "*")
        return tr("Every minute");

    // Every hour at minute 0
    if (min == "0" && hour == "*" && day == "*" && month == "*" && weekday == "*")
        return tr("Every hour");

    // Helpers: format hour as HH:00 AM/PM
    auto formatHour = [](const QString& h) -> QString {
        bool ok = false;
        int n = h.toInt(&ok);
        if (!ok || n < 0 || n > 23) return h + ":00";
        const QString suffix = n < 12 ? "AM" : "PM";
        const int display = n == 0 ? 12 : (n <= 12 ? n : n - 12);
        return QString("%1:00 %2").arg(display).arg(suffix);
    };

    static const QStringList dayNames = {
        "", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"
    };
    static const QStringList ordinals = {
        "", "1st", "2nd", "3rd", "4th", "5th", "6th", "7th", "8th", "9th", "10th",
        "11th", "12th", "13th", "14th", "15th", "16th", "17th", "18th", "19th", "20th",
        "21st", "22nd", "23rd", "24th", "25th", "26th", "27th", "28th", "29th", "30th", "31st"
    };

    if (min == "0" && day == "*" && month == "*") {
        // At H:00, every day
        if (weekday == "*") {
            return tr("At %1, every day").arg(formatHour(hour));
        }
        // At H:00, Mon-Fri
        if (weekday == "1-5") {
            return tr("At %1, Monday through Friday").arg(formatHour(hour));
        }
        // At H:00, every <day name>
        bool ok = false;
        int wd = weekday.toInt(&ok);
        if (ok && wd >= 0 && wd <= 7) {
            const int idx = (wd == 0 || wd == 7) ? 7 : wd;
            if (idx < dayNames.size()) {
                return tr("At %1, every %2").arg(formatHour(hour)).arg(dayNames[idx]);
            }
        }
    }

    // At H:00, on the Nth of every month
    if (min == "0" && weekday == "*" && month == "*") {
        bool dayOk = false;
        int d = day.toInt(&dayOk);
        if (dayOk && d >= 1 && d <= 31) {
            return tr("At %1, on the %2 of every month")
                .arg(formatHour(hour))
                .arg(ordinals[d]);
        }
    }

    return expr;
}

}
