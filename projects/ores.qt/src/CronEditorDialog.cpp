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
#include "ores.qt/CronEditorDialog.hpp"

#include <QRadioButton>
#include <QCheckBox>
#include <QComboBox>
#include <QLabel>
#include <QTabWidget>
#include <QVBoxLayout>
#include <QHBoxLayout>
#include <QGridLayout>
#include <QDialogButtonBox>
#include <QButtonGroup>
#include <QWidget>
#include <QFrame>

namespace ores::qt {

// ── CronFieldWidget ──────────────────────────────────────────────────────────

CronFieldWidget::CronFieldWidget(const FieldConfig& cfg, QWidget* parent)
    : QWidget(parent), cfg_(cfg) {

    auto* mainLayout = new QVBoxLayout(this);
    mainLayout->setSpacing(10);
    mainLayout->setContentsMargins(12, 12, 12, 12);

    auto* buttonGroup = new QButtonGroup(this);

    // ── Frequency row ─────────────────────────────────────────────────────
    freqRadio_ = new QRadioButton(tr("Frequency"), this);
    buttonGroup->addButton(freqRadio_);

    everyCombo_ = new QComboBox(this);
    everyCombo_->addItem(tr("Every"), 1);
    for (int step : cfg.freq_steps)
        everyCombo_->addItem(QString::number(step), step);

    startCombo_ = new QComboBox(this);
    for (int v = cfg.min_val; v <= cfg.max_val; ++v)
        startCombo_->addItem(labelFor(v), v);

    auto* freqRow    = new QWidget(this);
    auto* freqLayout = new QHBoxLayout(freqRow);
    freqLayout->setContentsMargins(0, 0, 0, 0);
    freqLayout->addWidget(freqRadio_);
    freqLayout->addSpacing(16);
    freqLayout->addWidget(new QLabel(tr("Every / At:"), this));
    freqLayout->addWidget(everyCombo_);
    freqLayout->addSpacing(12);
    freqLayout->addWidget(new QLabel(tr("Starting:"), this));
    freqLayout->addWidget(startCombo_);
    freqLayout->addStretch();
    mainLayout->addWidget(freqRow);

    // ── Range row ─────────────────────────────────────────────────────────
    rangeRadio_ = new QRadioButton(tr("Range"), this);
    buttonGroup->addButton(rangeRadio_);

    minCombo_ = new QComboBox(this);
    maxCombo_ = new QComboBox(this);
    for (int v = cfg.min_val; v <= cfg.max_val; ++v) {
        minCombo_->addItem(labelFor(v), v);
        maxCombo_->addItem(labelFor(v), v);
    }
    maxCombo_->setCurrentIndex(maxCombo_->count() - 1);

    auto* rangeRow    = new QWidget(this);
    auto* rangeLayout = new QHBoxLayout(rangeRow);
    rangeLayout->setContentsMargins(0, 0, 0, 0);
    rangeLayout->addWidget(rangeRadio_);
    rangeLayout->addSpacing(16);
    rangeLayout->addWidget(new QLabel(tr("Min:"), this));
    rangeLayout->addWidget(minCombo_);
    rangeLayout->addSpacing(12);
    rangeLayout->addWidget(new QLabel(tr("Max:"), this));
    rangeLayout->addWidget(maxCombo_);
    rangeLayout->addStretch();
    mainLayout->addWidget(rangeRow);

    // Horizontal separator
    auto* sep = new QFrame(this);
    sep->setFrameShape(QFrame::HLine);
    sep->setFrameShadow(QFrame::Sunken);
    mainLayout->addWidget(sep);

    // ── Choice radio + checkbox grid ──────────────────────────────────────
    choiceRadio_ = new QRadioButton(tr("Choice"), this);
    buttonGroup->addButton(choiceRadio_);

    auto* choiceHeader       = new QWidget(this);
    auto* choiceHeaderLayout = new QHBoxLayout(choiceHeader);
    choiceHeaderLayout->setContentsMargins(0, 0, 0, 0);
    choiceHeaderLayout->addWidget(choiceRadio_);
    choiceHeaderLayout->addStretch();
    mainLayout->addWidget(choiceHeader);

    auto* gridWidget = new QWidget(this);
    auto* gridLayout = new QGridLayout(gridWidget);
    gridLayout->setSpacing(4);
    gridLayout->setContentsMargins(24, 0, 0, 0);

    const int count = cfg.max_val - cfg.min_val + 1;
    for (int i = 0; i < count; ++i) {
        auto* cb = new QCheckBox(labelFor(cfg.min_val + i), gridWidget);
        checkboxes_.push_back(cb);
        connect(cb, &QCheckBox::toggled, this, &CronFieldWidget::changed);
        gridLayout->addWidget(cb, i / cfg.checkboxes_per_row,
                              i % cfg.checkboxes_per_row);
    }
    mainLayout->addWidget(gridWidget);
    mainLayout->addStretch();

    // ── Connections ───────────────────────────────────────────────────────
    connect(buttonGroup, &QButtonGroup::buttonClicked,
            this, &CronFieldWidget::onModeClicked);

    auto emitChanged = [this]() { emit changed(); };
    connect(everyCombo_, QOverload<int>::of(&QComboBox::currentIndexChanged),
            this, emitChanged);
    connect(startCombo_, QOverload<int>::of(&QComboBox::currentIndexChanged),
            this, emitChanged);
    connect(minCombo_,   QOverload<int>::of(&QComboBox::currentIndexChanged),
            this, emitChanged);
    connect(maxCombo_,   QOverload<int>::of(&QComboBox::currentIndexChanged),
            this, emitChanged);

    freqRadio_->setChecked(true);
    updateEnabled();
}

QString CronFieldWidget::labelFor(int v) const {
    if (!cfg_.labels.isEmpty()) {
        const int idx = v - cfg_.min_val;
        if (idx >= 0 && idx < cfg_.labels.size())
            return cfg_.labels[idx];
    }
    return QString("%1").arg(v, 2, 10, QChar('0'));
}

void CronFieldWidget::onModeClicked() {
    updateEnabled();
    emit changed();
}

void CronFieldWidget::updateEnabled() {
    const bool isFreq   = freqRadio_->isChecked();
    const bool isRange  = rangeRadio_->isChecked();
    const bool isChoice = choiceRadio_->isChecked();

    everyCombo_->setEnabled(isFreq);
    startCombo_->setEnabled(isFreq);
    minCombo_->setEnabled(isRange);
    maxCombo_->setEnabled(isRange);
    for (auto* cb : checkboxes_)
        cb->setEnabled(isChoice);
}

QString CronFieldWidget::value() const {
    if (freqRadio_->isChecked()) {
        const int step  = everyCombo_->currentData().toInt();
        const int start = startCombo_->currentData().toInt();
        const bool atMin = (start == cfg_.min_val);
        if (step == 1)
            return atMin ? "*" : QString::number(start);
        return atMin ? QString("*/%1").arg(step)
                     : QString("%1/%2").arg(start).arg(step);
    }

    if (rangeRadio_->isChecked()) {
        const int minV = minCombo_->currentData().toInt();
        const int maxV = maxCombo_->currentData().toInt();
        return QString("%1-%2").arg(minV).arg(maxV);
    }

    // Choice
    QStringList selected;
    for (int i = 0; i < checkboxes_.size(); ++i) {
        if (checkboxes_[i]->isChecked())
            selected << QString::number(cfg_.min_val + i);
    }
    const int total = cfg_.max_val - cfg_.min_val + 1;
    if (selected.isEmpty() || selected.size() == total)
        return "*";
    return selected.join(",");
}

void CronFieldWidget::setValue(const QString& field) {
    const QString f = field.trimmed();

    const QSignalBlocker sb_every(everyCombo_);
    const QSignalBlocker sb_start(startCombo_);
    const QSignalBlocker sb_min(minCombo_);
    const QSignalBlocker sb_max(maxCombo_);
    for (auto* cb : checkboxes_) cb->blockSignals(true);

    auto findAndSet = [](QComboBox* combo, int target) {
        for (int i = 0; i < combo->count(); ++i) {
            if (combo->itemData(i).toInt() == target) {
                combo->setCurrentIndex(i);
                return;
            }
        }
    };

    if (f == "*") {
        freqRadio_->setChecked(true);
        everyCombo_->setCurrentIndex(0);
        startCombo_->setCurrentIndex(0);
    } else if (f.contains('/')) {
        freqRadio_->setChecked(true);
        const QStringList parts = f.split('/');
        const int step  = parts[1].toInt();
        const int start = (parts[0] == "*") ? cfg_.min_val : parts[0].toInt();
        findAndSet(everyCombo_, step);
        findAndSet(startCombo_, start);
    } else if (f.contains('-')) {
        rangeRadio_->setChecked(true);
        const QStringList parts = f.split('-');
        findAndSet(minCombo_, parts[0].toInt());
        findAndSet(maxCombo_, parts[1].toInt());
    } else {
        choiceRadio_->setChecked(true);
        for (auto* cb : checkboxes_) cb->setChecked(false);
        for (const QString& v : f.split(',')) {
            bool ok = false;
            const int n = v.trimmed().toInt(&ok);
            if (ok) {
                const int idx = n - cfg_.min_val;
                if (idx >= 0 && idx < checkboxes_.size())
                    checkboxes_[idx]->setChecked(true);
            }
        }
    }

    for (auto* cb : checkboxes_) cb->blockSignals(false);
    updateEnabled();
}

// ── Field factory helpers ─────────────────────────────────────────────────────

namespace {

CronFieldWidget* makeMinuteField(QWidget* parent) {
    FieldConfig cfg;
    cfg.min_val            = 0;
    cfg.max_val            = 59;
    cfg.freq_steps         = {2, 3, 4, 5, 6, 10, 12, 15, 20, 30};
    cfg.checkboxes_per_row = 10;
    return new CronFieldWidget(cfg, parent);
}

CronFieldWidget* makeHourField(QWidget* parent) {
    FieldConfig cfg;
    cfg.min_val            = 0;
    cfg.max_val            = 23;
    cfg.freq_steps         = {2, 3, 4, 6, 8, 12};
    cfg.checkboxes_per_row = 12;
    return new CronFieldWidget(cfg, parent);
}

CronFieldWidget* makeDayField(QWidget* parent) {
    FieldConfig cfg;
    cfg.min_val            = 1;
    cfg.max_val            = 31;
    cfg.freq_steps         = {2, 3, 4, 5, 7, 10, 14};
    cfg.checkboxes_per_row = 10;
    return new CronFieldWidget(cfg, parent);
}

CronFieldWidget* makeMonthField(QWidget* parent) {
    FieldConfig cfg;
    cfg.min_val            = 1;
    cfg.max_val            = 12;
    cfg.labels             = {"Jan","Feb","Mar","Apr","May","Jun",
                              "Jul","Aug","Sep","Oct","Nov","Dec"};
    cfg.freq_steps         = {2, 3, 4, 6};
    cfg.checkboxes_per_row = 6;
    return new CronFieldWidget(cfg, parent);
}

CronFieldWidget* makeWeekdayField(QWidget* parent) {
    FieldConfig cfg;
    cfg.min_val            = 0;
    cfg.max_val            = 6;
    cfg.labels             = {"Sun","Mon","Tue","Wed","Thu","Fri","Sat"};
    cfg.freq_steps         = {2, 3};
    cfg.checkboxes_per_row = 7;
    return new CronFieldWidget(cfg, parent);
}

}  // anonymous namespace

// ── CronEditorDialog ──────────────────────────────────────────────────────────

CronEditorDialog::CronEditorDialog(QWidget* parent)
    : QDialog(parent) {

    setWindowTitle(tr("Cron Expression Builder"));
    setMinimumSize(640, 400);

    auto* mainLayout = new QVBoxLayout(this);
    mainLayout->setSpacing(8);

    // Description label in a styled frame
    descriptionLabel_ = new QLabel(this);
    descriptionLabel_->setWordWrap(true);
    descriptionLabel_->setSizePolicy(QSizePolicy::Expanding, QSizePolicy::Preferred);
    auto* descFrame  = new QFrame(this);
    descFrame->setFrameShape(QFrame::StyledPanel);
    auto* descLayout = new QHBoxLayout(descFrame);
    descLayout->addWidget(descriptionLabel_);
    mainLayout->addWidget(descFrame);

    // Tabs
    tabWidget_    = new QTabWidget(this);
    minuteField_  = makeMinuteField(tabWidget_);
    hourField_    = makeHourField(tabWidget_);
    dayField_     = makeDayField(tabWidget_);
    monthField_   = makeMonthField(tabWidget_);
    weekdayField_ = makeWeekdayField(tabWidget_);

    tabWidget_->addTab(minuteField_,  tr("Minutes"));
    tabWidget_->addTab(hourField_,    tr("Hours"));
    tabWidget_->addTab(dayField_,     tr("Days of Month"));
    tabWidget_->addTab(monthField_,   tr("Month"));
    tabWidget_->addTab(weekdayField_, tr("Days of Week"));
    mainLayout->addWidget(tabWidget_);

    // OK / Cancel
    auto* buttons = new QDialogButtonBox(
        QDialogButtonBox::Ok | QDialogButtonBox::Cancel, this);
    connect(buttons, &QDialogButtonBox::accepted, this, &QDialog::accept);
    connect(buttons, &QDialogButtonBox::rejected, this, &QDialog::reject);
    mainLayout->addWidget(buttons);

    // Update description whenever any field changes
    connect(minuteField_,  &CronFieldWidget::changed, this, &CronEditorDialog::onFieldChanged);
    connect(hourField_,    &CronFieldWidget::changed, this, &CronEditorDialog::onFieldChanged);
    connect(dayField_,     &CronFieldWidget::changed, this, &CronEditorDialog::onFieldChanged);
    connect(monthField_,   &CronFieldWidget::changed, this, &CronEditorDialog::onFieldChanged);
    connect(weekdayField_, &CronFieldWidget::changed, this, &CronEditorDialog::onFieldChanged);
}

QString CronEditorDialog::cronExpression() const {
    return QString("%1 %2 %3 %4 %5")
        .arg(minuteField_->value())
        .arg(hourField_->value())
        .arg(dayField_->value())
        .arg(monthField_->value())
        .arg(weekdayField_->value());
}

void CronEditorDialog::setCronExpression(const QString& expr) {
    const QStringList parts = expr.trimmed().split(' ', Qt::SkipEmptyParts);
    if (parts.size() != 5)
        return;
    minuteField_->setValue(parts[0]);
    hourField_->setValue(parts[1]);
    dayField_->setValue(parts[2]);
    monthField_->setValue(parts[3]);
    weekdayField_->setValue(parts[4]);
    onFieldChanged();
}

void CronEditorDialog::onFieldChanged() {
    descriptionLabel_->setText(describeExpression());
}

QString CronEditorDialog::describeExpression() const {
    const QString expr = cronExpression();
    const QStringList p = expr.split(' ', Qt::SkipEmptyParts);
    if (p.size() != 5)
        return expr;

    const QString& min     = p[0];
    const QString& hour    = p[1];
    const QString& day     = p[2];
    const QString& month   = p[3];
    const QString& weekday = p[4];

    if (min == "*" && hour == "*" && day == "*" && month == "*" && weekday == "*")
        return tr("Every minute");

    if (min == "0" && hour == "*" && day == "*" && month == "*" && weekday == "*")
        return tr("Every hour");

    auto formatHour = [](const QString& h) -> QString {
        bool ok = false;
        const int n = h.toInt(&ok);
        if (!ok || n < 0 || n > 23) return h + ":00";
        const QString suffix  = (n < 12) ? "AM" : "PM";
        const int     display = (n == 0) ? 12 : (n <= 12 ? n : n - 12);
        return QString("%1:00 %2").arg(display).arg(suffix);
    };

    static const QStringList dayNames = {
        "", "Monday", "Tuesday", "Wednesday",
        "Thursday", "Friday", "Saturday", "Sunday"
    };
    static const QStringList ordinals = {
        "", "1st", "2nd", "3rd", "4th", "5th", "6th", "7th", "8th", "9th", "10th",
        "11th","12th","13th","14th","15th","16th","17th","18th","19th","20th",
        "21st","22nd","23rd","24th","25th","26th","27th","28th","29th","30th","31st"
    };

    if (min == "0" && day == "*" && month == "*") {
        if (weekday == "*")
            return tr("At %1, every day").arg(formatHour(hour));
        if (weekday == "1-5")
            return tr("At %1, Monday through Friday").arg(formatHour(hour));
        bool ok = false;
        const int wd = weekday.toInt(&ok);
        if (ok && wd >= 0 && wd <= 7) {
            const int idx = (wd == 0 || wd == 7) ? 7 : wd;
            if (idx < (int)dayNames.size())
                return tr("At %1, every %2").arg(formatHour(hour)).arg(dayNames[idx]);
        }
    }

    if (min == "0" && weekday == "*" && month == "*") {
        bool dayOk = false;
        const int d = day.toInt(&dayOk);
        if (dayOk && d >= 1 && d <= 31)
            return tr("At %1, on the %2 of every month")
                .arg(formatHour(hour)).arg(ordinals[d]);
    }

    return expr;
}

}  // namespace ores::qt
