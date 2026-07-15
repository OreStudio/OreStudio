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
#include "ores.qt/CrmRateCellWidget.hpp"
#include "ores.qt/ColorConstants.hpp"
#include <QLabel>
#include <QMouseEvent>
#include <QVBoxLayout>
#include <algorithm>

namespace ores::qt {

namespace {

// The application-wide QSS stylesheet overrides QPalette-based colours,
// so per-label tinting has to go through setStyleSheet (matching the
// established pattern in FxSpotGridWindow) rather than QPalette.
void set_label_color(QLabel* label, const QColor& color) {
    label->setStyleSheet(QStringLiteral("color: %1;").arg(color.name()));
}

} // namespace

CrmRateCellWidget::CrmRateCellWidget(QWidget* parent)
    : QWidget(parent) {
    auto* layout = new QVBoxLayout(this);
    layout->setContentsMargins(2, 2, 2, 2);
    layout->setSpacing(0);

    pairLabel_ = new QLabel(this);
    auto pairFont = pairLabel_->font();
    pairFont.setPointSize(std::max(7, pairFont.pointSize() - 2));
    pairLabel_->setFont(pairFont);
    pairLabel_->setAlignment(Qt::AlignCenter);
    set_label_color(pairLabel_, color_constants::level_trace);
    layout->addWidget(pairLabel_);

    rateLabel_ = new QLabel(this);
    auto rateFont = rateLabel_->font();
    rateFont.setBold(true);
    rateLabel_->setFont(rateFont);
    rateLabel_->setAlignment(Qt::AlignCenter);
    layout->addWidget(rateLabel_);

    changeLabel_ = new QLabel(this);
    auto changeFont = changeLabel_->font();
    changeFont.setPointSize(std::max(7, changeFont.pointSize() - 2));
    changeLabel_->setFont(changeFont);
    changeLabel_->setAlignment(Qt::AlignCenter);
    layout->addWidget(changeLabel_);
}

void CrmRateCellWidget::setDash() {
    pairLabel_->clear();
    rateLabel_->setText(QStringLiteral("-"));
    set_label_color(rateLabel_, color_constants::level_trace);
    changeLabel_->clear();
}

void CrmRateCellWidget::setData(const QString& pairCode,
                                const QString& rateText,
                                const QString& changeText,
                                const QColor& changeColor,
                                const QColor& rateColor,
                                const QColor& pairColor,
                                const QString& pairPrefix) {
    pairLabel_->setText(pairPrefix.isEmpty() ? pairCode :
                                               pairPrefix + QStringLiteral(" ") + pairCode);
    set_label_color(pairLabel_, pairColor);
    rateLabel_->setText(rateText);
    set_label_color(rateLabel_, rateColor);
    changeLabel_->setText(changeText);
    set_label_color(changeLabel_, changeColor);
}

void CrmRateCellWidget::setPastelBackground(bool on) {
    setAutoFillBackground(on);
    // A light, low-saturation tint that reads clearly in both light and
    // dark themes without fighting the app's own accent colours -- a
    // pale lavender, distinct from the stale/unavailable amber/grey text
    // tinting used elsewhere in this cell.
    setStyleSheet(on ? QStringLiteral("background-color: rgba(180, 160, 220, 60);") : QString());
}

void CrmRateCellWidget::mousePressEvent(QMouseEvent*) {
    emit clicked();
}

}
