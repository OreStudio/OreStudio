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
#include "ores.qt/ProvenanceWidget.hpp"

#include "ui_ProvenanceWidget.h"
#include "ores.qt/RelativeTimeHelper.hpp"
#include "ores.qt/WidgetUtils.hpp"

namespace ores::qt {

ProvenanceWidget::ProvenanceWidget(QWidget* parent)
    : QWidget(parent),
      ui_(new Ui::ProvenanceWidget) {
    ui_->setupUi(this);
    WidgetUtils::setupComboBoxes(this);
}

ProvenanceWidget::~ProvenanceWidget() {
    delete ui_;
}

void ProvenanceWidget::populate(int version,
                                const std::string& modified_by,
                                const std::string& performed_by,
                                std::chrono::system_clock::time_point recorded_at,
                                const std::string& change_reason_code,
                                const std::string& change_commentary) {
    ui_->versionEdit->setText(QString::number(version));
    ui_->modifiedByEdit->setText(QString::fromStdString(modified_by));
    ui_->performedByEdit->setText(QString::fromStdString(performed_by));

    // Only format recorded_at if it is non-zero (not all entities track this).
    if (recorded_at.time_since_epoch().count() != 0) {
        ui_->recordedAtEdit->setText(relative_time_helper::format(recorded_at));
    } else {
        ui_->recordedAtEdit->clear();
    }

    ui_->changeReasonEdit->setText(QString::fromStdString(change_reason_code));
    ui_->commentaryEdit->setPlainText(QString::fromStdString(change_commentary));
}

void ProvenanceWidget::clear() {
    ui_->versionEdit->clear();
    ui_->modifiedByEdit->clear();
    ui_->performedByEdit->clear();
    ui_->recordedAtEdit->clear();
    ui_->changeReasonEdit->clear();
    ui_->commentaryEdit->clear();
}

}
