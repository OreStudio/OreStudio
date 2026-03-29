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
#include "ores.qt/CompositeLegsWidget.hpp"

#include <QHeaderView>
#include <QTableWidgetItem>
#include "ui_CompositeLegsWidget.h"

namespace ores::qt {

CompositeLegsWidget::CompositeLegsWidget(QWidget* parent)
    : QWidget(parent),
      ui_(new Ui::CompositeLegsWidget) {
    ui_->setupUi(this);
    setupUi();
    setupConnections();
}

CompositeLegsWidget::~CompositeLegsWidget() {
    delete ui_;
}

void CompositeLegsWidget::setupUi() {
    // Seq column is narrow; Trade ID stretches to fill.
    ui_->legsTable->horizontalHeader()->setSectionResizeMode(
        0, QHeaderView::ResizeToContents);
    ui_->legsTable->horizontalHeader()->setSectionResizeMode(
        1, QHeaderView::Stretch);
    ui_->legsTable->setEditTriggers(
        QAbstractItemView::DoubleClicked |
        QAbstractItemView::EditKeyPressed);
}

void CompositeLegsWidget::setupConnections() {
    connect(ui_->addLegButton, &QPushButton::clicked,
            this, &CompositeLegsWidget::onAddLegClicked);
    connect(ui_->removeLegButton, &QPushButton::clicked,
            this, &CompositeLegsWidget::onRemoveLegClicked);
    connect(ui_->legsTable, &QTableWidget::cellChanged,
            this, [this](int, int) { emit legsChanged(); });
}

void CompositeLegsWidget::setLegs(
    const std::vector<trading::domain::composite_leg>& legs) {
    // Block signals while repopulating to avoid spurious legsChanged emissions.
    ui_->legsTable->blockSignals(true);
    ui_->legsTable->setRowCount(0);
    for (const auto& leg : legs) {
        const int row = ui_->legsTable->rowCount();
        ui_->legsTable->insertRow(row);
        ui_->legsTable->setItem(
            row, 0,
            new QTableWidgetItem(QString::number(leg.leg_sequence)));
        ui_->legsTable->setItem(
            row, 1,
            new QTableWidgetItem(
                QString::fromStdString(leg.constituent_trade_id)));
    }
    ui_->legsTable->blockSignals(false);
}

std::vector<trading::domain::composite_leg>
CompositeLegsWidget::legs() const {
    std::vector<trading::domain::composite_leg> result;
    const int rows = ui_->legsTable->rowCount();
    result.reserve(static_cast<std::size_t>(rows));
    for (int row = 0; row < rows; ++row) {
        const auto* seqItem = ui_->legsTable->item(row, 0);
        const auto* tradeIdItem = ui_->legsTable->item(row, 1);
        const QString tradeId =
            tradeIdItem ? tradeIdItem->text().trimmed() : QString{};
        if (tradeId.isEmpty()) continue;
        trading::domain::composite_leg leg;
        leg.leg_sequence = seqItem ? seqItem->text().toInt() : (row + 1);
        leg.constituent_trade_id = tradeId.toStdString();
        result.push_back(std::move(leg));
    }
    return result;
}

void CompositeLegsWidget::setReadOnly(bool readOnly) {
    readOnly_ = readOnly;
    ui_->legsTable->setEditTriggers(
        readOnly ? QAbstractItemView::NoEditTriggers
                 : (QAbstractItemView::DoubleClicked |
                    QAbstractItemView::EditKeyPressed));
    ui_->addLegButton->setEnabled(!readOnly);
    ui_->removeLegButton->setEnabled(!readOnly);
}

void CompositeLegsWidget::onAddLegClicked() {
    const int row = ui_->legsTable->rowCount();
    ui_->legsTable->insertRow(row);
    ui_->legsTable->setItem(row, 0,
        new QTableWidgetItem(QString::number(row + 1)));
    ui_->legsTable->setItem(row, 1, new QTableWidgetItem(QString{}));
    ui_->legsTable->editItem(ui_->legsTable->item(row, 1));
    emit legsChanged();
}

void CompositeLegsWidget::onRemoveLegClicked() {
    const int row = ui_->legsTable->currentRow();
    if (row < 0) return;
    ui_->legsTable->removeRow(row);
    emit legsChanged();
}

}
