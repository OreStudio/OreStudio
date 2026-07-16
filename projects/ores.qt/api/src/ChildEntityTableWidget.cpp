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
#include "ores.qt/ChildEntityTableWidget.hpp"
#include "ores.qt/IconUtils.hpp"
#include <QHeaderView>
#include <QTableWidget>
#include <QToolBar>
#include <QVBoxLayout>

namespace ores::qt {

ChildEntityTableWidget::ChildEntityTableWidget(const QStringList& columnHeaders,
                                               const QString& addLabel,
                                               const QString& deleteLabel,
                                               QWidget* parent)
    : QWidget(parent)
    , layout_(new QVBoxLayout(this))
    , toolbar_(new QToolBar(this))
    , table_(new QTableWidget(this)) {

    toolbar_->setIconSize(QSize(16, 16));
    auto* addAction = toolbar_->addAction(
        IconUtils::createRecoloredIcon(Icon::Add, IconUtils::DefaultIconColor), addLabel);
    auto* deleteAction = toolbar_->addAction(
        IconUtils::createRecoloredIcon(Icon::Delete, IconUtils::DefaultIconColor), deleteLabel);
    connect(addAction, &QAction::triggered, this, &ChildEntityTableWidget::addRequested);
    connect(deleteAction, &QAction::triggered, this, [this]() {
        emit deleteRequested(table_->currentRow());
    });

    table_->setColumnCount(columnHeaders.size());
    table_->setHorizontalHeaderLabels(columnHeaders);
    table_->horizontalHeader()->setStretchLastSection(true);
    table_->setSelectionBehavior(QAbstractItemView::SelectRows);
    table_->setSelectionMode(QAbstractItemView::SingleSelection);
    table_->setEditTriggers(QAbstractItemView::NoEditTriggers);
    table_->verticalHeader()->setVisible(false);

    layout_->addWidget(toolbar_);
    layout_->addWidget(table_);
}

ChildEntityTableWidget::~ChildEntityTableWidget() = default;

void ChildEntityTableWidget::setRows(const std::vector<QStringList>& rows) {
    table_->setRowCount(static_cast<int>(rows.size()));
    for (int i = 0; i < static_cast<int>(rows.size()); ++i) {
        const auto& row = rows[static_cast<std::size_t>(i)];
        for (int col = 0; col < row.size(); ++col)
            table_->setItem(i, col, new QTableWidgetItem(row[col]));
    }
    table_->resizeColumnsToContents();
}

int ChildEntityTableWidget::currentRow() const {
    return table_->currentRow();
}

QTableWidget* ChildEntityTableWidget::table() const {
    return table_;
}

}
