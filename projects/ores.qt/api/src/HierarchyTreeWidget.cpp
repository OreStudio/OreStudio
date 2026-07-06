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
#include "ores.qt/HierarchyTreeWidget.hpp"
#include <QHeaderView>
#include <QItemSelectionModel>
#include <QTreeView>
#include <QVBoxLayout>

namespace ores::qt {

HierarchyTreeWidget::HierarchyTreeWidget(QWidget* parent)
    : QWidget(parent), layout_(new QVBoxLayout(this)), treeView_(new QTreeView(this)) {
    treeView_->setHeaderHidden(false);
    treeView_->setSelectionMode(QAbstractItemView::SingleSelection);
    treeView_->setSelectionBehavior(QAbstractItemView::SelectRows);
    treeView_->setUniformRowHeights(true);

    layout_->setContentsMargins(0, 0, 0, 0);
    layout_->addWidget(treeView_);
    setLayout(layout_);
}

HierarchyTreeWidget::~HierarchyTreeWidget() = default;

void HierarchyTreeWidget::setModel(QAbstractItemModel* model) {
    treeView_->setModel(model);

    if (model != nullptr) {
        connect(treeView_->selectionModel(), &QItemSelectionModel::currentChanged, this,
                [this](const QModelIndex& current, const QModelIndex&) {
                    emit selectionChanged(current);
                });
    }
}

QAbstractItemModel* HierarchyTreeWidget::model() const {
    return treeView_->model();
}

void HierarchyTreeWidget::expandAll() {
    treeView_->expandAll();
}

void HierarchyTreeWidget::collapseAll() {
    treeView_->collapseAll();
}

QTreeView* HierarchyTreeWidget::treeView() const {
    return treeView_;
}

}
