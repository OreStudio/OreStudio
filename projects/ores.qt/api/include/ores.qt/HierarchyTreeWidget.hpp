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
#ifndef ORES_QT_HIERARCHY_TREE_WIDGET_HPP
#define ORES_QT_HIERARCHY_TREE_WIDGET_HPP

#include "ores.qt/export.hpp"
#include <QWidget>

class QAbstractItemModel;
class QTreeView;
class QVBoxLayout;

namespace ores::qt {

/**
 * @brief Generic, entity-agnostic widget that displays a self-referencing
 * parent-child hierarchy as a tree.
 *
 * This widget has no knowledge of party, counterparty, or any other domain
 * entity, and makes no service/messaging calls itself: it merely displays
 * whatever QAbstractItemModel it is given (typically one built by
 * HierarchyModelBuilder) as a tree. Callers own the model's lifetime unless
 * they explicitly reparent it onto this widget.
 */
class ORES_QT_API HierarchyTreeWidget final : public QWidget {
    Q_OBJECT

public:
    explicit HierarchyTreeWidget(QWidget* parent = nullptr);
    ~HierarchyTreeWidget() override;

    /**
     * @brief Set the model to display. Ownership of the model is not taken
     * by this widget; callers remain responsible for its lifetime.
     */
    void setModel(QAbstractItemModel* model);

    /** @brief Access to the underlying model, or nullptr if none was set. */
    QAbstractItemModel* model() const;

    /** @brief Expand every node in the tree. */
    void expandAll();

    /** @brief Collapse every node in the tree. */
    void collapseAll();

    /** @brief The underlying QTreeView, for callers that need finer control
     * (selection mode, header configuration, etc.) than this class exposes
     * directly. */
    QTreeView* treeView() const;

signals:
    /** @brief Emitted when the current selection changes; carries the
     * QAbstractItemModel index of the newly selected item, or an invalid
     * index if the selection was cleared. */
    void selectionChanged(const QModelIndex& index);

private:
    QVBoxLayout* layout_;
    QTreeView* treeView_;
};

}

#endif
