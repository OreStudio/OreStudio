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
#ifndef ORES_QT_CHILD_ENTITY_TABLE_WIDGET_HPP
#define ORES_QT_CHILD_ENTITY_TABLE_WIDGET_HPP

#include "ores.qt/export.hpp"
#include <QStringList>
#include <QWidget>
#include <vector>

class QTableWidget;
class QToolBar;
class QVBoxLayout;

namespace ores::qt {

/**
 * @brief Generic, entity-agnostic widget embedding a composite child
 * entity (e.g. party identifiers, contact information) as an editable
 * table with Add/Delete toolbar actions.
 *
 * This widget has no knowledge of any domain entity or service/messaging
 * layer: callers populate rows via setRows(), and connect to
 * addRequested()/deleteRequested() to perform the actual load/save/delete
 * against whatever child entity they own.
 */
class ORES_QT_API ChildEntityTableWidget final : public QWidget {
    Q_OBJECT

public:
    /**
     * @param columnHeaders Column headers, left to right.
     * @param addLabel Toolbar tooltip for the add action (e.g. "Add Identifier").
     * @param deleteLabel Toolbar tooltip for the delete action.
     */
    ChildEntityTableWidget(const QStringList& columnHeaders,
                           const QString& addLabel,
                           const QString& deleteLabel,
                           QWidget* parent = nullptr);
    ~ChildEntityTableWidget() override;

    /** @brief Replace all rows; each inner list is one row's cell values, in column order. */
    void setRows(const std::vector<QStringList>& rows);

    /** @brief Index of the currently selected row, or -1 if none selected. */
    int currentRow() const;

    /** @brief The underlying table, for callers needing finer control (double-click, etc). */
    QTableWidget* table() const;

signals:
    void addRequested();
    void deleteRequested(int row);

private:
    QVBoxLayout* layout_;
    QToolBar* toolbar_;
    QTableWidget* table_;
};

}

#endif
