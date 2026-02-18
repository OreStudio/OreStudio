/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2025 Marco Craveiro <marco.craveiro@gmail.com>
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
#ifndef ORES_QT_CATALOG_MDI_WINDOW_HPP
#define ORES_QT_CATALOG_MDI_WINDOW_HPP

#include <QTableView>
#include <QToolBar>
#include "ores.qt/EntityListMdiWindow.hpp"
#include "ores.qt/ClientCatalogModel.hpp"
#include "ores.qt/ClientManager.hpp"
#include "ores.dq/domain/catalog.hpp"
#include "ores.logging/make_logger.hpp"

namespace ores::qt {

/**
 * @brief MDI window for displaying and managing catalogs.
 *
 * Provides a table view with toolbar actions for Add, Edit, Delete,
 * History, and Refresh operations.
 */
class CatalogMdiWindow : public EntityListMdiWindow {
    Q_OBJECT

private:
    inline static std::string_view logger_name = "ores.qt.catalog_mdi_window";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    explicit CatalogMdiWindow(ClientManager* clientManager,
                              const QString& username,
                              QWidget* parent = nullptr);

public slots:
    void reload() override;

signals:
    void statusChanged(const QString& message);
    void errorOccurred(const QString& message);
    void showCatalogDetails(const dq::domain::catalog& catalog);
    void addNewRequested();
    void showCatalogHistory(const QString& name);

private slots:
    void onAddClicked();
    void onEditClicked();
    void onDeleteClicked();
    void onHistoryClicked();
    void onRefreshClicked();
    void onDoubleClicked(const QModelIndex& index);

protected:
    QString normalRefreshTooltip() const override {
        return tr("Refresh catalogs");
    }

private:
    void setupUi();
    void setupToolbar();
    void setupConnections();
    void updateActionStates();

    ClientManager* clientManager_;
    QString username_;
    QTableView* tableView_;
    ClientCatalogModel* model_;
    QToolBar* toolbar_;
    QAction* addAction_;
    QAction* editAction_;
    QAction* deleteAction_;
    QAction* historyAction_;
    QAction* refreshAction_;
};

}

#endif
