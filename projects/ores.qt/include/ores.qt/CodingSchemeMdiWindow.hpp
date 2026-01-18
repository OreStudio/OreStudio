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
#ifndef ORES_QT_CODING_SCHEME_MDI_WINDOW_HPP
#define ORES_QT_CODING_SCHEME_MDI_WINDOW_HPP

#include <QWidget>
#include <QTableView>
#include <QToolBar>
#include <QSortFilterProxyModel>
#include "ores.qt/ClientManager.hpp"
#include "ores.qt/ClientCodingSchemeModel.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.dq/domain/coding_scheme.hpp"

namespace ores::qt {

class CodingSchemeMdiWindow final : public QWidget {
    Q_OBJECT

private:
    inline static std::string_view logger_name =
        "ores.qt.coding_scheme_mdi_window";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    explicit CodingSchemeMdiWindow(ClientManager* clientManager,
                                   const QString& username,
                                   QWidget* parent = nullptr);
    ~CodingSchemeMdiWindow() override = default;

    QSize sizeHint() const override { return QSize(1000, 600); }
    void reload();

signals:
    void statusChanged(const QString& message);
    void errorOccurred(const QString& error_message);
    void showSchemeDetails(const dq::domain::coding_scheme& scheme);
    void showSchemeHistory(const QString& code);
    void addNewRequested();

private slots:
    void onDataLoaded();
    void onLoadError(const QString& error_message, const QString& details = {});
    void onRowDoubleClicked(const QModelIndex& index);
    void onAddClicked();
    void onEditClicked();
    void onDeleteClicked();
    void onRefreshClicked();
    void onSelectionChanged();
    void onHistoryClicked();

private:
    void setupUi();
    void setupToolbar();
    void setupConnections();
    void updateActionStates();
    void saveColumnVisibility();
    void loadColumnVisibility();

    ClientManager* clientManager_;
    QString username_;
    ClientCodingSchemeModel* model_;
    QSortFilterProxyModel* proxyModel_;
    QTableView* tableView_;
    QToolBar* toolbar_;

    QAction* addAction_;
    QAction* editAction_;
    QAction* deleteAction_;
    QAction* refreshAction_;
    QAction* historyAction_;
};

}

#endif
