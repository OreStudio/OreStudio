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
#ifndef ORES_QT_ORIGIN_DIMENSION_MDI_WINDOW_HPP
#define ORES_QT_ORIGIN_DIMENSION_MDI_WINDOW_HPP

#include <QWidget>
#include <QTimer>
#include <QAction>
#include <QToolBar>
#include <QTableView>
#include <QSortFilterProxyModel>
#include "ores.qt/ClientManager.hpp"
#include "ores.qt/ClientOriginDimensionModel.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.dq/domain/origin_dimension.hpp"

namespace ores::qt {

/**
 * @brief MDI window for displaying and managing origin dimensions.
 *
 * Provides a table view of origin dimensions with toolbar actions
 * for reload and viewing details.
 */
class OriginDimensionMdiWindow final : public QWidget {
    Q_OBJECT

private:
    inline static std::string_view logger_name =
        "ores.qt.origin_dimension_mdi_window";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    explicit OriginDimensionMdiWindow(
        ClientManager* clientManager,
        const QString& username,
        QWidget* parent = nullptr);
    ~OriginDimensionMdiWindow() override = default;

    QSize sizeHint() const override;

public slots:
    void reload();
    void markAsStale();
    void clearStaleIndicator();

signals:
    void statusChanged(const QString& message);
    void errorOccurred(const QString& error_message);
    void showDimensionDetails(const dq::domain::origin_dimension& dimension);
    void addNewRequested();
    void dimensionDeleted(const QString& code);
    void showDimensionHistory(const QString& code);

public slots:
    void addNew();
    void editSelected();
    void deleteSelected();
    void viewHistorySelected();

private slots:
    void onDataLoaded();
    void onLoadError(const QString& error_message, const QString& details = {});
    void onSelectionChanged();
    void onDoubleClicked(const QModelIndex& index);

private slots:
    void showHeaderContextMenu(const QPoint& pos);

private:
    void setupUi();
    void setupToolbar();
    void setupTable();
    void setupColumnVisibility();
    void setupConnections();
    void startPulseAnimation();
    void updateActionStates();
    void saveSettings();
    void restoreSettings();

    ClientManager* clientManager_;
    QString username_;

    QToolBar* toolbar_;
    QTableView* tableView_;
    ClientOriginDimensionModel* model_;
    QSortFilterProxyModel* proxyModel_;

    // Toolbar actions
    QAction* reloadAction_;
    QAction* addAction_;
    QAction* editAction_;
    QAction* deleteAction_;
    QAction* historyAction_;

    // Stale indicator
    QIcon normalReloadIcon_;
    QIcon staleReloadIcon_;
    QTimer* pulseTimer_;
    bool pulseState_{false};
    int pulseCount_{0};
    static constexpr int pulse_interval_ms_ = 500;
    static constexpr int max_pulse_cycles_ = 6;
};

}

#endif
