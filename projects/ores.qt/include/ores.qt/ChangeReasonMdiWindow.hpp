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
#ifndef ORES_QT_CHANGE_REASON_MDI_WINDOW_HPP
#define ORES_QT_CHANGE_REASON_MDI_WINDOW_HPP

#include <QWidget>
#include <QTimer>
#include <QAction>
#include <QToolBar>
#include <QTableView>
#include <QSortFilterProxyModel>
#include "ores.qt/ClientManager.hpp"
#include "ores.qt/ClientChangeReasonModel.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.iam/domain/change_reason.hpp"

namespace ores::qt {

/**
 * @brief MDI window for displaying and managing change reasons.
 *
 * Provides a table view of change reasons with toolbar actions
 * for reload and viewing details.
 */
class ChangeReasonMdiWindow final : public QWidget {
    Q_OBJECT

private:
    inline static std::string_view logger_name =
        "ores.qt.change_reason_mdi_window";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    explicit ChangeReasonMdiWindow(
        ClientManager* clientManager,
        const QString& username,
        QWidget* parent = nullptr);
    ~ChangeReasonMdiWindow() override = default;

    QSize sizeHint() const override;

public slots:
    void reload();
    void markAsStale();
    void clearStaleIndicator();

signals:
    void statusChanged(const QString& message);
    void errorOccurred(const QString& error_message);
    void showReasonDetails(const iam::domain::change_reason& reason);

private slots:
    void onDataLoaded();
    void onLoadError(const QString& error_message);
    void onSelectionChanged();
    void onDoubleClicked(const QModelIndex& index);

private:
    void setupUi();
    void setupToolbar();
    void setupTable();
    void setupConnections();
    void startPulseAnimation();

    ClientManager* clientManager_;
    QString username_;

    QToolBar* toolbar_;
    QTableView* tableView_;
    ClientChangeReasonModel* model_;
    QSortFilterProxyModel* proxyModel_;

    // Toolbar actions
    QAction* reloadAction_;
    QAction* viewDetailsAction_;

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
