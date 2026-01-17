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
#ifndef ORES_QT_TELEMETRY_MDI_WINDOW_HPP
#define ORES_QT_TELEMETRY_MDI_WINDOW_HPP

#include <QWidget>
#include <QTreeWidget>
#include <QTableView>
#include <QTableWidget>
#include <QSplitter>
#include <QVBoxLayout>
#include <QHBoxLayout>
#include <QToolBar>
#include <QLabel>
#include <QTextEdit>
#include <QGroupBox>
#include <QTimer>
#include <QPushButton>
#include <QSortFilterProxyModel>
#include <QCloseEvent>
#include <memory>
#include <unordered_map>
#include "ores.qt/ClientManager.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.qt/ClientTelemetryLogModel.hpp"
#include "ores.qt/PaginationWidget.hpp"
#include "ores.iam/domain/session.hpp"

namespace ores::qt {

/**
 * @brief MDI window for viewing telemetry logs.
 *
 * Provides a tree view of sessions on the left and a log table on the right,
 * with filtering by log level and search capabilities.
 */
class TelemetryMdiWindow : public QWidget {
    Q_OBJECT

private:
    inline static std::string_view logger_name =
        "ores.qt.telemetry_mdi_window";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    explicit TelemetryMdiWindow(ClientManager* clientManager,
                                 const QString& username,
                                 QWidget* parent = nullptr);
    ~TelemetryMdiWindow() override;

    ClientTelemetryLogModel* logModel() const { return logModel_.get(); }

    QSize sizeHint() const override;

signals:
    void statusChanged(const QString& message);
    void errorOccurred(const QString& error_message);

public slots:
    void reload();
    void reloadSessions();
    void markAsStale();
    void clearStaleIndicator();

private slots:
    void onSessionsLoaded();
    void onSessionSelected(QTreeWidgetItem* item, int column);
    void onLogSelected(const QModelIndex& current, const QModelIndex& previous);
    void onLogsLoaded();
    void onLoadError(const QString& error_message, const QString& details = {});
    void onConnectionStateChanged();

private:
    void setupUi();
    void setupToolbar();
    void setupLevelFilters();
    void setupSessionTree();
    void setupLogTable();
    void setupDetailPanels();
    void setupReloadAction();
    void startPulseAnimation();
    void stopPulseAnimation();
    void loadSessions();
    void populateSessionTree(const std::vector<iam::domain::session>& sessions);
    void updateSessionInfoPanel(const iam::domain::session& session);
    void updateLogDetailPanel(const telemetry::domain::telemetry_log_entry& entry);
    void clearDetailPanels();
    void applyLevelFilter();
    void updateFilterButtonStyle(QPushButton* btn, bool enabled, const QColor& color);

protected:
    void closeEvent(QCloseEvent* event) override;

private:
    // Main layout
    QSplitter* mainSplitter_;
    QWidget* leftPanel_;
    QWidget* rightPanel_;

    // Left panel: session tree + session details
    QSplitter* leftSplitter_;
    QTreeWidget* sessionTree_;
    QGroupBox* sessionDetailsGroup_;
    QTableWidget* sessionDetailsTable_;

    // Right panel: toolbar + table + details
    QVBoxLayout* rightLayout_;
    QToolBar* toolBar_;
    QTableView* logTableView_;
    PaginationWidget* paginationWidget_;

    // Log detail panel (message only)
    QGroupBox* logDetailGroup_;
    QTextEdit* logMessageEdit_;

    // Level filter buttons
    QPushButton* filterTraceBtn_;
    QPushButton* filterDebugBtn_;
    QPushButton* filterInfoBtn_;
    QPushButton* filterWarnBtn_;
    QPushButton* filterErrorBtn_;

    // Toolbar actions
    QAction* reloadAction_;
    QIcon normalReloadIcon_;
    QIcon staleReloadIcon_;
    QTimer* pulseTimer_;
    bool pulseState_{false};
    int pulseCount_{0};

    // Model and data
    std::unique_ptr<ClientTelemetryLogModel> logModel_;
    QSortFilterProxyModel* proxyModel_;
    ClientManager* clientManager_;
    QString username_;
    bool isStale_{false};

    // Session data cache
    std::unordered_map<std::string, iam::domain::session> sessionCache_;
    std::optional<boost::uuids::uuid> selectedSessionId_;

    // Level filter state (true = show this level)
    bool showTrace_{true};
    bool showDebug_{true};
    bool showInfo_{true};
    bool showWarn_{true};
    bool showError_{true};
};

}

#endif
