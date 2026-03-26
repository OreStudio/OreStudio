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
#ifndef ORES_QT_COMPUTE_CONSOLE_WINDOW_HPP
#define ORES_QT_COMPUTE_CONSOLE_WINDOW_HPP

#include <memory>
#include <QTimer>
#include <QWidget>
#include <QAction>
#include <QSplitter>
#include <QTabWidget>
#include <QTableView>
#include <QToolBar>
#include <QSortFilterProxyModel>
#include <QFutureWatcher>
#include "ores.qt/ClientManager.hpp"
#include "ores.qt/HostDisplayNameCache.hpp"
#include "ores.qt/ComputeTaskViewModel.hpp"
#include "ores.qt/ComputeTransferModel.hpp"
#include "ores.qt/ClientHostModel.hpp"
#include "ores.qt/OreLogViewerWidget.hpp"
#include "ores.compute.api/domain/host.hpp"
#include "ores.logging/make_logger.hpp"

namespace ores::qt {

/**
 * @brief Unified compute console modelled on the BOINC manager UI.
 *
 * A top-level tab bar selects the active view:
 *
 *   Tasks      — joined result/workunit/batch table (top) with a per-task
 *                ORE log sub-pane below (driven by row selection).
 *   Hosts      — flat host table; also feeds the HostDisplayNameCache.
 *   Transfers  — live file transfer progress with a custom progress delegate.
 *
 * A toolbar with Refresh and Auto-refresh actions spans all tabs.
 */
class ComputeConsoleWindow : public QWidget {
    Q_OBJECT

private:
    inline static std::string_view logger_name =
        "ores.qt.compute_console_window";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    explicit ComputeConsoleWindow(ClientManager* clientManager,
                                  QWidget* parent = nullptr);

    /**
     * @brief Exposes the transfer model so external upload/download helpers
     *        can call add_transfer / update_progress / etc.
     */
    ComputeTransferModel* transfer_model() const { return transfer_model_.get(); }

    QSize sizeHint() const override { return {1200, 720}; }

public slots:
    void refresh();

signals:
    void statusChanged(const QString& message);
    void errorOccurred(const QString& error);

private slots:
    void on_hosts_loaded();
    void on_tasks_loaded();
    void on_tasks_error(const QString& message, const QString& details);
    void on_task_selection_changed();
    void on_tab_changed(int index);
    void on_auto_refresh_toggled(bool checked);

private:
    void setup_ui();
    void setup_toolbar();
    QWidget* make_tasks_tab();
    QWidget* make_hosts_tab();
    QWidget* make_transfers_tab();

    ClientManager* client_manager_;

    // Models
    HostDisplayNameCache* host_cache_{nullptr};        // owned by this
    std::unique_ptr<ComputeTaskViewModel> task_model_;
    std::unique_ptr<ClientHostModel>      host_model_;
    std::unique_ptr<ComputeTransferModel> transfer_model_;

    // Host fetch watcher (also populates host_cache_)
    using HostList = std::vector<compute::domain::host>;
    QFutureWatcher<HostList>* host_watcher_{nullptr};

    // Toolbar
    QToolBar* toolbar_{nullptr};
    QAction*  refresh_action_{nullptr};
    QAction*  auto_refresh_action_{nullptr};

    // Top-level tab bar
    QTabWidget* main_tabs_{nullptr};

    // Tasks tab widgets
    QTableView*           task_view_{nullptr};
    QSortFilterProxyModel* task_proxy_{nullptr};
    OreLogViewerWidget*   log_viewer_{nullptr};

    // Hosts tab
    QTableView*           host_view_{nullptr};
    QSortFilterProxyModel* host_proxy_{nullptr};

    // Transfers tab
    QTableView* transfer_view_{nullptr};

    QTimer* auto_refresh_timer_{nullptr};
};

} // namespace ores::qt

#endif
