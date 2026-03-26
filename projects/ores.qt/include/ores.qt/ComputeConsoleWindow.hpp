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
#include "ores.qt/OreLogViewerWidget.hpp"
#include "ores.compute.api/domain/host.hpp"
#include "ores.logging/make_logger.hpp"

namespace ores::qt {

/**
 * @brief Unified compute console: task list, live transfer progress, and
 *        per-result ORE engine log viewer.
 *
 * The top pane shows all compute tasks (joined result/workunit/batch rows)
 * via ComputeTaskViewModel.  Selecting a task populates the bottom tab:
 *
 *   - "ORE Logs"   — OreLogViewerWidget filtered to the selected result UUID.
 *   - "Transfers"  — ComputeTransferModel showing in-flight upload/download
 *                    progress for any active transfers.
 *
 * A HostDisplayNameCache is owned here and populated from a list-hosts fetch
 * on each refresh, providing whimsical names in the Host column.
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
     * @brief Exposes the transfer model so external code (upload/download
     *        helpers) can call add_transfer / update_progress / etc.
     */
    ComputeTransferModel* transfer_model() const { return transfer_model_.get(); }

    QSize sizeHint() const override { return {1200, 700}; }

public slots:
    void refresh();

signals:
    void statusChanged(const QString& message);
    void errorOccurred(const QString& error);

private slots:
    void on_host_cache_loaded();
    void on_tasks_loaded();
    void on_tasks_error(const QString& message, const QString& details);
    void on_task_selection_changed();
    void on_auto_refresh_toggled(bool checked);

private:
    void setup_ui();
    void setup_toolbar();
    void setup_task_table();
    void setup_bottom_tabs();
    void fetch_host_cache();

    ClientManager* client_manager_;

    // Data
    HostDisplayNameCache* host_cache_{nullptr};          // owned by this
    std::unique_ptr<ComputeTaskViewModel> task_model_;
    std::unique_ptr<ComputeTransferModel> transfer_model_;

    // Host cache fetch
    using HostList = std::vector<compute::domain::host>;
    QFutureWatcher<HostList>* host_watcher_{nullptr};

    // UI
    QToolBar* toolbar_{nullptr};
    QAction* refresh_action_{nullptr};
    QAction* auto_refresh_action_{nullptr};

    QSplitter* splitter_{nullptr};

    QTableView* task_view_{nullptr};
    QSortFilterProxyModel* task_proxy_{nullptr};

    QTabWidget* bottom_tabs_{nullptr};
    OreLogViewerWidget* log_viewer_{nullptr};
    QTableView* transfer_view_{nullptr};

    QTimer* auto_refresh_timer_{nullptr};
};

} // namespace ores::qt

#endif
