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
#include <string>
#include <QTimer>
#include <QWidget>
#include <QAction>
#include <QSplitter>
#include <QTabWidget>
#include <QTableView>
#include <QToolBar>
#include <QSortFilterProxyModel>
#include <QFutureWatcher>
#include "ores.qt/ChangeReasonCache.hpp"
#include "ores.qt/ClientManager.hpp"
#include "ores.qt/HostDisplayNameCache.hpp"
#include "ores.qt/ComputeTaskViewModel.hpp"
#include "ores.qt/ComputeTransferModel.hpp"
#include "ores.qt/ClientAppModel.hpp"
#include "ores.qt/ClientAppVersionModel.hpp"
#include "ores.qt/ClientHostModel.hpp"
#include "ores.compute.api/domain/host.hpp"
#include "ores.logging/make_logger.hpp"

namespace ores::qt {

class BadgeCache;

/**
 * @brief Unified compute console modelled on the BOINC manager UI.
 *
 * A top-level tab bar selects the active view:
 *
 *   Tasks     — joined result/workunit/batch table with a per-tab toolbar
 *               (New Batch, New Work Unit, Logs). Double-click opens the
 *               relevant detail dialog.
 *   Apps      — master-detail split: top pane lists apps; bottom pane shows
 *               app versions for the selected app. Double-click on either
 *               pane opens the detail dialog.
 *   Hosts     — flat host table; also feeds the HostDisplayNameCache.
 *   Transfers — live file transfer progress with a custom progress delegate.
 *
 * Each tab has its own toolbar (no global toolbar above the tab widget).
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
                                  ChangeReasonCache* changeReasonCache,
                                  BadgeCache* badgeCache,
                                  QWidget* parent = nullptr);

    /**
     * @brief Exposes the transfer model so external upload/download helpers
     *        can call add_transfer / update_progress / etc.
     */
    ComputeTransferModel* transfer_model() const { return transfer_model_.get(); }

    void setHttpBaseUrl(const std::string& url) { http_base_url_ = url; }

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
    void on_apps_loaded();
    void on_app_versions_loaded();
    void on_task_selection_changed();
    void on_app_selection_changed();
    void on_tab_changed(int index);
    void on_auto_refresh_toggled(bool checked);
    void on_new_application();
    void on_new_app_version();
    void on_new_batch();
    void on_new_work_unit();
    void on_show_details();
    void on_download_input();
    void on_download_output();
    void on_task_double_clicked(const QModelIndex& index);
    void on_app_double_clicked(const QModelIndex& index);
    void on_app_version_double_clicked(const QModelIndex& index);

private:
    void setup_ui();
    QWidget* make_tasks_tab();
    QWidget* make_apps_tab();
    QWidget* make_hosts_tab();
    QWidget* make_transfers_tab();

    QToolBar* make_tab_toolbar(Qt::ToolButtonStyle style = Qt::ToolButtonTextBesideIcon);

    // Tab indices (App Versions is now embedded in the Apps tab, not a top tab)
    static constexpr int kTasksTab     = 0;
    static constexpr int kAppsTab      = 1;
    static constexpr int kHostsTab     = 2;
    static constexpr int kTransfersTab = 3;

    ClientManager*     client_manager_;
    ChangeReasonCache* change_reason_cache_;
    BadgeCache* badge_cache_;
    std::string        http_base_url_;

    // Models
    HostDisplayNameCache*                  host_cache_{nullptr};  // owned by this
    std::unique_ptr<ComputeTaskViewModel>  task_model_;
    std::unique_ptr<ClientAppModel>        app_model_;
    std::unique_ptr<ClientAppVersionModel> app_version_model_;
    std::unique_ptr<ClientHostModel>       host_model_;
    std::unique_ptr<ComputeTransferModel>  transfer_model_;

    // Host fetch watcher (also populates host_cache_)
    using HostList = std::vector<compute::domain::host>;
    QFutureWatcher<HostList>* host_watcher_{nullptr};

    // Top-level tab bar
    QTabWidget* main_tabs_{nullptr};

    // Tasks tab
    QTableView*            task_view_{nullptr};
    QSortFilterProxyModel* task_proxy_{nullptr};
    QAction*               logs_action_{nullptr};
    QAction*               download_input_action_{nullptr};
    QAction*               download_output_action_{nullptr};
    QString                selected_result_id_;
    const compute_task*    selected_task_{nullptr};

    // Apps tab — master-detail split
    QTableView*            app_view_{nullptr};
    QSortFilterProxyModel* app_proxy_{nullptr};
    QTableView*            app_version_view_{nullptr};
    QSortFilterProxyModel* app_version_proxy_{nullptr};
    QAction*               new_app_version_action_{nullptr};  // enabled when app selected

    // Hosts tab
    QTableView*            host_view_{nullptr};
    QSortFilterProxyModel* host_proxy_{nullptr};

    // Transfers tab
    QTableView* transfer_view_{nullptr};

    QTimer* auto_refresh_timer_{nullptr};
};

} // namespace ores::qt

#endif
