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
#include "ores.qt/ComputeConsoleWindow.hpp"

#include <QtConcurrent>
#include <QVBoxLayout>
#include <QHeaderView>
#include <QSplitter>
#include <QToolBar>
#include <QPointer>
#include <QFile>
#include <QFileDialog>
#include <QFileInfo>
#include <QNetworkAccessManager>
#include <QNetworkReply>
#include <QNetworkRequest>
#include <QUrl>
#include "ores.qt/AppProvisionerWizard.hpp"
#include "ores.qt/DetailDialogBase.hpp"
#include "ores.qt/EntityItemDelegate.hpp"
#include "ores.qt/BadgeColors.hpp"
#include "ores.qt/AppDetailDialog.hpp"
#include "ores.qt/AppVersionDetailDialog.hpp"
#include "ores.qt/BatchDetailDialog.hpp"
#include "ores.qt/WorkunitDetailDialog.hpp"
#include "ores.qt/MessageBoxHelper.hpp"
#include "ores.qt/OreLogViewerWidget.hpp"
#include "ores.qt/ColorConstants.hpp"
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/TransferProgressDelegate.hpp"
#include "ores.compute.api/messaging/host_protocol.hpp"
#include <boost/uuid/uuid_io.hpp>

namespace ores::qt {

using namespace ores::logging;

ComputeConsoleWindow::ComputeConsoleWindow(ClientManager* clientManager,
                                           ChangeReasonCache* changeReasonCache,
                                           QWidget* parent)
    : QWidget(parent),
      client_manager_(clientManager),
      change_reason_cache_(changeReasonCache),
      host_cache_(new HostDisplayNameCache(this)),
      task_model_(std::make_unique<ComputeTaskViewModel>(clientManager, this)),
      app_model_(std::make_unique<ClientAppModel>(clientManager, this)),
      app_version_model_(std::make_unique<ClientAppVersionModel>(clientManager, this)),
      host_model_(std::make_unique<ClientHostModel>(clientManager, this)),
      transfer_model_(std::make_unique<ComputeTransferModel>(this)),
      host_watcher_(new QFutureWatcher<HostList>(this)),
      auto_refresh_timer_(new QTimer(this)) {

    task_model_->set_host_name_cache(host_cache_);

    connect(task_model_.get(), &ComputeTaskViewModel::dataLoaded,
            this, &ComputeConsoleWindow::on_tasks_loaded);
    connect(task_model_.get(), &ComputeTaskViewModel::loadError,
            this, &ComputeConsoleWindow::on_tasks_error);
    connect(app_model_.get(), &ClientAppModel::dataLoaded,
            this, &ComputeConsoleWindow::on_apps_loaded);
    connect(app_version_model_.get(), &ClientAppVersionModel::dataLoaded,
            this, &ComputeConsoleWindow::on_app_versions_loaded);
    connect(host_watcher_, &QFutureWatcher<HostList>::finished,
            this, &ComputeConsoleWindow::on_hosts_loaded);

    auto_refresh_timer_->setInterval(15000);
    connect(auto_refresh_timer_, &QTimer::timeout,
            this, &ComputeConsoleWindow::refresh);

    setup_ui();

    if (client_manager_ && client_manager_->isLoggedIn())
        QTimer::singleShot(0, this, &ComputeConsoleWindow::refresh);
}

// ── UI setup ──────────────────────────────────────────────────────────────────

QToolBar* ComputeConsoleWindow::make_tab_toolbar(Qt::ToolButtonStyle style) {
    auto* tb = new QToolBar(this);
    tb->setMovable(false);
    tb->setToolButtonStyle(style);
    return tb;
}

void ComputeConsoleWindow::setup_ui() {
    auto* layout = new QVBoxLayout(this);
    layout->setContentsMargins(0, 0, 0, 0);
    layout->setSpacing(0);

    main_tabs_ = new QTabWidget(this);
    main_tabs_->addTab(make_tasks_tab(),     tr("Tasks"));
    main_tabs_->addTab(make_apps_tab(),      tr("Apps"));
    main_tabs_->addTab(make_hosts_tab(),     tr("Hosts"));
    main_tabs_->addTab(make_transfers_tab(), tr("Transfers"));

    connect(main_tabs_, &QTabWidget::currentChanged,
            this, &ComputeConsoleWindow::on_tab_changed);

    layout->addWidget(main_tabs_);
}

QWidget* ComputeConsoleWindow::make_tasks_tab() {
    task_proxy_ = new QSortFilterProxyModel(this);
    task_proxy_->setSourceModel(task_model_.get());

    task_view_ = new QTableView(this);
    task_view_->setModel(task_proxy_);
    task_view_->setSelectionBehavior(QAbstractItemView::SelectRows);
    task_view_->setSelectionMode(QAbstractItemView::SingleSelection);
    task_view_->setEditTriggers(QAbstractItemView::NoEditTriggers);
    task_view_->setAlternatingRowColors(true);
    task_view_->setSortingEnabled(true);
    task_view_->verticalHeader()->setVisible(false);
    task_view_->horizontalHeader()->setStretchLastSection(true);
    task_view_->horizontalHeader()->setSectionResizeMode(
        ComputeTaskViewModel::Label, QHeaderView::ResizeToContents);

    using cs = column_style;
    auto* task_delegate = new EntityItemDelegate({
        cs::text_left,      // Label
        cs::badge_centered, // State
        cs::badge_centered, // Outcome
        cs::text_left,      // Host
        cs::mono_center,    // Duration
        cs::text_left,      // Batch
        cs::text_left,      // Received
    }, task_view_);
    task_delegate->set_badge_color_resolver(resolve_compute_task_badge_color);
    task_view_->setItemDelegate(task_delegate);

    connect(task_view_->selectionModel(),
            &QItemSelectionModel::selectionChanged,
            this, &ComputeConsoleWindow::on_task_selection_changed);
    connect(task_view_, &QTableView::doubleClicked,
            this, &ComputeConsoleWindow::on_task_double_clicked);

    auto* toolbar = make_tab_toolbar();

    auto* new_batch_action = new QAction(
        IconUtils::createRecoloredIcon(Icon::Add, color_constants::icon_color),
        tr("Batch"), this);
    connect(new_batch_action, &QAction::triggered,
            this, &ComputeConsoleWindow::on_new_batch);
    toolbar->addAction(new_batch_action);

    auto* new_wu_action = new QAction(
        IconUtils::createRecoloredIcon(Icon::Add, color_constants::icon_color),
        tr("Work Unit"), this);
    connect(new_wu_action, &QAction::triggered,
            this, &ComputeConsoleWindow::on_new_work_unit);
    toolbar->addAction(new_wu_action);

    toolbar->addSeparator();

    logs_action_ = new QAction(
        IconUtils::createRecoloredIcon(Icon::Notepad,
            color_constants::icon_color),
        tr("Logs"), this);
    logs_action_->setEnabled(false);
    connect(logs_action_, &QAction::triggered,
            this, &ComputeConsoleWindow::on_show_logs);
    toolbar->addAction(logs_action_);

    download_input_action_ = new QAction(
        IconUtils::createRecoloredIcon(Icon::ArrowDownload,
            color_constants::icon_color),
        tr("Input"), this);
    download_input_action_->setToolTip(tr("Download input archive"));
    download_input_action_->setEnabled(false);
    connect(download_input_action_, &QAction::triggered,
            this, &ComputeConsoleWindow::on_download_input);
    toolbar->addAction(download_input_action_);

    download_output_action_ = new QAction(
        IconUtils::createRecoloredIcon(Icon::ArrowDownload,
            color_constants::icon_color),
        tr("Output"), this);
    download_output_action_->setToolTip(tr("Download output archive"));
    download_output_action_->setEnabled(false);
    connect(download_output_action_, &QAction::triggered,
            this, &ComputeConsoleWindow::on_download_output);
    toolbar->addAction(download_output_action_);

    toolbar->addSeparator();

    auto* refresh_action = new QAction(
        IconUtils::createRecoloredIcon(Icon::ArrowClockwise,
            color_constants::icon_color),
        tr("Refresh"), this);
    connect(refresh_action, &QAction::triggered,
            this, &ComputeConsoleWindow::refresh);
    toolbar->addAction(refresh_action);

    auto* auto_refresh_action = new QAction(
        IconUtils::createRecoloredIcon(Icon::ArrowSync,
            color_constants::icon_color),
        tr("Auto-refresh"), this);
    auto_refresh_action->setCheckable(true);
    connect(auto_refresh_action, &QAction::toggled,
            this, &ComputeConsoleWindow::on_auto_refresh_toggled);
    toolbar->addAction(auto_refresh_action);

    auto* container = new QWidget(this);
    auto* vl = new QVBoxLayout(container);
    vl->setContentsMargins(0, 0, 0, 0);
    vl->setSpacing(0);
    vl->addWidget(toolbar);
    vl->addWidget(task_view_);
    return container;
}

QWidget* ComputeConsoleWindow::make_apps_tab() {
    // ── top pane: apps ────────────────────────────────────────────────────────
    app_proxy_ = new QSortFilterProxyModel(this);
    app_proxy_->setSourceModel(app_model_.get());

    app_view_ = new QTableView(this);
    app_view_->setModel(app_proxy_);
    app_view_->setSelectionBehavior(QAbstractItemView::SelectRows);
    app_view_->setSelectionMode(QAbstractItemView::SingleSelection);
    app_view_->setEditTriggers(QAbstractItemView::NoEditTriggers);
    app_view_->setAlternatingRowColors(true);
    app_view_->setSortingEnabled(true);
    app_view_->verticalHeader()->setVisible(false);
    app_view_->horizontalHeader()->setStretchLastSection(true);
    app_view_->horizontalHeader()->setSectionResizeMode(
        ClientAppModel::Name, QHeaderView::ResizeToContents);

    connect(app_view_->selectionModel(),
            &QItemSelectionModel::selectionChanged,
            this, &ComputeConsoleWindow::on_app_selection_changed);
    connect(app_view_, &QTableView::doubleClicked,
            this, &ComputeConsoleWindow::on_app_double_clicked);

    auto* app_toolbar = make_tab_toolbar();

    auto* new_app_action = new QAction(
        IconUtils::createRecoloredIcon(Icon::Add, color_constants::icon_color),
        tr("App"), this);
    connect(new_app_action, &QAction::triggered,
            this, &ComputeConsoleWindow::on_new_application);
    app_toolbar->addAction(new_app_action);

    app_toolbar->addSeparator();

    auto* refresh_apps = new QAction(
        IconUtils::createRecoloredIcon(Icon::ArrowClockwise,
            color_constants::icon_color),
        tr("Refresh"), this);
    connect(refresh_apps, &QAction::triggered, this, [this]() {
        app_model_->refresh();
        app_version_model_->refresh();
    });
    app_toolbar->addAction(refresh_apps);

    auto* top_pane = new QWidget(this);
    auto* top_vl = new QVBoxLayout(top_pane);
    top_vl->setContentsMargins(0, 0, 0, 0);
    top_vl->setSpacing(0);
    top_vl->addWidget(app_toolbar);
    top_vl->addWidget(app_view_);

    // ── bottom pane: app versions filtered by selected app ────────────────────
    app_version_proxy_ = new QSortFilterProxyModel(this);
    app_version_proxy_->setSourceModel(app_version_model_.get());
    app_version_proxy_->setFilterKeyColumn(ClientAppVersionModel::AppId);

    app_version_view_ = new QTableView(this);
    app_version_view_->setModel(app_version_proxy_);
    app_version_view_->setSelectionBehavior(QAbstractItemView::SelectRows);
    app_version_view_->setSelectionMode(QAbstractItemView::SingleSelection);
    app_version_view_->setEditTriggers(QAbstractItemView::NoEditTriggers);
    app_version_view_->setAlternatingRowColors(true);
    app_version_view_->setSortingEnabled(true);
    app_version_view_->verticalHeader()->setVisible(false);
    app_version_view_->horizontalHeader()->setStretchLastSection(true);
    app_version_view_->horizontalHeader()->setSectionResizeMode(
        ClientAppVersionModel::AppId, QHeaderView::ResizeToContents);

    connect(app_version_view_, &QTableView::doubleClicked,
            this, &ComputeConsoleWindow::on_app_version_double_clicked);

    auto* av_toolbar = make_tab_toolbar();

    new_app_version_action_ = new QAction(
        IconUtils::createRecoloredIcon(Icon::Add, color_constants::icon_color),
        tr("App Version"), this);
    new_app_version_action_->setEnabled(false);
    connect(new_app_version_action_, &QAction::triggered,
            this, &ComputeConsoleWindow::on_new_app_version);
    av_toolbar->addAction(new_app_version_action_);

    auto* bottom_pane = new QWidget(this);
    auto* bot_vl = new QVBoxLayout(bottom_pane);
    bot_vl->setContentsMargins(0, 0, 0, 0);
    bot_vl->setSpacing(0);
    bot_vl->addWidget(av_toolbar);
    bot_vl->addWidget(app_version_view_);

    // ── splitter ──────────────────────────────────────────────────────────────
    auto* splitter = new QSplitter(Qt::Vertical, this);
    splitter->addWidget(top_pane);
    splitter->addWidget(bottom_pane);
    splitter->setStretchFactor(0, 2);
    splitter->setStretchFactor(1, 1);
    return splitter;
}

QWidget* ComputeConsoleWindow::make_hosts_tab() {
    host_proxy_ = new QSortFilterProxyModel(this);
    host_proxy_->setSourceModel(host_model_.get());

    host_view_ = new QTableView(this);
    host_view_->setModel(host_proxy_);
    host_view_->setSelectionBehavior(QAbstractItemView::SelectRows);
    host_view_->setSelectionMode(QAbstractItemView::SingleSelection);
    host_view_->setEditTriggers(QAbstractItemView::NoEditTriggers);
    host_view_->setAlternatingRowColors(true);
    host_view_->setSortingEnabled(true);
    host_view_->verticalHeader()->setVisible(false);
    host_view_->horizontalHeader()->setStretchLastSection(true);
    host_view_->horizontalHeader()->setSectionResizeMode(
        ClientHostModel::DisplayName, QHeaderView::ResizeToContents);

    auto* toolbar = make_tab_toolbar();

    auto* refresh_action = new QAction(
        IconUtils::createRecoloredIcon(Icon::ArrowClockwise,
            color_constants::icon_color),
        tr("Refresh"), this);
    connect(refresh_action, &QAction::triggered, this, [this]() {
        host_model_->refresh();
    });
    toolbar->addAction(refresh_action);

    auto* container = new QWidget(this);
    auto* vl = new QVBoxLayout(container);
    vl->setContentsMargins(0, 0, 0, 0);
    vl->setSpacing(0);
    vl->addWidget(toolbar);
    vl->addWidget(host_view_);
    return container;
}

QWidget* ComputeConsoleWindow::make_transfers_tab() {
    transfer_view_ = new QTableView(this);
    transfer_view_->setModel(transfer_model_.get());
    transfer_view_->setItemDelegateForColumn(
        ComputeTransferModel::Progress,
        new TransferProgressDelegate(transfer_view_));
    transfer_view_->setSelectionBehavior(QAbstractItemView::SelectRows);
    transfer_view_->setSelectionMode(QAbstractItemView::SingleSelection);
    transfer_view_->setEditTriggers(QAbstractItemView::NoEditTriggers);
    transfer_view_->setAlternatingRowColors(true);
    transfer_view_->verticalHeader()->setVisible(false);
    transfer_view_->horizontalHeader()->setStretchLastSection(true);
    transfer_view_->horizontalHeader()->setSectionResizeMode(
        ComputeTransferModel::Filename, QHeaderView::Stretch);

    return transfer_view_;
}

// ── Data loading ──────────────────────────────────────────────────────────────

void ComputeConsoleWindow::refresh() {
    BOOST_LOG_SEV(lg(), debug) << "Refreshing compute console.";
    emit statusChanged(tr("Refreshing…"));

    if (!client_manager_ || !client_manager_->isConnected()) {
        emit errorOccurred(tr("Not connected to server"));
        return;
    }

    // Apps and app versions are independent — fetch immediately.
    app_model_->refresh();
    app_version_model_->refresh();

    // Fetch hosts first; on_hosts_loaded() populates the cache then
    // triggers the task refresh.
    QPointer<ComputeConsoleWindow> self = this;
    host_watcher_->setFuture(QtConcurrent::run([self]() -> HostList {
        if (!self || !self->client_manager_) return {};

        compute::messaging::list_hosts_request req;
        auto resp = self->client_manager_->
            process_authenticated_request(std::move(req));

        if (!resp) {
            BOOST_LOG_SEV(lg(), warn)
                << "Host fetch failed: " << resp.error();
            return {};
        }
        return std::move(resp->hosts);
    }));
}

void ComputeConsoleWindow::on_hosts_loaded() {
    const auto hosts = host_watcher_->result();
    host_cache_->populate_from(hosts);
    host_model_->refresh();

    BOOST_LOG_SEV(lg(), debug) << "Hosts loaded: " << hosts.size();
    task_model_->refresh();
}

void ComputeConsoleWindow::on_tasks_loaded() {
    const int n = task_model_->rowCount();
    const int h = host_model_->rowCount();
    emit statusChanged(tr("%1 tasks, %2 hosts").arg(n).arg(h));
    BOOST_LOG_SEV(lg(), info) << "Console loaded " << n << " tasks, "
                              << h << " hosts.";

    main_tabs_->setTabText(kTasksTab, tr("Tasks (%1)").arg(n));
    main_tabs_->setTabText(kHostsTab, tr("Hosts (%1)").arg(h));
}

void ComputeConsoleWindow::on_apps_loaded() {
    const int n = app_model_->rowCount();
    BOOST_LOG_SEV(lg(), debug) << "Apps loaded: " << n;
    main_tabs_->setTabText(kAppsTab, tr("Apps (%1)").arg(n));
}

void ComputeConsoleWindow::on_app_versions_loaded() {
    const int n = app_version_model_->rowCount();
    BOOST_LOG_SEV(lg(), debug) << "App versions loaded: " << n;
}

void ComputeConsoleWindow::on_tasks_error(
    const QString& message, const QString& /*details*/) {
    BOOST_LOG_SEV(lg(), error) << "Task load failed: " << message.toStdString();
    emit errorOccurred(message);
    emit statusChanged(tr("Error: %1").arg(message));
}

// ── Helpers ───────────────────────────────────────────────────────────────────

/**
 * Open a DetailDialogBase-derived widget as a proper top-level window.
 *
 * DetailDialogBase is a QWidget (not a QDialog), designed to live inside a
 * DetachableMdiSubWindow.  When opened directly from ComputeConsoleWindow we
 * have no MDI area, so we set Qt::Window to get a real window frame and
 * connect closeRequested → close() so the "Close" button works.
 */
static void show_detail_as_window(DetailDialogBase* dlg, const QString& title) {
    dlg->setWindowFlags(Qt::Window);
    dlg->setWindowTitle(title);
    dlg->setAttribute(Qt::WA_DeleteOnClose);
    QObject::connect(dlg, &DetailDialogBase::closeRequested,
                     dlg, &QWidget::close);
    dlg->show();
}

// ── Interaction ───────────────────────────────────────────────────────────────

void ComputeConsoleWindow::on_task_selection_changed() {
    const auto selected = task_view_->selectionModel()->selectedRows();
    if (selected.isEmpty()) {
        selected_result_id_.clear();
        selected_task_ = nullptr;
        logs_action_->setEnabled(false);
        download_input_action_->setEnabled(false);
        download_output_action_->setEnabled(false);
        return;
    }

    const QModelIndex src = task_proxy_->mapToSource(selected.first());
    const auto* task = task_model_->get_task(src.row());
    if (!task) return;

    selected_task_ = task;
    selected_result_id_ = QString::fromStdString(
        boost::uuids::to_string(task->result.id));
    logs_action_->setEnabled(true);
    download_input_action_->setEnabled(!task->workunit.input_uri.empty());
    // Output only available when job completed successfully (state=5, outcome=1)
    download_output_action_->setEnabled(
        task->result.server_state == 5 && task->result.outcome == 1);
}

void ComputeConsoleWindow::on_app_selection_changed() {
    const auto selected = app_view_->selectionModel()->selectedRows();
    if (selected.isEmpty()) {
        app_version_proxy_->setFilterFixedString({});
        new_app_version_action_->setEnabled(false);
        return;
    }

    const QModelIndex src = app_proxy_->mapToSource(selected.first());
    const auto* app = app_model_->getApp(src.row());
    if (!app) return;

    const auto uuid_str = boost::uuids::to_string(app->id);
    app_version_proxy_->setFilterFixedString(
        QString::fromStdString(uuid_str));
    new_app_version_action_->setEnabled(true);
}

void ComputeConsoleWindow::on_tab_changed(int /*index*/) {}

void ComputeConsoleWindow::on_new_application() {
    auto* wizard = new AppProvisionerWizard(
        client_manager_, change_reason_cache_, http_base_url_, this);
    wizard->setAttribute(Qt::WA_DeleteOnClose);
    connect(wizard, &AppProvisionerWizard::provisioned, this, [this]() {
        app_model_->refresh();
        app_version_model_->refresh();
    });
    wizard->open();
}

void ComputeConsoleWindow::on_new_app_version() {
    const auto selected = app_view_->selectionModel()->selectedRows();
    if (selected.isEmpty()) return;

    const QModelIndex src = app_proxy_->mapToSource(selected.first());
    const auto* app = app_model_->getApp(src.row());
    if (!app) return;

    compute::domain::app_version version;
    version.app_id = app->id;

    auto* dlg = new AppVersionDetailDialog(this);
    dlg->setClientManager(client_manager_);
    dlg->setUsername(client_manager_ ? client_manager_->currentUsername() : "");
    dlg->setChangeReasonCache(change_reason_cache_);
    dlg->setHttpBaseUrl(http_base_url_);
    dlg->setVersion(version);
    dlg->setCreateMode(true);
    connect(dlg, &AppVersionDetailDialog::app_versionSaved, this, [this](const QString&) {
        app_version_model_->refresh();
    });
    show_detail_as_window(dlg, tr("New App Version"));
}

void ComputeConsoleWindow::on_new_batch() {
    auto* dlg = new BatchDetailDialog(this);
    dlg->setClientManager(client_manager_);
    dlg->setUsername(client_manager_ ? client_manager_->currentUsername() : "");
    dlg->setChangeReasonCache(change_reason_cache_);
    dlg->setCreateMode(true);
    connect(dlg, &BatchDetailDialog::batchSaved, this, [this](const QString&) {
        refresh();
    });
    show_detail_as_window(dlg, tr("New Batch"));
}

void ComputeConsoleWindow::on_new_work_unit() {
    auto* dlg = new WorkunitDetailDialog(this);
    dlg->setClientManager(client_manager_);
    dlg->setUsername(client_manager_ ? client_manager_->currentUsername() : "");
    dlg->setChangeReasonCache(change_reason_cache_);
    dlg->setHttpBaseUrl(http_base_url_);
    dlg->setCreateMode(true);
    connect(dlg, &WorkunitDetailDialog::workunitSaved, this, [this](const QString&) {
        refresh();
    });
    show_detail_as_window(dlg, tr("New Work Unit"));
}

void ComputeConsoleWindow::on_show_logs() {
    if (selected_result_id_.isEmpty()) return;

    auto* viewer = new OreLogViewerWidget(client_manager_, this);
    viewer->setAttribute(Qt::WA_DeleteOnClose);
    viewer->setWindowTitle(tr("Logs — %1").arg(selected_result_id_));
    viewer->setWindowFlags(Qt::Window);
    viewer->resize(900, 600);
    viewer->load_result(selected_result_id_);
    viewer->show();
}

void ComputeConsoleWindow::on_download_input() {
    if (!selected_task_ || selected_task_->workunit.input_uri.empty()) return;

    const auto uri = QString::fromStdString(selected_task_->workunit.input_uri);
    const QString default_name = QFileInfo(uri).fileName();
    const QString save_path = QFileDialog::getSaveFileName(
        this, tr("Save Input Archive"), default_name,
        tr("Archives (*.tar.gz *.zip);;All Files (*)"));
    if (save_path.isEmpty()) return;

    const QUrl url = QUrl(QString::fromStdString(http_base_url_) + "/" + uri);
    auto* nam = new QNetworkAccessManager(this);
    auto* reply = nam->get(QNetworkRequest(url));
    QPointer<ComputeConsoleWindow> self = this;
    connect(reply, &QNetworkReply::finished, this,
        [self, reply, nam, save_path]() {
            reply->deleteLater();
            nam->deleteLater();
            if (!self) return;
            if (reply->error() != QNetworkReply::NoError) {
                MessageBoxHelper::critical(self, tr("Download Failed"),
                    reply->errorString());
                return;
            }
            QFile f(save_path);
            if (f.open(QIODevice::WriteOnly)) {
                f.write(reply->readAll());
                f.close();
            } else {
                MessageBoxHelper::critical(self, tr("Save Failed"),
                    tr("Cannot write to: %1").arg(save_path));
            }
        });
}

void ComputeConsoleWindow::on_download_output() {
    if (selected_result_id_.isEmpty()) return;

    const QString default_name =
        "output_" + selected_result_id_.left(8) + ".tar.gz";
    const QString save_path = QFileDialog::getSaveFileName(
        this, tr("Save Output Archive"), default_name,
        tr("Archives (*.tar.gz);;All Files (*)"));
    if (save_path.isEmpty()) return;

    const QUrl url = QUrl(
        QString::fromStdString(http_base_url_) +
        "/api/v1/compute/results/" + selected_result_id_ + "/output");
    auto* nam = new QNetworkAccessManager(this);
    auto* reply = nam->get(QNetworkRequest(url));
    QPointer<ComputeConsoleWindow> self = this;
    connect(reply, &QNetworkReply::finished, this,
        [self, reply, nam, save_path]() {
            reply->deleteLater();
            nam->deleteLater();
            if (!self) return;
            if (reply->error() != QNetworkReply::NoError) {
                MessageBoxHelper::critical(self, tr("Download Failed"),
                    reply->errorString());
                return;
            }
            QFile f(save_path);
            if (f.open(QIODevice::WriteOnly)) {
                f.write(reply->readAll());
                f.close();
            } else {
                MessageBoxHelper::critical(self, tr("Save Failed"),
                    tr("Cannot write to: %1").arg(save_path));
            }
        });
}

void ComputeConsoleWindow::on_task_double_clicked(const QModelIndex& index) {
    if (!index.isValid()) return;

    const QModelIndex src = task_proxy_->mapToSource(index);
    const auto* task = task_model_->get_task(src.row());
    if (!task) return;

    auto* dlg = new WorkunitDetailDialog(this);
    dlg->setClientManager(client_manager_);
    dlg->setUsername(client_manager_ ? client_manager_->currentUsername() : "");
    dlg->setChangeReasonCache(change_reason_cache_);
    dlg->setHttpBaseUrl(http_base_url_);
    dlg->setWorkunit(task->workunit);
    dlg->setCreateMode(false);
    connect(dlg, &WorkunitDetailDialog::workunitSaved, this, [this](const QString&) {
        refresh();
    });
    show_detail_as_window(dlg, tr("Work Unit"));
}

void ComputeConsoleWindow::on_app_double_clicked(const QModelIndex& index) {
    if (!index.isValid()) return;

    const QModelIndex src = app_proxy_->mapToSource(index);
    const auto* app = app_model_->getApp(src.row());
    if (!app) return;

    auto* dlg = new AppDetailDialog(this);
    dlg->setClientManager(client_manager_);
    dlg->setUsername(client_manager_ ? client_manager_->currentUsername() : "");
    dlg->setChangeReasonCache(change_reason_cache_);
    dlg->setApp(*app);
    dlg->setCreateMode(false);
    connect(dlg, &AppDetailDialog::appSaved, this, [this](const QString&) {
        app_model_->refresh();
    });
    show_detail_as_window(dlg, QString::fromStdString(app->name));
}

void ComputeConsoleWindow::on_app_version_double_clicked(const QModelIndex& index) {
    if (!index.isValid()) return;

    const QModelIndex src = app_version_proxy_->mapToSource(index);
    const auto* version = app_version_model_->getVersion(src.row());
    if (!version) return;

    auto* dlg = new AppVersionDetailDialog(this);
    dlg->setClientManager(client_manager_);
    dlg->setUsername(client_manager_ ? client_manager_->currentUsername() : "");
    dlg->setChangeReasonCache(change_reason_cache_);
    dlg->setHttpBaseUrl(http_base_url_);
    dlg->setVersion(*version);
    dlg->setCreateMode(false);
    connect(dlg, &AppVersionDetailDialog::app_versionSaved, this, [this](const QString&) {
        app_version_model_->refresh();
    });
    show_detail_as_window(dlg, tr("App Version"));
}

void ComputeConsoleWindow::on_auto_refresh_toggled(bool checked) {
    if (checked) {
        auto_refresh_timer_->start();
    } else {
        auto_refresh_timer_->stop();
    }
    BOOST_LOG_SEV(lg(), debug) << "Auto-refresh " << (checked ? "on" : "off");
}

} // namespace ores::qt
