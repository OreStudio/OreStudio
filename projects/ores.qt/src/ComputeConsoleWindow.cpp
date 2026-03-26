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
#include <QPointer>
#include "ores.qt/ColorConstants.hpp"
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/TransferProgressDelegate.hpp"
#include "ores.compute.api/messaging/host_protocol.hpp"
#include <boost/uuid/uuid_io.hpp>

namespace ores::qt {

using namespace ores::logging;

ComputeConsoleWindow::ComputeConsoleWindow(ClientManager* clientManager,
                                           QWidget* parent)
    : QWidget(parent),
      client_manager_(clientManager),
      host_cache_(new HostDisplayNameCache(this)),
      task_model_(std::make_unique<ComputeTaskViewModel>(clientManager, this)),
      host_model_(std::make_unique<ClientHostModel>(clientManager, this)),
      transfer_model_(std::make_unique<ComputeTransferModel>(this)),
      host_watcher_(new QFutureWatcher<HostList>(this)),
      auto_refresh_timer_(new QTimer(this)) {

    task_model_->set_host_name_cache(host_cache_);

    connect(task_model_.get(), &ComputeTaskViewModel::dataLoaded,
            this, &ComputeConsoleWindow::on_tasks_loaded);
    connect(task_model_.get(), &ComputeTaskViewModel::loadError,
            this, &ComputeConsoleWindow::on_tasks_error);
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

void ComputeConsoleWindow::setup_ui() {
    auto* layout = new QVBoxLayout(this);
    layout->setContentsMargins(0, 0, 0, 0);
    layout->setSpacing(0);

    setup_toolbar();
    layout->addWidget(toolbar_);

    main_tabs_ = new QTabWidget(this);
    main_tabs_->addTab(make_tasks_tab(),     tr("Tasks"));
    main_tabs_->addTab(make_hosts_tab(),     tr("Hosts"));
    main_tabs_->addTab(make_transfers_tab(), tr("Transfers"));

    connect(main_tabs_, &QTabWidget::currentChanged,
            this, &ComputeConsoleWindow::on_tab_changed);

    layout->addWidget(main_tabs_);
}

void ComputeConsoleWindow::setup_toolbar() {
    toolbar_ = new QToolBar(this);
    toolbar_->setMovable(false);
    toolbar_->setToolButtonStyle(Qt::ToolButtonIconOnly);

    refresh_action_ = new QAction(
        IconUtils::createRecoloredIcon(Icon::ArrowClockwise,
            color_constants::icon_color),
        tr("Refresh"), this);
    connect(refresh_action_, &QAction::triggered,
            this, &ComputeConsoleWindow::refresh);
    toolbar_->addAction(refresh_action_);

    toolbar_->addSeparator();

    auto_refresh_action_ = new QAction(
        IconUtils::createRecoloredIcon(Icon::ArrowSync,
            color_constants::icon_color),
        tr("Auto-refresh every 15 s"), this);
    auto_refresh_action_->setCheckable(true);
    connect(auto_refresh_action_, &QAction::toggled,
            this, &ComputeConsoleWindow::on_auto_refresh_toggled);
    toolbar_->addAction(auto_refresh_action_);
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

    connect(task_view_->selectionModel(),
            &QItemSelectionModel::selectionChanged,
            this, &ComputeConsoleWindow::on_task_selection_changed);

    log_viewer_ = new OreLogViewerWidget(client_manager_, this);

    auto* splitter = new QSplitter(Qt::Vertical, this);
    splitter->addWidget(task_view_);
    splitter->addWidget(log_viewer_);
    splitter->setStretchFactor(0, 3);
    splitter->setStretchFactor(1, 2);

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

    return host_view_;
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

    // Fetch hosts first; on_hosts_loaded() triggers the task refresh.
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

    main_tabs_->setTabText(0, tr("Tasks (%1)").arg(n));
    main_tabs_->setTabText(1, tr("Hosts (%1)").arg(h));
}

void ComputeConsoleWindow::on_tasks_error(
    const QString& message, const QString& /*details*/) {
    BOOST_LOG_SEV(lg(), error) << "Task load failed: " << message.toStdString();
    emit errorOccurred(message);
    emit statusChanged(tr("Error: %1").arg(message));
}

// ── Interaction ───────────────────────────────────────────────────────────────

void ComputeConsoleWindow::on_task_selection_changed() {
    const auto selected = task_view_->selectionModel()->selectedRows();
    if (selected.isEmpty()) {
        log_viewer_->clear();
        return;
    }

    const QModelIndex src = task_proxy_->mapToSource(selected.first());
    const auto* task = task_model_->get_task(src.row());
    if (!task) return;

    const auto result_id = QString::fromStdString(
        boost::uuids::to_string(task->result.id));
    log_viewer_->load_result(result_id);
}

void ComputeConsoleWindow::on_tab_changed(int /*index*/) {}

void ComputeConsoleWindow::on_auto_refresh_toggled(bool checked) {
    if (checked) {
        auto_refresh_timer_->start();
    } else {
        auto_refresh_timer_->stop();
    }
    BOOST_LOG_SEV(lg(), debug) << "Auto-refresh " << (checked ? "on" : "off");
}

} // namespace ores::qt
