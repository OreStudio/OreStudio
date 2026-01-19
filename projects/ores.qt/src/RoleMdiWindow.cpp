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
#include "ores.qt/RoleMdiWindow.hpp"

#include <vector>
#include <QtCore/QVariant>
#include <QtCore/QTimer>
#include <QtConcurrent>
#include <QFutureWatcher>
#include <QtWidgets/QWidget>
#include <QtWidgets/QHBoxLayout>
#include <QtWidgets/QHeaderView>
#include <QtWidgets/QTableView>
#include <QtWidgets/QScrollBar>
#include <QtWidgets/QApplication>
#include <QMessageBox>
#include <QToolBar>
#include <QAction>
#include <QSortFilterProxyModel>
#include <boost/uuid/uuid_io.hpp>
#include "ores.qt/ColorConstants.hpp"
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/MessageBoxHelper.hpp"

namespace ores::qt {

using namespace ores::logging;

RoleMdiWindow::
RoleMdiWindow(ClientManager* clientManager,
              const QString& username,
              QWidget* parent)
    : EntityListMdiWindow(parent),
      verticalLayout_(new QVBoxLayout(this)),
      roleTableView_(new QTableView(this)),
      toolBar_(new QToolBar(this)),
      reloadAction_(new QAction("Reload", this)),
      viewAction_(new QAction("View", this)),
      roleModel_(std::make_unique<ClientRoleModel>(clientManager)),
      proxyModel_(new QSortFilterProxyModel(this)),
      clientManager_(clientManager),
      username_(username) {

    BOOST_LOG_SEV(lg(), debug) << "Creating role MDI window";

    toolBar_->setMovable(false);
    toolBar_->setToolButtonStyle(Qt::ToolButtonTextUnderIcon);
    const auto& iconColor = color_constants::icon_color;

    // Setup reload action with normal and stale icons
    setupReloadAction();
    toolBar_->addAction(reloadAction_);

    toolBar_->addSeparator();

    viewAction_->setIcon(IconUtils::createRecoloredIcon(
            ":/icons/ic_fluent_edit_20_regular.svg", iconColor));
    viewAction_->setToolTip("View role details");
    connect(viewAction_, &QAction::triggered, this,
        &RoleMdiWindow::viewSelected);
    toolBar_->addAction(viewAction_);

    verticalLayout_->addWidget(toolBar_);
    verticalLayout_->addWidget(roleTableView_);

    roleTableView_->setObjectName("roleTableView");
    roleTableView_->setAlternatingRowColors(true);
    roleTableView_->setSelectionMode(QAbstractItemView::SingleSelection);
    roleTableView_->setSelectionBehavior(QAbstractItemView::SelectRows);
    roleTableView_->setWordWrap(false);

    proxyModel_->setSourceModel(roleModel_.get());
    roleTableView_->setModel(proxyModel_);
    roleTableView_->setSortingEnabled(true);
    roleTableView_->sortByColumn(0, Qt::AscendingOrder);

    QHeaderView* horizontalHeader(roleTableView_->horizontalHeader());
    roleTableView_->verticalHeader()->setSectionResizeMode(QHeaderView::Fixed);
    horizontalHeader->setSectionResizeMode(QHeaderView::ResizeToContents);

    // Connect signals
    connect(roleModel_.get(), &ClientRoleModel::dataLoaded,
            this, &RoleMdiWindow::onDataLoaded);
    connect(roleModel_.get(), &ClientRoleModel::loadError,
            this, &RoleMdiWindow::onLoadError);
    connect(roleTableView_, &QTableView::doubleClicked,
            this, &RoleMdiWindow::onRowDoubleClicked);
    connect(roleTableView_->selectionModel(),
        &QItemSelectionModel::selectionChanged,
            this, &RoleMdiWindow::onSelectionChanged);

    // Connect connection state signals
    if (clientManager_) {
        connect(clientManager_, &ClientManager::connected,
            this, &RoleMdiWindow::onConnectionStateChanged);
        connect(clientManager_, &ClientManager::disconnected,
            this, &RoleMdiWindow::onConnectionStateChanged);
    }

    updateActionStates();

    emit statusChanged("Loading roles...");

    // Initial load
    if (clientManager_->isConnected()) {
        roleModel_->refresh();
    } else {
        emit statusChanged("Disconnected - Offline");
        toolBar_->setEnabled(false);
    }
}

RoleMdiWindow::~RoleMdiWindow() {
    BOOST_LOG_SEV(lg(), debug) << "Destroying role MDI window";

    const auto watchers = findChildren<QFutureWatcherBase*>();
    for (auto* watcher : watchers) {
        disconnect(watcher, nullptr, this, nullptr);
        watcher->cancel();
        watcher->waitForFinished();
    }
}

void RoleMdiWindow::onConnectionStateChanged() {
    const bool connected = clientManager_ && clientManager_->isConnected();
    toolBar_->setEnabled(connected);

    if (connected) {
        emit statusChanged("Connected");
    } else {
        emit statusChanged("Disconnected - Offline");
    }
}

void RoleMdiWindow::reload() {
    BOOST_LOG_SEV(lg(), debug) << "Reload requested";
    if (!clientManager_->isConnected()) {
        emit statusChanged("Cannot reload - Disconnected");
        return;
    }
    emit statusChanged("Reloading roles...");
    clearStaleIndicator();
    roleModel_->refresh();
}

void RoleMdiWindow::onDataLoaded() {
    const auto loaded = roleModel_->rowCount();

    const QString message = QString("Loaded %1 roles").arg(loaded);
    emit statusChanged(message);
    BOOST_LOG_SEV(lg(), debug) << "Role data loaded successfully: "
                             << loaded << " roles";

    if (roleModel_->rowCount() > 0 &&
        roleTableView_->selectionModel()->selectedRows().isEmpty()) {
        roleTableView_->selectRow(0);
        BOOST_LOG_SEV(lg(), debug) << "Auto-selected first row";
    }
}

void RoleMdiWindow::onLoadError(const QString& error_message,
                                 const QString& details) {
    emit errorOccurred(error_message);
    BOOST_LOG_SEV(lg(), error) << "Error loading roles: "
                              << error_message.toStdString();

    MessageBoxHelper::critical(this, tr("Load Error"), error_message, details);
}

void RoleMdiWindow::onRowDoubleClicked(const QModelIndex& index) {
    if (!index.isValid()) {
        return;
    }

    // Map proxy index to source index
    const QModelIndex sourceIndex = proxyModel_->mapToSource(index);
    const auto* role = roleModel_->getRole(sourceIndex.row());
    if (!role) {
        BOOST_LOG_SEV(lg(), warn) << "Failed to get role for row: "
                                 << sourceIndex.row();
        return;
    }

    BOOST_LOG_SEV(lg(), debug) << "Emitting showRoleDetails for role: "
                             << role->name;
    emit showRoleDetails(*role);
}

void RoleMdiWindow::onSelectionChanged() {
    const int selection_count = roleTableView_->selectionModel()->selectedRows().count();
    updateActionStates();
    emit selectionChanged(selection_count);
}

void RoleMdiWindow::viewSelected() {
    const auto selected = roleTableView_->selectionModel()->selectedRows();
    if (selected.isEmpty()) {
        BOOST_LOG_SEV(lg(), warn) << "View requested but no row selected";
        return;
    }

    onRowDoubleClicked(selected.first());
}

QSize RoleMdiWindow::sizeHint() const {
    const int minimumWidth = 700;
    const int minimumHeight = 400;

    QSize baseSize = QWidget::sizeHint();

    return { qMax(baseSize.width(), minimumWidth),
             qMax(baseSize.height(), minimumHeight) };
}

void RoleMdiWindow::updateActionStates() {
    const int selection_count = roleTableView_
        ->selectionModel()->selectedRows().count();
    const bool hasSingleSelection = selection_count == 1;

    viewAction_->setEnabled(hasSingleSelection);
}

void RoleMdiWindow::setupReloadAction() {
    const auto& iconColor = color_constants::icon_color;

    reloadAction_->setIcon(IconUtils::createRecoloredIcon(
        ":/icons/ic_fluent_arrow_sync_20_regular.svg", iconColor));
    connect(reloadAction_, &QAction::triggered, this, &RoleMdiWindow::reload);

    initializeStaleIndicator(reloadAction_, ":/icons/ic_fluent_arrow_sync_20_regular.svg");
}

}
